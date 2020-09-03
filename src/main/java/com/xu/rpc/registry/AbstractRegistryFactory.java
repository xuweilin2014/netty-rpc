package com.xu.rpc.registry;

import com.xu.rpc.commons.Assert;
import com.xu.rpc.commons.URL;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public abstract class AbstractRegistryFactory implements RegistryFactory{

    private static final ConcurrentHashMap<String, Registry> registries = new ConcurrentHashMap<>();

    private static final Lock lock = new ReentrantLock();

    @Override
    public Registry getRegistry(URL url) {
        String key = getRegistryKey(url);
        // 加锁，防止注册中心的重复创建
        lock.lock();
        try{
            Registry registry = registries.get(key);
            if (registry == null){
                registry = createRegistry(url);
                if (registry == null){
                    throw new IllegalStateException("error occurs in creating registry.");
                }
                registries.put(key, registry);
            }
            return registry;
        }finally {
            lock.unlock();
        }
    }

    public void destroy(){
        // TODO: 2020/8/12
    }

    // RegistryKey 组成为：注册中心名称://IP地址:PORT
    // 也就是在一台客户机上，使用相同地址的相同类型注册中心的话，就会获取到相同的 Registry 对象类型
    private String getRegistryKey(URL url){
        Assert.notNull(url, "url cannot be null, when fetching registry.");
        return url.getProtocol() + "://" + url.getHost() + ":" + url.getPort();
    }

    public abstract Registry createRegistry(URL url);
}
