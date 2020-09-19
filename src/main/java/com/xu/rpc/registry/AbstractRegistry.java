package com.xu.rpc.registry;

import com.xu.rpc.commons.Assert;
import com.xu.rpc.commons.URL;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.parallel.NamedThreadFactory;
import io.netty.util.internal.ConcurrentSet;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

public abstract class AbstractRegistry implements Registry {

    protected static Logger logger = Logger.getLogger(AbstractRegistry.class);

    private final List<URL> registered = new CopyOnWriteArrayList<>();

    private final Map<URL, Set<NotifyListener>> subscribed = new ConcurrentHashMap<>();

    private static final ExecutorService cacheExecutor = Executors.newFixedThreadPool(1, new NamedThreadFactory("RegistrySaveCacheThread", true));

    private static final int MAX_SAVE_CACHE_RETRY_TIMES = 3;

    private final AtomicLong cacheVersion = new AtomicLong(0);

    private final Properties properties = new Properties();

    private final File file;

    private final AtomicBoolean destroyed = new AtomicBoolean(false);

    private URL registryUrl;

    public AbstractRegistry(URL url){
        this.registryUrl = url;

        // 如果用户在 <nettyrpc:registry/> 中配置了 file 属性的话，使用用户自己的设置，否则使用默认的设置
        String defaultPath = url.getServiceName() + "-" + url.getParameter(RpcConfig.APPLICATION_KEY) + "-cache";
        String path = url.getParameter(RpcConfig.FILE_KEY, defaultPath);
        this.file = new File(path);
        // 如果 file 文件已经存在的话，那么它的父目录肯定已经被创建了
        if (!file.exists()){
            if (file.getParentFile() != null && !file.getParentFile().exists()){
                // 创建 file 的父目录，创建失败的话，打印警告的日志信息
                if (!file.getParentFile().mkdirs()) {
                    logger.warn("cannot create parent directories " + file.getParentFile());
                }
            }
        }

        // 如果 file 缓存文件存在的话，就从 file 文件读取缓存到内存的 properties 对象中
        if (file.exists()){
            try (FileInputStream fis = new FileInputStream(file)){
                properties.load(fis);
                logger.info("load cache urls from cache file " + file.getName());
            } catch (IOException e) {
                logger.warn("cannot load cache information from file " + file.getPath());
            }
        }
    }

    @Override
    public URL getUrl() {
        return registryUrl;
    }

    @Override
    public void register(URL url) {
        if (destroyed.get())
            return;
        Assert.notNull(url, "url cannot be null when register an url.");
        logger.info("registered " + url.toFullString());
        registered.add(url);
    }

    @Override
    public void unregister(URL url) {
        if (destroyed.get())
            return;
        Assert.notNull(url, "url cannot be null when unregister an url.");
        logger.info("unregistered " + url.toFullString());
        registered.remove(url);
    }

    @Override
    public void subscribe(URL url, NotifyListener listener) {
        if (destroyed.get())
            return;
        if (listener == null)
            throw new IllegalArgumentException("listener == null.");
        if (url == null)
            throw new IllegalArgumentException("url == null.");
        Set<NotifyListener> listeners = subscribed.get(url);
        if (listeners == null){
            subscribed.put(url, new CopyOnWriteArraySet<>());
            listeners = subscribed.get(url);
        }
        listeners.add(listener);
    }

    @Override
    public void unsubscribe(URL url, NotifyListener listener) {
        if (destroyed.get())
            return;
        if (listener == null)
            throw new IllegalArgumentException("listener == null.");
        if (url == null)
            throw new IllegalArgumentException("url == null.");
        Set<NotifyListener> listeners = subscribed.get(url);
        if (listeners == null){
            subscribed.put(url, new CopyOnWriteArraySet<>());
            listeners = subscribed.get(url);
        }
        listeners.remove(listener);
    }

    public void notify(URL url, NotifyListener listener, List<URL> urls){
        if (destroyed.get())
            return;
        if (listener == null)
            throw new IllegalArgumentException("listener == null.");
        if (url == null)
            throw new IllegalArgumentException("url == null.");
        if (urls == null || urls.size() == 0){
            logger.info("provider update is ignored.");
            return;
        }
        List<URL> result = new ArrayList<>();
        for (URL u : urls) {
            String providerServiceName = u.getServiceName();
            String consumerServiceName = url.getServiceName();
            if (StringUtils.equals(providerServiceName, consumerServiceName)){
                result.add(u);
            }
        }

        // 每当注册中心的提供者 url 发生了改变时，就会从注册中心获取到最新的提供者的全量数据，并将其保存到内存中的
        // properties 对象里面
        saveCacheUrls(url, result);
        listener.notify(result);
    }

    private void saveCacheUrls(URL url, List<URL> invokerUrls){
        if (url == null){
            throw new IllegalArgumentException("url == null.");
        }
        if (invokerUrls == null || invokerUrls.size() == 0)
            return;

        StringBuilder cache = new StringBuilder();
        for (int i = 0; i < invokerUrls.size(); i++) {
            if (i > 0)
                cache.append(RpcConfig.HEX_SEPARATOR);
            cache.append(invokerUrls.get(i).toFullString());
        }

        // properties 里面以服务名作为键，并且以各个提供者的 url 字符串为值，并且各个 url 之间使用 # 作为分隔
        properties.setProperty(url.getServiceName(), cache.toString());
        cacheExecutor.submit(new SaveCacheTask(cacheVersion.incrementAndGet(), 0));
    }

    private void doSaveCacheUrls(long version, int retries) {
        // 如果此 task 中缓存的版本小于缓存的最新版本，或者重试次数达到了上限，那么直接返回
        if (version < cacheVersion.get() || retries >= MAX_SAVE_CACHE_RETRY_TIMES){
            return;
        }

        if (file == null)
            return;

        String path = file.getAbsolutePath();
        synchronized (path.intern()){
            try{
                if (!file.exists() && !file.createNewFile()) {
                    throw new IllegalStateException("cannot create file " + file.getName() + ", waiting for retry later.");
                }

                try(FileOutputStream fos = new FileOutputStream(file)){
                    // 将内存中 properties 对象中所保存的缓存 url 写入到磁盘里面
                    properties.store(fos, "SaveCacheUrls");
                    logger.info("save new urls to cache file " + file.getName() + ", version " + version);
                }catch (IOException e){
                    throw e;
                }
            }catch (Throwable t){
                if (version < cacheVersion.get())
                    return;

                logger.warn("cannot save cached urls to file " + file.getName() + " waiting for retry later.");
                cacheExecutor.submit(new SaveCacheTask(cacheVersion.incrementAndGet(), ++retries));
            }

        }
    }


    public List<URL> getRegistered() {
        return registered;
    }

    public Map<URL, Set<NotifyListener>> getSubscribed() {
        return subscribed;
    }

    public void destroy(){
        if (!destroyed.compareAndSet(false, true)){
            return;
        }

        logger.info("close connection to registry: " + registryUrl);

        // 取消注册在注册中心上的节点
        ArrayList<URL> registered = new ArrayList<>(getRegistered());
        if (registered.size() > 0){
            for (URL url : registered) {
                try {
                    unregister(url);
                    logger.info("unregister url " + url);
                } catch (Exception e) {
                    logger.error("failed to destroy url " + url + " , caused by " + e.getMessage());
                }
            }
        }

        // 取消注册在注册中心上的监听器
        HashMap<URL, Set<NotifyListener>> subscribed = new HashMap<>(getSubscribed());
        for (Map.Entry<URL, Set<NotifyListener>> entry : subscribed.entrySet()) {
            URL url = entry.getKey();
            Set<NotifyListener> listeners = entry.getValue();
            try {
                for (NotifyListener listener : listeners) {
                    unsubscribe(url, listener);
                    logger.info("unregister the listener for url " + url);
                }
            } catch (Exception e) {
                logger.error("failed to destroy the listener for url " + url + " , caused  by " + e.getMessage());
            }
        }
    }
    
    public List<URL> getCachedUrls(URL url){
        for (Map.Entry<Object, Object> entry : properties.entrySet()) {
            String key = (String) entry.getKey();
            String value = (String) entry.getValue();
            if (key != null && key.length() > 0 && url.getServiceName().equalsIgnoreCase(key)
                    && value != null && value.length() > 0){
                String[] us = value.split(RpcConfig.HEX_SEPARATOR);
                List<URL> urls = new ArrayList<>();
                for (String u : us) {
                    urls.add(URL.valueOf(u));
                }
                return urls;
            }
        }
        return null;
    }

    private class SaveCacheTask implements Runnable{

        private final int retries;

        private final long version;

        public SaveCacheTask(long version, int retries){
            this.version = version;
            this.retries = retries;
        }

        @Override
        public void run() {
            doSaveCacheUrls(version, retries);
        }
    }
}
