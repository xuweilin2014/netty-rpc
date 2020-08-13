package com.newlandframework.rpc.registry;

import com.newlandframework.rpc.core.RpcSystemConfig;
import com.newlandframework.rpc.parallel.NamedThreadFactory;
import com.newlandframework.rpc.util.URL;
import org.apache.log4j.Logger;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicBoolean;

// 失败重试
public abstract class FailbackRegistry extends AbstractRegistry{

    private List<URL> failedRegisteredURLs = new CopyOnWriteArrayList<>();

    private List<URL> failedUnregisteredURLs = new CopyOnWriteArrayList<>();

    private ConcurrentHashMap<URL, NotifyListener> failedSubscribedListeners = new ConcurrentHashMap<>();

    private ConcurrentHashMap<URL, NotifyListener> failedUnsubscribedListeners = new ConcurrentHashMap<>();

    private AtomicBoolean destroyed = new AtomicBoolean(false);

    private static ScheduledExecutorService retryExecutor = Executors.newScheduledThreadPool(1, new NamedThreadFactory("ZookeeperRetryThread", true));

    public FailbackRegistry(){
        int retryTime = RpcSystemConfig.RETRY_PERIOD;
        retryExecutor.scheduleWithFixedDelay(new Runnable() {
            @Override
            public void run() {
                retry();
            }
        }, retryTime, retryTime, TimeUnit.MILLISECONDS);
    }

    @Override
    public void register(URL url) {
        // 一个 Registry 注册中心的对象可能会被多个线程同时使用，所以有可能一个线程会关闭掉这个 Registry 对象，
        // 因此使用之前，必须先检查 destroyed 值
        if (destroyed.get()){
            return;
        }
        super.register(url);
        failedRegisteredURLs.remove(url);
        failedUnregisteredURLs.remove(url);
        try{
            doRegister(url);
        }catch (Throwable t){
            logger.warn("failed to register " + url.toFullString() + ", retry will start soon.");
            failedRegisteredURLs.add(url);
        }
    }

    @Override
    public void unregister(URL url) {
        if (destroyed.get()){
            return;
        }
        // 调用父类的 unregister，移除掉这个 url
        super.unregister(url);
        failedRegisteredURLs.remove(url);
        failedUnregisteredURLs.remove(url);
        try{
            doUnregister(url);
        }catch (Throwable t){
            logger.warn("failed to unregister " + url.toFullString() + ", retry will start soon.");
            failedUnregisteredURLs.add(url);
        }
    }

    @Override
    public void subscribe(URL url) {
        super.subscribe(url);
    }

    @Override
    public void unsubscribe(URL url) {
        super.unsubscribe(url);
    }

    private void retry(){
        if (destroyed.get()){
            return;
        }

        // 处理注册失败的 url，进行重新注册
        if (failedRegisteredURLs.size() > 0){
            try{
                for (URL url : failedRegisteredURLs) {
                    try{
                        doRegister(url);
                        failedRegisteredURLs.remove(url);
                    }catch (Throwable t){
                        logger.warn("failed to register " + url.toFullString() + " again, waiting for another retry.");
                    }
                }
            }catch (Throwable t){
                logger.warn("failed to register " + failedRegisteredURLs + " again, waiting for another retry.");
            }
        }

        // 处理取消注册失败的 url，进行重新取消注册
        if (failedUnregisteredURLs.size() > 0){
            try{
                for (URL url : failedUnregisteredURLs) {
                    try{
                        doUnregister(url);
                        failedUnregisteredURLs.remove(url);
                    }catch (Throwable t){
                        logger.warn("failed to unregister " + url.toFullString() + " again, waiting for another retry.");
                    }
                }
            }catch (Throwable t){
                logger.warn("failed to unregister " + failedUnregisteredURLs + " again, waiting for another retry.");
            }
        }

        // TODO: 2020/8/12
    }

    public void recover(){
        if (destroyed.get()){
            return;
        }
        List<URL> registeredURL = new ArrayList<>(getRegisteredURL());
        for (URL url : registeredURL) {
            logger.info("re-register " + url.toFullString());
            failedRegisteredURLs.add(url);
        }

        // TODO: 2020/8/12 对监听器进行重新注册
    }

    public void destroy(){
        // TODO: 2020/8/12
    }

    public abstract void doRegister(URL url);

    public abstract void doUnregister(URL url);

    public abstract void doSubscribe(URL url);

    public abstract void doUnsubscribe(URL url);
}
