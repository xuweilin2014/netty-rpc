package com.xu.rpc.registry;

import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.parallel.NamedThreadFactory;
import com.xu.rpc.commons.URL;
import io.netty.util.internal.ConcurrentSet;

import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicBoolean;

// 失败重试
public abstract class FailbackRegistry extends AbstractRegistry{

    private List<URL> failedRegisteredURLs = new CopyOnWriteArrayList<>();

    private List<URL> failedUnregisteredURLs = new CopyOnWriteArrayList<>();

    private Map<URL, Set<NotifyListener>> failedSubscribedListeners = new ConcurrentHashMap<>();

    private Map<URL, Set<NotifyListener>> failedUnsubscribedListeners = new ConcurrentHashMap<>();

    private Map<URL, Map<NotifyListener, List<URL>>> failedNotified = new ConcurrentHashMap<>();

    private AtomicBoolean destroyed = new AtomicBoolean(false);

    private static ScheduledExecutorService retryExecutor = Executors.newScheduledThreadPool(1, new NamedThreadFactory("ZookeeperRetryThread", true));

    private ScheduledFuture<?> retryFuture;

    public FailbackRegistry(URL url){
        super(url);
        int retryTime = RpcConfig.RETRY_PERIOD;
        retryFuture = retryExecutor.scheduleWithFixedDelay(new Runnable() {
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
            logger.error("failed to register " + url.toFullString() + ", retry will start soon.");
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
            logger.error("failed to unregister " + url.toFullString() + ", retry will start soon.");
            failedUnregisteredURLs.add(url);
        }
    }

    @Override
    public void subscribe(URL url, NotifyListener listener) {
        if (destroyed.get()){
            return;
        }
        super.subscribe(url, listener);
        // 如果是重新注册监听器的话，就先移除掉这个 listener
        removeFailedSubscribed(url, listener);

        try {
            doSubscribe(url, listener);
        } catch (Exception e) {
            // 当向注册中心上注册监听器失败时，就从磁盘上获取缓存的提供者 urls，然后执行 notify 方法，并且把此 listener
            // 添加到 failedSubscribedListeners 中，等待随后进行重试，重新注册
            List<URL> cachedUrls = getCachedUrls(url);
            if (cachedUrls != null && cachedUrls.size() > 0){
                notify(url, listener, cachedUrls);
            }

            logger.error("failed to subscribe url " + url + " waiting for retry later, caused by " + e.getMessage());
            addFailedSubscribed(url, listener);
        }
    }

    private void addFailedSubscribed(URL url, NotifyListener listener){
        Set<NotifyListener> listeners = failedSubscribedListeners.get(url);
        if (listeners == null){
            failedSubscribedListeners.put(url, new ConcurrentSet<>());
            listeners = failedSubscribedListeners.get(url);
        }
        listeners.add(listener);
    }

    private void removeFailedSubscribed(URL url, NotifyListener listener){
        Set<NotifyListener> listeners = failedSubscribedListeners.get(url);
        if (listeners != null) {
            listeners.remove(listener);
        }
        listeners = failedUnsubscribedListeners.get(url);
        if (listeners != null) {
            listeners.remove(listener);
        }
        Map<NotifyListener, List<URL>> listenerListMap = failedNotified.get(url);
        if (listenerListMap != null) {
            listenerListMap.remove(listener);
        }
    }

    @Override
    public void unsubscribe(URL url, NotifyListener listener) {
        if (destroyed.get()){
            return;
        }
        super.unsubscribe(url, listener);
        removeFailedSubscribed(url, listener);

        try {
            doUnsubscribe(url, listener);
        } catch (Exception e) {
            // 取消注册失败，等待之后进行重试
            logger.error("failed to unsubscribe url " + url + " waiting for retry later, caused by " + e.getMessage());
            Set<NotifyListener> listeners = failedUnsubscribedListeners.get(url);
            if (listeners == null){
                failedUnsubscribedListeners.put(url, new ConcurrentSet<>());
                listeners = failedUnsubscribedListeners.get(url);
            }
            listeners.add(listener);
        }
    }

    @Override
    public void notify(URL url, NotifyListener listener, List<URL> urls){
        try {
            doNotify(url, listener, urls);
        } catch (Exception e) {
            logger.error("failed to notify for subscribe url " + url + " waiting for retry later, caused by " + e.getMessage());
            Map<NotifyListener, List<URL>> map = failedNotified.get(url);
            if (map == null){
                failedNotified.put(url, new ConcurrentHashMap<>());
                map = failedNotified.get(url);
            }
            map.put(listener, urls);
        }
    }

    public void doNotify(URL url, NotifyListener listener, List<URL> urls){
        super.notify(url, listener, urls);
    }

    private void retry(){
        if (destroyed.get()){
            return;
        }

        // 处理注册失败的 url，进行重新注册
        if (failedRegisteredURLs.size() > 0){
            ArrayList<URL> failedRegistered = new ArrayList<>(failedRegisteredURLs);
            for (URL url : failedRegistered) {
                try{
                    doRegister(url);
                    failedRegistered.remove(url);
                }catch (Throwable t){
                    logger.error("failed to register " + url.toFullString() + " again, waiting for another retry.");
                }
            }
        }

        // 处理取消注册失败的 url，进行重新取消注册
        if (failedUnregisteredURLs.size() > 0){
            ArrayList<URL> failedRegistered = new ArrayList<>(failedUnregisteredURLs);
            for (URL url : failedRegistered) {
                try{
                    doUnregister(url);
                    failedRegistered.remove(url);
                }catch (Throwable t){
                    logger.error("failed to unregister " + url.toFullString() + " again, waiting for another retry.");
                }
            }
        }

        if (failedSubscribedListeners.size() > 0) {
            HashMap<URL, Set<NotifyListener>> subscribed = new HashMap<>(failedSubscribedListeners);
            for (Map.Entry<URL, Set<NotifyListener>> entry : subscribed.entrySet()) {
                URL url = entry.getKey();
                Set<NotifyListener> listeners = entry.getValue();
                for (NotifyListener listener : listeners) {
                    try {
                        doSubscribe(url, listener);
                        listeners.remove(listener);
                    } catch (Throwable t) {
                        logger.error("failed to subscribe " + url + " again, waiting for another retry.");
                    }
                }
            }
        }

        if (failedUnsubscribedListeners.size() > 0){
            HashMap<URL, Set<NotifyListener>> unsubscribed = new HashMap<>(failedUnsubscribedListeners);
            for (Map.Entry<URL, Set<NotifyListener>> entry : unsubscribed.entrySet()) {
                URL url = entry.getKey();
                Set<NotifyListener> listeners = entry.getValue();
                for (NotifyListener listener : listeners) {
                    try{
                        doUnsubscribe(url, listener);
                        listeners.remove(listener);
                    }catch (Throwable t){
                        logger.error("failed to unsubscribe " + url + " again, waiting for another retry.");
                    }
                }
            }
        }

        if (failedNotified.size() > 0){
            HashMap<URL, Map<NotifyListener,List<URL>>> map = new HashMap<>(failedNotified);
            if (!map.isEmpty()){
                for (Map<NotifyListener, List<URL>> listenerUrlMap : map.values()) {
                    for (Map.Entry<NotifyListener, List<URL>> entry : listenerUrlMap.entrySet()) {
                        NotifyListener listener = entry.getKey();
                        List<URL> urls = entry.getValue();
                        try{
                            listener.notify(urls);
                            listenerUrlMap.remove(listener);
                        }catch (Throwable t){
                            logger.error("failed to notify " + urls + " again, waiting for another retry.");
                        }
                    }
                }
            }
        }
    }

    // 当重新开启一个新的会话时，重新注册先前会话中的临时节点与监听器
    public void recover(){
        if (destroyed.get()){
            return;
        }

        List<URL> registered = new ArrayList<>(getRegistered());
        for (URL url : registered) {
            logger.info("re-register " + url.toFullString());
            failedRegisteredURLs.add(url);
        }

        Map<URL, Set<NotifyListener>> subscribed = new HashMap<>(getSubscribed());
        for (Map.Entry<URL, Set<NotifyListener>> entry : subscribed.entrySet()) {
            URL url = entry.getKey();
            Set<NotifyListener> listeners = entry.getValue();
            logger.info("re-subscribe for url " + url);
            for (NotifyListener listener : listeners) {
                addFailedSubscribed(url, listener);
            }
        }
    }

    public void destroy(){
        if (destroyed.compareAndSet(false, true)){
            super.destroy();
            try {
                retryFuture.cancel(true);
            } catch (Exception e) {
                logger.error("failed to cancel retryFuture, caused by " + e.getMessage());
            }
        }
    }

    public abstract void doRegister(URL url);

    public abstract void doUnregister(URL url);

    public abstract void doSubscribe(URL url, NotifyListener listener);

    public abstract void doUnsubscribe(URL url, NotifyListener listener);
}
