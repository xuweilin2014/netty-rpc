package com.xu.rpc.registry;

import com.xu.rpc.util.Assert;
import com.xu.rpc.util.URL;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;

public abstract class AbstractRegistry implements Registry {

    protected static Logger logger = Logger.getLogger(AbstractRegistry.class);

    private List<URL> registeredURL = new CopyOnWriteArrayList<>();

    private Map<URL,NotifyListener> subscribed = new ConcurrentHashMap<>();

    private Map<URL, List<URL>> notified = new ConcurrentHashMap<>();

    @Override
    public void register(URL url) {
        Assert.notNull(url, "url cannot be null when register an url.");
        logger.info("registered " + url.toFullString());
        registeredURL.add(url);
    }

    @Override
    public void unregister(URL url) {
        Assert.notNull(url, "url cannot be null when unregister an url.");
        logger.info("unregistered " + url.toFullString());
        registeredURL.remove(url);
    }

    @Override
    public void subscribe(URL url, NotifyListener listener) {
        if (listener == null)
            throw new IllegalArgumentException("listener == null.");
        if (url == null)
            throw new IllegalArgumentException("url == null.");
        subscribed.put(url, listener);
    }

    @Override
    public void unsubscribe(URL url, NotifyListener listener) {
        if (listener == null)
            throw new IllegalArgumentException("listener == null.");
        if (url == null)
            throw new IllegalArgumentException("url == null.");
        subscribed.remove(url);
    }

    public void notify(URL url, NotifyListener listener, List<URL> urls){
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
        notified.put(url, result);
        listener.notify(result);
    }

    public List<URL> getRegisteredURL() {
        return registeredURL;
    }

    public Map<URL, NotifyListener> getSubscribed() {
        return subscribed;
    }

    public void destroy(){
        // TODO: 2020/8/12  
    }
}
