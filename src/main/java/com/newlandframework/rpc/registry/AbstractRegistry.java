package com.newlandframework.rpc.registry;

import com.newlandframework.rpc.util.Assert;
import com.newlandframework.rpc.util.URL;
import org.apache.log4j.Logger;

import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;

public abstract class AbstractRegistry implements Registry {

    protected static Logger logger = Logger.getLogger(AbstractRegistry.class);

    private List<URL> registeredURL = new CopyOnWriteArrayList<>();

    private ConcurrentHashMap<URL, NotifyListener> subscribedURL = new ConcurrentHashMap<>();

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
    public void subscribe(URL url) {
        // TODO: 2020/8/12  
    }

    @Override
    public void unsubscribe(URL url) {
        // TODO: 2020/8/12  
    }

    public List<URL> getRegisteredURL() {
        return registeredURL;
    }

    public ConcurrentHashMap<URL, NotifyListener> getSubscribedURL() {
        return subscribedURL;
    }

    public void destroy(){
        // TODO: 2020/8/12  
    }
}
