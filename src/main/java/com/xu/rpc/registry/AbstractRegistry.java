package com.xu.rpc.registry;

import com.xu.rpc.commons.Assert;
import com.xu.rpc.commons.URL;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicBoolean;

public abstract class AbstractRegistry implements Registry {

    protected static Logger logger = Logger.getLogger(AbstractRegistry.class);

    private List<URL> registered = new CopyOnWriteArrayList<>();

    private Map<URL,NotifyListener> subscribed = new ConcurrentHashMap<>();

    private Map<URL, List<URL>> notified = new ConcurrentHashMap<>();

    private AtomicBoolean destroyed = new AtomicBoolean(false);

    private URL registryURL;

    public AbstractRegistry(URL url){
        this.registryURL = url;
    }

    @Override
    public URL getURL() {
        return registryURL;
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
        subscribed.put(url, listener);
    }

    @Override
    public void unsubscribe(URL url, NotifyListener listener) {
        if (destroyed.get())
            return;
        if (listener == null)
            throw new IllegalArgumentException("listener == null.");
        if (url == null)
            throw new IllegalArgumentException("url == null.");
        subscribed.remove(url);
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
        notified.put(url, result);
        listener.notify(result);
    }

    public List<URL> getRegistered() {
        return registered;
    }

    public Map<URL, NotifyListener> getSubscribed() {
        return subscribed;
    }

    public void destroy(){
        if (!destroyed.compareAndSet(false, true)){
            return;
        }

        logger.info("destroy the registry for url " + registryURL);

        ArrayList<URL> registered = new ArrayList<>(getRegistered());
        if (registered.size() > 0){
            for (URL url : registered) {
                try {
                    unregister(url);
                    logger.info("destroy url " + url);
                } catch (Exception e) {
                    logger.error("failed to destroy url " + url + " , caused by " + e.getMessage());
                }
            }
        }

        HashMap<URL, NotifyListener> subscribed = new HashMap<>(getSubscribed());
        for (Map.Entry<URL, NotifyListener> entry : subscribed.entrySet()) {
            URL url = entry.getKey();
            NotifyListener listener = entry.getValue();
            try {
                unsubscribe(url, listener);
                logger.info("destroy the listener for url " + url);
            } catch (Exception e) {
                logger.error("failed to destroy the listener for url " + url + " , caused  by " + e.getMessage());
            }
        }
    }
}
