package com.xu.rpc.registry;

import com.xu.rpc.util.URL;

public interface Registry {

    public void register(URL url);

    public void unregister(URL url);

    public void subscribe(URL url, NotifyListener listener);

    public void unsubscribe(URL url, NotifyListener listener);

}
