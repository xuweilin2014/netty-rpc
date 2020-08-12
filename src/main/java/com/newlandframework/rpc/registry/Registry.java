package com.newlandframework.rpc.registry;

import com.newlandframework.rpc.util.URL;

public interface Registry {

    public void register(URL url);

    public void unregister(URL url);

    public void subscribe(URL url);

    public void unsubscribe(URL url);

}
