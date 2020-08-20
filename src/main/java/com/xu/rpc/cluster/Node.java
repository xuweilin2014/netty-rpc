package com.xu.rpc.cluster;

import com.xu.rpc.util.URL;

public interface Node {

    public URL getURL();

    public Class<?> getInterface();

    public boolean isAvailable();

    public void destroy();

}
