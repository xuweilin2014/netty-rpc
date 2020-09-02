package com.xu.rpc.cluster;

import com.xu.rpc.util.URL;

public interface Node {

    public URL getUrl();

    public boolean isAvailable();

    public void destroy();

}
