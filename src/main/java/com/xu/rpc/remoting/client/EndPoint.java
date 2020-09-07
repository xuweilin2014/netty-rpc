package com.xu.rpc.remoting.client;

import com.xu.rpc.commons.URL;

public interface EndPoint {

    public void close(int timeout);

    public boolean isClosed();

    public URL getUrl();

}
