package com.xu.rpc.remoting.client;

import com.xu.rpc.util.URL;

public interface EndPoint {

    public void close();

    public void close(int timeout);

    public boolean isClosed();

    public URL getUrl();

    public boolean isConnected();

}
