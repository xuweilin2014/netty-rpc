package com.xu.rpc.remoting.client;

import com.xu.rpc.commons.URL;
import com.xu.rpc.exception.RemotingException;

public interface EndPoint {

    public void close(int timeout);

    public boolean isClosed();

    public URL getUrl();

}
