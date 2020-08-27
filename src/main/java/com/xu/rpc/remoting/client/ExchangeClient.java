package com.xu.rpc.remoting.client;

import com.xu.rpc.async.ResponseFuture;
import com.xu.rpc.exception.RemotingException;
import com.xu.rpc.util.URL;

public interface ExchangeClient {

    public void close();

    public void close(int timeout);

    public boolean isClosed();

    public URL getUrl();

    public ResponseFuture request(Object request, int timeout) throws RemotingException;

    public ResponseFuture request(Object request) throws RemotingException;

}
