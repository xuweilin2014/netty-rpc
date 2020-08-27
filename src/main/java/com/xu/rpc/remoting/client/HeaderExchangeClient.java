package com.xu.rpc.remoting.client;

import com.xu.rpc.async.ResponseFuture;
import com.xu.rpc.exception.RemotingException;
import com.xu.rpc.util.URL;

public class HeaderExchangeClient implements ExchangeClient {

    public HeaderExchangeClient(Client client) {
    }

    @Override
    public void close() {

    }

    @Override
    public void close(int timeout) {

    }

    @Override
    public boolean isClosed() {
        return false;
    }

    @Override
    public URL getUrl() {
        return null;
    }

    @Override
    public ResponseFuture request(Object request, int timeout) throws RemotingException {
        return null;
    }

    @Override
    public ResponseFuture request(Object request) throws RemotingException {
        return null;
    }

}
