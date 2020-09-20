package com.xu.rpc.remoting.client;

import com.xu.rpc.async.RpcFuture;
import com.xu.rpc.exception.RemotingException;
import com.xu.rpc.commons.URL;

import java.util.concurrent.atomic.AtomicInteger;

public class ReferenceCountClient implements ExchangeClient{

    private URL url;

    private AtomicInteger referenceCount = new AtomicInteger(0);

    private ExchangeClient client;

    public ReferenceCountClient(URL url, ExchangeClient client) {
        this.url = url;
        this.client = client;
        referenceCount.incrementAndGet();
    }

    public void incrementAndGet(){
        referenceCount.incrementAndGet();
    }

    public void close(){
        close(0);
    }

    public void close(int timeout){
        if (referenceCount.decrementAndGet() <= 0){
            if (timeout == 0){
                client.close(0);
            }else {
                client.close(timeout);
            }
        }
    }

    @Override
    public boolean isClosed() {
        return client.isClosed();
    }

    public URL getUrl() {
        return url;
    }

    @Override
    public boolean isConnected() {
        return client.isConnected();
    }

    @Override
    public RpcFuture request(Object request, int timeout) throws RemotingException {
        return client.request(request, timeout);
    }

    @Override
    public RpcFuture request(Object request) throws RemotingException {
        return client.request(request);
    }
}
