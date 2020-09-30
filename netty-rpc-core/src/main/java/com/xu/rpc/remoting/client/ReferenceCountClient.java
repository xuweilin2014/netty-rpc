package com.xu.rpc.remoting.client;

import com.xu.rpc.commons.exception.RemotingException;
import com.xu.rpc.async.RpcFuture;
import com.xu.rpc.commons.URL;

import java.util.concurrent.atomic.AtomicInteger;

/**
 * 含有 HeaderExchangeClient 对象，并且在其基础上增加了一个引用计数的功能。这是因为客户端采用共享连接的模式，
 * 对于同一个服务端地址，使用同一个连接，这也就意味着多个客户端会使用同一个连接，因此当一个客户端关闭连接时，
 * 如果还有其它客户端来使用，不能真的将其关闭，只能计数器减一
 */
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
