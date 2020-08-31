package com.xu.rpc.protocol;

import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.model.MessageRequest;
import com.xu.rpc.remoting.client.ExchangeClient;
import com.xu.rpc.util.URL;

import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

public abstract class AbstractInvoker implements Invoker {

    private Class<?> type;

    private URL url;

    private AtomicBoolean destroyed = new AtomicBoolean(false);

    public AbstractInvoker(Class<?> type, URL url) {
        this.type = type;
        this.url = url;
    }

    @Override
    public URL getURL() {
        return url;
    }

    @Override
    public Class<?> getInterface() {
        return type;
    }

    @Override
    public void destroy() {
        destroyed.compareAndSet(false, true);
    }

    public boolean isDestroyed(){
        return destroyed.get();
    }

    @Override
    public RpcResult invoke(RpcInvocation invocation) throws RpcException {
        try{
            return doInvoke(invocation);
        } catch (Throwable e){
            return new RpcResult(e);
        }

    }

    public abstract RpcResult doInvoke(RpcInvocation invocation) throws RpcException;

}
