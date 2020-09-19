package com.xu.rpc.protocol;

import com.xu.rpc.commons.URL;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;
import com.xu.rpc.exception.RpcException;
import java.util.concurrent.atomic.AtomicBoolean;

public abstract class AbstractInvoker<T> implements Invoker<T> {

    private Class<?> type;

    private URL url;

    private AtomicBoolean destroyed = new AtomicBoolean(false);

    public AbstractInvoker(Class<?> type, URL url) {
        this.type = type;
        this.url = url;
    }

    @Override
    public URL getUrl() {
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
            // 调用链为 AbstractInvoker -> RpcInvoker -> HeaderExchangeClient -> NettyClient
            // 在后面 3 个的调用中，直接抛出任何异常，即使在 catch 语句块汇总还是会抛出异常
            return new RpcResult(e);
        }

    }

    public abstract RpcResult doInvoke(RpcInvocation invocation) throws RpcException;

}
