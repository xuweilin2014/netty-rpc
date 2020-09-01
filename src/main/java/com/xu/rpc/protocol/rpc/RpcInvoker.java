package com.xu.rpc.protocol.rpc;

import com.xu.rpc.async.FutureWrapper;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.RpcContext;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;
import com.xu.rpc.exception.RemotingException;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.exception.RpcTimeoutException;
import com.xu.rpc.protocol.AbstractInvoker;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.remoting.client.ExchangeClient;
import com.xu.rpc.util.URL;

import java.util.Set;
import java.util.concurrent.Future;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * RpcInvoker#invoke方法通过向服务器发起远程调用
 */
public class RpcInvoker extends AbstractInvoker {

    private final Set<Invoker> invokers;

    private final ExchangeClient client;

    private final Lock lock = new ReentrantLock();

    public RpcInvoker(Class<?> type, URL url, Set<Invoker> invokers, ExchangeClient client) {
        super(type, url);
        this.invokers = invokers;
        this.client = client;
    }

    @Override
    public RpcResult doInvoke(RpcInvocation invocation) throws RpcException {
        int timeout = getURL().getParameter(RpcConfig.TIMEOUT_KEY, RpcConfig.DEFAULT_TIMEOUT);
        boolean isAsync = getURL().getParameter(RpcConfig.ASYNC_KEY, false);

        try{
            // 如果为异步调用的话
            if (isAsync){
                Future<?> future = new FutureWrapper(client.request(invocation, timeout));
                RpcContext.getContext().setFuture(future);
                // 返回一个空的结果
                return new RpcResult();
            // 如果为同步调用的话
            }else {
                RpcContext.getContext().setFuture(null);
                Object result = client.request(invocation, timeout).get();
                return new RpcResult(result);
            }
        }catch (RpcTimeoutException ex){
            throw new RpcException("invoke method " + invocation.getMethodName() + " timeout, caused by " + ex.getMessage());
        }catch (RemotingException ex){
            throw new RpcException("failed to invoke the remote method " + invocation.getMethodName() + ", caused by " + ex.getMessage());
        } catch (InterruptedException e) {
            throw new RpcException("failed to invoke the remote method" + invocation.getMethodName() + " caused by interruption.");
        }
    }

    @Override
    public boolean isAvailable() {
        if (isDestroyed())
            return false;
        return client.isConnected();
    }

    @Override
    public void destroy() {
        if (isDestroyed())
            return;
        lock.lock();
        try{
            if (isDestroyed())
                return;
            super.destroy();
            invokers.remove(this);
            client.close();
        }finally {
            lock.unlock();
        }
    }
}

