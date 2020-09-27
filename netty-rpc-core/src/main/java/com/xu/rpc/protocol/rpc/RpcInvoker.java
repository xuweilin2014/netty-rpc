package com.xu.rpc.protocol.rpc;

import com.xu.rpc.async.FutureWrapper;
import com.xu.rpc.commons.URL;
import com.xu.rpc.commons.exception.RemotingException;
import com.xu.rpc.commons.exception.RpcException;
import com.xu.rpc.commons.exception.RpcTimeoutException;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.RpcContext;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;
import com.xu.rpc.protocol.AbstractInvoker;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.remoting.client.ExchangeClient;

import java.util.Set;
import java.util.concurrent.Future;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * RpcInvoker#invoke方法通过向服务器发起远程调用
 */
public class RpcInvoker<T> extends AbstractInvoker<T> {

    private Set<Invoker> invokers;

    private ExchangeClient client;

    private final Lock lock = new ReentrantLock();

    public RpcInvoker(Class<?> type, URL url, Set<Invoker> invokers, ExchangeClient client) {
        super(type, url);
        this.invokers = invokers;
        this.client = client;
    }

    @Override
    public RpcResult doInvoke(RpcInvocation invocation) throws RpcException {
        int timeout = getUrl().getParameter(RpcConfig.TIMEOUT_KEY, RpcConfig.DEFAULT_TIMEOUT);
        String isAsync = invocation.getAttachments().get(RpcConfig.ASYNC_KEY);
        String oneWay = invocation.getAttachments().get(RpcConfig.ONE_WAY_KEY);
        String token = getUrl().getParameter(RpcConfig.TOKEN_KEY);

        // 在向服务器发送请求之前，先检查服务器是否开启了 token 验证，如果开启了的话，就把 token 放入到
        // RpcInvocation 中保存起来
        if (token != null && token.length() != 0){
            invocation.getAttachments().put(RpcConfig.TOKEN_KEY, token);
        }

        try{
            // oneWay 为 true，表明方法没有返回值，所以发送完毕之后，不用等待直接返回一个空结果（会被忽略）
            if (RpcConfig.TRUE.equals(oneWay)){
                RpcContext.getContext().setFuture(null);
                client.request(invocation, timeout);
                return new RpcResult();
            // 如果为异步调用的话
            }else if (RpcConfig.TRUE.equals(isAsync)){
                Future<?> future = new FutureWrapper(client.request(invocation, timeout));
                RpcContext.getContext().setFuture(future);
                // 返回一个空的结果
                return new RpcResult();
            // 如果为同步调用的话
            }else {
                RpcContext.getContext().setFuture(null);
                // 阻塞直到结果返回
                Object result = client.request(invocation, timeout).get();
                return new RpcResult(result);
            }
        }catch (RpcTimeoutException ex){
            throw new RpcException("invoke service: " + invocation.getServiceType().getName() + " method: " + invocation.getMethodName() + " timeout");
        }catch (RemotingException ex){
            throw new RpcException("failed to invoke the service: " + invocation.getServiceType().getName() + " remote method: " + invocation.getMethodName() + " caused by " + ex
            .getMessage());
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
            client.close(RpcConfig.DEFAULT_TIMEOUT);
        }finally {
            lock.unlock();
        }
    }

    @Override
    public int hashCode() {
        int result = 0;
        result = 31 * result + (client != null ? client.hashCode() : 0);
        result = 31 * result + (getInterface() != null ? getInterface().hashCode() : 0);
        result = 31 * result + (getUrl() != null ? getUrl().hashCode() : 0);
        return result;
    }
}

