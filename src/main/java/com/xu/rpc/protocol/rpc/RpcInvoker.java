package com.xu.rpc.protocol.rpc;

import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.model.MessageRequest;
import com.xu.rpc.protocol.AbstractInvoker;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.remoting.client.ExchangeClient;
import com.xu.rpc.util.URL;

import java.util.Set;

/**
 * RpcInvoker#invoke方法通过向服务器发起远程调用
 */
public class RpcInvoker extends AbstractInvoker {


    private Set<Invoker> invokers;

    private ExchangeClient client;

    public RpcInvoker(Class<?> type, URL url, Set<Invoker> invokers, ExchangeClient client) {
        super(type, url);
        this.invokers = invokers;
        this.client = client;
    }

    @Override
    public Object doInvoke(RpcInvocation invocation) throws RpcException {
        return null;
    }

    @Override
    public boolean isAvailable() {
        return false;
    }

    @Override
    public void destroy() {

    }
}

