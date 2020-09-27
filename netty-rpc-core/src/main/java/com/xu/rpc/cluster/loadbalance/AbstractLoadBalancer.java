package com.xu.rpc.cluster.loadbalance;

import com.xu.rpc.commons.URL;
import com.xu.rpc.commons.exception.RpcException;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.cluster.LoadBalancer;
import com.xu.rpc.core.RpcInvocation;

import java.util.List;

public abstract class AbstractLoadBalancer implements LoadBalancer {

    @Override
    public Invoker select(RpcInvocation invocation, List<Invoker> invokers, URL url) throws RpcException {
        if (invokers == null || invokers.size() == 0)
            return null;

        if (invokers.size() == 1)
            return invokers.get(0);

        return doSelect(invocation, invokers, url);
    }

    public abstract Invoker doSelect(RpcInvocation invocation, List<Invoker> invokers, URL url) throws RpcException;
}
