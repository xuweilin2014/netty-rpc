package com.xu.rpc.cluster.loadbalance;

import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.commons.URL;

import java.util.List;

public class ConsistentHashLoadBalancer extends AbstractLoadBalancer{

    public static final String NAME = "hash";

    @Override
    public Invoker doSelect(RpcInvocation invocation, List<Invoker> invokers, URL url) throws RpcException {
        // TODO: 2020/8/30
        return null;
    }
}
