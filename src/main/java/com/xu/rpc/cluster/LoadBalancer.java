package com.xu.rpc.cluster;

import com.xu.rpc.cluster.loadbalance.RandomLoadBalancer;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.extension.Extension;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.util.URL;

import java.util.List;

@Extension(RandomLoadBalancer.NAME)
public interface LoadBalancer {

    public Invoker select(RpcInvocation invocation, List<Invoker> invokers, URL url) throws RpcException;

}
