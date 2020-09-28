package com.xu.rpc.cluster.support;

import com.xu.rpc.commons.exception.RpcException;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.cluster.AbstractClusterInvoker;
import com.xu.rpc.cluster.Directory;
import com.xu.rpc.cluster.LoadBalancer;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;

import java.util.List;

public class AvailableClusterInvoker extends AbstractClusterInvoker {

    public AvailableClusterInvoker(Directory directory) {
        super(directory);
    }

    @Override
    public RpcResult doInvoke(RpcInvocation invocation, List<Invoker> invokers, LoadBalancer loadBalance) throws RpcException {
        for (Invoker invoker : invokers) {
            if (invoker.isAvailable())
                return invoker.invoke(invocation);
        }

        throw new RpcException("no invoker available.");
    }

}