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
        // 有多个注册中心时，就会使用此 AvailableCluster，将多个 Invoker 聚合起来。每一个 Invoker 代表一个
        // 注册中心上面的所有提供者。最后在 AvailableClusterInvoker 选择时，如果某个可用的 Invoker 返回
        for (Invoker invoker : invokers) {
            if (invoker.isAvailable())
                return invoker.invoke(invocation);
        }

        throw new RpcException("no invoker available.");
    }

}
