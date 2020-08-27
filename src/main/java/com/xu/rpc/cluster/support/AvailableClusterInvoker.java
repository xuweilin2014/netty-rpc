package com.xu.rpc.cluster.support;

import com.xu.rpc.cluster.AbstractClusterInvoker;
import com.xu.rpc.cluster.Directory;
import com.xu.rpc.cluster.LoadBalance;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.protocol.Invoker;

import java.util.List;

public class AvailableClusterInvoker extends AbstractClusterInvoker {

    public AvailableClusterInvoker(Directory directory) {
        super(directory);
    }

    @Override
    public Object doInvoke(RpcInvocation invocation, List<Invoker> invokers, LoadBalance loadBalance) throws RpcException{
        for (Invoker invoker : invokers) {
            if (invoker.isAvailable())
                return invoker.invoke(invocation);
        }

        throw new RpcException("no invoker available.");
    }

}
