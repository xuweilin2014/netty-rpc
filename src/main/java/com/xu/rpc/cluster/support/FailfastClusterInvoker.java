package com.xu.rpc.cluster.support;

import com.xu.rpc.cluster.AbstractClusterInvoker;
import com.xu.rpc.cluster.Directory;
import com.xu.rpc.cluster.LoadBalancer;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.protocol.Invoker;

import java.util.List;

// 快速失败：只进行一次调用，失败之后直接抛出异常，适合增加、删除记录这种需要保持幂等的操作
public class FailfastClusterInvoker extends AbstractClusterInvoker {

    public FailfastClusterInvoker(Directory directory) {
        super(directory);
    }

    @Override
    public RpcResult doInvoke(RpcInvocation invocation, List<Invoker> invokers, LoadBalancer loadBalance) throws RpcException{
        if (invokers == null || invokers.isEmpty())
            throw new RpcException("there is no invoker to invoke.");

        Invoker invoker = select(invokers, null, loadBalance, invocation);
        try {
            return invoker.invoke(invocation);
        } catch (RpcException e) {
            throw new RpcException("failed to invoke the invoker.");
        }
    }

}
