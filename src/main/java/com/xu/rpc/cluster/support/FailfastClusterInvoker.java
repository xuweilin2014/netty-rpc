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
            throw new RpcException("there is no invoker to be invoked.");

        Invoker invoker = select(invokers, null, loadBalance, invocation);
        try {
            RpcResult result = invoker.invoke(invocation);

            if (result.getException() != null)
                throw result.getException();

            return result;
        } catch (Throwable e) {
            throw new RpcException("failed to invoke the invoker, caused by " + e.getMessage());
        }
    }

}
