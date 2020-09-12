package com.xu.rpc.filter.support;

import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.filter.ChainFilter;
import com.xu.rpc.protocol.Invoker;

public class ClassLoaderChainFilter implements ChainFilter {
    @Override
    public RpcResult intercept(Invoker invoker, RpcInvocation invocation) throws RpcException {
        ClassLoader ccl = Thread.currentThread().getContextClassLoader();
        try {
            Thread.currentThread().setContextClassLoader(invoker.getInterface().getClassLoader());
            return invoker.invoke(invocation);
        } finally {
            Thread.currentThread().setContextClassLoader(ccl);
        }
    }
}
