package com.xu.rpc.services.filter;

import com.xu.rpc.commons.exception.RpcException;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;
import com.xu.rpc.filter.ChainFilter;
import com.xu.rpc.protocol.Invoker;

public class FilterTwo implements ChainFilter {
    @Override
    public RpcResult intercept(Invoker invoker, RpcInvocation invocation) throws RpcException {
        RpcResult result = invoker.invoke(invocation);
        System.out.println("invoke filter two");
        return result;
    }
}
