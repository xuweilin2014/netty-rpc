package com.xu.rpc.filter.support;

import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.filter.ChainFilter;
import com.xu.rpc.protocol.Invoker;

public class RateLimiterFilter implements ChainFilter {

    @Override
    public RpcResult intercept(Invoker invoker, RpcInvocation invocation) throws RpcException {
        // TODO: 2020/9/4  
        return null;
    }

}
