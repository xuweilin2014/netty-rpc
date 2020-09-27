package com.xu.rpc.filter;

import com.xu.rpc.commons.exception.RpcException;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;
import com.xu.rpc.core.extension.Extension;

@Extension(value = "echo")
public interface ChainFilter {

    RpcResult intercept(Invoker invoker, RpcInvocation invocation) throws RpcException;

}


