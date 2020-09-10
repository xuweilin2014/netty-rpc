package com.xu.rpc.filter;

import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;
import com.xu.rpc.core.extension.Extension;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.model.MessageRequest;
import com.xu.rpc.protocol.Invoker;

@Extension(value = "echo")
public interface ChainFilter {

    RpcResult intercept(Invoker invoker, RpcInvocation invocation) throws RpcException;

}


