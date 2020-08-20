package com.xu.rpc.filter;

import com.xu.rpc.core.extension.Extension;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.model.MessageRequest;
import com.xu.rpc.protocol.Invoker;

@Extension(value = "echo")
public interface ChainFilter {

    Object intercept(Invoker invoker, MessageRequest request) throws RpcException;

}


