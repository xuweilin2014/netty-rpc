package com.newlandframework.rpc.filter;

import com.newlandframework.rpc.core.extension.Extension;
import com.newlandframework.rpc.model.MessageRequest;
import com.newlandframework.rpc.protocol.Invoker;

@Extension(value = "echo")
public interface ChainFilter {

    Object intercept(Invoker invoker, MessageRequest request) throws Throwable;

    void reject(Invoker invoker, MessageRequest request);

}


