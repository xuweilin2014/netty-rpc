package com.newlandframework.rpc.filter.support;

import com.newlandframework.rpc.core.ModuleInvoker;
import com.newlandframework.rpc.filter.ChainFilter;
import com.newlandframework.rpc.model.MessageRequest;


public class EchoChainFilter implements ChainFilter {
    @Override
    public Object invoke(ModuleInvoker<?> invoker, MessageRequest request) throws Throwable {
        Object o = null;
        try {
            System.out.println("[NettyRPC 2.0]: EchoChainFilter##TRACE MESSAGE-ID:" + request.getMessageId());
            o = invoker.invoke(request);
            return o;
        } catch (Throwable throwable) {
            throwable.printStackTrace();
            throw throwable;
        }
    }
}

