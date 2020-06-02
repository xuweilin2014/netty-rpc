package com.newlandframework.rpc.filter.support;

import com.newlandframework.rpc.core.ChainFilterInvoker;
import com.newlandframework.rpc.filter.ChainFilter;
import com.newlandframework.rpc.model.MessageRequest;


public class EchoChainFilter implements ChainFilter {
    @Override
    public Object intercept(ChainFilterInvoker<?> invoker, MessageRequest request) throws Throwable {
        Object o = null;
        try {
            System.err.println("【NettyRPC 2.0】 拦截器链-1##TRACE MESSAGE-ID:" + request.getMessageId());
            o = invoker.invoke(request);
            return o;
        } catch (Throwable throwable) {
            throwable.printStackTrace();
            throw throwable;
        }
    }
}

