package com.newlandframework.rpc.filter.support;

import com.newlandframework.rpc.filter.AbstractChainFilter;
import com.newlandframework.rpc.filter.ChainFilter;
import com.newlandframework.rpc.model.MessageRequest;
import com.newlandframework.rpc.protocol.Invoker;


public class EchoChainFilter extends AbstractChainFilter {

    @Override
    public Object doIntercept(Invoker invoker, MessageRequest request) throws Throwable {
        Object o = null;
        System.err.println("【NettyRPC 2.0】 拦截器链-1##TRACE MESSAGE-ID:" + request.getMessageId());
        o = invoker.invoke(request);
        return o;
    }

}

