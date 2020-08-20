package com.xu.rpc.filter.support;

import com.xu.rpc.filter.AbstractChainFilter;
import com.xu.rpc.model.MessageRequest;
import com.xu.rpc.protocol.Invoker;


public class EchoChainFilter extends AbstractChainFilter {

    @Override
    public Object doIntercept(Invoker invoker, MessageRequest request) throws Throwable {
        Object o = null;
        System.err.println("【NettyRPC 2.0】 拦截器链-1##TRACE MESSAGE-ID:" + request.getMessageId());
        o = invoker.invoke(request);
        return o;
    }

}
