package com.newlandframework.rpc.filter.support;

import com.newlandframework.rpc.filter.AbstractChainFilter;
import com.newlandframework.rpc.filter.ChainFilter;
import com.newlandframework.rpc.model.MessageRequest;
import com.newlandframework.rpc.protocol.Invoker;


public class ClassLoaderChainFilter extends AbstractChainFilter {

    @Override
    public Object doIntercept(Invoker invoker, MessageRequest request) throws Throwable {
        ClassLoader ocl = Thread.currentThread().getContextClassLoader();
        Thread.currentThread().setContextClassLoader(invoker.getInterface().getClassLoader());

        System.err.println("【NettyRPC 2.0】: 拦截器链-2##TRACE MESSAGE-ID:" + request.getMessageId());

        Object result = null;
        try {
            result = invoker.invoke(request);
            return result;
        } finally {
            Thread.currentThread().setContextClassLoader(ocl);
        }
    }

}

