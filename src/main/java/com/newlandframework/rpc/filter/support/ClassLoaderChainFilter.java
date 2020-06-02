package com.newlandframework.rpc.filter.support;

import com.newlandframework.rpc.core.ChainFilterInvoker;
import com.newlandframework.rpc.filter.ChainFilter;
import com.newlandframework.rpc.model.MessageRequest;


public class ClassLoaderChainFilter implements ChainFilter {
    @Override
    public Object intercept(ChainFilterInvoker<?> invoker, MessageRequest request) throws Throwable {
        ClassLoader ocl = Thread.currentThread().getContextClassLoader();
        Thread.currentThread().setContextClassLoader(invoker.getInterface().getClassLoader());

        System.err.println("【NettyRPC 2.0】: 拦截器链-2##TRACE MESSAGE-ID:" + request.getMessageId());

        Object result = null;
        try {
            result = invoker.invoke(request);
            return result;
        } catch (Throwable throwable) {
            throwable.printStackTrace();
            throw throwable;
        } finally {
            Thread.currentThread().setContextClassLoader(ocl);
        }
    }
}

