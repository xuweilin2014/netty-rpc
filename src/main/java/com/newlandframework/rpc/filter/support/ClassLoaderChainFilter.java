package com.newlandframework.rpc.filter.support;

import com.newlandframework.rpc.core.ModuleInvoker;
import com.newlandframework.rpc.filter.ChainFilter;
import com.newlandframework.rpc.model.MessageRequest;


public class ClassLoaderChainFilter implements ChainFilter {
    @Override
    public Object invoke(ModuleInvoker<?> invoker, MessageRequest request) throws Throwable {
        ClassLoader ocl = Thread.currentThread().getContextClassLoader();
        Thread.currentThread().setContextClassLoader(invoker.getInterface().getClassLoader());

        System.out.println("[NettyRPC 2.0]: ClassLoaderChainFilter##TRACE MESSAGE-ID:" + request.getMessageId());

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

