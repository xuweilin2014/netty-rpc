package com.newlandframework.rpc.filter;

import com.newlandframework.rpc.core.ChainFilterInvoker;
import com.newlandframework.rpc.model.MessageRequest;

public interface ChainFilter {
    Object intercept(ChainFilterInvoker<?> invoker, MessageRequest request) throws Throwable;
}

