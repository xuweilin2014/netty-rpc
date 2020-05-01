package com.newlandframework.rpc.filter;

import com.newlandframework.rpc.core.ModularInvoker;
import com.newlandframework.rpc.model.MessageRequest;

public interface ChainFilter {
    Object invoke(ModularInvoker<?> invoker, MessageRequest request) throws Throwable;
}

