package com.newlandframework.rpc.filter;

import com.newlandframework.rpc.core.ModuleInvoker;
import com.newlandframework.rpc.model.MessageRequest;

public interface ChainFilter {
    Object invoke(ModuleInvoker<?> invoker, MessageRequest request) throws Throwable;
}

