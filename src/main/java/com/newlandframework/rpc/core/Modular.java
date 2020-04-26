package com.newlandframework.rpc.core;

import com.newlandframework.rpc.model.MessageRequest;


public interface Modular {
    <T> ModuleProvider<T> getProvider(ModuleInvoker<T> invoker, MessageRequest request);
}

