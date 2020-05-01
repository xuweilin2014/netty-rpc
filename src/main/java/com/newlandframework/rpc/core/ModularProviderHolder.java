package com.newlandframework.rpc.core;

import com.newlandframework.rpc.model.MessageRequest;


public interface ModularProviderHolder {
    <T> ModularProvider<T> getProvider(ModularInvoker<T> invoker, MessageRequest request);
}

