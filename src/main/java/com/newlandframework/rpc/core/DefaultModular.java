package com.newlandframework.rpc.core;

import com.newlandframework.rpc.model.MessageRequest;


public class DefaultModular implements ModularProviderHolder {
    @Override
    public <T> ModularProvider<T> getProvider(ModularInvoker<T> invoker, MessageRequest request) {
        return new ModularProvider<T>() {
            @Override
            public ModularInvoker<T> getInvoker() {
                return invoker;
            }

            @Override
            public void destoryInvoker() {
                invoker.destroy();
            }
        };
    }
}

