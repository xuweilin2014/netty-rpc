package com.newlandframework.rpc.core;

import com.newlandframework.rpc.model.MessageRequest;


public class DefaultModular implements Modular {
    @Override
    public <T> ModuleProvider<T> getProvider(ModuleInvoker<T> invoker, MessageRequest request) {
        return new ModuleProvider<T>() {
            @Override
            public ModuleInvoker<T> getInvoker() {
                return invoker;
            }

            @Override
            public void destoryInvoker() {
                invoker.destroy();
            }
        };
    }
}

