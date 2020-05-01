package com.newlandframework.rpc.core;

/**
 *
 */
public interface ModularProvider<T> {
    ModularInvoker<T> getInvoker();

    void destoryInvoker();
}

