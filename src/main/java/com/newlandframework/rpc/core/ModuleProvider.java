package com.newlandframework.rpc.core;

/**
 *
 */
public interface ModuleProvider<T> {
    ModuleInvoker<T> getInvoker();

    void destoryInvoker();
}

