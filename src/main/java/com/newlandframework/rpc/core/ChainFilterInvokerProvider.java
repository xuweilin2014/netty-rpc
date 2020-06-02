package com.newlandframework.rpc.core;

/**
 *
 */
public interface ChainFilterInvokerProvider<T> {
    ChainFilterInvoker<T> getInvoker();

    void destroyInvoker();
}

