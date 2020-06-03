package com.newlandframework.rpc.core;

import com.newlandframework.rpc.model.MessageRequest;

public interface ChainFilterInvoker<T> {

    Class<T> getInterface();

    Object invoke(MessageRequest request) throws Throwable;

    void destroy();
}
