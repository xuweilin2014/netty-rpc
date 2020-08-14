package com.newlandframework.rpc.filter;

import com.newlandframework.rpc.model.MessageRequest;
import com.newlandframework.rpc.protocol.Invoker;
import com.newlandframework.rpc.util.Assert;

public abstract class AbstractChainFilter implements ChainFilter {

    @Override
    public Object intercept(Invoker invoker, MessageRequest request) throws Throwable {
        return doIntercept(invoker, request);
    }

    @Override
    public final void reject(Invoker invoker, MessageRequest request) {
        Assert.notNull(invoker, "invoker == null");
        Assert.notNull(request, "request == null");

        request.setRejected(true);
    }

    public abstract Object doIntercept(Invoker invoker, MessageRequest request) throws Throwable;

}
