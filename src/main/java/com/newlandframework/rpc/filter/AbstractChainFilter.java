package com.newlandframework.rpc.filter;

import com.newlandframework.rpc.model.MessageRequest;
import com.newlandframework.rpc.protocol.Invoker;
import com.newlandframework.rpc.util.Assert;

public abstract class AbstractChainFilter implements ChainFilter {

    @Override
    public Object intercept(Invoker invoker, MessageRequest request){
        return doIntercept(invoker, request);
    }

    public abstract Object doIntercept(Invoker invoker, MessageRequest request);

}
