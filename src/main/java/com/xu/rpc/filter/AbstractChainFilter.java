package com.xu.rpc.filter;

import com.xu.rpc.model.MessageRequest;
import com.xu.rpc.protocol.Invoker;

public abstract class AbstractChainFilter implements ChainFilter {

    @Override
    public Object intercept(Invoker invoker, MessageRequest request){
        return doIntercept(invoker, request);
    }

    public abstract Object doIntercept(Invoker invoker, MessageRequest request);

}
