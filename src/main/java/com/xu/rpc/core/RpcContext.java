package com.xu.rpc.core;

import java.util.concurrent.Future;

public class RpcContext {

    private static final ThreadLocal<RpcContext> context = new ThreadLocal<RpcContext>(){
        @Override
        protected RpcContext initialValue() {
            return new RpcContext();
        }
    };

    private Future<?> future;

    public RpcContext(){
    }

    public static RpcContext getContext() {
        return context.get();
    }

    public void setFuture(Future<?> future) {
        this.future = future;
    }

    public Future<?> getFuture() {
        return future;
    }
}
