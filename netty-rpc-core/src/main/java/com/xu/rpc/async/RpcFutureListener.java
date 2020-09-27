package com.xu.rpc.async;

public interface RpcFutureListener {

    public void done(Object result);

    public void caught(Throwable t);

}
