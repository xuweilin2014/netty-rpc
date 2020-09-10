package com.xu.rpc.async;

import com.xu.rpc.core.RpcResult;

public interface RpcFutureListener {

    public void done(Object result);

    public void caught(Throwable t);

}
