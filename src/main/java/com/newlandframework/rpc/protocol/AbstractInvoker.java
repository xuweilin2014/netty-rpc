package com.newlandframework.rpc.protocol;

import com.newlandframework.rpc.exception.RpcException;
import com.newlandframework.rpc.model.MessageRequest;

public abstract class AbstractInvoker implements Invoker {

    @Override
    public Object invoke(MessageRequest request) throws RpcException {
        return doInvoke(request);
    }

    public abstract Object doInvoke(MessageRequest request) throws RpcException;

}
