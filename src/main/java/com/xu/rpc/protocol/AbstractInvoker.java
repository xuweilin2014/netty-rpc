package com.xu.rpc.protocol;

import com.xu.rpc.exception.RpcException;
import com.xu.rpc.model.MessageRequest;

public abstract class AbstractInvoker implements Invoker {

    @Override
    public Object invoke(MessageRequest request) throws RpcException {
        return doInvoke(request);
    }

    public abstract Object doInvoke(MessageRequest request) throws RpcException;

}
