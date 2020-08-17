package com.newlandframework.rpc.protocol;

import com.newlandframework.rpc.exception.RpcException;
import com.newlandframework.rpc.model.MessageRequest;
import com.newlandframework.rpc.util.URL;

public interface Invoker {

    public Object invoke(MessageRequest request) throws RpcException;

    public URL getURL();

    public Class<?> getInterface();
}
