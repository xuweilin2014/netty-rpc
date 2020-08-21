package com.xu.rpc.protocol;

import com.xu.rpc.cluster.Node;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.model.MessageRequest;
import com.xu.rpc.util.URL;

public interface Invoker extends Node {

    public Class<?> getInterface();

    public Object invoke(MessageRequest request) throws RpcException;

}
