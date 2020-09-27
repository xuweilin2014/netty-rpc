package com.xu.rpc.protocol;

import com.xu.rpc.commons.exception.RpcException;
import com.xu.rpc.cluster.Node;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;

public interface Invoker<T> extends Node {

    public Class<?> getInterface();

    public RpcResult invoke(RpcInvocation invocation) throws RpcException;

}
