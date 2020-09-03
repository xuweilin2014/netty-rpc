package com.xu.rpc.protocol;

import com.xu.rpc.cluster.Node;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;
import com.xu.rpc.exception.RpcException;

public interface Invoker<T> extends Node {

    public Class<?> getInterface();

    public RpcResult invoke(RpcInvocation invocation) throws RpcException;

}
