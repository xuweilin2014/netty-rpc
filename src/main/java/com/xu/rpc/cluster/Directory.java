package com.xu.rpc.cluster;

import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.protocol.Invoker;

import java.util.List;

public interface Directory extends Node {

    public Class<?> getInterface();

    public List<Invoker> getInvokers(RpcInvocation invocation) throws RpcException;

}
