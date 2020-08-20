package com.xu.rpc.cluster;

import com.xu.rpc.exception.RpcException;
import com.xu.rpc.model.MessageRequest;
import com.xu.rpc.protocol.Invoker;

import java.util.List;

public interface Directory extends Node {

    public List<Invoker> getInvokers(String method) throws RpcException;

}
