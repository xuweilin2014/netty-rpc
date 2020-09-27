package com.xu.rpc.remoting.client;

import com.xu.rpc.commons.exception.RemotingException;
import com.xu.rpc.async.RpcFuture;

public interface ExchangeClient extends EndPoint{

    public RpcFuture request(Object request, int timeout) throws RemotingException;

    public RpcFuture request(Object request) throws RemotingException;

    public boolean isConnected();
}
