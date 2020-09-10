package com.xu.rpc.remoting.client;

import com.xu.rpc.async.RpcFuture;
import com.xu.rpc.exception.RemotingException;

public interface ExchangeClient extends EndPoint{

    public RpcFuture request(Object request, int timeout) throws RemotingException;

    public RpcFuture request(Object request) throws RemotingException;

    public boolean isConnected();
}
