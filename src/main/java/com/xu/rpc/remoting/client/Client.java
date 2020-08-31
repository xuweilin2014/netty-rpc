package com.xu.rpc.remoting.client;

import com.xu.rpc.async.RpcFuture;
import com.xu.rpc.exception.RemotingException;
import com.xu.rpc.util.URL;

public interface Client extends EndPoint{

    public void send(Object message) throws RemotingException;

}
