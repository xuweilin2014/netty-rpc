package com.xu.rpc.remoting.client;

import com.xu.rpc.commons.exception.RemotingException;
import com.xu.rpc.remoting.exchanger.RpcChannel;

public interface Client extends EndPoint{

    public void send(Object message) throws RemotingException;

    public RpcChannel getChannel();

    public void reconnect() throws RemotingException;

    public boolean isConnected();
}
