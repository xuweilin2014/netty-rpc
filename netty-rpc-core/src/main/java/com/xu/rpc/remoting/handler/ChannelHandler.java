package com.xu.rpc.remoting.handler;

import com.xu.rpc.commons.exception.RemotingException;
import com.xu.rpc.remoting.exchanger.RpcChannel;

public interface ChannelHandler {

    public void sent(RpcChannel channel, Object message) throws RemotingException;

    public void received(RpcChannel channel, Object message) throws RemotingException;

    public void connected(RpcChannel channel) throws RemotingException;

    public void disconnected(RpcChannel channel) throws RemotingException;

}
