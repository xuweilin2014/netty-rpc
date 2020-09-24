package com.xu.rpc.remoting.handler;

import com.xu.rpc.core.RpcResult;
import com.xu.rpc.exception.RemotingException;
import com.xu.rpc.model.MessageRequest;
import com.xu.rpc.remoting.exchanger.RpcChannel;
import io.netty.channel.Channel;

public abstract class ReplyHandler implements ChannelHandler {

    public abstract RpcResult reply(Object message, RpcChannel channel) throws RemotingException;

    @Override
    public void sent(RpcChannel channel, Object message) throws RemotingException {
        // do nothing
    }

    @Override
    public void received(RpcChannel channel, Object message) throws RemotingException {
        // do nothing
    }

    @Override
    public void connected(RpcChannel channel) throws RemotingException {
        // do nothing
    }

    @Override
    public void disconnected(RpcChannel channel) throws RemotingException {
        // do nothing
    }
}
