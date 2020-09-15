package com.xu.rpc.remoting.handler;

import com.xu.rpc.exception.RemotingException;
import com.xu.rpc.model.MessageRequest;
import io.netty.channel.Channel;

public abstract class ReplyHandler implements ChannelHandler {

    public abstract Object reply(Object message, Channel channel) throws RemotingException;


    @Override
    public void sent(Channel channel, Object message) throws RemotingException {
        // do nothing
    }

    @Override
    public void received(Channel channel, Object message) throws RemotingException {
        // do nothing
    }

    @Override
    public void connected(Channel channel) throws RemotingException {
        // do nothing
    }

    @Override
    public void disconnected(Channel channel) throws RemotingException {
        // do nothing
    }
}
