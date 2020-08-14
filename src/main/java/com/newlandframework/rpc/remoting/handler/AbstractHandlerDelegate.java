package com.newlandframework.rpc.remoting.handler;

import com.newlandframework.rpc.exception.RemotingException;
import com.newlandframework.rpc.util.Assert;
import io.netty.channel.Channel;

public class AbstractHandlerDelegate implements ChannelHandler {

    protected ChannelHandler handler;

    public AbstractHandlerDelegate(ChannelHandler handler){
        Assert.notNull(handler, "handler cannot be null.");
        this.handler = handler;
    }

    @Override
    public void received(Channel channel, Object message) throws RemotingException {
        handler.received(channel, message);
    }

    @Override
    public void connected(Channel channel) throws RemotingException {
        handler.connected(channel);
    }

    @Override
    public void disconnected(Channel channel) throws RemotingException {
        handler.disconnected(channel);
    }

    @Override
    public void sent(Channel channel, Object message) throws RemotingException {
        handler.sent(channel, message);
    }
}
