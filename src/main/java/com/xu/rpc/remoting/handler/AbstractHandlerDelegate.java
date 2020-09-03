package com.xu.rpc.remoting.handler;

import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.exception.RemotingException;
import com.xu.rpc.commons.Assert;
import io.netty.channel.Channel;

public class AbstractHandlerDelegate implements ChannelHandler {

    protected ChannelHandler handler;

    public AbstractHandlerDelegate(){}

    public AbstractHandlerDelegate(ChannelHandler handler){
        Assert.notNull(handler, "handler cannot be null.");
        this.handler = handler;
    }

    @Override
    public void received(Channel channel, Object message) throws RemotingException {
        setReadTimestamp(channel);
        handler.received(channel, message);
    }

    @Override
    public void connected(Channel channel) throws RemotingException {
        setReadTimestamp(channel);
        setWriteTimestamp(channel);
        handler.connected(channel);
    }

    @Override
    public void disconnected(Channel channel) throws RemotingException {
        clearReadTimestamp(channel);
        clearWriteTimestamp(channel);
        handler.disconnected(channel);
    }

    @Override
    public void sent(Channel channel, Object message) throws RemotingException {
        setWriteTimestamp(channel);
        handler.sent(channel, message);
    }

    public final void setReadTimestamp(Channel channel){
        channel.attr(RpcConfig.LAST_READ_TIMESTAMP).set(System.currentTimeMillis());
    }

    public final void setWriteTimestamp(Channel channel){
        channel.attr(RpcConfig.LAST_WRITE_TIMESTAMP).set(System.currentTimeMillis());
    }

    public final void clearReadTimestamp(Channel channel){
        channel.attr(RpcConfig.LAST_READ_TIMESTAMP).remove();
    }

    public final void clearWriteTimestamp(Channel channel){
        channel.attr(RpcConfig.LAST_WRITE_TIMESTAMP).remove();
    }
}
