package com.xu.rpc.remoting.handler;

import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.exception.RemotingException;
import com.xu.rpc.commons.Assert;
import com.xu.rpc.remoting.exchanger.RpcChannel;
import io.netty.channel.Channel;

public class AbstractHandlerDelegate implements ChannelHandler {

    protected ChannelHandler handler;

    public AbstractHandlerDelegate(){}

    public AbstractHandlerDelegate(ChannelHandler handler){
        Assert.notNull(handler, "handler cannot be null.");
        this.handler = handler;
    }

    @Override
    public void received(RpcChannel channel, Object message) throws RemotingException {
        setReadTimestamp(channel);
        handler.received(channel, message);
    }

    @Override
    public void connected(RpcChannel channel) throws RemotingException {
        setReadTimestamp(channel);
        setWriteTimestamp(channel);
        handler.connected(channel);
    }

    @Override
    public void disconnected(RpcChannel channel) throws RemotingException {
        clearReadTimestamp(channel);
        clearWriteTimestamp(channel);
        handler.disconnected(channel);
    }

    @Override
    public void sent(RpcChannel channel, Object message) throws RemotingException {
        setWriteTimestamp(channel);
        handler.sent(channel, message);
    }

    public final void setReadTimestamp(RpcChannel channel){
        channel.setAttribute(RpcConfig.LAST_READ_TIMESTAMP, System.currentTimeMillis());
    }

    public final void setWriteTimestamp(RpcChannel channel){
        channel.setAttribute(RpcConfig.LAST_WRITE_TIMESTAMP, System.currentTimeMillis());
    }

    public final void clearReadTimestamp(RpcChannel channel){
        channel.removeAttribute(RpcConfig.LAST_READ_TIMESTAMP);
    }

    public final void clearWriteTimestamp(RpcChannel channel){
        channel.removeAttribute(RpcConfig.LAST_WRITE_TIMESTAMP);
    }
}
