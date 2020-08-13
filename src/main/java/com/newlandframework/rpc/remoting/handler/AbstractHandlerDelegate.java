package com.newlandframework.rpc.remoting.handler;

import com.newlandframework.rpc.util.Assert;
import io.netty.channel.Channel;

public class AbstractHandlerDelegate implements ChannelHandler {

    protected ChannelHandler handler;

    public AbstractHandlerDelegate(ChannelHandler handler){
        Assert.notNull(handler, "handler cannot be null.");
        this.handler = handler;
    }

    @Override
    public void received(Channel channel, Object message) {

    }
}
