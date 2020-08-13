package com.newlandframework.rpc.remoting.handler;

import io.netty.channel.Channel;

public interface ChannelHandler {

    public void received(Channel channel, Object message);

}
