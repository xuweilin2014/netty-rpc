package com.newlandframework.rpc.remoting.handler;

import com.newlandframework.rpc.model.MessageRequest;
import io.netty.channel.Channel;

public interface ReplyHandler extends ChannelHandler {

    public Object reply(MessageRequest request, Channel channel);

}
