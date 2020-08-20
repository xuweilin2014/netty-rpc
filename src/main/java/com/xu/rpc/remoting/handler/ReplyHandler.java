package com.xu.rpc.remoting.handler;

import com.xu.rpc.model.MessageRequest;
import io.netty.channel.Channel;

public interface ReplyHandler extends ChannelHandler {

    public Object reply(MessageRequest request, Channel channel);

}
