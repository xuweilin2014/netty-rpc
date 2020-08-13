package com.newlandframework.rpc.remoting.execution;

import com.newlandframework.rpc.model.MessageRequest;
import com.newlandframework.rpc.model.MessageResponse;
import com.newlandframework.rpc.remoting.handler.ChannelHandler;
import io.netty.channel.Channel;


public class RecvTask extends AbstractRecvTask {

    public RecvTask(MessageRequest request, ChannelHandler handler, Channel channel) {
        super(request, handler, channel);
    }

    @Override
    public void run() {
        super.run();
        channel.writeAndFlush(response);
    }
}

