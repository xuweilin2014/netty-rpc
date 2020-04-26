package com.newlandframework.rpc.netty.handler;

import io.netty.channel.ChannelPipeline;


public interface NettyRpcSendHandler {
    void handle(ChannelPipeline pipeline);
}

