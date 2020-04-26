
package com.newlandframework.rpc.netty.handler;

import io.netty.channel.ChannelPipeline;

import java.util.Map;


public interface NettyRpcRecvHandler {
    void handle(Map<String, Object> handlerMap, ChannelPipeline pipeline);
}

