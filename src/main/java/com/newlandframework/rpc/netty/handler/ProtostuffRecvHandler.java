
package com.newlandframework.rpc.netty.handler;

import com.newlandframework.rpc.netty.MessageRecvHandler;
import com.newlandframework.rpc.serialize.protostuff.ProtostuffCodecUtil;
import com.newlandframework.rpc.serialize.protostuff.ProtostuffDecoder;
import com.newlandframework.rpc.serialize.protostuff.ProtostuffEncoder;
import io.netty.channel.ChannelPipeline;

import java.util.Map;

public class ProtostuffRecvHandler implements NettyRpcRecvHandler {
    @Override
    public void handle(Map<String, Object> handlerMap, ChannelPipeline pipeline) {
        ProtostuffCodecUtil util = new ProtostuffCodecUtil();
        util.setRpcDirect(true);
        pipeline.addLast(new ProtostuffEncoder(util));
        pipeline.addLast(new ProtostuffDecoder(util));
        pipeline.addLast(new MessageRecvHandler(handlerMap));
    }
}

