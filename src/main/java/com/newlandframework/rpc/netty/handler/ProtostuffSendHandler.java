package com.newlandframework.rpc.netty.handler;

import com.newlandframework.rpc.netty.MessageSendHandler;
import com.newlandframework.rpc.serialize.protostuff.ProtostuffCodecUtil;
import com.newlandframework.rpc.serialize.protostuff.ProtostuffDecoder;
import com.newlandframework.rpc.serialize.protostuff.ProtostuffEncoder;
import io.netty.channel.ChannelPipeline;

public class ProtostuffSendHandler implements NettyRpcSendHandler {
    @Override
    public void handle(ChannelPipeline pipeline) {
        ProtostuffCodecUtil util = new ProtostuffCodecUtil();
        util.setRpcDirect(false);
        pipeline.addLast(new ProtostuffEncoder(util));
        pipeline.addLast(new ProtostuffDecoder(util));
        pipeline.addLast(new MessageSendHandler());
    }
}
