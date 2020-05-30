package com.newlandframework.rpc.netty.handler;

import com.newlandframework.rpc.netty.client.MessageSendHandler;
import com.newlandframework.rpc.serialize.MessageCodecUtil;
import io.netty.channel.ChannelPipeline;
import io.netty.handler.codec.LengthFieldBasedFrameDecoder;
import io.netty.handler.codec.LengthFieldPrepender;
import io.netty.handler.codec.serialization.ClassResolvers;
import io.netty.handler.codec.serialization.ObjectDecoder;
import io.netty.handler.codec.serialization.ObjectEncoder;

public class JdkNativeSendHandler implements NettyRpcSendHandler {
    @Override
    public void handle(ChannelPipeline pipeline) {
        pipeline.addLast(new LengthFieldBasedFrameDecoder(Integer.MAX_VALUE, 0, MessageCodecUtil.MESSAGE_LENGTH, 0, MessageCodecUtil.MESSAGE_LENGTH));
        pipeline.addLast(new LengthFieldPrepender(MessageCodecUtil.MESSAGE_LENGTH));
        pipeline.addLast(new ObjectEncoder());
        pipeline.addLast(new ObjectDecoder(Integer.MAX_VALUE, ClassResolvers.weakCachingConcurrentResolver(this.getClass().getClassLoader())));
        pipeline.addLast(new MessageSendHandler());
    }
}
