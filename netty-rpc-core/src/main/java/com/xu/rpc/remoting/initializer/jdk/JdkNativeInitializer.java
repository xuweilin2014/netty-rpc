package com.xu.rpc.remoting.initializer.jdk;

import com.xu.rpc.serialize.MessageCodecUtil;
import com.xu.rpc.remoting.initializer.NettyRpcInitializer;
import io.netty.channel.ChannelDuplexHandler;
import io.netty.channel.ChannelPipeline;
import io.netty.handler.codec.LengthFieldBasedFrameDecoder;
import io.netty.handler.codec.LengthFieldPrepender;
import io.netty.handler.codec.serialization.ClassResolvers;
import io.netty.handler.codec.serialization.ObjectDecoder;
import io.netty.handler.codec.serialization.ObjectEncoder;

public class JdkNativeInitializer implements NettyRpcInitializer {

    @Override
    public void handle(ChannelPipeline pipeline, ChannelDuplexHandler handler) {
        pipeline.addLast(new LengthFieldBasedFrameDecoder(Integer.MAX_VALUE, 0, MessageCodecUtil.MESSAGE_LENGTH, 0, MessageCodecUtil.MESSAGE_LENGTH));
        pipeline.addLast(new LengthFieldPrepender(MessageCodecUtil.MESSAGE_LENGTH));
        pipeline.addLast(new ObjectEncoder());
        pipeline.addLast(new ObjectDecoder(Integer.MAX_VALUE, ClassResolvers.weakCachingConcurrentResolver(this.getClass().getClassLoader())));
        pipeline.addLast(handler);
    }

}
