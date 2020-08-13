package com.newlandframework.rpc.remoting.initializer.jdk;

import com.newlandframework.rpc.remoting.initializer.NettyRpcInitializer;
import com.newlandframework.rpc.remoting.handler.NettyServerHandler;
import com.newlandframework.rpc.remoting.server.NettyServer;
import com.newlandframework.rpc.serialize.MessageCodecUtil;
import io.netty.channel.ChannelPipeline;
import io.netty.handler.codec.LengthFieldBasedFrameDecoder;
import io.netty.handler.codec.LengthFieldPrepender;
import io.netty.handler.codec.serialization.ClassResolvers;
import io.netty.handler.codec.serialization.ObjectDecoder;
import io.netty.handler.codec.serialization.ObjectEncoder;

import java.util.Map;

/**
 * 如果用户选择的传输协议是JDK自带的话，会调用此类中的handle方法，
 */
public class JdkNativeRecvInitializer implements NettyRpcInitializer {

    @Override
    public void handle(ChannelPipeline pipeline, NettyServerHandler handler) {
        pipeline.addLast(new LengthFieldBasedFrameDecoder(Integer.MAX_VALUE, 0, MessageCodecUtil.MESSAGE_LENGTH, 0, MessageCodecUtil.MESSAGE_LENGTH));
        pipeline.addLast(new LengthFieldPrepender(MessageCodecUtil.MESSAGE_LENGTH));
        pipeline.addLast(new ObjectEncoder());
        pipeline.addLast(new ObjectDecoder(Integer.MAX_VALUE, ClassResolvers.weakCachingConcurrentResolver(this.getClass().getClassLoader())));
        pipeline.addLast(handler);
    }

}

