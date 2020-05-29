package com.newlandframework.rpc.netty.handler;

import com.newlandframework.rpc.netty.MessageSendHandler;
import com.newlandframework.rpc.serialize.kryo.KryoCodecUtil;
import com.newlandframework.rpc.serialize.kryo.KryoDecoder;
import com.newlandframework.rpc.serialize.kryo.KryoEncoder;
import com.newlandframework.rpc.serialize.kryo.KryoPoolFactory;
import io.netty.channel.ChannelPipeline;

public class KryoSendHandler implements NettyRpcSendHandler {
    @Override
    public void handle(ChannelPipeline pipeline) {
        KryoCodecUtil util = new KryoCodecUtil(KryoPoolFactory.getKryoPoolInstance());
        pipeline.addLast(new KryoEncoder(util));
        pipeline.addLast(new KryoDecoder(util));
        pipeline.addLast(new MessageSendHandler());
    }
}

