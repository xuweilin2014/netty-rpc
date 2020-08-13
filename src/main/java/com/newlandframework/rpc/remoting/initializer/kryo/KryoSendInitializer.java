package com.newlandframework.rpc.remoting.initializer.kryo;

import com.newlandframework.rpc.remoting.handler.NettyClientHandler;
import com.newlandframework.rpc.remoting.initializer.NettyRpcInitializer;
import com.newlandframework.rpc.serialize.kryo.KryoCodecUtil;
import com.newlandframework.rpc.serialize.kryo.KryoDecoder;
import com.newlandframework.rpc.serialize.kryo.KryoEncoder;
import com.newlandframework.rpc.serialize.kryo.KryoPoolFactory;
import io.netty.channel.ChannelPipeline;

public class KryoSendInitializer implements NettyRpcInitializer {
    @Override
    public void handle(ChannelPipeline pipeline) {
        KryoCodecUtil util = new KryoCodecUtil(KryoPoolFactory.getKryoPoolInstance());
        pipeline.addLast(new KryoEncoder(util));
        pipeline.addLast(new KryoDecoder(util));
        pipeline.addLast(new NettyClientHandler());
    }
}

