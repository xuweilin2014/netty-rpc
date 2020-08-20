package com.xu.rpc.remoting.initializer.kryo;

import com.xu.rpc.remoting.initializer.NettyRpcInitializer;
import com.xu.rpc.remoting.handler.NettyServerHandler;
import com.xu.rpc.serialize.kryo.KryoCodecUtil;
import com.xu.rpc.serialize.kryo.KryoDecoder;
import com.xu.rpc.serialize.kryo.KryoEncoder;
import com.xu.rpc.serialize.kryo.KryoPoolFactory;
import io.netty.channel.ChannelPipeline;

public class KryoRecvInitializer implements NettyRpcInitializer {

    @Override
    public void handle(ChannelPipeline pipeline, NettyServerHandler handler) {
        KryoCodecUtil util = new KryoCodecUtil(KryoPoolFactory.getKryoPoolInstance());
        pipeline.addLast(new KryoEncoder(util));
        pipeline.addLast(new KryoDecoder(util));
        pipeline.addLast(handler);
    }

}

