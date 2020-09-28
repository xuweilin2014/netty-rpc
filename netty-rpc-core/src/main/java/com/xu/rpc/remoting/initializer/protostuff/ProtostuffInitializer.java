package com.xu.rpc.remoting.initializer.protostuff;

import com.xu.rpc.remoting.initializer.NettyRpcInitializer;
import com.xu.rpc.serialize.kryo.KryoCodecUtil;
import com.xu.rpc.serialize.kryo.KryoDecoder;
import com.xu.rpc.serialize.kryo.KryoEncoder;
import com.xu.rpc.serialize.kryo.KryoPoolFactory;
import com.xu.rpc.serialize.protostuff.ProtostuffCodecUtil;
import com.xu.rpc.serialize.protostuff.ProtostuffDecoder;
import com.xu.rpc.serialize.protostuff.ProtostuffEncoder;
import io.netty.channel.ChannelDuplexHandler;
import io.netty.channel.ChannelPipeline;

public class ProtostuffInitializer implements NettyRpcInitializer {

    @Override
    public void handle(ChannelPipeline pipeline, ChannelDuplexHandler handler) {
        ProtostuffCodecUtil util = new ProtostuffCodecUtil();
        util.setRpcDirect(false);
        pipeline.addLast(new ProtostuffEncoder(util));
        pipeline.addLast(new ProtostuffDecoder(util));
        pipeline.addLast(handler);
    }

}
