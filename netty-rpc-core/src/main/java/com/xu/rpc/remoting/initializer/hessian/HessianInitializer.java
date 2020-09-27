package com.xu.rpc.remoting.initializer.hessian;

import com.xu.rpc.serialize.hessian.HessianCodecUtil;
import com.xu.rpc.serialize.hessian.HessianDecoder;
import com.xu.rpc.serialize.hessian.HessianEncoder;
import com.xu.rpc.remoting.initializer.NettyRpcInitializer;
import io.netty.channel.ChannelDuplexHandler;
import io.netty.channel.ChannelPipeline;


public class HessianInitializer implements NettyRpcInitializer {

    @Override
    public void handle(ChannelPipeline pipeline, ChannelDuplexHandler handler) {
        HessianCodecUtil util = new HessianCodecUtil();
        pipeline.addLast(new HessianEncoder(util));
        pipeline.addLast(new HessianDecoder(util));
        pipeline.addLast(handler);
    }

}

