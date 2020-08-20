package com.xu.rpc.remoting.initializer.hessian;

import com.xu.rpc.remoting.initializer.NettyRpcInitializer;
import com.xu.rpc.remoting.handler.NettyServerHandler;
import com.xu.rpc.serialize.hessian.HessianCodecUtil;
import com.xu.rpc.serialize.hessian.HessianDecoder;
import com.xu.rpc.serialize.hessian.HessianEncoder;
import io.netty.channel.ChannelPipeline;


public class HessianRecvInitializer implements NettyRpcInitializer {

    @Override
    public void handle(ChannelPipeline pipeline, NettyServerHandler handler) {
        HessianCodecUtil util = new HessianCodecUtil();
        pipeline.addLast(new HessianEncoder(util));
        pipeline.addLast(new HessianDecoder(util));
        pipeline.addLast(handler);
    }

}

