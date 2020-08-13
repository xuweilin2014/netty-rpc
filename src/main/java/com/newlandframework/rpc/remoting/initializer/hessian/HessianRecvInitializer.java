package com.newlandframework.rpc.remoting.initializer.hessian;

import com.newlandframework.rpc.remoting.initializer.NettyRpcInitializer;
import com.newlandframework.rpc.remoting.handler.NettyServerHandler;
import com.newlandframework.rpc.serialize.hessian.HessianCodecUtil;
import com.newlandframework.rpc.serialize.hessian.HessianDecoder;
import com.newlandframework.rpc.serialize.hessian.HessianEncoder;
import io.netty.channel.ChannelPipeline;

import java.util.Map;


public class HessianRecvInitializer implements NettyRpcInitializer {

    @Override
    public void handle(ChannelPipeline pipeline, NettyServerHandler handler) {
        HessianCodecUtil util = new HessianCodecUtil();
        pipeline.addLast(new HessianEncoder(util));
        pipeline.addLast(new HessianDecoder(util));
        pipeline.addLast(handler);
    }

}

