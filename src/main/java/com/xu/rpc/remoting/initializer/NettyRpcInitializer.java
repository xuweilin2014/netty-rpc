
package com.xu.rpc.remoting.initializer;

import com.xu.rpc.remoting.handler.NettyServerHandler;
import io.netty.channel.ChannelPipeline;


public interface NettyRpcInitializer {

    void handle(ChannelPipeline pipeline, NettyServerHandler serverHandler);

}

