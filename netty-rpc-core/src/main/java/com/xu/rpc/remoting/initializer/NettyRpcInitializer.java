
package com.xu.rpc.remoting.initializer;

import io.netty.channel.ChannelDuplexHandler;
import io.netty.channel.ChannelPipeline;


public interface NettyRpcInitializer {

    void handle(ChannelPipeline pipeline, ChannelDuplexHandler handler);

}

