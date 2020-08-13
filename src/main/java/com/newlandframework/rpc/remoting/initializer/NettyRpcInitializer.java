
package com.newlandframework.rpc.remoting.initializer;

import com.newlandframework.rpc.remoting.handler.NettyServerHandler;
import com.newlandframework.rpc.remoting.server.NettyServer;
import io.netty.channel.ChannelPipeline;

import java.util.Map;


public interface NettyRpcInitializer {

    void handle(ChannelPipeline pipeline, NettyServerHandler serverHandler);

}

