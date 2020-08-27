
package com.xu.rpc.remoting.initializer;

import com.xu.rpc.remoting.handler.NettyServerHandler;
import com.xu.rpc.serialize.Serialization;
import io.netty.channel.ChannelDuplexHandler;
import io.netty.channel.ChannelPipeline;


public interface SerializeFrame {

    void select(Serialization protocol, ChannelPipeline pipeline, ChannelDuplexHandler handler);

}

