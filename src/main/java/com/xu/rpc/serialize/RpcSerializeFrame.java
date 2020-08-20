
package com.xu.rpc.serialize;

import com.xu.rpc.remoting.handler.NettyServerHandler;
import io.netty.channel.ChannelPipeline;


public interface RpcSerializeFrame {

    void select(Serialization protocol, ChannelPipeline pipeline, NettyServerHandler handler);

}

