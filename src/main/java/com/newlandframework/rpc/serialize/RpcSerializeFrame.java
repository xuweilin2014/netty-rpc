
package com.newlandframework.rpc.serialize;

import com.newlandframework.rpc.remoting.handler.NettyServerHandler;
import io.netty.channel.ChannelPipeline;


public interface RpcSerializeFrame {

    void select(Serialization protocol, ChannelPipeline pipeline, NettyServerHandler handler);

}

