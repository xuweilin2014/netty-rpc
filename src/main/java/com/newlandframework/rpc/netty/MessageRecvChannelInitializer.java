package com.newlandframework.rpc.netty;

import io.netty.channel.ChannelInitializer;
import io.netty.channel.ChannelPipeline;
import io.netty.channel.socket.SocketChannel;

import java.util.Map;

import com.newlandframework.rpc.serialize.RpcSerializeProtocol;


public class MessageRecvChannelInitializer extends ChannelInitializer<SocketChannel> {

    private RpcSerializeProtocol protocol;
    private RpcRecvSerializeFrame frame = null;

    MessageRecvChannelInitializer buildRpcSerializeProtocol(RpcSerializeProtocol protocol) {
        this.protocol = protocol;
        return this;
    }

    MessageRecvChannelInitializer(Map<String, Object> handlerMap) {
        frame = new RpcRecvSerializeFrame(handlerMap);
    }

    @Override
    protected void initChannel(SocketChannel socketChannel) throws Exception {
        ChannelPipeline pipeline = socketChannel.pipeline();
        frame.select(protocol, pipeline);
    }
}
