package com.newlandframework.rpc.netty.server;

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

    /**
     * NioSocketChannel实现了socketChannel接口，NioSocketChannel表示Netty为客户端建立的连接，用来
     * 处理客户端的读写请求，这里获取到此channel对应的pipeline，然后根据用户选择的传输协议不同，往pipeline中添加不同的
     * handler。
     */
    @Override
    protected void initChannel(SocketChannel socketChannel) throws Exception {
        ChannelPipeline pipeline = socketChannel.pipeline();
        frame.select(protocol, pipeline);
    }
}
