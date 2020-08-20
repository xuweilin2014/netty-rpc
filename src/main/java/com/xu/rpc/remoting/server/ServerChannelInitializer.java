package com.xu.rpc.remoting.server;

import com.xu.rpc.remoting.handler.NettyServerHandler;
import io.netty.channel.ChannelInitializer;
import io.netty.channel.ChannelPipeline;
import io.netty.channel.socket.SocketChannel;

import com.xu.rpc.serialize.Serialization;


public class ServerChannelInitializer extends ChannelInitializer<SocketChannel> {

    private Serialization serialization;

    private NettyServerHandler handler;

    private static RpcRecvSerializeFrame frame = new RpcRecvSerializeFrame();

    ServerChannelInitializer(Serialization serialization, NettyServerHandler handler) {
        this.serialization = serialization;
        this.handler = handler;
    }

    /**
     * NioSocketChannel实现了socketChannel接口，NioSocketChannel表示Netty为客户端建立的连接，用来
     * 处理客户端的读写请求，这里获取到此channel对应的pipeline，然后根据用户选择的传输协议不同，往pipeline中添加不同的
     * handler。
     */
    @Override
    protected void initChannel(SocketChannel socketChannel) throws Exception {
        ChannelPipeline pipeline = socketChannel.pipeline();
        frame.select(serialization, pipeline, handler);
    }
}
