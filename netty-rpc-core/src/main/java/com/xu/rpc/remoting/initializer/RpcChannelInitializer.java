package com.xu.rpc.remoting.initializer;

import com.xu.rpc.serialize.Serialization;
import io.netty.channel.ChannelDuplexHandler;
import io.netty.channel.ChannelInitializer;
import io.netty.channel.ChannelPipeline;
import io.netty.channel.socket.SocketChannel;


public class RpcChannelInitializer extends ChannelInitializer<SocketChannel> {

    private Serialization serialization;

    private ChannelDuplexHandler handler;

    private static RpcSerializeFrame frame = new RpcSerializeFrame();

    public RpcChannelInitializer(Serialization serialization, ChannelDuplexHandler handler) {
        this.serialization = serialization;
        this.handler = handler;
    }

    /**
     * NioSocketChannel实现了socketChannel接口，NioSocketChannel表示Netty为客户端建立的连接，用来
     * 处理客户端的读写请求，这里获取到此channel对应的pipeline，然后根据用户选择的传输协议不同，往pipeline中添加不同的
     * handler。
     */
    @Override
    protected void initChannel(SocketChannel socketChannel) {
        ChannelPipeline pipeline = socketChannel.pipeline();
        frame.select(serialization, pipeline, handler);
    }
}
