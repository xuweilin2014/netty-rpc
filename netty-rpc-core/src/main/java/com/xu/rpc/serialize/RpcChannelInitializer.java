package com.xu.rpc.serialize;

import com.xu.rpc.commons.URL;
import com.xu.rpc.commons.util.AdaptiveExtensionUtils;
import com.xu.rpc.core.RpcConfig;
import io.netty.channel.ChannelDuplexHandler;
import io.netty.channel.ChannelInitializer;
import io.netty.channel.ChannelPipeline;
import io.netty.channel.socket.SocketChannel;
import io.netty.handler.codec.LengthFieldBasedFrameDecoder;
import io.netty.handler.codec.LengthFieldPrepender;


public class RpcChannelInitializer extends ChannelInitializer<SocketChannel> {

    private final URL url;

    private final ChannelDuplexHandler handler;

    public RpcChannelInitializer(URL url, ChannelDuplexHandler handler) {
        this.url = url;
        this.handler = handler;
    }

    /**
     * NioSocketChannel实现了socketChannel接口，NioSocketChannel表示Netty为客户端建立的连接，用来
     * 处理客户端的读写请求，这里获取到此channel对应的pipeline，然后根据用户选择的传输协议不同，往pipeline中添加不同的
     * handler。
     */
    @Override
    protected void initChannel(SocketChannel socketChannel) {
        SerializeFactory serializeFactory = AdaptiveExtensionUtils.getSerializeFactory(url);
        Serialize serialize = serializeFactory.getSerialize(url);

        ChannelPipeline pipeline = socketChannel.pipeline();
                // ByteBuf -> Message
        pipeline.addLast(new LengthFieldPrepender(RpcConfig.LENGTH_FIELD_LENGTH, RpcConfig.LENGTH_ADJUSTMENT))
                // Message -> ByteBuf
                .addLast(new MessageEncoder(serialize))
                // ByteBuf -> Message
                .addLast( new LengthFieldBasedFrameDecoder(RpcConfig.MAX_FRAME_LENGTH, RpcConfig.LENGTH_FIELD_OFFSET, RpcConfig.LENGTH_FIELD_LENGTH, RpcConfig.LENGTH_ADJUSTMENT, RpcConfig.INITIAL_BYTES_TO_STRIP))
                // Message -> Message
                .addLast(new MessageDecoder(serialize))
                .addLast(handler);
    }
}
