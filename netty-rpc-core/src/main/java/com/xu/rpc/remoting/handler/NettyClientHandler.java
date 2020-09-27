package com.xu.rpc.remoting.handler;

import com.xu.rpc.remoting.exchanger.NettyChannel;
import com.xu.rpc.remoting.exchanger.RpcChannel;
import com.xu.rpc.commons.URL;
import io.netty.channel.*;

import org.apache.log4j.Logger;

@io.netty.channel.ChannelHandler.Sharable
public class NettyClientHandler extends ChannelDuplexHandler{

    private static final Logger logger = Logger.getLogger(NettyClientHandler.class);

    private final ChannelHandler handler;

    private final URL url;

    public NettyClientHandler(ChannelHandler handler, URL url) {
        this.handler = handler;
        this.url = url;
    }

    @Override
    public void write(ChannelHandlerContext ctx, Object msg, ChannelPromise promise) throws Exception {
        super.write(ctx, msg, promise);
        RpcChannel channel = NettyChannel.getChannel(ctx.channel(), url);
        handler.sent(channel, msg);
    }

    @Override
    public void channelRead(ChannelHandlerContext ctx, Object msg) throws Exception {
        handler.received(NettyChannel.getChannel(ctx.channel(), url), msg);
    }

    @Override
    public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) throws Exception {
        logger.error("error occurs and the channel will be closed, error: " + cause.getMessage());
        ctx.close();
    }

    @Override
    public void channelActive(ChannelHandlerContext ctx) throws Exception {
        handler.connected(NettyChannel.getChannel(ctx.channel(), url));
    }

    @Override
    public void channelInactive(ChannelHandlerContext ctx) throws Exception {
        handler.disconnected(NettyChannel.getChannel(ctx.channel(), url));
    }
}
