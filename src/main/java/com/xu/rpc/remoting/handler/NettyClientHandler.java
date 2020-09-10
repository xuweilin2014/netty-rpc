package com.xu.rpc.remoting.handler;

import io.netty.buffer.Unpooled;
import io.netty.channel.*;

import java.net.SocketAddress;
import java.util.concurrent.ConcurrentHashMap;

import com.xu.rpc.model.MessageRequest;
import com.xu.rpc.model.MessageResponse;
import org.apache.log4j.Logger;

@io.netty.channel.ChannelHandler.Sharable
public class NettyClientHandler extends ChannelDuplexHandler{

    private static final Logger logger = Logger.getLogger(NettyClientHandler.class);

    private ChannelHandler handler;

    public NettyClientHandler(ChannelHandler handler){
        this.handler = handler;
    }

    @Override
    public void write(ChannelHandlerContext ctx, Object msg, ChannelPromise promise) throws Exception {
        super.write(ctx, msg, promise);
        handler.sent(ctx.channel(), msg);
    }

    @Override
    public void channelRead(ChannelHandlerContext ctx, Object msg) throws Exception {
        handler.received(ctx.channel(), msg);
    }

    @Override
    public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) throws Exception {
        logger.error("error occurs and the channel will be closed, error: " + cause.getMessage());
        ctx.close();
    }

    @Override
    public void channelActive(ChannelHandlerContext ctx) throws Exception {
        handler.connected(ctx.channel());
    }

    @Override
    public void channelInactive(ChannelHandlerContext ctx) throws Exception {
        handler.disconnected(ctx.channel());
    }
}
