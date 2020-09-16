package com.xu.rpc.remoting.handler;

import com.xu.rpc.commons.util.RpcUtils;
import io.netty.channel.Channel;
import io.netty.channel.ChannelDuplexHandler;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelPromise;
import org.apache.log4j.Logger;

import java.net.InetSocketAddress;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@io.netty.channel.ChannelHandler.Sharable
public class NettyServerHandler extends ChannelDuplexHandler{

    private static final Logger logger = Logger.getLogger(NettyServerHandler.class);

    private final ChannelHandler handler;

    private final Map<String, Channel> channels = new ConcurrentHashMap<>();

    public NettyServerHandler(ChannelHandler handler){
        this.handler = handler;
    }

    @Override
    public void channelRead(ChannelHandlerContext ctx, Object msg) throws Exception {
        Channel channel = ctx.channel();
        handler.received(channel, msg);
    }

    @Override
    public void write(ChannelHandlerContext ctx, Object msg, ChannelPromise promise) throws Exception {
        super.write(ctx, msg, promise);
        handler.sent(ctx.channel(), msg);
    }

    @Override
    public void channelActive(ChannelHandlerContext ctx) throws Exception {
        Channel channel = ctx.channel();
        channels.put(RpcUtils.toAddressString((InetSocketAddress) channel.remoteAddress()), channel);
        handler.connected(channel);
    }

    @Override
    public void channelInactive(ChannelHandlerContext ctx) throws Exception {
        Channel channel = ctx.channel();
        channels.remove(RpcUtils.toAddressString((InetSocketAddress) channel.remoteAddress()));
        handler.disconnected(channel);
    }

    @Override
    public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) {
        logger.error("error occurs and the channel will be closed, error: " + cause.getMessage());
        ctx.close();
    }

    public Map<String, Channel> getChannels() {
        return channels;
    }
}

