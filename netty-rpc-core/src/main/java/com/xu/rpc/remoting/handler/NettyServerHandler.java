package com.xu.rpc.remoting.handler;

import com.xu.rpc.remoting.exchanger.NettyChannel;
import com.xu.rpc.remoting.exchanger.RpcChannel;
import com.xu.rpc.commons.URL;
import com.xu.rpc.commons.util.RpcUtils;
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

    private final URL url;

    private final Map<String, RpcChannel> channels = new ConcurrentHashMap<>();

    public NettyServerHandler(ChannelHandler handler, URL url){
        this.handler = handler;
        this.url = url;
    }

    @Override
    public void channelRead(ChannelHandlerContext ctx, Object msg) throws Exception {
        handler.received(NettyChannel.getChannel(ctx.channel(), url), msg);
    }

    @Override
    public void write(ChannelHandlerContext ctx, Object msg, ChannelPromise promise) throws Exception {
        super.write(ctx, msg, promise);
        handler.sent(NettyChannel.getChannel(ctx.channel(), url), msg);
    }

    @Override
    public void channelActive(ChannelHandlerContext ctx) throws Exception {
        RpcChannel channel = NettyChannel.getChannel(ctx.channel(), url);
        channels.put(RpcUtils.toAddressString((InetSocketAddress) channel.getRemoteAddress()), channel);
        handler.connected(channel);
    }

    @Override
    public void channelInactive(ChannelHandlerContext ctx) throws Exception {
        RpcChannel channel = NettyChannel.getChannel(ctx.channel(), url);
        channels.remove(RpcUtils.toAddressString((InetSocketAddress) channel.getRemoteAddress()));
        handler.disconnected(channel);
    }

    @Override
    public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) {
        logger.error("error occurs and the channel will be closed, error: " + cause.getMessage());
        ctx.close();
    }

    public Map<String, RpcChannel> getChannels() {
        return channels;
    }
}

