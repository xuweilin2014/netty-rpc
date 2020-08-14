package com.newlandframework.rpc.remoting.handler;

import com.newlandframework.rpc.exception.RemotingException;
import com.newlandframework.rpc.remoting.execution.RecvInitializeTaskFacade;
import com.newlandframework.rpc.remoting.server.NettyServer;
import com.newlandframework.rpc.util.NetUtil;
import io.netty.channel.*;

import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;

import com.newlandframework.rpc.model.MessageRequest;
import com.newlandframework.rpc.model.MessageResponse;

@io.netty.channel.ChannelHandler.Sharable
public class NettyServerHandler extends ChannelDuplexHandler{

    private ChannelHandler handler;

    private Map<String, Channel> channels = new ConcurrentHashMap<>();

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
        channels.put(NetUtil.toAddressString((InetSocketAddress) channel.remoteAddress()), channel);
        handler.connected(channel);
    }

    @Override
    public void channelInactive(ChannelHandlerContext ctx) throws Exception {
        Channel channel = ctx.channel();
        channels.remove(NetUtil.toAddressString((InetSocketAddress) channel.remoteAddress()));
        handler.disconnected(channel);
    }

    @Override
    public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) {
        cause.printStackTrace();
        ctx.close();
    }

    public Map<String, Channel> getChannels() {
        return channels;
    }
}

