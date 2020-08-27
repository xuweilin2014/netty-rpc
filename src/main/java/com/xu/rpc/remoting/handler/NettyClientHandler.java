package com.xu.rpc.remoting.handler;

import io.netty.buffer.Unpooled;
import io.netty.channel.*;

import java.net.SocketAddress;
import java.util.concurrent.ConcurrentHashMap;

import com.xu.rpc.core.MessageCallBack;
import com.xu.rpc.model.MessageRequest;
import com.xu.rpc.model.MessageResponse;


public class NettyClientHandler extends ChannelDuplexHandler{
    // 用来存储正在进行中，还没有返回的RPC调用
    private ConcurrentHashMap<String, MessageCallBack> mapCallBack = new ConcurrentHashMap<String, MessageCallBack>();
    private volatile Channel channel;
    private SocketAddress remoteAddr;

    public NettyClientHandler(ChannelHandler handler){

    }

    public Channel getChannel() {
        return channel;
    }

    public SocketAddress getRemoteAddr() {
        return remoteAddr;
    }

    @Override
    public void channelActive(ChannelHandlerContext ctx) throws Exception {
        super.channelActive(ctx);
        this.remoteAddr = this.channel.remoteAddress();
    }

    @Override
    public void channelRegistered(ChannelHandlerContext ctx) throws Exception {
        super.channelRegistered(ctx);
        this.channel = ctx.channel();
    }

    @Override
    public void channelRead(ChannelHandlerContext ctx, Object msg) throws Exception {
        MessageResponse response = (MessageResponse) msg;
        String messageId = response.getMessageId();
        MessageCallBack callBack = mapCallBack.get(messageId);
        if (callBack != null) {
            mapCallBack.remove(messageId);
            callBack.over(response);
        }
    }

    @Override
    public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) throws Exception {
        cause.printStackTrace();
        ctx.close();
    }

    public void close() {
        channel.writeAndFlush(Unpooled.EMPTY_BUFFER).addListener(ChannelFutureListener.CLOSE);
    }

    public MessageCallBack sendRequest(MessageRequest request) {
        MessageCallBack callBack = new MessageCallBack(request);
        mapCallBack.put(request.getMessageId(), callBack);
        channel.writeAndFlush(request);
        return callBack;
    }
}
