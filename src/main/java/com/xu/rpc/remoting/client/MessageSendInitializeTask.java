package com.xu.rpc.remoting.client;

import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.remoting.handler.NettyClientHandler;
import io.netty.bootstrap.Bootstrap;
import io.netty.channel.*;
import io.netty.channel.socket.nio.NioSocketChannel;

import java.net.InetSocketAddress;
import java.util.concurrent.Callable;
import java.util.concurrent.TimeUnit;

import com.xu.rpc.serialize.Serialization;

/**
 * 此Task交给线程池来执行，它的主要任务是连接到服务器端，并且如果没有连上的话，就每隔10s重试一次
 */
public class MessageSendInitializeTask implements Callable<Boolean> {

    private EventLoopGroup eventLoopGroup = null;
    private InetSocketAddress remoteAddr;
    private Serialization protocol;

    MessageSendInitializeTask(EventLoopGroup eventLoopGroup, InetSocketAddress remoteAddr, Serialization protocol) {
        this.eventLoopGroup = eventLoopGroup;
        this.remoteAddr = remoteAddr;
        this.protocol = protocol;
    }

    @Override
    public Boolean call() {
        Bootstrap b = new Bootstrap();
        b.group(eventLoopGroup)
                .channel(NioSocketChannel.class)
                .option(ChannelOption.SO_KEEPALIVE, true)
                .remoteAddress(remoteAddr);
        b.handler(new ClientChannelInitializer().buildRpcSerializeProtocol(protocol));

        ChannelFuture channelFuture = b.connect();
        channelFuture.addListener(new ChannelFutureListener() {
            @Override
            public void operationComplete(final ChannelFuture channelFuture) throws Exception {
                if (channelFuture.isSuccess()) {
                    //如果客户端连接到服务器成功的话，就把此pipeline中的MessageSendHandler保存到RpcServerLoader中
                    NettyClientHandler handler = channelFuture.channel().pipeline().get(NettyClientHandler.class);
                    RpcServerLoader.getInstance().setMessageSendHandler(handler);
                } else {
                    //如果客户端连接失败的话，则每隔10s再重试一次
                    EventLoop loop = (EventLoop) eventLoopGroup.schedule(new Runnable() {
                        @Override
                        public void run() {
                            System.out.println("NettyRPC server is down, start to reconnecting to: " + remoteAddr.getAddress().getHostAddress()
                                    + ':' + remoteAddr.getPort());
                            //失败后再调用call方法一次
                            call();
                        }
                    }, RpcConfig.SYSTEM_PROPERTY_CLIENT_RECONNECT_DELAY, TimeUnit.SECONDS);
                }
            }
        });
        return Boolean.TRUE;
    }
}
