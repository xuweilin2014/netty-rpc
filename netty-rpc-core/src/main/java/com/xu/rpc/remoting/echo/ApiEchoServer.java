package com.xu.rpc.remoting.echo;

import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.commons.URL;
import io.netty.bootstrap.ServerBootstrap;
import io.netty.channel.*;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.SocketChannel;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import io.netty.handler.codec.http.HttpServerCodec;
import org.apache.log4j.Logger;


public class ApiEchoServer{

    public static final Logger logger = Logger.getLogger(ApiEchoHandler.class);

    private EventLoopGroup bossGroup;

    private EventLoopGroup workerGroup;

    private Channel channel;

    private String host;

    private int port;

    private URL url;

    public ApiEchoServer(URL url) {
        this.url = url;
        this.host = url.getHost();
        this.port = url.getParameter(RpcConfig.ECHO_PORT_KEY, RpcConfig.ECHO_PORT);
    }
    
    public void start() {
        this.bossGroup = new NioEventLoopGroup(1);
        this.workerGroup = new NioEventLoopGroup();

        try {
            ServerBootstrap b = new ServerBootstrap();
            b.option(ChannelOption.SO_BACKLOG, 1024);
            b.group(bossGroup, workerGroup)
                    .channel(NioServerSocketChannel.class)
                    /*
                     * 这里的childHandler方法是当有新连接建立时（即NioSocketChannel），把ApiEchoInitializer添加到这个
                     * channel对应的pipeline中，然后当此channel最终注册到某个NioEventLoop上时，回调这个ApiEchoInitializer
                     * 中的handlerAdded方法，最终调用到其中的initChannel方法，初始化新连接，往其中添加各种handler
                     */
                    .childHandler(new ChannelInitializer<SocketChannel>() {
                        @Override
                        protected void initChannel(SocketChannel ch) throws Exception {
                            ChannelPipeline p = ch.pipeline();
                            p.addLast(new HttpServerCodec());
                            p.addLast(new ApiEchoHandler(url, host, port));
                        }
                    });

            this.channel = b.bind(host, port).sync().channel();

            logger.info("echo server starts successfully! netty-rpc server api interface:" + "http" + "://" + host + ":" + port + "/netty-rpc/ability");
            if (url.getParameter(RpcConfig.METRICS_KEY, true)){
                logger.info("echo server starts successfully! netty-rpc server metrics:" + "http" + "://" + host + ":" + port + "/netty-rpc/metrics");
            }

        } catch (Throwable e) {
            logger.error("failed to start echo server, caused by: " + e.getMessage());
        }
    }
    
    public void stop(){
        try {
            channel.close();
        } catch (Exception e) {
            logger.warn("failed to close the echo server channel " + channel);
        }

        try {
            bossGroup.shutdownGracefully();
            workerGroup.shutdownGracefully();
        } catch (Exception e) {
            logger.warn("failed to shutdown the event loop group.");
        }
    }

    public String getHost() {
        return host;
    }

    public int getPort(){
        return RpcConfig.ECHO_PORT;
    }
}

