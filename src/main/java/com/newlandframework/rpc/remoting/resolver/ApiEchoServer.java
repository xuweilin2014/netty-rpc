package com.newlandframework.rpc.remoting.resolver;

import com.newlandframework.rpc.core.RpcSystemConfig;
import io.netty.bootstrap.ServerBootstrap;
import io.netty.channel.Channel;
import io.netty.channel.ChannelOption;
import io.netty.channel.EventLoopGroup;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import io.netty.handler.logging.LogLevel;
import io.netty.handler.logging.LoggingHandler;
import io.netty.handler.ssl.SslContext;
import io.netty.handler.ssl.SslContextBuilder;
import io.netty.handler.ssl.util.SelfSignedCertificate;
import org.apache.log4j.Logger;

import java.util.concurrent.Callable;


public class ApiEchoServer{

    public static final Logger logger = Logger.getLogger(ApiEchoHandler.class);

    private static final boolean SSL = System.getProperty("ssl") != null;

    private String host;

    private int port;
    
    private boolean started = false;

    public ApiEchoServer(String host, int port) {
        this.host = host;
        this.port = port;
    }
    
    public void start() {
        EventLoopGroup bossGroup = new NioEventLoopGroup(1);
        EventLoopGroup workerGroup = new NioEventLoopGroup();

        try {
            SslContext sslCtx = null;
            if (SSL) {
                SelfSignedCertificate ssc = new SelfSignedCertificate();
                sslCtx = SslContextBuilder.forServer(ssc.certificate(), ssc.privateKey()).build();
            }

            ServerBootstrap b = new ServerBootstrap();
            b.option(ChannelOption.SO_BACKLOG, 1024);
            b.group(bossGroup, workerGroup)
                    .channel(NioServerSocketChannel.class)
                    .handler(new LoggingHandler(LogLevel.INFO))
                    /*
                     * 这里的childHandler方法是当有新连接建立时（即NioSocketChannel），把ApiEchoInitializer添加到这个
                     * channel对应的pipeline中，然后当此channel最终注册到某个NioEventLoop上时，回调这个ApiEchoInitializer
                     * 中的handlerAdded方法，最终调用到其中的initChannel方法，初始化新连接，往其中添加各种handler
                     */
                    .childHandler(new ApiEchoInitializer(sslCtx));

            Channel ch = b.bind(host, port).sync().channel();

            logger.info("NettyRPC server api interface:" + (SSL ? "https" : "http") + "://" + host + ":" + port + "/NettyRPC.html");
            if (RpcSystemConfig.SYSTEM_PROPERTY_JMX_METRICS_SUPPORT){
                logger.info("NettyRPC server metrics:" +
                        (SSL ? "https" : "http") + "://" + host + ":" + port + "/NettyRPC.html/metrics");
            }

            ch.closeFuture().sync();
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            bossGroup.shutdownGracefully();
            workerGroup.shutdownGracefully();
        }
    }
    
    public void stop(){
        // TODO: 2020/8/15  
    }
}

