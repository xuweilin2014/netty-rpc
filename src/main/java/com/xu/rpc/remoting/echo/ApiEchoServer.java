package com.xu.rpc.remoting.echo;

import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.commons.URL;
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


public class ApiEchoServer{

    public static final Logger logger = Logger.getLogger(ApiEchoHandler.class);

    private static final boolean SSL = System.getProperty("ssl") != null;

    private EventLoopGroup bossGroup;

    private EventLoopGroup workerGroup;

    private Channel channel;

    private String host;

    private URL url;

    public ApiEchoServer(URL url) {
        this.url = url;
        this.host = url.getHost();
    }
    
    public void start() {
        this.bossGroup = new NioEventLoopGroup(1);
        this.workerGroup = new NioEventLoopGroup();

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

            this.channel = b.bind(host, RpcConfig.ECHO_PORT).sync().channel();

            logger.info("netty-rpc server api interface:" + (SSL ? "https" : "http") + "://" + host + ":" + RpcConfig.ECHO_PORT + "/netty-rpc.html");
            if (url.getParameter(RpcConfig.METRICS, true)){
                logger.info("netty-rpc server metrics:" + (SSL ? "https" : "http") + "://" + host + ":" + RpcConfig.ECHO_PORT + "/netty-rpc.html/metrics");
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
}

