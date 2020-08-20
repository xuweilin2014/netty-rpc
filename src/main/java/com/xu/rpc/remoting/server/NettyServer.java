package com.xu.rpc.remoting.server;

import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.parallel.NamedThreadFactory;
import com.xu.rpc.remoting.handler.ChannelHandler;
import com.xu.rpc.remoting.handler.ChannelHandlers;
import com.xu.rpc.remoting.handler.NettyServerHandler;
import com.xu.rpc.serialize.Serialization;
import com.xu.rpc.util.URL;
import io.netty.bootstrap.ServerBootstrap;
import io.netty.channel.*;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import org.apache.log4j.Logger;

import java.util.Date;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ThreadFactory;

/**
 * MessageRecvExecutor实现了 ApplicationContextAware 接口：
 *
 * 在大部分情况下，容器中的Bean处于容器管理下，无需主动访问容器，只需接受容器的依赖注入即可。但在某些特殊的情况下，Bean需要实现某个功能，
 * 但该功能必须借助于Spring容器才能实现，此时就必须让该Bean先获取Spring容器，然后借助于Spring容器实现该功能。为了让Bean获取它所在的Spring容器，
 * 可以让该Bean实现ApplicationContextAware接口。
 *
 * Spring容器会检测容器中的所有Bean，如果发现某个Bean实现了ApplicationContextAware接口，Spring容器会在创建该Bean之后，
 * 自动调用该Bean的setApplicationContextAware()方法，调用该方法时，会将容器本身作为参数传给该方法。
 */
public class NettyServer extends AbstractServer {

    public static final Logger logger = Logger.getLogger(NettyServer.class);

    // Rpc服务器的IP地址
    private String host;

    // Rpc服务器的端口号
    private Integer port;

    //传输数据所使用的序列化协议
    private Serialization serialization;

    //SYSTEM_PROPERTY_PARALLEL的值为处理器的数量
    private static final int PARALLEL = RpcConfig.SYSTEM_PROPERTY_PARALLEL * 2;

    //在此类中用来创建Netty中的worker group中的线程，并且不是守护线程
    private ThreadFactory threadRpcFactory = new NamedThreadFactory("NettyWorkerThreadPool");

    EventLoopGroup boss = new NioEventLoopGroup();

    EventLoopGroup worker = new NioEventLoopGroup(PARALLEL, threadRpcFactory);

    private Map<String, Channel> channels = new ConcurrentHashMap<>();

    @SuppressWarnings("ConstantConditions")
    public NettyServer(URL url, ChannelHandler handler) {
        super(url, ChannelHandlers.wrapHandler(handler));

        String serialize = url.getParameter(RpcConfig.SERIALIZE, RpcConfig.JDK_SERIALIZE);
        this.serialization = Enum.valueOf(Serialization.class, serialize);

        this.host = url.getParameter(RpcConfig.HOST);
        if (host == null || host.length() == 0)
            throw new IllegalStateException("the netty server cannot open with host being empty.");

        try {
            this.port = Integer.parseInt(url.getParameter(RpcConfig.PORT));
        } catch (NumberFormatException e) {
            throw new IllegalStateException("port value is invalid");
        }
    }

    @Override
    public void doOpen() {
        try {
            NettyServerHandler serverHandler = new NettyServerHandler(handler);
            ServerBootstrap bootstrap = new ServerBootstrap();
            bootstrap.group(boss, worker).channel(NioServerSocketChannel.class)
                    // 根据用户所选择的序列化协议不同，往NioSocketChannel对应的pipeline中添加不同的handler的类型也不同
                    .childHandler(new ServerChannelInitializer(serialization, serverHandler))
                    .option(ChannelOption.SO_BACKLOG, 128)
                    .childOption(ChannelOption.SO_KEEPALIVE, true);

            ChannelFuture future;

            channels = serverHandler.getChannels();
            future = bootstrap.bind(host, port).sync();
            future.addListener(new ChannelFutureListener() {
                /**
                 * 在RPC服务器启动时，指定其监听一个ip地址127.0.0.1:18887，客户端发送调用请求到这个客户端。
                 * 不过当指定RPC服务器监听上面这个ip地址之后，还必须让RPC服务器监听另外一个ip地址127.0.0.1:18886。
                 * 这个端口是用来监听浏览器发送过来的http请求，然后把Rpc服务器可以提供的服务（也就是各个接口中的方法签名）
                 * 展示在网页中，让用户可以直接知道.
                 *
                 * 在NettyRpcRegistry和NettyRpcReference中分别启动服务器监听和客户端，都是非阻塞进行的
                 */
                @Override
                public void operationComplete(final ChannelFuture channelFuture) throws Exception {
                    if (channelFuture.isSuccess()) {
                        logger.info("netty rpc Server start success! ip: " + host + " port:" + port +
                                " start-time: " + new Date() + " jmx-invoke-metrics: " + (url.getParameter(RpcConfig.METRICS, true) ?
                                "open" : "close"));
                    }
                }
            });

        } catch (InterruptedException e) {
            logger.error("failed to start netty rpc server. caused by " + e.getMessage());
        }
    }

    @Override
    public void doClose() {
        // TODO: 2020/8/13
    }

}
