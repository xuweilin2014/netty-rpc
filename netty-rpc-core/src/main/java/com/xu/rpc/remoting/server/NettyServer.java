package com.xu.rpc.remoting.server;

import com.xu.rpc.remoting.exchanger.NettyChannel;
import com.xu.rpc.remoting.exchanger.RpcChannel;
import com.xu.rpc.remoting.handler.ChannelHandler;
import com.xu.rpc.remoting.handler.ChannelHandlers;
import com.xu.rpc.remoting.handler.ExchangeHandler;
import com.xu.rpc.serialize.RpcChannelInitializer;
import com.xu.rpc.serialize.Serialization;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.commons.parallel.NamedThreadFactory;
import com.xu.rpc.remoting.handler.NettyServerHandler;
import com.xu.rpc.commons.URL;
import io.netty.bootstrap.ServerBootstrap;
import io.netty.buffer.PooledByteBufAllocator;
import io.netty.channel.*;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import org.apache.log4j.Logger;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

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
public class NettyServer implements Server{

    public static final Logger logger = Logger.getLogger(NettyServer.class);

    // Rpc服务器的IP地址
    private String host;

    // Rpc服务器的端口号
    private Integer port;

    //SYSTEM_PROPERTY_PARALLEL的值为处理器的数量
    private static final int PARALLEL = RpcConfig.SYSTEM_PROPERTY_PARALLEL * 2;

    //在此类中用来创建Netty中的worker group中的线程，并且是守护线程
    private ThreadFactory threadRpcFactory = new NamedThreadFactory("NettyWorkerThread", true);

    private EventLoopGroup boss = new NioEventLoopGroup();

    private EventLoopGroup worker = new NioEventLoopGroup(PARALLEL, threadRpcFactory);

    protected com.xu.rpc.remoting.handler.ChannelHandler handler;

    protected URL url;

    private Map<String, RpcChannel> channels = new ConcurrentHashMap<>();

    private RpcChannel channel;

    private final AtomicBoolean closed = new AtomicBoolean(false);

    @SuppressWarnings("ConstantConditions")
    public NettyServer(URL url, ChannelHandler handler) {
        this.url = url;
        this.handler = ChannelHandlers.wrapHandler(handler);

        this.host = url.getHost();
        if (host == null || host.length() == 0)
            throw new IllegalStateException("the netty server cannot open with host being empty.");

        try {
            this.port = url.getPort();
        } catch (NumberFormatException e) {
            throw new IllegalStateException("port value is invalid");
        }

        // 打开 rpc 服务器
        open();
    }

    public void open() {
        try {
            NettyServerHandler serverHandler = new NettyServerHandler(handler, url);
            ServerBootstrap bootstrap = new ServerBootstrap();
            bootstrap.group(boss, worker).channel(NioServerSocketChannel.class)
                    // 根据用户所选择的序列化协议不同，往NioSocketChannel对应的pipeline中添加不同的handler的类型也不同
                    .childHandler(new RpcChannelInitializer(url, serverHandler))
                    .option(ChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT)
                    .childOption(ChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT)
                    // 服务器配置项：BACKLOG
                    // TCP维护有两个队列，分别称为A和B, 客户端发送SYN，服务器接收到后发送SYN ACK，将客户端放入到A队列
                    // 客户端接收到后再次发送ACK，服务器接收到后将客户端从A队列移至B队列，服务器的accept返回。A和B队列长度之和为backlog,
                    // 当A和B队列长度之和大于backlog时，新的连接会被TCP内核拒绝。注意：backlog对程序的连接数并无影响，影响的只是还没有被accept取出的连接数。
                    .option(ChannelOption.SO_BACKLOG, 128)
                    //指定接收缓冲区大小
                    .option(ChannelOption.SO_RCVBUF, 32 * 1024);

            ChannelFuture future;

            channels = serverHandler.getChannels();
            future = bootstrap.bind(host, port).sync();
            this.channel = NettyChannel.getChannel(future.channel(), url);
            future.addListener(new ChannelFutureListener() {
                /*
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
                                " start-time: " + new Date() + " jmx-invoke-metrics: " + (url.getParameter(RpcConfig.METRICS_KEY, true) ?
                                "open" : "close"));
                    }
                }
            });

        } catch (InterruptedException e) {
            logger.error("failed to start netty rpc server. caused by " + e.getMessage());
        }
    }

    // 返回连接到这个服务器的所有客户端连接
    @Override
    public List<RpcChannel> getChannels() {
        List<RpcChannel> rpcChannels = new ArrayList<>(channels.values());
        return Collections.unmodifiableList(rpcChannels);
    }

    @Override
    public void close(int timeout) {
        if (!closed.compareAndSet(false, true))
            return;

        gracefulShutdown(ExchangeHandler.getExecutor(), timeout);

        if (channel != null){
            try {
                channel.close();
            } catch (Exception e) {
                logger.warn("error occurs when trying to close the channel " + channel);
            }
        }

        for (RpcChannel channel : channels.values()) {
            try {
                channel.close();
            } catch (Exception e) {
                logger.warn("error occurs when trying to close the channel " + channel);
            } finally {
                channels.clear();
            }
        }

        try {
            worker.shutdownGracefully();
            boss.shutdownGracefully();
        } catch (Exception e) {
            logger.warn("error occurs when trying to shutdown the event loop group.");
        }
    }

    private void gracefulShutdown(ExecutorService executor, int timeout) {
        if (executor.isShutdown())
            return;
        // 关闭线程池，将线程池的状态设置为 shutdown，不会再接收新的任务
        executor.shutdown();
        try {
            // 等待 timeout 时间，让线程池中正在执行的任务执行完毕，如果在 timeout 时间内执行完毕，那么返回 true
            // 否则，返回 false
            if (!executor.awaitTermination(timeout, TimeUnit.MILLISECONDS)){
                // 如果还有任务没有执行完毕，就调用 shutdownNow，会调用每个线程的 interrupt 方法，
                // 当然，线程处于阻塞状态，就会立即退出，抛出异常，否则其它状态时，线程不一定会停止运行
                executor.shutdownNow();
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            executor.shutdownNow();
        }
    }

    @Override
    public boolean isClosed() {
        return closed.get();
    }

    @Override
    public URL getUrl() {
        return url;
    }
}
