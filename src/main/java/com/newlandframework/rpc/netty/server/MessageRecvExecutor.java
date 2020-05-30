package com.newlandframework.rpc.netty.server;

import com.google.common.util.concurrent.FutureCallback;
import com.google.common.util.concurrent.Futures;
import com.google.common.util.concurrent.ListenableFuture;
import com.google.common.util.concurrent.ListeningExecutorService;
import com.google.common.util.concurrent.MoreExecutors;

import com.newlandframework.rpc.jmx.ModuleMetricsHandler;
import io.netty.bootstrap.ServerBootstrap;
import io.netty.channel.ChannelFuture;
import io.netty.channel.ChannelFutureListener;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelOption;
import io.netty.channel.EventLoopGroup;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.nio.NioServerSocketChannel;

import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.logging.Level;

import com.newlandframework.rpc.core.RpcSystemConfig;
import com.newlandframework.rpc.parallel.NamedThreadFactory;
import com.newlandframework.rpc.parallel.RpcThreadPool;
import com.newlandframework.rpc.model.MessageKeyVal;
import com.newlandframework.rpc.model.MessageRequest;
import com.newlandframework.rpc.model.MessageResponse;
import com.newlandframework.rpc.serialize.RpcSerializeProtocol;
import com.newlandframework.rpc.compiler.AccessAdaptiveProvider;
import com.newlandframework.rpc.core.AbilityDetailProvider;
import com.newlandframework.rpc.netty.resolver.ApiEchoResolver;

import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;

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
public class MessageRecvExecutor implements ApplicationContextAware {

    //Rpc服务器的IP地址和端口号
    private String serverAddress;

    //服务能力开放的端口
    private int echoApiPort;

    //传输数据所使用的序列化协议
    private RpcSerializeProtocol serializeProtocol = RpcSerializeProtocol.JDKSERIALIZE;

    private static final String DELIMITER = RpcSystemConfig.DELIMITER;

    //SYSTEM_PROPERTY_PARALLEL的值为处理器的数量
    private static final int PARALLEL = RpcSystemConfig.SYSTEM_PROPERTY_PARALLEL * 2;

    //线程的数量，默认为16
    private static int threadNums = RpcSystemConfig.SYSTEM_PROPERTY_THREADPOOL_THREAD_NUMS;

    //队列的数量，默认为-1
    private static int queueNums = RpcSystemConfig.SYSTEM_PROPERTY_THREADPOOL_QUEUE_NUMS;

    //线程池，也是使用单例模式
    private static volatile ListeningExecutorService threadPoolExecutor;

    private Map<String, Object> handlerMap = new ConcurrentHashMap<String, Object>();

    private int numberOfEchoThreadsPool = 1;

    //在此类中用来创建Netty中的worker group中的线程，并且不是守护线程
    private ThreadFactory threadRpcFactory = new NamedThreadFactory("NettyWorkerThreadPool");

    EventLoopGroup boss = new NioEventLoopGroup();

    EventLoopGroup worker = new NioEventLoopGroup(PARALLEL, threadRpcFactory);

    private MessageRecvExecutor() {
        handlerMap.clear();

        //提前往handlerMap中放入一些对象，比如AbilityDetailProvider
        register();
    }

    private static class MessageRecvExecutorHolder {
        static final MessageRecvExecutor INSTANCE = new MessageRecvExecutor();
    }

    //通过Holder模式实现了单例模式，即MessageRecvExecutor在整个RPC服务器中是唯一的
    public static MessageRecvExecutor getInstance() {
        return MessageRecvExecutorHolder.INSTANCE;
    }

    public static void submit(Callable<Boolean> task, final ChannelHandlerContext ctx,
                              final MessageRequest request, final MessageResponse response) {
        if (threadPoolExecutor == null) {
            synchronized (MessageRecvExecutor.class) {
                if (threadPoolExecutor == null) {
                    threadPoolExecutor = MoreExecutors.listeningDecorator(
                            (ThreadPoolExecutor) RpcThreadPool.getExecutor(threadNums, queueNums));
                }
            }
        }

        ListenableFuture<Boolean> listenableFuture = threadPoolExecutor.submit(task);
        Futures.addCallback(listenableFuture, new FutureCallback<Boolean>() {
            @Override
            public void onSuccess(Boolean result) {
                ctx.writeAndFlush(response).addListener(new ChannelFutureListener() {
                    @Override
                    public void operationComplete(ChannelFuture channelFuture) throws Exception {
                        System.out.println("RPC Server Send message-id respone:" + request.getMessageId());
                    }
                });
            }

            @Override
            public void onFailure(Throwable t) {
                t.printStackTrace();
            }
        }, threadPoolExecutor);
    }

    @Override
    public void setApplicationContext(ApplicationContext ctx) throws BeansException {
        try {
            MessageKeyVal keyVal = (MessageKeyVal) ctx.getBean(Class.forName("com.newlandframework.rpc.model.MessageKeyVal"));
            Map<String, Object> rpcServiceObject = keyVal.getMessageKeyVal();

            Set s = rpcServiceObject.entrySet();
            Iterator<Map.Entry<String, Object>> it = s.iterator();
            Map.Entry<String, Object> entry;

            while (it.hasNext()) {
                entry = it.next();
                handlerMap.put(entry.getKey(), entry.getValue());
            }
        } catch (ClassNotFoundException ex) {
            java.util.logging.Logger.getLogger(MessageRecvExecutor.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    public void start() {
        try {
            ServerBootstrap bootstrap = new ServerBootstrap();
            bootstrap.group(boss, worker).channel(NioServerSocketChannel.class)
                    //根据用户所选择的序列化协议不同，往NioSocketChannel对应的pipeline中添加不同的handler的类型也不同
                    .childHandler(new MessageRecvChannelInitializer(handlerMap).buildRpcSerializeProtocol(serializeProtocol))
                    .option(ChannelOption.SO_BACKLOG, 128)
                    .childOption(ChannelOption.SO_KEEPALIVE, true);
            // MessageRecvExecutor.DELIMITER = ":"，将IP地址和端口号分割成数组
            String[] ipAddr = serverAddress.split(MessageRecvExecutor.DELIMITER);
            // IP_PORT_ARRAY_LENGTH = 2
            if (ipAddr.length == RpcSystemConfig.IP_PORT_ARRAY_LENGTH) {
                final String host = ipAddr[0];
                final int port = Integer.parseInt(ipAddr[1]);
                ChannelFuture future = null;

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
                            final ExecutorService executor = Executors.newFixedThreadPool(numberOfEchoThreadsPool);
                            //把指定RPC服务器监听指定ip地址的过程放到线程池中去执行
                            executor.submit(new ApiEchoResolver(host, echoApiPort));
                            System.out.printf("Netty RPC Server start success!\n【ip】:%s\n【port】:%d\n【protocol】:%s\n" +
                                            "【start-time】:%s\n【jmx-invoke-metrics】:%s\n",
                                    host, port, serializeProtocol, ModuleMetricsHandler.getStartTime(), (RpcSystemConfig.SYSTEM_PROPERTY_JMX_METRICS_SUPPORT ?
                                            "open" : "close"));
                            channelFuture.channel().closeFuture().sync().addListener(new ChannelFutureListener() {
                                @Override
                                public void operationComplete(ChannelFuture future) throws Exception {
                                    executor.shutdownNow();
                                }
                            });
                        }
                    }
                });
            } else {
                System.err.printf("Netty RPC Server start fail!\n");
            }
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    public void stop() {
        worker.shutdownGracefully();
        boss.shutdownGracefully();
    }

    private void register() {
        // handlerMap中的键值对类型有以下几种可能：
        // String -> ServiceFilterBinder对象、String -> AccessAdaptiveProvider对象、String -> AbilityDetailProvider对象
        handlerMap.put(RpcSystemConfig.RPC_COMPILER_SPI_ATTR, new AccessAdaptiveProvider());
        handlerMap.put(RpcSystemConfig.RPC_ABILITY_DETAIL_SPI_ATTR, new AbilityDetailProvider());
    }

    public Map<String, Object> getHandlerMap() {
        return handlerMap;
    }

    public void setHandlerMap(Map<String, Object> handlerMap) {
        this.handlerMap = handlerMap;
    }

    public String getServerAddress() {
        return serverAddress;
    }

    public void setServerAddress(String serverAddress) {
        this.serverAddress = serverAddress;
    }

    public RpcSerializeProtocol getSerializeProtocol() {
        return serializeProtocol;
    }

    public void setSerializeProtocol(RpcSerializeProtocol serializeProtocol) {
        this.serializeProtocol = serializeProtocol;
    }

    public int getEchoApiPort() {
        return echoApiPort;
    }

    public void setEchoApiPort(int echoApiPort) {
        this.echoApiPort = echoApiPort;
    }
}
