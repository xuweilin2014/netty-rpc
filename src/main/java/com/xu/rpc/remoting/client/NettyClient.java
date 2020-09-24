package com.xu.rpc.remoting.client;

import com.xu.rpc.commons.URL;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.exception.RemotingException;
import com.xu.rpc.parallel.NamedThreadFactory;
import com.xu.rpc.remoting.exchanger.NettyChannel;
import com.xu.rpc.remoting.exchanger.RpcChannel;
import com.xu.rpc.remoting.handler.ChannelHandler;
import com.xu.rpc.remoting.handler.ChannelHandlers;
import com.xu.rpc.remoting.handler.NettyClientHandler;
import com.xu.rpc.remoting.initializer.RpcChannelInitializer;
import com.xu.rpc.serialize.Serialization;
import io.netty.bootstrap.Bootstrap;
import io.netty.channel.Channel;
import io.netty.channel.ChannelFuture;
import io.netty.channel.ChannelOption;
import io.netty.channel.EventLoopGroup;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.nio.NioSocketChannel;
import org.apache.log4j.Logger;

import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class NettyClient implements Client {

    public static final Logger logger = Logger.getLogger(NettyClient.class);

    private Serialization serialization;

    private String host;

    private int port;

    private Bootstrap bootstrap;

    private volatile RpcChannel channel;

    // 客户端NioEventLoopGroup中NioEventLoop的数量大小
    private static final int PARALLEL = RpcConfig.SYSTEM_PROPERTY_PARALLEL * 2;

    // 使用 NamedThreadFactory 来创建Netty中的worker group中的线程，并且是守护线程。这么做事因为，当程序退出 main 方法之后（main线程是用户线程），
    // JVM 还会继续等待所有的非守护线程（用户线程）执行完毕，所以必须设置为守护线程，否则无法进入优雅停机逻辑
    private ThreadFactory threadRpcFactory = new NamedThreadFactory("NettyWorkerThread", true);

    private EventLoopGroup eventLoopGroup = new NioEventLoopGroup(PARALLEL, threadRpcFactory);

    private volatile AtomicBoolean closed = new AtomicBoolean(false);

    protected URL url;

    protected ChannelHandler handler;

    protected int timeout;

    private static ScheduledThreadPoolExecutor reconnectExecutor = new ScheduledThreadPoolExecutor(2, new NamedThreadFactory("RpcClientReconnectThread", true));

    private ScheduledFuture<?> reconnectFuture;

    private long lastConnectedTime = System.currentTimeMillis();

    private final Lock lock = new ReentrantLock();

    public NettyClient(URL url, ChannelHandler handler) throws RemotingException {
        this.url = url;
        this.handler = ChannelHandlers.wrapHandler(handler);

        // 客户端发送数据的序列化协议，默认为JDK自带的序列化方法
        String serialize = url.getParameter(RpcConfig.SERIALIZE, RpcConfig.JDK_SERIALIZE).toUpperCase();
        this.serialization = Enum.valueOf(Serialization.class, serialize);

        this.host = url.getHost();
        if (host == null || host.length() == 0)
            throw new IllegalStateException("consumer cannot connect to provider with host being empty. " +
                    "url for the provider :" + url);
        this.port = url.getPort();

        try {
            open();
            logger.info("netty client opens successfully, and it will start to listen to address " + host + ":" + port);
        } catch (Throwable t) {
            close();
            // 客户端启动不了可以算做是比较严重的错误，所以必须抛出异常
            throw new RemotingException("failed to start client. caused by " + t.getMessage());
        }

        try {
            connect();
        } catch (Throwable t) {
            close();
            throw new RemotingException("failed to connect to server, waiting for retry. caused by " + t.getMessage());
        }
    }

    private void open(){
        NettyClientHandler clientHandler = new NettyClientHandler(handler, url);
        bootstrap = new Bootstrap();
        bootstrap.group(eventLoopGroup)
                .channel(NioSocketChannel.class)
                .option(ChannelOption.SO_KEEPALIVE, true)
                .option(ChannelOption.TCP_NODELAY, true)
                .handler(new RpcChannelInitializer(serialization, clientHandler));
    }

    private void connect() throws RemotingException {
        lock.lock();
        try{
            if (isConnected())
                return;

            // 开启连接检测线程，每隔固定时间检测连接是否有效,如果连接有效，直接返回；否则，尝试重新连接，并且超过一定时间连接不上打印错误日志
            startReconnectThread();
            doConnect();

            if (!isConnected()){
                throw new RemotingException("failed to connect to server :" + getRemoteAddress() + " from client: " + getLocalAddress());
            }else {
                logger.info("successfully connected to server :" + getRemoteAddress() + " from client " + getClass().getSimpleName()
                        + " with address " + getLocalAddress());
            }
        }catch (Throwable t){
           throw new RemotingException("failed to connect to server :" + getRemoteAddress() + " from client: " + getLocalAddress()
                    + ", caused by " + t.getMessage());
        } finally {
            lock.unlock();
        }
    }

    private void doConnect() throws RemotingException {
        ChannelFuture channelFuture = bootstrap.connect(new InetSocketAddress(host, port));
        boolean res = channelFuture.awaitUninterruptibly(3000, TimeUnit.MILLISECONDS);

        // 连接到服务器成功的话，如果存在旧的连接，必须先把旧的连接关闭掉
        if (res && channelFuture.isSuccess()){
            RpcChannel newChannel = NettyChannel.getChannel(channelFuture.channel(), getUrl());

            try{
                RpcChannel oldChannel = this.channel;
                if (oldChannel != null){
                    logger.info("closing old channel: " + oldChannel);
                    oldChannel.close();
                }
            }finally {
                if (isClosed()){
                    try{
                        if (newChannel != null)
                            newChannel.close();
                    }finally {
                        this.channel = null;
                    }
                }else{
                    this.channel = newChannel;
                    logger.info("new channel:" + newChannel);
                }
            }
        }else {
            throw new RemotingException("connect to server failed! server address is " + getRemoteAddress() + " , client address is "
                                            + getLocalAddress());
        }
    }

    public void startReconnectThread(){
        if (reconnectFuture == null || reconnectFuture.isCancelled()){
            // 默认每隔 2000ms 执行一次，检查客户端到服务器端的连接是否断开，如果断开则进行重新连接
            // 如果重连时间超过了 timeout 时间限制就打印错误日志
            reconnectFuture = reconnectExecutor.scheduleWithFixedDelay(new Runnable() {
                @Override
                public void run() {
                    try{
                        if (isConnected()){
                            lastConnectedTime = System.currentTimeMillis();
                        }else {
                            connect();
                        }
                    }catch (Throwable t){
                        if (System.currentTimeMillis() - lastConnectedTime >= RpcConfig.RECONNECT_TIMEOUT) {
                            if (!isConnected()) {
                                logger.error("Failed to reconnect to server: " + getRemoteAddress() + " from client: "
                                        + getLocalAddress() + " with url [" + url.toFullString() + "]");
                            }
                        }
                    }
                }
            }, RpcConfig.RECONNECT_INTERVAL, RpcConfig.RECONNECT_INTERVAL, TimeUnit.MILLISECONDS);
        }
    }

    public void closeReconnectThread(){
        try{
            if (reconnectFuture != null && !reconnectFuture.isCancelled()){
                reconnectFuture.cancel(true);
                reconnectExecutor.purge();
            }
        }catch (Throwable t){
            logger.warn(t.getMessage());
        }
    }

    @Override
    public void send(Object message) throws RemotingException {
        if (channel == null)
            logger.warn("cannot send message to remote when channel still is null");

        if (isClosed()){
            logger.warn("client " + this.channel.getLocalAddress() + " is closed, cannot send message to remote " + channel.getRemoteAddress());
        }

        channel.send(message);
    }

    public boolean isClosed(){
        return closed.get();
    }

    @Override
    public URL getUrl() {
        return url;
    }

    @Override
    public void close(int timeout){
        if (closed.get())
            return;

        close();
    }

    private void close(){
        if (closed.compareAndSet(false, true)){
            try {
                // 不能关闭掉 reconnectExecutor，因为断线重连可能会需要把新的任务加入进去
                disconnect();
                eventLoopGroup.shutdownGracefully();
            } catch (Throwable e) {
                logger.warn(e.getMessage());
            }
        }
    }

    @Override
    public void reconnect() throws RemotingException {
        disconnect();
        connect();
    }

    public void disconnect() {
        lock.lock();
        try{
            if (channel != null) {
                // 阻塞直到 channel 关闭完成，否则可能会调用后面的 connect 方法时，channel 还没有关闭
                channel.close();
                logger.info("netty client is closed, close channel " + channel);
            }
            closeReconnectThread();
        }catch (Throwable t){
            logger.error(t.getMessage());
        }finally {
            lock.unlock();
        }
    }

    public synchronized boolean isConnected() {
        if (channel == null)
            return false;
        return channel.isConnected();
    }

    public SocketAddress getLocalAddress() {
        if (channel == null)
            return null;

        return channel.getLocalAddress();
    }

    public SocketAddress getRemoteAddress() {
        if (channel == null)
            return null;

        return channel.getRemoteAddress();
    }

    @Override
    public RpcChannel getChannel() {
        return channel;
    }

    @Override
    public String toString() {
        return "NettyClient [" + getLocalAddress() + "] -> [" + getRemoteAddress() + "]";
    }
}
