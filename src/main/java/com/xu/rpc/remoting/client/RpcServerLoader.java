package com.xu.rpc.remoting.client;

import com.google.common.util.concurrent.FutureCallback;
import com.google.common.util.concurrent.Futures;
import com.google.common.util.concurrent.ListenableFuture;
import com.google.common.util.concurrent.ListeningExecutorService;
import com.google.common.util.concurrent.MoreExecutors;

import com.xu.rpc.remoting.handler.NettyClientHandler;
import com.xu.rpc.serialize.Serialization;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.parallel.RpcThreadPool;

import io.netty.channel.EventLoopGroup;
import io.netty.channel.nio.NioEventLoopGroup;

import java.net.InetSocketAddress;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Level;
import java.util.logging.Logger;


public class RpcServerLoader {

    private static volatile RpcServerLoader rpcServerLoader;

    private static final String DELIMITER = RpcConfig.DELIMITER;

    //客户端发送数据的序列化协议，默认为JDK自带的序列化方法
    private Serialization serializationProtocol = Serialization.JDKSERIALIZE;

    //客户端NioEventLoopGroup中NioEventLoop的数量大小
    private static final int PARALLEL = RpcConfig.SYSTEM_PROPERTY_PARALLEL * 2;

    private EventLoopGroup eventLoopGroup = new NioEventLoopGroup(PARALLEL);

    //线程池中核心线程的数量，默认为16
    private static int threadNums = RpcConfig.SYSTEM_PROPERTY_THREADPOOL_THREAD_NUMS;

    //线程池中任务队列的大小
    private static int queueNums = RpcConfig.SYSTEM_PROPERTY_THREADPOOL_QUEUE_NUMS;

    //线程池
    private static ListeningExecutorService threadPoolExecutor =
            MoreExecutors.listeningDecorator((ThreadPoolExecutor) RpcThreadPool.getExecutor(threadNums, queueNums));

    private NettyClientHandler messageSendHandler = null;

    private Lock lock = new ReentrantLock();

    private Condition connectStatus = lock.newCondition();

    private Condition handlerStatus = lock.newCondition();

    private RpcServerLoader() {
    }

    //利用饱汉模式实现单例模式
    public static RpcServerLoader getInstance() {
        if (rpcServerLoader == null) {
            synchronized (RpcServerLoader.class) {
                if (rpcServerLoader == null) {
                    rpcServerLoader = new RpcServerLoader();
                }
            }
        }
        return rpcServerLoader;
    }

    public void load(String serverAddress, Serialization serializationProtocol) {
        String[] ipAddr = serverAddress.split(RpcServerLoader.DELIMITER);

        //RpcSystemConfig.IP_PORT_ARRAY_LENGTH的值为2
        if (ipAddr.length == RpcConfig.IP_PORT_ARRAY_LENGTH) {
            String host = ipAddr[0];
            int port = Integer.parseInt(ipAddr[1]);
            final InetSocketAddress remoteAddr = new InetSocketAddress(host, port);

            //将客户端连接服务器端的任务包装成一个Task，放入到线程池中去执行。这是因为客户端发起对服务器的连接是在SpringIoC容器启动，对
            //XML文件中的bean进行初始化的时候进行的，所以为了不让线程阻塞在这里，妨碍到其它bean的初始化工作，把这个任务交给其它线程来执行
            ListenableFuture<Boolean> listenableFuture = threadPoolExecutor.submit(
                    new MessageSendInitializeTask(eventLoopGroup, remoteAddr, serializationProtocol));

            Futures.addCallback(listenableFuture, new FutureCallback<Boolean>() {
                @Override
                public void onSuccess(Boolean result) {
                    try {
                        lock.lock();

                        /*
                         * 为什么必须要有handlerStatus这个Condition变量？加入把类中和handlerStatus相关的代码去除的话，会发生
                         * 以下情况：
                         *
                         * FutureCallback中的回调方法必须得等到提交的MessageSendInitializeTask执行完（这里不包括MessageSendInitializeTask#call
                         * 方法中，addListener添加的监听器中的代码，这部分代码由另外的线程来执行），才会被调用，并且由threadPoolExecutor中的其它线程来调用执行.
                         *
                         * 因此，这样就会导致一个问题，就是MessageSendInitializeTask#call方法执行完之后，可能会立即执行FutureCallback中的
                         * 方法，即执行connectStatus.signalAll，通知在getMessageSendHandler中阻塞的线程，尽管此时messageSendHandler可能还没有被设置，
                         * 即handler为null。在getMessageSendHandler中，被阻塞的线程被唤醒，但是此时handler依然为null，所以继续阻塞。
                         *
                         * 而随后addListener中的监听器中的代码被其它线程执行，调用setMessageSendHandler方法，但是在getMessageSendHandler中的线程
                         * 依然阻塞，无法将其唤醒。
                         *
                         *
                         * 即本质上是因为connectStatus.signalAll的调用先于handler的设置造成的。
                         */

                        //当此时handler还没被设置好时，不能执行到后面的connectStatus.signalAll()，只能够先进行阻塞，等待setMessageSendHandler
                        //方法调用后，发出的signal信号
                        while (messageSendHandler == null) {
                            handlerStatus.await();
                        }
                        // 当客户端与服务器的连接建立成功后，通知所有调用getMessageSendHandler阻塞的线程
                        if (result.equals(Boolean.TRUE)) {
                            connectStatus.signalAll();
                            System.out.printf("Netty RPC Client start success!\nip:%s\nport:%d\nprotocol:%s\n\n", host, port, serializationProtocol);
                        }
                    } catch (InterruptedException ex) {
                        Logger.getLogger(RpcServerLoader.class.getName()).log(Level.SEVERE, null, ex);
                    } finally {
                        lock.unlock();
                    }
                }
                @Override
                public void onFailure(Throwable t) {
                    t.printStackTrace();
                }
            }, threadPoolExecutor);
        }
    }

    public void setMessageSendHandler(NettyClientHandler messageInHandler) {
        try {
            lock.lock();
            this.messageSendHandler = messageInHandler;
            handlerStatus.signal();
        } finally {
            lock.unlock();
        }
    }

    public NettyClientHandler getMessageSendHandler() throws InterruptedException {
        try {
            //在一个客户端中通过多个线程向一个服务器发起请求时，可能客户端和服务器端的连接还没有建立好，即
            //messageSendHandler为null，因此阻塞，等待连接建立
            lock.lock();
            while (messageSendHandler == null) {
                connectStatus.await();
            }
            return messageSendHandler;
        } finally {
            lock.unlock();
        }
    }

    public void unLoad() {
        messageSendHandler.close();
        threadPoolExecutor.shutdown();
        eventLoopGroup.shutdownGracefully();
    }

    public void setSerializationProtocol(Serialization serializationProtocol) {
        this.serializationProtocol = serializationProtocol;
    }
}
