package com.xu.rpc.protocol.rpc;

import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.exception.RemotingException;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.jmx.MetricsHtmlBuilder;
import com.xu.rpc.jmx.MetricsServer;
import com.xu.rpc.model.MessageRequest;
import com.xu.rpc.protocol.AbstractProtocol;
import com.xu.rpc.protocol.Exporter;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.remoting.client.ExchangeClient;
import com.xu.rpc.remoting.client.ReferenceCountClient;
import com.xu.rpc.remoting.exchanger.Exchangers;
import com.xu.rpc.remoting.handler.ReplyHandler;
import com.xu.rpc.remoting.echo.ApiEchoServer;
import com.xu.rpc.remoting.server.HeaderExchangeServer;
import com.xu.rpc.util.URL;
import io.netty.channel.Channel;

import java.net.InetSocketAddress;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class RpcProtocol extends AbstractProtocol {

    // ip:port -> server
    private final Map<String, HeaderExchangeServer> servers = new ConcurrentHashMap<>();

    private MetricsServer metricsServer;

    private final AtomicBoolean started = new AtomicBoolean(false);

    private final Map<String, ReferenceCountClient> referenceCountClients = new ConcurrentHashMap<>();

    private ApiEchoServer echoServer;

    private Lock lock = new ReentrantLock();

    private ReplyHandler replyHandler = new ReplyHandler() {
        @Override
        public Object reply(Object message, Channel channel) throws RemotingException {
            if (message instanceof RpcInvocation){
                RpcInvocation invocation = (RpcInvocation) message;

                InetSocketAddress socketAddress = (InetSocketAddress) channel.localAddress();
                int port = socketAddress.getPort();

                // service key 为 ServiceName:Port
                String serviceKey = getServiceKey(invocation.getServiceType().getName(), port);
                Exporter exporter = exporters.get(serviceKey);

                try {
                    return  exporter.getInvoker().invoke(invocation);
                } catch (Throwable throwable) {
                    throw new RpcException("errors occur when executing method : " + throwable.getMessage(), throwable);
                }
            }
            throw new RemotingException("unsupported message type :" + message.getClass().getName() + " , consumer address :"
                    + channel.remoteAddress());
        }
    };

    @Override
    public Exporter export(Invoker invoker) throws RpcException {
        // 1.从 invoker 中获取 url
        URL url = invoker.getURL();
        // 2.根据 url 获取 ServiceKey, ServiceKey 由以下两个部分组成：服务名:端口号
        String serviceKey = getServiceKey(url);
        // 3.创建 RpcExporter
        RpcExporter rpcExporter = new RpcExporter(invoker, serviceKey, exporters);
        // 4.把 RpcExporter 保存到 AbstractProtocol 中的 exporters 中
        exporters.put(serviceKey, rpcExporter);
        // 5.开启服务器
        openServer(url);
        // 6.开启 JMX 服务器
        openJmxServer(url);
        // 7.开启 echo 服务器，用于进行服务能力展示以及服务调用详情展示
        openEchoServer(url);

        return rpcExporter;
    }

    private void openEchoServer(URL url) {
        if (started.get())
            return;

        if (started.compareAndSet(false, true)) {
            // echo server 监听默认的端口 18882
            echoServer = new ApiEchoServer(url);
            echoServer.start();
        }
    }

    private void openJmxServer(URL url) {
        if (metricsServer != null){
            return;
        }

        boolean metrics = url.getParameter(RpcConfig.METRICS, true);
        // 如果用户在 <nettyrpc:monitor/> 中配置了 metrics 为 false，就不开启 jmx 服务器，关闭掉监控功能
        if (!metrics)
            return;

        try{
            lock.lock();
            metricsServer = new MetricsServer(url);
            metricsServer.start();
            MetricsHtmlBuilder.getInstance().setMetricsServer(metricsServer);
        }finally {
            lock.unlock();
        }
    }

    private void openServer(URL url) {
        // address 为 IP地址:PORT，一个特定的 ip 地址 + 端口号只能启动一个服务器
        String address = url.getAddress();
        HeaderExchangeServer server = servers.get(address);

        try{
            lock.lock();
            if (server == null) {
                try {
                    server = Exchangers.bind(url, replyHandler);
                } catch (Exception e) {
                    throw new IllegalStateException("failed to start the netty server.");
                }
                servers.put(address, server);
            }
        }finally {
            lock.unlock();;
        }
    }

    // 这里的 url 为注册在注册中心上的 url，也就是 RpcInvoker 中的 url，而不是客户端自己的 url
    @Override
    public Invoker refer(URL url, Class<?> type) throws RpcException {
        RpcInvoker invoker = new RpcInvoker(type, url, invokers, getClient(url));
        invokers.add(invoker);
        return invoker;
    }

    private ExchangeClient getClient(URL url) {
        String address = url.getAddress();
        ReferenceCountClient client = referenceCountClients.get(address);
        if (client != null){
            if (client.isClosed()){
                referenceCountClients.remove(address);
            }else {
                client.incrementAndGet();
                return client;
            }
        }

        // str1 = new String("abc");
        // str2 = new String("abc");
        // 调用 str1.intern()，编译器会将 "abc" 字符串添加到常量池中，并且返回指向该常量的引用。
        // 再调用 str2.intern() 时，因为常量池中已经存在 "abc" 字符串，所以直接返回 "abc" 的引用。
        // address.intern() 添加 synchronized 关键字，所以当创建连接相同地址的客户端时，进行并发控制。
        synchronized (address.intern()){
            ExchangeClient exchangeClient = null;
            try {
                exchangeClient = Exchangers.connect(url, replyHandler);
                client = new ReferenceCountClient(url, exchangeClient);
                return client;
            } catch (RemotingException e) {
                throw new RpcException("failed to connect to server " + address, e);
            }
        }
    }


    @Override
    public void destroy() {
        // TODO: 2020/8/16
    }

    public String getServiceKey(URL url){
        return super.getServiceKey(url.getServiceName(), url.getPort());
    }
}
