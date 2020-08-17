package com.newlandframework.rpc.protocol.rpc;

import com.newlandframework.rpc.core.RpcSystemConfig;
import com.newlandframework.rpc.exception.RemotingException;
import com.newlandframework.rpc.exception.RpcException;
import com.newlandframework.rpc.jmx.MetricsHtmlBuilder;
import com.newlandframework.rpc.jmx.MetricsServer;
import com.newlandframework.rpc.model.MessageRequest;
import com.newlandframework.rpc.protocol.AbstractProtocol;
import com.newlandframework.rpc.protocol.Exporter;
import com.newlandframework.rpc.protocol.Invoker;
import com.newlandframework.rpc.remoting.exchanger.Exchangers;
import com.newlandframework.rpc.remoting.handler.ReplyHandler;
import com.newlandframework.rpc.remoting.echo.ApiEchoServer;
import com.newlandframework.rpc.remoting.server.HeaderExchangeServer;
import com.newlandframework.rpc.remoting.server.Server;
import com.newlandframework.rpc.util.URL;
import io.netty.channel.Channel;

import java.net.InetSocketAddress;
import java.util.Collections;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class RpcProtocol extends AbstractProtocol {

    // ip:port -> server
    private final Map<String, HeaderExchangeServer> servers = new ConcurrentHashMap<>();

    private static MetricsServer metricsServer;

    private final AtomicBoolean started = new AtomicBoolean(false);

    private static ApiEchoServer echoServer;

    private static Lock lock = new ReentrantLock();

    private ReplyHandler replyHandler = new ReplyHandler() {
        @Override
        public void sent(Channel channel, Object message) {
        }

        @Override
        public void received(Channel channel, Object message) {
            throw new UnsupportedOperationException("cannot execute received function in ReplyHandler.");
        }

        @Override
        public void connected(Channel channel) {
        }

        @Override
        public void disconnected(Channel channel) {
        }

        @Override
        public Object reply(MessageRequest request, Channel channel) {
            InetSocketAddress socketAddress = (InetSocketAddress) channel.localAddress();
            int port = socketAddress.getPort();
            String serviceKey = getServiceKey(request.getInterfaceName(), port);
            Exporter exporter = exporters.get(serviceKey);

            try {
                return  exporter.getInvoker().invoke(request);
            } catch (Throwable throwable) {
                throw new RpcException("errors occur when executing method : " + throwable.getMessage(), throwable);
            }
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

        boolean metrics = url.getParameter(RpcSystemConfig.METRICS, true);
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

    @Override
    public Invoker refer(URL url) throws RpcException {
        // TODO: 2020/8/16
        return null;
    }

    @Override
    public void destroy() {
        // TODO: 2020/8/16
    }

    public String getServiceKey(URL url){
        return super.getServiceKey(url.getServiceName(), url.getPort());
    }
}
