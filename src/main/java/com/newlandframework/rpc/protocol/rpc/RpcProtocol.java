package com.newlandframework.rpc.protocol.rpc;

import com.newlandframework.rpc.exception.RemotingException;
import com.newlandframework.rpc.exception.RpcException;
import com.newlandframework.rpc.jmx.MetricsServer;
import com.newlandframework.rpc.model.MessageRequest;
import com.newlandframework.rpc.protocol.AbstractProtocol;
import com.newlandframework.rpc.protocol.Exporter;
import com.newlandframework.rpc.protocol.Invoker;
import com.newlandframework.rpc.remoting.exchanger.Exchangers;
import com.newlandframework.rpc.remoting.handler.ReplyHandler;
import com.newlandframework.rpc.remoting.server.HeaderExchangeServer;
import com.newlandframework.rpc.util.URL;
import io.netty.channel.Channel;

import java.net.InetSocketAddress;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class RpcProtocol extends AbstractProtocol {

    private static final Map<String, HeaderExchangeServer> servers = new ConcurrentHashMap<>();

    private static Lock lock = new ReentrantLock();

    private static Map<Integer, MetricsServer> metricsServers = null;

    private ReplyHandler replyHandler = new ReplyHandler() {
        @Override
        public void sent(Channel channel, Object message) throws RemotingException {
        }

        @Override
        public void received(Channel channel, Object message) {
            throw new UnsupportedOperationException("cannot execute received function in ReplyHandler.");
        }

        @Override
        public void connected(Channel channel) throws RemotingException {

        }

        @Override
        public void disconnected(Channel channel) throws RemotingException {

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

        return rpcExporter;
    }

    private void openJmxServer(URL url) {
        // TODO: 2020/8/14
        String address = url.getAddress();
        HeaderExchangeServer server = servers.get(address);
        if (server == null)
            throw new IllegalStateException("server is null, cannot start jmx server.");


        MetricsServer.getInstance().start(address);
    }

    private void openServer(URL url) {
        // address 为 IP地址:PORT，一个特定的 ip 地址 + 端口号只能启动一个服务器
        String address = url.getAddress();
        lock.lock();
        try{
            HeaderExchangeServer server = servers.get(address);
            if (server == null) {
                try {
                    server = Exchangers.bind(url, replyHandler);
                } catch (Exception e) {
                    throw new IllegalStateException("failed to start the netty server.");
                }
                servers.put(address, server);
            }
        }finally {
            lock.unlock();
        }

    }

    @Override
    public Invoker refer(URL url) throws RpcException {
        return null;
    }

    @Override
    public void destroy() {

    }

    public String getServiceKey(URL url){
        return super.getServiceKey(url.getServiceName(), url.getPort());
    }
}
