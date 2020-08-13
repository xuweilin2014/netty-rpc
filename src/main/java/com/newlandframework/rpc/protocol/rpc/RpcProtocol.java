package com.newlandframework.rpc.protocol.rpc;

import com.newlandframework.rpc.exception.RpcException;
import com.newlandframework.rpc.model.MessageRequest;
import com.newlandframework.rpc.protocol.AbstractProtocol;
import com.newlandframework.rpc.protocol.Exporter;
import com.newlandframework.rpc.protocol.Invoker;
import com.newlandframework.rpc.remoting.exchanger.Exchangers;
import com.newlandframework.rpc.remoting.handler.ChannelHandler;
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

    private ReplyHandler replyHandler = new ReplyHandler() {
        @Override
        public void received(Channel channel, Object message) {
            throw new UnsupportedOperationException("cannot execute received function in ReplyHandler.");
        }

        @Override
        public Object reply(MessageRequest request, Channel channel) {
            InetSocketAddress socketAddress = (InetSocketAddress) channel.localAddress();
            int port = socketAddress.getPort();
            String serviceKey = getServiceKey(request.getInterfaceName(), port);
            Exporter exporter = exporters.get(serviceKey);
            return exporter.getInvoker().invoke(request);
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

        return rpcExporter;
    }

    public String getServiceKey(URL url){
        return super.getServiceKey(url.getServiceName(), url.getPort());
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
}
