package com.xu.rpc.protocol.rpc;

import com.xu.rpc.commons.util.ReflectionUtils;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;
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
import com.xu.rpc.commons.URL;
import io.netty.channel.Channel;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

import java.lang.reflect.Method;
import java.net.InetSocketAddress;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class RpcProtocol extends AbstractProtocol {

    // ip:port -> server
    private final Map<String, HeaderExchangeServer> servers = new ConcurrentHashMap<>();

    public static final String NAME = "rpc";

    private static final Logger logger = Logger.getLogger(RpcProtocol.class);

    private static MetricsServer metricsServer;

    private final AtomicBoolean started = new AtomicBoolean(false);

    private final Map<String, ReferenceCountClient> referenceCountClients = new ConcurrentHashMap<>();

    private static ApiEchoServer echoServer;

    private final Lock lock = new ReentrantLock();

    private static final int DEFAULT_SHUTDOWN_WAIT_TIME = 5000;

    private ReplyHandler replyHandler = new ReplyHandler() {
        @Override
        public RpcResult reply(Object message, Channel channel) throws RemotingException {
            if (message instanceof MessageRequest){
                MessageRequest request = (MessageRequest) message;

                InetSocketAddress socketAddress = (InetSocketAddress) channel.localAddress();
                int port = socketAddress.getPort();

                // service key 为 ServiceName:Port
                String serviceKey = getServiceKey(request.getInterfaceName(), port);
                Exporter exporter = exporters.get(serviceKey);
                RpcResult result = null;

                try {
                    if (!StringUtils.isEmpty(request.getInterfaceName())){
                        Class<?> cls = Thread.currentThread().getContextClassLoader().loadClass(request.getInterfaceName());
                        Method method = ReflectionUtils.getDeclaredMethod(cls, request.getMethodName(), request.getTypeParameters());
                        RpcInvocation invocation = new RpcInvocation(method, request.getParametersVal());
                        // 如果 request 中含有令牌，则将其保存到 RpcInvocation 中
                        invocation.getAttachments().put(RpcConfig.TOKEN_KEY, StringUtils.isEmpty(request.getToken()) ? "" : request.getToken());
                        result = exporter.getInvoker().invoke(invocation);
                        return result;
                    }
                } catch (Throwable t) {
                    result = new RpcResult(t);
                    result.setResult(null);
                    return result;
                }

                throw new RemotingException("cannot invoke method.");
            }

            throw new RemotingException("unsupported message type :" + message.getClass().getName() + " , consumer address :"
                    + channel.remoteAddress());
        }
    };

    @Override
    public <T> Exporter<T> export(Invoker<T> invoker) throws RpcException {
        // 1.从 invoker 中获取 url
        URL url = invoker.getUrl();
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

        boolean metrics = url.getParameter(RpcConfig.METRICS_KEY, true);
        // 如果用户在 <nettyrpc:application/> 中配置了 metrics 为 false，就不开启 jmx 服务器，关闭掉监控功能
        if (!metrics)
            return;

        try{
            lock.lock();
            if (metricsServer == null){
                metricsServer = new MetricsServer(url);
                metricsServer.start();
                MetricsHtmlBuilder.getInstance().setMetricsServer(metricsServer);
            }
        } finally {
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
    public <T> Invoker<T> refer(URL url, Class<?> type) throws RpcException {
        RpcInvoker<T> invoker = new RpcInvoker<T>(type, url, invokers, getClient(url));
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
                referenceCountClients.put(address, client);
                return client;
            } catch (RemotingException e) {
                throw new RpcException("failed to connect to server " + address, e);
            }
        }
    }

    @Override
    public void destroy() {
        // 关闭掉启动的 rpc 服务器
        for (String key : servers.keySet()) {
            HeaderExchangeServer server = servers.remove(key);
            if (server != null){
                try {
                    // 等待一定时间，保证 rpc 服务器已经接收到的请求处理完成之后，才能够下线
                    server.close(DEFAULT_SHUTDOWN_WAIT_TIME);
                    logger.info("server " + key + " is closed.");
                } catch (Exception e) {
                    logger.warn("failed to close the server " + key + " in " + DEFAULT_SHUTDOWN_WAIT_TIME + " seconds.");
                }
            }
        }

        try {
            // 关闭掉 jmx 服务器
            if (metricsServer != null) {
                metricsServer.stop();
                logger.info("metrics server host " + metricsServer.getHost() + " port " + metricsServer.getPort() + " is closed.");
            }
        } catch (Exception e) {
            logger.warn("failed to close the metrics server.");
        }

        try {
            // 关闭掉内置的 http 服务器
            if (echoServer != null) {
                echoServer.stop();
                logger.info("echo server host " + echoServer.getHost() + " port " + echoServer.getPort() + " is closed.");
            }
        } catch (Exception e) {
            logger.warn("failed to stop the echo server.");
        }

        // 关闭掉客户端
        for (String key : referenceCountClients.keySet()) {
            ReferenceCountClient client = referenceCountClients.remove(key);
            try {
                // 等待一段时间之后再关闭掉客户端，等待已经发出的响应请求返回
                client.close(DEFAULT_SHUTDOWN_WAIT_TIME);
                logger.info("client " + key + " is closed.");
            } catch (Exception e) {
                logger.warn("failed to close the client " + key + " in " + DEFAULT_SHUTDOWN_WAIT_TIME + " seconds.");
            }
        }

        // 销毁掉各种 invoker 和 exporter
        super.destroy();
    }

    public String getServiceKey(URL url){
        return super.getServiceKey(url.getServiceName(), url.getPort());
    }

    @Override
    public String doGetName() {
        return NAME;
    }
}
