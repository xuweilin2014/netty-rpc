package com.xu.rpc.remoting.client;

import com.xu.rpc.async.DefaultRpcFuture;
import com.xu.rpc.async.RpcFuture;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.exception.RemotingException;
import com.xu.rpc.model.MessageRequest;
import com.xu.rpc.remoting.exchanger.HeartbeatExchangeEndpoint;
import com.xu.rpc.commons.URL;
import org.apache.log4j.Logger;

import java.util.UUID;

public class HeaderExchangeClient extends HeartbeatExchangeEndpoint implements ExchangeClient {

    private static final Logger logger = Logger.getLogger(HeaderExchangeClient.class);

    // client 为 NettyClient
    private final Client client;

    public HeaderExchangeClient(Client client) {
        super(client);
        this.client = client;
        startHeartbeat(client);
    }

    @Override
    public void close(int timeout) {
        // 关闭掉心跳机制
        super.close();
        if (timeout > 0){
            long start = System.currentTimeMillis();
            // 若关闭服务消费者，已经发出的服务请求，需要等待响应返回，若在 timeout 时间间隔内响应没有返回，强制关闭
            while (!DefaultRpcFuture.getFutures().isEmpty()
                    && System.currentTimeMillis() - start < timeout){
                try {
                    Thread.sleep(10);
                } catch (InterruptedException e) {
                    logger.warn(e.getMessage());
                }
            }
        }

        // 在 timeout 时间间隔内关闭掉 NettyClient
        client.close(timeout);
    }

    @Override
    public boolean isClosed() {
        return client.isClosed();
    }

    @Override
    public URL getUrl() {
        return client.getUrl();
    }

    @Override
    public boolean isConnected() {
        return client.isConnected();
    }

    @Override
    public RpcFuture request(Object invocation, int timeout) throws RemotingException {
        if (isClosed()){
            throw new RemotingException("client " + client + "is closed, cannot send message anymore.");
        }

        // 生成 request 信息
        MessageRequest req = new MessageRequest();
        RpcInvocation inv = (RpcInvocation) invocation;

        req.setInterfaceName(inv.getServiceType().getName());
        req.setMessageId(UUID.randomUUID().toString());
        req.setMethodName(inv.getMethodName());
        req.setParametersVal(inv.getParameters());
        req.setTypeParameters(inv.getParameterTypes());
        req.setToken(inv.getAttachments().get(RpcConfig.TOKEN_KEY));

        RpcFuture future = new DefaultRpcFuture(timeout, req, client.getUrl());

        try {
            // 由 NettyClient 发送给服务器端
            client.send(req);
        } catch (Throwable e) {
            future.cancel();
            throw e;
        }

        return future;
    }

    @Override
    public RpcFuture request(Object request) throws RemotingException {
        // 使用默认的超时时间，也就是 2000ms
        return request(request, RpcConfig.DEFAULT_TIMEOUT);
    }

}
