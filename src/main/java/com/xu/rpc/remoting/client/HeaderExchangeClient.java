package com.xu.rpc.remoting.client;

import com.xu.rpc.async.DefaultRpcFuture;
import com.xu.rpc.async.RpcFuture;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.exception.RemotingException;
import com.xu.rpc.model.MessageRequest;
import com.xu.rpc.remoting.exchanger.HeartbeatExchangeEndpoint;
import com.xu.rpc.util.URL;
import javafx.scene.paint.Stop;
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
    public void close() {
        super.close();
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

        RpcFuture future = new DefaultRpcFuture(timeout, req, client.getUrl());

        try {
            // 由 NettyClient 发送给服务器端
            client.send(req);
        } catch (RemotingException e) {
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

    @Override
    public void doClose() {
        // do nothing
    }
}
