package com.xu.rpc.remoting.client;

import com.xu.rpc.async.DefaultRpcFuture;
import com.xu.rpc.async.RpcFuture;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.exception.RemotingException;
import com.xu.rpc.model.MessageRequest;
import com.xu.rpc.remoting.exchanger.HeartbeatExchangeEndpoint;
import com.xu.rpc.util.URL;
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
        // TODO: 2020/8/31
    }

    @Override
    public boolean isClosed() {
        // TODO: 2020/8/31
        return false;
    }

    @Override
    public URL getUrl() {
        // TODO: 2020/8/31
        return null;
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
        // TODO: 2020/9/1  
        return null;
    }

}
