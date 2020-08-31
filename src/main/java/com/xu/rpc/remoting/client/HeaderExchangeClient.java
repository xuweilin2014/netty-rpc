package com.xu.rpc.remoting.client;

import com.xu.rpc.async.DefaultRpcFuture;
import com.xu.rpc.async.RpcFuture;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.exception.RemotingException;
import com.xu.rpc.model.MessageRequest;
import com.xu.rpc.util.URL;

import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;

public class HeaderExchangeClient implements ExchangeClient {

    // client 为 NettyClient
    private final Client client;

    private final AtomicBoolean closed = new AtomicBoolean(false);

    public HeaderExchangeClient(Client client) {
        this.client = client;
    }

    @Override
    public void close() {
        // TODO: 2020/8/31
    }

    @Override
    public void close(int timeout) {
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
        if (closed.get()){
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
        return null;
    }

    @Override
    public boolean isConnected() {
        // TODO: 2020/8/31
        return false;
    }

}
