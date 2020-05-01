package com.newlandframework.rpc.netty;

import com.google.common.reflect.Reflection;
import com.newlandframework.rpc.serialize.RpcSerializeProtocol;

/**
 * 客户端执行模块，在此项目中，只支持一个客户端与一个RPC服务器的通信
 */
public class MessageSendExecutor {

    private static class MessageSendExecutorHolder {
        private static final MessageSendExecutor INSTANCE = new MessageSendExecutor();
    }

    public static MessageSendExecutor getInstance() {
        return MessageSendExecutorHolder.INSTANCE;
    }

    private RpcServerLoader loader = RpcServerLoader.getInstance();

    private MessageSendExecutor() {

    }

    public MessageSendExecutor(String serverAddress, RpcSerializeProtocol serializeProtocol) {
        loader.load(serverAddress, serializeProtocol);
    }

    public void setRpcServerLoader(String serverAddress, RpcSerializeProtocol serializeProtocol) {
        loader.load(serverAddress, serializeProtocol);
    }

    public void stop() {
        loader.unLoad();
    }

    public <T> T execute(Class<T> rpcInterface) throws Exception {
        return (T) Reflection.newProxy(rpcInterface, new MessageSendProxy<T>());
    }
}

