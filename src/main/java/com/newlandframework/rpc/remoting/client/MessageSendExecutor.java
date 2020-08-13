package com.newlandframework.rpc.remoting.client;

import com.newlandframework.rpc.serialize.Serialization;

import java.lang.reflect.Proxy;

/**
 * 客户端执行模块，在此项目中，只支持一个客户端与一个RPC服务器的通信
 */
public class MessageSendExecutor {

    private static class MessageSendExecutorHolder {
        private static final MessageSendExecutor INSTANCE = new MessageSendExecutor();
    }

    //通过使用Holder模式来实现单例模式
    public static MessageSendExecutor getInstance() {
        return MessageSendExecutorHolder.INSTANCE;
    }

    //RpcServerLoader也使用了单例模式，在客户端中是唯一的
    private RpcServerLoader loader = RpcServerLoader.getInstance();

    private MessageSendExecutor() {
    }

    public MessageSendExecutor(String serverAddress, Serialization serializationProtocol) {
        loader.load(serverAddress, serializationProtocol);
    }

    public void setRpcServerLoader(String serverAddress, Serialization serializationProtocol) {
        loader.load(serverAddress, serializationProtocol);
    }

    public void stop() {
        loader.unLoad();
    }

    public Object getProxy(Class<?> rpcInterface) throws Exception {
        return  Proxy.newProxyInstance(rpcInterface.getClassLoader(),
                new Class<?>[]{rpcInterface}, new MessageSendProxy());
    }
}

