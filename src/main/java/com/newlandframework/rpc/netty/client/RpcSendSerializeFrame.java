package com.newlandframework.rpc.netty.client;

import com.newlandframework.rpc.netty.handler.NettyRpcSendHandler;
import com.newlandframework.rpc.netty.handler.JdkNativeSendHandler;
import com.newlandframework.rpc.netty.handler.KryoSendHandler;
import com.newlandframework.rpc.netty.handler.HessianSendHandler;
import com.newlandframework.rpc.netty.handler.ProtostuffSendHandler;
import com.newlandframework.rpc.serialize.RpcSerializeFrame;
import com.newlandframework.rpc.serialize.RpcSerializeProtocol;
import com.google.common.collect.ClassToInstanceMap;
import com.google.common.collect.MutableClassToInstanceMap;
import io.netty.channel.ChannelPipeline;


public class RpcSendSerializeFrame implements RpcSerializeFrame {
    private static ClassToInstanceMap<NettyRpcSendHandler> handler = MutableClassToInstanceMap.create();

    static {
        handler.putInstance(JdkNativeSendHandler.class, new JdkNativeSendHandler());
        handler.putInstance(KryoSendHandler.class, new KryoSendHandler());
        handler.putInstance(HessianSendHandler.class, new HessianSendHandler());
        handler.putInstance(ProtostuffSendHandler.class, new ProtostuffSendHandler());
    }

    @Override
    public void select(RpcSerializeProtocol protocol, ChannelPipeline pipeline) {
        switch (protocol) {
            case JDKSERIALIZE: {
                handler.getInstance(JdkNativeSendHandler.class).handle(pipeline);
                break;
            }
            case KRYOSERIALIZE: {
                handler.getInstance(KryoSendHandler.class).handle(pipeline);
                break;
            }
            case HESSIANSERIALIZE: {
                handler.getInstance(HessianSendHandler.class).handle(pipeline);
                break;
            }
            case PROTOSTUFFSERIALIZE: {
                handler.getInstance(ProtostuffSendHandler.class).handle(pipeline);
                break;
            }
            default: {
                break;
            }
        }
    }
}

