package com.xu.rpc.remoting.client;

import com.xu.rpc.remoting.handler.NettyServerHandler;
import com.xu.rpc.remoting.initializer.jdk.JdkNativeSendInitializer;
import com.xu.rpc.remoting.initializer.kryo.KryoSendInitializer;
import com.xu.rpc.remoting.initializer.hessian.HessianSendInitializer;
import com.xu.rpc.remoting.server.ServerChannelInitializer;
import com.xu.rpc.serialize.RpcSerializeFrame;
import com.xu.rpc.serialize.Serialization;
import com.google.common.collect.ClassToInstanceMap;
import com.google.common.collect.MutableClassToInstanceMap;
import io.netty.channel.ChannelPipeline;


public class RpcSendSerializeFrame implements RpcSerializeFrame {
    private static ClassToInstanceMap<ServerChannelInitializer> handler = MutableClassToInstanceMap.create();

    static {
        handler.putInstance(JdkNativeSendInitializer.class, new JdkNativeSendInitializer());
        handler.putInstance(KryoSendInitializer.class, new KryoSendInitializer());
        handler.putInstance(HessianSendInitializer.class, new HessianSendInitializer());
    }

    @Override
    public void select(Serialization protocol, ChannelPipeline pipeline) {
        switch (protocol) {
            case JDKSERIALIZE: {
                handler.getInstance(JdkNativeSendInitializer.class).handle(pipeline);
                break;
            }
            case KRYOSERIALIZE: {
                handler.getInstance(KryoSendInitializer.class).handle(pipeline);
                break;
            }
            case HESSIANSERIALIZE: {
                handler.getInstance(HessianSendInitializer.class).handle(pipeline);
                break;
            }
            default: {
                break;
            }
        }
    }

    @Override
    public void select(Serialization protocol, ChannelPipeline pipeline, NettyServerHandler handler) {

    }
}

