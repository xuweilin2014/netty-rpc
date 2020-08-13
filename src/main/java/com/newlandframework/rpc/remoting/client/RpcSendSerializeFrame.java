package com.newlandframework.rpc.remoting.client;

import com.newlandframework.rpc.remoting.initializer.jdk.JdkNativeSendInitializer;
import com.newlandframework.rpc.remoting.initializer.kryo.KryoSendInitializer;
import com.newlandframework.rpc.remoting.initializer.hessian.HessianSendInitializer;
import com.newlandframework.rpc.remoting.initializer.protostuff.ProtostuffSendInitializer;
import com.newlandframework.rpc.serialize.RpcSerializeFrame;
import com.newlandframework.rpc.serialize.Serialization;
import com.google.common.collect.ClassToInstanceMap;
import com.google.common.collect.MutableClassToInstanceMap;
import io.netty.channel.ChannelPipeline;


public class RpcSendSerializeFrame implements RpcSerializeFrame {
    private static ClassToInstanceMap<NettyRpcSendInitializer> handler = MutableClassToInstanceMap.create();

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
            case PROTOSTUFFSERIALIZE: {
                handler.getInstance(ProtostuffSendInitializer.class).handle(pipeline);
                break;
            }
            default: {
                break;
            }
        }
    }
}

