package com.xu.rpc.remoting.initializer;

import com.google.common.collect.ClassToInstanceMap;
import com.google.common.collect.MutableClassToInstanceMap;
import com.xu.rpc.remoting.initializer.hessian.HessianInitializer;
import com.xu.rpc.remoting.initializer.jdk.JdkNativeInitializer;
import com.xu.rpc.remoting.initializer.kryo.KryoInitializer;
import com.xu.rpc.serialize.Serialization;
import io.netty.channel.ChannelDuplexHandler;
import io.netty.channel.ChannelPipeline;


public class RpcSerializeFrame implements SerializeFrame {

    public RpcSerializeFrame() {
    }

    private static ClassToInstanceMap<NettyRpcInitializer> handler = MutableClassToInstanceMap.create();

    static {
        handler.putInstance(JdkNativeInitializer.class, new JdkNativeInitializer());
        handler.putInstance(KryoInitializer.class, new KryoInitializer());
        handler.putInstance(HessianInitializer.class, new HessianInitializer());
    }

    @Override
    public void select(Serialization serialization, ChannelPipeline pipeline, ChannelDuplexHandler serverHandler) {
        switch (serialization) {
            case JDKSERIALIZE: {
                handler.getInstance(JdkNativeInitializer.class).handle(pipeline, serverHandler);
                break;
            }
            case KRYOSERIALIZE: {
                handler.getInstance(KryoInitializer.class).handle(pipeline, serverHandler);
                break;
            }
            case HESSIANSERIALIZE: {
                handler.getInstance(HessianInitializer.class).handle(pipeline, serverHandler);
                break;
            }
            default: {
                break;
            }
        }
    }
}
