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

    private static ClassToInstanceMap<NettyRpcInitializer> initializers = MutableClassToInstanceMap.create();

    static {
        initializers.putInstance(JdkNativeInitializer.class, new JdkNativeInitializer());
        initializers.putInstance(KryoInitializer.class, new KryoInitializer());
        initializers.putInstance(HessianInitializer.class, new HessianInitializer());
    }

    @Override
    public void select(Serialization serialization, ChannelPipeline pipeline, ChannelDuplexHandler handler) {
        switch (serialization) {
            case JDKSERIALIZE: {
                initializers.getInstance(JdkNativeInitializer.class).handle(pipeline, handler);
                break;
            }
            case KRYOSERIALIZE: {
                initializers.getInstance(KryoInitializer.class).handle(pipeline, handler);
                break;
            }
            case HESSIANSERIALIZE: {
                initializers.getInstance(HessianInitializer.class).handle(pipeline, handler);
                break;
            }
            default: {
                break;
            }
        }
    }
}
