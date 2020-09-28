package com.xu.rpc.remoting.initializer;

import com.google.common.collect.ClassToInstanceMap;
import com.google.common.collect.MutableClassToInstanceMap;
import com.xu.rpc.remoting.initializer.hessian.HessianInitializer;
import com.xu.rpc.remoting.initializer.jdk.JdkNativeInitializer;
import com.xu.rpc.remoting.initializer.kryo.KryoInitializer;
import com.xu.rpc.remoting.initializer.protostuff.ProtostuffInitializer;
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
        initializers.putInstance(ProtostuffInitializer.class, new ProtostuffInitializer());
    }

    @Override
    public void select(Serialization serialization, ChannelPipeline pipeline, ChannelDuplexHandler handler) {
        switch (serialization) {
            case JDK: {
                initializers.getInstance(JdkNativeInitializer.class).handle(pipeline, handler);
                break;
            }
            case KRYO: {
                initializers.getInstance(KryoInitializer.class).handle(pipeline, handler);
                break;
            }
            case HESSIAN: {
                initializers.getInstance(HessianInitializer.class).handle(pipeline, handler);
                break;
            }
            case PROTOSTUFF:{
                initializers.getInstance(ProtostuffInitializer.class).handle(pipeline, handler);
                break;
            }
            default: {
                break;
            }
        }
    }
}
