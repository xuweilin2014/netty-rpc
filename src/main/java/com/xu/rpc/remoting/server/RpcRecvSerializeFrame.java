package com.xu.rpc.remoting.server;

import com.google.common.collect.ClassToInstanceMap;
import com.google.common.collect.MutableClassToInstanceMap;
import com.xu.rpc.remoting.handler.NettyServerHandler;
import com.xu.rpc.remoting.initializer.NettyRpcInitializer;
import com.xu.rpc.remoting.initializer.hessian.HessianRecvInitializer;
import com.xu.rpc.remoting.initializer.jdk.JdkNativeRecvInitializer;
import com.xu.rpc.remoting.initializer.kryo.KryoRecvInitializer;
import com.xu.rpc.serialize.RpcSerializeFrame;
import com.xu.rpc.serialize.Serialization;
import io.netty.channel.ChannelPipeline;


public class RpcRecvSerializeFrame implements RpcSerializeFrame {

    public RpcRecvSerializeFrame() {
    }

    private static ClassToInstanceMap<NettyRpcInitializer> handler = MutableClassToInstanceMap.create();

    static {
        handler.putInstance(JdkNativeRecvInitializer.class, new JdkNativeRecvInitializer());
        handler.putInstance(KryoRecvInitializer.class, new KryoRecvInitializer());
        handler.putInstance(HessianRecvInitializer.class, new HessianRecvInitializer());
    }

    @Override
    public void select(Serialization serialization, ChannelPipeline pipeline, NettyServerHandler serverHandler) {
        switch (serialization) {
            case JDKSERIALIZE: {
                handler.getInstance(JdkNativeRecvInitializer.class).handle(pipeline, serverHandler);
                break;
            }
            case KRYOSERIALIZE: {
                handler.getInstance(KryoRecvInitializer.class).handle(pipeline, serverHandler);
                break;
            }
            case HESSIANSERIALIZE: {
                handler.getInstance(HessianRecvInitializer.class).handle(pipeline, serverHandler);
                break;
            }
            default: {
                break;
            }
        }
    }
}
