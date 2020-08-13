package com.newlandframework.rpc.remoting.server;

import com.google.common.collect.ClassToInstanceMap;
import com.google.common.collect.MutableClassToInstanceMap;
import com.newlandframework.rpc.remoting.handler.NettyServerHandler;
import com.newlandframework.rpc.remoting.initializer.NettyRpcInitializer;
import com.newlandframework.rpc.remoting.initializer.hessian.HessianRecvInitializer;
import com.newlandframework.rpc.remoting.initializer.jdk.JdkNativeRecvInitializer;
import com.newlandframework.rpc.remoting.initializer.kryo.KryoRecvInitializer;
import com.newlandframework.rpc.serialize.RpcSerializeFrame;
import com.newlandframework.rpc.serialize.Serialization;
import io.netty.channel.ChannelHandler;
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
