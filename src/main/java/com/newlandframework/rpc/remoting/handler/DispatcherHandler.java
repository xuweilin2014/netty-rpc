package com.newlandframework.rpc.remoting.handler;

import com.newlandframework.rpc.model.MessageRequest;
import com.newlandframework.rpc.model.MessageResponse;
import com.newlandframework.rpc.parallel.NamedThreadFactory;
import com.newlandframework.rpc.remoting.execution.RecvInitializeTaskFacade;
import com.newlandframework.rpc.remoting.server.NettyServer;
import io.netty.channel.Channel;

import java.util.concurrent.Executor;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class DispatcherHandler implements ChannelHandler {

    private static final ExecutorService executor = Executors.newCachedThreadPool(new NamedThreadFactory("RpcExecutionThread", true));

    private ChannelHandler handler;

    public DispatcherHandler(ChannelHandler handler) {
        handler = handler;
    }

    /**
     * 此方法接收客户端发过来的Rpc调用请求，并且将方法的实际调用包装成一个Task，然后放到线程池中去执行。不会阻塞Netty的worker线程。
     * 根据是否开启JMX监控，返回的Task不同。
     */
    public void received(Channel channel, Object message){
        if (message instanceof MessageRequest){
            RecvInitializeTaskFacade facade = new RecvInitializeTaskFacade((MessageRequest) message, handler, channel);
            Runnable recvTask = facade.getTask();
            // 将任务 recvTask 放入到 MessageRecvExecutor 中的线程池中去执行。调用 recvTask 中的 call 方法，
            // 即调用 RpcServer 本地的方法，如果方法调用成功，就将结果封装成 MessageResponse 并且通过 Netty 发送至客户端
            executor.submit(recvTask);
        }else if (message instanceof MessageResponse){

        }
    }

}
