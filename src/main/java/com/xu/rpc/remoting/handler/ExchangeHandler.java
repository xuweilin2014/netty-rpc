package com.xu.rpc.remoting.handler;

import com.xu.rpc.async.DefaultRpcFuture;
import com.xu.rpc.exception.RemotingException;
import com.xu.rpc.model.MessageRequest;
import com.xu.rpc.model.MessageResponse;
import com.xu.rpc.parallel.NamedThreadFactory;
import com.xu.rpc.remoting.support.RecvExecutionTask;
import io.netty.channel.Channel;
import io.netty.util.internal.ConcurrentSet;

import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class ExchangeHandler extends AbstractHandlerDelegate {

    private static final ExecutorService executor = Executors.newCachedThreadPool(new NamedThreadFactory("RpcExecutionThread", true));

    private static Map<String, Channel> channels = new ConcurrentHashMap<>();

    public ExchangeHandler(ChannelHandler handler) {
        super(handler);
    }

    /**
     * 此方法接收客户端发过来的Rpc调用请求，并且将方法的实际调用包装成一个Task，然后放到线程池中去执行。不会阻塞Netty的worker线程。
     * 根据是否开启JMX监控，返回的Task不同。
     */
    public void received(Channel channel, Object message){
        setReadTimestamp(channel);
        if (message instanceof MessageRequest){
            MessageRequest request = (MessageRequest) message;
            // 保存发送过来请求，当请求在服务器端处理完毕之后，会调用 remove 清除掉，
            // 后面优雅停机时会使用其判断服务器端是否还有正在执行任务
            channels.put(request.getMessageId(), channel);
            // 服务端接收到请求，进行处理
            RecvExecutionTask recvTask = new RecvExecutionTask(request, handler, channel);
            // 将任务 recvTask 放入到 MessageRecvExecutor 中的线程池中去执行。调用 recvTask 中的 call 方法，
            // 即调用 RpcServer 本地的方法，如果方法调用成功，就将结果封装成 MessageResponse 并且通过 Netty 发送至客户端
            executor.submit(recvTask);
        }else if (message instanceof MessageResponse){
            // 客户端接收到响应，进行处理
            DefaultRpcFuture.received((MessageResponse) message);
        }
    }

    public static Map<String, Channel> getChannels() {
        return channels;
    }
}
