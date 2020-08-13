package com.newlandframework.rpc.remoting.execution;

import com.newlandframework.rpc.core.RpcSystemConfig;
import com.newlandframework.rpc.model.MessageRequest;
import com.newlandframework.rpc.remoting.handler.ChannelHandler;
import io.netty.channel.Channel;

/**
 * 如果用户开启了JMX监控，就返回MessageRecvInitializeTask；
 * 如果用户没有开启JMX监控，就直接返回MessageRecvInitializeTaskAdapter；
 */
public class RecvInitializeTaskFacade {

    private MessageRequest request;

    private ChannelHandler handler;

    private Channel channel;

    private boolean isMetrics = RpcSystemConfig.SYSTEM_PROPERTY_JMX_METRICS_SUPPORT;

    public RecvInitializeTaskFacade(MessageRequest request, ChannelHandler handler, Channel channel) {
        this.request = request;
        this.handler = handler;
        this.channel = channel;
    }

    public Runnable getTask() {
        return isMetrics ? new MetricsRecvTask(request, handler, channel)
                : new RecvTask(request, handler, channel);
    }
}

