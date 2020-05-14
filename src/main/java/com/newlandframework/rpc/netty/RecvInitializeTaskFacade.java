package com.newlandframework.rpc.netty;

import com.newlandframework.rpc.core.RpcSystemConfig;
import com.newlandframework.rpc.model.MessageRequest;
import com.newlandframework.rpc.model.MessageResponse;

import java.util.Map;
import java.util.concurrent.Callable;

/**
 * 如果用户开启了JMX监控，就返回MessageRecvInitializeTask；
 * 如果用户没有开启JMX监控，就直接返回MessageRecvInitializeTaskAdapter；
 */
public class RecvInitializeTaskFacade {
    private MessageRequest request;
    private MessageResponse response;
    private Map<String, Object> handlerMap;
    private boolean isMetrics = RpcSystemConfig.SYSTEM_PROPERTY_JMX_METRICS_SUPPORT;

    public RecvInitializeTaskFacade(MessageRequest request, MessageResponse response, Map<String, Object> handlerMap) {
        this.request = request;
        this.response = response;
        this.handlerMap = handlerMap;
    }

    public Callable<Boolean> getTask() {
        return isMetrics ? new MessageRecvInitializeTask(request, response, handlerMap)
                : new MessageRecvInitializeTaskAdapter(request, response, handlerMap);
    }
}

