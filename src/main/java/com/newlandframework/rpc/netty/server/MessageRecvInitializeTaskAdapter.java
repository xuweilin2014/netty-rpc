package com.newlandframework.rpc.netty.server;

import com.newlandframework.rpc.model.MessageRequest;
import com.newlandframework.rpc.model.MessageResponse;
import com.newlandframework.rpc.netty.server.AbstractMessageRecvInitializeTask;

import java.util.Map;


public class MessageRecvInitializeTaskAdapter extends AbstractMessageRecvInitializeTask {
    public MessageRecvInitializeTaskAdapter(MessageRequest request, MessageResponse response, Map<String, Object> handlerMap) {
        super(request, response, handlerMap);
    }

    @Override
    protected void injectInvoke() {
    }

    @Override
    protected void injectSuccInvoke(long invokeTimespan) {
    }

    @Override
    protected void injectFailInvoke(Throwable error) {
    }

    @Override
    protected void injectFilterInvoke() {
    }
}

