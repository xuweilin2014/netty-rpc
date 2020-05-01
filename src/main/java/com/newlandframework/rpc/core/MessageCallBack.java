package com.newlandframework.rpc.core;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import com.newlandframework.rpc.exception.InvokeModuleException;
import com.newlandframework.rpc.exception.InvokeTimeoutException;
import com.newlandframework.rpc.exception.RejectResponeException;
import com.newlandframework.rpc.model.MessageRequest;
import com.newlandframework.rpc.model.MessageResponse;


public class MessageCallBack {

    private MessageRequest request;
    private MessageResponse response;
    private Lock lock = new ReentrantLock();
    private Condition finish = lock.newCondition();

    public MessageCallBack(MessageRequest request) {
        this.request = request;
    }

    public Object start() {
        try {
            lock.lock();
            await();
            if (this.response != null) {
                boolean isInvokeSucc = getInvokeResult();
                if (isInvokeSucc) {
                    if (this.response.getError().isEmpty()) {
                        return this.response.getResult();
                    } else {
                        throw new InvokeModuleException(this.response.getError());
                    }
                } else {
                    throw new RejectResponeException(RpcSystemConfig.FILTER_RESPONSE_MSG);
                }
            } else {
                return null;
            }
        } finally {
            lock.unlock();
        }
    }

    public void over(MessageResponse reponse) {
        try {
            lock.lock();
            finish.signal();
            this.response = reponse;
        } finally {
            lock.unlock();
        }
    }

    private void await() {
        boolean isTimeout = false;
        try {
            isTimeout = finish.await(RpcSystemConfig.SYSTEM_PROPERTY_MESSAGE_CALLBACK_TIMEOUT, TimeUnit.MILLISECONDS);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        if (!isTimeout) {
            throw new InvokeTimeoutException(RpcSystemConfig.TIMEOUT_RESPONSE_MSG);
        }
    }

    private boolean getInvokeResult() {
        return (!this.response.getError().equals(RpcSystemConfig.FILTER_RESPONSE_MSG) &&
                (!this.response.isReturnNotNull() || (this.response.isReturnNotNull() && this.response.getResult() != null)));
    }
}
