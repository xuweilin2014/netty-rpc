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
                if (response.getInvokeStatus().isRejected()){
                    throw new RejectResponeException(RpcSystemConfig.FILTER_RESPONSE_MSG);
                }else if (response.getInvokeStatus().isDone()){
                    return this.response.getResult();
                }else{
                    throw new InvokeModuleException(this.response.getError());
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
            //在finish上阻塞，等待调用结果的返回，默认的等待时间为30s
            //如果await等待调用结果返回的时间没有超时，而是正常返回，则返回true；如果await等待的时间超时了，那么就会返回false
            isTimeout = finish.await(RpcSystemConfig.SYSTEM_PROPERTY_MESSAGE_CALLBACK_TIMEOUT, TimeUnit.MILLISECONDS);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        // 如果超时了，则会抛出自定义的异常InvokeTimeoutException。由于MessageCallBack中的此方法是在线程池中执行，因此抛出的异常会被捕获，
        // 并且最终保存到FutureTask中的outcome属性中
        if (!isTimeout) {
            throw new InvokeTimeoutException(RpcSystemConfig.TIMEOUT_RESPONSE_MSG);
        }
    }

}
