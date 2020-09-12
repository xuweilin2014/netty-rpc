package com.xu.rpc.async;

import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.exception.RemotingException;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.exception.RpcTimeoutException;
import com.xu.rpc.model.MessageRequest;
import com.xu.rpc.model.MessageResponse;
import com.xu.rpc.parallel.NamedThreadFactory;
import com.xu.rpc.remoting.support.MethodInvokeStatus;
import com.xu.rpc.commons.URL;
import org.apache.log4j.Logger;

import java.text.SimpleDateFormat;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class DefaultRpcFuture implements RpcFuture{

    private static final Logger logger = Logger.getLogger(DefaultRpcFuture.class);

    private static final Map<String, DefaultRpcFuture> futures = new ConcurrentHashMap<>();

    private static final int TIMEOUT_SCAN_INTERVAL = 500;

    private static final ScheduledThreadPoolExecutor timeoutExecutor = new ScheduledThreadPoolExecutor(1, new NamedThreadFactory("RpcTimeoutFutureScan", true));

    private final List<RpcFutureListener> listeners = new CopyOnWriteArrayList<>();

    private final int timeout;

    private final Lock lock = new ReentrantLock();

    private final Condition done = lock.newCondition();

    private final MessageRequest request;

    private final String id;

    private final long start = System.currentTimeMillis();

    private volatile MessageResponse response;

    static {
        // 开启一个定时任务，用来检测 futures 集合中的 future 是否超时，如果发生超时的话，就创建一个 response 对象，然后调用 doReceived 方法让其返回
        timeoutExecutor.scheduleWithFixedDelay(new Runnable() {
            @Override
            public void run() {
                if (futures.size() != 0) {
                    try{
                        for (DefaultRpcFuture future : futures.values()) {
                            if (future == null || future.isDone() || future.isCancelled()) {
                                continue;
                            }

                            if (System.currentTimeMillis() - future.start >= future.timeout) {
                                MessageResponse response = new MessageResponse();
                                response.setInvokeStatus(MethodInvokeStatus.TIMEOUT);
                                response.setError(new RpcTimeoutException("future for invoking method exceeds time limit."));
                                response.setMessageId(future.getId());

                                future.doReceived(response);
                            }
                        }
                    }catch (Throwable t){
                        logger.error("error occurs when scanning the future, caused by " + t.getMessage());
                    }
                }
            }
        }, TIMEOUT_SCAN_INTERVAL, TIMEOUT_SCAN_INTERVAL, TimeUnit.MILLISECONDS);
    }

    public DefaultRpcFuture(int timeout, MessageRequest request, URL url){
        this.request = request;
        this.timeout = timeout > 0 ? timeout : url.getParameter(RpcConfig.TIMEOUT_KEY, RpcConfig.DEFAULT_TIMEOUT);
        id = request.getMessageId();

        futures.put(id, this);
    }

    public static Map<String, RpcFuture> getFutures(){
        return Collections.unmodifiableMap(futures);
    }

    public static void received(MessageResponse response){
        try{
            DefaultRpcFuture future = futures.get(response.getMessageId());
            if (future != null){
                future.doReceived(response);
            }else {
                logger.warn("timeout response arrived at " + (new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
                        .format(new Date())));
            }
        }finally {
            futures.remove(response.getMessageId());
        }
    }

    private void doReceived(MessageResponse response){
        lock.lock();
        try{
            this.response = response;
            done.signal();
        }finally {
            lock.unlock();
        }
        if (listeners.size() != 0)
            notifyRpcListeners();
    }

    public void notifyRpcListeners(){
        if (listeners.size() == 0)
            return;

        // 因为 response 为 volatile 类型的变量，所以先保存一份副本
        MessageResponse res = response;
        if (res == null){
            throw new IllegalStateException("response == null.");
        }

        List<RpcFutureListener> listenersCopy = new ArrayList<>(listeners);
        for (RpcFutureListener listener : listenersCopy) {
            listeners.remove(listener);
            doNotifyRpcListener(listener);
        }
    }

    public void doNotifyRpcListener(RpcFutureListener listener){
        if (listener == null)
            throw new IllegalArgumentException("listener == null.");

        // 因为 response 为 volatile 类型的变量，所以先保存一份副本
        MessageResponse res = response;
        if (res == null){
            throw new IllegalStateException("response == null.");
        }

        if (isCancelled()){
            logger.error("future is cancelled, cannot proceed to execute the listener.");
            return;
        }

        // 成功执行时，回调 listener 的 done 方法
        if (res.getInvokeStatus().isDone()){
            try {
                listener.done(res.getResult());
            } catch (Exception e) {
                logger.error("error occurs when call back the done method of listener with result " + res.getResult());
            }
            // 调用超时的时候，回调 listener 的 caught 方法
        }else if (res.getInvokeStatus().isTimeout()){
            try {
                RpcTimeoutException ex = new RpcTimeoutException(res.getError().getMessage());
                listener.caught(ex);
            }catch (Exception e){
                logger.error("error occurs when call back the caught method of listener with error " + res.getError().getMessage());
            }
            // 调用发生了其它异常时，回调 listener 的 caught 方法
        }else {
            try {
                RpcException ex = new RpcException(res.getError().getMessage());
                listener.caught(ex);
            }catch (Exception e){
                logger.error("error occurs when call back the caught method of listener with error " + res.getError().getMessage());
            }
        }
    }

    @Override
    public Object get() throws RemotingException, InterruptedException {
        return get(timeout);
    }

    @Override
    public Object get(int timeout) throws RemotingException, InterruptedException {
        if (timeout <= 0)
            timeout = RpcConfig.DEFAULT_TIMEOUT;

        if (!isDone()){
            long start = System.currentTimeMillis();
            lock.lock();
            try{
                while (!isDone()){
                    done.await(timeout, TimeUnit.MILLISECONDS);
                    if (isDone() || System.currentTimeMillis() - start >= timeout)
                        break;
                }
            } catch (InterruptedException e) {
                throw e;
            } finally {
                lock.unlock();
            }
        }

        return getReturnValue();
    }

    private Object getReturnValue() throws RpcTimeoutException {
        if (!isDone())
            throw new RpcTimeoutException("invoke method " + request.getMethodName() + " for service " + request.getInterfaceName()
                    + " timeout, cost " + (System.currentTimeMillis() - start) + ", time limit " + timeout);

        if (response.getInvokeStatus().isCancelled())
            throw new RpcException("future is cancelled.");

        if (response.getInvokeStatus().isExceptional())
            throw new RpcException("exception occurs when rpc server executes the service " + request.getInterfaceName()
                    + ", caused by " + response.getError().getMessage());

        if (response.getInvokeStatus().isDone()){
            return response.getResult();
        }

        // 这里也会抛出超时异常，是因为在 DefaultRpcFuture 的超时检测线程中，如果发现某个 future 超时，则会创建 response，并将
        // response 的 status 设置为 Timeout
        if (response.getInvokeStatus().isTimeout()){
            throw new RpcTimeoutException("invoke method " + request.getMethodName() + " for service " + request.getInterfaceName()
                    + " timeout, cost " + (System.currentTimeMillis() - start) + ", time limit " + timeout);
        }

        throw new RpcException("invoke method " + request.getMethodName() + " error.");
    }

    @Override
    public void addListener(RpcFutureListener listener) {
        if (isDone()){
            doNotifyRpcListener(listener);
        }else{
            lock.lock();
            try{
                listeners.add(listener);
            }finally {
                lock.unlock();
            }

            // 如果添加 listener 之后，发现 future 执行完成，那么立即执行注册的所有 listener，
            // 否则这个 listener 将不会被执行
            if (isDone()){
                notifyRpcListeners();
            }
        }
    }

    @Override
    public boolean isDone() {
        return response != null;
    }

    @Override
    public void cancel() {
        this.response = new MessageResponse();
        response.setMessageId(id);
        response.setInvokeStatus(MethodInvokeStatus.CANCELLED);
        futures.remove(id);
    }

    @Override
    public boolean isCancelled() {
        return response.getInvokeStatus().isCancelled();
    }

    public String getId() {
        return id;
    }
}
