package com.xu.rpc.async;

import com.xu.rpc.exception.RemotingException;

public interface RpcFuture {

    public Object get() throws RemotingException, InterruptedException;

    public Object get(int timeoutInMillis) throws RemotingException, InterruptedException;

    public void addListener(RpcFutureListener listener);

    public boolean isDone();

    public void cancel();

    public boolean isCancelled();

}
