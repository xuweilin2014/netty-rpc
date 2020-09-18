package com.xu.rpc.async;

import com.xu.rpc.core.RpcResult;
import com.xu.rpc.exception.RemotingException;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.exception.RpcTimeoutException;
import lombok.SneakyThrows;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

public class FutureWrapper implements Future {

    private final RpcFuture future;

    public FutureWrapper(RpcFuture future) {
        this.future = future;
    }

    @Override
    public boolean cancel(boolean mayInterruptIfRunning) {
        future.cancel();
        return true;
    }

    @Override
    public boolean isCancelled() {
        return future.isCancelled();
    }

    @Override
    public boolean isDone() {
        return future.isDone();
    }

    @Override
    public Object get() throws InterruptedException, ExecutionException {
        try {
            return future.get();
        } catch (RemotingException e) {
            throw new ExecutionException(e);
        } catch (Throwable throwable) {
            throw new RpcException(throwable.getMessage());
        }
    }

    @Override
    public Object get(long timeout, TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
        try {
            return future.get((int) unit.convert(timeout, TimeUnit.MILLISECONDS));
        } catch (RpcTimeoutException ex){
            throw new TimeoutException(ex.getMessage());
        }catch (RemotingException e) {
            throw new ExecutionException(e);
        } catch (Throwable throwable) {
            throw new RpcException(throwable.getMessage());
        }
    }

}
