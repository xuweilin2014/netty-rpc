package com.xu.rpc.async;

import java.util.concurrent.Callable;
import java.util.concurrent.FutureTask;


public class AsyncFuture<V> extends FutureTask<V> {
    private Thread callerThread;
    private Thread runnerThread;
    private long startTime = 0L;
    private long endTime = 0L;

    public AsyncFuture(Callable<V> callable) {
        super(callable);
        callerThread = Thread.currentThread();
    }

    /**
     * 当FutureTask中的run方法运行完时，就会调用done方法，设置好endTime时间。
     */
    @Override
    protected void done() {
        endTime = System.currentTimeMillis();
    }

    @Override
    public void run() {
        startTime = System.currentTimeMillis();
        runnerThread = Thread.currentThread();
        super.run();
    }

    public Thread getCallerThread() {
        return callerThread;
    }

    public Thread getRunnerThread() {
        return runnerThread;
    }

    public long getStartTime() {
        return startTime;
    }

    public long getEndTime() {
        return endTime;
    }
}

