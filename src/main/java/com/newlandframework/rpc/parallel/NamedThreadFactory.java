package com.newlandframework.rpc.parallel;

import org.apache.commons.lang3.StringUtils;

import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * 自定义的线程工厂，用来创建线程池中的线程
 */
public class NamedThreadFactory implements ThreadFactory {

    private static final AtomicInteger THREAD_NUMBER = new AtomicInteger(1);

    private final AtomicInteger mThreadNum = new AtomicInteger(1);

    private final String prefix;

    private final boolean daemoThread;

    private final ThreadGroup threadGroup;

    public NamedThreadFactory() {
        this("rpcserver-threadpool-" + THREAD_NUMBER.getAndIncrement(), false);
    }

    public NamedThreadFactory(String prefix) {
        this(prefix, false);
    }

    public NamedThreadFactory(String prefix, boolean daemon) {
        this.prefix = StringUtils.isNotEmpty(prefix) ? prefix + "-thread-" : "";
        //daemon表示创建的线程是否为守护线程
        daemoThread = daemon;
        SecurityManager s = System.getSecurityManager();
        //threadGroup表示当前线程所在的线程组
        threadGroup = (s == null) ? Thread.currentThread().getThreadGroup() : s.getThreadGroup();
    }

    @Override
    public Thread newThread(Runnable runnable) {
        //此线程工厂创建的线程的名字为RpcThreadPool-thread-线程编号
        String name = prefix + mThreadNum.getAndIncrement();
        Thread ret = new Thread(threadGroup, runnable, name);
        ret.setDaemon(daemoThread);
        return ret;
    }

    public ThreadGroup getThreadGroup() {
        return threadGroup;
    }
}

