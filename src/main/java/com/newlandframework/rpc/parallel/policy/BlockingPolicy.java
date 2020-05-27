package com.newlandframework.rpc.parallel.policy;

import java.util.concurrent.RejectedExecutionHandler;
import java.util.concurrent.ThreadPoolExecutor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * BlockingPolicy是将任务直接提交到任务队列中，如果阻塞队列是满的话，直接阻塞等待阻塞队列中有任务被工作线程
 * 获取执行消耗掉
 */
public class BlockingPolicy implements RejectedExecutionHandler {
    private static final Logger LOG = LoggerFactory.getLogger(BlockingPolicy.class);

    private String threadName;

    public BlockingPolicy() {
        this(null);
    }

    public BlockingPolicy(String threadName) {
        this.threadName = threadName;
    }

    @Override
    public void rejectedExecution(Runnable runnable, ThreadPoolExecutor executor) {
        if (threadName != null) {
            LOG.error("RPC Thread pool [{}] is exhausted, executor={}", threadName, executor.toString());
        }

        if (!executor.isShutdown()) {
            try {
                executor.getQueue().put(runnable);
            } catch (InterruptedException e) {
            }
        }
    }
}

