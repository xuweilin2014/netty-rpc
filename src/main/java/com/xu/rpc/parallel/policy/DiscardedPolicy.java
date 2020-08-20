package com.xu.rpc.parallel.policy;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.RejectedExecutionHandler;
import java.util.concurrent.ThreadPoolExecutor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * DiscardPolicy是将任务队列中的任务丢弃一半，然后再把Runnable任务放入到任务队列中
 */
public class DiscardedPolicy implements RejectedExecutionHandler {
    private static final Logger LOG = LoggerFactory.getLogger(DiscardedPolicy.class);

    private String threadName;

    public DiscardedPolicy() {
        this(null);
    }

    public DiscardedPolicy(String threadName) {
        this.threadName = threadName;
    }

    @Override
    public void rejectedExecution(Runnable runnable, ThreadPoolExecutor executor) {
        if (threadName != null) {
            LOG.error("RPC Thread pool [{}] is exhausted, executor={}", threadName, executor.toString());
        }

        if (!executor.isShutdown()) {
            BlockingQueue<Runnable> queue = executor.getQueue();
            int discardSize = queue.size() >> 1;
            for (int i = 0; i < discardSize; i++) {
                queue.poll();
            }

            queue.offer(runnable);
        }
    }
}
