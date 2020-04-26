package com.newlandframework.rpc.parallel;

import com.newlandframework.rpc.core.RpcSystemConfig;
import com.newlandframework.rpc.jmx.ThreadPoolMonitorProvider;
import com.newlandframework.rpc.jmx.ThreadPoolStatus;
import com.newlandframework.rpc.parallel.policy.AbortPolicy;
import com.newlandframework.rpc.parallel.policy.BlockingPolicy;
import com.newlandframework.rpc.parallel.policy.CallerRunsPolicy;
import com.newlandframework.rpc.parallel.policy.DiscardedPolicy;
import com.newlandframework.rpc.parallel.policy.RejectedPolicy;
import com.newlandframework.rpc.parallel.policy.RejectedPolicyType;

import javax.management.InstanceNotFoundException;
import javax.management.MBeanException;
import javax.management.MalformedObjectNameException;
import javax.management.ReflectionException;
import java.io.IOException;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.Executor;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.RejectedExecutionHandler;

/**
 *  RpcThreadPool是对线程池的封装
 */
public class RpcThreadPool {
    private static final Timer TIMER = new Timer("ThreadPoolMonitor", true);
    private static long monitorDelay = 100L;
    private static long monitorPeriod = 300L;

    private static RejectedExecutionHandler createPolicy() {
        RejectedPolicyType rejectedPolicyType = RejectedPolicyType.fromString(System.getProperty(RpcSystemConfig.SYSTEM_PROPERTY_THREADPOOL_REJECTED_POLICY_ATTR, "AbortPolicy"));

        switch (rejectedPolicyType) {
            case BLOCKING_POLICY:
                return new BlockingPolicy();
            case CALLER_RUNS_POLICY:
                return new CallerRunsPolicy();
            case ABORT_POLICY:
                return new AbortPolicy();
            case REJECTED_POLICY:
                return new RejectedPolicy();
            case DISCARDED_POLICY:
                return new DiscardedPolicy();
            default: {
                break;
            }
        }

        return null;
    }

    private static BlockingQueue<Runnable> createBlockingQueue(int queues) {
        BlockingQueueType queueType = BlockingQueueType.fromString(System.getProperty(RpcSystemConfig.SYSTEM_PROPERTY_THREADPOOL_QUEUE_NAME_ATTR, "LinkedBlockingQueue"));

        switch (queueType) {
            case LINKED_BLOCKING_QUEUE:
                return new LinkedBlockingQueue<Runnable>();
            case ARRAY_BLOCKING_QUEUE:
                return new ArrayBlockingQueue<Runnable>(RpcSystemConfig.SYSTEM_PROPERTY_PARALLEL * queues);
            case SYNCHRONOUS_QUEUE:
                return new SynchronousQueue<Runnable>();
            default: {
                break;
            }
        }

        return null;
    }

    public static Executor getExecutor(int threads, int queues) {
        // 独立出线程池主要是为了应对复杂耗I/O操作的业务，不阻塞netty的handler线程而引入
        // 当然如果业务足够简单，把处理逻辑写入netty的handler（ChannelInboundHandlerAdapter）也未尝不可
        System.out.println("ThreadPool Core[threads:" + threads + ", queues:" + queues + "]");
        String name = "RpcThreadPool";
        ThreadPoolExecutor executor = new ThreadPoolExecutor(threads, threads, 0, TimeUnit.MILLISECONDS,
                createBlockingQueue(queues),
                new NamedThreadFactory(name, true), createPolicy());
        return executor;
    }

    public static Executor getExecutorWithJmx(int threads, int queues) {
        final ThreadPoolExecutor executor = (ThreadPoolExecutor) getExecutor(threads, queues);
        TIMER.scheduleAtFixedRate(new TimerTask() {

            @Override
            public void run() {
                ThreadPoolStatus status = new ThreadPoolStatus();
                status.setPoolSize(executor.getPoolSize());
                status.setActiveCount(executor.getActiveCount());
                status.setCorePoolSize(executor.getCorePoolSize());
                status.setMaximumPoolSize(executor.getMaximumPoolSize());
                status.setLargestPoolSize(executor.getLargestPoolSize());
                status.setTaskCount(executor.getTaskCount());
                status.setCompletedTaskCount(executor.getCompletedTaskCount());

                try {
                    ThreadPoolMonitorProvider.monitor(status);
                } catch (IOException e) {
                    e.printStackTrace();
                } catch (MalformedObjectNameException e) {
                    e.printStackTrace();
                } catch (ReflectionException e) {
                    e.printStackTrace();
                } catch (MBeanException e) {
                    e.printStackTrace();
                } catch (InstanceNotFoundException e) {
                    e.printStackTrace();
                }
            }
        }, monitorDelay, monitorDelay);
        return executor;
    }
}

