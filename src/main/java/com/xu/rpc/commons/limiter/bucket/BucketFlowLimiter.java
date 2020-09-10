package com.xu.rpc.commons.limiter.bucket;

import com.xu.rpc.commons.limiter.FlowLimiter;
import com.xu.rpc.commons.limiter.TimestampHolder;

import java.util.Optional;
import java.util.concurrent.*;
import java.util.concurrent.locks.LockSupport;

public class BucketFlowLimiter implements FlowLimiter {

    private static final int DEFAULT_RATE_LIMIT_PER_SECOND = Integer.MAX_VALUE;

    private static final long NANOSECOND = 1000 * 1000 * 1000;

    private BlockingQueue<Thread> bucket;

    public BucketFlowLimiter() {
        this(DEFAULT_RATE_LIMIT_PER_SECOND);
    }

    public BucketFlowLimiter(int rate) {
        if (rate <= 0) {
            throw new IllegalArgumentException();
        }

        bucket = new LinkedBlockingQueue<>(rate);
        ExecutorService threadPool = Executors.newSingleThreadExecutor();

        TimestampHolder holder = new TimestampHolder(System.nanoTime());
        long interval = NANOSECOND / rate;
        threadPool.submit(() -> {
            while (true) {
                long cur = System.nanoTime();
                if (cur - holder.getTimestamp() >= interval) {
                    Thread thread = bucket.poll();
                    Optional.ofNullable(thread).ifPresent(LockSupport::unpark);
                    holder.setTimestamp(cur);
                }

                try {
                    TimeUnit.NANOSECONDS.sleep(1000);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
        });
    }

    @Override
    public boolean tryAcquire() {
        if (bucket.remainingCapacity() == 0) {
            return false;
        }

        bucket.add(Thread.currentThread());
        LockSupport.park();
        return true;
    }

}
