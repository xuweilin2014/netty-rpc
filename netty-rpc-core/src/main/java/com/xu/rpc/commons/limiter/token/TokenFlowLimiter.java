package com.xu.rpc.commons.limiter.token;

import com.xu.rpc.commons.limiter.FlowLimiter;
import com.xu.rpc.commons.limiter.TimestampHolder;

import java.util.Queue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

public class TokenFlowLimiter implements FlowLimiter {

    private static final int DEFAULT_RATE_LIMIT_PER_SECOND = Integer.MAX_VALUE;

    private static final long NANOSECOND = 1000 * 1000 * 1000;

    private final long interval;

    private static final Byte TOKEN = 0;

    private Queue<Byte> tokenBucket;

    public TokenFlowLimiter() {
        this(DEFAULT_RATE_LIMIT_PER_SECOND);
    }

    public TokenFlowLimiter(int rate) {
        if (rate <= 0) {
            throw new IllegalArgumentException();
        }

        tokenBucket = new LinkedBlockingQueue<>(rate);
        ExecutorService threadPool = Executors.newSingleThreadExecutor();

        TimestampHolder holder = new TimestampHolder(System.nanoTime());
        this.interval = NANOSECOND / rate;
        threadPool.submit(() -> {
            while (true) {
                long cur = System.nanoTime();
                if (cur - holder.getTimestamp() >= interval) {
                    tokenBucket.offer(TOKEN);
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
        Object token = tokenBucket.poll();
        return token == null;
    }

}
