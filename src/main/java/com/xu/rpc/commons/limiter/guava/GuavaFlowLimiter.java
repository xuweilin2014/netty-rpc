package com.xu.rpc.commons.limiter.guava;

import com.google.common.util.concurrent.RateLimiter;
import com.xu.rpc.commons.limiter.FlowLimiter;
import java.util.concurrent.atomic.AtomicLong;

public class GuavaFlowLimiter implements FlowLimiter {

    private AtomicLong counter;

    private final RateLimiter limiter;

    public GuavaFlowLimiter(int rate){
        this.limiter = RateLimiter.create(rate);
    }

    @Override
    public boolean tryAcquire() {
        return limiter.tryAcquire();
    }

}
