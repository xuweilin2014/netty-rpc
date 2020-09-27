package com.xu.rpc.commons.limiter;

import com.xu.rpc.commons.URL;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;

public abstract class AbstractRateLimiterFactory implements RateLimiterFactory {

    private static final Map<String, FlowLimiter> limiters = new ConcurrentHashMap<>();

    private static final ReentrantLock lock = new ReentrantLock();

    @Override
    public FlowLimiter getRateLimiter(URL url) {
        String serviceKey = url.getServiceName();
        lock.lock();
        try{
            FlowLimiter limiter = limiters.get(serviceKey);
            if (limiter == null){
                limiters.put(serviceKey, createRateLimiter(url));
                limiter = limiters.get(serviceKey);
            }

            return limiter;
        }finally {
            lock.unlock();
        }
    }

    public abstract FlowLimiter createRateLimiter(URL url);
}
