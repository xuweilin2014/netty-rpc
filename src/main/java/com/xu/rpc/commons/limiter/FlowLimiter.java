package com.xu.rpc.commons.limiter;

public interface FlowLimiter {

    boolean tryAcquire();

}
