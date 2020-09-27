package com.xu.rpc.commons.limiter;

import com.xu.rpc.commons.URL;

public interface RateLimiterFactory {

    FlowLimiter getRateLimiter(URL url);

}
