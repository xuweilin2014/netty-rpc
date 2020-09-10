package com.xu.rpc.commons.limiter.guava;

import com.xu.rpc.commons.URL;
import com.xu.rpc.commons.limiter.AbstractRateLimiterFactory;
import com.xu.rpc.commons.limiter.FlowLimiter;
import com.xu.rpc.core.RpcConfig;

public class GuavaFlowLimiterFactory extends AbstractRateLimiterFactory {

    @Override
    public FlowLimiter createRateLimiter(URL url) {
        return new GuavaFlowLimiter(url.getParameter(RpcConfig.RATE_KEY,
                RpcConfig.DEFAULT_RATE_LIMIT_PER_SECOND));
    }

}
