package com.xu.rpc.commons.limiter.token;

import com.xu.rpc.commons.URL;
import com.xu.rpc.commons.limiter.AbstractRateLimiterFactory;
import com.xu.rpc.commons.limiter.FlowLimiter;
import com.xu.rpc.core.RpcConfig;

public class TokenFlowLimiterFactory extends AbstractRateLimiterFactory {

    @Override
    public FlowLimiter createRateLimiter(URL url) {
        return new TokenFlowLimiter(url.getParameter(RpcConfig.RATE_KEY,
                RpcConfig.DEFAULT_RATE_LIMIT_PER_SECOND));
    }

}
