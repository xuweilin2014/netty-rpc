package com.xu.rpc.commons.limiter.bucket;

import com.xu.rpc.commons.URL;
import com.xu.rpc.commons.limiter.AbstractRateLimiterFactory;
import com.xu.rpc.commons.limiter.FlowLimiter;
import com.xu.rpc.core.RpcConfig;

public class BucketFlowLimiterFactory extends AbstractRateLimiterFactory {

    @Override
    public FlowLimiter createRateLimiter(URL url) {
        return new BucketFlowLimiter(url.getParameter(RpcConfig.RATE_KEY,
                RpcConfig.DEFAULT_RATE_LIMIT_PER_SECOND));
    }

}
