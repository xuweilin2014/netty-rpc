package com.xu.rpc.filter.support;

import com.xu.rpc.commons.URL;
import com.xu.rpc.commons.exception.RpcException;
import com.xu.rpc.commons.limiter.FlowLimiter;
import com.xu.rpc.commons.limiter.RateLimiterFactory;
import com.xu.rpc.commons.util.AdaptiveExtensionUtils;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;
import com.xu.rpc.core.extension.Activate;
import com.xu.rpc.filter.ChainFilter;

@Activate(group = {RpcConfig.PROVIDER}, value = RpcConfig.LIMITER_KEY, order = 2)
public class RateLimiterChainFilter implements ChainFilter {

    @Override
    public RpcResult intercept(Invoker invoker, RpcInvocation invocation) throws RpcException {
        URL url = invoker.getUrl();
        String limiter = url.getParameter(RpcConfig.LIMITER_KEY);
        // 如果用户开启了限流的话
        if (limiter != null && limiter.length() > 0){
            RateLimiterFactory limiterFactory = AdaptiveExtensionUtils.getLimiterFactory(url);
            // 获取到对应的限流工具
            FlowLimiter rateLimiter = limiterFactory.getRateLimiter(url);
            // 如果 tryAcquire 返回 false，那么就表示此请求被限流
            if (!rateLimiter.tryAcquire()){
                throw new RpcException("the request is blocked because of the rate limiter.");
            }
        }
        return invoker.invoke(invocation);
    }

}
