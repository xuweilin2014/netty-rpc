package com.xu.rpc.filter.support;

import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.extension.Activate;
import com.xu.rpc.filter.ChainFilter;
import org.apache.log4j.Logger;

import java.util.Arrays;

@Activate(group = {RpcConfig.PROVIDER}, order = 11)
public class TimeoutChainFilter implements ChainFilter {

    public static final Logger logger = Logger.getLogger(TimeoutChainFilter.class);

    @Override
    public RpcResult intercept(Invoker invoker, RpcInvocation invocation) {
        long start = System.currentTimeMillis();
        RpcResult result = invoker.invoke(invocation);
        long elapsed = System.currentTimeMillis() - start;
        if (invoker.getUrl() != null
                && elapsed > invoker.getUrl().getParameter("timeout", Integer.MAX_VALUE)) {
            logger.warn("invoke time out. method: " + invocation.getMethodName()
                        + " arguments: " + Arrays.toString(invocation.getParameters()) + " , url is "
                        + invoker.getUrl() + ", invoke elapsed " + elapsed + " ms.");

        }
        result.setInvokeTimespan(elapsed);
        return result;
    }
}
