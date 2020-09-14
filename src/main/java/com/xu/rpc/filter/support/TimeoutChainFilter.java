package com.xu.rpc.filter.support;

import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.extension.Activate;
import com.xu.rpc.filter.ChainFilter;
import com.xu.rpc.protocol.Invoker;
import org.apache.commons.lang3.time.StopWatch;
import org.apache.log4j.Logger;

import java.util.Arrays;

@Activate(group = {RpcConfig.PROVIDER}, order = 1)
public class TimeoutChainFilter implements ChainFilter {

    public static final Logger logger = Logger.getLogger(TimeoutChainFilter.class);

    private StopWatch sw = new StopWatch();

    @Override
    public RpcResult intercept(Invoker invoker, RpcInvocation invocation) {
        sw.reset();
        sw.start();
        RpcResult result = invoker.invoke(invocation);
        sw.stop();
        long elapsed = sw.getTime();
        if (invoker.getUrl() != null
                && elapsed > invoker.getUrl().getParameter("timeout", Integer.MAX_VALUE)) {
            logger.warn("invoke time out. method: " + invocation.getMethodName()
                        + " arguments: " + Arrays.toString(invocation.getParameters()) + " , url is "
                        + invoker.getUrl() + ", invoke elapsed " + elapsed + " ms.");

        }
        result.setInvokeTimespan(sw.getTime());
        return result;
    }
}
