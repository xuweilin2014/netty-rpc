package com.newlandframework.rpc.filter.support;

import com.newlandframework.rpc.core.RpcResult;
import com.newlandframework.rpc.core.RpcSystemConfig;
import com.newlandframework.rpc.core.extension.Activate;
import com.newlandframework.rpc.filter.AbstractChainFilter;
import com.newlandframework.rpc.model.MessageRequest;
import com.newlandframework.rpc.protocol.Invoker;
import org.apache.commons.lang3.time.StopWatch;
import org.apache.log4j.Logger;

import java.util.Arrays;

@Activate(group = {RpcSystemConfig.PROVIDER}, order = 1)
public class TimeoutChainFilter extends AbstractChainFilter {

    public static final Logger logger = Logger.getLogger(TimeoutChainFilter.class);

    private StopWatch sw = new StopWatch();

    @Override
    public Object doIntercept(Invoker invoker, MessageRequest request) {
        sw.reset();
        sw.start();
        RpcResult result = (RpcResult) invoker.invoke(request);
        sw.stop();
        long elapsed = sw.getTime();
        if (invoker.getURL() != null
                && elapsed > invoker.getURL().getParameter("timeout", Integer.MAX_VALUE)) {
            logger.warn("invoke time out. method: " + request.getMethodName()
                        + " arguments: " + Arrays.toString(request.getParametersVal()) + " , url is "
                        + invoker.getURL() + ", invoke elapsed " + elapsed + " ms.");

        }
        result.setInvokeTimespan(sw.getTime());
        return result;
    }
}
