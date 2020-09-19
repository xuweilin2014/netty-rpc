package com.xu.rpc.filter.support;

import com.xu.rpc.cluster.support.wrapper.MockClusterWrapper;
import com.xu.rpc.commons.DateStore;
import com.xu.rpc.commons.util.ReflectionUtils;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;
import com.xu.rpc.core.extension.Activate;
import com.xu.rpc.core.extension.Attribute;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.filter.ChainFilter;
import com.xu.rpc.protocol.Invoker;
import org.apache.log4j.Logger;

import java.util.Base64;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;


@Activate(group = RpcConfig.PROVIDER, order = -1)
public class MockChainFilter implements ChainFilter {

    @SuppressWarnings("unchecked")
    private static Set<String> blacklist = (Set<String>) DateStore.get(RpcConfig.MOCK_SET_KEY);

    private static final Logger logger = Logger.getLogger(MockChainFilter.class);

    @SuppressWarnings("unchecked")
    @Override
    public RpcResult intercept(Invoker invoker, RpcInvocation invocation) throws RpcException {
        if (blacklist == null){
            DateStore.put(RpcConfig.MOCK_SET_KEY, new CopyOnWriteArraySet<>());
            blacklist = (Set<String>) DateStore.get(RpcConfig.MOCK_SET_KEY);
        }

        String interfaceName = invocation.getServiceType().getName();
        String methodName = new ReflectionUtils().getMethodSignature(invocation.getMethod());
        if (blacklist.contains(interfaceName + RpcConfig.HEX_SEPARATOR + methodName)){
            logger.warn("service " + interfaceName + RpcConfig.HEX_SEPARATOR + methodName + " cannot be invoked, since mock configuration is set.");
            return new RpcResult();
        }

        return invoker.invoke(invocation);
    }

}
