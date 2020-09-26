package com.xu.rpc.filter.support;

import com.xu.rpc.cluster.support.wrapper.MockClusterWrapper;
import com.xu.rpc.commons.DateStore;
import com.xu.rpc.commons.URL;
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
import java.util.HashMap;
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

        URL url = invoker.getUrl();
        Map<String, String> map = new HashMap<>();
        ReflectionUtils utils = new ReflectionUtils();
        map.put(RpcConfig.METHOD_KEY, utils.getMethodSignature(invocation.getMethod()));
        URL keyUrl = new URL(url.getProtocol(), url.getHost(), url.getPort(), url.getPath(), map);

        // 黑名单集合中是否包括这个 keyUrl，也就是要调用的服务，如果包括，则输出一条警告日志，返回一个空结果
        if (blacklist.contains(keyUrl.toFullString())){
            logger.warn("service " + keyUrl.toFullString() + " cannot be invoked, since mock configuration is set");
            return new RpcResult();
        }

        return invoker.invoke(invocation);
    }

}
