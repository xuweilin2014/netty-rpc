package com.xu.rpc.cluster.loadbalance;

import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.util.URL;
import org.apache.log4j.Logger;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

// 加权的轮询操作
public class RoundRobinLoadBalancer extends AbstractLoadBalancer{

    public static final String NAME = "roundrobin";

    private static final Logger logger = Logger.getLogger(RoundRobinLoadBalancer.class);

    private static final Map<String, AtomicInteger> callers = new ConcurrentHashMap<>();

    @Override
    public Invoker doSelect(RpcInvocation invocation, List<Invoker> invokers, URL url) {
        // key 为全限定类名 + 方法名
        String key = invocation.getServiceType().getName() + "." + invocation.getMethodName();
        Map<Invoker, AtomicInteger> invokerToWeight = new HashMap<>();
        int weightSum = 0;
        int maxWeight = Integer.MIN_VALUE;

        for (Invoker invoker : invokers) {
            try {
                int weight = invoker.getURL().getParameter(RpcConfig.WEIGHT_KEY, RpcConfig.DEFAULT_WEIGHT);
                weightSum += weight;
                maxWeight = Math.max(maxWeight, weight);
                invokerToWeight.put(invoker, new AtomicInteger(weight));
            } catch (Exception e) {
                logger.error("failed to get the weight of the invoker " + invoker);
            }
        }

        AtomicInteger times = callers.get(key);
        if (times == null){
            callers.put(key, new AtomicInteger(0));
            times = callers.get(key);
        }

        int currentTimes = times.getAndIncrement();
        int mod = currentTimes % weightSum;

        for (int i = 0; i < maxWeight; i++) {
            for (Map.Entry<Invoker, AtomicInteger> entry : invokerToWeight.entrySet()) {
                Invoker invoker = entry.getKey();
                AtomicInteger weight = entry.getValue();

                if (mod == 0 && weight.get() > 0)
                    return invoker;

                if (weight.get() > 0){
                    weight.decrementAndGet();
                    mod--;
                }
            }
        }

        throw new RpcException("cannot choose an invoker with weighted round robin policy.");
    }
}
