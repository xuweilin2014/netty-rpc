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
import java.util.Random;

// 加权随机
public class RandomLoadBalancer extends AbstractLoadBalancer {

    private static final Logger logger = Logger.getLogger(RandomLoadBalancer.class);

    public static final String NAME = "random";

    private static final Random random = new Random();

    @Override
    public Invoker doSelect(RpcInvocation invocation, List<Invoker> invokers, URL url) throws RpcException {
        int weightSum = 0;
        Map<Invoker, Integer> invokerToWeight = new HashMap<>();

        for (Invoker invoker : invokers) {
            try {
                int weight = invoker.getURL().getParameter(RpcConfig.WEIGHT_KEY, RpcConfig.DEFAULT_WEIGHT);
                invokerToWeight.put(invoker, weight);
                weightSum += weight;
            } catch (Exception e) {
                logger.error("failed to get the weight of the invoker " + invoker);
            }
        }

        int target = random.nextInt(weightSum);
        for (Map.Entry<Invoker, Integer> entry : invokerToWeight.entrySet()) {
            Invoker invoker = entry.getKey();
            Integer weight = entry.getValue();

            target -= weight;
            if (target < 0)
                return invoker;
        }

        throw new RpcException("cannot choose an invoker with weighted random policy.");
    }
}
