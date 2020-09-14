package com.xu.rpc.cluster.loadbalance;

import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.commons.URL;
import org.apache.log4j.Logger;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

// 加权轮询：对轮询过程进行加权，以调控每台服务器的负载。经过加权后，每台服务器能够得到的请求数比例，接近或等于他们的权重比
public class RoundRobinLoadBalancer extends AbstractLoadBalancer{

    public static final String NAME = "roundrobin";

    private static final Logger logger = Logger.getLogger(RoundRobinLoadBalancer.class);

    private static final Map<String, AtomicInteger> callers = new ConcurrentHashMap<>();

    @Override
    public Invoker doSelect(RpcInvocation invocation, List<Invoker> invokers, URL url) {
        // key 为全限定类名 + 方法名
        String key = invocation.getServiceType().getName() + "." + invocation.getMethodName();
        Map<Invoker, IntegerWrapper> invokerToWeight = new HashMap<>();
        int weightSum = 0;
        int maxWeight = Integer.MIN_VALUE;

        for (Invoker invoker : invokers) {
            try {
                int weight = invoker.getUrl().getParameter(RpcConfig.WEIGHT_KEY, RpcConfig.DEFAULT_WEIGHT);
                weightSum += weight;
                maxWeight = Math.max(maxWeight, weight);
                invokerToWeight.put(invoker, new IntegerWrapper(weight));
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

        // 个人觉得进行 maxWeight 次遍历的主要原因是： mod 的取值范围为 [0, weightSum - 1]，而 maxWeight * invokerLen >= weightSum，那么 maxWeight * invokerLen > weightSum - 1
        // 所以这样肯定可以最终选择一个 invoker
        for (int i = 0; i < maxWeight; i++) {
            for (Map.Entry<Invoker, IntegerWrapper> entry : invokerToWeight.entrySet()) {
                Invoker invoker = entry.getKey();
                IntegerWrapper weight = entry.getValue();

                if (mod == 0 && weight.get() > 0)
                    return invoker;

                if (weight.get() > 0){
                    weight.decrement();
                    mod--;
                }
            }
        }

        throw new RpcException("cannot choose an invoker with weighted round robin policy.");
    }

    private static class IntegerWrapper{

        private int val;

        public IntegerWrapper(int value){
            this.val = value;
        }

        public void decrement(){
            this.val--;
        }

        public int get(){
            return val;
        }
    }
}
