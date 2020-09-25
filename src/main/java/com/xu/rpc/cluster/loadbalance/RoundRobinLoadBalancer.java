package com.xu.rpc.cluster.loadbalance;

import com.sun.org.apache.bcel.internal.generic.IF_ACMPEQ;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.commons.URL;
import lombok.Getter;
import lombok.Setter;
import org.apache.log4j.Logger;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.IntBinaryOperator;

// 加权轮询：对轮询过程进行加权，以调控每台服务器的负载。经过加权后，每台服务器能够得到的请求数比例，接近或等于他们的权重比
public class RoundRobinLoadBalancer extends AbstractLoadBalancer{

    public static final String NAME = "roundrobin";

    private static final Map<String, Map<String, RoundRobinInvoker>> currentWeights = new ConcurrentHashMap<>();

    /*
     * 使用 Nginx 的平滑加权轮询（weighted round robin）调度算法的过程如下：
     *
     * 假设有 N 台实例 S = {S1, S2, …, Sn}，配置权重 W = {W1, W2, …, Wn}，有效权重 CW = {CW1, CW2, …, CWn}。
     * 每个实例 i 除了存在一个配置权重 Wi 外，还存在一个当前有效权重 CWi，且 CWi 初始化为 Wi；指示变量 currentPos 表示当前选择的实例 ID，
     * 初始化为 -1；所有实例的配置权重和为 weightSum；
     *
     * 1、初始每个实例 i 的当前有效权重 CWi 为配置权重 Wi，并求得配置权重和 weightSum；
     * 2、选出当前有效权重最大的实例，将当前有效权重 CWi 减去所有实例的权重和 weightSum，且变量 currentPos 指向此位置；
     * 3、将每个实例 i 的当前有效权重 CWi 都加上配置权重 Wi；
     * 4、此时变量 currentPos 指向的实例就是需调度的实例；
     * 5、每次调度重复上述步骤 2、3、4；
     *
     * 具体实现是：有 n 个节点，n 个节点的有效权重 CWi 初始值为 [0,0,...,0]，也就是都为 0。接下来选择分为两步：
     * 1、为每个节点的有效权重 CWi 加上它自身的配置权重值 Wi；
     * 2、选择有效权重最大的节点减去配置权重和 WeightSum；
     * 然后每次调度不断循环，就可以找出对应的服务器
     */
    @Override
    public Invoker doSelect(RpcInvocation invocation, List<Invoker> invokers, URL url) {
        String key = invokers.get(0).getUrl().getServiceName() + RpcConfig.HEX_SEPARATOR + invocation.getMethodName();
        int weightSum = 0;
        int maxWeight = Integer.MIN_VALUE;
        RoundRobinInvoker rrInvoker = null;
        Invoker selectedInvoker = null;

        Map<String, RoundRobinInvoker> map = currentWeights.get(key);
        if (map == null){
            currentWeights.put(key, new ConcurrentHashMap<>());
            map = currentWeights.get(key);
        }

        // 先淘汰掉 map 中已经无法访问的 invoker
        Map<String, RoundRobinInvoker> copyRoundRobin = new ConcurrentHashMap<>(map);
        for (Map.Entry<String, RoundRobinInvoker> entry : copyRoundRobin.entrySet()) {
            RoundRobinInvoker roundRobinInvoker = entry.getValue();
            if (!roundRobinInvoker.getInvoker().isAvailable()){
                copyRoundRobin.remove(entry.getKey());
            }
        }
        currentWeights.put(key, copyRoundRobin);

        for (Invoker invoker : invokers) {
            String urlKey = invoker.getUrl().toFullString();
            // 获取到 invoker 的最新权重值
            int weight = invoker.getUrl().getParameter(RpcConfig.WEIGHT_KEY, RpcConfig.DEFAULT_WEIGHT);
            RoundRobinInvoker roundRobinInvoker = copyRoundRobin.get(urlKey);

            // 检测当前的 invoker 是否有 roundRobinInvoker 没有创建，也就是说 invoker 是否是新上线的
            if (roundRobinInvoker == null){
                roundRobinInvoker = new RoundRobinInvoker(weight, invoker);
                copyRoundRobin.put(urlKey, roundRobinInvoker);
                roundRobinInvoker = copyRoundRobin.get(urlKey);
            }

            // 检测 invoker 的配置权重值是否被更新了，如果是的话，就相应更新 roundRobinInvoker 中的配置权重
            if (roundRobinInvoker.getWeight() != weight){
                roundRobinInvoker.setWeight(weight);
            }

            // 给每个结点的有效权重加上自身的配置权重
            roundRobinInvoker.syncChangeCurrent(weight);
            // 选择有效权重最大的结点，并且保存下来，作为通过负载均衡选择到的 invoker
            if (roundRobinInvoker.getCurrentWeight() > maxWeight){
                selectedInvoker = invoker;
                rrInvoker = roundRobinInvoker;
                maxWeight = roundRobinInvoker.getCurrentWeight();
            }

            // 计算配置权重之和
            weightSum += roundRobinInvoker.getWeight();
        }

        if (selectedInvoker != null){
            // 选择有效权重最大的节点减去配置权重和 WeightSum
            rrInvoker.syncChangeCurrent(-weightSum);
            return selectedInvoker;
        }

        throw new RpcException("cannot choose an invoker with weighted round robin policy");
    }

    @Getter
    @Setter
    private static class RoundRobinInvoker {
        // 配置权重
        private int weight;
        // 当前权重
        private AtomicInteger currentWeight = new AtomicInteger(0);

        private Invoker invoker;

        RoundRobinInvoker(int weight, Invoker invoker){
            this.weight = weight;
            this.invoker = invoker;
        }

        void syncChangeCurrent(int delta){
            currentWeight.accumulateAndGet(delta, new IntBinaryOperator() {
                @Override
                public int applyAsInt(int left, int right) {
                    return left + right;
                }
            });
        }

        int getCurrentWeight(){
            return currentWeight.get();
        }
    }


}
