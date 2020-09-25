package com.xu.rpc.cluster.loadbalance;

import com.xu.rpc.commons.URL;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.protocol.Invoker;

import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;

// 一致性哈希
public class ConsistentHashLoadBalancer extends AbstractLoadBalancer{

    public static final String NAME = "hash";

    private static Map<String, ConsistentHashSelector> selectors = new ConcurrentHashMap<>();

    @Override
    public Invoker doSelect(RpcInvocation invocation, List<Invoker> invokers, URL url) throws RpcException {
        // key的值一般为 服务全类名#方法名，比如：com.dubbo.simple.common.DemoService.sayHello
        String key = invokers.get(0).getUrl().getServiceName() + RpcConfig.HEX_SEPARATOR + invocation.getMethodName();

        // 当满足两个条件之一时，会创建或者说重新创建 selector
        // 1.与 key 相对应的 selector 还没有被创建过，selector == null
        // 2.与 key 相对应的 invokers 发生了变化，可能是数量发生了变化，也可能是某一个 invoker 的状态发生了变化，selector.hashcode != invokers.hashCode()
        //
        // 由于 Invoker 重写了 hashcode 方法，和对象中的每一个属性值有关；List 的 hashcode 值与 List 中每一个 Invoker 的
        // hashcode 值有关，因此当 List 中的 invoker 增加、删除或者属性发生改变时，都会影响到 List 的 hashcode 值
        ConsistentHashSelector selector = selectors.get(key);
        if (selector == null || selector.hashcode != invokers.hashCode()){
            selectors.put(key, new ConsistentHashSelector(invokers, invokers.hashCode()));
            selector = selectors.get(key);
        }

        // 调用 select 方法进行负载均衡逻辑
        return selector.select(invocation);
    }

    private static class ConsistentHashSelector{

        private static final int REPLICA_NUMBER = 160;

        private TreeMap<Long, Invoker> virtualNodes;

        private int hashcode;

        public ConsistentHashSelector(List<Invoker> invokers, int hashcode){
            this.hashcode = hashcode;
            virtualNodes = new TreeMap<>();

            // 为每个 invoker 生成 160 个虚拟节点
            for (Invoker invoker : invokers) {
                for (int i = 0; i < REPLICA_NUMBER; i++) {
                    String address = invoker.getUrl().getAddress();
                    // 在生成虚拟节点的时候，使用的 hash 字符串为：ip:port + 节点序号
                    long key = FNVHash(address + i);
                    virtualNodes.put(key, invoker);
                }
            }
        }

        // FNV能快速hash大量数据并保持较小的冲突率，它的高度分散使它适用于hash一些非常相近的字符串，比如URL，hostname，filename，IP等
        private static Long FNVHash(String key) {
            final int p = 16777619;
            long hash = 2166136261L;
            for (int idx = 0, num = key.length(); idx < num; ++idx) {
                hash = (hash ^ key.charAt(idx)) * p;
            }
            hash += hash << 13;
            hash ^= hash >> 7;
            hash += hash << 3;
            hash ^= hash >> 17;
            hash += hash << 5;

            if (hash < 0) {
                hash = Math.abs(hash);
            }
            return hash;
        }

        public Invoker select(RpcInvocation invocation){
            long key = FNVHash(invocation.key());
            // 到 TreeMap 中查找第一个节点值大于或等于当前 hash 的 Invoker
            Map.Entry<Long, Invoker> entry = virtualNodes.tailMap(key, true).firstEntry();
            // 如果 hash 大于 Invoker 在圆环上最大的位置，此时 entry = null，需要将 TreeMap 的头节点赋值给 entry 类似于取模运算中，大于最大值后，会自动回环从0开始
            if (entry == null) {
                return virtualNodes.firstEntry().getValue();
            }

            return entry.getValue();
        }

    }
}
