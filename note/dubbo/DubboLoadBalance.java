public class DubboLoadBalance{

    @SPI(RandomLoadBalance.NAME)
    public interface LoadBalance {
        @Adaptive("loadbalance")
        <T> Invoker<T> select(List<Invoker<T>> invokers, URL url, Invocation invocation) throws RpcException;
    }

    /**
     * 在Dubbo中，所有的负载均衡都继承自AbstractLoadBalance，它实现了LoadBalance接口，并且并封装了一些公共的逻辑
     */
    public static abstract class AbstractLoadBalance implements LoadBalance {
    
        /**
         * select 方法的逻辑比较简单，首先会检测 invokers 集合的合法性，然后再检测 invokers 集合元素数量。
         * 如果只包含一个 Invoker，直接返回该 Inovker 即可。如果包含多个 Invoker，此时需要通过负载均衡算法选择一个 Invoker。
         */
        public <T> Invoker<T> select(List<Invoker<T>> invokers, URL url, Invocation invocation) {
            if (invokers == null || invokers.size() == 0)
                return null;
            // 如果 invokers 列表中仅有一个 Invoker，直接返回即可，无需进行负载均衡
            if (invokers.size() == 1)
                return invokers.get(0);
            
            // 调用 doSelect 方法进行负载均衡，该方法为抽象方法，由子类实现
            return doSelect(invokers, url, invocation);
        }
    
        protected abstract <T> Invoker<T> doSelect(List<Invoker<T>> invokers, URL url, Invocation invocation);
    
        // 对服务提供者进行权重计算
        // 有一点需要注意的就是保证当服务运行时长小于服务预热时间时，对服务进行降权，避免让服务在启动之初就处于高负载状态。
        // 服务预热是一个优化手段，与此类似的还有 JVM 预热。主要目的是让服务启动后"低功率"运行一段时间，使其效率慢慢提升至最佳状态。
        protected int getWeight(Invoker<?> invoker, Invocation invocation) {
            // 从 url 中获取权重 weight 的配置值，默认的权重为100
            // Constants.WEIGHT_KEY的值为 weight
            int weight = invoker.getUrl().getMethodParameter(invocation.getMethodName(), Constants.WEIGHT_KEY, Constants.DEFAULT_WEIGHT);
            if (weight > 0) {
                // 获取服务提供者启动时间戳
                long timestamp = invoker.getUrl().getParameter(Constants.REMOTE_TIMESTAMP_KEY, 0L);
                if (timestamp > 0L) {
                    // 计算服务提供者的运行时长
                    int uptime = (int) (System.currentTimeMillis() - timestamp);
                    // 获取服务预热时间，默认为10分钟
                    int warmup = invoker.getUrl().getParameter(Constants.WARMUP_KEY, Constants.DEFAULT_WARMUP);
                    // 如果服务运行时间小于预热时间，则重新计算服务权重，即降权
                    if (uptime > 0 && uptime < warmup) {
                        weight = calculateWarmupWeight(uptime, warmup, weight);
                    }
                }
            }
            return weight;
        }

        static int calculateWarmupWeight(int uptime, int warmup, int weight) {
            // 计算权重，下面代码逻辑上形似于 (uptime / warmup) * weight。随着服务运行时间 uptime 增大，权重计算值 ww 会慢慢接近配置值 weight
            int ww = (int) ((float) uptime / ((float) warmup / (float) weight));
            return ww < 1 ? 1 : (ww > weight ? weight : ww);
        }
    
    }

    /**
     * RandomLoadBalance 是加权随机算法的具体实现，它的算法思想很简单。假设我们有一组服务器 servers = [A, B, C]，他们对应的权重为 weights = [5, 3, 2]，
     * 权重总和为10。现在把这些权重值平铺在一维坐标值上，[0, 5) 区间属于服务器 A，[5, 8) 区间属于服务器 B，[8, 10) 区间属于服务器 C。接下来通过随机数生
     * 成器生成一个范围在 [0, 10) 之间的随机数，然后计算这个随机数会落到哪个区间上。比如数字3会落到服务器 A 对应的区间上，此时返回服务器 A 即可。
     * 权重越大的机器，在坐标轴上对应的区间范围就越大，因此随机数生成器生成的数字就会有更大的概率落到此区间内。只要随机数生成器产生的随机数分布性很好，
     * 在经过多次选择后，每个服务器被选中的次数比例接近其权重比例。
     */
    public class RandomLoadBalance extends AbstractLoadBalance {

        public static final String NAME = "random";
    
        private final Random random = new Random();
    
        protected <T> Invoker<T> doSelect(List<Invoker<T>> invokers, URL url, Invocation invocation) {
            int length = invokers.size(); // Number of invokers
            int totalWeight = 0; // The sum of weights
            boolean sameWeight = true; // Every invoker has the same weight?

            // 下面这个循环有两个作用，第一是计算总权重 totalWeight；第二是检测每个服务提供者的权重是否相同
            for (int i = 0; i < length; i++) {
                int weight = getWeight(invokers.get(i), invocation);
                // 累加权重
                totalWeight += weight; // Sum
                // 检测当前服务提供者的权重与上一个服务提供者的权重是否相同，不相同的话，则将 sameWeight 置为 false。
                if (sameWeight && i > 0
                        && weight != getWeight(invokers.get(i - 1), invocation)) {
                    sameWeight = false;
                }
            }

            // 下面的 if 分支主要用于获取随机数，并计算随机数落在哪个区间上
            if (totalWeight > 0 && !sameWeight) {
                // If (not every invoker has the same weight & at least one invoker's weight>0), select randomly based on totalWeight.
                // 随机获取一个 [0, totalWeight) 区间内的数字
                int offset = random.nextInt(totalWeight);
                // Return a invoker based on the random value.
                // 循环让 offset 数减去服务提供者权重值，当 offset 小于0时，返回相应的 Invoker。
                // 举例说明一下，我们有 servers = [A, B, C]，weights = [5, 3, 2]，offset = 7。
                // 第一次循环，offset - 5 = 2 > 0，即 offset > 5，
                // 表明其不会落在服务器 A 对应的区间上。
                // 第二次循环，offset - 3 = -1 < 0，即 5 < offset < 8，
                // 表明其会落在服务器 B 对应的区间上
                for (int i = 0; i < length; i++) {
                    // 让随机值 offset 减去权重值
                    offset -= getWeight(invokers.get(i), invocation);
                    if (offset < 0) {
                        // 返回相应的 Invoker
                        return invokers.get(i);
                    }
                }
            }

            // 如果所有服务提供者权重值相同，此时直接随机返回一个即可
            // If all invokers have the same weight value or totalWeight=0, return evenly.
            return invokers.get(random.nextInt(length));
        }
    
    }

    /**
     * LeastActiveLoadBalance 翻译过来是最小活跃数负载均衡。活跃调用数越小，表明该服务提供者效率越高，单位时间内可处理更多的请求。
     * 此时应优先将请求分配给该服务提供者。在具体实现中，每个服务提供者对应一个活跃数 active。初始情况下，所有服务提供者活跃数均为0。
     * 每收到一个请求，活跃数加1，完成请求后则将活跃数减1。在服务运行一段时间后，性能好的服务提供者处理请求的速度更快，因此活跃数下降的也越快，
     * 此时这样的服务提供者能够优先获取到新的服务请求、这就是最小活跃数负载均衡算法的基本思想。除了最小活跃数，LeastActiveLoadBalance 
     * 在实现上还引入了权重值。所以准确的来说，LeastActiveLoadBalance 是基于加权最小活跃数算法实现的
     */
    public class LeastActiveLoadBalance extends AbstractLoadBalance {

        public static final String NAME = "leastactive";
    
        private final Random random = new Random();
    
        // Dubbo 2.6.0 版本
        protected <T> Invoker<T> doSelect(List<Invoker<T>> invokers, URL url, Invocation invocation) {
            int length = invokers.size(); // Number of invokers
            // 最小的活跃数
            int leastActive = -1; // The least active value of all invokers
            // 具有相同“最小活跃数”的服务者提供者（以下用 Invoker 代称）数量
            // The number of invokers having the same least active value (leastActive)
            int leastCount = 0; 
            // leastIndexs 用于记录具有相同"最小活跃数"的 Invoker 在 invokers 列表中的下标信息
            int[] leastIndexs = new int[length]; 
            int totalWeight = 0; // The sum of weights
            // 第一个最小活跃数的 Invoker 权重值，用于与其他具有相同最小活跃数的 Invoker 的权重进行对比，
            // 以检测是否“所有具有相同最小活跃数的 Invoker 的权重”均相等
            int firstWeight = 0; // Initial value, used for comparision
            boolean sameWeight = true; // Every invoker has the same weight value?
            for (int i = 0; i < length; i++) {
                Invoker<T> invoker = invokers.get(i);
                int active = RpcStatus.getStatus(invoker.getUrl(), invocation.getMethodName()).getActive(); // Active number
                int weight = invoker.getUrl().getMethodParameter(invocation.getMethodName(), Constants.WEIGHT_KEY, Constants.DEFAULT_WEIGHT); // Weight
                if (leastActive == -1 || active < leastActive) { // Restart, when find a invoker having smaller least active value.
                    leastActive = active; // Record the current least active value
                    leastCount = 1; // Reset leastCount, count again based on current leastCount
                    leastIndexs[0] = i; // Reset
                    totalWeight = weight; // Reset
                    firstWeight = weight; // Record the weight the first invoker
                    sameWeight = true; // Reset, every invoker has the same weight value?
                } else if (active == leastActive) { // If current invoker's active value equals with leaseActive, then accumulating.
                    leastIndexs[leastCount++] = i; // Record index number of this invoker
                    totalWeight += weight; // Add this invoker's weight to totalWeight.
                    // If every invoker has the same weight?
                    if (sameWeight && i > 0
                            && weight != firstWeight) {
                        sameWeight = false;
                    }
                }
            }
            // assert(leastCount > 0)
            if (leastCount == 1) {
                // If we got exactly one invoker having the least active value, return this invoker directly.
                return invokers.get(leastIndexs[0]);
            }
            if (!sameWeight && totalWeight > 0) {
                // If (not every invoker has the same weight & at least one invoker's weight>0), select randomly based on totalWeight.
                int offsetWeight = random.nextInt(totalWeight);
                // Return a invoker based on the random value.
                for (int i = 0; i < leastCount; i++) {
                    int leastIndex = leastIndexs[i];
                    offsetWeight -= getWeight(invokers.get(leastIndex), invocation);
                    if (offsetWeight <= 0)
                        return invokers.get(leastIndex);
                }
            }
            // If all invokers have the same weight value or totalWeight=0, return evenly.
            return invokers.get(leastIndexs[random.nextInt(leastCount)]);
        }

        // Dubbo 2.6.5 版本
        @Override
        protected <T> Invoker<T> doSelect(List<Invoker<T>> invokers, URL url, Invocation invocation) {
            int length = invokers.size(); // Number of invokers
            int leastActive = -1; // The least active value of all invokers
            int leastCount = 0; // The number of invokers having the same least active value (leastActive)
            int[] leastIndexs = new int[length]; // The index of invokers having the same least active value
                                                 // (leastActive)
            int totalWeight = 0; // The sum of with warmup weights
            int firstWeight = 0; // Initial value, used for comparision
            boolean sameWeight = true; // Every invoker has the same weight value?
            for (int i = 0; i < length; i++) {
                Invoker<T> invoker = invokers.get(i);
                int active = RpcStatus.getStatus(invoker.getUrl(), invocation.getMethodName()).getActive(); // Active
                                                                                                            // number
                int afterWarmup = getWeight(invoker, invocation); // Weight
                if (leastActive == -1 || active < leastActive) { // Restart, when find a invoker having smaller least
                                                                 // active value.
                    leastActive = active; // Record the current least active value
                    leastCount = 1; // Reset leastCount, count again based on current leastCount
                    leastIndexs[0] = i; // Reset
                    totalWeight = afterWarmup; // Reset
                    firstWeight = afterWarmup; // Record the weight the first invoker
                    sameWeight = true; // Reset, every invoker has the same weight value?
                } else if (active == leastActive) { // If current invoker's active value equals with leaseActive, then
                                                    // accumulating.
                    leastIndexs[leastCount++] = i; // Record index number of this invoker
                    totalWeight += afterWarmup; // Add this invoker's weight to totalWeight.
                    // If every invoker has the same weight?
                    if (sameWeight && i > 0 && afterWarmup != firstWeight) {
                        sameWeight = false;
                    }
                }
            }
            // assert(leastCount > 0)
            if (leastCount == 1) {
                // If we got exactly one invoker having the least active value, return this
                // invoker directly.
                return invokers.get(leastIndexs[0]);
            }
            if (!sameWeight && totalWeight > 0) {
                // If (not every invoker has the same weight & at least one invoker's weight>0),
                // select randomly based on totalWeight.
                int offsetWeight = random.nextInt(totalWeight) + 1;
                // Return a invoker based on the random value.
                for (int i = 0; i < leastCount; i++) {
                    int leastIndex = leastIndexs[i];
                    offsetWeight -= getWeight(invokers.get(leastIndex), invocation);
                    if (offsetWeight <= 0)
                        return invokers.get(leastIndex);
                }
            }
            // If all invokers have the same weight value or totalWeight=0, return evenly.
            return invokers.get(leastIndexs[random.nextInt(leastCount)]);
        }
    }

}