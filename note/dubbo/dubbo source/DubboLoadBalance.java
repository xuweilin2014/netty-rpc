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
     * 在实现上还引入了权重值。所以准确的来说，LeastActiveLoadBalance 是基于加权最小活跃数算法实现的。
     * 
     * 下面代码的逻辑如下：遍历 Invokers 列表，计算这些 Invoker 的权重之和 totalWeight，并且找到具有最小活跃数的 Invoker。如果有多个 Invoker 具有相同的最小活跃数，
     * 那么用数组把他们的下标保存起来。最后如果只有一个 Invoker 具有最小活跃数，那么就直接返回这个 Invoker；如果有多个 Invoker 具有最小活跃数，并且他们的权重值
     * 相同，那么随机选择其中一个 Invoker 返回；如果有多个 Invoker 具有最小活跃数，并且他们的权重值不同，那么此时的处理方式和 RandomLoadBalance 一致
     * 
     */
    public class LeastActiveLoadBalance extends AbstractLoadBalance {

        public static final String NAME = "leastactive";
    
        private final Random random = new Random();
    
        // Dubbo 2.6.0 版本
        protected <T> Invoker<T> doSelect(List<Invoker<T>> invokers, URL url, Invocation invocation) {

            int length = invokers.size(); // Number of invokers
            // 最小的活跃数
            int leastActive = -1; 
            // 具有相同"最小活跃数"的服务者提供者（以下用 Invoker 代称）数量
            int leastCount = 0; 
            // leastIndexs 用于记录具有相同"最小活跃数"的 Invoker 在 invokers 列表中的下标信息
            int[] leastIndexs = new int[length]; 
            int totalWeight = 0; 
            // 第一个最小活跃数的 Invoker 权重值，用于与其他具有相同最小活跃数的 Invoker 的权重进行对比，以检测是否所有 "具有相同最小活跃数的 Invoker " 的权重均相等
            int firstWeight = 0; 
            boolean sameWeight = true; 

            for (int i = 0; i < length; i++) {
                Invoker<T> invoker = invokers.get(i);
                // 获取 Invoker 对应的活跃数 
                int active = RpcStatus.getStatus(invoker.getUrl(), invocation.getMethodName()).getActive(); 
                // 获取 Invoker 对应的权重 ⭐️
                int weight = invoker.getUrl().getMethodParameter(invocation.getMethodName(), Constants.WEIGHT_KEY, Constants.DEFAULT_WEIGHT); 

                // 如果发现了更小的活跃数，重新开始
                if (leastActive == -1 || active < leastActive) { 
                    // 使用当前活跃数 active 更新最小活跃数 leastActive
                    leastActive = active; 
                    // 更新 leastCount 为 1
                    leastCount = 1; 
                    // 记录当前下标值到 leastIndexs 中
                    leastIndexs[0] = i; 
                    totalWeight = weight;
                    firstWeight = weight; 
                    sameWeight = true; 

                // 当前 Invoker 的活跃数 active 与最小活跃数 leastActive 相同 
                } else if (active == leastActive) { 
                    // 在 leastIndexs 中记录下当前 Invoker 在 invokers 集合中的下标
                    leastIndexs[leastCount++] = i; 
                    totalWeight += weight; 
                    // 检测当前 Invoker 的权重与 firstWeight 是否相等，不相等则将 sameWeight 置为 false
                    if (sameWeight && i > 0
                            && weight != firstWeight) {
                        sameWeight = false;
                    }
                }
            }

            // 当只有一个 Invoker 具有最小活跃数，此时直接返回该 Invoker 即可
            if (leastCount == 1) {
                return invokers.get(leastIndexs[0]);
            }

            // 有多个 Invoker 具有相同的最小活跃数，但它们之间的权重不同
            if (!sameWeight && totalWeight > 0) {
                // 随机生成一个 [0, totalWeight) 之间的数字
                int offsetWeight = random.nextInt(totalWeight);
                // 循环让随机数减去具有最小活跃数的 Invoker 的权重值，当 offset 小于等于0时，返回相应的 Invoker
                for (int i = 0; i < leastCount; i++) {
                    int leastIndex = leastIndexs[i];
                    // 获取权重值，并让随机数减去权重值 - ⭐️
                    offsetWeight -= getWeight(invokers.get(leastIndex), invocation);
                    if (offsetWeight <= 0)
                        return invokers.get(leastIndex);
                }
            }

            // 如果权重相同或权重为0时，随机返回一个 Invoker
            return invokers.get(leastIndexs[random.nextInt(leastCount)]);
        }

        /**
         * 
         * Dubbo 2.6.0 版本有两个小问题：
         * 
         * 1.第一个问题出在服务预热阶段，第一行代码直接从 url 中取权重值，未被降权过。第二行代码获取到的是经过降权后的权重。第一行代码获取到的权重值最终会被累加到权重总和 totalWeight 中，
         * 这个时候会导致一个问题。offsetWeight 是一个在 [0, totalWeight) 范围内的随机数，而它所减去的是经过降权的权重。很有可能在经过 leastCount 次运算后，offsetWeight 仍然是大于0的，
         * 导致无法选中 Invoker。
         * 
         * 2.第二个问题出在当一组 Invoker 具有相同的最小活跃数，且其中一个 Invoker 的权重值为1，此时这个 Invoker 无法被选中。举例说明，假设有一组 Invoker 的权重为 5、2、1，offsetWeight 最大值为 7。
         * 假设 offsetWeight = 7，你会发现，当 for 循环进行第二次遍历后 offsetWeight = 7 - 5 - 2 = 0，提前返回了。此时，此时权重为1的 Invoker 就没有机会被选中了。
         * 
         * 上面这两个问题在 Dubbo 2.6.5 版本中被修复
         */

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

    /**
     * 基本思想：算法提出之初是用于大规模缓存系统的负载均衡。它的工作过程是这样的，首先根据 ip 或者其他的信息为缓存节点生成一个 hash，
     * 并将这个 hash 投射到 [0, 232 - 1] 的圆环上。当有查询或写入请求时，则为缓存项的 key 生成一个 hash 值。然后查找第一个大于或等于该 hash 值的缓存节点，并到这个节点中查询或写入缓存项。如果当前节点挂了，
     * 则在下一次查询或写入缓存时，为缓存项查找另一个大于其 hash 值的缓存节点即可。大致效果如下图所示，每个缓存节点在圆环上占据一个位置。如果缓存项的 key 的 hash 值小于缓存节点 hash 值，
     * 则到该缓存节点中存储或读取缓存项。比如下面绿色点对应的缓存项将会被存储到 cache-2 节点中。由于 cache-3 挂了，原本应该存到该节点中的缓存项最终会存储到 cache-4 节点中。
     * 
     * 实现逻辑：Dubbo 实现的负载均衡是客户端负载均衡。初始化的时候根据用户配置的 hash.nodes 创建指定数目的虚拟节点，默认为 160 个。获取配置的用作Hash映射的参数的索引，默认对第一个参数进行 hash 运算。
     * 需要特别说明的是，客户端发送的请求的 hash 值只和参数值有关，具有相同参数值的请求，在虚拟节点不发生变化的情况下，会传递给相同的 Invoker 进行处理。在 Dubbo 实现的一致性 hash 算法中，每一个节点 invoker
     * 的 hash 值就代表一个虚拟节点
     */
    public class ConsistentHashLoadBalance extends AbstractLoadBalance {

        private final ConcurrentMap<String, ConsistentHashSelector<?>> selectors = new ConcurrentHashMap<String, ConsistentHashSelector<?>>();
    
        /**
         * doSelect主要作了一些前置逻辑，比如检查invokers列表是否发生了变动过，以及创建ConsistentHashSelector对象。接下来，就会调用
         * ConsistentHashSelector对象的select方法，进行负载均衡逻辑
         */
        @SuppressWarnings("unchecked")
        @Override
        protected <T> Invoker<T> doSelect(List<Invoker<T>> invokers, URL url, Invocation invocation) {
            // key的值一般为：com.dubbo.simple.common.DemoService.sayHello
            String key = invokers.get(0).getUrl().getServiceKey() + "." + invocation.getMethodName();

            // 获取 invokers 列表的原始 hash 值
            int identityHashCode = System.identityHashCode(invokers);
            ConsistentHashSelector<T> selector = (ConsistentHashSelector<T>) selectors.get(key);

            // 如果 invokers 集合是一个新的 List 对象，这意味着服务提供者的数量发生了变化，可能新增也可能减少了。此时 selector.identityHashCode != identityHashCode 条件成立
            if (selector == null || selector.identityHashCode != identityHashCode) {
                // 创建新的 ConsistentHashSelector
                selectors.put(key, new ConsistentHashSelector<T>(invokers, invocation.getMethodName(), identityHashCode));
                selector = (ConsistentHashSelector<T>) selectors.get(key);
            }

            // 调用 ConsistentHashSelector 的 select 方法选择 Invoker
            return selector.select(invocation);
        }
    
        private static final class ConsistentHashSelector<T> {
    
            /**
             * 存储Hash值与节点映射关系的TreeMap
             */
            private final TreeMap<Long, Invoker<T>> virtualInvokers;
    
            /**
             * 节点数目
             */
            private final int replicaNumber;
    
            /**
             * 用来识别Invoker列表是否发生变更的Hash码
             */
            private final int identityHashCode;
    
            /**
             * 请求中用来作Hash映射的参数的索引
             */
            private final int[] argumentIndex;
    
            /**
             * 为每个Invoker都创建replicaNumber个节点，Hash值与Invoker的映射关系即象征着一个节点，这个关系存储在TreeMap中
             */
            ConsistentHashSelector(List<Invoker<T>> invokers, String methodName, int identityHashCode) {
                this.virtualInvokers = new TreeMap<Long, Invoker<T>>();
                this.identityHashCode = identityHashCode;
                URL url = invokers.get(0).getUrl();
                // 获取虚拟节点数目，默认为 160
                this.replicaNumber = url.getMethodParameter(methodName, "hash.nodes", 160);
                // 获取配置的用作Hash映射的参数的索引，默认对第一个参数进行 hash 运算
                String[] index = Constants.COMMA_SPLIT_PATTERN.split(url.getMethodParameter(methodName, "hash.arguments", "0"));
                argumentIndex = new int[index.length];
                for (int i = 0; i < index.length; i++) {
                    argumentIndex[i] = Integer.parseInt(index[i]);
                }

                /**
                 * 以replicaNumber取默认值160为例，假设当前遍历到的Invoker地址为127.0.0.1:20880，它会依次获得"127.0.0.1:208800"、"127.0.0.1:208801"、......、"127.0.0.1:2088040" 的md5摘要，
                 * 在每次获得摘要之后，还会对该摘要进行四次数位级别的散列。大致可以猜到其目的应该是为了加强散列效果。
                 */

                // 遍历所有 invoker 对象
                for (Invoker<T> invoker : invokers) {
                    // 获取 provider 的 ip + port
                    String address = invoker.getUrl().getAddress();
                    for (int i = 0; i < replicaNumber / 4; i++) {
                        // 对 address + i 进行 md5 运算，得到一个长度为16的字节数组
                        byte[] digest = md5(address + i);
                        // 对 digest 部分字节进行4次 hash 运算，得到四个不同的 long 型正整数
                        for (int h = 0; h < 4; h++) {
                            // h = 0 时，取 digest 中下标为 0 ~ 3 的4个字节进行位运算
                            // h = 1 时，取 digest 中下标为 4 ~ 7 的4个字节进行位运算
                            // h = 2 时，取 digest 中下标为 8 ~ 11 的4个字节进行位运算
                            // h = 3 时，取 digest 中下标为 12 ~ 15 的4个字节进行位运算
                            long m = hash(digest, h);
                            // 将 hash 到 invoker 的映射关系存储到 virtualInvokers 中，virtualInvokers 需要提供高效的查询操作，因此选用 TreeMap 作为存储结构
                            virtualInvokers.put(m, invoker);
                        }
                    }
                }
            }
    
            public Invoker<T> select(Invocation invocation) {
                // 根据 invocation 的【参数值】来确定 key，默认使用第一个参数来做 hash 计算。
                // argumentIndex 是在初始化 Selector 的时候一起赋值的，代表着需要用哪几个请求参数作 Hash 映射获取 Invoker。
                // 比如：有方法 methodA(Integer a, Integer b, Integer c)，如果 argumentIndex 的值为{0,2}，那么即用a和c拼接的字符串来计算 Hash 值。
                String key = toKey(invocation.getArguments());
                // 获取【参数值】的 md5 编码
                byte[] digest = md5(key);
                return selectForKey(hash(digest, 0));
            }
    
            // 根据参数索引获取参数，并将所有参数拼接成字符串
            private String toKey(Object[] args) {
                StringBuilder buf = new StringBuilder();
                for (int i : argumentIndex) {
                    if (i >= 0 && i < args.length) {
                        buf.append(args[i]);
                    }
                }
                return buf.toString();
            }
    
            private Invoker<T> selectForKey(long hash) {
                // 到 TreeMap 中查找第一个节点值大于或等于当前 hash 的 Invoker
                Map.Entry<Long, Invoker<T>> entry = virtualInvokers.tailMap(hash, true).firstEntry();
                // 如果 hash 大于 Invoker 在圆环上最大的位置，此时 entry = null，需要将 TreeMap 的头节点赋值给 entry
                // 类似于取模运算中，大于最大值后，会自动回环从0开始
                if (entry == null) {
                    entry = virtualInvokers.firstEntry();
                }

                // 返回 Invoker
                return entry.getValue();
            }
    
        }
    
    }

    /**
     * 
     * 加权轮询的基本思想：从最简单的轮询开始讲起，所谓轮询是指将请求轮流分配给每台服务器。举个例子，我们有三台服务器 A、B、C。我们将第一个请求分配给服务器 A，第二个请求分配给服务器 B，第三个请求分配给服务器 C，
     * 第四个请求再次分配给服务器 A。这个过程就叫做轮询。轮询是一种无状态负载均衡算法，实现简单，适用于每台服务器性能相近的场景下。但现实情况下，我们并不能保证每台服务器性能均相近。如果我们将等量的请求分配给性能较差的服务器，
     * 这显然是不合理的。因此，这个时候我们需要对轮询过程进行加权，以调控每台服务器的负载。经过加权后，每台服务器能够得到的请求数比例，接近或等于他们的权重比。比如服务器 A、B、C 权重比为 5:2:1。那么在8次请求中，
     * 服务器 A 将收到其中的5次请求，服务器 B 会收到其中的2次请求，服务器 C 则收到其中的1次请求。
     * 
     * 下面我们举例进行说明，假设我们有三台服务器 servers = [A, B, C]，对应的权重为 weights = [2, 5, 1]。接下来对上面的逻辑进行简单的模拟。
     * 
     * mod = 0：满足条件，此时直接返回服务器 A
     * 
     * mod = 1：需要进行一次递减操作才能满足条件，此时返回服务器 B
     * 
     * mod = 2：需要进行两次递减操作才能满足条件，此时返回服务器 C
     * 
     * mod = 3：需要进行三次递减操作才能满足条件，经过递减后，服务器权重为 [1, 4, 0]，此时返回服务器 A
     * 
     * mod = 4：需要进行四次递减操作才能满足条件，经过递减后，服务器权重为 [0, 4, 0]，此时返回服务器 B
     * 
     * mod = 5：需要进行五次递减操作才能满足条件，经过递减后，服务器权重为 [0, 3, 0]，此时返回服务器 B
     * 
     * mod = 6：需要进行六次递减操作才能满足条件，经过递减后，服务器权重为 [0, 2, 0]，此时返回服务器 B
     * 
     * mod = 7：需要进行七次递减操作才能满足条件，经过递减后，服务器权重为 [0, 1, 0]，此时返回服务器 B
     * 
     * 经过8次调用后，我们得到的负载均衡结果为 [A, B, C, A, B, B, B, B]，次数比 A:B:C = 2:5:1，等于权重比。当 sequence = 8 时，mod = 0，此时重头再来。
     * 从上面的模拟过程可以看出，当 mod >= 3 后，服务器 C 就不会被选中了，因为它的权重被减为0了。当 mod >= 4 后，服务器 A 的权重被减为0，此后 A 就不会再被选中。
     * 
     * 综上，dubbo 中 RoundRobinLoadBalance 负载均衡策略的实现逻辑如下：
     * 
     * 将 invokers 集合中每个 invoker 的 weight 累加起来，得到 weightSum，然后将调用次数对 weightSum 取模，得到 mod。
     * 然后遍历 invokers 集合，对于遍历到的每一个invoker：
     * 1.如果 mod 为0，则直接返回这个 invoker
     * 2.如果 mod 不为0
     *      i.如果其 weight 不为0，且 mod 不为0，那么就把 mod 和这个 weight 的值减一，接着遍历下一个 invoker
     *      ii.如果其 weight 已经被减为0了，那么就不做操作，直接遍历下一个 invoker
     * 
     */
    public class RoundRobinLoadBalance extends AbstractLoadBalance {

        public static final String NAME = "roundrobin";
    
        private final ConcurrentMap<String, AtomicPositiveInteger> sequences = new ConcurrentHashMap<String, AtomicPositiveInteger>();
    
        @Override
        protected <T> Invoker<T> doSelect(List<Invoker<T>> invokers, URL url, Invocation invocation) {
            // key = 全限定类名 + "." + 方法名，比如 com.xxx.DemoService.sayHello
            String key = invokers.get(0).getUrl().getServiceKey() + "." + invocation.getMethodName();
            int length = invokers.size(); 
            // 最大权重
            int maxWeight = 0;
            // 最小权重
            int minWeight = Integer.MAX_VALUE; // The minimum weight
            final LinkedHashMap<Invoker<T>, IntegerWrapper> invokerToWeightMap = new LinkedHashMap<Invoker<T>, IntegerWrapper>();
            // 权重总和
            int weightSum = 0;

            // 下面这个循环主要用于查找最大和最小权重，计算权重总和等
            for (int i = 0; i < length; i++) {
                int weight = getWeight(invokers.get(i), invocation);
                // 获取最大和最小权重
                maxWeight = Math.max(maxWeight, weight); // Choose the maximum weight
                minWeight = Math.min(minWeight, weight); // Choose the minimum weight
                if (weight > 0) {
                    // 将 weight 封装到 IntegerWrapper 中
                    invokerToWeightMap.put(invokers.get(i), new IntegerWrapper(weight));
                    // 累加权重
                    weightSum += weight;
                }
            }

            // 查找 key 对应的 AtomicPositiveInteger 实例，为空则创建。这里可以把 AtomicPositiveInteger 看成一个黑盒，大家只要知道
            // AtomicPositiveInteger 用于记录服务的调用编号即可。
            AtomicPositiveInteger sequence = sequences.get(key);
            if (sequence == null) {
                sequences.putIfAbsent(key, new AtomicPositiveInteger());
                sequence = sequences.get(key);
            }

            // 获取当前的调用编号
            int currentSequence = sequence.getAndIncrement();
            // 如果最小权重小于最大权重，表明服务提供者之间的权重是不相等的
            if (maxWeight > 0 && minWeight < maxWeight) {
                // 使用调用编号对权重总和进行取余操作
                int mod = currentSequence % weightSum;

                // 进行 maxWeight 次遍历
                // 个人觉得进行 maxWeight 次遍历的主要原因是： mod 的取值范围为 [0, weightSum - 1]，而 maxWeight * invokerLen >= weightSum，那么 maxWeight * invokerLen > weightSum - 1
                // 所以这样肯定可以最终选择一个 invoker
                for (int i = 0; i < maxWeight; i++) {
                    // 遍历 invokerToWeightMap
                    for (Map.Entry<Invoker<T>, IntegerWrapper> each : invokerToWeightMap.entrySet()) {
                        // 获取 Invoker
                        final Invoker<T> k = each.getKey();
                        // 获取权重包装类 IntegerWrapper
                        final IntegerWrapper v = each.getValue();

                        // 如果 mod = 0，且权重大于0，此时返回相应的 Invoker
                        if (mod == 0 && v.getValue() > 0) {
                            return k;
                        }

                        // mod != 0，且权重大于0，此时对权重和 mod 分别进行自减操作
                        if (v.getValue() > 0) {
                            v.decrement();
                            mod--;
                        }
                    }
                }
            }

            // 服务提供者之间的权重相等，此时通过轮询选择 Invoker
            return invokers.get(currentSequence % length);
        }
    
        private static final class IntegerWrapper {
            private int value;
    
            public IntegerWrapper(int value) {
                this.value = value;
            }
    
            public int getValue() {
                return value;
            }
    
            public void setValue(int value) {
                this.value = value;
            }
    
            public void decrement() {
                this.value--;
            }
        }
    
    }

    

}