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

        static int calculateWarmupWeight(int uptime, int warmup, int weight) {
            int ww = (int) ((float) uptime / ((float) warmup / (float) weight));
            return ww < 1 ? 1 : (ww > weight ? weight : ww);
        }
    
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
    
        protected int getWeight(Invoker<?> invoker, Invocation invocation) {
            int weight = invoker.getUrl().getMethodParameter(invocation.getMethodName(), Constants.WEIGHT_KEY, Constants.DEFAULT_WEIGHT);
            if (weight > 0) {
                long timestamp = invoker.getUrl().getParameter(Constants.REMOTE_TIMESTAMP_KEY, 0L);
                if (timestamp > 0L) {
                    int uptime = (int) (System.currentTimeMillis() - timestamp);
                    int warmup = invoker.getUrl().getParameter(Constants.WARMUP_KEY, Constants.DEFAULT_WARMUP);
                    if (uptime > 0 && uptime < warmup) {
                        weight = calculateWarmupWeight(uptime, warmup, weight);
                    }
                }
            }
            return weight;
        }
    
    }

}