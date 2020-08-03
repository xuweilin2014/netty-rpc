public class DubboCluster{

    /**
     * 在对集群相关代码进行分析之前，这里有必要先来介绍一下集群容错的所有组件。包含 Cluster、Cluster Invoker、Directory、Router 和 LoadBalance 等。
     * 集群工作过程可分为两个阶段：
     * 第一个阶段是在服务消费者初始化期间，集群 Cluster 实现类为服务消费者创建 Cluster Invoker 实例。
     * 
     * 第二个阶段是在服务消费者进行远程调用时。以 FailoverClusterInvoker 为例，该类型 Cluster Invoker 首先会调用 Directory 的 list 方法列举 Invoker 列表（可将 Invoker 简单理解为服务提供者）。Directory 的用途是保存 Invoker，
     * 可简单类比为 List<Invoker>。其实现类 RegistryDirectory 是一个动态服务目录，可感知注册中心配置的变化，它所持有的 Invoker 列表会随着注册中心内容的变化而变化。每次变化后，
     * RegistryDirectory 会动态增删 Invoker，并调用 Router 的 route 方法进行路由，过滤掉不符合路由规则的 Invoker。当 FailoverClusterInvoker 拿到 Directory 返回的 Invoker 列表后，
     * 它会通过 LoadBalance 从 Invoker 列表中选择一个 Invoker。最后 FailoverClusterInvoker 会将参数传给 LoadBalance 选择出的 Invoker 实例的 invoke 方法，进行真正的远程调用。
     * 
     * 以上就是集群工作的整个流程，这里并没介绍集群是如何容错的。Dubbo 主要提供了这样几种容错方式：
     * Failover Cluster - 失败自动切换
     * Failfast Cluster - 快速失败
     * Failsafe Cluster - 失败安全
     * Failback Cluster - 失败自动恢复
     * Forking Cluster - 并行调用多个服务提供者
     */

     /**
      * Cluster Invoker和Cluster是不同的，Cluster 是接口，而 Cluster Invoker 是一种 Invoker。服务提供者的选择逻辑，以及远程调用失败后的的处理逻辑均是封装在 Cluster Invoker 中。
      * 而Cluster 的用途比较简单，仅用于生成 Cluster Invoker。比如下面的 FailoverCluster 仅仅用于生成 FailoverClusterInvoker对象。
      */
    public class FailoverCluster implements Cluster {

        public final static String NAME = "failover";
    
        public <T> Invoker<T> join(Directory<T> directory) throws RpcException {
            //创建并返回FailoverClusterInvoker对象
            return new FailoverClusterInvoker<T>(directory);
        }
    
    }

    public class AvailableCluster implements Cluster {

        public static final String NAME = "available";
    
        public <T> Invoker<T> join(Directory<T> directory) throws RpcException {
    
            return new AbstractClusterInvoker<T>(directory) {
                public Result doInvoke(Invocation invocation, List<Invoker<T>> invokers, LoadBalance loadbalance) throws RpcException {
                    for (Invoker<T> invoker : invokers) {
                        if (invoker.isAvailable()) {
                            return invoker.invoke(invocation);
                        }
                    }
                    throw new RpcException("No provider available in " + invokers);
                }
            };
    
        }
    
    }

    /**
     * Cluster invoker 的 invoke 过程大致分为两个过程：
     * 1.通过 url 参数获取到用户所使用的负载均衡策略
     * 2.获取到 directory 中所保存的 invoker 对象集合
     * 3.根据用户所选择的负载均衡策略从 invoker 集合中选择一个 Invoker
     * 4.对 invoker 进行 invoke 调用
     * 5.如果调用成功，则直接返回结果
     * 6.如果调用失败，则根据不同的容错策略，在不同的子类中（其实从实现上来讲，2-6，都是在不同的子类中进行，但是2-5在不同的子类中步骤完全一样）进行不同的处理：
     *      1) FailoverClusterInvoker：失败自动切换，也就是一个 invoker 调用失败之后，调用 invoker 集合中的下一个 invoker
     *      2) FailfastClusterInvoker：快速失败，也就是调用失败之后，直接抛出异常
     *      3) FailsafeClusterInvoker：失败安全，调用失败之后，仅仅会使用日志打印出失败的信息，然后返回一个空结果
     *      4) FailbackClusterInvoker：失败自动恢复，也就是 Invoker 调用失败之后，会再定时重新发送
     *      5) ForkingClusterInvoker：并行调用多个服务提供者，只要有一个服务提供者 Invoker 调用成功返回了结果，即使其余的调用全部失败，也看做调用成功
     */
    public abstract class AbstractClusterInvoker<T> implements Invoker<T>{

        public Result invoke(final Invocation invocation) throws RpcException {

            checkWhetherDestroyed();
    
            LoadBalance loadbalance;
    
            // 列举 Invoker
            List<Invoker<T>> invokers = list(invocation);
            if (invokers != null && invokers.size() > 0) {
                // 加载 LoadBalance，默认的负载均衡策略为 random
                loadbalance = ExtensionLoader.getExtensionLoader(LoadBalance.class).getExtension(invokers.get(0).getUrl()
                        .getMethodParameter(invocation.getMethodName(), Constants.LOADBALANCE_KEY, Constants.DEFAULT_LOADBALANCE));
            } else {
                loadbalance = ExtensionLoader.getExtensionLoader(LoadBalance.class).getExtension(Constants.DEFAULT_LOADBALANCE);
            }
            RpcUtils.attachInvocationIdIfAsync(getUrl(), invocation);
            // 调用模板方法 doInvoke 进行后续操作
            return doInvoke(invocation, invokers, loadbalance);
        }

        //抽象方法，由AbstractClusterInvoker的子类实现
        protected abstract Result doInvoke(Invocation invocation, List<Invoker<T>> invokers,
                                       LoadBalance loadbalance) throws RpcException;

        
        protected List<Invoker<T>> list(Invocation invocation) throws RpcException {
            // 调用 Directory 的 list 方法列举 Invoker
            List<Invoker<T>> invokers = directory.list(invocation);
            return invokers;
        }


        protected Invoker<T> select(LoadBalance loadbalance, Invocation invocation, List<Invoker<T>> invokers, List<Invoker<T>> selected) throws RpcException {
            if (invokers == null || invokers.size() == 0)
                return null;
            //获取调用方法名
            String methodName = invocation == null ? "" : invocation.getMethodName();
            // 获取 sticky 配置，sticky 表示粘滞连接。所谓粘滞连接是指让服务消费者尽可能的调用同一个服务提供者，除非该提供者挂了再进行切换
            // CLUSTER_STICKY_KEY为sticky，DEFAULT_CLUSTER_STICKY为false
            boolean sticky = invokers.get(0).getUrl().getMethodParameter(methodName, Constants.CLUSTER_STICKY_KEY, Constants.DEFAULT_CLUSTER_STICKY);
            {
                // 检测 invokers 列表是否包含 stickyInvoker，如果不包含，说明 stickyInvoker 代表的服务提供者挂了，此时需要将其置空
                // 这里的 invokers 列表可以看作是存活着的服务提供者列表，如果这个列表不包含 stickyInvoker，那么自然而然地认为 stickyInvoker 挂了
                if (stickyInvoker != null && !invokers.contains(stickyInvoker)) {
                    stickyInvoker = null;
                }
                
                // stickyInvoker不为null，并且没在已选列表中，返回上次的服务提供者stickyInvoker，但之前强制校验可达性，如果没有开启强制校验，也不能返回 stickyInvoker。
                // selected 如果包含的 stickyInvoker 的话，说明 stickyInvoker 在此之前没有成功提供服务（但其仍然处于存活状态）。此时我们认为这个服务不可靠，
                // 不应该在重试期间内再次被调用，因此这个时候不会返回该 stickyInvoker。如果 selected 不包含 stickyInvoker，此时还需要进行可用性检测，比如检测服务提供者网络连通性等。
                // 当可用性检测通过，才可返回 stickyInvoker。
                if (sticky && stickyInvoker != null && (selected == null || !selected.contains(stickyInvoker))) {
                    if (availablecheck && stickyInvoker.isAvailable()) {
                        return stickyInvoker;
                    }
                }
            }
            // 如果线程走到当前代码处，说明没有开启 sticky 配置，或者前面的 stickyInvoker 为空，或者不可用。此时继续调用 doSelect 选择 Invoker
            Invoker<T> invoker = doselect(loadbalance, invocation, invokers, selected);
    
            // 如果 sticky 为 true，则将负载均衡组件选出的 Invoker 赋值给 stickyInvoker
            if (sticky) {
                stickyInvoker = invoker;
            }
            return invoker;
        }

        /**
         * doselect方法的流程如下：
         * 1.通过负载均衡组件选择invoker
         * 2.如果选出来的 Invoker 不稳定，或不可用，此时需要调用 reselect 方法进行重选。若 reselect 选出来的 Invoker 为空，此时定位 invoker 在 invokers 列表中的位置 index，然后获取 index + 1 处的 invoker，
         */
        private Invoker<T> doselect(LoadBalance loadbalance, Invocation invocation, List<Invoker<T>> invokers, List<Invoker<T>> selected) throws RpcException {
            if (invokers == null || invokers.size() == 0)
                return null;
            if (invokers.size() == 1)
                return invokers.get(0);

            // If we only have two invokers, use round-robin instead.
            if (invokers.size() == 2 && selected != null && selected.size() > 0) {
                return selected.get(0) == invokers.get(0) ? invokers.get(1) : invokers.get(0);
            }

            // 通过负载均衡组件选择 Invoker
            Invoker<T> invoker = loadbalance.select(invokers, getUrl(), invocation);
    
            // 如果 selected 包含负载均衡选择出的 Invoker，或者该 Invoker 无法经过可用性检查（同时 availablecheck 为 true），此时进行重选
            // selected 集合中的 invoker 已经被 invoke 过，但是无法成功提供服务
            if ((selected != null && selected.contains(invoker))
                    || (!invoker.isAvailable() && getUrl() != null && availablecheck)) {
                try {
                    // 进行重选
                    Invoker<T> rinvoker = reselect(loadbalance, invocation, invokers, selected, availablecheck);
                    if (rinvoker != null) {
                        // 如果 rinvoker 不为空，则将其赋值给 invoker
                        invoker = rinvoker;
                    } else {
                        /// rinvoker 为空，定位 invoker 在 invokers 中的位置
                        int index = invokers.indexOf(invoker);
                        try {
                            // 获取 index + 1 位置处的 Invoker，以下代码等价于：invoker = invokers.get((index + 1) % invokers.size());
                            invoker = index < invokers.size() - 1 ? invokers.get(index + 1) : invoker;
                        } catch (Exception e) {
                            logger.warn(e.getMessage() + " may because invokers list dynamic change, ignore.", e);
                        }
                    }
                } catch (Throwable t) {
                    logger.error("clustor relselect fail reason is :" + t.getMessage() + " if can not slove ,you can set cluster.availablecheck=false in url", t);
                }
            }
            return invoker;
        }

    }

    /**
     * FailoverClusterInvoker 在调用失败时，会自动切换 Invoker 进行重试。默认配置下，Dubbo 会使用这个类作为缺省 Cluster Invoker
     */
    public class FailoverClusterInvoker<T> extends AbstractClusterInvoker<T>{

        /**
         * doInvoke的主要流程如下所示：
         * 1.首先获取重试次数，然后根据重试次数进行循环调用，失败后再重试
         * 2.接着，通过select使用负载均衡组件选择一个Invoker，然后在调用此Invoker的invoke方法
         * 3.如果失败了，记录下异常，并进行重试。重试时会再次调用父类的 list 方法列举 Invoker
         */
        public Result doInvoke(Invocation invocation, final List<Invoker<T>> invokers, LoadBalance loadbalance) throws RpcException {
            List<Invoker<T>> copyinvokers = invokers;
            checkInvokers(copyinvokers, invocation);

            // 获取方法中所配置的重试次数，也就是<dubbo:method/>标签中retries元素的值
            int len = getUrl().getMethodParameter(invocation.getMethodName(), Constants.RETRIES_KEY, Constants.DEFAULT_RETRIES) + 1;
            if (len <= 0) {
                len = 1;
            }
            // retry loop.
            RpcException le = null; // last exception.
            List<Invoker<T>> invoked = new ArrayList<Invoker<T>>(copyinvokers.size()); // invoked invokers.
            Set<String> providers = new HashSet<String>(len);

            // 根据重试次数的大小进行循环调用
            for (int i = 0; i < len; i++) {

                // 在进行重试的时候重新列举 Invoker，这样做的好处是，如果某个服务挂了，通过调用 list 可得到最新可用的 Invoker 列表
                if (i > 0) {
                    checkWhetherDestroyed();
                    copyinvokers = list(invocation);
                    // 对 copyinvokers 进行判空检查
                    checkInvokers(copyinvokers, invocation);
                }

                // 通过负载均衡选择 Invoker
                Invoker<T> invoker = select(loadbalance, invocation, copyinvokers, invoked);
                // 添加到 invoker 到 invoked 列表中，invoked 集合中的 invoker 没有正常提供服务
                invoked.add(invoker);
                // 设置 invoked 到 RPC 上下文中
                RpcContext.getContext().setInvokers((List) invoked);
                try {
                    // 调用目标 Invoker 的 invoke 方法
                    Result result = invoker.invoke(invocation);
                    if (le != null && logger.isWarnEnabled()) {
                        // 省略日志代码
                    }
                    return result;
                } catch (RpcException e) {
                    if (e.isBiz()) { // biz exception.
                        throw e;
                    }
                    le = e;
                } catch (Throwable e) {
                    le = new RpcException(e.getMessage(), e);
                } finally {
                    providers.add(invoker.getUrl().getAddress());
                }
            }
            
            //若重试失败，则抛出异常
            throw new RpcException("Failed to invoke the method ...");
        }

    }

    /**
     * FailbackClusterInvoker 会在调用失败后，返回一个空结果给服务消费者。并通过定时任务对失败的调用进行重新调用，适合执行消息通知等操作
     * 
     * doInvoker该方法负责初次的远程调用。若远程调用失败，则通过 addFailed 方法将调用信息存入到 failed 中，等待定时重试。addFailed 在开始阶段会根据 retryFuture 为空与否，来决定是否开启定时任务。
     * retryFailed 方法则是包含了失败重试的逻辑，该方法会对 failed 进行遍历，然后依次对 Invoker 进行调用。调用成功则将 Invoker 从 failed 中移除，调用失败则忽略失败原因。
     */
    public class FailbackClusterInvoker<T> extends AbstractClusterInvoker<T> {

        private static final Logger logger = LoggerFactory.getLogger(FailbackClusterInvoker.class);
    
        private static final long RETRY_FAILED_PERIOD = 5 * 1000;
    
        private final ScheduledExecutorService scheduledExecutorService = Executors.newScheduledThreadPool(2, new NamedThreadFactory("failback-cluster-timer", true));

        private final ConcurrentMap<Invocation, AbstractClusterInvoker<?>> failed = new ConcurrentHashMap<Invocation, AbstractClusterInvoker<?>>();

        // 使用饿汉模式创建 retryFuture，保证在一个 Invoker 对象中只会创建一个 retryFuture
        private volatile ScheduledFuture<?> retryFuture;
    
        public FailbackClusterInvoker(Directory<T> directory) {
            super(directory);
        }
    
        private void addFailed(Invocation invocation, AbstractClusterInvoker<?> router) {
            if (retryFuture == null) {
                synchronized (this) {
                    if (retryFuture == null) {
                        // 创建定时任务，每隔5秒执行一次
                        retryFuture = scheduledExecutorService.scheduleWithFixedDelay(new Runnable() {
    
                            public void run() {
                                try {
                                    // 对失败的调用进行重试
                                    retryFailed();
                                } catch (Throwable t) { // Defensive fault tolerance
                                    // 如果发生异常，仅打印异常日志，不抛出
                                    logger.error("Unexpected error occur at collect statistic", t);
                                }
                            }
                        }, RETRY_FAILED_PERIOD, RETRY_FAILED_PERIOD, TimeUnit.MILLISECONDS);
                    }
                }
            }
            // 添加 invocation 和 invoker 到 failed 中
            failed.put(invocation, router);
        }
    
        void retryFailed() {
            if (failed.size() == 0) {
                return;
            }

            // 遍历 failed，对失败的调用进行重试
            for (Map.Entry<Invocation, AbstractClusterInvoker<?>> entry : new HashMap<Invocation, AbstractClusterInvoker<?>>(
                    failed).entrySet()) {
                Invocation invocation = entry.getKey();
                Invoker<?> invoker = entry.getValue();
                try {
                    // 再次进行调用
                    invoker.invoke(invocation);
                    // 调用成功后，从 failed 中移除 invoker
                    failed.remove(invocation);
                } catch (Throwable e) {
                    // 仅打印异常，不抛出
                    logger.error("Failed retry to invoke method " + invocation.getMethodName() + ", waiting again.", e);
                }
            }
        }
    
        protected Result doInvoke(Invocation invocation, List<Invoker<T>> invokers, LoadBalance loadbalance) throws RpcException {
            try {
                checkInvokers(invokers, invocation);
                Invoker<T> invoker = select(loadbalance, invocation, invokers, null);
                return invoker.invoke(invocation);
            } catch (Throwable e) {
                // 如果调用过程中发生异常，此时仅打印错误日志，不抛出异常
                logger.error("Failback to invoke method " + invocation.getMethodName() + ", wait for retry in background. Ignored exception: "
                        + e.getMessage() + ", ", e);
                // 如果此 invoker 调用是第一次失败，那么 retryFuture 为空，也就意味着重新执行 invoker 调用的定时任务还没有开启，因此会在 addFailed 方法中开启
                // 定时任务。如果不是第一次调用失败，那么只会在 failed 集合中添加此 invocation -> invoker 键值对，等待被已经开启的定时任务调用
                addFailed(invocation, this);
                // 返回一个空结果给服务消费者
                return new RpcResult(); // ignore
            }
        }
    
    }

    // FailfastClusterInvoker 只会进行一次调用，失败后立即抛出异常。适用于幂等操作，比如新增记录
    public class FailfastClusterInvoker<T> extends AbstractClusterInvoker<T> {

        public FailfastClusterInvoker(Directory<T> directory) {
            super(directory);
        }
    
        public Result doInvoke(Invocation invocation, List<Invoker<T>> invokers, LoadBalance loadbalance) throws RpcException {
            checkInvokers(invokers, invocation);
            // 选择 Invoker
            Invoker<T> invoker = select(loadbalance, invocation, invokers, null);
            try {
                // 调用 Invoker
                return invoker.invoke(invocation);
            } catch (Throwable e) {
                if (e instanceof RpcException && ((RpcException) e).isBiz()) { // biz exception.
                    // 抛出异常
                    throw (RpcException) e;
                }
                // 抛出异常
                throw new RpcException(e instanceof RpcException ? ((RpcException) e).getCode() : 0, "Failfast invoke providers " + invoker.getUrl() + " " + loadbalance.getClass().getSimpleName() + " select from all providers " + invokers + " for service " + getInterface().getName() + " method " + invocation.getMethodName() + " on consumer " + NetUtils.getLocalHost() + " use dubbo version " + Version.getVersion() + ", but no luck to perform the invocation. Last error is: " + e.getMessage(), e.getCause() != null ? e.getCause() : e);
            }
        }
    }

    // FailsafeClusterInvoker 是一种失败安全的 Cluster Invoker。所谓的失败安全是指，当调用过程中出现异常时，FailsafeClusterInvoker 仅会打印异常，而不会抛出异常。适用于写入审计日志等操作。
    public class FailsafeClusterInvoker<T> extends AbstractClusterInvoker<T> {
        private static final Logger logger = LoggerFactory.getLogger(FailsafeClusterInvoker.class);
    
        public FailsafeClusterInvoker(Directory<T> directory) {
            super(directory);
        }
    
        public Result doInvoke(Invocation invocation, List<Invoker<T>> invokers, LoadBalance loadbalance) throws RpcException {
            try {
                checkInvokers(invokers, invocation);
                // 选择 Invoker
                Invoker<T> invoker = select(loadbalance, invocation, invokers, null);
                return invoker.invoke(invocation);
            } catch (Throwable e) {
                logger.error("Failsafe ignore exception: " + e.getMessage(), e);
                // 返回空结果忽略错误
                return new RpcResult(); // ignore
            }
        }
    }

    /**
     * ForkingClusterInvoker 会在运行时通过线程池创建多个线程，并发调用多个服务提供者。只要有一个服务提供者成功返回了结果，doInvoke 方法就会立即结束运行。
     * ForkingClusterInvoker 的应用场景是在一些对实时性要求比较高读操作（注意是读操作，并行写操作可能不安全）下使用
     */
    public class ForkingClusterInvoker<T> extends AbstractClusterInvoker<T> {

        private final ExecutorService executor = Executors.newCachedThreadPool(new NamedThreadFactory("forking-cluster-timer", true));
    
        public ForkingClusterInvoker(Directory<T> directory) {
            super(directory);
        }
    
        @SuppressWarnings({"unchecked", "rawtypes"})
        public Result doInvoke(final Invocation invocation, List<Invoker<T>> invokers, LoadBalance loadbalance) throws RpcException {
            checkInvokers(invokers, invocation);
            final List<Invoker<T>> selected;
            // FORKS_KEY 的值为 forks，DEFAULT_FORKS 的值为 2
            // TIMEOUT_KEY 的值为 timeout，DEFAULT_TIMEOUT 的值为 1000
            final int forks = getUrl().getParameter(Constants.FORKS_KEY, Constants.DEFAULT_FORKS);
            final int timeout = getUrl().getParameter(Constants.TIMEOUT_KEY, Constants.DEFAULT_TIMEOUT);
            // 如果 forks 配置不合理或者说 forks 的值大于 invokers 的 size（也就是服务提供者的数量），则直接将 invokers 赋值给 selected
            if (forks <= 0 || forks >= invokers.size()) {
                selected = invokers;
            } else {
                // 循环并且使用负载均衡算法选择 forks 个数量的 invoker 放入到 selected 集合中
                selected = new ArrayList<Invoker<T>>();
                for (int i = 0; i < forks; i++) {
                    // ForkingClusterInvoker 的调用策略和粘滞调用相矛盾（每次尽量调用同一个 Invoker），而 selected 会添加每次返回的 Invoker ，
                    // 所以可以认为 ForkingClusterInvoker 不支持粘滞调用
                    Invoker<T> invoker = select(loadbalance, invocation, invokers, selected);
                    if (!selected.contains(invoker)) {//Avoid add the same invoker several times.
                        selected.add(invoker);
                    }
                }
            }

            // ----------------------✨ 分割线1 ✨---------------------- //

            RpcContext.getContext().setInvokers((List) selected);
            final AtomicInteger count = new AtomicInteger();
            final BlockingQueue<Object> ref = new LinkedBlockingQueue<Object>();
            // 遍历 selected 集合，为每个 invoker 创建一个执行线程，调用 invoker 的 invoke 方法。
            for (final Invoker<T> invoker : selected) {
                executor.execute(new Runnable() {
                    public void run() {
                        try {
                            Result result = invoker.invoke(invocation);
                            // 将结果存到阻塞队列中
                            ref.offer(result);
                        } catch (Throwable e) {
                            int value = count.incrementAndGet();
                            // 仅在 value 大于等于 selected.size() 时，才将异常对象放入阻塞队列中。这是因为在并行调用多个服务提供者的情况下，只要有一个服务提供者能够成功返回结果，而其他全部失败。
                            // 此时 ForkingClusterInvoker 仍应该返回成功的结果，而非抛出异常。因此只有在 selected.size 次调用之后，才能把调用失败抛出的异常添加到阻塞队列中。而后面只会取出阻塞队列的
                            // 第一个元素，如果前 selected.size - 1 次调用都失败了，那么取出的阻塞队列的第一个元素就是这个异常，随后会抛出。如果前面 selected.size -1 次调用有一次成功了，取出的就是
                            // 调用成功的结果，第 selected.size 次调用抛出的异常就会被忽略
                            if (value >= selected.size()) {
                                ref.offer(e);
                            }
                        }
                    }
                });
            }

            // ----------------------✨ 分割线2 ✨---------------------- //

            try {
                // 从阻塞队列中取出远程调用结果
                Object ret = ref.poll(timeout, TimeUnit.MILLISECONDS);

                // 如果结果类型为 Throwable，说明 forks 个 invoker 的调用全部失败了。则抛出异常
                if (ret instanceof Throwable) {
                    Throwable e = (Throwable) ret;
                    throw new RpcException(e instanceof RpcException ? ((RpcException) e).getCode() : 0, "Failed to forking invoke provider " + selected + ", but no luck to perform the invocation. Last error is: " + e.getMessage(), e.getCause() != null ? e.getCause() : e);
                }
                return (Result) ret;
            } catch (InterruptedException e) {
                throw new RpcException("Failed to forking invoke provider " + selected + ", but no luck to perform the invocation. Last error is: " + e.getMessage(), e);
            }
        }
    }







}