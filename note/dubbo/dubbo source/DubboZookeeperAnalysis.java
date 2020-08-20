public class DubboSubscribeNotify {

    /********************************************************** Zookeeper 订阅与通知 ****************************************************************************** */

    /**
     * 每次通过getObject获取一个远程代理对象时，就会创建一个RegistryDirectory，含有特定的Invoker集合（这里Invoker可以简单的看成服务
     * 提供者），而RegistryDiectory对象之后，就会向Zookeeper注册中心上创建3个节点（providers、configurators、routers），并且
     * 在这三个节点上注册子节点变化监听器，每当这3个节点的子节点发生变化（增加/删除子节点）的时候，就会回调RegistryDirectory中的notify方法，进行相应的更新。
     * 
     * 每个<dubbo:reference/>标签对应的对象在被引用的时候，都会调用创建一个RegistryDirectory，并且向providers、configurators、routers这
     * 3个节点上注册子节点监听器，从而根据配置的变化，及时地更新自己的信息
     */

    // 服务引用过程中的订阅与通知
    public class RegistryProtocol implements Protocol {

        public <T> Invoker<T> refer(Class<T> type, URL url) throws RpcException {
            // 取 registry 参数值，并将其设置为协议头，比如url中配置了registry=zookeeper，那么就将zookeeper设置为url的协议，
            // 然后把registry=zookeeper从url中移除掉
            url = url.setProtocol(url.getParameter(Constants.REGISTRY_KEY, Constants.DEFAULT_REGISTRY))
                    .removeParameter(Constants.REGISTRY_KEY);

            // registryFactory为RegistryFactory$Adaptive，所以这里会根据url中的protocol，也就是url中协议的类型来调用对应RegistryFactory对象
            // 的getRegistry方法。这里则是调用ZookeeperRegistryFactory对象的getRegistry方法，返回一个ZookeeperRegistry对象
            Registry registry = registryFactory.getRegistry(url);
            if (RegistryService.class.equals(type)) {
                return proxyFactory.getInvoker((T) registry, type, url);
            }

            // group="a,b" or group="*"
            // 将 url 查询字符串转为 Map
            Map<String, String> qs = StringUtils.parseQueryString(url.getParameterAndDecoded(Constants.REFER_KEY));
            // 获取 group 配置
            String group = qs.get(Constants.GROUP_KEY);
            if (group != null && group.length() > 0) {
                if ((Constants.COMMA_SPLIT_PATTERN.split(group)).length > 1 || "*".equals(group)) {
                    return doRefer(getMergeableCluster(), registry, type, url);
                }
            }

            // 调用 doRefer 继续执行服务引用逻辑
            return doRefer(cluster, registry, type, url);
        }

        private <T> Invoker<T> doRefer(Cluster cluster, Registry registry, Class<T> type, URL url) {
            RegistryDirectory<T> directory = new RegistryDirectory<T>(type, url);
            directory.setRegistry(registry);
            directory.setProtocol(protocol);
            // all attributes of REFER_KEY
            Map<String, String> parameters = new HashMap<String, String>(directory.getUrl().getParameters());
            URL subscribeUrl = new URL(Constants.CONSUMER_PROTOCOL, parameters.remove(Constants.REGISTER_IP_KEY), 0,
                    type.getName(), parameters);

            // 服务消费方向注册中心注册自己（也就是在consumers目录下创建一个新节点），供其他层使用，比如服务治理
            if (!Constants.ANY_VALUE.equals(url.getServiceInterface())
                    && url.getParameter(Constants.REGISTER_KEY, true)) {
                registry.register(subscribeUrl.addParameters(Constants.CATEGORY_KEY, Constants.CONSUMERS_CATEGORY,
                        Constants.CHECK_KEY, String.valueOf(false)));
            }

            // 进行到这里时，subscribeUrl为：
            // consumer://169.254.207.250/com.dubbo.simple.common.DemoService?application=consumer-of-helloworld-app&dubbo=2.6.0&
            // interface=com.dubbo.simple.common.DemoService&methods=sayHello,sayGoodBye&pid=15336&side=consumer&timestamp=1593437387673
            // 这里给url添加一个参数：category=providers,configurators,router，表示要订阅这三个服务目录，所谓的订阅其实就是在这三个节点
            // 上注册了子节点变化监听器，如果其子节点发生了变化，就会通知RegistryDirectory（也就是调用RegistryDirectory的notify方法）
            directory.subscribe(subscribeUrl.addParameter(Constants.CATEGORY_KEY, Constants.PROVIDERS_CATEGORY + ","
                    + Constants.CONFIGURATORS_CATEGORY + "," + Constants.ROUTERS_CATEGORY));

            /**
             * 这里的Cluster是Cluster$Adaptive类型的对象，调用join方法时有如下代码：
             * 
             * Cluster cluster = ExtensionLoader.getExtensionLoader(Cluster.class).getExtension("FailoverCluster");
             * cluster.join(directory);
             * 
             * Cluster默认的扩展为FailoverCluster，而获取到的cluster对象实际上是一个Wrapper包装类MockClusterWrapper，
             * 在这个MockClusterWrapper中的join方法会返回一个MockClusterInvoker对象，而这个MockClusterInvoker中封装了
             * FailoverClusterInvoker，并且这个FailoverClusterInvoker中保存了下面代码中传入的参数directory，用于获取
             * invoker集合。
             * 
             * 最终，cluster.join返回的是一个MockClusterInvoker对象，这个对象以及其中封装的FailoverClusterInvoker，并不是真正的可以
             * 用来向服务器发起Rpc调用的Invoker，而是分别包装了服务降级和集群容错逻辑。而FailoverClusterInvoker中的directory（RegistryDirectory）
             * 目录中则真正持有可以向服务器进行远程调用的Invoker，比如DubboInvoker
             */
            Invoker invoker = cluster.join(directory);
            ProviderConsumerRegTable.registerConsuemr(invoker, url, subscribeUrl, directory);
            return invoker;
        }

    }

    public class RegistryDirectory<T> extends AbstractDirectory<T> implements NotifyListener {

        public void subscribe(URL url) {
            setConsumerUrl(url);
            // url为订阅条件，不能为空
            // this为变更事件监听器，不能为空，RegistryDirectory实现了NotifyListener接口，其本身可以看做是一个事件监听器
            // 这里不管registry具体类型是什么，都会调用FailbackRegistry的subscribe方法
            registry.subscribe(url, this);
        }

    }

    public static class UrlUtils{

        public static boolean isMatch(URL consumerUrl, URL providerUrl) {

            String consumerInterface = consumerUrl.getServiceInterface();
            String providerInterface = providerUrl.getServiceInterface();
            if (!(Constants.ANY_VALUE.equals(consumerInterface) || StringUtils.isEquals(consumerInterface, providerInterface)))
                return false;
    
            if (!isMatchCategory(providerUrl.getParameter(Constants.CATEGORY_KEY, Constants.DEFAULT_CATEGORY),
                    consumerUrl.getParameter(Constants.CATEGORY_KEY, Constants.DEFAULT_CATEGORY))) {
                return false;
            }
            if (!providerUrl.getParameter(Constants.ENABLED_KEY, true)
                    && !Constants.ANY_VALUE.equals(consumerUrl.getParameter(Constants.ENABLED_KEY))) {
                return false;
            }
    
            String consumerGroup = consumerUrl.getParameter(Constants.GROUP_KEY);
            String consumerVersion = consumerUrl.getParameter(Constants.VERSION_KEY);
            String consumerClassifier = consumerUrl.getParameter(Constants.CLASSIFIER_KEY, Constants.ANY_VALUE);
    
            String providerGroup = providerUrl.getParameter(Constants.GROUP_KEY);
            String providerVersion = providerUrl.getParameter(Constants.VERSION_KEY);
            String providerClassifier = providerUrl.getParameter(Constants.CLASSIFIER_KEY, Constants.ANY_VALUE);

            return (Constants.ANY_VALUE.equals(consumerGroup) || StringUtils.isEquals(consumerGroup, providerGroup) || StringUtils.isContains(consumerGroup, providerGroup))
                    && (Constants.ANY_VALUE.equals(consumerVersion) || StringUtils.isEquals(consumerVersion, providerVersion))
                    && (consumerClassifier == null || Constants.ANY_VALUE.equals(consumerClassifier) || StringUtils.isEquals(consumerClassifier, providerClassifier));
        }

    }

    public abstract class AbstractRegistry implements Registry {

        private final ConcurrentMap<URL, Set<NotifyListener>> subscribed = new ConcurrentHashMap<URL, Set<NotifyListener>>();

        public void subscribe(URL url, NotifyListener listener) {
            if (url == null) {
                throw new IllegalArgumentException("subscribe url == null");
            }
            if (listener == null) {
                throw new IllegalArgumentException("subscribe listener == null");
            }
            if (logger.isInfoEnabled()) {
                logger.info("Subscribe: " + url);
            }
            // 从缓存中获取已经订阅了url的监听器集合
            Set<NotifyListener> listeners = subscribed.get(url);
            if (listeners == null) {
                subscribed.putIfAbsent(url, new ConcurrentHashSet<NotifyListener>());
                listeners = subscribed.get(url);
            }
            // 将当前监听器添加到对此url对应的监听器集合中
            listeners.add(listener);
        }

        protected void notify(URL url, NotifyListener listener, List<URL> urls) {
            // 检查各个参数的合法性，比如是否为null

            Map<String, List<URL>> result = new HashMap<String, List<URL>>();
            // result中存储的为zookeeper中各个目录（category）下的url，也就是category -> urls 的映射关系
            // 并且还会检测这些url和消费者的url是否相匹配（传进来的参数中，url为消费者的URL，表示要调用服务的相关配置信息，比如服务的接口名是否相同，
            // 版本，组等等）
            for (URL u : urls) {
                if (UrlUtils.isMatch(url, u)) {
                    String category = u.getParameter(Constants.CATEGORY_KEY, Constants.DEFAULT_CATEGORY);
                    List<URL> categoryList = result.get(category);
                    if (categoryList == null) {
                        categoryList = new ArrayList<URL>();
                        result.put(category, categoryList);
                    }
                    categoryList.add(u);
                }
            }
            if (result.size() == 0) {
                return;
            }
            
            // 已经通知过的category
            Map<String, List<URL>> categoryNotified = notified.get(url);
            if (categoryNotified == null) {
                notified.putIfAbsent(url, new ConcurrentHashMap<String, List<URL>>());
                categoryNotified = notified.get(url);
            }
            for (Map.Entry<String, List<URL>> entry : result.entrySet()) {
                String category = entry.getKey();
                List<URL> categoryList = entry.getValue();
                categoryNotified.put(category, categoryList);
                saveProperties(url);
                // 最终调用RegistryDirectory中的notify方法
                listener.notify(categoryList);
            }
        }

    }

    // AbstractRegistry的子类为FailbackRegistry，而RedisRegistry、ZookeeperRegistry、DubboRegistry以及MulticastRegistry都继承于FailbackRegistry
    public abstract class FailbackRegistry extends AbstractRegistry {
  
        // FailbackRegistry的subscribe方法中包含了失败重试的逻辑
        @Override
        public void subscribe(URL url, NotifyListener listener) {
            if (destroyed.get()) {
                return;
            }
            // 调用AbstractRegistry中的subscribe方法，将此listener添加到这个url的监听器集合当中
            super.subscribe(url, listener);
            removeFailedSubscribed(url, listener);
            try {
                // Sending a subscription request to the server side
                // 向服务器端发送订阅请求，我们这里使用的是ZookeeperRegistry
                doSubscribe(url, listener);
            } catch (Exception e) {
                // 省略代码
            }
        }

        // urls为订阅的各个目录下的子节点url值集合，比如providers、configurators、routers这三个目录下面的子节点集合
        @Override
        protected void notify(URL url, NotifyListener listener, List<URL> urls) {
            if (url == null) {
                throw new IllegalArgumentException("notify url == null");
            }
            if (listener == null) {
                throw new IllegalArgumentException("notify listener == null");
            }
            try {
                // doNotify方法中没做处理，而是直接调用父类中的notify方法
                doNotify(url, listener, urls);
            } catch (Exception t) {
                // 将失败的通知请求记录到失败列表，定时重试
                Map<NotifyListener, List<URL>> listeners = failedNotified.get(url);
                if (listeners == null) {
                    failedNotified.putIfAbsent(url, new ConcurrentHashMap<NotifyListener, List<URL>>());
                    listeners = failedNotified.get(url);
                }
                listeners.put(listener, urls);
                logger.error("Failed to notify for subscribe " + url + ", waiting for retry, cause: " + t.getMessage(), t);
            }
        }

    }

    public class ZookeeperRegistry extends FailbackRegistry {

        // zkClient默认是ZkclientZookeeperClient类型的对象
        private final ZookeeperClient zkClient;

        // 向url中category指明的目录注册监听器listener
        protected void doSubscribe(final URL url, final NotifyListener listener) {
            try {
                if (Constants.ANY_VALUE.equals(url.getServiceInterface())) {
                    // 省略代码.....
                } else {
                    List<URL> urls = new ArrayList<URL>();
                    // 这里的path分别为providers，routers，configurators三种
                    for (String path : toCategoriesPath(url)) {
                        ConcurrentMap<NotifyListener, ChildListener> listeners = zkListeners.get(url);
                        if (listeners == null) {
                            zkListeners.putIfAbsent(url, new ConcurrentHashMap<NotifyListener, ChildListener>());
                            listeners = zkListeners.get(url);
                        }
                        // 根据我们的listener获取一个ChildListener实例
                        ChildListener zkListener = listeners.get(listener);
                        // 没有的话就创建一个ChildListener实例。
                        if (zkListener == null) {
                            listeners.putIfAbsent(listener, new ChildListener() {
                                public void childChanged(String parentPath, List<String> currentChilds) {
                                    // 如果 path 下面的子节点的状态发生变化（增加或者删除节点），那么就会最终调用ZookeeperRegistry下面的notify方法
                                    // 这个ChildListener接口用于把zkclient的事件（IZkChildListener）转换到registry事件（NotifyListener）。
                                    // 这里的做法可以更好的把zkclient的api和dubbo真身的注册中心逻辑分离开，毕竟dubbo除了zkclient以外还可以选择curator。
                                    ZookeeperRegistry.this.notify(url, listener, toUrlsWithEmpty(url, parentPath, currentChilds));
                                }
                            });
                            zkListener = listeners.get(listener);
                        }
                        // 根据path在Zookeeper中创建节点
                        zkClient.create(path, false);
                        // 这里zkClient是dubbo的ZkclientZookeeperClient，在addChildListener中会转化为ZkClient（Zookeeper的开源客户端）中的Listener
                        List<String> children = zkClient.addChildListener(path, zkListener);
                        if (children != null) {
                            urls.addAll(toUrlsWithEmpty(url, path, children));
                        }
                    }
                    // 订阅完成之后，进行通知，调用ZookeeperRegistry的父类FailbackRegistry的notify方法
                    // 这里的urls为订阅的各个path节点下面子节点的值的集合，如果path下面没有子节点，则为empty://开头的url
                    notify(url, listener, urls);
                }
            } catch (Throwable e) {
                throw new RpcException("Failed to subscribe " + url + " to zookeeper " + getUrl() + ", cause: " + e.getMessage(), e);
            }
        }

        private String toServicePath(URL url) {
            String name = url.getServiceInterface();
            if (Constants.ANY_VALUE.equals(name)) {
                return toRootPath();
            }
            return toRootDir() + URL.encode(name);
        }
    
        private String[] toCategoriesPath(URL url) {
            String[] categroies;
            if (Constants.ANY_VALUE.equals(url.getParameter(Constants.CATEGORY_KEY))) {
                categroies = new String[]{Constants.PROVIDERS_CATEGORY, Constants.CONSUMERS_CATEGORY,
                        Constants.ROUTERS_CATEGORY, Constants.CONFIGURATORS_CATEGORY};
            } else {
                categroies = url.getParameter(Constants.CATEGORY_KEY, new String[]{Constants.DEFAULT_CATEGORY});
            }
            String[] paths = new String[categroies.length];
            for (int i = 0; i < categroies.length; i++) {
                paths[i] = toServicePath(url) + Constants.PATH_SEPARATOR + categroies[i];
            }
            return paths;
        }
    }

    public interface ChildListener {

        void childChanged(String path, List<String> children);
    
    }

    // AbstractZookeeperClient有两个子类：CuratorZookeeperClient和ZkclientZookeeperClient
    public abstract class AbstractZookeeperClient<TargetChildListener> implements ZookeeperClient {

        public List<String> addChildListener(String path, final ChildListener listener) {
            // listeners是ChildListener到TargetChildListener的映射，目的应该是把ZkClient/Curator的逻辑与dubbo的逻辑进行一个分离
            ConcurrentMap<ChildListener, TargetChildListener> listeners = childListeners.get(path);
            if (listeners == null) {
                childListeners.putIfAbsent(path, new ConcurrentHashMap<ChildListener, TargetChildListener>());
                listeners = childListeners.get(path);
            }
            TargetChildListener targetListener = listeners.get(listener);
            if (targetListener == null) {
                // createTargetListener是一个模板方法，具体由子类进行实现，即创建ZkClient或者Curator这两种客户端所对应的监听器对象
                listeners.putIfAbsent(listener, createTargetChildListener(path, listener));
                targetListener = listeners.get(listener);
            }
            // addTargetChildListener也是模板方法，将前面创建好的监听器注册到path路径上
            return addTargetChildListener(path, targetListener);
        }

        protected abstract TargetChildListener createTargetChildListener(String path, ChildListener listener);

        protected abstract List<String> addTargetChildListener(String path, TargetChildListener listener);

    }

    /*********************************** ✨Zookeeper的ZkClient客户端✨  ******************************************** */

    public class ZkclientZookeeperClient extends AbstractZookeeperClient<IZkChildListener> {

        /**
         * 在原生的zookeeper中，使用watcher需要每次先注册，而且使用一次就需要注册一次。而再zkClient中，没有注册watcher的必要，而是引入了listener的概念，
         * 即只要client在某一个节点中注册了listener，只要服务端发生变化，就会通知当前注册listener的客户端。
         * zkClient中主要有3种listener事件可以注册，分别是节点发生变化，节点内容发生变化，状态发生变化。注册这三种listener,服务端可以主动通知客户端最新的数据。
         * zkClient提供了三种事件监听接口，分别是：
         * 1.IZkDataListener ：当前节点数据内容或版本发生变化或者当前节点被删除，触发当前接口
         * 2.IZkChildListener: 节点列表变化事件监听，即当前节点的子节点发生变化(删除)或子节点列表发生变化(新增或删除)，触发当前接口
         * 3.IZkStateListener：zookeeper连接状态发生变化时，触发该接口，这个在zkClient连接zookeeper时，用到比较多
         */
        public IZkChildListener createTargetChildListener(String path, final ChildListener listener) {
            // 创建一个IZkChildListener，用于监听path下面节点的变化，如果发生了变化，则会回调我们定义的ChildListener，
            // 并且回调时，zkClient会传入当前path下面的节点列表，最终会调用ZookeeperRegistry的notify方法
            return new IZkChildListener() {
                public void handleChildChange(String parentPath, List<String> currentChilds)
                        throws Exception {
                    listener.childChanged(parentPath, currentChilds);
                }
            };
        }

        public List<String> addTargetChildListener(String path, final IZkChildListener listener) {
            return client.subscribeChildChanges(path, listener);
        }

    }

    public class ZkClientWrapper{

        public List<String> subscribeChildChanges(String path, final IZkChildListener listener) {
            Assert.notNull(client, new IllegalStateException("Zookeeper is not connected yet!"));
            return client.subscribeChildChanges(path, listener);
        }

    }

    public class ZkClient implements Watcher {

        public List<String> subscribeChildChanges(String path, IZkChildListener listener) {
            synchronized (_childListener) {
                Set<IZkChildListener> listeners = _childListener.get(path);
                if (listeners == null) {
                    listeners = new CopyOnWriteArraySet<IZkChildListener>();
                    _childListener.put(path, listeners);
                }
                listeners.add(listener);
            }

            /**
             * Installs a child watch for the given path.
             * return the current children of the path or null if the zk node with the given path doesn't exist.
             */
            return watchForChilds(path);
        }

    }

    /*********************************************************************** ✨Zookeeper 连接的创建✨ ******************************************************************** */

    // 执行注册中心缓存的逻辑
    public abstract class AbstractRegistry implements Registry {

        // 已经注册的 URL 集合
        private final Set<URL> registered = new ConcurrentHashSet<URL>();
        // 已经订阅的监听器集合
        private final ConcurrentMap<URL, Set<NotifyListener>> subscribed = new ConcurrentHashMap<URL, Set<NotifyListener>>();
        // 已经通知的 URL 集合
        private final ConcurrentMap<URL, Map<String, List<URL>>> notified = new ConcurrentHashMap<URL, Map<String, List<URL>>>();

        public void register(URL url) {
            if (url == null) {
                throw new IllegalArgumentException("register url == null");
            }
            if (logger.isInfoEnabled()) {
                logger.info("Register: " + url);
            }
            // 添加 url 到已经注册过的 URL 集合中，之后 recover 建立新会话或者 destroy 取消注册到注册中心上的 url 时，都会用到
            registered.add(url);
        }

        public void unregister(URL url) {
            if (url == null) {
                throw new IllegalArgumentException("unregister url == null");
            }
            if (logger.isInfoEnabled()) {
                logger.info("Unregister: " + url);
            }
            registered.remove(url);
        }

        public void destroy() {
            if (!destroyed.compareAndSet(false, true)) {
                return;
            }
    
            if (logger.isInfoEnabled()) {
                logger.info("Destroy registry:" + getUrl());
            }
            Set<URL> destroyRegistered = new HashSet<URL>(getRegistered());
            // 遍历已经注册过的 URL 集合，对每一个 URL 进行取消注册
            if (!destroyRegistered.isEmpty()) {
                for (URL url : new HashSet<URL>(getRegistered())) {
                    if (url.getParameter(Constants.DYNAMIC_KEY, true)) {
                        try {
                            unregister(url);
                            if (logger.isInfoEnabled()) {
                                logger.info("Destroy unregister url " + url);
                            }
                        } catch (Throwable t) {
                            logger.warn("Failed to unregister url " + url + " to registry " + getUrl() + " on destroy, cause: " + t.getMessage(), t);
                        }
                    }
                }
            }
            // 遍历已经订阅的监听器集合，对每一个监听器取消订阅，其实也就是取消监听
            Map<URL, Set<NotifyListener>> destroySubscribed = new HashMap<URL, Set<NotifyListener>>(getSubscribed());
            if (!destroySubscribed.isEmpty()) {
                for (Map.Entry<URL, Set<NotifyListener>> entry : destroySubscribed.entrySet()) {
                    URL url = entry.getKey();
                    for (NotifyListener listener : entry.getValue()) {
                        try {
                            unsubscribe(url, listener);
                            if (logger.isInfoEnabled()) {
                                logger.info("Destroy unsubscribe url " + url);
                            }
                        } catch (Throwable t) {
                            logger.warn("Failed to unsubscribe url " + url + " to registry " + getUrl() + " on destroy, cause: " + t.getMessage(), t);
                        }
                    }
                }
            }
        }
    }

    // 执行失败重试的逻辑
    public abstract class FailbackRegistry extends AbstractRegistry {

        // Scheduled executor service
        private final ScheduledExecutorService retryExecutor = Executors.newScheduledThreadPool(1, new NamedThreadFactory("DubboRegistryFailedRetryTimer", true));

        // Timer for failure retry, regular check if there is a request for failure, and if there is, an unlimited retry
        private final ScheduledFuture<?> retryFuture;

        // 发起注册失败的 URL 集合
        private final Set<URL> failedRegistered = new ConcurrentHashSet<URL>();

        // 取消注册失败的 URL 集合
        private final Set<URL> failedUnregistered = new ConcurrentHashSet<URL>();

        // 发起订阅失败的 URL 集合
        private final ConcurrentMap<URL, Set<NotifyListener>> failedSubscribed = new ConcurrentHashMap<URL, Set<NotifyListener>>();

        // 取消订阅失败的 URL 集合
        private final ConcurrentMap<URL, Set<NotifyListener>> failedUnsubscribed = new ConcurrentHashMap<URL, Set<NotifyListener>>();

        //通知失败的 URL 集合
        private final ConcurrentMap<URL, Map<NotifyListener, List<URL>>> failedNotified = new ConcurrentHashMap<URL, Map<NotifyListener, List<URL>>>();

        private AtomicBoolean destroyed = new AtomicBoolean(false);

        // 在 FailbackRegistry 中定义了一个 ScheduledExecutorService ，每经过固定的时间间隔，大约 5s，就调用 FailbackRegistry#retry 方法
        // 在定时器中调用retry方法的时候，会把这五个集合分别遍历和重试，重试成功则从集合中移除。FailbackRegistry实现了 subscribe、register等通用方法，
        // 里面调用了未实现的模板方法，会由子类实现。通用方法会调用这些模板方法，如果捕获到异常，则会把URL添加到对应的重试集合中，以供定时器去重试
        public FailbackRegistry(URL url) {
            super(url);
            int retryPeriod = url.getParameter(Constants.REGISTRY_RETRY_PERIOD_KEY, Constants.DEFAULT_REGISTRY_RETRY_PERIOD);
            this.retryFuture = retryExecutor.scheduleWithFixedDelay(new Runnable() {
                public void run() {
                    // Check and connect to the registry
                    try {
                        retry();
                    } catch (Throwable t) { // Defensive fault tolerance
                        logger.error("Unexpected error occur at failed retry, cause: " + t.getMessage(), t);
                    }
                }
            }, retryPeriod, retryPeriod, TimeUnit.MILLISECONDS);
        }

        @Override
        public void register(URL url) {
            if (destroyed.get()){
                return;
            }
            super.register(url);
            failedRegistered.remove(url);
            failedUnregistered.remove(url);
            try {
                // Sending a registration request to the server side
                doRegister(url);
            } catch (Exception e) {
                // 省略代码

                // Record a failed registration request to a failed list, retry regularly
                failedRegistered.add(url);
            }
        }

        @Override
        public void unregister(URL url) {
            if (destroyed.get()){
                return;
            }
            super.unregister(url);
            failedRegistered.remove(url);
            failedUnregistered.remove(url);
            try {
                // Sending a cancellation request to the server side
                doUnregister(url);
            } catch (Exception e) {
                // 省略代码

                // Record a failed registration request to a failed list, retry regularly
                failedUnregistered.add(url);
            }
        }

        // handleNewSession is Called after the zookeeper session has expired and a new session has been created. You would have to re-create any ephemeral nodes here.
        // 当和 zookeeper 的一个新的会话建立时，就会回调这里的 recover 方法，把保存起来已经 注册过的 URL 和已经订阅过的监听器再注册和订阅一遍
        protected void recover() throws Exception {
            // register
            Set<URL> recoverRegistered = new HashSet<URL>(getRegistered());
            if (!recoverRegistered.isEmpty()) {
                if (logger.isInfoEnabled()) {
                    logger.info("Recover register url " + recoverRegistered);
                }
                for (URL url : recoverRegistered) {
                    failedRegistered.add(url);
                }
            }
            // subscribe
            Map<URL, Set<NotifyListener>> recoverSubscribed = new HashMap<URL, Set<NotifyListener>>(getSubscribed());
            if (!recoverSubscribed.isEmpty()) {
                if (logger.isInfoEnabled()) {
                    logger.info("Recover subscribe url " + recoverSubscribed.keySet());
                }
                for (Map.Entry<URL, Set<NotifyListener>> entry : recoverSubscribed.entrySet()) {
                    URL url = entry.getKey();
                    for (NotifyListener listener : entry.getValue()) {
                        addFailedSubscribed(url, listener);
                    }
                }
            }
        }

        protected void retry() {
            // 遍历 注册失败的 URL 集合，对其中的每一个 URL 进行注册重试
            if (!failedRegistered.isEmpty()) {
                Set<URL> failed = new HashSet<URL>(failedRegistered);
                if (failed.size() > 0) {
                    if (logger.isInfoEnabled()) {
                        logger.info("Retry register " + failed);
                    }
                    try {
                        for (URL url : failed) {
                            try {
                                doRegister(url);
                                failedRegistered.remove(url);
                            } catch (Throwable t) { // Ignore all the exceptions and wait for the next retry
                                logger.warn("Failed to retry register " + failed + ", waiting for again, cause: " + t.getMessage(), t);
                            }
                        }
                    } catch (Throwable t) { // Ignore all the exceptions and wait for the next retry
                        logger.warn("Failed to retry register " + failed + ", waiting for again, cause: " + t.getMessage(), t);
                    }
                }
            }

            // 遍历 取消注册失败的 URL 集合，对其中的每一个 URL 进行取消注册重试
            if (!failedUnregistered.isEmpty()) {
                Set<URL> failed = new HashSet<URL>(failedUnregistered);
                if (failed.size() > 0) {
                    if (logger.isInfoEnabled()) {
                        logger.info("Retry unregister " + failed);
                    }
                    try {
                        for (URL url : failed) {
                            try {
                                doUnregister(url);
                                failedUnregistered.remove(url);
                            } catch (Throwable t) { // Ignore all the exceptions and wait for the next retry
                                logger.warn("Failed to retry unregister  " + failed + ", waiting for again, cause: " + t.getMessage(), t);
                            }
                        }
                    } catch (Throwable t) { // Ignore all the exceptions and wait for the next retry
                        logger.warn("Failed to retry unregister  " + failed + ", waiting for again, cause: " + t.getMessage(), t);
                    }
                }
            }

            // 省略代码
        }

        @Override
        public void destroy() {
            if (!canDestroy()){
                return;
            }
            super.destroy();
            try {
                // 取消此任务的执行
                retryFuture.cancel(true);
            } catch (Throwable t) {
                logger.warn(t.getMessage(), t);
            }
        }
    }

    public class ZookeeperRegistry extends FailbackRegistry {

        private final ZookeeperClient zkClient;

        public ZookeeperRegistry(URL url, ZookeeperTransporter zookeeperTransporter) {
            super(url);
            if (url.isAnyHost()) {
                throw new IllegalStateException("registry address == null");
            }
            String group = url.getParameter(Constants.GROUP_KEY, DEFAULT_ROOT);
            if (!group.startsWith(Constants.PATH_SEPARATOR)) {
                group = Constants.PATH_SEPARATOR + group;
            }
            this.root = group;
            // zkClient 是 ZkclientZookeeperClient 类型的对象
            zkClient = zookeeperTransporter.connect(url);
            zkClient.addStateListener(new StateListener() {
                public void stateChanged(int state) {
                    if (state == RECONNECTED) {
                        try {
                            recover();
                        } catch (Exception e) {
                            logger.error(e.getMessage(), e);
                        }
                    }
                }
            });
        }

    }

    public abstract class AbstractZookeeperClient<TargetChildListener> implements ZookeeperClient {
        
        private final URL url;

        private final Set<StateListener> stateListeners = new CopyOnWriteArraySet<StateListener>();

        public void addStateListener(StateListener listener) {
            stateListeners.add(listener);
        }
    
        public void removeStateListener(StateListener listener) {
            stateListeners.remove(listener);
        }
    
        public Set<StateListener> getSessionListeners() {
            return stateListeners;
        }

        protected void stateChanged(int state) {
            for (StateListener sessionListener : getSessionListeners()) {
                sessionListener.stateChanged(state);
            }
        }
    }

    public class ZkclientZookeeperClient extends AbstractZookeeperClient<IZkChildListener> {

        public ZkclientZookeeperClient(URL url) {
            super(url);
            client = new ZkClientWrapper(url.getBackupAddress(), 30000);
            client.addListener(new IZkStateListener() {
                public void handleStateChanged(KeeperState state) throws Exception {
                    ZkclientZookeeperClient.this.state = state;
                    if (state == KeeperState.Disconnected) {
                        stateChanged(StateListener.DISCONNECTED);
                    } else if (state == KeeperState.SyncConnected) {
                        stateChanged(StateListener.CONNECTED);
                    }
                }
    
                public void handleNewSession() throws Exception {
                    stateChanged(StateListener.RECONNECTED);
                }
            });
            client.start();
        }

    }

    public class ZkClientWrapper {

        private long timeout;
        private ZkClient client;
        private volatile KeeperState state;
        private ListenableFutureTask<ZkClient> listenableFutureTask;
        private volatile boolean started = false;


        public ZkClientWrapper(final String serverAddr, long timeout) {
            this.timeout = timeout;
            listenableFutureTask = ListenableFutureTask.create(new Callable<ZkClient>() {
                @Override
                public ZkClient call() throws Exception {
                    return new ZkClient(serverAddr, Integer.MAX_VALUE);
                }
            });
        }

        public void start() {
            if (!started) {
                Thread connectThread = new Thread(listenableFutureTask);
                connectThread.setName("DubboZkclientConnector");
                connectThread.setDaemon(true);
                connectThread.start();
                try {
                    client = listenableFutureTask.get(timeout, TimeUnit.MILLISECONDS);
                } catch (Throwable t) {
                    logger.error("Timeout! zookeeper server can not be connected in : " + timeout + "ms!", t);
                }
                started = true;
            } else {
                logger.warn("Zkclient has already been started!");
            }
        }

        public void addListener(final IZkStateListener listener) {
            listenableFutureTask.addListener(new Runnable() {
                @Override
                public void run() {
                    try {
                        client = listenableFutureTask.get();
                        client.subscribeStateChanges(listener);
                    } catch (InterruptedException e) {
                        logger.warn(Thread.currentThread().getName() + " was interrupted unexpectedly, which may cause unpredictable exception!");
                    } catch (ExecutionException e) {
                        logger.error("Got an exception when trying to create zkclient instance, can not connect to zookeeper server, please check!", e);
                    }
                }
            });
        }

    }

}