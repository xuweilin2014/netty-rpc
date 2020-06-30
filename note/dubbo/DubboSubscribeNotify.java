public class DubboSubscribeNotify {

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

    /******************************************** ✨Zookeeper的Curator客户端✨ ***************************************** */

    public class CuratorZookeeperClient extends AbstractZookeeperClient<CuratorWatcher> {

        public CuratorWatcher createTargetChildListener(String path, ChildListener listener) {
            return new CuratorWatcherImpl(listener);
        }

        private class CuratorWatcherImpl implements CuratorWatcher {

            private volatile ChildListener listener;
    
            public CuratorWatcherImpl(ChildListener listener) {
                this.listener = listener;
            }
    
            public void unwatch() {
                this.listener = null;
            }
    
            public void process(WatchedEvent event) throws Exception {
                if (listener != null) {
                    String path = event.getPath() == null ? "" : event.getPath();
                    listener.childChanged(path,
                            // if path is null, curator using watcher will throw NullPointerException.
                            // if client connect or disconnect to server, zookeeper will queue
                            // watched event(Watcher.Event.EventType.None, .., path = null).
                            StringUtils.isNotEmpty(path)
                                    ? client.getChildren().usingWatcher(this).forPath(path)
                                    : Collections.<String>emptyList());
                }
            }
        }

        public List<String> addTargetChildListener(String path, CuratorWatcher listener) {
            try {
                return client.getChildren().usingWatcher(listener).forPath(path);
            } catch (NoNodeException e) {
                return null;
            } catch (Exception e) {
                throw new IllegalStateException(e.getMessage(), e);
            }
        }
    }


}