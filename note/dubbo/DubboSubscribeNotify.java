public class DubboSubscribeNotify {

    /**
     * 每次通过getObject获取一个远程代理对象时，就会创建一个RegistryDirectory，含有特定的Invoker集合（这里Invoker可以简单的看成服务
     * 提供者），而RegistryDiectory对象之后，就会向Zookeeper注册中心上创建3个节点（providers、configurators、routers），并且
     * 在这三个节点上注册子节点变化监听器，每当这3个节点的子节点发生变化（增加/删除子节点）的时候，就会回调RegistryDirectory中的notify方法，进行相应的更新。
     * 
     * 每个<dubbo:reference/>标签对应的对象在被引用的时候，都会调用创建一个RegistryDirectory，并且向providers、configurators、routers这
     * 3个节点上注册子节点监听器，从而根据配置的变化，及时地更新自己的信息
     */

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
            // 这里给url添加一个参数：category=providers,configurators,router，表示要订阅这三个服务目录
            directory.subscribe(subscribeUrl.addParameter(Constants.CATEGORY_KEY, Constants.PROVIDERS_CATEGORY + ","
                    + Constants.CONFIGURATORS_CATEGORY + "," + Constants.ROUTERS_CATEGORY));

            Invoker invoker = cluster.join(directory);
            ProviderConsumerRegTable.registerConsuemr(invoker, url, subscribeUrl, directory);
            return invoker;
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
            // 将当前监听器添加到监听器的集合中
            listeners.add(listener);
        }

    }

    // AbstractRegistry的子类为FailbackRegistry，而RedisRegistry、ZookeeperRegistry、DubboRegistry以及MulticastRegistry都继承于FailbackRegistry
    public abstract class FailbackRegistry extends AbstractRegistry {

        @Override
        public void register(URL url) {
            if (destroyed.get()) {
                return;
            }
            super.register(url);
            failedRegistered.remove(url);
            failedUnregistered.remove(url);
            try {
                // 模板代码，由具体的子类实现，比如ZookeeperRegistry
                doRegister(url);
            } catch (Exception e) {
                // 省略代码
            }
        }

    
        @Override
        public void subscribe(URL url, NotifyListener listener) {
            if (destroyed.get()) {
                return;
            }
            // 将此listener添加到这个url的监听器集合当中
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

    }

    public class ZookeeperRegistry extends FailbackRegistry {
        protected void doRegister(URL url) {
            try {
                // 在zookeeper上注册节点
                zkClient.create(toUrlPath(url), url.getParameter(Constants.DYNAMIC_KEY, true));
            } catch (Throwable e) {
                throw new RpcException(
                        "Failed to register " + url + " to zookeeper " + getUrl() + ", cause: " + e.getMessage(), e);
            }
        }

        protected void doSubscribe(final URL url, final NotifyListener listener) {
            try {
                if (Constants.ANY_VALUE.equals(url.getServiceInterface())) {
                    String root = toRootPath();
                    ConcurrentMap<NotifyListener, ChildListener> listeners = zkListeners.get(url);
                    if (listeners == null) {
                        zkListeners.putIfAbsent(url, new ConcurrentHashMap<NotifyListener, ChildListener>());
                        listeners = zkListeners.get(url);
                    }
                    ChildListener zkListener = listeners.get(listener);
                    if (zkListener == null) {
                        listeners.putIfAbsent(listener, new ChildListener() {
                            public void childChanged(String parentPath, List<String> currentChilds) {
                                for (String child : currentChilds) {
                                    child = URL.decode(child);
                                    if (!anyServices.contains(child)) {
                                        anyServices.add(child);
                                        subscribe(url.setPath(child).addParameters(Constants.INTERFACE_KEY, child,
                                                Constants.CHECK_KEY, String.valueOf(false)), listener);
                                    }
                                }
                            }
                        });
                        zkListener = listeners.get(listener);
                    }
                    zkClient.create(root, false);
                    List<String> services = zkClient.addChildListener(root, zkListener);
                    if (services != null && services.size() > 0) {
                        for (String service : services) {
                            service = URL.decode(service);
                            anyServices.add(service);
                            subscribe(url.setPath(service).addParameters(Constants.INTERFACE_KEY, service,
                                    Constants.CHECK_KEY, String.valueOf(false)), listener);
                        }
                    }
                } else {
                    List<URL> urls = new ArrayList<URL>();
                    // 这里的path分别为providers，routers，configurators三种
                    // 
                    for (String path : toCategoriesPath(url)) {
                        ConcurrentMap<NotifyListener, ChildListener> listeners = zkListeners.get(url);
                        if (listeners == null) {
                            zkListeners.putIfAbsent(url, new ConcurrentHashMap<NotifyListener, ChildListener>());
                            listeners = zkListeners.get(url);
                        }
                        ChildListener zkListener = listeners.get(listener);
                        if (zkListener == null) {
                            listeners.putIfAbsent(listener, new ChildListener() {
                                public void childChanged(String parentPath, List<String> currentChilds) {
                                    ZookeeperRegistry.this.notify(url, listener, toUrlsWithEmpty(url, parentPath, currentChilds));
                                }
                            });
                            zkListener = listeners.get(listener);
                        }
                        zkClient.create(path, false);
                        List<String> children = zkClient.addChildListener(path, zkListener);
                        if (children != null) {
                            urls.addAll(toUrlsWithEmpty(url, path, children));
                        }
                    }
                    notify(url, listener, urls);
                }
            } catch (Throwable e) {
                throw new RpcException("Failed to subscribe " + url + " to zookeeper " + getUrl() + ", cause: " + e.getMessage(), e);
            }
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

}