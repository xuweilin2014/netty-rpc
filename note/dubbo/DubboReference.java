public class DubboReference{

    /**
     * Dubbo 服务引用的时机有两个，第一个是在 Spring 容器调用 ReferenceBean 的 afterPropertiesSet 方法时引用服务，第二个是在 ReferenceBean 对应的服务被注入到其他类中时引用。
     * 这两个引用服务的时机区别在于，第一个是饿汉式的，第二个是懒汉式的。默认情况下，Dubbo 使用懒汉式引用服务。如果需要使用饿汉式，可通过配置 <dubbo:reference> 的 init 属性开启。
     * 下面我们按照 Dubbo 默认配置进行分析，整个分析过程从 ReferenceBean 的 getObject 方法开始。
     * 
     * 当我们的服务被注入到其他类中时，Spring 会第一时间调用 getObject 方法，并由该方法执行服务引用逻辑。按照惯例，在进行具体工作之前，需先进行配置检查与收集工作。接着根据收集到的信息决定服务引用的方式，
     * 有三种，第一种是引用本地 (JVM) 服务，第二是通过直连方式引用远程服务，第三是通过注册中心引用远程服务。
     * 
     * 不管是哪种引用方式，最后都会得到一个 Invoker 实例。如果有多个注册中心，多个服务提供者，这个时候会得到一组 Invoker 实例，此时需要通过集群管理类 Cluster 将多个 Invoker 合并成一个实例。
     * 合并后的 Invoker 实例已经具备调用本地或远程服务的能力了，但并不能将此实例暴露给用户使用，这会对用户业务代码造成侵入。此时框架还需要通过代理工厂类 (ProxyFactory) 为服务接口生成代理类，
     * 并让代理类去调用 Invoker 逻辑。避免了 Dubbo 框架代码对业务代码的侵入，同时也让框架更容易使用。
     */

    //class:ReferenceBean
    //ReferenceBean实现了Spring中FactoryBean接口的getObject方法
    public Object getObject() throws Exception {
        return get();
    }

    public class ReferenceConfig<T> extends AbstractReferenceConfig{

        private transient volatile T ref;
        private transient volatile Invoker<?> invoker;
        private transient volatile boolean initialized;
        private transient volatile boolean destroyed;

        public synchronized T get() {
            if (destroyed) {
                throw new IllegalStateException("Already destroyed!");
            }
            //检测 ref 是否为空，为空则通过 init 方法创建
            if (ref == null) {
                //init 方法主要用于处理配置，以及调用 createProxy 生成代理类
                init();
            }
            return ref;
        }

        /**
         * init方法主要的作用是
         * 1.创建一个ConsumerConfig对象（如果不存在的话），并且将<dubbo:consumer/>和<dubbo:reference/>标签中的配置信息保存到ConsumerConfig
         * 和ReferenceConfig对象中，接着通过反射获取到interface属性对应的Class值
         * 2.从系统属性或配置文件中加载与接口名相对应的配置，并将解析结果赋值给 url 字段，这个url字段用于点对点调用
         * 3.检测几个核心配置类是否为空，为空则尝试从其他配置类中获取
         * 4.用于收集各种配置，并将配置存储到 map 中。比如side信息、dubbo版本、进程号、时间戳、方法列表等等
         * 5.处理 MethodConfig 实例。该实例包含了事件通知配置，比如 onreturn、onthrow、oninvoke 等，同时会处理<dubbo:method/>中已经被废弃的属性retry
         * 6.获取服务消费者 ip，以及调用 createProxy 创建代理对象
         */
        private void init() {
            //避免重复初始化
            if (initialized) {
                return;
            }
            initialized = true;
            //检测接口名合法性
            if (interfaceName == null || interfaceName.length() == 0) {
                throw new IllegalStateException("<dubbo:reference interface=\"\" /> interface not allow null!");
            }
            
            //1.检测 consumer 变量（consumer变量为ConsumerConfig类型，表示<dubbo:consumer/>标签中的配置信息）是否为空，为空则创建，
            //2.并且通过 appendProperties 方法把 <dubbo:consumer/> 中的配置信息保存到 ConsumerConfig 类型的对象中（也就是 consumer 对象）中去
            checkDefault();
            //将标签<dubbo:reference/>中的配置信息保存到ReferenceConfig对象中
            appendProperties(this);
            if (getGeneric() == null && getConsumer() != null) {
                setGeneric(getConsumer().getGeneric());
            }

            //检测是否为泛化接口
            if (ProtocolUtils.isGeneric(getGeneric())) {
                interfaceClass = GenericService.class;
            } else {
                try {
                    interfaceClass = Class.forName(interfaceName, true, Thread.currentThread()
                            .getContextClassLoader());
                } catch (ClassNotFoundException e) {
                    throw new IllegalStateException(e.getMessage(), e);
                }
                //这个方法用来检查<dubbo:reference/>标签中interface属性所指定的类是否存在，以及是否是一个接口
                checkInterfaceAndMethods(interfaceClass, methods);
            }

            // -------------------------------✨ 分割线1 ✨------------------------------
            
            //从系统属性或配置文件中加载与接口名相对应的配置，并将解析结果赋值给 url 字段

            // -------------------------------✨ 分割线2 ✨------------------------------
            
            //检测几个核心配置类是否为空，为空则尝试从其他配置类中获取

            // -------------------------------✨ 分割线3 ✨------------------------------

            //将side信息（这里是consumer），dubbo的版本，时间戳以及进程号放入到map中
            Map<String, String> map = new HashMap<String, String>();
            Map<Object, Object> attributes = new HashMap<Object, Object>();
            map.put(Constants.SIDE_KEY, Constants.CONSUMER_SIDE);
            map.put(Constants.DUBBO_VERSION_KEY, Version.getVersion());
            map.put(Constants.TIMESTAMP_KEY, String.valueOf(System.currentTimeMillis()));
            if (ConfigUtils.getPid() > 0) {
                map.put(Constants.PID_KEY, String.valueOf(ConfigUtils.getPid()));
            }

            //非泛化服务
            if (!isGeneric()) {
                //获取版本
                String revision = Version.getVersion(interfaceClass, version);
                if (revision != null && revision.length() > 0) {
                    map.put("revision", revision);
                }
    
                //Wrapper会对interfaceClass表示的类进行包装，生成一个Wrapper$N类，这个类继承了Wrapper类，同时也实现了Wrapper类中的抽象方法。比如getMethodNames
                //以及getPropertyNames等。这里的getMethodNames返回的是interfaceClass这个被包装类中的方法名称，然后将其添加到map对象中
                String[] methods = Wrapper.getWrapper(interfaceClass).getMethodNames();
                if (methods.length == 0) {
                    logger.warn("NO method found in service interface " + interfaceClass.getName());
                    map.put("methods", Constants.ANY_VALUE);
                } else {
                    map.put("methods", StringUtils.join(new HashSet<String>(Arrays.asList(methods)), ","));
                }
            }

            //Constants.INTERFACE_KEY的值为interface，将 interface -> interfaceName 键值对保存到map中
            //interfaceName为<dubbo:reference/>中interface属性的值，比如com.dubbo.simple.common.DemoService
            map.put(Constants.INTERFACE_KEY, interfaceName);

            //将ApplicationConfig、ModuleConfig、ConsumerConfig以及ReferenceConfig中的配置信息放入到map中
            appendParameters(map, application);
            appendParameters(map, module);
            appendParameters(map, consumer, Constants.DEFAULT_KEY);
            appendParameters(map, this);

            //获取到map中的服务名，也就是 group/interfaceName:version
            String prifix = StringUtils.getServiceKey(map);
            
            // -------------------------------✨ 分割线4 ✨------------------------------

            if (methods != null && methods.size() > 0) {
                //遍历 MethodConfig 列表
                for (MethodConfig method : methods) {
                    //将MethodConfig对象中的配置信息放入到map中。一个MethodConfig对象对应于一个<dubbo:method/>标签
                    appendParameters(map, method, method.getName());
                    String retryKey = method.getName() + ".retry";
                    //检测 map 是否包含 methodName.retry，如果有的话，从map中删除，如果retry配置的是false的话，取而代之使用retries属性（值为0），
                    //如果配置的是true的话，则删除后不再进行配置
                    if (map.containsKey(retryKey)) {
                        String retryValue = map.remove(retryKey);
                        if ("false".equals(retryValue)) {
                            map.put(method.getName() + ".retries", "0");
                        }
                    }

                    //添加 MethodConfig 中的“属性”字段到 attributes。比如 onreturn、onthrow、oninvoke 等。这3个属性为attribute属性，不在URL中体现
                    appendAttributes(attributes, method, prifix + "." + method.getName());
                    checkAndConvertImplicitConfig(method, map, attributes);
                }
            }
    
            // -------------------------------✨ 分割线5 ✨------------------------------

            //获取到服务消费者ip地址
            String hostToRegistry = ConfigUtils.getSystemProperty(Constants.DUBBO_IP_TO_REGISTRY);
            if (hostToRegistry == null || hostToRegistry.length() == 0) {
                hostToRegistry = NetUtils.getLocalHost();
            } else if (isInvalidLocalHost(hostToRegistry)) {
                throw new IllegalArgumentException("Specified invalid registry ip from property:" + Constants.DUBBO_IP_TO_REGISTRY + ", value:" + hostToRegistry);
            }
            map.put(Constants.REGISTER_IP_KEY, hostToRegistry);
    
            //存储 attributes 到系统上下文中
            StaticContext.getSystemContext().putAll(attributes);

            //创建代理类
            ref = createProxy(map);

            //根据服务名，ReferenceConfig，代理类构建 ConsumerModel，并将 ConsumerModel 存入到 ApplicationModel 中
            ConsumerModel consumerModel = new ConsumerModel(getUniqueServiceName(), this, ref, interfaceClass.getMethods());
            ApplicationModel.initConsumerModel(getUniqueServiceName(), consumerModel);
        }


        /**
         * 1.根据配置检查是否为本地调用
         * 2.若是，调用 InjvmProtocol 的 refer 方法生成 InjvmInvoker 实例
         * 3.若不是
         *      3.1.读取直连配置项，并且将url存储到urls中
         *      3.2.若不是，读取注册中心url，并且将url存储到urls中
         *      3.3.若 urls 元素数量为1，则直接通过 Protocol 自适应拓展类构建 Invoker 实例接口
         *      3.4.若 urls 元素数量大于1，即存在多个注册中心或服务直连 url，此时先根据每个 url 构建 Invoker。然后再通过 Cluster 合并多个 Invoker，最后调用 ProxyFactory 生成代理类
         */
        private T createProxy(Map<String, String> map) {
            URL tmpUrl = new URL("temp", "localhost", 0, map);
            final boolean isJvmRefer;
            if (isInjvm() == null) {
                //url 配置被指定，则不做本地引用
                if (url != null && url.length() > 0) { 
                    isJvmRefer = false;
                //根据 url 的协议、scope 以及 injvm 等参数检测是否需要本地引用, 比如如果用户显式配置了 scope=local，此时 isInjvmRefer 返回 true
                } else if (InjvmProtocol.getInjvmProtocol().isInjvmRefer(tmpUrl)) {
                    // by default, reference local service if there is
                    isJvmRefer = true;
                } else {
                    isJvmRefer = false;
                }
            } else {
                isJvmRefer = isInjvm().booleanValue();
            }
    
            //本地引用
            if (isJvmRefer) {
                //生成本地引用 URL，协议为 injvm
                URL url = new URL(Constants.LOCAL_PROTOCOL, NetUtils.LOCALHOST, 0, interfaceClass.getName()).addParameters(map);
                //调用 refer 方法构建 InjvmInvoker 实例
                invoker = refprotocol.refer(interfaceClass, url);
                if (logger.isInfoEnabled()) {
                    logger.info("Using injvm service " + interfaceClass.getName());
                }
            
            //远程引用
            } else {
                //url 不为空，表明用户可能想进行点对点调用
                if (url != null && url.length() > 0) { 
                    //当需要配置多个 url 时，可用分号进行分割，这里会进行切分
                    String[] us = Constants.SEMICOLON_SPLIT_PATTERN.split(url);
                    if (us != null && us.length > 0) {
                        for (String u : us) {
                            URL url = URL.valueOf(u);
                            if (url.getPath() == null || url.getPath().length() == 0) {
                                url = url.setPath(interfaceName);
                            }
                            if (Constants.REGISTRY_PROTOCOL.equals(url.getProtocol())) {
                                urls.add(url.addParameterAndEncoded(Constants.REFER_KEY, StringUtils.toQueryString(map)));
                            } else {
                                urls.add(ClusterUtils.mergeUrl(url, map));
                            }
                        }
                    }
                
                } else { // assemble URL from register center's configuration

                    //加载注册中心 url
                    List<URL> us = loadRegistries(false);
                    if (us != null && us.size() > 0) {
                        for (URL u : us) {
                            URL monitorUrl = loadMonitor(u);
                            if (monitorUrl != null) {
                                map.put(Constants.MONITOR_KEY, URL.encode(monitorUrl.toFullString()));
                            }
                            //添加 refer 参数到 url 中，并将 url 添加到 urls 中
                            urls.add(u.addParameterAndEncoded(Constants.REFER_KEY, StringUtils.toQueryString(map)));
                        }
                    }

                    //未配置注册中心，抛出异常
                    if (urls == null || urls.size() == 0) {
                        throw new IllegalStateException("No such any registry to reference " + interfaceName + " on the consumer " + NetUtils.getLocalHost() + " use dubbo version " + Version.getVersion() + ", please config <dubbo:registry address=\"...\" /> to your spring config.");
                    }
                }
    
                //单个注册中心或者单个服务提供者
                if (urls.size() == 1) {
                    //调用 RegistryProtocol 的 refer 构建 Invoker 实例
                    invoker = refprotocol.refer(interfaceClass, urls.get(0));

                //多个注册中心或者多个服务提供者
                } else {
                    List<Invoker<?>> invokers = new ArrayList<Invoker<?>>();
                    URL registryURL = null;

                    //获取所有的 Invoker
                    for (URL url : urls) {
                        //通过 refprotocol 调用 refer 构建 Invoker，refprotocol 会在运行时根据 url 协议头加载指定的 Protocol 实例，并调用实例的 refer 方法。
                        //这里一般是调用RegistryProtocol的refer方法
                        invokers.add(refprotocol.refer(interfaceClass, url));
                        if (Constants.REGISTRY_PROTOCOL.equals(url.getProtocol())) {
                            registryURL = url; // use last registry url
                        }
                    }

                    if (registryURL != null) { 
                        //如果注册中心链接不为空，则将使用 AvailableCluster
                        URL u = registryURL.addParameter(Constants.CLUSTER_KEY, AvailableCluster.NAME);
                        //创建 StaticDirectory 实例，并由 Cluster 对多个 Invoker 进行合并
                        invoker = cluster.join(new StaticDirectory(u, invokers));
                    } else { // not a registry url
                        invoker = cluster.join(new StaticDirectory(invokers));
                    }
                }
            }
    
            Boolean c = check;
            if (c == null && consumer != null) {
                c = consumer.isCheck();
            }
            if (c == null) {
                c = true; // default true
            }

            //invoker 可用性检查
            if (c && !invoker.isAvailable()) {
                throw new IllegalStateException("Failed to check the status of the service " + interfaceName + ". No provider available for the service " + 
                (group == null ? "" : group + "/") + interfaceName + (version == null ? "" : ":" + version) + " from the url " + invoker.getUrl() + " to the consumer " + 
                NetUtils.getLocalHost() + " use dubbo version " + Version.getVersion());
            }
            if (logger.isInfoEnabled()) {
                logger.info("Refer dubbo service " + interfaceClass.getName() + " from url " + invoker.getUrl());
            }
            
            //生成代理类，proxyFactory的类型为ProxyFactory$Adaptive，调用ProxyFactory$Adaptive的getProxy方法时，会根据invoker中的url的proxy参数值，来获取对应的扩展
            //然后调用对应扩展的getProxy方法。由于在获取扩展的时候可能还有对扩展进行Wrapper包装，所以实际返回的可能是一个Wrapper包装类对象，在这里就是
            //StubProxyFactoryWrapper类对象。
            return (T) proxyFactory.getProxy(invoker);
        }

    }

    @SPI("javassist")
    public interface ProxyFactory {
        
        @Adaptive({Constants.PROXY_KEY})
        <T> T getProxy(Invoker<T> invoker) throws RpcException;
        
        @Adaptive({Constants.PROXY_KEY})
        <T> Invoker<T> getInvoker(T proxy, Class<T> type, URL url) throws RpcException;

    }

    public class RegistryProtocol implements Protocol{

        public <T> Invoker<T> refer(Class<T> type, URL url) throws RpcException {
            //取 registry 参数值，并将其设置为协议头，比如url中配置了registry=zookeeper，那么就将zookeeper设置为url的协议，
            //然后把registry=zookeeper从url中移除掉
            url = url.setProtocol(url.getParameter(Constants.REGISTRY_KEY, Constants.DEFAULT_REGISTRY)).removeParameter(Constants.REGISTRY_KEY);

            //registryFactory为RegistryFactory$Adaptive，所以这里会根据url中的protocol，也就是url中协议的类型来调用对应RegistryFactory对象
            //的getRegistry方法。这里则是调用ZookeeperRegistryFactory对象的getRegistry方法，返回一个ZookeeperRegistry对象
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
                if ((Constants.COMMA_SPLIT_PATTERN.split(group)).length > 1
                        || "*".equals(group)) {
                    return doRefer(getMergeableCluster(), registry, type, url);
                }
            }

            // 调用 doRefer 继续执行服务引用逻辑
            return doRefer(cluster, registry, type, url);
        }

        /**
         * doRefer 方法创建一个 RegistryDirectory 实例，然后生成服务者消费者链接，并向注册中心进行注册。注册完毕后，紧接着订阅 providers、configurators、routers 等节点下的数据。
         * 完成订阅后，RegistryDirectory 会收到这几个节点下的子节点信息。由于一个服务可能部署在多台服务器上，这样就会在 providers 产生多个节点，这个时候就需要 Cluster 将多个服务节点合并为一个，
         * 并生成一个 Invoker。
         */
        private <T> Invoker<T> doRefer(Cluster cluster, Registry registry, Class<T> type, URL url) {
            //创建 RegistryDirectory 实例
            RegistryDirectory<T> directory = new RegistryDirectory<T>(type, url);

            //设置注册中心和协议
            directory.setRegistry(registry);
            directory.setProtocol(protocol);
            // all attributes of REFER_KEY
            Map<String, String> parameters = new HashMap<String, String>(directory.getUrl().getParameters());

            //生成服务消费者链接
            URL subscribeUrl = new URL(Constants.CONSUMER_PROTOCOL, parameters.remove(Constants.REGISTER_IP_KEY), 0, type.getName(), parameters);

            //注册服务消费者，在 consumers 目录下新节点
            if (!Constants.ANY_VALUE.equals(url.getServiceInterface())
                    && url.getParameter(Constants.REGISTER_KEY, true)) {
                registry.register(subscribeUrl.addParameters(Constants.CATEGORY_KEY, Constants.CONSUMERS_CATEGORY,
                        Constants.CHECK_KEY, String.valueOf(false)));
            }

            //订阅 providers、configurators、routers 等节点数据
            directory.subscribe(subscribeUrl.addParameter(Constants.CATEGORY_KEY,
                    Constants.PROVIDERS_CATEGORY
                            + "," + Constants.CONFIGURATORS_CATEGORY
                            + "," + Constants.ROUTERS_CATEGORY));
    
            //一个注册中心可能有多个服务提供者，因此这里需要将多个服务提供者合并为一个
            Invoker invoker = cluster.join(directory);
            ProviderConsumerRegTable.registerConsuemr(invoker, url, subscribeUrl, directory);
            return invoker;
        }

    }

    public class StubProxyFactoryWrapper implements ProxyFactory {

        private final ProxyFactory proxyFactory;

        public StubProxyFactoryWrapper(ProxyFactory proxyFactory) {
            this.proxyFactory = proxyFactory;
        }

        public <T> T getProxy(Invoker<T> invoker) throws RpcException {
            //这里的proxyFactory为在createExtension时传入的真正拓展类对象，这里也就是JavassistProxyFactory类对象，调用会进入到
            //JavassistProxyFactory类的父类AbstractProxyFactory中去
            T proxy = proxyFactory.getProxy(invoker);
            if (GenericService.class != invoker.getInterface()) {
                //代码省略
            }
            return proxy;
        }

    }

    public abstract class AbstractProxyFactory implements ProxyFactory {

        public <T> T getProxy(Invoker<T> invoker) throws RpcException {
            Class<?>[] interfaces = null;
            String config = invoker.getUrl().getParameter("interfaces");
            if (config != null && config.length() > 0) {
                String[] types = Constants.COMMA_SPLIT_PATTERN.split(config);
                if (types != null && types.length > 0) {
                    interfaces = new Class<?>[types.length + 2];
                    interfaces[0] = invoker.getInterface();
                    interfaces[1] = EchoService.class;
                    for (int i = 0; i < types.length; i++) {
                        interfaces[i + 1] = ReflectUtils.forName(types[i]);
                    }
                }
            }

            //获取invoker对应的interface接口，并且再添加一个EchoService类型的接口
            if (interfaces == null) {
                interfaces = new Class<?>[]{invoker.getInterface(), EchoService.class};
            }
            return getProxy(invoker, interfaces);
        }
    
        public abstract <T> T getProxy(Invoker<T> invoker, Class<?>[] types);
    
    }

    public class JavassistProxyFactory extends AbstractProxyFactory {

        @SuppressWarnings("unchecked")
        public <T> T getProxy(Invoker<T> invoker, Class<?>[] interfaces) {
            // 生成 Proxy 子类（Proxy 是抽象类）。并调用 Proxy 子类的 newInstance 方法创建 Proxy 实例
            return (T) Proxy.getProxy(interfaces).newInstance(new InvokerInvocationHandler(invoker));
        }
    
    }

    public static abstract class Proxy{

        private static final AtomicLong PROXY_CLASS_COUNTER = new AtomicLong(0);

        private static final Map<ClassLoader, Map<String, Object>> ProxyCacheMap = new WeakHashMap<ClassLoader, Map<String, Object>>();

        private static final Object PendingGenerationMarker = new Object();

        public static Proxy getProxy(Class<?>... ics) {
            return getProxy(ClassHelper.getClassLoader(Proxy.class), ics);
        }

        public static Proxy getProxy(ClassLoader cl, Class<?>... ics) {
            if (ics.length > 65535)
                throw new IllegalArgumentException("interface limit exceeded");
    
            StringBuilder sb = new StringBuilder();
            //遍历接口列表
            for (int i = 0; i < ics.length; i++) {
                String itf = ics[i].getName();
                //检测类型是否为接口
                if (!ics[i].isInterface())
                    throw new RuntimeException(itf + " is not a interface.");
    
                Class<?> tmp = null;
                try {
                    //重新加载接口类
                    tmp = Class.forName(itf, false, cl);
                } catch (ClassNotFoundException e) {
                }
                //检测接口是否相同，这里 tmp 有可能为空
                if (tmp != ics[i])
                    throw new IllegalArgumentException(ics[i] + " is not visible from class loader");
                //拼接接口全限定名，分隔符为 ;
                sb.append(itf).append(';');
            }
    
            // use interface class name list as key.
            String key = sb.toString();
    
            // get cache by class loader.
            Map<String, Object> cache;
            synchronized (ProxyCacheMap) {
                cache = ProxyCacheMap.get(cl);
                if (cache == null) {
                    cache = new HashMap<String, Object>();
                    ProxyCacheMap.put(cl, cache);
                }
            }
    
            Proxy proxy = null;
            synchronized (cache) {
                do {
                    //从缓存中获取 Reference<Proxy> 实例
                    Object value = cache.get(key);
                    if (value instanceof Reference<?>) {
                        proxy = (Proxy) ((Reference<?>) value).get();
                        if (proxy != null)
                            return proxy;
                    }
                    //并发控制，保证只有一个线程可以进行后续操作
                    if (value == PendingGenerationMarker) {
                        try {
                            //其他线程在此处进行等待
                            cache.wait();
                        } catch (InterruptedException e) {
                        }
                    } else {
                        //放置标志位到缓存中，然后跳出 while 循环进行后续操作
                        cache.put(key, PendingGenerationMarker);
                        break;
                    }
                }
                while (true);
            }
    
            // -------------------------------✨ 分割线1 ✨------------------------------
            //下面的代码用于构建接口代理类

            long id = PROXY_CLASS_COUNTER.getAndIncrement();
            String pkg = null;
            ClassGenerator ccp = null, ccm = null;
            try {
                // 创建 ClassGenerator 对象
                ccp = ClassGenerator.newInstance(cl);
    
                Set<String> worked = new HashSet<String>();
                List<Method> methods = new ArrayList<Method>();
    
                for (int i = 0; i < ics.length; i++) {
                    //检测接口访问级别是否为 protected 或 privete
                    if (!Modifier.isPublic(ics[i].getModifiers())) {
                        //省略代码
                    }
                    //添加接口到 ClassGenerator 中
                    ccp.addInterface(ics[i]);
    
                    // 遍历接口方法
                    for (Method method : ics[i].getMethods()) {
                        // 获取方法描述，可理解为方法签名
                        String desc = ReflectUtils.getDesc(method);
                        // 如果方法描述字符串已在 worked 中，则忽略。考虑这种情况，A 接口和 B 接口中包含一个完全相同的方法
                        if (worked.contains(desc))
                            continue;
                        worked.add(desc);
    
                        int ix = methods.size();
                        // 获取方法返回值类型
                        Class<?> rt = method.getReturnType();
                        // 获取参数列表
                        Class<?>[] pts = method.getParameterTypes();
    
                        // 生成 Object[] args = new Object[1...N]，method所代表的方法中有多少个参数，就创建包含对应多少元素的args数组
                        StringBuilder code = new StringBuilder("Object[] args = new Object[").append(pts.length).append("];");
                        for (int j = 0; j < pts.length; j++)
                            // 生成 args[1...N] = ($w)$1...N;也就是将方法实际调用的各个参数赋值给args数组的各元素
                            code.append(" args[").append(j).append("] = ($w)$").append(j + 1).append(";");
                        // 生成 InvokerHandler 接口的 invoker 方法调用语句，如下：
                        // Object ret = handler.invoke(this, methods[1...N], args);
                        code.append(" Object ret = handler.invoke(this, methods[" + ix + "], args);");

                        // 返回值不为 void的话，就生成返回语句，形如 return (java.lang.String) ret;
                        if (!Void.TYPE.equals(rt))
                            code.append(" return ").append(asArgument(rt, "ret")).append(";");
    
                        methods.add(method);
                        // 添加方法名、访问控制符、参数列表、方法代码等信息到 ClassGenerator 中 
                        ccp.addMethod(method.getName(), method.getModifiers(), rt, pts, method.getExceptionTypes(), code.toString());
                    }
                }
    
                if (pkg == null)
                    pkg = PACKAGE_NAME;
    
                // 构建接口代理类名称：pkg + ".proxy" + id，比如 org.apache.dubbo.proxy0
                String pcn = pkg + ".proxy" + id;
                ccp.setClassName(pcn);
                ccp.addField("public static java.lang.reflect.Method[] methods;");
                // 生成 private java.lang.reflect.InvocationHandler handler;
                ccp.addField("private " + InvocationHandler.class.getName() + " handler;");

                // 为接口代理类添加带有 InvocationHandler 参数的构造方法，比如：
                // porxy0(java.lang.reflect.InvocationHandler arg0) {
                //     handler=$1;
    	        // }
                ccp.addConstructor(Modifier.PUBLIC, new Class<?>[]{InvocationHandler.class}, new Class<?>[0], "handler=$1;");
                ccp.addDefaultConstructor();

                // 生成接口代理类
                Class<?> clazz = ccp.toClass();
                clazz.getField("methods").set(null, methods.toArray(new Method[0]));
    
                // -------------------------------✨ 分割线2 ✨------------------------------
                //下面的代码用于构建Proxy抽象类的子类
                //这里要注意区分ccp和ccm的区别，ccp 用于为服务接口生成代理类，比如我们有一个 DemoService 接口，这个接口代理类就是由 ccp 生成的。ccm 则是用于为 org.apache.dubbo.common.bytecode.Proxy 抽象类生成子类，
                //主要是实现 Proxy 类的抽象方法 public Object newInstance(InvocationHandler handler);

                // 构建 Proxy 子类名称，比如 Proxy1，Proxy2 等
                String fcn = Proxy.class.getName() + id;
                ccm = ClassGenerator.newInstance(cl);
                ccm.setClassName(fcn);
                ccm.addDefaultConstructor();
                ccm.setSuperClass(Proxy.class);
                // 为 Proxy 的抽象方法 newInstance 生成实现代码，形如：
                // public Object newInstance(java.lang.reflect.InvocationHandler h) { 
                //     return new org.apache.dubbo.proxy0($1);
                // }
                ccm.addMethod("public Object newInstance(" + InvocationHandler.class.getName() + " h){ return new " + pcn + "($1); }");
                // 生成 Proxy 实现类
                Class<?> pc = ccm.toClass();
                // 通过反射创建 Proxy 实例
                proxy = (Proxy) pc.newInstance();
            } catch (RuntimeException e) {
                throw e;
            } catch (Exception e) {
                throw new RuntimeException(e.getMessage(), e);
            } finally {
                // release ClassGenerator
                if (ccp != null)
                    ccp.release();
                if (ccm != null)
                    ccm.release();
                synchronized (cache) {
                    if (proxy == null)
                        cache.remove(key);
                    else
                        cache.put(key, new WeakReference<Proxy>(proxy));
                    // 唤醒其他等待线程
                    cache.notifyAll();
                }
            }
            return proxy;
        }

    }


    public class InvokerInvocationHandler implements InvocationHandler {

        private final Invoker<?> invoker;
    
        public InvokerInvocationHandler(Invoker<?> invoker) {
            this.invoker = invoker;
        }
    
        public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
            String methodName = method.getName();
            
            //省略代码

            return invoker.invoke(new RpcInvocation(method, args)).recreate();
        }
    
    }


    public class Proxy0 extends com.alibaba.dubbo.common.bytecode.Proxy{

        public Object newInstance(java.lang.reflect.InvocationHandler h) { 
            return new org.apache.dubbo.proxy0($1);
        }

    }

    public class proxy0 implements org.apache.dubbo.demo.DemoService {
        
        public static java.lang.reflect.Method[] methods;
        private java.lang.reflect.InvocationHandler handler;

        public proxy0() {
        }

        public proxy0(java.lang.reflect.InvocationHandler arg0) {
            handler = $1;
        }

        public java.lang.String sayHello(java.lang.String arg0) {
            Object[] args = new Object[1];
            args[0] = ($w) $1;
            Object ret = handler.invoke(this, methods[0], args);
            return (java.lang.String) ret;
        }
    }




}