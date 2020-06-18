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

            //Constants.INTERFACE_KEY的值为interface，将interface -> interfaceName 键值对保存到map中
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
    
                //单个注册中心
                if (urls.size() == 1) {
                    //调用 RegistryProtocol 的 refer 构建 Invoker 实例
                    invoker = refprotocol.refer(interfaceClass, urls.get(0));

                //多个注册中心
                } else {
                    List<Invoker<?>> invokers = new ArrayList<Invoker<?>>();
                    URL registryURL = null;

                    //获取所有的 Invoker
                    for (URL url : urls) {
                        // 通过 refprotocol 调用 refer 构建 Invoker，refprotocol 会在运行时根据 url 协议头加载指定的 Protocol 实例，并调用实例的 refer 方法
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
                throw new IllegalStateException("Failed to check the status of the service " + interfaceName + ". No provider available for the service " + (group == null ? "" : group + "/") + interfaceName + (version == null ? "" : ":" + version) + " from the url " + invoker.getUrl() + " to the consumer " + NetUtils.getLocalHost() + " use dubbo version " + Version.getVersion());
            }
            if (logger.isInfoEnabled()) {
                logger.info("Refer dubbo service " + interfaceClass.getName() + " from url " + invoker.getUrl());
            }
            
            //生成代理类
            return (T) proxyFactory.getProxy(invoker);
        }

    }


    


}