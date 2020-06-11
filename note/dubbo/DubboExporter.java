public class DubboExporter{

    /**
     * [dubbo://169.254.207.250:20880/com.xu.gmall.service.UserService?anyhost=true&application=user-service-provider&dubbo=2.6.2&
     * generic=false&interface=com.xu.gmall.service.UserService&methods=getUserAddressList&pid=1308&side=provider&timestamp=1591591491230]
     */ 
    //我们配置的每一个<dubbo:service/>标签都会被解析成一个ServiceBean对象
    class ServiceBean<T> extends ServiceConfig<T> implements InitializingBean, DisposableBean, 
                            ApplicationContextAware, ApplicationListener<ContextRefreshedEvent>, BeanNameAware {

        public void onApplicationEvent(ContextRefreshedEvent event) {
            //是否有延迟导出 && 是否已导出 && 是不是已被取消导出
            if (isDelay() && !isExported() && !isUnexported()) {
                /**
                 * export方法会进行服务导出操作，服务导出的具体流程如下：
                 * 1.对配置进行检查
                 * 2.生成URL
                 * 3.导出服务到本地/导出服务到远程
                 * 4.向注册中心注册服务
                 */
                export();
            }
        }

    }

    class ServiceConfig<T> extends AbstractServiceConfig{

        private final List<URL> urls = new ArrayList<URL>();

        public synchronized void export() {
            //获取 export 和 delay 配置
            if (provider != null) {
                if (export == null) {
                    export = provider.getExport();
                }
                if (delay == null) {
                    delay = provider.getDelay();
                }
            }

            //如果 export 为 false，则不导出服务
            //export配置决定了是否导出服务。有时候我们只是想本地启动服务进行一些调试工作，这个时候我们并不希望把本地启动的服务暴露出去给别人调用。
            //此时，我们就可以通过配置 export 禁止服务导出，如下所示：
            //<dubbo:provider export="false" />
            if (export != null && !export) {
                return;
            }
    
            //delay > 0，延时导出服务
            //delay属性在<dubbo:service/>标签中被配置，延迟注册服务时间(毫秒) ，设为-1时，表示延迟到Spring容器初始化完成时暴露服务
            if (delay != null && delay > 0) {
                //延时指定毫秒导出服务
                delayExportExecutor.schedule(new Runnable() {
                    @Override
                    public void run() {
                        doExport();
                    }
                }, delay, TimeUnit.MILLISECONDS);
            } else {
                //立即导出服务
                doExport();
            }
        }

        protected synchronized void doExport() {
            //如果服务已经被取消导出了，就直接抛出异常
            if (unexported) {
                throw new IllegalStateException("Already unexported!");
            }
            //如果服务已经被导出过了，就直接返回，没有必要再进行导出
            if (exported) {
                return;
            }
            exported = true;

            //检测 interfaceName 是否合法
            if (interfaceName == null || interfaceName.length() == 0) {
                throw new IllegalStateException("<dubbo:service interface=\"\" /> interface not allow null!");
            }
            //检测 provider 是否为空，为空则新建一个，并通过系统变量为其初始化
            checkDefault();

            //下面几个 if 语句用于检测 provider、application 等核心配置类对象是否为空，
            // 若为空，则尝试从其他配置类对象中获取相应的实例。
            if (provider != null) {
                if (application == null) {
                    application = provider.getApplication();
                }
                if (module == null) {
                    module = provider.getModule();
                }
                if (registries == null) {
                    registries = provider.getRegistries();
                }
                if (monitor == null) {
                    monitor = provider.getMonitor();
                }
                if (protocols == null) {
                    protocols = provider.getProtocols();
                }
            }
            if (module != null) {
                //省略代码
            }
            if (application != null) {
                //省略代码
            }

            //检测 ref 是否泛化服务类型
            if (ref instanceof GenericService) {
                //省略代码
            } else {
                try {
                    //这里获得的就是接口名所指定的接口类对象，比如HelloService.class
                    interfaceClass = Class.forName(interfaceName, true, Thread.currentThread()
                            .getContextClassLoader());
                } catch (ClassNotFoundException e) {
                    throw new IllegalStateException(e.getMessage(), e);
                }
                //对 interfaceClass，以及 <dubbo:method> 必要字段进行检查
                //检查interfaceClass是否为null，以及是否为一个接口；另外检查methods中的每一个method是否真的存在于interfaceClass
                //所表明的接口中
                checkInterfaceAndMethods(interfaceClass, methods);
                //对 ref 合法性进行检测，也就是ref是否为null，ref表示的对象是否实现了interfaceClass指明的接口
                checkRef();
                //设置 generic = "false"
                generic = Boolean.FALSE.toString();
            }

            //local参数和stub参数等价，但是已经被废弃了
            if (local != null) {
                //省略代码
            }

            //stub属性用来配置dubbo的存根属性，stub属性有两种配置方式：
            //1.stub="true"：指定 stub="true" 来告诉 Dubbo 框架使用本地存根，并且使用约定的方式，这个时候，本地存根的包名需要和服务接口的包名一致，
            //类名必须在服务接口的类名后加上 Stub 的后缀
            //2.stub="存根的全类名"：不使用约定的方式，直接指定我们定义的存根的全类名
            if (stub != null) {
                if ("true".equals(stub)) {
                    stub = interfaceName + "Stub";
                }
                Class<?> stubClass;
                try {
                    stubClass = ClassHelper.forNameWithThreadContextClassLoader(stub);
                } catch (ClassNotFoundException e) {
                    throw new IllegalStateException(e.getMessage(), e);
                }
                //存根必须要实现标签中interface所指定的接口
                if (!interfaceClass.isAssignableFrom(stubClass)) {
                    throw new IllegalStateException("The stub implementation class " + stubClass.getName() + " not implement interface " + interfaceName);
                }
            }

            //检测各种对象是否为空，为空则新建，或者抛出异常
            checkApplication();
            checkRegistry();
            checkProtocol();
            appendProperties(this);
            checkStubAndMock(interfaceClass);

            //path为服务路径，默认为接口名
            if (path == null || path.length() == 0) {
                path = interfaceName;
            }

            //导出服务
            doExportUrls();

            //ProviderModel 表示服务提供者模型，此对象中存储了和服务提供者相关的信息。比如服务的配置信息，服务实例等。
            //每个被导出的服务对应一个 ProviderModel。ApplicationModel 持有所有的 ProviderModel。
            ProviderModel providerModel = new ProviderModel(getUniqueServiceName(), this, ref);
            ApplicationModel.initProviderModel(getUniqueServiceName(), providerModel);
        }

        /**
         * <dubbo:registry/>用来配置注册中心，address属性为注册中心服务器地址，如果地址没有端口缺省为9090。同一集群内的多个地址用逗号分隔，
         * 如：ip:port,ip:port，不同集群的注册中心，请配置多个<dubbo:registry>标签。<dubbo:registry/>标签中还有两个属性：
         * register:是否向此注册中心注册服务，如果设为false，将只订阅，不注册，默认为true。
         * subscribe:是否向此注册中心订阅服务，如果设为false，将只注册，不订阅，默认为true。
         * <dubbo:service/>中registry标签如果没有配置的话，默认向所有registry注册。如果配置了的话，向指定注册中心注册，在多个注册中心时使用，
         * 值为<dubbo:registry>的id属性，多个注册中心ID用逗号分隔，如果不想将该服务注册到任何registry，可将值设为N/A
         * <dubbo:service/>中protocol标签使用指定的协议暴露服务，在多协议时使用，值为<dubbo:protocol>的id属性，多个协议ID用逗号分隔
         * 
         * *************************************************总结******************************************
         * 根据用户在每一个<dubbo:service/>标签中配置的注册中心的个数，依据<dubbo:registry/>、<dubbo:application/>等标签中的
         * 属性，组装好所有注册中心的URL。然后将<dubbo:service/>中的服务注册到这些注册中心上（不同的协议要注册不同的次数）
         */
        private void doExportUrls() {

            //首先是通过 loadRegistries 加载注册中心链接，用户可能在<dubbo:service/>中指定了多个注册中心。所以，获得的registryURLs可能有多个，
            //要把此服务注册到这些注册中心上去。
            List<URL> registryURLs = loadRegistries(true);

            //同理，用户可能在<dubbo:service/>中指定了使用多个协议，
            //因此，遍历protocols集合根据不同的协议导出每个服务，并且将服务注册到注册中心
            for (ProtocolConfig protocolConfig : protocols) {
                doExportUrlsFor1Protocol(protocolConfig, registryURLs);
            }
        }

        private void doExportUrlsFor1Protocol(ProtocolConfig protocolConfig, List<URL> registryURLs) {
            String name = protocolConfig.getName();

            //如果协议为空，或者说空字符串，那么将协议名变量设置为dubbo，默认协议为dubbo
            if (name == null || name.length() == 0) {
                name = "dubbo";
            }
    
            Map<String, String> map = new HashMap<String, String>();
            //添加 side（是提供端provider还是消费端consumer）、版本、时间戳以及进程号等信息到 map 中
            map.put(Constants.SIDE_KEY, Constants.PROVIDER_SIDE);
            map.put(Constants.DUBBO_VERSION_KEY, Version.getVersion());
            map.put(Constants.TIMESTAMP_KEY, String.valueOf(System.currentTimeMillis()));
            if (ConfigUtils.getPid() > 0) {
                map.put(Constants.PID_KEY, String.valueOf(ConfigUtils.getPid()));
            }

            //通过反射将对象的信息添加到map中，包括ApplicationConfig、ModuleConfig、ProviderConfig以及
            //ProtocolConfig和ServiceConfig对象中的配置信息放到map中，用来组装url
            appendParameters(map, application);
            appendParameters(map, module);
            appendParameters(map, provider, Constants.DEFAULT_KEY);
            appendParameters(map, protocolConfig);
            appendParameters(map, this);

            //methods 为 MethodConfig 集合，MethodConfig 中存储了 <dubbo:method> 标签的配置信息。
            //<dubbo:method/>标签为<dubbo:service/>或者<dubbo:reference/>标签的子标签，用于控制到方法级。
            //<dubbo:argument/>方法参数配置，标签为 <dubbo:method> 的子标签，用于方法参数的特征描述。
            //<dubbo:method name="findXxx" timeout="3000" retries="2">
            //    <dubbo:argument index="0" callback="true" />
            //</dubbo:method>
            //其中callback的作用是:如果为callback，服务提供方将生成反向代理，可以从服务提供方反向调用消费方，通常用于事件推送。
            //Callback 接口的实现类在 Consumer 端，当方法发生调用时，Consumer 端会自动 export 一个 Callback 服务。而 Provider 端在处理调用时，
            //判断如果参数是 Callback，则生成了一个 proxy，因此服务实现类里在调用 Callback 方法的时候，会被传递到 Consumer 端执行 Callback 实现类的代码。
            /**
             * 获取 ArgumentConfig 列表
               for (遍历 ArgumentConfig 列表) {
                    if (type 不为 null，也不为空串) {    // 分支1
                    1. 通过反射获取 interfaceClass 的方法列表
                        for (遍历方法列表) {
                            1. 比对方法名，查找目标方法
                            2. 通过反射获取目标方法的参数类型数组 argtypes
                            if (index != -1) {    // 分支2
                                1. 从 argtypes 数组中获取下标 index 处的元素 argType
                                2. 检测 argType 的名称与 ArgumentConfig 中的 type 属性是否一致
                                3. 添加 ArgumentConfig 字段信息到 map 中，或抛出异常
                            } else {    // 分支3
                                1. 遍历参数类型数组 argtypes，查找 argument.type 类型的参数
                                2. 添加 ArgumentConfig 字段信息到 map 中
                            }
                        }
                    } else if (index != -1) {    // 分支4
                    1. 添加 ArgumentConfig 字段信息到 map 中
                    }
                }
            */
            if (methods != null && !methods.isEmpty()) {
                for (MethodConfig method : methods) {
                    //添加 MethodConfig 对象的字段信息到 map 中，键 = 方法名.属性名。
                    //比如存储 <dubbo:method name="sayHello" retries="2"> 对应的 MethodConfig，
                    //键 = sayHello.retries，map = {"sayHello.retries": 2, "xxx": "yyy"}
                    appendParameters(map, method, method.getName());

                    //retry属性已经被废弃，由retries属性代替。
                    String retryKey = method.getName() + ".retry";
                    if (map.containsKey(retryKey)) {
                        String retryValue = map.remove(retryKey);
                        //如果配置的retry属性为false，那么就移除掉，设置新的retries属性为0
                        if ("false".equals(retryValue)) {
                            map.put(method.getName() + ".retries", "0");
                        }
                    }

                    //从methodConfig中获取ArgumentConfig对象列表
                    List<ArgumentConfig> arguments = method.getArguments();
                    if (arguments != null && !arguments.isEmpty()) {
                        for (ArgumentConfig argument : arguments) {
                            //检测 type 属性是否为空，或者空串（分支1 ⭐️）
                            if (argument.getType() != null && argument.getType().length() > 0) {
                                Method[] methods = interfaceClass.getMethods();
                                // visit all methods
                                if (methods != null && methods.length > 0) {
                                    for (int i = 0; i < methods.length; i++) {
                                        String methodName = methods[i].getName();
                                        //比对方法名，查找目标方法
                                        if (methodName.equals(method.getName())) {
                                            Class<?>[] argtypes = methods[i].getParameterTypes();
                                            // one callback in the method
                                            if (argument.getIndex() != -1) {
                                                //检测 ArgumentConfig 中的 type 属性与方法参数列表中的参数名称是否一致，不一致则抛出异常(分支2 ⭐️)
                                                if (argtypes[argument.getIndex()].getName().equals(argument.getType())) {
                                                    //添加 ArgumentConfig 字段信息到 map 中，注意，只有<dubbo:argument/>标签中，callback属性由用户配置之后（不管是true还是false）
                                                    //才会将argument字段的信息添加到map中，如果没有配置callback属性，则不会添加到map中。键前缀 = 方法名.index.callback，比如
                                                    //map = {"sayHello.3.callback": true}，下面的appendParameters同理
                                                    appendParameters(map, argument, method.getName() + "." + argument.getIndex());
                                                } else {
                                                    // 分支3 ⭐️
                                                    throw new IllegalArgumentException("argument config error : the index attribute and type attribute not match :index :" + argument.getIndex() + ", type:" + argument.getType());
                                                }
                                            } else {
                                                //multiple callbacks in the method
                                                for (int j = 0; j < argtypes.length; j++) {
                                                    Class<?> argclazz = argtypes[j];
                                                    //从参数类型列表中查找类型名称为argument.type的参数
                                                    if (argclazz.getName().equals(argument.getType())) {
                                                        appendParameters(map, argument, method.getName() + "." + j);
                                                        if (argument.getIndex() != -1 && argument.getIndex() != j) {
                                                            throw new IllegalArgumentException("argument config error : the index attribute and type attribute not match :index :" + argument.getIndex() + ", type:" + argument.getType());
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            //用户未配置 type 属性，但配置了 index 属性，且 index != -1
                            } else if (argument.getIndex() != -1) {
                                //添加 ArgumentConfig 字段信息到 map 中
                                appendParameters(map, argument, method.getName() + "." + argument.getIndex());
                            } else {
                                throw new IllegalArgumentException("argument config must set index or type attribute.eg: <dubbo:argument index='0' .../> or <dubbo:argument type=xxx .../>");
                            }
    
                        }
                    }
                } // end of methods for
            }
    
            //检测 generic 是否为 "true"，并根据检测结果向 map 中添加不同的信息
            if (ProtocolUtils.isGeneric(generic)) {
                map.put(Constants.GENERIC_KEY, generic);
                map.put(Constants.METHODS_KEY, Constants.ANY_VALUE);
            } else {
                String revision = Version.getVersion(interfaceClass, version);
                if (revision != null && revision.length() > 0) {
                    map.put("revision", revision);
                }
                
                //为接口生成包裹类 Wrapper，Wrapper 中包含了接口的详细信息，比如接口方法名数组，字段信息等。这里获取到了特定接口中方法名字数组
                String[] methods = Wrapper.getWrapper(interfaceClass).getMethodNames();
                //添加方法名到 map 中，如果包含多个方法名，则用逗号隔开，比如 method = init,destroy
                if (methods.length == 0) {
                    logger.warn("NO method found in service interface " + interfaceClass.getName());
                    map.put(Constants.METHODS_KEY, Constants.ANY_VALUE);
                } else {
                    //将逗号作为分隔符连接方法名，并将连接后的字符串放入 map 中，最终会在map中形成如下键值对：
                    //methods -> func A, func B
                    map.put(Constants.METHODS_KEY, StringUtils.join(new HashSet<String>(Arrays.asList(methods)), ","));
                }
            }
            
            //代码省略

            //获取上下文路径
            String contextPath = protocolConfig.getContextpath();
            if ((contextPath == null || contextPath.length() == 0) && provider != null) {
                contextPath = provider.getContextpath();
            }
    
            //获取到本机的ip地址和端口号
            String host = this.findConfigedHosts(protocolConfig, registryURLs, map);
            Integer port = this.findConfigedPorts(protocolConfig, name, map);

            //根据本机的ip地址和端口号，以及上面map中的各种信息，包括但不限于提供方的应用名称，即application，
            //dubbo的版本号，接口的全类名，接口中的所有方法名（如果不止一个的话，使用逗号分隔），进程id和时间戳。
            URL url = new URL(name, host, port, (contextPath == null || contextPath.length() == 0 ? "" : contextPath + "/") + path, map);
            
            String scope = url.getParameter(Constants.SCOPE_KEY);

            //下面将要进行服务导出，先导出到本地，再导出到远程
            //如果配置scope为none，则不进行导出操作，注意这与scope为null不同，如果scope为null，还是会执行导出操作
            if (!Constants.SCOPE_NONE.toString().equalsIgnoreCase(scope)) {
    
                //scope != remote，导出到本地
                if (!Constants.SCOPE_REMOTE.toString().equalsIgnoreCase(scope)) {
                    exportLocal(url);
                }

                //scope != local，导出到远程
                if (!Constants.SCOPE_LOCAL.toString().equalsIgnoreCase(scope)) {
                    if (logger.isInfoEnabled()) {
                        logger.info("Export dubbo service " + interfaceClass.getName() + " to url " + url);
                    }
                    if (registryURLs != null && !registryURLs.isEmpty()) {
                        for (URL registryURL : registryURLs) {
                            url = url.addParameterIfAbsent(Constants.DYNAMIC_KEY, registryURL.getParameter(Constants.DYNAMIC_KEY));

                            //加载监视器链接
                            URL monitorUrl = loadMonitor(registryURL);
                            if (monitorUrl != null) {
                                //将监视器链接作为参数添加到 url 中
                                url = url.addParameterAndEncoded(Constants.MONITOR_KEY, monitorUrl.toFullString());
                            }
                            if (logger.isInfoEnabled()) {
                                logger.info("Register dubbo service " + interfaceClass.getName() + " url " + url + " to registry " + registryURL);
                            }
                            //为服务提供类(ref)生成 Invoker
                            Invoker<?> invoker = proxyFactory.getInvoker(ref, (Class) interfaceClass, registryURL.addParameterAndEncoded(Constants.EXPORT_KEY, url.toFullString()));
                            //DelegateProviderMetaDataInvoker 仅用于持有 Invoker 和 ServiceConfig
                            DelegateProviderMetaDataInvoker wrapperInvoker = new DelegateProviderMetaDataInvoker(invoker, this);
    
                            //导出服务，并生成 Exporter
                            Exporter<?> exporter = protocol.export(wrapperInvoker);
                            exporters.add(exporter);
                        }
                    } else {
                        //不存在注册中心，仅导出服务
                        Invoker<?> invoker = proxyFactory.getInvoker(ref, (Class) interfaceClass, url);
                        DelegateProviderMetaDataInvoker wrapperInvoker = new DelegateProviderMetaDataInvoker(invoker, this);
    
                        Exporter<?> exporter = protocol.export(wrapperInvoker);
                        exporters.add(exporter);
                    }
                }
            }

            //将配置好的URL放入到urls列表集合中
            this.urls.add(url);
        }

        private void exportLocal(URL url) {
            //如果 URL 的协议头等于 injvm，说明已经导出到本地了，无需再次导出
            if (!Constants.LOCAL_PROTOCOL.equalsIgnoreCase(url.getProtocol())) {
                //设置协议头为 injvm，并且将原协议URL中的主机名和端口号替换为localhost和0,
                URL local = URL.valueOf(url.toFullString())
                        .setProtocol(Constants.LOCAL_PROTOCOL)
                        .setHost(LOCALHOST)
                        .setPort(0);
                ServiceClassHolder.getInstance().pushServiceClass(getServiceClass(ref));

                //创建 Invoker，并导出服务，这里的 protocol 会在运行时调用 InjvmProtocol 的 export 方法，将协议导出到本地
                Exporter<?> exporter = protocol.export(
                        proxyFactory.getInvoker(ref, (Class) interfaceClass, local));
                exporters.add(exporter);
                logger.info("Export dubbo service " + interfaceClass.getName() + " to local registry");
            }
        }
    }

    //class:InjvmProtocol
    public <T> Exporter<T> export(Invoker<T> invoker) throws RpcException {
        //exporterMap是InjvmProtocol的父类AbstractProtocol中的属性，因此export方法会创建一个
        //InjvmExporter对象，并且把这个对象添加到exporterMap中。
        return new InjvmExporter<T>(invoker, invoker.getUrl().getServiceKey(), exporterMap);
    }

    class InjvmExporter<T> extends AbstractExporter<T> {

        private final String key;
    
        private final Map<String, Exporter<?>> exporterMap;
    
        InjvmExporter(Invoker<T> invoker, String key, Map<String, Exporter<?>> exporterMap) {
            super(invoker);
            this.key = key;
            this.exporterMap = exporterMap;
            exporterMap.put(key, this);
        }
    
        @Override
        public void unexport() {
            super.unexport();
            exporterMap.remove(key);
        }
    
    }

    public class AbstractInterfaceConfig extends AbstractMethodConfig{

        /**
         * 
         * loadRegistries方法的逻辑如下：
         * 1.检测是否存在注册中心配置类，不存在则抛出异常
         * 2.构建参数映射集合，也就是 map
         * 3.根据参数映射集合以及地址address，解析出URL列表
         * 4.遍历URL列表，并根据条件决定是否将其添加到 registryList 中
         */
        protected List<URL> loadRegistries(boolean provider) {
            //检测是否存在注册中心配置类，不存在则抛出异常
            checkRegistry();
            List<URL> registryList = new ArrayList<URL>();

            //这里的registries是RegistryConfig对象的集合，RegistryConfig是<dubbo:registry/>标签配置的一个抽象表示。
            //如果我们配置了多个注册中心的话（存在多个<dubbo:registry/>标签），就会存在对应数量的RegistryConfig对象。比如：
            //<dubbo:registry address="zookeeper://127.0.0.1:2181"/>
            //<dubbo:registry address="zookeeper://127.0.0.2:2181"/>
            //那么RegistryConfig对象就会有2个，代表了上面这两个注册中心配置
            //
            //注意，也可以如下配置：
            //
            //<dubbo:registry protocol="zookeeper" address="127.0.0.2:2181,127.0.0.1:2181"/>，这样会得到一个RegistryConfig对象，并且解析得到的urls
            //数组中只有一个url，这个url为：
            //zookeeper://127.0.0.1:2181/com.alibaba.dubbo.registry.RegistryService?application=user-service-provider&backup=127.0.0.2:2181
            //&dubbo=2.6.2&pid=17376&timestamp=1591607738629
            if (registries != null && !registries.isEmpty()) {

                for (RegistryConfig config : registries) {
                    String address = config.getAddress();
                    if (address == null || address.length() == 0) {
                        //若 address 为空，则将其设为 0.0.0.0
                        address = Constants.ANYHOST_VALUE;
                    }

                    //从系统属性中加载注册中心地址
                    String sysaddress = System.getProperty("dubbo.registry.address");
                    if (sysaddress != null && sysaddress.length() > 0) {
                        address = sysaddress;
                    }

                    //判断address是否合法
                    if (address != null && address.length() > 0
                            && !RegistryConfig.NO_AVAILABLE.equalsIgnoreCase(address)) {
                        Map<String, String> map = new HashMap<String, String>();
                        //添加<dubbo:application/>标签中的信息到map中，比如name、owner以及organization属性
                        appendParameters(map, application);
                        //添加 RegistryConfig 字段信息到 map 中，比如address、register、group以及check等
                        appendParameters(map, config);
                        //添加服务路径信息到 map 中，默认和interface一致
                        map.put("path", RegistryService.class.getName());
                        //添加dubbo的版本信息到map中
                        map.put("dubbo", Version.getVersion());
                        //添加时间戳TIMESTAMP到map中
                        map.put(Constants.TIMESTAMP_KEY, String.valueOf(System.currentTimeMillis()));
                        if (ConfigUtils.getPid() > 0) {
                            map.put(Constants.PID_KEY, String.valueOf(ConfigUtils.getPid()));
                        }
                        //添加协议protocol到map中
                        if (!map.containsKey("protocol")) {
                            if (ExtensionLoader.getExtensionLoader(RegistryFactory.class).hasExtension("remote")) {
                                map.put("protocol", "remote");
                            } else {
                                map.put("protocol", "dubbo");
                            }
                        }

                        //解析得到 URL 列表
                        List<URL> urls = UrlUtils.parseURLs(address, map);
                        for (URL url : urls) {
                            //在url中添加一个registry属性，并且将registry设置为我们在<dubbo:registry/>中所配置的协议名
                            url = url.addParameter(Constants.REGISTRY_KEY, url.getProtocol());
                            //将 URL 协议头设置为 registry
                            url = url.setProtocol(Constants.REGISTRY_PROTOCOL);
                            //通过判断条件，决定是否添加 url 到 registryList 中，条件如下：
                            //(服务提供者 && register = true 或 null) 
                            //   || (非服务提供者 && subscribe = true 或 null)
                            if ((provider && url.getParameter(Constants.REGISTER_KEY, true))
                                    || (!provider && url.getParameter(Constants.SUBSCRIBE_KEY, true))) {
                                registryList.add(url);
                            }
                        }
                    }
                }

            }
            return registryList;
        }

    }


    public class JavassistProxyFactory extends AbstractProxyFactory {

        @Override
        @SuppressWarnings("unchecked")
        public <T> T getProxy(Invoker<T> invoker, Class<?>[] interfaces) {
            return (T) Proxy.getProxy(interfaces).newInstance(new InvokerInvocationHandler(invoker));
        }
    
        //proxyFactory.getInvoker(ref, (Class) interfaceClass, registryURL.addParameterAndEncoded(Constants.EXPORT_KEY, url.toFullString()))
        //registryURL.addParameterAndEncoded对url和registryURL进行了一定的拼接和重组，生成了如下的结果：

        //registry://127.0.0.1:2181/com.alibaba.dubbo.registry.RegistryService?application=user-service-provider&dubbo=2.6.2&
        //export=dubbo://169.254.207.250:20880/com.xu.gmall.service.UserService?anyhost=true&application=user-service-provider&
        //bind.ip=169.254.207.250&bind.port=20880&dubbo=2.6.2&generic=false&interface=com.xu.gmall.service.UserService&
        //methods=getUserAddressList,calc&pid=11032&side=provider&timestamp=1591630213319&pid=11032&registry=zookeeper&timestamp=1591630213288

        //url：dubbo://169.254.207.250:20880/com.xu.gmall.service.UserService?anyhost=true&application=user-service-provider&bind.ip=169.254.207.250&
        //bind.port=20880&dubbo=2.6.2&generic=false&interface=com.xu.gmall.service.UserService&methods=getUserAddressList,calc&
        //pid=11032&side=provider&timestamp=1591630213319

        //registryURL：registry://127.0.0.1:2181/com.alibaba.dubbo.registry.RegistryService?application=user-service-provider&
        //dubbo=2.6.2&pid=11032&registry=zookeeper&timestamp=1591630213288

        //proxy可以为<dubbo:service/>中interface指定接口的具体实现类
        @Override
        public <T> Invoker<T> getInvoker(T proxy, Class<T> type, URL url) {
            //为目标类创建 Wrapper
            final Wrapper wrapper = Wrapper.getWrapper(proxy.getClass().getName().indexOf('$') < 0 ? proxy.getClass() : type);
            //创建匿名 Invoker 类对象，并实现 doInvoke 方法。
            return new AbstractProxyInvoker<T>(proxy, type, url) {
                @Override
                protected Object doInvoke(T proxy, String methodName,
                                          Class<?>[] parameterTypes,
                                          Object[] arguments) throws Throwable {
                    //调用 Wrapper 的 invokeMethod 方法，invokeMethod 最终会调用目标方法
                    return wrapper.invokeMethod(proxy, methodName, parameterTypes, arguments);
                }
            };
        }
    
    }

    public class RegistryProtocol implements Protocol{

        public <T> Exporter<T> export(final Invoker<T> originInvoker) throws RpcException {
            //导出服务
            final ExporterChangeableWrapper<T> exporter = doLocalExport(originInvoker);
    
            // 获取注册中心 URL，以 zookeeper 注册中心为例，得到的示例 URL 如下：
            // zookeeper://127.0.0.1:2181/com.alibaba.dubbo.registry.RegistryService?application=demo-provider&dubbo=2.0.2&
            //export=dubbo%3A%2F%2F172.17.48.52%3A20880%2Fcom.alibaba.dubbo.demo.DemoService%3Fanyhost%3Dtrue%26application%3Ddemo-provider
            URL registryUrl = getRegistryUrl(originInvoker);
    
            //省略代码
    
            //获取 register 参数
            boolean register = registedProviderUrl.getParameter("register", true);
            //向服务提供者与消费者注册表中注册服务提供者
            ProviderConsumerRegTable.registerProvider(originInvoker, registryUrl, registedProviderUrl);
    
            //根据 register 的值决定是否注册服务
            if (register) {
                //向注册中心注册服务
                register(registryUrl, registedProviderUrl);
                ProviderConsumerRegTable.getProviderWrapper(originInvoker).setReg(true);
            }
    
            //获取订阅 URL，比如：
            //provider://172.17.48.52:20880/com.alibaba.dubbo.demo.DemoService?category=configurators&check=false&anyhost=true&
            //application=demo-provider&dubbo=2.0.2&generic=false&interface=com.alibaba.dubbo.demo.DemoService&methods=sayHello
            final URL overrideSubscribeUrl = getSubscribedOverrideUrl(registedProviderUrl);

            //创建监听器
            final OverrideListener overrideSubscribeListener = new OverrideListener(overrideSubscribeUrl, originInvoker);
            overrideListeners.put(overrideSubscribeUrl, overrideSubscribeListener);

            //向注册中心进行订阅 override 数据
            registry.subscribe(overrideSubscribeUrl, overrideSubscribeListener);
            
            //创建并返回 DestroyableExporter
            return new DestroyableExporter<T>(exporter, originInvoker, overrideSubscribeUrl, registedProviderUrl);
        }

        public void register(URL registryUrl, URL registedProviderUrl) {
            //获取 Registry，这里获取的是ZookeeperRegistry，也有可能是RedisRegistry。
            Registry registry = registryFactory.getRegistry(registryUrl);
            //调用Registry对象中的具体register方法
            registry.register(registedProviderUrl);
        }
    }

    //class:AbstractRegistryFactory
    public Registry getRegistry(URL url) {
        url = url.setPath(RegistryService.class.getName())
                .addParameter(Constants.INTERFACE_KEY, RegistryService.class.getName())
                .removeParameters(Constants.EXPORT_KEY, Constants.REFER_KEY);
        String key = url.toServiceString();
        // Lock the registry access process to ensure a single instance of the registry
        LOCK.lock();
        try {
            //访问缓存
            Registry registry = REGISTRIES.get(key);
            if (registry != null) {
                return registry;
            }
            //缓存未命中，创建 Registry 实例
            registry = createRegistry(url);
            if (registry == null) {
                throw new IllegalStateException("Can not create registry " + url);
            }
            //写入缓存
            REGISTRIES.put(key, registry);
            return registry;
        } finally {
            // Release the lock
            LOCK.unlock();
        }
    }

    public class ZookeeperRegistryFactory extends AbstractRegistryFactory {

        private ZookeeperTransporter zookeeperTransporter;
    
        public void setZookeeperTransporter(ZookeeperTransporter zookeeperTransporter) {
            this.zookeeperTransporter = zookeeperTransporter;
        }
    
        //创建ZookeeperRegistry对象
        @Override
        public Registry createRegistry(URL url) {
            return new ZookeeperRegistry(url, zookeeperTransporter);
        }
    
    }

    //class:ZookeeperRegistry
    public ZookeeperRegistry(URL url, ZookeeperTransporter zookeeperTransporter) {
        super(url);
        if (url.isAnyHost()) {
            throw new IllegalStateException("registry address == null");
        }

        //获取组名，默认为 dubbo
        String group = url.getParameter(Constants.GROUP_KEY, DEFAULT_ROOT);
        if (!group.startsWith(Constants.PATH_SEPARATOR)) {
            // group = "/" + group
            group = Constants.PATH_SEPARATOR + group;
        }

        this.root = group;
        //创建 Zookeeper 客户端，默认为 CuratorZookeeperTransporter
        zkClient = zookeeperTransporter.connect(url);
        //添加状态监听器
        zkClient.addStateListener(new StateListener() {
            @Override
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

    //class:FailbackRegistry
    public void register(URL url) {
        super.register(url);
        failedRegistered.remove(url);
        failedUnregistered.remove(url);
        try {
            //模板方法，由子类实现
            doRegister(url);
        } catch (Exception e) {
            //省略代码.....
        }
    }

    //class:ZookeeperRegistry
    protected void doRegister(URL url) {
        try {
            //通过 Zookeeper 客户端创建节点，节点路径由 toUrlPath 方法生成，路径格式如下:
            //   /${group}/${serviceInterface}/providers/${url}
            //比如：
            //   /dubbo/com.tianxiaobo.DemoService/providers/dubbo://ip地址:port/.....
            zkClient.create(toUrlPath(url), url.getParameter(Constants.DYNAMIC_KEY, true));
        } catch (Throwable e) {
            throw new RpcException("Failed to register " + url + " to zookeeper " + getUrl() + ", cause: " + e.getMessage(), e);
        }
    }

}