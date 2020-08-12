public class DubboExporter {

    /**
     * 
     * 服务导出的流程：
     * 1.进行各种参数和配置的检查
     * 
     * 2.将前面的各种配置信息组装成为一个URL
     * 
     * 3.获取所有注册中心的URL地址
     * 
     * 4.获取用户所配置的协议种类（比如Dubbo，Http）。然后分别使用每一种协议，将每一个服务导出到本地（自动使用Injvm协议进行）。
     * 同时，根据每一个注册中心的URL，将每一个服务进行导出（使用各自的协议进行，比如Dubbo，Http，并且导出的 exporters 保存在 AbstractProtocol 中）
     * ，并且注册在所有的注册中心上面。注意，导出（export）和注册（register）操作都在 RegistryProtocol 中进行，在 RegistryProtocol 中
     * 会调用各自协议的 XXXProtocol#export。另外，在使用各自协议进行导出的话，如果有需要的话，还会启动服务器
     * 
     * 另外，需要注意的是，服务的引用也是在 RegistryProtocol 的 refer 方法中完成的，其中会对注册中心进行订阅。订阅实际上是 RegistryDirectory 这个 Listener，
     * 在注册中心上注册一个监听器，然后当节点数发生变化时，就返回。
     * 
     */

    /**
     * [dubbo://169.254.207.250:20880/com.xu.gmall.service.UserService?anyhost=true&application=user-service-provider&dubbo=2.6.2&
     * generic=false&interface=com.xu.gmall.service.UserService&methods=getUserAddressList&pid=1308&side=provider&timestamp=1591591491230]
     */
    // 我们配置的每一个<dubbo:service/>标签都会被解析成一个ServiceBean对象
    class ServiceBean<T> extends ServiceConfig<T> implements InitializingBean, DisposableBean, ApplicationContextAware,
            ApplicationListener<ContextRefreshedEvent>, BeanNameAware {

        public void onApplicationEvent(ContextRefreshedEvent event) {
            // 是否有延迟导出 && 是否已导出 && 是不是已被取消导出
            if (isDelay() && !isExported() && !isUnexported()) {
                /**
                 * export方法会进行服务导出操作，服务导出的具体流程如下： 
                 * 1.对配置进行检查 
                 * 2.将前面的各种配置信息组装成为一个URL
                 * 3.导出服务到本地
                 * 4.导出服务到远程，导出服务到远程的时候还会启动服务器，同时还会向注册中心注册服务。如果<dubbo:service/>指明了多个协议以及多个注册中心，
                 * 那么就会将此服务使用每种协议注册到所有的注册中心上。
                 * 上面2,3,4都是在doExportUrlsFor1Protocol方法中完成的
                 */
                export();
            }
        }

    }

    class ServiceConfig<T> extends AbstractServiceConfig {

        private static final Protocol protocol = ExtensionLoader.getExtensionLoader(Protocol.class)
                .getAdaptiveExtension();

        private static final ProxyFactory proxyFactory = ExtensionLoader.getExtensionLoader(ProxyFactory.class)
                .getAdaptiveExtension();

        private final List<URL> urls = new ArrayList<URL>();

        public synchronized void export() {
            // 获取 export 和 delay 配置
            if (provider != null) {
                if (export == null) {
                    export = provider.getExport();
                }
                if (delay == null) {
                    delay = provider.getDelay();
                }
            }

            // 如果 export 为 false，则不导出服务
            // export配置决定了是否导出服务。有时候我们只是想本地启动服务进行一些调试工作，这个时候我们并不希望把本地启动的服务暴露出去给别人调用。
            // 此时，我们就可以通过配置 export 禁止服务导出，如下所示：
            // <dubbo:provider export="false" />
            if (export != null && !export) {
                return;
            }

            // delay > 0，延时导出服务
            // delay属性在<dubbo:service/>标签中被配置，延迟注册服务时间(毫秒) ，设为-1时，表示延迟到Spring容器初始化完成时暴露服务
            if (delay != null && delay > 0) {
                // 延时指定毫秒导出服务
                delayExportExecutor.schedule(new Runnable() {
                    @Override
                    public void run() {
                        doExport();
                    }
                }, delay, TimeUnit.MILLISECONDS);
            } else {
                // 立即导出服务
                doExport();
            }
        }

        protected synchronized void doExport() {
            // 如果服务已经被取消导出了，就直接抛出异常
            if (unexported) {
                throw new IllegalStateException("Already unexported!");
            }
            // 如果服务已经被导出过了，就直接返回，没有必要再进行导出
            if (exported) {
                return;
            }
            exported = true;

            // 检测 interfaceName 是否合法
            if (interfaceName == null || interfaceName.length() == 0) {
                throw new IllegalStateException("<dubbo:service interface=\"\" /> interface not allow null!");
            }
            // 检测 provider 是否为空，为空则新建一个，并通过系统变量为其初始化
            checkDefault();

            // 下面几个 if 语句用于检测 provider、application 等核心配置类对象是否为空，
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
                // 省略代码
            }
            if (application != null) {
                // 省略代码
            }

            // 检测 ref 是否泛化服务类型
            /**
             * 通过 Spring 暴露泛化服务：
             * <bean id="genericService" class="com.foo.MyGenericService" />
             * <dubbo:service interface="com.foo.BarService" ref="genericService" />
             */
            if (ref instanceof GenericService) {
                // 设置 interfaceClass 为 GenericService.class
                interfaceClass = GenericService.class;
                if (StringUtils.isEmpty(generic)) {
                    // 设置 generic = "true"
                    generic = Boolean.TRUE.toString();
                }
            
            // ref 非 GenericService 类型
            } else {
                try {
                    // 这里获得的就是接口名所指定的接口类对象，比如HelloService.class
                    interfaceClass = Class.forName(interfaceName, true, Thread.currentThread().getContextClassLoader());
                } catch (ClassNotFoundException e) {
                    throw new IllegalStateException(e.getMessage(), e);
                }
                // 对 interfaceClass，以及 <dubbo:method> 必要字段进行检查
                // 检查interfaceClass是否为null，以及是否为一个接口；另外检查methods中的每一个method是否真的存在于interfaceClass
                // 所表明的接口中
                checkInterfaceAndMethods(interfaceClass, methods);
                // 对 ref 合法性进行检测，也就是ref是否为null，ref表示的对象是否实现了interfaceClass指明的接口
                checkRef();
                // 设置 generic = "false"
                generic = Boolean.FALSE.toString();
            }

            // local参数和stub参数等价，但是已经被废弃了
            if (local != null) {
                // 省略代码
            }

            // stub属性用来配置dubbo的存根属性，stub属性有两种配置方式：
            // 1.stub="true"：指定 stub="true" 来告诉 Dubbo 框架使用本地存根，并且使用约定的方式，这个时候，本地存根的包名需要和服务接口的包名一致，
            // 类名必须在服务接口的类名后加上 Stub 的后缀
            // 2.stub="存根的全类名"：不使用约定的方式，直接指定我们定义的存根的全类名
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
                // 存根必须要实现标签中interface所指定的接口
                if (!interfaceClass.isAssignableFrom(stubClass)) {
                    throw new IllegalStateException("The stub implementation class " + stubClass.getName()
                            + " not implement interface " + interfaceName);
                }
            }

            // 检测各种对象是否为空，为空则新建，或者抛出异常
            checkApplication();
            checkRegistry();
            checkProtocol();
            appendProperties(this);
            checkStubAndMock(interfaceClass);

            // path为服务路径，默认为接口名
            if (path == null || path.length() == 0) {
                path = interfaceName;
            }

            // 导出服务
            doExportUrls();

            // ProviderModel 表示服务提供者模型，此对象中存储了和服务提供者相关的信息。比如服务的配置信息，服务实例等。
            // 每个被导出的服务对应一个 ProviderModel。ApplicationModel 持有所有的 ProviderModel。
            ProviderModel providerModel = new ProviderModel(getUniqueServiceName(), this, ref);
            ApplicationModel.initProviderModel(getUniqueServiceName(), providerModel);
        }

        /**
         * 
         * <dubbo:registry/>用来配置注册中心，address属性为注册中心服务器地址，如果地址没有端口缺省为9090。同一集群内的多个地址用逗号分隔，
         * 如：ip:port,ip:port，不同集群的注册中心，请配置多个<dubbo:registry>标签。
         * 
         * <dubbo:registry/>标签中还有两个属性：
         * register:是否向此注册中心注册服务，如果设为false，将只订阅，不注册，默认为true。
         * subscribe:是否向此注册中心订阅服务，如果设为false，将只注册，不订阅，默认为true。
         * 
         * <dubbo:service/>中registry标签如果没有配置的话，默认向所有registry注册。如果配置了的话，则只会向指定注册中心注册。在多个注册中心时使用，
         * 值为<dubbo:registry>的id属性，多个注册中心ID用逗号分隔，如果不想将该服务注册到任何registry，可将值设为N/A
         * 
         * <dubbo:service/>中protocol标签使用指定的协议暴露服务，在多协议时使用，值为<dubbo:protocol>的id属性，多个协议ID用逗号分隔
         * 
         * *************************************************总结************************************************************************
         * 
         * 根据用户在每一个<dubbo:service/>标签中配置的注册中心的个数，依据<dubbo:registry/>、<dubbo:application/>等标签中的
         * 属性，组装好所有注册中心的URL。然后将<dubbo:service/>中的服务注册到这些注册中心上（不同的协议要注册不同的次数）
         * 
         */
        private void doExportUrls() {

            // 首先是通过 loadRegistries 加载注册中心链接，用户可能在<dubbo:service/>中指定了多个注册中心。所以，获得的registryURLs可能有多个，
            // 要把此服务注册到这些注册中心上去。
            List<URL> registryURLs = loadRegistries(true);

            // 同理，用户可能在<dubbo:service/>中指定了使用多个协议，
            // 因此，遍历protocols集合根据不同的协议导出每个服务，并且将服务注册到每一个注册中心上
            for (ProtocolConfig protocolConfig : protocols) {
                doExportUrlsFor1Protocol(protocolConfig, registryURLs);
            }
        }

        private void doExportUrlsFor1Protocol(ProtocolConfig protocolConfig, List<URL> registryURLs) {
            String name = protocolConfig.getName();

            // 如果协议为空，或者说空字符串，那么将协议名变量设置为dubbo，默认协议为dubbo
            if (name == null || name.length() == 0) {
                name = "dubbo";
            }

            Map<String, String> map = new HashMap<String, String>();
            // 添加 side（是提供端provider还是消费端consumer）、dubbo版本、时间戳以及进程号等信息到 map 中
            map.put(Constants.SIDE_KEY, Constants.PROVIDER_SIDE);
            map.put(Constants.DUBBO_VERSION_KEY, Version.getVersion());
            map.put(Constants.TIMESTAMP_KEY, String.valueOf(System.currentTimeMillis()));
            if (ConfigUtils.getPid() > 0) {
                map.put(Constants.PID_KEY, String.valueOf(ConfigUtils.getPid()));
            }

            // 通过反射将对象的信息添加到map中，包括ApplicationConfig、ModuleConfig、ProviderConfig、
            // ProtocolConfig和ServiceConfig对象中的配置信息放到map中，用来组装url
            appendParameters(map, application);
            appendParameters(map, module);
            appendParameters(map, provider, Constants.DEFAULT_KEY);
            appendParameters(map, protocolConfig);
            appendParameters(map, this);

            // methods 为 MethodConfig 集合，MethodConfig 中存储了 <dubbo:method> 标签的配置信息。
            // <dubbo:method/>标签为<dubbo:service/>或者<dubbo:reference/>标签的子标签，用于控制到方法级。
            // <dubbo:argument/>方法参数配置，标签为 <dubbo:method> 的子标签，用于方法参数的特征描述。
            //
            // <dubbo:method name="findXxx" timeout="3000" retries="2">
            //      <dubbo:argument index="0" callback="true" />
            // </dubbo:method>
            //
            // 其中callback的作用是:如果为callback，服务提供方将生成反向代理，可以从服务提供方反向调用消费方，通常用于事件推送。
            // Callback 接口的实现类在 Consumer 端，当方法发生调用时，Consumer 端会自动 export 一个 Callback 服务。而
            // Provider 端在处理调用时，判断如果参数是 Callback，则生成了一个 proxy，因此服务实现类里在调用 Callback 方法的时候，
            // 会被传递到 Consumer 端执行Callback 实现类的代码。
            //
            // 获取 ArgumentConfig 列表
            // for (遍历 ArgumentConfig 列表) {
            //      if (type 不为 null，也不为空串) {    // 分支1
            //      1. 通过反射获取 interfaceClass 的方法列表
            //      for (遍历方法列表) {
            //           1. 比对方法名，查找目标方法
            //           2. 通过反射获取目标方法的参数类型数组 argtypes
            //           if (index != -1) {    // 分支2
            //                  1. 从 argtypes 数组中获取下标 index 处的元素 argType
            //                  2. 检测 argType 的名称与 ArgumentConfig 中的 type 属性是否一致
            //                  3. 添加 ArgumentConfig 字段信息到 map 中，或抛出异常
            //              } else {    // 分支3
            //                  1. 遍历参数类型数组 argtypes，查找 argument.type 类型的参数
            //                  2. 添加 ArgumentConfig 字段信息到 map 中
            //              }
            //          }
            //      } else if (index != -1) {    // 分支4
            //          1. 添加 ArgumentConfig 字段信息到 map 中
            //      }
            //  }
            //
            if (methods != null && !methods.isEmpty()) {
                for (MethodConfig method : methods) {
                    // 添加 MethodConfig 对象的字段信息到 map 中，键 = 方法名.属性名。
                    // 比如存储 <dubbo:method name="sayHello" retries="2"> 对应的 MethodConfig，
                    // 键 = sayHello.retries，map = {"sayHello.retries": 2, "xxx": "yyy"}
                    appendParameters(map, method, method.getName());

                    // retry属性已经被废弃，由retries属性代替。
                    String retryKey = method.getName() + ".retry";
                    if (map.containsKey(retryKey)) {
                        String retryValue = map.remove(retryKey);
                        // 如果配置的retry属性为false，那么就移除掉，设置新的retries属性为0
                        if ("false".equals(retryValue)) {
                            map.put(method.getName() + ".retries", "0");
                        }
                    }

                    // 从methodConfig中获取ArgumentConfig对象列表
                    List<ArgumentConfig> arguments = method.getArguments();
                    
                    // 省略代码
                } // end of methods for
            }

            // 检测 generic 是否为 "true"，并根据检测结果向 map 中添加不同的信息
            if (ProtocolUtils.isGeneric(generic)) {
                map.put("generic", generic);
                // Constants.ANY_VALUE 的值为 *
                map.put("methods", Constants.ANY_VALUE);
            } else {
                String revision = Version.getVersion(interfaceClass, version);
                if (revision != null && revision.length() > 0) {
                    map.put("revision", revision);
                }

                // 为接口生成包裹类 Wrapper，Wrapper 中包含了接口的详细信息，比如接口方法名数组，字段信息等。这里获取到了特定接口中方法名字数组
                // 具体的说就是这里通过 javassist 为 interfaceClass 生成了一个动态代理类，在这个代理类中，有一个属性 mns，它是字符串数组，
                // 保存了 interfaceClass 中所有的方法名。通过调用 getMethodNames 方法，直接返回这个属性
                String[] methods = Wrapper.getWrapper(interfaceClass).getMethodNames();
                // 添加方法名到 map 中，如果包含多个方法名，则用逗号隔开，比如 method = init,destroy
                if (methods.length == 0) {
                    logger.warn("NO method found in service interface " + interfaceClass.getName());
                    map.put(Constants.METHODS_KEY, Constants.ANY_VALUE);
                } else {
                    // 将逗号作为分隔符连接方法名，并将连接后的字符串放入 map 中，最终会在map中形成如下键值对：
                    // methods -> func A, func B
                    map.put(Constants.METHODS_KEY, StringUtils.join(new HashSet<String>(Arrays.asList(methods)), ","));
                }
            }

            // 代码省略

            if ("injvm".equals(protocolConfig.getName())) {
                protocolConfig.setRegister(false);
                map.put("notify", "false");
            }

            // 获取上下文路径
            String contextPath = protocolConfig.getContextpath();
            if ((contextPath == null || contextPath.length() == 0) && provider != null) {
                contextPath = provider.getContextpath();
            }

            // 获取到本机的ip地址和端口号
            String host = this.findConfigedHosts(protocolConfig, registryURLs, map);
            Integer port = this.findConfigedPorts(protocolConfig, name, map);

            // 根据本机的 ip 和 port，以及上面map中的各种信息，包括但不限于提供方的应用名称，即application，
            // dubbo的版本号，接口的全类名，接口中的所有方法名（如果不止一个的话，使用逗号分隔），进程id和时间戳。
            URL url = new URL(name, host, port, (contextPath == null || contextPath.length() == 0 ? "" : contextPath + "/") + path, map);
            String scope = url.getParameter(Constants.SCOPE_KEY);

            // 下面将要进行服务导出，先导出到本地，再导出到远程。如果配置scope为none，则不进行导出操作，注意这与scope为null不同，
            // 如果scope为null，还是会执行导出操作。scope为local，则只导出到本地；如果scope为remote，则只导出到远程
            // 在下面进行服务导出的过程当中，会创建 Invoker。Invoker 是实体域，它是 Dubbo 的核心模型，其它模型都向它靠扰，或转换成它，它代表一个可执行体，
            // 可向它发起 invoke 调用，它有可能是一个本地的实现，也可能是一个远程的实现，也可能一个集群实现。
            if (!Constants.SCOPE_NONE.toString().equalsIgnoreCase(scope)) {

                // scope != remote，导出到本地
                if (!Constants.SCOPE_REMOTE.toString().equalsIgnoreCase(scope)) {
                    exportLocal(url);
                }

                // scope != local，导出到远程
                if (!Constants.SCOPE_LOCAL.toString().equalsIgnoreCase(scope)) {
                    if (logger.isInfoEnabled()) {
                        logger.info("Export dubbo service " + interfaceClass.getName() + " to url " + url);
                    }
                    if (registryURLs != null && !registryURLs.isEmpty()) {
                        for (URL registryURL : registryURLs) {
                            url = url.addParameterIfAbsent(Constants.DYNAMIC_KEY,
                                    registryURL.getParameter(Constants.DYNAMIC_KEY));

                            // 加载监视器链接
                            URL monitorUrl = loadMonitor(registryURL);
                            if (monitorUrl != null) {
                                // 将监视器链接作为参数添加到 url 中
                                url = url.addParameterAndEncoded(Constants.MONITOR_KEY, monitorUrl.toFullString());
                            }
                            if (logger.isInfoEnabled()) {
                                logger.info("Register dubbo service " + interfaceClass.getName() + " url " + url
                                        + " to registry " + registryURL);
                            }
                            // 为服务提供类(ref)生成 Invoker
                            Invoker<?> invoker = proxyFactory.getInvoker(ref, (Class) interfaceClass,
                                    registryURL.addParameterAndEncoded(Constants.EXPORT_KEY, url.toFullString()));
                            // DelegateProviderMetaDataInvoker 作用为简单地持有 Invoker 和 ServiceConfig
                            DelegateProviderMetaDataInvoker wrapperInvoker = new DelegateProviderMetaDataInvoker(
                                    invoker, this);

                            // 导出服务，并生成 Exporter，由于这里的 protocol 也是使用自适应拓展模式生成的一个代理对象 Protocol$Adaptive，
                            // 而 wrapperInvoker 中持有一个 Invoker 对象，这个 Invoker 对象的 url 协议为 registry，因此实际调用的是 RegistryPortocol 的 export 方法
                            Exporter<?> exporter = protocol.export(wrapperInvoker);
                            exporters.add(exporter);
                        }
                    } else {
                        // 不存在注册中心，仅导出服务
                        Invoker<?> invoker = proxyFactory.getInvoker(ref, (Class) interfaceClass, url);
                        DelegateProviderMetaDataInvoker wrapperInvoker = new DelegateProviderMetaDataInvoker(invoker, this);

                        Exporter<?> exporter = protocol.export(wrapperInvoker);
                        exporters.add(exporter);
                    }
                }
            }

            // 将配置好的URL放入到urls列表集合中
            this.urls.add(url);
        }

        private void exportLocal(URL url) {
            // 如果 URL 的协议头等于 injvm，就不会再多此一举继续执行ServiceConfig#exportLocal方法，而是会在后面直接调用protocol.export生成一个Exporter
            if (!Constants.LOCAL_PROTOCOL.equalsIgnoreCase(url.getProtocol())) {
                // 设置协议头为 injvm，并且将原协议URL中的主机名和端口号设置为新的值，也就是localhost和0,
                URL local = URL.valueOf(url.toFullString()).setProtocol(Constants.LOCAL_PROTOCOL).setHost(LOCALHOST)
                        .setPort(0);
                ServiceClassHolder.getInstance().pushServiceClass(getServiceClass(ref));

                // 创建 Invoker，并导出服务，这里的 protocol 会在运行时调用 InjvmProtocol 的 export 方法，将协议导出到本地。
                // protocol = ExtensionLoader.getExtensionLoader(Protocol.class).getAdaptiveExtension();
                // 因为protocol采用自适应拓展的形式根据URL参数获取具体的实现类，或者说拓展。这里local中使用的协议被替换为injvm，因此
                // 当调用export方法时，实际获取到的是InjvmProtocol的export方法。同理
                // proxyFactory = ExtensionLoader.getExtensionLoader(ProxyFactory.class).getAdaptiveExtension();
                // 由于local这个URL中没有proxy这个参数存在，因此获取到的proxyFactory默认为JavassistProxyFactory
                Exporter<?> exporter = protocol.export(proxyFactory.getInvoker(ref, (Class) interfaceClass, local));
                exporters.add(exporter);
                logger.info("Export dubbo service " + interfaceClass.getName() + " to local registry");
            }
        }
    }

    public class JavassistProxyFactory extends AbstractProxyFactory {

        @Override
        @SuppressWarnings("unchecked")
        public <T> T getProxy(Invoker<T> invoker, Class<?>[] interfaces) {
            return (T) Proxy.getProxy(interfaces).newInstance(new InvokerInvocationHandler(invoker));
        }

        // proxyFactory.getInvoker(ref, (Class) interfaceClass,
        // registryURL.addParameterAndEncoded(Constants.EXPORT_KEY, url.toFullString()))
        // registryURL.addParameterAndEncoded对url和registryURL进行了一定的拼接和重组，生成了如下的结果：

        // registry://127.0.0.1:2181/com.alibaba.dubbo.registry.RegistryService?application=user-service-provider&dubbo=2.6.2&
        // export=dubbo://169.254.207.250:20880/com.xu.gmall.service.UserService?anyhost=true&application=user-service-provider&
        // bind.ip=169.254.207.250&bind.port=20880&dubbo=2.6.2&generic=false&interface=com.xu.gmall.service.UserService&
        // methods=getUserAddressList,calc&pid=11032&side=provider&timestamp=1591630213319&pid=11032&registry=zookeeper&timestamp=1591630213288

        // url：dubbo://169.254.207.250:20880/com.xu.gmall.service.UserService?anyhost=true&application=user-service-provider&bind.ip=169.254.207.250&
        // bind.port=20880&dubbo=2.6.2&generic=false&interface=com.xu.gmall.service.UserService&methods=getUserAddressList,calc&
        // pid=11032&side=provider&timestamp=1591630213319

        // registryURL：registry://127.0.0.1:2181/com.alibaba.dubbo.registry.RegistryService?application=user-service-provider&
        // dubbo=2.6.2&pid=11032&registry=zookeeper&timestamp=1591630213288

        // proxy可以为<dubbo:service/>中interface指定接口的具体实现类，比如UserServiceImpl
        // type则为proxy对象所具体实现的接口
        @Override
        public <T> Invoker<T> getInvoker(T proxy, Class<T> type, URL url) {
            // 为目标类创建 Wrapper
            final Wrapper wrapper = Wrapper.getWrapper(proxy.getClass().getName().indexOf('$') < 0 ? proxy.getClass() : type);
            // 创建匿名 Invoker 类对象，并实现 doInvoke 方法。
            // JavassistProxyFactory 创建了一个继承自 AbstractProxyInvoker 类的匿名对象，并覆写了抽象方法 doInvoke。覆写后的 doInvoke 逻辑比较简单，
            // 仅是将调用请求转发给了 Wrapper 类的 invokeMethod 方法
            return new AbstractProxyInvoker<T>(proxy, type, url) {
                @Override
                protected Object doInvoke(T proxy, String methodName, Class<?>[] parameterTypes, Object[] arguments)
                        throws Throwable {
                    // 调用 Wrapper 的 invokeMethod 方法，invokeMethod 最终会调用目标方法
                    return wrapper.invokeMethod(proxy, methodName, parameterTypes, arguments);
                }
            };
        }

    }

    // class:InjvmProtocol
    public <T> Exporter<T> export(Invoker<T> invoker) throws RpcException {
        // exporterMap是InjvmProtocol的父类AbstractProtocol中的属性，因此export方法会创建一个
        // InjvmExporter对象，并且把这个对象添加到exporterMap中。
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

    public class AbstractInterfaceConfig extends AbstractMethodConfig {

        /**
         * loadRegistries方法的逻辑如下： 
         * 1.检测是否存在注册中心配置类，不存在则抛出异常 
         * 2.构建参数映射集合，也就是 map
         * 3.根据参数映射集合以及地址address，解析出URL列表 
         * 4.遍历URL列表，并根据条件决定是否将其添加到 registryList 中
         * 
         * loadRegistries 这个方法被 ServiceConfig#doExportUrls 和 ReferenceConfig#createProxy 方法调用。ServiceConfig 调用
         * 这个方法（provider 参数的值为 true），来获得注册中心的地址，从而可以将服务注册到这些注册中心上面；ReferenceConfig 调用这个方法，
         * （provider 参数的值为 false），获得注册中心的地址，从而可以从这些注册中心上面订阅服务
         */
        protected List<URL> loadRegistries(boolean provider) {
            // 检测是否存在注册中心配置类，不存在则抛出异常
            checkRegistry();
            List<URL> registryList = new ArrayList<URL>();

            // 这里的registries是RegistryConfig对象的集合，RegistryConfig是<dubbo:registry/>标签配置的一个抽象表示。
            // 如果我们配置了多个注册中心的话（存在多个<dubbo:registry/>标签），就会存在对应数量的RegistryConfig对象。比如：
            // <dubbo:registry address="zookeeper://127.0.0.1:2181"/>
            // <dubbo:registry address="zookeeper://127.0.0.2:2181"/>
            // 那么RegistryConfig对象就会有2个，代表了上面这两个注册中心配置
            //
            // 注意，也可以如下配置：
            //
            // <dubbo:registry protocol="zookeeper" address="127.0.0.2:2181,127.0.0.1:2181"/>，这样会得到一个RegistryConfig对象，并且解析得到的urls
            // 数组中只有一个url，这个url为：
            // zookeeper://127.0.0.1:2181/com.alibaba.dubbo.registry.RegistryService?application=user-service-provider&backup=127.0.0.2:2181
            // &dubbo=2.6.2&pid=17376&timestamp=1591607738629
            if (registries != null && !registries.isEmpty()) {

                for (RegistryConfig config : registries) {
                    String address = config.getAddress();
                    if (address == null || address.length() == 0) {
                        // 若 address 为空，则将其设为 0.0.0.0
                        address = Constants.ANYHOST_VALUE;
                    }

                    // 从系统属性中加载注册中心地址
                    String sysaddress = System.getProperty("dubbo.registry.address");
                    if (sysaddress != null && sysaddress.length() > 0) {
                        address = sysaddress;
                    }

                    // 判断address是否合法
                    if (address != null && address.length() > 0
                            && !RegistryConfig.NO_AVAILABLE.equalsIgnoreCase(address)) {
                        Map<String, String> map = new HashMap<String, String>();
                        // 添加<dubbo:application/>标签中的信息到map中，比如name、owner以及organization属性
                        appendParameters(map, application);
                        // 添加 RegistryConfig 字段信息到 map 中，比如address、register、group以及check等
                        appendParameters(map, config);
                        // 添加服务路径信息到 map 中，默认和interface一致
                        map.put("path", RegistryService.class.getName());
                        // 添加dubbo的版本信息到map中
                        map.put("dubbo", Version.getVersion());
                        // 添加时间戳TIMESTAMP到map中
                        map.put(Constants.TIMESTAMP_KEY, String.valueOf(System.currentTimeMillis()));
                        if (ConfigUtils.getPid() > 0) {
                            map.put(Constants.PID_KEY, String.valueOf(ConfigUtils.getPid()));
                        }
                        // 添加协议protocol到map中
                        if (!map.containsKey("protocol")) {
                            if (ExtensionLoader.getExtensionLoader(RegistryFactory.class).hasExtension("remote")) {
                                map.put("protocol", "remote");
                            } else {
                                map.put("protocol", "dubbo");
                            }
                        }

                        // 解析得到 URL 列表
                        List<URL> urls = UrlUtils.parseURLs(address, map);
                        for (URL url : urls) {
                            // 在url中添加一个registry属性，并且将registry设置为我们在<dubbo:registry/>中所配置的协议名
                            url = url.addParameter(Constants.REGISTRY_KEY, url.getProtocol());
                            // 将 URL 协议头设置为 registry
                            url = url.setProtocol(Constants.REGISTRY_PROTOCOL);
                            // 通过判断条件，决定是否添加 url 到 registryList 中，条件如下：
                            // (服务提供者 && register = true 或 null) || (非服务提供者 && subscribe = true 或 null)
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

    public class RegistryProtocol implements Protocol {

        public <T> Exporter<T> export(final Invoker<T> originInvoker) throws RpcException {
            // 导出服务，并且启动服务器监听特定端口
            final ExporterChangeableWrapper<T> exporter = doLocalExport(originInvoker);

            // 获取注册中心 URL，以 zookeeper 注册中心为例，得到的示例 URL 如下：
            // zookeeper://127.0.0.1:2181/com.alibaba.dubbo.registry.RegistryService?application=demo-provider&dubbo=2.0.2&
            // export=dubbo%3A%2F%2F172.17.48.52%3A20880%2Fcom.alibaba.dubbo.demo.DemoService%3Fanyhost%3Dtrue%26application%3Ddemo-provider
            URL registryUrl = getRegistryUrl(originInvoker);

            final Registry registry = getRegistry(originInvoker);
            final URL registedProviderUrl = getRegistedProviderUrl(originInvoker);

            // 获取 register 参数
            boolean register = registedProviderUrl.getParameter("register", true);
            // 向服务提供者与消费者注册表中注册服务提供者
            ProviderConsumerRegTable.registerProvider(originInvoker, registryUrl, registedProviderUrl);

            // 根据 register 的值决定是否注册服务
            if (register) {
                // 向注册中心注册服务
                register(registryUrl, registedProviderUrl);
                ProviderConsumerRegTable.getProviderWrapper(originInvoker).setReg(true);
            }

            // 获取订阅 URL，比如：
            // provider://172.17.48.52:20880/com.alibaba.dubbo.demo.DemoService?category=configurators&check=false&anyhost=true&
            // application=demo-provider&dubbo=2.0.2&generic=false&interface=com.alibaba.dubbo.demo.DemoService&methods=sayHello
            final URL overrideSubscribeUrl = getSubscribedOverrideUrl(registedProviderUrl);

            // 创建监听器
            final OverrideListener overrideSubscribeListener = new OverrideListener(overrideSubscribeUrl,
                    originInvoker);
            overrideListeners.put(overrideSubscribeUrl, overrideSubscribeListener);

            // 向注册中心进行订阅 override 数据
            registry.subscribe(overrideSubscribeUrl, overrideSubscribeListener);

            return new Exporter<T>() {
                public Invoker<T> getInvoker() {
                    return exporter.getInvoker();
                }
    
                public void unexport() {
                    try {
                        exporter.unexport();
                    } catch (Throwable t) {
                        logger.warn(t.getMessage(), t);
                    }
                    try {
                        registry.unregister(registedProviderUrl);
                    } catch (Throwable t) {
                        logger.warn(t.getMessage(), t);
                    }
                    try {
                        overrideListeners.remove(overrideSubscribeUrl);
                        registry.unsubscribe(overrideSubscribeUrl, overrideSubscribeListener);
                    } catch (Throwable t) {
                        logger.warn(t.getMessage(), t);
                    }
                }
            };
        }

        private <T> ExporterChangeableWrapper<T> doLocalExport(final Invoker<T> originInvoker) {
            String key = getCacheKey(originInvoker);
            // 访问缓存
            ExporterChangeableWrapper<T> exporter = (ExporterChangeableWrapper<T>) bounds.get(key);
            if (exporter == null) {
                synchronized (bounds) {
                    exporter = (ExporterChangeableWrapper<T>) bounds.get(key);
                    if (exporter == null) {
                        // 将Invoker进行了一个封装，将invoker和invoker中的url的provider部分，也就是export=dubbo://....的封装在一起，
                        // 称为InvokerDelegete对象
                        final Invoker<?> invokerDelegete = new InvokerDelegete<T>(originInvoker, getProviderUrl(originInvoker));
                        // 调用 protocol 的 export 方法导出服务。这里的invokerDelegete中的url如上所述是以dubbo协议开头的，因此
                        // 这里实际调用的是DubboProtocol的export方法，返回的也是一个DubboExporter对象。并且在导出的过程中，还会启动
                        // 服务器监听（默认是Netty），监听的地址就是本机的ip地址，以及在<dubbo:protocol/>指定好的端口号
                        exporter = new ExporterChangeableWrapper<T>((Exporter<T>) protocol.export(invokerDelegete),
                                originInvoker);
                        // 写缓存
                        bounds.put(key, exporter);
                    }
                }
            }
            return exporter;
        }

        private URL getProviderUrl(final Invoker<?> origininvoker) {
            String export = origininvoker.getUrl().getParameterAndDecoded(Constants.EXPORT_KEY);
            if (export == null || export.length() == 0) {
                throw new IllegalArgumentException("The registry export url is null! registry: " + origininvoker.getUrl());
            }
    
            URL providerUrl = URL.valueOf(export);
            return providerUrl;
        }

        /**
         * 两个作用： 1.获取注册中心实例 2.向注册中心注册服务
         */
        public void register(URL registryUrl, URL registedProviderUrl) {
            // 这里的 registryFactory 的类型为 RegistryFactory$Adaptive，会根据 registryUrl 中协议的名称来获取 RegistryFactory 类型的扩展，
            // 比如如果 registryUrl 的协议为 zookeeper，那么会获取到 ZookeeperRegistryFactory 对象，然后调用其 getRegistry 方法。
            // 获取 Registry，这里获取的是ZookeeperRegistry，当然也有可能是RedisRegistry。
            Registry registry = registryFactory.getRegistry(registryUrl);
            // 调用Registry对象中的具体register方法
            registry.register(registedProviderUrl);
        }
    }

    public class DubboProtocol extends AbstractProtocol {

        @Override
        public <T> Exporter<T> export(Invoker<T> invoker) throws RpcException {
            URL url = invoker.getUrl();

            // 获取服务标识，理解成服务坐标也行。由服务组名，服务名，服务版本号以及端口组成。比如：
            // demoGroup/com.alibaba.dubbo.demo.DemoService:1.0.1:20880
            String key = serviceKey(url);
            // 创建 DubboExporter
            DubboExporter<T> exporter = new DubboExporter<T>(invoker, key, exporterMap);
            // 将 <key, exporter> 键值对放入缓存中
            exporterMap.put(key, exporter);

            // 以下代码应该和本地存根有关，代码不难看懂，但具体用途暂时不清楚，先忽略
            Boolean isStubSupportEvent = url.getParameter(Constants.STUB_EVENT_KEY, Constants.DEFAULT_STUB_EVENT);
            Boolean isCallbackservice = url.getParameter(Constants.IS_CALLBACK_SERVICE, false);
            if (isStubSupportEvent && !isCallbackservice) {
                String stubServiceMethods = url.getParameter(Constants.STUB_EVENT_METHODS_KEY);
                if (stubServiceMethods == null || stubServiceMethods.length() == 0) {
                    if (logger.isWarnEnabled()) {
                        logger.warn(new IllegalStateException("consumer [" + url.getParameter(Constants.INTERFACE_KEY)
                                + "], has set stubproxy support event ,but no stub methods founded."));
                    }
                } else {
                    stubServiceMethodsMap.put(url.getServiceKey(), stubServiceMethods);
                }
            }

            // 启动服务器
            openServer(url);
            // 优化序列化
            optimizeSerialization(url);
            return exporter;
        }

        private void openServer(URL url) {
            // 获取 host:port，并将其作为服务器实例的 key，用于标识当前的服务器实例
            String key = url.getAddress();
            // client can export a service which's only for server to invoke
            boolean isServer = url.getParameter(Constants.IS_SERVER_KEY, true);
            if (isServer) {
                // 访问缓存
                ExchangeServer server = serverMap.get(key);
                // 如下，在同一台机器上（单网卡，也就是单IP地址），同一个端口上仅允许启动一个服务器实例。若某个端口上已有服务器实例，
                // 此时则调用 reset 方法重置服务器的一些配置
                if (server == null) {
                    // 创建服务器实例
                    serverMap.put(key, createServer(url));
                } else {
                    // 服务器已创建，则根据 url 中的配置重置服务器
                    server.reset(url);
                }
            }
        }

        private ExchangeServer createServer(URL url) {
            // send readonly event when server closes, it's enabled by default
            url = url.addParameterIfAbsent(Constants.CHANNEL_READONLYEVENT_SENT_KEY, Boolean.TRUE.toString());
            // 添加心跳检测配置到url中
            url = url.addParameterIfAbsent(Constants.HEARTBEAT_KEY, String.valueOf(Constants.DEFAULT_HEARTBEAT));
            // 获取 server 参数，默认为 netty，获取到这个server参数之后，会进行判断dubbo是否支持创建这种类型的服务器。
            // Constants.DEFAULT_REMOTING_SERVER为netty
            String str = url.getParameter(Constants.SERVER_KEY, Constants.DEFAULT_REMOTING_SERVER);
            // 通过 SPI 检测是否存在 server 参数所代表的 Transporter 拓展，不存在则抛出异常
            if (str != null && str.length() > 0
                    && !ExtensionLoader.getExtensionLoader(Transporter.class).hasExtension(str))
                throw new RpcException("Unsupported server type: " + str + ", url: " + url);

            // 添加编码解码器参数
            url = url.addParameter(Constants.CODEC_KEY, DubboCodec.NAME);
            ExchangeServer server;
            try {
                // 创建 ExchangeServer
                server = Exchangers.bind(url, requestHandler);
            } catch (RemotingException e) {
                throw new RpcException("Fail to start server(url: " + url + ") " + e.getMessage(), e);
            }

            // 获取 client 参数，可指定 netty，mina
            str = url.getParameter(Constants.CLIENT_KEY);
            if (str != null && str.length() > 0) {
                // 获取所有的 Transporter 实现类名称集合，比如 supportedTypes = [netty, mina]
                Set<String> supportedTypes = ExtensionLoader.getExtensionLoader(Transporter.class).getSupportedExtensions();
                // 检测当前 Dubbo 所支持的 Transporter 实现类名称列表中，是否包含 client 所表示的 Transporter，若不包含，则抛出异常
                if (!supportedTypes.contains(str)) {
                    throw new RpcException("Unsupported client type...");
                }
            }

            return server;
        }

    }

    // Exchanger facade
    public static class Exchangers {

        public static ExchangeServer bind(URL url, ExchangeHandler handler) throws RemotingException {
            if (url == null) {
                throw new IllegalArgumentException("url == null");
            }
            if (handler == null) {
                throw new IllegalArgumentException("handler == null");
            }
            url = url.addParameterIfAbsent(Constants.CODEC_KEY, "exchange");
            return getExchanger(url).bind(url, handler);
        }

        public static Exchanger getExchanger(URL url) {
            // EXCHANGER_KEY为exchanger，而DEFAULT_EXCHANGER为header
            String type = url.getParameter(Constants.EXCHANGER_KEY, Constants.DEFAULT_EXCHANGER);
            // 获取exchanger类型的扩展，根据上面一行代码可知，默认获取到的exchanger为HeaderExchanger
            return getExchanger(type);
        }

        public static Exchanger getExchanger(String type) {
            return ExtensionLoader.getExtensionLoader(Exchanger.class).getExtension(type);
        }

    }

    public class HeaderExchanger implements Exchanger {

        public static final String NAME = "header";

        @Override
        public ExchangeClient connect(URL url, ExchangeHandler handler) throws RemotingException {
            return new HeaderExchangeClient(
                    Transporters.connect(url, new DecodeHandler(new HeaderExchangeHandler(handler))), true);
        }

        @Override
        public ExchangeServer bind(URL url, ExchangeHandler handler) throws RemotingException {
            // 创建 HeaderExchangeServer 实例，该方法包含了多步操作，分别如下：
            // 1. new HeaderExchangeHandler(handler)
            // 2. new DecodeHandler(new HeaderExchangeHandler(handler))
            // 3. Transporters.bind(url, new DecodeHandler(new HeaderExchangeHandler(handler)))
            // 
            // Transporters.bind 会让服务器开始监听特定端口，而 HeaderExchangeServer 则会添加心跳逻辑
            return new HeaderExchangeServer(Transporters.bind(url, new DecodeHandler(new HeaderExchangeHandler(handler))));
        }

    }

    public static class Transporters {
        public static Server bind(URL url, ChannelHandler... handlers) throws RemotingException {
            // 省略参数检查代码
            ChannelHandler handler;
            if (handlers.length == 1) {
                handler = handlers[0];
            } else {
                handler = new ChannelHandlerDispatcher(handlers);
            }
            // 由于Transporter接口上的@SPI注解为netty（Netty3），因此在url中如果没有指定transporter参数的话，默认获取到的Transporter扩展
            // 为NettyTransporter（Netty3类型），不过这里以Netty4为例进行讲解
            return getTransporter().bind(url, handler);
        }

        public static Transporter getTransporter() {
            return ExtensionLoader.getExtensionLoader(Transporter.class).getAdaptiveExtension();
        }
    }

    public class NettyTransporter implements Transporter {

        public static final String NAME = "netty4";

        @Override
        public Server bind(URL url, ChannelHandler listener) throws RemotingException {
            return new NettyServer(url, listener);
        }

        @Override
        public Client connect(URL url, ChannelHandler listener) throws RemotingException {
            return new NettyClient(url, listener);
        }

    }

    public abstract class AbstractServer extends AbstractEndpoint implements Server {

        public AbstractServer(URL url, ChannelHandler handler) throws RemotingException {
            super(url, handler);
            localAddress = getUrl().toInetSocketAddress();
            // 获取要监听的ip地址和端口号
            String bindIp = getUrl().getParameter(Constants.BIND_IP_KEY, getUrl().getHost());
            int bindPort = getUrl().getParameter(Constants.BIND_PORT_KEY, getUrl().getPort());
            if (url.getParameter(Constants.ANYHOST_KEY, false) || NetUtils.isInvalidLocalHost(bindIp)) {
                bindIp = NetUtils.ANYHOST;
            }
            bindAddress = new InetSocketAddress(bindIp, bindPort);
            // 获取最大可接受连接数
            this.accepts = url.getParameter(Constants.ACCEPTS_KEY, Constants.DEFAULT_ACCEPTS);
            this.idleTimeout = url.getParameter(Constants.IDLE_TIMEOUT_KEY, Constants.DEFAULT_IDLE_TIMEOUT);
            try {
                // 调用模板方法 doOpen启动服务器
                doOpen();
                if (logger.isInfoEnabled()) {
                    logger.info("Start " + getClass().getSimpleName() + " bind " + getBindAddress() + ", export "
                            + getLocalAddress());
                }
            } catch (Throwable t) {
                throw new RemotingException(url.toInetSocketAddress(), null, "Failed to bind "
                        + getClass().getSimpleName() + " on " + getLocalAddress() + ", cause: " + t.getMessage(), t);
            }
            // fixme replace this with better method
            DataStore dataStore = ExtensionLoader.getExtensionLoader(DataStore.class).getDefaultExtension();
            executor = (ExecutorService) dataStore.get(Constants.EXECUTOR_SERVICE_COMPONENT_KEY,
                    Integer.toString(url.getPort()));
        }

        protected abstract void doOpen() throws Throwable;

        protected abstract void doClose() throws Throwable;

    }

    public class NettyServer extends AbstractServer implements Server {

        private Map<String, Channel> channels; // <ip:port, channel>
        
        private ServerBootstrap bootstrap;
        
        private io.netty.channel.Channel channel;
        
        private EventLoopGroup bossGroup;
        
        private EventLoopGroup workerGroup;

        public NettyServer(URL url, ChannelHandler handler) throws RemotingException {
            super(url, ChannelHandlers.wrap(handler, ExecutorUtil.setThreadName(url, SERVER_THREAD_POOL_NAME)));
        }

        @Override
        protected void doOpen() throws Throwable {
            NettyHelper.setNettyLoggerFactory();

            bootstrap = new ServerBootstrap();
            // 创建boss和worker线程池
            bossGroup = new NioEventLoopGroup(1, new DefaultThreadFactory("NettyServerBoss", true));
            workerGroup = new NioEventLoopGroup(
                    getUrl().getPositiveParameter(Constants.IO_THREADS_KEY, Constants.DEFAULT_IO_THREADS),
                    new DefaultThreadFactory("NettyServerWorker", true));

            final NettyServerHandler nettyServerHandler = new NettyServerHandler(getUrl(), this);
            channels = nettyServerHandler.getChannels();

            bootstrap.group(bossGroup, workerGroup).channel(NioServerSocketChannel.class)
                    .childOption(ChannelOption.TCP_NODELAY, Boolean.TRUE)
                    .childOption(ChannelOption.SO_REUSEADDR, Boolean.TRUE)
                    .childOption(ChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT)
                    .childHandler(new ChannelInitializer<NioSocketChannel>() {
                        @Override
                        protected void initChannel(NioSocketChannel ch) throws Exception {
                            NettyCodecAdapter adapter = new NettyCodecAdapter(getCodec(), getUrl(), NettyServer.this);
                            ch.pipeline()// .addLast("logging",new LoggingHandler(LogLevel.INFO))//for debug
                                    .addLast("decoder", adapter.getDecoder()).addLast("encoder", adapter.getEncoder())
                                    .addLast("handler", nettyServerHandler);
                        }
                    });
            // bind
            ChannelFuture channelFuture = bootstrap.bind(getBindAddress());
            channelFuture.syncUninterruptibly();
            channel = channelFuture.channel();

        }
    }

    /********************************************************************** 服务注册 ***************************************************************************************** */

    /**
     * 两个作用： 1.获取注册中心实例 2.向注册中心注册服务
     */
    public void register(URL registryUrl, URL registedProviderUrl) {
        // 这里的 registryFactory 的类型为 RegistryFactory$Adaptive，会根据 registryUrl 中协议的名称来获取
        // RegistryFactory 类型的扩展，比如如果 registryUrl 的协议为 zookeeper，那么会获取到 ZookeeperRegistryFactory 对象，然后调用其
        // getRegistry 方法。获取 Registry，这里获取的是ZookeeperRegistry，会在创建 ZookeeperRegistry 对象的过程中同时创建一个
        // Zookeeper 的客户端 zkClient
        Registry registry = registryFactory.getRegistry(registryUrl);
        // 调用Registry对象中的具体register方法，将服务的 url 注册到注册中心上的特定目录上。
        registry.register(registedProviderUrl);
    }

    // 根据url中参数protocol的值，也就是url所使用的协议来判断使用哪一个扩展，如果没有指明Protocol，则默认
    // 使用DubboRegistryProtocol
    @SPI("dubbo")
    public interface RegistryFactory {
        @Adaptive({ "protocol" })
        Registry getRegistry(URL url);
    }

    public abstract class AbstractRegistryFactory implements RegistryFactory {

        // class:AbstractRegistryFactory
        public Registry getRegistry(URL url) {
            url = url.setPath(RegistryService.class.getName())
                    .addParameter(Constants.INTERFACE_KEY, RegistryService.class.getName())
                    .removeParameters(Constants.EXPORT_KEY, Constants.REFER_KEY);
            String key = url.toServiceString();
            // Lock the registry access process to ensure a single instance of the registry
            // 每一个服务都会通过 getRegistry 获取到注册中心，然后注册到上面去，所以为了避免注册中心的重复创建，
            // 使用锁进行同步控制
            LOCK.lock();
            try {
                // 访问缓存
                Registry registry = REGISTRIES.get(key);
                if (registry != null) {
                    return registry;
                }

                // 缓存未命中，创建 Registry 实例
                registry = createRegistry(url);
                if (registry == null) {
                    throw new IllegalStateException("Can not create registry " + url);
                }

                // 写入缓存
                REGISTRIES.put(key, registry);
                return registry;
            } finally {
                // Release the lock
                LOCK.unlock();
            }
        }

        protected abstract Registry createRegistry(URL url);

    }

    public class ZookeeperRegistryFactory extends AbstractRegistryFactory {

        // zookeeperTransporter 由 SPI 在运行时注入，类型为 ZookeeperTransporter$Adaptive
        private ZookeeperTransporter zookeeperTransporter;

        public void setZookeeperTransporter(ZookeeperTransporter zookeeperTransporter) {
            this.zookeeperTransporter = zookeeperTransporter;
        }

        // 创建ZookeeperRegistry对象
        @Override
        public Registry createRegistry(URL url) {
            return new ZookeeperRegistry(url, zookeeperTransporter);
        }

    }

    public abstract class FailbackRegistry extends AbstractRegistry {
        // class:FailbackRegistry
        public void register(URL url) {
            super.register(url);
            failedRegistered.remove(url);
            failedUnregistered.remove(url);
            try {
                // 模板方法，由子类实现
                doRegister(url);
            } catch (Exception e) {
                // 省略代码.....
            }
        }
    }

    public class ZookeeperRegistry extends FailbackRegistry {

        // class:ZookeeperRegistry
        public ZookeeperRegistry(URL url, ZookeeperTransporter zookeeperTransporter) {
            super(url);
            if (url.isAnyHost()) {
                throw new IllegalStateException("registry address == null");
            }

            // 获取组名，默认为 dubbo
            String group = url.getParameter(Constants.GROUP_KEY, DEFAULT_ROOT);
            if (!group.startsWith(Constants.PATH_SEPARATOR)) {
                // group = "/" + group
                group = Constants.PATH_SEPARATOR + group;
            }

            this.root = group;
            // 创建 Zookeeper 客户端，默认为 ZkclientZookeeperTransporter
            zkClient = zookeeperTransporter.connect(url);
            // 添加状态监听器
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

        // class:ZookeeperRegistry
        protected void doRegister(URL url) {
            try {
                // 通过 Zookeeper 客户端创建节点，节点路径由 toUrlPath 方法生成，路径格式如下:
                // /${group}/${serviceInterface}/providers/${url}
                //
                // 比如：
                // /dubbo/com.tianxiaobo.DemoService/providers/dubbo://ip地址:port/.....
                zkClient.create(toUrlPath(url), url.getParameter(Constants.DYNAMIC_KEY, true));
            } catch (Throwable e) {
                throw new RpcException(
                        "Failed to register " + url + " to zookeeper " + getUrl() + ", cause: " + e.getMessage(), e);
            }
        }

    }

/*
 * Decompiled with CFR.
 *
 * Could not load the following classes:
 *  com.dubbo.simple.common.DemoService
 */

public class Wrapper0 extends Wrapper implements ClassGenerator.DC {

    public static String[] pns;
    public static Map pts;
    public static String[] mns;
    public static String[] dmns;
    public static Class[] mts0;
    public static Class[] mts1;

    public Class getPropertyType(String string) {
        return (Class) pts.get(string);
    }

    @Override
    public Object getPropertyValue(Object object, String string) {
        try {
            DemoService demoService = (DemoService) object;
        } catch (Throwable throwable) {
            throw new IllegalArgumentException(throwable);
        }
        throw new NoSuchPropertyException(new StringBuffer().append("Not found property \"").append(string)
                .append("\" filed or setter method in class com.dubbo.simple.common.DemoService.").toString());
    }

    @Override
    public void setPropertyValue(Object object, String string, Object object2) {
        try {
            DemoService demoService = (DemoService) object;
        } catch (Throwable throwable) {
            throw new IllegalArgumentException(throwable);
        }
        throw new NoSuchPropertyException(new StringBuffer().append("Not found property \"").append(string)
                .append("\" filed or setter method in class com.dubbo.simple.common.DemoService.").toString());
    }

    @Override
    public String[] getPropertyNames() {
        return pns;
    }

    public Object invokeMethod(Object object, String string, Class[] arrclass, Object[] arrobject)
            throws InvocationTargetException {
        DemoService demoService;
        try {
            demoService = (DemoService) object;
        } catch (Throwable throwable) {
            throw new IllegalArgumentException(throwable);
        }
        try {
            if ("sayGoodBye".equals(string) && arrclass.length == 1) {
                return demoService.sayGoodBye((String) arrobject[0]);
            }
            if ("sayHello".equals(string) && arrclass.length == 1) {
                return demoService.sayHello((String) arrobject[0]);
            }
        } catch (Throwable throwable) {
            throw new InvocationTargetException(throwable);
        }
        throw new NoSuchMethodException(new StringBuffer().append("Not found method \"").append(string)
                .append("\" in class com.dubbo.simple.common.DemoService.").toString());
    }

    @Override
    public String[] getMethodNames() {
        return mns;
    }

    @Override
    public String[] getDeclaredMethodNames() {
        return dmns;
    }

    @Override
    public boolean hasProperty(String string) {
        return pts.containsKey(string);
    }
}

}