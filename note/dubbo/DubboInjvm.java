public class DubboInjvm{

    /**
     * 
     * dubbo是一个远程调用的框架，但是它没有理由不支持本地调用，本地调用要比远程调用简单的多。
     * 
     * 使用 Dubbo 本地调用不需做特殊配置，按正常 Dubbo 服务暴露服务即可。任一服务在暴露远程服务的同时，也会同时以 injvm 的协议暴露本地服务。injvm 是一个伪协议，
     * 不会像其他协议那样对外开启端口，只用于本地调用的目的。
     * 
     * 与真正的本地方法调用不同的是，Dubbo 本地调用会经过 Filter 链，其中包括了 Consumer 端的 Filter 链以及 Provider 端的 Filter 链。通过这样的机制，本地消费者和
     * 其他消费者都是统一对待，统一监控，服务统一进行治理。
     * 
     * 默认情况下，本地调用是自动开启的，不需要做额外的配置。只有只有当需要关闭的时候，才需要通过 scope 的配置来显式的关闭。但是，特别需要指出的是，在下面的几种情况下，
     * 本地调用是无法使用的：
     * 第一，泛化调用的时候无法使用本地调用。
     * 第二，消费者明确指定 URL 发起直连调用。当然，如果消费者指定的是 injvm 的 URL，最终的调用也是走本地调用的，比如：
     * 
     * <Dubbo:reference id="demoService" interface="org.apache.Dubbo.samples.local.api.DemoService" url="injvm://127.0.0.1/org.apache.Dubbo.samples.local.api.DemoService"/>
     * 
     * 本地调用是可以显式关闭的，通过这种方式，服务提供者可以做到对远端服务消费者和本地消费者一视同仁（即本地调用的流程与远端服务消费者一样）。
     * 具体做法是通过 scope="remote" 来关闭 injvm 协议的暴露，这样，即使是本地调用者，也需要从注册中心上获取服务地址列表，然后才能发起调用，
     * 而这个时候的调用过程，与远端的服务消费者的过程是一致的。
     * 
     * <bean id="target" class="org.apache.Dubbo.samples.local.impl.DemoServiceImpl"/>
     * <!-- 服务提供者指定 scope="remote" -->
     * <Dubbo:service interface="org.apache.Dubbo.samples.local.api.DemoService" ref="target" scope="remote"/>
     * <Dubbo:reference id="demoService" interface="org.apache.Dubbo.samples.local.api.DemoService"/>
     * 
     */

    public static class InjvmDemo {
        public static void main(String[] args) {
            ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("classpath:consumer.xml");
            context.start();
    
            DemoService demoService = (DemoService) context.getBean("demoService");
            String s = demoService.sayHello("world");
            System.out.println(s);
        }
    }

    /**
     * 下面我们以如下配置来说明本地调用的流程
     * 
     * <dubbo:application name="consumer-of-helloworld-app"  />
     * <dubbo:registry address="zookeeper://127.0.0.1:2181" />
     * <dubbo:protocol name="injvm" port="20880" />
     * <!-- 提供者端配置 -->
     * <dubbo:service interface="com.dubbo.simple.common.DemoService" ref="demo"/>
     * <bean id="demo" class="com.dubbo.simple.server.DemoServiceImpl"/>
     * <!-- 消费者端配置 -->
     * <dubbo:reference id="demoService" interface="com.dubbo.simple.common.DemoService" />
     * 
     * 具体的示例代码如上面的InjvmDemo所示。
     * 
     * 首先也是和远程调用一样先进行服务导出（在ServiceConfig中），只不过在远程调用中，还会默认进行本地导出，即调用ServiceConfig中的
     * exportLocal方法，将url中的协议替换为injvm，然后根据自适应拓展获取到InjvmProtocol，进而生成InjvmExporter对象。而我们进行本地调用时，由于url为injvm://...，
     * 因此不会多此一举调用exportLocal方法，直接在原本进行远程服务导出的代码中，根据自适应拓展功能，也会获取到InjvmProtocol，生成InjvmExporter，并且这生成的exporter会放在
     * exportedMap中。
     * 
     * 进行服务引用的流程也和远程调用一样，在ReferenceConfig#createProxy方法中，如果是本地调用的话，就直接通过 refprotocol.refer 生成一个InjvmInvoker，并且
     * 传入ProxyFactory#getProxy方法中，生成一个远程服务代理。
     * 
     * 在调用InjvmInvoker的invoke方法时，和远程调用的过程不同，远程调用生成的Invoker的invoke方法是向远程服务器发起调用，而在InjvmInvoker的invoke方法中，首先通过exportedMap获取到
     * 和url对应的invoker，它是直接获取到exporter中的invoker对象，从而调用其invoke方法。注意在exporter中的invoker对象一般都是通过ProxyFactory类中的
     * getInvoker方法获取到的，它会真正调用执行具体的方法（比如DemoServiceImpl的sayHello方法）。也就是说，通过本地调用没有经过网络传输的过程，不过还是会经过一系列的拦截器
     */

    public static abstract class AbstractProtocol implements Protocol{

        // 服务暴露集合
        protected final Map<String, Exporter<?>> exporterMap = new ConcurrentHashMap<String, Exporter<?>>();

        protected final Set<Invoker<?>> invokers = new ConcurrentHashSet<Invoker<?>>();

        protected static String serviceKey(URL url) {
            return ProtocolUtils.serviceKey(url);
        }
    }

    public static class InjvmProtocol extends AbstractProtocol implements Protocol {

        public static final String NAME = Constants.LOCAL_PROTOCOL;
    
        public static final int DEFAULT_PORT = 0;

        private static InjvmProtocol INSTANCE;
    
        public InjvmProtocol() {
            INSTANCE = this;
        }
    
        public static InjvmProtocol getInjvmProtocol() {
            if (INSTANCE == null) {
                ExtensionLoader.getExtensionLoader(Protocol.class).getExtension(InjvmProtocol.NAME); // load
            }
            return INSTANCE;
        }
    
        static Exporter<?> getExporter(Map<String, Exporter<?>> map, URL key) {
            Exporter<?> result = null;
    
            if (!key.getServiceKey().contains("*")) {
                result = map.get(key.getServiceKey());
            } else {
                if (map != null && !map.isEmpty()) {
                    for (Exporter<?> exporter : map.values()) {
                        if (UrlUtils.isServiceKeyMatch(key, exporter.getInvoker().getUrl())) {
                            result = exporter;
                            break;
                        }
                    }
                }
            }
    
            if (result == null) {
                return null;
            } else if (ProtocolUtils.isGeneric(
                    result.getInvoker().getUrl().getParameter(Constants.GENERIC_KEY))) {
                return null;
            } else {
                return result;
            }
        }
    
        public int getDefaultPort() {
            return DEFAULT_PORT;
        }
    
        public <T> Exporter<T> export(Invoker<T> invoker) throws RpcException {
            return new InjvmExporter<T>(invoker, invoker.getUrl().getServiceKey(), exporterMap);
        }
    
        public <T> Invoker<T> refer(Class<T> serviceType, URL url) throws RpcException {
            return new InjvmInvoker<T>(serviceType, url, url.getServiceKey(), exporterMap);
        }
    
        public boolean isInjvmRefer(URL url) {
            final boolean isJvmRefer;
            // 获取url中scope参数的值
            String scope = url.getParameter(Constants.SCOPE_KEY);
            // Since injvm protocol is configured explicitly, we don't need to set any extra flag, use normal refer process.
            // 判断url的协议是否为injvm
            if (Constants.LOCAL_PROTOCOL.toString().equals(url.getProtocol())) {
                isJvmRefer = false;
            } else if (Constants.SCOPE_LOCAL.equals(scope) || (url.getParameter("injvm", false))) {
                // if it's declared as local reference
                // 'scope=local' is equivalent to 'injvm=true', injvm will be deprecated in the future release
                // 如果scope配置的为local，或者injvm配置为true，那么表明使用本地调用
                isJvmRefer = true;
            } else if (Constants.SCOPE_REMOTE.equals(scope)) {
                // it's declared as remote reference
                // 如果在<dubbo:reference/>标签中指明scope为remote，那么明确表明不使用本地服务，使用远程服务
                isJvmRefer = false;
            } else if (url.getParameter(Constants.GENERIC_KEY, false)) {
                // generic invocation is not local reference
                // 使用泛化服务时，不使用本地调用
                isJvmRefer = false;
            } else if (getExporter(exporterMap, url) != null) {
                // by default, go through local reference if there's the service exposed locally
                isJvmRefer = true;
            } else {
                isJvmRefer = false;
            }
            return isJvmRefer;
        }
    }


    public class ServiceConfig<T> extends AbstractServiceConfig {

        private void doExportUrlsFor1Protocol(ProtocolConfig protocolConfig, List<URL> registryURLs) {
            // 获取url中配置的scope信息
            String scope = url.getParameter(Constants.SCOPE_KEY);
            // 如果socpe为none，则不对服务进行导出（本地导出和远程导出都不启用）
            if (!Constants.SCOPE_NONE.toString().equalsIgnoreCase(scope)) {
                // export to local if the config is not remote (export to remote only when config is remote)
                // dubbo默认会在对服务进行远程导出之前进行本地导出，除非显式地指定scope=remote，这样就不会进行本地导出
                if (!Constants.SCOPE_REMOTE.toString().equalsIgnoreCase(scope)) {
                    exportLocal(url);
                }

                // export to remote if the config is not local (export to local only when config is local)
                // 当scope配置为local时，则只进行本地导出，不进行远程导出
                if (!Constants.SCOPE_LOCAL.toString().equalsIgnoreCase(scope)) {
                    // 远程导出其它代码省略
                    if (registryURLs != null && registryURLs.size() > 0) {
                        for (URL registryURL : registryURLs) {
                            url = url.addParameterIfAbsent("dynamic", registryURL.getParameter("dynamic"));
                            URL monitorUrl = loadMonitor(registryURL);
                            if (monitorUrl != null) {
                                url = url.addParameterAndEncoded(Constants.MONITOR_KEY, monitorUrl.toFullString());
                            }
                            if (logger.isInfoEnabled()) {
                                logger.info("Register dubbo service " + interfaceClass.getName() + " url " + url + " to registry " + registryURL);
                            }
                            Invoker<?> invoker = proxyFactory.getInvoker(ref, (Class) interfaceClass, registryURL.addParameterAndEncoded(Constants.EXPORT_KEY, url.toFullString()));
                            DelegateProviderMetaDataInvoker wrapperInvoker = new DelegateProviderMetaDataInvoker(invoker, this);
    
                            Exporter<?> exporter = protocol.export(wrapperInvoker);
                            exporters.add(exporter);
                        }
                    }
                }
            }
        }

        private void exportLocal(URL url) {
            if (!Constants.LOCAL_PROTOCOL.equalsIgnoreCase(url.getProtocol())) {
                // 如果说url的协议不为injvm的话，就会对url进行重新设置，也就是将协议设置为injvm，ip地址设置为localhost，port设置为0
                // 这么做的目的是在下面调用protocol的export方法时使用InjvmProtocol的export方法，生成InjvmExporter。由于protocol的类型为Protocol$Adaptive，
                // 因此会根据url协议的类型，自动选择对应的拓展
                URL local = URL.valueOf(url.toFullString())
                        .setProtocol(Constants.LOCAL_PROTOCOL)
                        .setHost(LOCALHOST)
                        .setPort(0);
                ServiceClassHolder.getInstance().pushServiceClass(getServiceClass(ref));
                Exporter<?> exporter = protocol.export(proxyFactory.getInvoker(ref, (Class) interfaceClass, local));
                exporters.add(exporter);
                logger.info("Export dubbo service " + interfaceClass.getName() + " to local registry");
            }
        }

    }

    public class ReferenceConfig<T> extends AbstractReferenceConfig{

        private T createProxy(Map<String, String> map) {
            URL tmpUrl = new URL("temp", "localhost", 0, map);
            final boolean isJvmRefer;
            if (isInjvm() == null) {
                if (url != null && url.length() > 0) { // if a url is specified, don't do local reference
                    isJvmRefer = false;
                } else if (InjvmProtocol.getInjvmProtocol().isInjvmRefer(tmpUrl)) {
                    // by default, reference local service if there is
                    isJvmRefer = true;
                } else {
                    isJvmRefer = false;
                }
            } else {
                isJvmRefer = isInjvm().booleanValue();
            }
    
            // 如果是本地调用的话，就使用InjvmProtocol生成一个InjvmInvoker
            if (isJvmRefer) {
                URL url = new URL(Constants.LOCAL_PROTOCOL, NetUtils.LOCALHOST, 0, interfaceClass.getName()).addParameters(map);
                invoker = refprotocol.refer(interfaceClass, url);
                if (logger.isInfoEnabled()) {
                    logger.info("Using injvm service " + interfaceClass.getName());
                }
            } else {
                // 代码省略
            }
    
            // 代码省略

            // create service proxy
            // 将前面生成的InjvmInvoker，使用ProxyFactory类中的getProxy包装成一个远程服务代理，然后返回
            // 在这里本地调用与远程调用不同的是，远程调用生成的Invoker的doInvoke方法是向远程服务器发起调用，而在InjvmInvoker的doInvoke方法中，
            // 它是直接获取到exporter中的invoker对象，从而调用其invoke方法。注意在exporter中的invoker对象一般都是通过ProxyFactory类中的
            // getInvoker方法获取到的，它会真正调用执行具体的方法，也就是说，通过本地调用没有经过网络传输的过程，不过还是会经过一系列的
            // 拦截器
            return (T) proxyFactory.getProxy(invoker);
        }

    }

    public abstract class AbstractExporter<T> implements Exporter<T> {

        protected final Logger logger = LoggerFactory.getLogger(getClass());
    
        private final Invoker<T> invoker;
    
        private volatile boolean unexported = false;
    
        public AbstractExporter(Invoker<T> invoker) {
            if (invoker == null)
                throw new IllegalStateException("service invoker == null");
            if (invoker.getInterface() == null)
                throw new IllegalStateException("service type == null");
            if (invoker.getUrl() == null)
                throw new IllegalStateException("service url == null");
            this.invoker = invoker;
        }
    
        public Invoker<T> getInvoker() {
            return invoker;
        }
    
        public void unexport() {
            if (unexported) {
                return;
            }
            unexported = true;
            getInvoker().destroy();
        }
    
        public String toString() {
            return getInvoker().toString();
        }
    
    }

    /**
     * 生成Exporter的过程和远程调用相同，只不过在为远程调用进行服务导出生成Exporter时，一般会默认也同时进行本地导出。但如果配置进行本地调用的话，
     * （也就是url为injvm://....），那么就不会再多此一举调用ServiceConfig#exportLocal方法，而是会直接调用protocol.export生成一个Exporter。
     * 这个Exporter中封装了一个ProxyFactory生成的Invoker对象（用于执行具体的调用逻辑）。
     */
    class InjvmExporter<T> extends AbstractExporter<T> {

        private final String key;
    
        private final Map<String, Exporter<?>> exporterMap;
    
        InjvmExporter(Invoker<T> invoker, String key, Map<String, Exporter<?>> exporterMap) {
            super(invoker);
            this.key = key;
            this.exporterMap = exporterMap;
            exporterMap.put(key, this);
        }
    
        public void unexport() {
            super.unexport();
            exporterMap.remove(key);
        }
    
    }

    /**
     * 在InjvmInvoker中，和其它的Invoker（比如DubboInvoker）不同，这个InjvmInvoker是直接从exporterMap中获取到url对应的exporter，
     * 然后再获取到exporter中封装的invoker调用其invoke方法。这个invoker是ProxyFactory生成的一个对象，它会调用执行具体的方法。
     */
    class InjvmInvoker<T> extends AbstractInvoker<T> {

        private final String key;
    
        private final Map<String, Exporter<?>> exporterMap;
    
        InjvmInvoker(Class<T> type, URL url, String key, Map<String, Exporter<?>> exporterMap) {
            super(type, url);
            this.key = key;
            this.exporterMap = exporterMap;
        }
    
        @Override
        public boolean isAvailable() {
            InjvmExporter<?> exporter = (InjvmExporter<?>) exporterMap.get(key);
            if (exporter == null) {
                return false;
            } else {
                return super.isAvailable();
            }
        }
    
        public Result doInvoke(Invocation invocation) throws Throwable {
            Exporter<?> exporter = InjvmProtocol.getExporter(exporterMap, getUrl());
            if (exporter == null) {
                throw new RpcException("Service [" + key + "] not found.");
            }
            RpcContext.getContext().setRemoteAddress(NetUtils.LOCALHOST, 0);
            return exporter.getInvoker().invoke(invocation);
        }

    }

}