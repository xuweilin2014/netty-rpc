public class DubboInjvm{

    /**
     * 
     * dubbo是一个远程调用的框架，但是它没有理由不支持本地调用，本地调用要比远程调用简单的多。
     * 
     * 使用 Dubbo 本地调用不需做特殊配置，按正常 Dubbo 服务暴露服务即可。任一服务在暴露远程服务的同时，也会同时以 injvm 的协议暴露本地服务。
     * injvm 是一个伪协议，不会像其他协议那样对外开启端口，只用于本地调用的目的。
     * 
     * 与真正的本地方法调用不同的是，Dubbo 本地调用会经过 Filter 链，其中包括了 Consumer 端的 Filter 链以及 Provider 端的 Filter 链。
     * 通过这样的机制，本地消费者和其他消费者都是统一对待，统一监控，服务统一进行治理。
     * 
     * 默认情况下，本地调用是自动开启的，不需要做额外的配置。只有只有当需要关闭的时候，才需要通过 scope 的配置来显式的关闭。但是，
     * 特别需要指出的是，在下面的几种情况下，本地调用是无法使用的.
     * 第一，泛化调用的时候无法使用本地调用。
     * 第二，消费者明确指定 URL 发起直连调用。当然，如果消费者指定的是 injvm 的 URL，最终的调用也是走本地调用的，比如：
     * <Dubbo:reference id="demoService" interface="org.apache.Dubbo.samples.local.api.DemoService" url="injvm://127.0.0.1/org.apache.Dubbo.samples.local.api.DemoService"/>
     * 
     * 本地调用是可以显式关闭的，通过这种方式，服务提供者可以做到对远端服务消费者和本地消费者一视同仁（即本地调用的流程与远端服务消费者一样）。
     * 具体做法是通过 scope="remote" 来关闭 injvm 协议的暴露，这样，即使是本地调用者，也需要从注册中心上获取服务地址列表，然后才能发起调用，
     * 而这个时候的调用过程，与远端的服务消费者的过程是一致的。
     * <bean id="target" class="org.apache.Dubbo.samples.local.impl.DemoServiceImpl"/>
     * <!-- 服务提供者指定 scope="remote" -->
     * <Dubbo:service interface="org.apache.Dubbo.samples.local.api.DemoService" ref="target" scope="remote"/>
     * <Dubbo:reference id="demoService" interface="org.apache.Dubbo.samples.local.api.DemoService"/>
     * 
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
                    // 远程导出代码省略
                }
            }
        }

        private void exportLocal(URL url) {
            if (!Constants.LOCAL_PROTOCOL.equalsIgnoreCase(url.getProtocol())) {
                // 如果说url的协议不为injvm的话，就会对url进行重新设置，也就是将协议设置为injvm，ip地址
                // 设置为localhost，port设置为0
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