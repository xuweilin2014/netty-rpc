public class DubboDirectory{

    /**
     * 服务目录中存储了一些和服务提供者有关的信息，通过服务目录，服务消费者可获取到服务提供者的信息，比如 ip、端口、服务协议等。通过这些信息，服务消费者就可通过 Netty 等客户端进行远程调用。
     * 在一个服务集群中，服务提供者数量并不是一成不变的，如果集群中新增了一台机器，相应地在服务目录中就要新增一条服务提供者记录。或者，如果服务提供者的配置修改了，服务目录中的记录也要做相应的更新。
     * 如果这样说，服务目录和注册中心的功能不就雷同了吗？确实如此，这里这么说是为了方便大家理解。实际上服务目录在获取注册中心的服务配置信息后，会为每条配置信息生成一个 Invoker 对象，
     * 并把这个 Invoker 对象存储起来，这个 Invoker 才是服务目录最终持有的对象。Invoker 有什么用呢？看名字就知道了，这是一个具有远程调用功能的对象。讲到这大家应该知道了什么是服务目录了，
     * 它可以看做是 Invoker 集合，且这个集合中的元素会随注册中心的变化而进行动态调整。
     */

    //Invocation有两个实现类：RpcInvocation和DecodeableRpcInvocation
    public interface Invocation {

        String getMethodName();
    
        Class<?>[] getParameterTypes();
    
        Object[] getArguments();
    
        Map<String, String> getAttachments();
    
        String getAttachment(String key);
    
        String getAttachment(String key, String defaultValue);
    
        Invoker<?> getInvoker();
    
    }

    /**
     * RpcInvocation是对Invoker和方法调用各项参数的一个封装，比如方法调用的名字methodName、参数类型parameterTypes、具体参数arguments
     */
    public class RpcInvocation implements Invocation, Serializable {

        private static final long serialVersionUID = -4355285085441097045L;
        // Rpc调用的方法名称
        private String methodName;
        // Rpc调用的参数类型
        private Class<?>[] parameterTypes;
        // Rpc调用的具体参数
        private Object[] arguments;
        // Rpc调用的附属信息
        private Map<String, String> attachments;

        private transient Invoker<?> invoker;

        public RpcInvocation() {
        }

        public RpcInvocation(String methodName, Class<?>[] parameterTypes, Object[] arguments, Map<String, String> attachments, Invoker<?> invoker) {
            this.methodName = methodName;
            this.parameterTypes = parameterTypes == null ? new Class<?>[0] : parameterTypes;
            this.arguments = arguments == null ? new Object[0] : arguments;
            this.attachments = attachments == null ? new HashMap<String, String>() : attachments;
            this.invoker = invoker;
        }

    }

    // Registry、Monitor、Invoker等类都实现了Node接口，这个接口包含了一个获取配置信息的方法 getUrl，实现该接口的类可以向外提供配置信息。
    public interface Node {
        URL getUrl();
    
        boolean isAvailable();

        void destroy();

    }

    // 服务目录目前内置的实现类有两个：StaticDirectory和RegistryDirectory，他们都是AbstractDirectory的子类，AbstractDirectory
    // 实现了Directory接口，这个接口中包含了一个重要方法的定义，那就是list，用于列举invoker
    public interface Directory<T> extends Node {

        Class<T> getInterface();
    
        List<Invoker<T>> list(Invocation invocation) throws RpcException;
    
    }

    public abstract class AbstractDirectory<T> implements Directory<T>{

        private volatile boolean destroyed = false;

        // AbstractDirectory 封装了 Invoker 列举流程，具体的列举逻辑则由子类实现，这是典型的模板模式。list方法的流程如下：
        // 1.调用 doList 获取 Invoker 列表
        // 2.根据 Router 的 getUrl 返回值为空与否，以及 runtime 参数决定是否进行服务路由
        public List<Invoker<T>> list(Invocation invocation) throws RpcException {
            if (destroyed) {
                throw new RpcException("Directory already destroyed .url: " + getUrl());
            }

            // 调用 doList 方法列举 Invoker，doList 是模板方法，由子类实现
            List<Invoker<T>> invokers = doList(invocation);
            // 获取路由 Router 列表
            List<Router> localRouters = this.routers; // local reference
            if (localRouters != null && localRouters.size() > 0) {
                for (Router router : localRouters) {
                    try {
                        // 获取 runtime 参数，并根据参数决定是否进行路由
                        // runtime 参数这里简单说明一下，这个参数决定了是否在每次调用服务时都执行路由规则。如果 runtime 为 true，
                        // 那么每次调用服务前，都需要进行服务路由。这个对性能造成影响，配置时需要注意。
                        if (router.getUrl() == null || router.getUrl().getParameter(Constants.RUNTIME_KEY, false)) {
                            // 进行服务路由
                            invokers = router.route(invokers, getConsumerUrl(), invocation);
                        }
                    } catch (Throwable t) {
                        logger.error("Failed to execute router: " + getUrl() + ", cause: " + t.getMessage(), t);
                    }
                }
            }
            return invokers;
        }

        public void destroy() {
            destroyed = true;
        }

        // 模板方法，由子类实现
        protected abstract List<Invoker<T>> doList(Invocation invocation) throws RpcException;

    }

    //StaticDirectory 即静态服务目录，顾名思义，它内部存放的 Invoker 是不会变动的。所以，理论上它和不可变 List 的功能很相似。
    public class StaticDirectory<T> extends AbstractDirectory<T> {

        private final List<Invoker<T>> invokers;
    
        //省略其它构造方法

        public StaticDirectory(URL url, List<Invoker<T>> invokers, List<Router> routers) {
            super(url == null && invokers != null && invokers.size() > 0 ? invokers.get(0).getUrl() : url, routers);
            if (invokers == null || invokers.size() == 0)
                throw new IllegalArgumentException("invokers == null");
            this.invokers = invokers;
        }
    
        public Class<T> getInterface() {
            return invokers.get(0).getInterface();
        }
    
        // 检测服务目录是否可用
        public boolean isAvailable() {
            if (isDestroyed()) {
                return false;
            }
            for (Invoker<T> invoker : invokers) {
                if (invoker.isAvailable()) {
                    // 只要有一个 Invoker 是可用的，就认为当前目录是可用的
                    return true;
                }
            }
            return false;
        }
    
        public void destroy() {
            if (isDestroyed()) {
                return;
            }
            // 调用父类销毁逻辑
            super.destroy();
            // 遍历 Invoker 列表，并执行相应的销毁逻辑
            for (Invoker<T> invoker : invokers) {
                invoker.destroy();
            }
            invokers.clear();
        }
    
        //直接返回StaticDirectory中所持有的invokers
        @Override
        protected List<Invoker<T>> doList(Invocation invocation) throws RpcException {
            return invokers;
        }
    
    }

    /**
     * RegistryDirectory 可以看成是一个注册在一个注册中心上一个服务目录下的监听器，并且还含有Invoker的集合。它会根据服务目录下子节点的变化（增加或者删除）来动态地
     * 修改它持有的Invoker集合。
     * 
     * RegistryDirectory 是一种动态服务目录，实现了 NotifyListener 接口。当注册中心服务配置发生变化后，RegistryDirectory 可收到与当前服务相关的变化。收到变更通知后，
     * RegistryDirectory 可根据配置变更信息刷新 Invoker 列表。RegistryDirectory 中有几个比较重要的逻辑:
     * 第一是 Invoker 的列举逻辑，第二是接收服务配置变更的逻辑，第三是 Invoker 列表的刷新逻辑。
     */
    public class RegistryDirectory<T> extends AbstractDirectory<T> implements NotifyListener{
        
        private volatile Map<String, Invoker<T>> urlInvokerMap;

        public List<Invoker<T>> doList(Invocation invocation) {
            if (forbidden) {
                // 服务提供者禁用了服务或者没有对应的服务提供者，此时抛出 No provider 异常
                throw new RpcException(RpcException.FORBIDDEN_EXCEPTION,
                    "No provider available from registry " + getUrl().getAddress() + " for service " + getConsumerUrl().getServiceKey() + " on consumer " +  NetUtils.getLocalHost()
                        + " use dubbo version " + Version.getVersion() + ", may be providers disabled or not registered ?");
            }

            List<Invoker<T>> invokers = null;
            // 获取 Invoker 本地缓存
            Map<String, List<Invoker<T>>> localMethodInvokerMap = this.methodInvokerMap; // local reference
            if (localMethodInvokerMap != null && localMethodInvokerMap.size() > 0) {
                // 获取方法名和参数列表
                String methodName = RpcUtils.getMethodName(invocation);
                Object[] args = RpcUtils.getArguments(invocation);
                // 检测参数列表的第一个参数是否为 String 或 enum 类型
                if (args != null && args.length > 0 && args[0] != null
                        && (args[0] instanceof String || args[0].getClass().isEnum())) {
                    invokers = localMethodInvokerMap.get(methodName + "." + args[0]); // The routing can be enumerated according to the first parameter
                }

                // 通过方法名获取 Invoker 列表
                if (invokers == null) {
                    invokers = localMethodInvokerMap.get(methodName);
                }

                // 通过星号 * 获取Invokers列表
                if (invokers == null) {
                    //Constants.ANY_VALUE的值为 *
                    invokers = localMethodInvokerMap.get(Constants.ANY_VALUE);
                }
            }
            return invokers == null ? new ArrayList<Invoker<T>>(0) : invokers;
        }

        // RegistryDirectory 是一个动态服务目录，会随注册中心配置的变化进行动态调整。因此 RegistryDirectory 实现了 NotifyListener 接口，
        // 通过这个接口获取注册中心变更通知。
        // 
        // 如上，notify 方法首先是根据 url 的 category 参数对 url 进行分门别类存储，然后通过 toRouters 和 toConfigurators 将 url 列表转成 Router 和 Configurator 列表。
        // 最后调用 refreshInvoker 方法刷新 Invoker 列表。
        public synchronized void notify(List<URL> urls) {
            // 定义三个集合，分别用于存放服务提供者 url，路由 url，配置器 url
            List<URL> invokerUrls = new ArrayList<URL>();
            List<URL> routerUrls = new ArrayList<URL>();
            List<URL> configuratorUrls = new ArrayList<URL>();

            for (URL url : urls) {
                String protocol = url.getProtocol();
                //用来获取url中的category参数，默认是providers。在zookeeper中，注册中心的目录有4种：providers、routers、configurators、consumers
                String category = url.getParameter(Constants.CATEGORY_KEY, Constants.DEFAULT_CATEGORY);
                // 根据 category 参数将 url 分别放到不同的列表中
                if (Constants.ROUTERS_CATEGORY.equals(category)
                        || Constants.ROUTE_PROTOCOL.equals(protocol)) {
                    // 添加路由器 url
                    routerUrls.add(url);
                } else if (Constants.CONFIGURATORS_CATEGORY.equals(category)
                        || Constants.OVERRIDE_PROTOCOL.equals(protocol)) {
                    // 添加配置器 url
                    configuratorUrls.add(url);
                } else if (Constants.PROVIDERS_CATEGORY.equals(category)) {
                    // 添加服务提供者 url
                    invokerUrls.add(url);
                } else {
                    // 忽略不支持的 category
                    logger.warn("Unsupported category " + category + " in notified url: " + url + " from registry " + getUrl().getAddress() + " to consumer " + NetUtils.getLocalHost());
                }
            }

            // configurators
            if (configuratorUrls != null && configuratorUrls.size() > 0) {
                // 将url转变为configurator
                this.configurators = toConfigurators(configuratorUrls);
            }
            // routers
            if (routerUrls != null && routerUrls.size() > 0) {
                List<Router> routers = toRouters(routerUrls);
                if (routers != null) { // null - do nothing
                    // 将url转变为router
                    setRouters(routers);
                }
            }

            // providers
            // 刷新 Invoker 列表
            refreshInvoker(invokerUrls);
        }

        /**
         * refreshInvoker 方法执行流程：
         * 1.根据入参 invokerUrls 的数量和协议头判断是否禁用所有的服务，如果禁用，则销毁所有的 Invoker。
         * 2.若不禁用，则更新本地的 url 缓存
         * 3.将 url 转成 Invoker，得到 <url, Invoker> 的映射关系。
         * 4.进一步进行转换，得到 <methodName, Invoker 列表> 映射关系。然后进行多组 Invoker 合并操作，并且将合并操作的结果赋值给 methodInvokerMap，
         * 这个 methodInvokerMap 在 doList 方法中会用到，doList 方法中会对该方法进行读操作，在这里是写操作。
         * 5.销毁无用的 Invoker，避免服务消费者调用已下线的服务的服务。
         */
        private void refreshInvoker(List<URL> invokerUrls) {
            // Constants.EMPTY_PROTOCOL的字符串值为empty
            // 首先会根据入参 invokerUrls 的数量和协议头是否为 empty 来判断是否禁用所有的服务，如果禁用，则将 forbidden 设为 true，并销毁所有的 Invoker
            // 也就是说，如果 invokerUrls 集合中只有一个 url，并且这个 url 的协议为 empty，那么说明没有provider（在providers目录下的子节点为null）或者
            // 禁用所有服务。
            if (invokerUrls != null && invokerUrls.size() == 1 && invokerUrls.get(0) != null
                    && Constants.EMPTY_PROTOCOL.equals(invokerUrls.get(0).getProtocol())) {
                // 设置 forbidden 为 true
                this.forbidden = true; 
                // 销毁所有 Invoker
                this.methodInvokerMap = null; 
                destroyAllInvokers(); 
            } else {
                this.forbidden = false; // Allow to access
                Map<String, Invoker<T>> oldUrlInvokerMap = this.urlInvokerMap; // local reference
                if (invokerUrls.size() == 0 && this.cachedInvokerUrls != null) {
                    // 如果 invokerUrls 为空的话，就添加本地缓存的 url 到 invokerUrls 中
                    invokerUrls.addAll(this.cachedInvokerUrls);
                } else {
                    this.cachedInvokerUrls = new HashSet<URL>();
                    // 如果 invokerUrls 不为空的话，就将 invokerUrls 缓存到 cachedInvokerUrls 中去，从而更新本地缓存 
                    this.cachedInvokerUrls.addAll(invokerUrls);//Cached invoker urls, convenient for comparison
                }
                if (invokerUrls.size() == 0) {
                    return;
                }
                // 将 url 转换成 Invoker，得到 <url, Invoker> 的映射关系
                Map<String, Invoker<T>> newUrlInvokerMap = toInvokers(invokerUrls);// Translate url list to Invoker map
                // 将 newUrlInvokerMap 转成 <methodName, Invoker列表> 的映射关系
                Map<String, List<Invoker<T>>> newMethodInvokerMap = toMethodInvokers(newUrlInvokerMap); // Change method name to map Invoker Map

                // 转换出错，直接打印异常，并返回
                if (newUrlInvokerMap == null || newUrlInvokerMap.size() == 0) {
                    logger.error(new IllegalStateException("urls to invokers error .invokerUrls.size :" + invokerUrls.size() + ", invoker.size :0. urls :" + invokerUrls.toString()));
                    return;
                }

                // 合并多个组的 Invoker
                this.methodInvokerMap = multiGroup ? toMergeMethodInvokerMap(newMethodInvokerMap) : newMethodInvokerMap;
                // 使用最新的 invokerUrls 来获取新的 newUrlInvokerMap，并且赋值给本地的 urlInvokerMap 进行更新
                this.urlInvokerMap = newUrlInvokerMap;
                try {
                    // 销毁无用的 Invoker
                    destroyUnusedInvokers(oldUrlInvokerMap, newUrlInvokerMap); // Close the unused Invoker
                } catch (Exception e) {
                    logger.warn("destroyUnusedInvokers error. ", e);
                }
            }
        }

        //url中的各种配置参数，在RegistryDirectory的构造函数中初始化
        private final Map<String, String> queryMap; // Initialization at construction time, assertion not null

        private Map<String, Invoker<T>> toInvokers(List<URL> urls) {
            Map<String, Invoker<T>> newUrlInvokerMap = new HashMap<String, Invoker<T>>();
            if (urls == null || urls.size() == 0) {
                return newUrlInvokerMap;
            }
            Set<String> keys = new HashSet<String>();
            // 获取服务消费端配置的协议
            String queryProtocols = this.queryMap.get(Constants.PROTOCOL_KEY);

            for (URL providerUrl : urls) {
                // If protocol is configured at the reference side, only the matching protocol is selected
                // 如果在消费者一端配置了具体的协议的话，那么只会选择满足这个协议的 providerUrl
                if (queryProtocols != null && queryProtocols.length() > 0) {
                    boolean accept = false;
                    String[] acceptProtocols = queryProtocols.split(",");
                    for (String acceptProtocol : acceptProtocols) {
                        // 检测 providerUrl 中的协议是否被服务消费者所支持
                        if (providerUrl.getProtocol().equals(acceptProtocol)) {
                            accept = true;
                            break;
                        }
                    }
                    // 如果不支持，那么就直接考虑下一个 providerUrl
                    if (!accept) {
                        continue;
                    }
                }
                // 忽略 empty 协议
                if (Constants.EMPTY_PROTOCOL.equals(providerUrl.getProtocol())) {
                    continue;
                }
                // 通过 SPI 检测服务端协议是否被消费端支持，不支持则抛出异常
                if (!ExtensionLoader.getExtensionLoader(Protocol.class).hasExtension(providerUrl.getProtocol())) {
                    logger.error(new IllegalStateException("Unsupported protocol " + providerUrl.getProtocol() + " in notified url: " + providerUrl + " from registry " + getUrl().getAddress() + " to consumer " + NetUtils.getLocalHost()
                            + ", supported protocol: " + ExtensionLoader.getExtensionLoader(Protocol.class).getSupportedExtensions()));
                    continue;
                }
                URL url = mergeUrl(providerUrl);
    
                String key = url.toFullString(); // The parameter urls are sorted
                if (keys.contains(key)) { // Repeated url
                    continue;
                }
                keys.add(key);
                // 将本地 Invoker 缓存赋值给 localUrlInvokerMap
                Map<String, Invoker<T>> localUrlInvokerMap = this.urlInvokerMap; // local reference
                // 获取与 url 对应的 Invoker
                Invoker<T> invoker = localUrlInvokerMap == null ? null : localUrlInvokerMap.get(key);
                // 缓存未命中的话，就重新创建一个invoker对象，放入到newUrlInvokerMap中
                if (invoker == null) { // Not in the cache, refer again
                    try {
                        boolean enabled = true;
                        if (url.hasParameter(Constants.DISABLED_KEY)) {
                            // 获取 disable 配置，取反，然后赋值给 enable 变量
                            enabled = !url.getParameter(Constants.DISABLED_KEY, false);
                        } else {
                            // 获取 enable 配置，并赋值给 enable 变量
                            enabled = url.getParameter(Constants.ENABLED_KEY, true);
                        }
                        if (enabled) {
                            // 调用 refer 获取 Invoker，比如 DubboInvoker 对象，这里的 Invoker 对象可以向发起远程服务调用，
                            invoker = new InvokerDelegate<T>(protocol.refer(serviceType, url), url, providerUrl);
                        }
                    } catch (Throwable t) {
                        logger.error("Failed to refer invoker for interface:" + serviceType + ",url:(" + url + ")" + t.getMessage(), t);
                    }

                    if (invoker != null) { // Put new invoker in cache
                        newUrlInvokerMap.put(key, invoker);
                    }
                } else {
                    newUrlInvokerMap.put(key, invoker);
                }
            }
            keys.clear();
            return newUrlInvokerMap;
        }
    }




}