public class DubboReference2{

    /**
     * /META-INF/dubbo/internal/com.alibaba.dubbo.remoting.exchange.Exchanger文件内容如下：
     * header=com.alibaba.dubbo.remoting.exchange.support.header.HeaderExchanger
     * 
     * /META-INF/dubbo/com.alibaba.dubbo.remoting.Transporter文件内容如下：
     * netty=com.alibaba.dubbo.remoting.transport.netty.NettyTransporter
     * netty4=com.alibaba.dubbo.remoting.transport.netty4.NettyTransporter
     * mina=com.alibaba.dubbo.remoting.transport.mina.MinaTransporter
     * grizzly=com.alibaba.dubbo.remoting.transport.grizzly.GrizzlyTransporter
     * 
     */

    /**
     * 主要介绍远程通信层：Exchanger层和Transporter层
     */

    /**
     * 共享连接就是一个客户端到一个特定地址（ip地址+端口号）的只有一个连接，这个客户端的所有Invoker如果要向这个地址发起请求，都必须使用这个连接。
     * 如果不是共享连接，那么每个Invoker都会创建connections（根据用户配置）个到特定地址的连接，不会对连接进行复用
     */

    /**
     * 服务器Server在接收到消息之后，根据不同的派发策略，或者将其派发到线程池中去处理，或者直接在IO线程上进行处理。
     * Dispatcher 就是线程派发器。需要说明的是，Dispatcher 真实的职责创建具有线程派发能力的 ChannelHandler，比如 AllChannelHandler、MessageOnlyChannelHandler 
     * 和 ExecutionChannelHandler 等，其本身并不具备线程派发能力。Dubbo 支持 5 种不同的线程派发策略，不过默认使用第二种 AllDispatcher 策略。如下所示：
     * 
     * DirectDispatcher：所有消息都不派发到线程池，全部在 IO 线程上直接执行
     * AllDispatcher：所有消息都派发到线程池，包括请求，响应，连接事件，断开事件等
     * MessageOnlyDispatcher：只有【请求】和【响应】消息派发到线程池，其它消息均在 IO 线程上执行
     * ExecutionDispatcher：只有【请求】消息派发到线程池，不含响应。其它消息均在 IO 线程上执行
     * ConnectionOrderedDispatcher：在 IO 线程上，将连接断开事件放入队列，有序逐个执行，其它消息派发到线程池
     * 
     * Dispatcher接口是调度器接口，dispatch是线程池的调度方法，这边有几个注意点：
     * 
     * 1.该接口是一个可扩展接口，并且默认实现AllDispatcher，也就是所有消息都派发到线程池，包括请求，响应，连接事件，断开事件，心跳等。
     * 2.用了Adaptive注解，也就是按照URL中配置来加载实现类，后面两个参数是为了兼容老版本，如果这是三个key对应的值都为空，就选择AllDispatcher来实现。
     */

    /**
     * Dubbo心跳检测
     * 
     * 一个合格的心跳方案应该满足如下两个要求：
     * 
     * 1.心跳检测需要容错
     * 
     * 网络通信永远要考虑到最坏的情况，一次心跳失败，不能认定为连接不通，多次心跳失败，才能采取相应的措施。在 dubbo 中，默认是在 3 个心跳时间间隔内，
     * 如果另一方没有发送心跳包或者其它数据，那么就会采取相应的措施。如果是客户端的话，就会进行重连操作；如果是服务器端，就会直接关闭连接。具体可看 HeartbeatTask 的相关代码。
     * 
     * 2.心跳检测不需要忙等待
     * 
     * 忙检测的对立面是空闲检测，我们做心跳的初衷，是为了保证连接的可用性，以保证及时采取断连，重连等措施。如果一条通道上有频繁的 RPC 调用正在进行，
     * 我们不应该为通道增加负担去发送心跳包。心跳扮演的角色应当是晴天收伞，雨天送伞。Dubbo 采取的是双向心跳设计，即服务端会向客户端发送心跳，客户端也会向服务端发送心跳，
     * 接收的一方更新 lastRead 字段，发送的一方更新 lastWrite 字段，超过心跳间隙的时间，便发送心跳请求给对端。这里的 lastRead/lastWrite 同样会被同一个通道上的普通调用更新，
     * 通过更新这两个字段，实现了只在连接空闲时才会真正发送空闲报文的机制。
     * 
     * 不仅仅心跳请求会更新 lastRead 和 lastWrite，普通请求也会。这对应了我们预备知识中的空闲检测机制
     * 
     * 总结：
     * 
     * 1.心跳请求发送以及超时处理
     * 
     * Dubbo 对于建立的每一个连接，同时在客户端和服务端使用 ScheduledThreadPoolExecutor 开启了1个定时任务 HeartbeatTask，用于定时发送心跳以及定时重连、断连，
     * 在服务端和客户端开启的任务相同。
     * 
     * 定时发送心跳的任务负责在连接空闲时，向对端发送心跳包。定时重连、断连的任务负责检测 lastRead 是否在超时周期内仍未被更新，如果判定为超时，
     * 客户端处理的逻辑是重连，服务端则采取断连的措施。
     * 
     * 2.心跳请求的接收与处理
     * 
     * 在请求接收的Handler处理链路中，包含有一个 心跳消息处理器HeartbeatHandler，对于所有类型的请求消息，该处理器都会更新对应通道中 最近一次接收消息的时间。
     * 对于心跳请求消息，该处理器接收心跳请求并构建对应的心跳响应通过通道Channel发送回去；对于心跳响应消息，则直接返回；对于其他数据，则交给另外的handler进行处理。
     */

    /**
     * 连接服务器失败进行重连操作（创建 NettyClient 或者心跳超时后重连的时候）
     * 
     * 在前面心跳机制的代码中，客户端与服务器通过是否接收到对方发送的心跳包或者其它数据，来判断连接有否有效或者存活，如果超过一定时间没有接收到的话，就会进行重
     * 连操作。
     * 
     * 在创建NettyClient，并且连接服务器的过程中，还会创建一个重连线程（reconnect thread）。它会定时检测channel是否出于连接状态（代码层面，通过 SocketChannelImpl 
     * 中 state 变量的值来判断是否连接）。如果连接出于断开状态，则会调用doConnect进行重连操作，当重连次数达到一定值还没有成功时，就会打印警告和错误日志。
     * 
     * 心跳和重连线程可以看作是相互补充，其中重连线程通过代码层面的判断 channel 是否出于连接状态（不过比如直接拔网线等行为，应用层是接收不到这个信号的，
     * 所以netty框架还会认为这个链接是正常的，从而产生错误），而心跳则是通过是否真正接收到对方发来的数据来判断。并且重连线程进行连接状态判断和重连操作的时间间隔为 2 s。
     */

    /**
     * 超时机制
     * 
     * 1.超时针对的是服务端还是客户端？超时是针对消费端的，消费端会抛出TimeoutException，而服务器端的TimeoutFilter会根据timeout检测到操作超时，打出warn日志
     * 2.超时在哪设置？超时在消费端、服务器端设置，dubbo会合并这两个设置
     * 3.超时设置的优先级是什么？consumer方法级别 > provider 方法级别 > consumer 接口级别 > provider 接口级别 > consumer 全局级别 > provider 全局级别。如果都没配置，那么就是dubbo默认的1秒
     * 4.超时解决的是什么问题 ？最主要是宝贵的线程，客户端的用户线程不能因为服务端超时而一直类似wait， 导致无法正常响应其他业务。
     * 
     * 1）对于客户端而言，在向服务器发送 Rpc 请求时，会获取到 URL 中配置好的超时时间 timeout，在创建 DefaultFuture 对象时传入。接着调用 DefaultFuture 的 get 方法，阻塞
     * timeout 时间，直到超时或者结果返回。
     * 
     * 在创建 DefaultFuture 对象时，会创建一个扫描线程，扫描 FUTURES 中的 future，如果现在的时间减去 DefaultFuture 创建的时间大于超时时间 timeout 的话，就创建一个 Response，
     * 设置为超时状态（可根据是否真正发送分为客户端超时以及服务端超时），然后唤醒阻塞在 get 方法上的用户线程。
     * 
     * 2）对于服务端而言，在 Dubboprotocol$1#reply 方法中，调用 invoker.invoke 方法的过程中，会经过多个 Filter 才会调用执行服务器端的具体方法。其中有一个 Filter 是 TimeoutFilter，
     * 它会记录方法调用完成所花的具体时间，如果大于用户在服务端配置的超时时间，就会打印出警告信息。所以说，客户端超时会抛出异常，而服务器端超时只会记录警告信息。
     */

    /**
     * 在dubbo的用户手册中，对配置有这样的推荐用法：在Provider上尽量多配置Consumer端属性
     * 
     * 原因为：作服务的提供者，比服务使用方更清楚服务性能参数，如调用的超时时间，合理的重试次数，等等。在Provider配置后，Consumer不配置则会使用Provider的配置值，
     * 即Provider配置可以作为Consumer的缺省值。否则，Consumer会使用Consumer端的全局设置，这对于Provider不可控的，并且往往是不合理的
     * 
     * 配置的覆盖规则：1) 方法级配置别优于接口级别，即小Scope优先 2) Consumer端配置 优于 Provider配置 优于 全局配置，最后是Dubbo Hard Code的配置值（见配置文档）。
     * 
     * <dubbo:service interface="com.alibaba.hello.api.HelloService" version="1.0.0" ref="helloService" timeout="300" retry="2" loadbalance="random" actives="0"/>
     * <dubbo:service interface="com.alibaba.hello.api.WorldService" version="1.0.0" ref="helloService" timeout="300" retry="2" loadbalance="random" actives="0" >
     *      <dubbo:method name="findAllPerson" timeout="10000" retries="9" loadbalance="leastactive" actives="5" />
     * <dubbo:service/>
     * 
     * 在Provider可以配置的Consumer端属性有：
     * 
     * timeout，方法调用超时
     * retries，失败重试次数，缺省是2（表示加上第一次调用，会调用3次）
     * loadbalance，负载均衡算法（有多个Provider时，如何挑选Provider调用），缺省是随机（random）。还可以有轮训(roundrobin)、最不活跃优先（leastactive，指从Consumer端并发调用最好的Provider，可以减少的反应慢的Provider的调用，因为反应更容易累积并发的调用）
     * actives，消费者端，最大并发调用限制，即当Consumer对一个服务的并发调用到上限后，新调用会Wait直到超时。在方法上配置（dubbo:method ）则并发限制针对方法，在接口上配置（dubbo:service），则并发限制针对服务。
     * 
     * Provider上可以配置的Provider端属性有：
     * 
     * threads，服务线程池大小
     * executes，一个服务提供者并行执行请求上限，即当Provider对一个服务的并发调用到上限后，新调用会Wait（Consumer可能到超时）。在方法上配置（dubbo:method ）则并发限制针对方法，在接口上配置（dubbo:service），则并发限制针对服务。
     */

    @Activate(group = Constants.PROVIDER)
    public class TimeoutFilter implements Filter {

        private static final Logger logger = LoggerFactory.getLogger(TimeoutFilter.class);

        public Result invoke(Invoker<?> invoker, Invocation invocation) throws RpcException {
            long start = System.currentTimeMillis();
            Result result = invoker.invoke(invocation);
            long elapsed = System.currentTimeMillis() - start;
            if (invoker.getUrl() != null && elapsed > invoker.getUrl().getMethodParameter(invocation.getMethodName(), "timeout", Integer.MAX_VALUE)) {
                if (logger.isWarnEnabled()) {
                    logger.warn("invoke time out. method: " + invocation.getMethodName() + " arguments: "
                            + Arrays.toString(invocation.getArguments()) + " , url is " + invoker.getUrl()
                            + ", invoke elapsed " + elapsed + " ms.");
                }
            }
            return result;
        }

    }

    @SPI(AllDispatcher.NAME)
    public interface Dispatcher {

        @Adaptive({ Constants.DISPATCHER_KEY, "dispather", "channel.handler" })
        ChannelHandler dispatch(ChannelHandler handler, URL url);

    }

    // HeaderExchanger.NAME = header
    @SPI(HeaderExchanger.NAME) 
    public interface Exchanger {

        @Adaptive({ Constants.EXCHANGER_KEY })
        ExchangeServer bind(URL url, ExchangeHandler handler) throws RemotingException;

        @Adaptive({ Constants.EXCHANGER_KEY })
        ExchangeClient connect(URL url, ExchangeHandler handler) throws RemotingException;
    }

    // Transporter接口是网络传输接口，默认实现是NettyTransporter（netty3）
    @SPI("netty")
    public interface Transporter {
        // 绑定一个服务器，即监听服务器的一个地址
        @Adaptive({Constants.SERVER_KEY, Constants.TRANSPORTER_KEY})
        Server bind(URL url, ChannelHandler handler) throws RemotingException;
    
        // 连接一个服务器，即创建一个客户端
        @Adaptive({Constants.CLIENT_KEY, Constants.TRANSPORTER_KEY})
        Client connect(URL url, ChannelHandler handler) throws RemotingException;
    }

    public class DubboProtocol extends AbstractProtocol{
        
        public <T> Invoker<T> refer(Class<T> serviceType, URL url) throws RpcException {
            optimizeSerialization(url);
            // create rpc invoker.
            DubboInvoker<T> invoker = new DubboInvoker<T>(serviceType, url, getClients(url), invokers);
            invokers.add(invoker);
            return invoker;
        }

        /**
         * 这个方法用于获取客户端实例，实例类型为 ExchangeClient。ExchangeClient 实际上并不具备通信能力，它需要基于更底层的客户端实例进行通信。
         * 比如 NettyClient、MinaClient 等，默认情况下，Dubbo 使用 NettyClient 进行通信（Netty3）
         */
        private ExchangeClient[] getClients(URL url) {
            // whether to share connection
            // 是否共享连接
            boolean service_share_connect = false;
            // 获取URL中connections参数（也就是连接数）的值，默认为0，表示未配置
            int connections = url.getParameter(Constants.CONNECTIONS_KEY, 0);
            // if not configured, connection is shared, otherwise, one connection for one service
            // 如果没有配置connections参数的话，那么默认就使用共享连接
            // 共享连接就是一个客户端到一个特定地址（ip地址+端口号）的只有一个连接，这个客户端的所有Invoker如果要向这个地址发起请求，都必须使用这个连接。
            // 如果不是共享连接，那么每个Invoker都会创建connections个到特定地址的连接，不会对连接进行复用
            if (connections == 0) {
                service_share_connect = true;
                connections = 1;
            }
    
            ExchangeClient[] clients = new ExchangeClient[connections];
            for (int i = 0; i < clients.length; i++) {
                if (service_share_connect) {
                    // 获取共享客户端
                    clients[i] = getSharedClient(url);
                } else {
                    // 初始化新的客户端
                    clients[i] = initClient(url);
                }
            }
            return clients;
        }

        // 下面方法先访问缓存，若缓存未命中，则通过 initClient 方法创建新的 ExchangeClient 实例，并将该实例传给 
        // ReferenceCountExchangeClient 构造方法创建一个带有引用计数功能的 ExchangeClient 实例，记录有多少个Invoker在使用该 ExchangeClient实例
        private ExchangeClient getSharedClient(URL url) {
            String key = url.getAddress();
            // 获取带有"引用计数"功能的 ExchangeClient
            ReferenceCountExchangeClient client = referenceClientMap.get(key);
            // 如果存在共享连接且连接可用则直接返回
            if (client != null) {
                if (!client.isClosed()) {
                    // 增加引用计数
                    client.incrementAndGetCount();
                    return client;
                } else {
                    referenceClientMap.remove(key);
                }
            }
            synchronized (key.intern()) {
                // 创建客户端，一般是 HeaderExchangeClient 客户端，它实现了 ExchangeClient 接口
                ExchangeClient exchangeClient = initClient(url);
                // 将 ExchangeClient 实例传给 ReferenceCountExchangeClient，这里使用了装饰模式
                client = new ReferenceCountExchangeClient(exchangeClient, ghostClientMap);
                referenceClientMap.put(key, client);
                ghostClientMap.remove(key);
                return client;
            }
        }

        private ExchangeClient initClient(URL url) {
            // client type setting.
            String str = url.getParameter(Constants.CLIENT_KEY, url.getParameter(Constants.SERVER_KEY, Constants.DEFAULT_REMOTING_CLIENT));
            String version = url.getParameter(Constants.DUBBO_VERSION_KEY);
            boolean compatible = (version != null && version.startsWith("1.0."));

            // 添加编解码参数到 url 中
            url = url.addParameter(Constants.CODEC_KEY, DubboCodec.NAME);
            // enable heartbeat by default
            // 在url中添加heartbeat参数，默认是6000ms，也就是说默认开启心跳机制
            url = url.addParameterIfAbsent(Constants.HEARTBEAT_KEY, String.valueOf(Constants.DEFAULT_HEARTBEAT));
    
            // BIO is not allowed since it has severe performance issue.
            if (str != null && str.length() > 0 && !ExtensionLoader.getExtensionLoader(Transporter.class).hasExtension(str)) {
                throw new RpcException("Unsupported client type: " + str + "," +
                        " supported client type is " + StringUtils.join(ExtensionLoader.getExtensionLoader(Transporter.class).getSupportedExtensions(), " "));
            }
    
            ExchangeClient client;
            try {
                // connection should be lazy
                // 这里的 LazyConnectExchangeClient 类只有在 request 方法被调用时，才会通过 Exchangers 的 connect 方法创建 ExchangeClient 客户端
                // 所以被称为懒加载
                if (url.getParameter(Constants.LAZY_CONNECT_KEY, false)) {
                    client = new LazyConnectExchangeClient(url, requestHandler);
                } else {
                    client = Exchangers.connect(url, requestHandler);
                }
            } catch (RemotingException e) {
                throw new RpcException("Fail to create remoting client for service(" + url + "): " + e.getMessage(), e);
            }
            return client;
        }

    }

    // 将请求交HeaderExchangeClient处理，不进行任何其他操作。
    // ReferenceCountExchangeClient 使用refenceCount记录下使用这个客户端发送请求的Invoker数量，并且调用close尝试关闭这个client时，
    // 并不会真正地关闭这个Client，而是减少refenceCount的值，等到refenceCount的值小于等于0，才回真正关闭掉客户端
    final class ReferenceCountExchangeClient implements ExchangeClient{

        private final URL url;
        private final AtomicInteger refenceCount = new AtomicInteger(0);
        private final ConcurrentMap<String, LazyConnectExchangeClient> ghostClientMap;
        private ExchangeClient client;

        public ReferenceCountExchangeClient(ExchangeClient client, ConcurrentMap<String, LazyConnectExchangeClient> ghostClientMap) {
            this.client = client;
            refenceCount.incrementAndGet();
            this.url = client.getUrl();
            if (ghostClientMap == null) {
                throw new IllegalStateException("ghostClientMap can not be null, url: " + url);
            }
            this.ghostClientMap = ghostClientMap;
        }

        public void close() {
            close(0);
        }
    
        public void close(int timeout) {
            if (refenceCount.decrementAndGet() <= 0) {
                if (timeout == 0) {
                    client.close();
                } else {
                    client.close(timeout);
                }
                client = replaceWithLazyClient();
            }
        }

        public void incrementAndGetCount() {
            refenceCount.incrementAndGet();
        }

    }

    public static class Exchangers {

        public static ExchangeClient connect(URL url, ExchangeHandler handler) throws RemotingException {
            if (url == null) {
                throw new IllegalArgumentException("url == null");
            }
            if (handler == null) {
                throw new IllegalArgumentException("handler == null");
            }
            url = url.addParameterIfAbsent(Constants.CODEC_KEY, "exchange");
            return getExchanger(url).connect(url, handler);
        }

        public static Exchanger getExchanger(URL url) {
            String type = url.getParameter(Constants.EXCHANGER_KEY, Constants.DEFAULT_EXCHANGER);
            return getExchanger(type);
        }
    
        public static Exchanger getExchanger(String type) {
            return ExtensionLoader.getExtensionLoader(Exchanger.class).getExtension(type);
        }

    }

    public class HeaderExchanger implements Exchanger {

        public static final String NAME = "header";
    
        public ExchangeClient connect(URL url, ExchangeHandler handler) throws RemotingException {
            return new HeaderExchangeClient(Transporters.connect(url, new DecodeHandler(new HeaderExchangeHandler(handler))), true);
        }
    
        public ExchangeServer bind(URL url, ExchangeHandler handler) throws RemotingException {
            return new HeaderExchangeServer(Transporters.bind(url, new DecodeHandler(new HeaderExchangeHandler(handler))));
        }
    
    }

    public class NettyTransporter implements Transporter {

        public static final String NAME = "netty4";
    
        public Server bind(URL url, ChannelHandler listener) throws RemotingException {
            return new NettyServer(url, listener);
        }
    
        public Client connect(URL url, ChannelHandler listener) throws RemotingException {
            return new NettyClient(url, listener);
        }
    
    }

    /**
     * 使用了外观设计模式，通过该类的包装，我们就不会看到内部具体的实现细节，这样降低了程序的复杂度，也提高了程序的可维护性。
     */
    public static class Transporters {
    
        private Transporters() {
        }
    
    
        public static Client connect(String url, ChannelHandler... handler) throws RemotingException {
            return connect(URL.valueOf(url), handler);
        }
    
        public static Client connect(URL url, ChannelHandler... handlers) throws RemotingException {
            if (url == null) {
                throw new IllegalArgumentException("url == null");
            }
            ChannelHandler handler;
            if (handlers == null || handlers.length == 0) {
                handler = new ChannelHandlerAdapter();
            } else if (handlers.length == 1) {
                handler = handlers[0];
            } else {
                handler = new ChannelHandlerDispatcher(handlers);
            }
            // 调用Transporter的实现类对象的connect方法。例如实现NettyTransporter，则调用NettyTransporter的connect，并且返回相应的NettyClient
            return getTransporter().connect(url, handler);
        }
    
        public static Transporter getTransporter() {
            return ExtensionLoader.getExtensionLoader(Transporter.class).getAdaptiveExtension();
        }
    
    }

    public static abstract class AbstractClient extends AbstractEndpoint implements Client {

        private final boolean send_reconnect;

        private final long shutdown_timeout;

        private final int reconnect_warning_period;
        // the last successed connected time
        private long lastConnectedTime = System.currentTimeMillis();

        private final AtomicInteger reconnect_count = new AtomicInteger(0);

         // Reconnection error log has been called before?
        private final AtomicBoolean reconnect_error_log_flag = new AtomicBoolean(false);

        private volatile ScheduledFuture<?> reconnectExecutorFuture = null;

        public AbstractClient(URL url, ChannelHandler handler) throws RemotingException {
            super(url, handler);
    
            /**
             * 客户端需要提供重连机制，所以初始化的几个参数都和重连有关：
             */

            // send_reconnect表示在发送消息时发现连接已经断开是否发起重连
            send_reconnect = url.getParameter(Constants.SEND_RECONNECT_KEY, false);
            // shutdown_timeout表示连接服务器一直连接不上的超时时间
            shutdown_timeout = url.getParameter(Constants.SHUTDOWN_TIMEOUT_KEY, Constants.DEFAULT_SHUTDOWN_TIMEOUT);
            // The default reconnection interval is 2s, 1800 means warning interval is 1 hour.
            // reconnect_warning_period表示经过多少次重连尝试之后报一次重连警告
            reconnect_warning_period = url.getParameter("reconnect.waring.period", 1800);
    
            try {
                doOpen();
            } catch (Throwable t) {
                // 省略代码....
            }
            try {
                // connect.
                connect();
                if (logger.isInfoEnabled()) {
                    logger.info("Start " + getClass().getSimpleName() + " " + NetUtils.getLocalAddress() + " connect to the server " + getRemoteAddress());
                }
            } catch (RemotingException t) {
                // 省略代码....
            } catch (Throwable t) {
                // 省略代码....
            }
    
            executor = (ExecutorService) ExtensionLoader.getExtensionLoader(DataStore.class)
                    .getDefaultExtension().get(Constants.CONSUMER_SIDE, Integer.toString(url.getPort()));
            ExtensionLoader.getExtensionLoader(DataStore.class)
                    .getDefaultExtension().remove(Constants.CONSUMER_SIDE, Integer.toString(url.getPort()));
        }

        protected void connect() throws RemotingException {
            connectLock.lock();
            try {
                // 判断是否已经连接
                if (isConnected()) {
                    return;
                }

                // 初始化连接状态检查器，定期检查channel是否连接，连接断开会进行重连操作
                initConnectStatusCheckCommand();
                doConnect();

                if (!isConnected()) {
                    throw new RemotingException(this, "Failed connect to server ....");
                } else {
                    if (logger.isInfoEnabled()) {
                        logger.info("Successed connect to server ...." );
                    }
                }

                // 如果连接没有建立好，要么在上面的 if 判断中抛出异常，要么在 doConnect 方法的执行过程中抛出异常，被下面的 try-catch 语句块捕获。
                // 连接建立好或者重新建立好之后，将reconnect_count（表示重连次数）以及reconnect_error_log_flag（是否已经记录了重连警告信息）
                // 重置为初始值0和false
                reconnect_count.set(0);
                reconnect_error_log_flag.set(false);
            } catch (RemotingException e) {
                throw e;
            } catch (Throwable e) {
                throw new RemotingException(this, "Failed connect to server " + getRemoteAddress() + " from " + getClass().getSimpleName() + " "
                        + NetUtils.getLocalHost() + " using dubbo version " + Version.getVersion()
                        + ", cause: " + e.getMessage(), e);
            } finally {
                connectLock.unlock();
            }
        }

        // 创建了一个Runnable，用来检测是否连接，如果连接断开，调用connect方法；定时调度交给 ScheduledThreadPoolExecutor 来执行
        private synchronized void initConnectStatusCheckCommand() {
            // reconnect=false to close reconnect
            // 获取到<dubbo:reference/>中的reconnect参数。参数的值可以为true/false，也可以为进行重连接操作的间隔。reconnect参数如果为false，将其设置为0，
            // 表示进行重连接操作，reconnect如果为true，就将其设置为 2000ms，或者用户自己设置的时间间隔数
            int reconnect = getReconnectParam(getUrl());

            // reconnectExecutorFuture == null || reconnectExecutorFuture.isCancelled() 这个判断条件保证了当再一次连接操作的时候，
            // 不会重复创建定时任务交给线程池执行。
            if (reconnect > 0 && (reconnectExecutorFuture == null || reconnectExecutorFuture.isCancelled())) {
                Runnable connectStatusCheckCommand = new Runnable() {
                    public void run() {
                        try {
                            if (!isConnected()) {
                                connect();
                            } else {
                                // 记录连上服务器的时间
                                lastConnectedTime = System.currentTimeMillis();
                            }
                        } catch (Throwable t) {
                            String errorMsg = "client reconnect to " + getUrl().getAddress() + " find error . url: " + getUrl();
                            // wait registry sync provider list
                            // shutdown_timeout表示服务器一直连不上的超时时间，如果距离上次连上的时间间隔（lastConnectedTime）超过
                            // 了shutdown_timeout，且还没有在日志中记录重连警告，那么就在日志里面进行记录
                            if (System.currentTimeMillis() - lastConnectedTime > shutdown_timeout) {
                                if (!reconnect_error_log_flag.get()) {
                                    reconnect_error_log_flag.set(true);
                                    logger.error(errorMsg, t);
                                    return;
                                }
                            }
                            // 重连次数达到 reconnect_warning_period 或者其整数倍之后，才会报一次重连警告
                            if (reconnect_count.getAndIncrement() % reconnect_warning_period == 0) {
                                logger.warn(errorMsg, t);
                            }
                        }
                    }
                };
                // 创建好定时任务之后，就交给 ScheduledThreadPoolExecutor 来执行
                reconnectExecutorFuture = reconnectExecutorService
                                                .scheduleWithFixedDelay(connectStatusCheckCommand, reconnect, reconnect, TimeUnit.MILLISECONDS);
            }
        }

        private static int getReconnectParam(URL url) {
            int reconnect;
            // 获取 url 中 reconnect 参数的值
            String param = url.getParameter(Constants.RECONNECT_KEY);
            // 如果用户没有配置 reconnect 的值，或者配置为 true，那么会把 reconnect 设置为 2000 ms
            if (param == null || param.length() == 0 || "true".equalsIgnoreCase(param)) {
                reconnect = Constants.DEFAULT_RECONNECT_PERIOD;
            
            // 如果用户配置为 false，则把 reconnect 设置为 0
            } else if ("false".equalsIgnoreCase(param)) {
                reconnect = 0;
            } else {
                try {
                    reconnect = Integer.parseInt(param);
                } catch (Exception e) {
                    throw new IllegalArgumentException("reconnect param must be nonnegative integer or false/true. input is:" + param);
                }
                if (reconnect < 0) {
                    throw new IllegalArgumentException("reconnect param must be nonnegative integer or false/true. input is:" + param);
                }
            }
            return reconnect;
        }

    }

    public class NettyClient extends AbstractClient {

        private static final NioEventLoopGroup nioEventLoopGroup = new NioEventLoopGroup(Constants.DEFAULT_IO_THREADS, new DefaultThreadFactory("NettyClientWorker", true));
    
        private Bootstrap bootstrap;
    
        private volatile Channel channel; // volatile, please copy reference to use
    
        public NettyClient(final URL url, final ChannelHandler handler) throws RemotingException {
            super(url, wrapChannelHandler(url, handler));
        }
    
        // 设置Netty客户端的一些启动参数
        @Override
        protected void doOpen() throws Throwable {
            NettyHelper.setNettyLoggerFactory();
            final NettyClientHandler nettyClientHandler = new NettyClientHandler(getUrl(), this);
            bootstrap = new Bootstrap();
            bootstrap.group(nioEventLoopGroup)
                    .option(ChannelOption.SO_KEEPALIVE, true)
                    .option(ChannelOption.TCP_NODELAY, true)
                    .option(ChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT)
                    //.option(ChannelOption.CONNECT_TIMEOUT_MILLIS, getTimeout())
                    .channel(NioSocketChannel.class);
    
            if (getTimeout() < 3000) {
                bootstrap.option(ChannelOption.CONNECT_TIMEOUT_MILLIS, 3000);
            } else {
                bootstrap.option(ChannelOption.CONNECT_TIMEOUT_MILLIS, getTimeout());
            }
    
            bootstrap.handler(new ChannelInitializer() {
                protected void initChannel(Channel ch) throws Exception {
                    NettyCodecAdapter adapter = new NettyCodecAdapter(getCodec(), getUrl(), NettyClient.this);
                    ch.pipeline().addLast("logging",new LoggingHandler(LogLevel.INFO))//for debug
                            .addLast("decoder", adapter.getDecoder())
                            .addLast("encoder", adapter.getEncoder())
                            .addLast("handler", nettyClientHandler);
                }
            });
        }
    
        // 和服务端建立新连接，并且替换掉旧的连接
        // 替换掉就得连接的原因是在 AbstractClient 的 connect 代码中，会添加一个定时任务主动检测客户端到服务端的连接是否已经断开，如果断开则会进行重连。
        // 所以，doConnect有可能会被反复调用，因此在进行重连的时候，必须要替换掉之前的channel
        protected void doConnect() throws Throwable {
            long start = System.currentTimeMillis();
            ChannelFuture future = bootstrap.connect(getConnectAddress());
            try {
                boolean ret = future.awaitUninterruptibly(3000, TimeUnit.MILLISECONDS);
                // 如果连接建立成功的话，若存在旧的连接，那么就将旧的连接关闭掉。
                // 但是若客户端已经关闭了的话，就把新建立的连接也关闭掉，并且把 NettyClient.this.channel 置为null
                // 若客户端仍然正常，那么就将新连接保存到 NettyClient.this.channel 中
                if (ret && future.isSuccess()) {
                    Channel newChannel = future.channel();
                    try {
                        // Close old channel
                        Channel oldChannel = NettyClient.this.channel; // copy reference
                        if (oldChannel != null) {
                            try {
                                if (logger.isInfoEnabled()) {
                                    logger.info("Close old netty channel " + oldChannel + " on create new netty channel " + newChannel);
                                }
                                oldChannel.close();
                            } finally {
                                NettyChannel.removeChannelIfDisconnected(oldChannel);
                            }
                        }
                    } finally {
                        if (NettyClient.this.isClosed()) {
                            try {
                                if (logger.isInfoEnabled()) {
                                    logger.info("Close new netty channel " + newChannel + ", because the client closed.");
                                }
                                newChannel.close();
                            } finally {
                                NettyClient.this.channel = null;
                                NettyChannel.removeChannelIfDisconnected(newChannel);
                            }
                        } else {
                            NettyClient.this.channel = newChannel;
                        }
                    }
                } else if (future.cause() != null) {
                    throw new RemotingException(this, "client(url: " + getUrl() + ") failed to connect to server "
                            + getRemoteAddress() + ", error message is:" + future.cause().getMessage(), future.cause());
                } else {
                    throw new RemotingException(this, "client(url: " + getUrl() + ") failed to connect to server "
                            + getRemoteAddress() + " client-side timeout "
                            + getConnectTimeout() + "ms (elapsed: " + (System.currentTimeMillis() - start) + "ms) from netty client "
                            + NetUtils.getLocalHost() + " using dubbo version " + Version.getVersion());
                }
            } finally {
                if (!isConnected()) {
                    //future.cancel(true);
                }
            }
        }
    }

    public class HeaderExchangeServer implements ExchangeServer {

        private final ScheduledExecutorService scheduled = Executors.newScheduledThreadPool(1, new NamedThreadFactory("dubbo-remoting-server-heartbeat", true));
        private final Server server;
        // heartbeat timer
        private ScheduledFuture<?> heatbeatTimer;
        // heartbeat timeout (ms), default value is 0 , won't execute a heartbeat.
        private int heartbeat;
        private int heartbeatTimeout;
        private AtomicBoolean closed = new AtomicBoolean(false);

        public HeaderExchangeServer(Server server) {
            if (server == null) {
                throw new IllegalArgumentException("server == null");
            }
            this.server = server;
            this.heartbeat = server.getUrl().getParameter(Constants.HEARTBEAT_KEY, 0);
            this.heartbeatTimeout = server.getUrl().getParameter(Constants.HEARTBEAT_TIMEOUT_KEY, heartbeat * 3);
            if (heartbeatTimeout < heartbeat * 2) {
                throw new IllegalStateException("heartbeatTimeout < heartbeatInterval * 2");
            }
            startHeatbeatTimer();
        }

        private void startHeatbeatTimer() {
            stopHeartbeatTimer();
            if (heartbeat > 0) {
                heatbeatTimer = scheduled.scheduleWithFixedDelay(
                        new HeartBeatTask(new HeartBeatTask.ChannelProvider() {
                            public Collection<Channel> getChannels() {
                                return Collections.unmodifiableCollection(
                                        // 一个服务器端可能有多个客户端与其相连，所以可能有多个Channel，而且必须往其中的每一个Channel上
                                        // 都发送心跳包。同时，HeaderExchangeServer不是Channel的子类，但是其中的server属性（也就是NettyServer类型对象）
                                        // 中保存了连接到这个服务器的所有channel
                                        HeaderExchangeServer.this.getChannels());
                            }
                        }, heartbeat, heartbeatTimeout),
                        heartbeat, heartbeat, TimeUnit.MILLISECONDS);
            }
        }
    
        private void stopHeartbeatTimer() {
            try {
                ScheduledFuture<?> timer = heatbeatTimer;
                if (timer != null && !timer.isCancelled()) {
                    timer.cancel(true);
                }
            } catch (Throwable t) {
                logger.warn(t.getMessage(), t);
            } finally {
                heatbeatTimer = null;
            }
        }

    }

    public class HeaderExchangeClient implements ExchangeClient {

        private static final Logger logger = LoggerFactory.getLogger(HeaderExchangeClient.class);
    
        private static final ScheduledThreadPoolExecutor scheduled = new ScheduledThreadPoolExecutor(2, new NamedThreadFactory("dubbo-remoting-client-heartbeat", true));
        private final Client client;
        private final ExchangeChannel channel;
        // heartbeat timer
        private ScheduledFuture<?> heartbeatTimer;
        private int heartbeat;
        // heartbeat timeout (ms), default value is 0 , won't execute a heartbeat.
        private int heartbeatTimeout;
    
        // HeaderExchangeClient提供心跳检查功能，并且将 send、request、close 等事件转由HeaderExchangeChannel处理
        public HeaderExchangeClient(Client client, boolean needHeartbeat) {
            if (client == null) {
                throw new IllegalArgumentException("client == null");
            }
            this.client = client;
            this.channel = new HeaderExchangeChannel(client);
            String dubbo = client.getUrl().getParameter(Constants.DUBBO_VERSION_KEY);
            this.heartbeat = client.getUrl().getParameter(Constants.HEARTBEAT_KEY, dubbo != null && dubbo.startsWith("1.0.") ? Constants.DEFAULT_HEARTBEAT : 0);
            // 心跳检测超时的时间，也就是说如果客户端在这个时间段内没有收到服务器端发送过来的心跳包，或者其它数据，那么就会进行重连操作
            // 默认为 3 个心跳时间间隔
            this.heartbeatTimeout = client.getUrl().getParameter(Constants.HEARTBEAT_TIMEOUT_KEY, heartbeat * 3);
            if (heartbeatTimeout < heartbeat * 2) {
                throw new IllegalStateException("heartbeatTimeout < heartbeatInterval * 2");
            }
            if (needHeartbeat) {
                startHeatbeatTimer();
            }
        }

        private void startHeatbeatTimer() {
            // 开始新的心跳之前，先从线程池中移除掉之前的心跳包发送任务
            stopHeartbeatTimer();
            if (heartbeat > 0) {
                // 以固定的时间间隔向服务器发送心跳包，第一个heartbeat是initialDelay，第二个heartbeat是delay
                heartbeatTimer = scheduled.scheduleWithFixedDelay(
                        new HeartBeatTask(new HeartBeatTask.ChannelProvider() {
                            public Collection<Channel> getChannels() {
                                // 这个方法主要用于只有一个元素的优化，减少内存分配，无需分配额外的内存，可以从SingletonList内部类看得出来,由于只有一个element,因此可以做到内存分配最小化。
                                // 采用这种方式是因为一个客户端对应于一个Channel，只需要往一个Channel上发送心跳包，而一个服务器Server则对应于多个Channel，需要往多个Channel上发送
                                // 同样的心跳包，两者都使用同样的HeartBeatTask类对象。因此对于客户端，即使只有一个Channel，也要将其包装成Collection对象
                                //
                                // HeaderExchangeClient是Channel的子类，所以这里将HeaderExchangeClient自己作为对象传入进去。
                                return Collections.<Channel>singletonList(HeaderExchangeClient.this);
                            }
                        }, heartbeat, heartbeatTimeout),
                        heartbeat, heartbeat, TimeUnit.MILLISECONDS);
            }
        }

        private void stopHeartbeatTimer() {
            // 如果之前启动了心跳任务（heartbeatTimer不为null）且还没有被取消，则cancel掉这个心跳包发送任务，并且将其从线程池中清除掉
            if (heartbeatTimer != null && !heartbeatTimer.isCancelled()) {
                try {
                    heartbeatTimer.cancel(true);
                    // purge会把线程池的任务队列中已经被取消的任务移除掉
                    scheduled.purge();
                } catch (Throwable e) {
                    if (logger.isWarnEnabled()) {
                        logger.warn(e.getMessage(), e);
                    }
                }
            }
            heartbeatTimer = null;
        }
    
        public ResponseFuture request(Object request) throws RemotingException {
            return channel.request(request);
        }
    
        public ResponseFuture request(Object request, int timeout) throws RemotingException {
            return channel.request(request, timeout);
        }
    
        public void send(Object message) throws RemotingException {
            channel.send(message);
        }
    
        public void send(Object message, boolean sent) throws RemotingException {
            channel.send(message, sent);
        }

    
        @Override
        public String toString() {
            return "HeaderExchangeClient [channel=" + channel + "]";
        }
    }


    /**
     * 使用ScheduledThreadPoolExecutor来进行定时发送心跳包的任务，每个任务的执行间隔为heartbeat。在任务的执行过程中，如果发现
     * 连接最近处于空闲状态，也就是最近连接双方没有发送过心跳数据或者其它读写数据，那么就会发送心跳请求给对方。另外如果在
     * heartbeatTimeout时间内，channel双方都没有检测到对方发送过来的数据，就会进行相应的处理。
     */
    final static class HeartBeatTask implements Runnable {

        private static final Logger logger = LoggerFactory.getLogger(HeartBeatTask.class);
    
        private ChannelProvider channelProvider;
    
        private int heartbeat;
    
        private int heartbeatTimeout;
    
        HeartBeatTask(ChannelProvider provider, int heartbeat, int heartbeatTimeout) {
            this.channelProvider = provider;
            this.heartbeat = heartbeat;
            this.heartbeatTimeout = heartbeatTimeout;
        }
    
        /**
         * Dubbo 中采取的是双向心跳，即服务端会向客户端发送心跳，客户端也会向服务端发送心跳。HeartbeatTask 的主要作用就是发送心跳包，以及
         * 当超出设置的心跳总时间之后，进行相应的处理。
         */
        public void run() {
            try {
                long now = System.currentTimeMillis();
                for (Channel channel : channelProvider.getChannels()) {
                    if (channel.isClosed()) {
                        continue;
                    }
                    try {
                        // 根据心跳检测不需要忙检测的原因，在 channel 中保存了两个属性：KEY_WRITE_TIMESTAMP 和 KEY_READ_TIMESTAMP，
                        // 分别表示在这个 channel 中，上一次读或者是上一次写的时间戳。这里的读或者写，不仅仅在channel上发送或者接收心跳包
                        // 的时候进行更新（HeartbeatHandler中的sent和received方法），在这个channel上进行读写普通数据时，也进行更新（HeaderExchangeHandler中的sent和received方法）。
                        //
                        // 因此通过更新这两个字段，实现了只在连接空闲时才会真正发送心跳包的机制。
                        Long lastRead = (Long) channel.getAttribute(
                                HeaderExchangeHandler.KEY_READ_TIMESTAMP);
                        Long lastWrite = (Long) channel.getAttribute(
                                HeaderExchangeHandler.KEY_WRITE_TIMESTAMP);
                        if ((lastRead != null && now - lastRead > heartbeat)
                                || (lastWrite != null && now - lastWrite > heartbeat)) {
                            Request req = new Request();
                            req.setVersion("2.0.0");
                            req.setTwoWay(true);
                            req.setEvent(Request.HEARTBEAT_EVENT);
                            channel.send(req);
                            if (logger.isDebugEnabled()) {
                                logger.debug("Send heartbeat to remote channel " + channel.getRemoteAddress()
                                        + ", cause: The channel has no data-transmission exceeds a heartbeat period: " + heartbeat + "ms");
                            }
                        }

                        // 如果在设定的 heartbeatTimeout 时间里面客户端与服务器双方都没有接收到对方发送过来的心跳包或者其它数据，就会进行相应处理
                        if (lastRead != null && now - lastRead > heartbeatTimeout) {
                            logger.warn("Close channel " + channel
                                    + ", because heartbeat read idle time out: " + heartbeatTimeout + "ms");
                            // 如果是客户端的话，就进行重连操作
                            if (channel instanceof Client) {
                                try {
                                    ((Client) channel).reconnect();
                                } catch (Exception e) {
                                    //do nothing
                                }
                            
                            // 如果是服务器端的话，直接断开连接，这样的考虑是合理的，客户端调用是强依赖可用连接的，而服务端可以等待客户端重新建立连接。
                            } else {
                                channel.close();
                            }
                        }
                    } catch (Throwable t) {
                        logger.warn("Exception when heartbeat to remote channel " + channel.getRemoteAddress(), t);
                    }
                }
            } catch (Throwable t) {
                logger.warn("Unhandled exception when heartbeat, cause: " + t.getMessage(), t);
            }
        }
    
        interface ChannelProvider {
            Collection<Channel> getChannels();
        }
    
    }

    /**
     * HeartbeatHandler主要用来接收处理另外一方发送过来的心跳包，如果是其它数据的话，则直接交给其它handler（比如AllChannelHandler来进行处理）。
     * 1.如果接收到的是心跳包请求，要给对方返回一个心跳响应
     * 2.如果接收到的是心跳包响应，直接返回
     * 3.如果接收到的是其它数据，则交给其它的handler进行处理
     */
    public class HeartbeatHandler extends AbstractChannelHandlerDelegate {

        public static String KEY_READ_TIMESTAMP = "READ_TIMESTAMP";

        public static String KEY_WRITE_TIMESTAMP = "WRITE_TIMESTAMP";

        public void sent(Channel channel, Object message) throws RemotingException {
            setWriteTimestamp(channel);
            handler.sent(channel, message);
        }

        public void received(Channel channel, Object message) throws RemotingException {
            // 更新 channel 上的 KEY_READ_TIMESTAMP
            setReadTimestamp(channel);
            // 如果接收到的是心跳包请求
            if (isHeartbeatRequest(message)) {
                Request req = (Request) message;
                if (req.isTwoWay()) {
                    Response res = new Response(req.getId(), req.getVersion());
                    res.setEvent(Response.HEARTBEAT_EVENT);
                    channel.send(res);
                    if (logger.isInfoEnabled()) {
                        int heartbeat = channel.getUrl().getParameter(Constants.HEARTBEAT_KEY, 0);
                        if (logger.isDebugEnabled()) {
                            logger.debug("Received heartbeat from remote channel " + channel.getRemoteAddress()
                                    + ", cause: The channel has no data-transmission exceeds a heartbeat period"
                                    + (heartbeat > 0 ? ": " + heartbeat + "ms" : ""));
                        }
                    }
                }
                return;
            }
            // 如果接收到的是心跳包响应
            if (isHeartbeatResponse(message)) {
                if (logger.isDebugEnabled()) {
                    logger.debug(
                            new StringBuilder(32)
                                    .append("Receive heartbeat response in thread ")
                                    .append(Thread.currentThread().getName())
                                    .toString());
                }
                return;
            }
            // 如果接收到的是其它数据
            handler.received(channel, message);
        }

        private void setReadTimestamp(Channel channel) {
            channel.setAttribute(KEY_READ_TIMESTAMP, System.currentTimeMillis());
        }
    
        private void setWriteTimestamp(Channel channel) {
            channel.setAttribute(KEY_WRITE_TIMESTAMP, System.currentTimeMillis());
        }

    }


}