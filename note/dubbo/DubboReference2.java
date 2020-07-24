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
        // 绑定一个服务器
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
                // 创建 ExchangeClient 客户端，一般是 HeaderExchangeClient 客户端
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
            // 调用Transporter的实现类对象的connect方法。
            // 例如实现NettyTransporter，则调用NettyTransporter的connect，并且返回相应的NettyClient
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

        private long lastConnectedTime = System.currentTimeMillis();

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

        // 创建了一个Runnable，用来检测是否连接，如果连接断开，调用connect方法；定时调度交给ScheduledThreadPoolExecutor来执行
        private synchronized void initConnectStatusCheckCommand() {
            // reconnect=false to close reconnect
            // 获取到<dubbo:reference/>中的reconnect参数。参数的值可以为true/false，也可以为进行重连接操作的间隔。reconnect参数如果为false，将其设置为0，
            // 表示进行重连接操作，reconnect如果为true，就将其设置为 2000ms，或者用户自己设置的时间间隔数
            int reconnect = getReconnectParam(getUrl());

            // reconnectExecutorFuture == null || reconnectExecutorFuture.isCancelled() 这个判断条件保证了当再一次
            if (reconnect > 0 && (reconnectExecutorFuture == null || reconnectExecutorFuture.isCancelled())) {
                Runnable connectStatusCheckCommand = new Runnable() {
                    public void run() {
                        try {
                            if (!isConnected()) {
                                connect();
                            } else {
                                lastConnectedTime = System.currentTimeMillis();
                            }
                        } catch (Throwable t) {
                            String errorMsg = "client reconnect to " + getUrl().getAddress() + " find error . url: "
                                    + getUrl();
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
            String param = url.getParameter(Constants.RECONNECT_KEY);
            if (param == null || param.length() == 0 || "true".equalsIgnoreCase(param)) {
                reconnect = Constants.DEFAULT_RECONNECT_PERIOD;
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
        // 替换掉就得连接的原因是在 AbstractClient 的connect代码中，会添加一个定时任务主动检测客户端到服务端的连接是否已经断开，如果断开则会进行重连
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
    
        // HeaderExchangeClient提供心跳检查功能，并且将send、request、close等事件转由HeaderExchangeChannel处理
        public HeaderExchangeClient(Client client, boolean needHeartbeat) {
            if (client == null) {
                throw new IllegalArgumentException("client == null");
            }
            this.client = client;
            this.channel = new HeaderExchangeChannel(client);
            String dubbo = client.getUrl().getParameter(Constants.DUBBO_VERSION_KEY);
            this.heartbeat = client.getUrl().getParameter(Constants.HEARTBEAT_KEY, dubbo != null && dubbo.startsWith("1.0.") ? Constants.DEFAULT_HEARTBEAT : 0);
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
                                // 采用这种方式是因为一个客户端ExchangeClient对应于一个Channel，只需要往一个Channel上发送心跳包，而一个服务器Server则对应于多个Channel，需要往多个Channel上发送
                                // 同样的心跳包，两者都使用同样的HeartBeatTask类对象。因此对于客户端，即使只有一个Channel，也要将其包装成Collection对象
                                return Collections.<Channel>singletonList(HeaderExchangeClient.this);
                            }
                        }, heartbeat, heartbeatTimeout),
                        heartbeat, heartbeat, TimeUnit.MILLISECONDS);
            }
        }

        private void stopHeartbeatTimer() {
            // 如果之前启动了心跳任务（heartbeatTimer不为null）且还没有被取消，则cancel掉这个心跳包发送任务，
            // 并且将其从线程池中清除掉
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


    


}