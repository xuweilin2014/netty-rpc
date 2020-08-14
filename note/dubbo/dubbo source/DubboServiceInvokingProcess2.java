public class DubboServiceInvokingProcess2{

    /**
     * 本节主要分析消费者和提供者两端handler的创建
     */

    /**
     * META-INF/dubbo/internal/com.alibaba.dubbo.common.threadpool.ThreadPool文件中内容如下：
     * 
     * fixed=com.alibaba.dubbo.common.threadpool.support.fixed.FixedThreadPool
     * cached=com.alibaba.dubbo.common.threadpool.support.cached.CachedThreadPool
     * limited=com.alibaba.dubbo.common.threadpool.support.limited.LimitedThreadPool
     */

    /**    
     * 1.服务器端接收到客户端发过来的Request请求：
     * 
     * 解码器将数据包解析成 Request 对象后，NettyServerHandler 的 channelRead 方法紧接着会收到这个对象，并将这个对象继续向下传递。这期间该对象会被依次传递给
     * AbstractPeer、MultiMessageHandler、HeartbeatHandler 以及 AllChannelHandler。最后由 AllChannelHandler 将该对象封装到 Runnable 实现类 ChannelEventRunnable 对象中， 
     * 并将 Runnable 放入线程池中执行后续的调用逻辑。
     * 
     * 整个调用栈如下：
     * 
     * NettyServerHandler#channelRead(ChannelHandlerContext, Object) —>
     * AbstractPeer#received(Channel, Object) —>
     * MultiMessageHandler#received(Channel, Object) —>
     * HeartbeatHandler#received(Channel, Object) —>
     * AllChannelHandler#received(Channel, Object) —>
     * ExecutorService#execute(Runnable) // 由线程池执行后续的调用逻辑
     * 
     * 服务器端对客户端发送过来的请求处理过程如下：
     * 
     * ChannelEventRunnable#run()
     * —> DecodeHandler#received(Channel, Object)
     * —> HeaderExchangeHandler#received(Channel, Object)
     * —> HeaderExchangeHandler#handleRequest(ExchangeChannel, Request)
     * —> DubboProtocol.requestHandler#reply(ExchangeChannel, Object)
     * —> Filter#invoke(Invoker, Invocation)
     * —> AbstractProxyInvoker#invoke(Invocation)
     * —> Wrapper0#invokeMethod(Object, String, Class[], Object[])
     * —> DemoServiceImpl#sayHello(String)
     * 
     * 2.客户端接收到服务器端发过来的Response请求：
     * 
     * 解码器将数据包解析成 Request, NettyClientHandler 的 channelRead 方法紧接着会收到这个对象, 并将这个对象继续向下传递. 这期间该对象会被依次传递给
     * AbstractPeer, MultiMessageHandler, HeartbeatHandler 以及 AllChannelHandler。最后由 AllChannelHandler 将该对象封装到 Runnable 实现类 ChannelEventRunnable 的对象中， 
     * 并将 Runnable 放入线程池中执行后续的调用逻辑。
     * 
     * 整个调用栈如下：
     * 
     * NettyClientHandler#channelRead(ChannelHandlerContext, Object) 
     * —> AbstractPeer#received(Channel, Object) 
     * —> MultiMessageHandler#received(Channel, Object) 
     * —> HeartbeatHandler#received(Channel, Object) 
     * —> AllChannelHandler#received(Channel, Object) 
     * —> ExecutorService#execute(Runnable) // 由线程池执行后续的调用逻辑
     * 
     * 客户端对服务器端发过来的响应处理过程的如下：
     * 
     * ChannelEventRunnable#run()
     * —> DecodeHandler#received(Channel, Object)
     * —> HeaderExchangeHandler#received(Channel, Object)
     * —> HeaderExchangeHandler#handleResponse(ExchangeChannel, Response)
     * -> DefaultFuture#received(Channel, Response)
     */

    /**
     * 对于客户端，它的底层 ChannelPipeline 结构为：
     * 
     * decoder -> encoder -> NettyClientHandler
     * 
     * 其中NettyClientHandler是Netty中ChannelDuplexHandler的子类，可以处理入站和出站数据。其中，有一个ChannelHandler类型的handler对象，它真正的类型是NettyClient。NettyClient
     * 的父类实现了ChannelHandler接口。而NettyClient中的handler也经过了层层封装。具体的层次如下：
     * 
     * NettyClientHandler（ChannelHandler handler）
     * -> NettyClient（ChannelHandler handler）
     * -> MultiMessageHandler（ChannelHandler handler，继承了AbstractChannelHandlerDelegate类）
     * -> HeartbeatHandler（ChannelHandler handler，继承了AbstractChannelHandlerDelegate类）
     * -> AllChannelHandler（ChannelHandler handler，继承的WrappedChannelHandler实现了ChannelHandlerDelegate接口）
     * -> DecodeHandler（ChannelHandler handler，继承了AbstractChannelHandlerDelegate类）
     * -> HeaderExchangeHandler（ExchangeHandler，实现了ChannelHandlerDelegate接口）
     * -> DubboProtocol$1
     */

    /**
     * 对于服务端，它的底层 ChannelPipeline 结构为：
     * 
     * decode -> encoder -> NettyServerHandler
     * 
     * 其中NettyServerHandler是Netty中ChannelDuplexHandler的子类，可以处理入站和出站数据。其中其中，有一个ChannelHandler类型的handler对象，它真正的类型是NettyServer。NettyServer
     * 的父类实现了ChannelHandler接口。而NettyServer中的handler也经过了层层封装。具体的层次如下：
     * 
     * NettServerHandler
     * -> NettyServer（ChannelHandler handler）
     * -> MultiMessageHandler（ChannelHandler handler，继承了AbstractChannelHandlerDelegate类）
     * -> HeartbeatHandler（ChannelHandler handler，继承了AbstractChannelHandlerDelegate类）
     * -> AllChannelHandler（ChannelHandler handler，继承的WrappedChannelHandler实现了ChannelHandlerDelegate接口）
     * -> DecodeHandler（ChannelHandler handler，继承了AbstractChannelHandlerDelegate类）
     * -> HeaderExchangeHandler（ExchangeHandler，实现了ChannelHandlerDelegate接口）
     * -> DubboProtocol$1
     */

    /**
     * Server的创建在DubboProtocol的createServer中进行，具体的调用栈如下：
     * 
     * DubboProtocol#export
     * -> DubboProtocol#openServer
     * -> DubboProtocol#createServer
     * -> Exchangers#bind
     * -> HeaderExchanger#bind
     * -> Transporters#connect
     * -> NettyTransporter#bind
     * 
     * 最后返回一个HeaderExchangeServer对象，其中封装了NettyServer
     */

    /**
     * Client的创建在DubboProtocol的getSharedClient和initClient中进行。
     * 
     * initClient返回一个HeaderExchangeClient对象，其中封装了NettyClient对象。
     * getSharedClient在initClient的基础上又封装了一层，会返回一个ReferenceCountExchangeClient，其中包含了HeaderExchangeClient对象，而
     * HeaderExchangeClient对象中自然也封装了NettyClient对象
     */

    public class HeaderExchanger implements Exchanger {

        public static final String NAME = "header";
    
        public ExchangeClient connect(URL url, ExchangeHandler handler) throws RemotingException {
            return new HeaderExchangeClient(Transporters.connect(url, new DecodeHandler(new HeaderExchangeHandler(handler))), true);
        }
    
        public ExchangeServer bind(URL url, ExchangeHandler handler) throws RemotingException {
            return new HeaderExchangeServer(Transporters.bind(url, new DecodeHandler(new HeaderExchangeHandler(handler))));
        }
    
    }

    

    public abstract class AbstractPeer implements Endpoint, ChannelHandler {

        private final ChannelHandler handler;

        private volatile URL url;

        public AbstractPeer(URL url, ChannelHandler handler) {
            if (url == null) {
                throw new IllegalArgumentException("url == null");
            }
            if (handler == null) {
                throw new IllegalArgumentException("handler == null");
            }
            this.url = url;
            // handler的类型是: MultiMessageHandler -> HeartbeatHandler -> AllChannelHandler -> DecodeHandler -> HeaderExchangeHandler -> DubboProtocol$1
            this.handler = handler;
        }

        public void received(Channel ch, Object msg) throws RemotingException {
            if (closed) {
                return;
            }
            // 调用 MultiMessageHandler 的 received 方法
            handler.received(ch, msg);
        }

    }

    public abstract class AbstractEndpoint extends AbstractPeer implements Resetable {

        public AbstractEndpoint(URL url, ChannelHandler handler) {
            super(url, handler);
            this.codec = getChannelCodec(url);
            this.timeout = url.getPositiveParameter(Constants.TIMEOUT_KEY, Constants.DEFAULT_TIMEOUT);
            this.connectTimeout = url.getPositiveParameter(Constants.CONNECT_TIMEOUT_KEY, Constants.DEFAULT_CONNECT_TIMEOUT);
        }

    }

    public static abstract class AbstractClient extends AbstractEndpoint implements Client {

        public AbstractClient(URL url, ChannelHandler handler) throws RemotingException {
            super(url, handler);
            // 省略代码
        }

        protected static ChannelHandler wrapChannelHandler(URL url, ChannelHandler handler) {
            // CLIENT_THREAD_POOL_NAME的值为DubboClientHandler，给url设置threadname参数的值，比如threadname=DubboClientHandler-10.181.31
            url = ExecutorUtil.setThreadName(url, CLIENT_THREAD_POOL_NAME);
            // 如果url没有设置threadpool参数的值，那么就默认给它添加 threadpool = cached
            url = url.addParameterIfAbsent(Constants.THREADPOOL_KEY, Constants.DEFAULT_CLIENT_THREADPOOL);
            return ChannelHandlers.wrap(handler, url);
        }

    }

    /**
     * NettyClient的继承链如下：
     * NettyClient -> AbstractClient -> AbstractEndpoint -> AbstractPeer -> ChannelHandler
     */
    public class NettyClient extends AbstractClient {

        private volatile Channel channel;

        private static final NioEventLoopGroup nioEventLoopGroup = new NioEventLoopGroup(Constants.DEFAULT_IO_THREADS, new DefaultThreadFactory("NettyClientWorker", true));

        private Bootstrap bootstrap;

        public NettyClient(final URL url, final ChannelHandler handler) throws RemotingException {
            // 传入的handler是HeaderExchanger#connect方法创建的DecodeHandler，而其中还封装了HeaderExchangeHandler，
            // 而在HeaderExchangeHandler中还封装了DubboProtocol$1，也就是DubboProtocol的一个内部类
            // handler的类型为：DecodeHandler -> HeaderExchangeHandler -> DubboProtocol$1
            super(url, wrapChannelHandler(url, handler));
        }

        // 设置Netty客户端的一些启动参数
        @Override
        protected void doOpen() throws Throwable {
            NettyHelper.setNettyLoggerFactory();
            // NettyClient父类实现了ChannelHandler接口
            final NettyClientHandler nettyClientHandler = new NettyClientHandler(getUrl(), this);
            bootstrap = new Bootstrap();
            bootstrap.group(nioEventLoopGroup)
                    .option(ChannelOption.SO_KEEPALIVE, true)
                    .option(ChannelOption.TCP_NODELAY, true)
                    .option(ChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT)
                    //.option(ChannelOption.CONNECT_TIMEOUT_MILLIS, getTimeout())
                    .channel(NioSocketChannel.class);
    
            // 设置超时时间
            if (getTimeout() < 3000) {
                bootstrap.option(ChannelOption.CONNECT_TIMEOUT_MILLIS, 3000);
            } else {
                bootstrap.option(ChannelOption.CONNECT_TIMEOUT_MILLIS, getTimeout());
            }
    
            // 设定好 channel 中的各个 handler，其中最重要的就是 NettyClientHandler
            bootstrap.handler(new ChannelInitializer() {
                protected void initChannel(Channel ch) throws Exception {
                    NettyCodecAdapter adapter = new NettyCodecAdapter(getCodec(), getUrl(), NettyClient.this);
                    ch.pipeline().addLast("decoder", adapter.getDecoder())
                            .addLast("encoder", adapter.getEncoder())
                            .addLast("handler", nettyClientHandler);
                }
            });
        }
    }

    public static class ChannelHandlers {

        private static ChannelHandlers INSTANCE = new ChannelHandlers();
    
        protected ChannelHandlers() {
        }
    
        public static ChannelHandler wrap(ChannelHandler handler, URL url) {
            return ChannelHandlers.getInstance().wrapInternal(handler, url);
        }
    
        protected static ChannelHandlers getInstance() {
            return INSTANCE;
        }
    
        protected ChannelHandler wrapInternal(ChannelHandler handler, URL url) {
            // 使用自适应拓展机制，根据url中dispatcher参数的值，获取对应的扩展，也就是Dispatcher接口的实现类，然后调用其dispatch方法。
            // url中没有 dispatcher 参数的话，默认扩展是AllDispatcher，表示所有的消息都发送到线程池中去处理
            // 这里的handler类型任然是 DecodeHandler
            return new MultiMessageHandler(new HeartbeatHandler(
                ExtensionLoader.getExtensionLoader(Dispatcher.class).getAdaptiveExtension().dispatch(handler, url)));
        }
    }

    @SPI(AllDispatcher.NAME)
    public interface Dispatcher {

        @Adaptive({ Constants.DISPATCHER_KEY, "dispather", "channel.handler" })
        // The last two parameters are reserved for compatibility with the old configuration
        ChannelHandler dispatch(ChannelHandler handler, URL url);

    }

    public class AllDispatcher implements Dispatcher {

        public static final String NAME = "all";
    
        public ChannelHandler dispatch(ChannelHandler handler, URL url) {
            return new AllChannelHandler(handler, url);
        }
    }

    public class WrappedChannelHandler implements ChannelHandlerDelegate {
    
        protected static final ExecutorService SHARED_EXECUTOR = Executors.newCachedThreadPool(new NamedThreadFactory("DubboSharedHandler", true));
    
        protected final ExecutorService executor;
    
        protected final ChannelHandler handler;
    
        protected final URL url;
    
        public WrappedChannelHandler(ChannelHandler handler, URL url) {
            this.handler = handler;
            this.url = url;

            // 根据用户 url 中的配置选择合适的线程池，用来处理客户端发送过来的各种请求
            executor = (ExecutorService) ExtensionLoader.getExtensionLoader(ThreadPool.class).getAdaptiveExtension().getExecutor(url);
    
            String componentKey = Constants.EXECUTOR_SERVICE_COMPONENT_KEY;
            if (Constants.CONSUMER_SIDE.equalsIgnoreCase(url.getParameter(Constants.SIDE_KEY))) {
                componentKey = Constants.CONSUMER_SIDE;
            }
            DataStore dataStore = ExtensionLoader.getExtensionLoader(DataStore.class).getDefaultExtension();
            dataStore.put(componentKey, Integer.toString(url.getPort()), executor);
        }


        public ChannelHandler getHandler() {
            if (handler instanceof ChannelHandlerDelegate) {
                return ((ChannelHandlerDelegate) handler).getHandler();
            } else {
                return handler;
            }
        }
    }

    public class AllChannelHandler extends WrappedChannelHandler {

        public AllChannelHandler(ChannelHandler handler, URL url) {
            // 根据url中的threadpool参数获取合适的线程池扩展
            super(handler, url);
        }
    
        public void connected(Channel channel) throws RemotingException {
            ExecutorService cexecutor = getExecutorService();
            try {
                cexecutor.execute(new ChannelEventRunnable(channel, handler, ChannelState.CONNECTED));
            } catch (Throwable t) {
                throw new ExecutionException("connect event", channel, getClass() + " error when process connected event .", t);
            }
        }
    
        public void disconnected(Channel channel) throws RemotingException {
            ExecutorService cexecutor = getExecutorService();
            try {
                cexecutor.execute(new ChannelEventRunnable(channel, handler, ChannelState.DISCONNECTED));
            } catch (Throwable t) {
                throw new ExecutionException("disconnect event", channel, getClass() + " error when process disconnected event .", t);
            }
        }
    
        // 处理请求和响应消息，这里的 message 变量类型可能是 Request，也可能是 Response
        public void received(Channel channel, Object message) throws RemotingException {
            ExecutorService cexecutor = getExecutorService();
            try {
                // 将请求和响应消息封装到 ChannelEventRunnable 对象中，然后派发到线程池中处理
                // 这里的 handler 是 DecodeHandler 类型的对象
                cexecutor.execute(new ChannelEventRunnable(channel, handler, ChannelState.RECEIVED, message));
            } catch (Throwable t) {
                // fix The thread pool is full, refuses to call, does not return, and causes the consumer to wait for time out
                // 如果通信方式为双向通信，此时将 Server side ... threadpool is exhausted 错误信息封装到 Response 中，并返回给服务消费方。
                if(message instanceof Request && t instanceof RejectedExecutionException){
                    Request request = (Request)message;
                    if(request.isTwoWay()){
                        String msg = "Server side(" + url.getIp() + "," + url.getPort() + ") threadpool is exhausted ,detail msg:" + t.getMessage();
                        Response response = new Response(request.getId(), request.getVersion());
                        response.setStatus(Response.SERVER_THREADPOOL_EXHAUSTED_ERROR);
                        response.setErrorMessage(msg);
                        // 返回包含错误信息的 Response 对象
                        channel.send(response);
                        return;
                    }
                }
                throw new ExecutionException(message, channel, getClass() + " error when process received event .", t);
            }
        }
    
        public void caught(Channel channel, Throwable exception) throws RemotingException {
            ExecutorService cexecutor = getExecutorService();
            try {
                cexecutor.execute(new ChannelEventRunnable(channel, handler, ChannelState.CAUGHT, exception));
            } catch (Throwable t) {
                throw new ExecutionException("caught event", channel, getClass() + " error when process caught event .", t);
            }
        }
    
        private ExecutorService getExecutorService() {
            ExecutorService cexecutor = executor;
            if (cexecutor == null || cexecutor.isShutdown()) {
                cexecutor = SHARED_EXECUTOR;
            }
            return cexecutor;
        }
    }

    public abstract class AbstractChannelHandlerDelegate implements ChannelHandlerDelegate {

        protected ChannelHandler handler;
    
        protected AbstractChannelHandlerDelegate(ChannelHandler handler) {
            Assert.notNull(handler, "handler == null");
            this.handler = handler;
        }
    
        public ChannelHandler getHandler() {
            if (handler instanceof ChannelHandlerDelegate) {
                return ((ChannelHandlerDelegate) handler).getHandler();
            }
            return handler;
        }
    
    }

    public class MultiMessageHandler extends AbstractChannelHandlerDelegate {

        public MultiMessageHandler(ChannelHandler handler) {
            // handler类型是：HeartbeatHandler -> AllChannelHandler -> DecodeHandler -> HeaderExchangeHandler -> DubboProtocol$1
            super(handler);
        }
    
        @SuppressWarnings("unchecked")
        @Override
        public void received(Channel channel, Object message) throws RemotingException {
            if (message instanceof MultiMessage) {
                MultiMessage list = (MultiMessage) message;
                for (Object obj : list) {
                    handler.received(channel, obj);
                }
            } else {
                handler.received(channel, message);
            }
        }
    }

    public class HeartbeatHandler extends AbstractChannelHandlerDelegate {

        private static final Logger logger = LoggerFactory.getLogger(HeartbeatHandler.class);
    
        public static String KEY_READ_TIMESTAMP = "READ_TIMESTAMP";
    
        public static String KEY_WRITE_TIMESTAMP = "WRITE_TIMESTAMP";
    
        public HeartbeatHandler(ChannelHandler handler) {
            // handler类型是：AllChannelHandler -> -> DecodeHandler -> HeaderExchangeHandler -> DubboProtocol$1
            super(handler);
        }
    
        public void sent(Channel channel, Object message) throws RemotingException {
            setWriteTimestamp(channel);
            handler.sent(channel, message);
        }
    
        public void received(Channel channel, Object message) throws RemotingException {
            setReadTimestamp(channel);
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
            handler.received(channel, message);
        }
    }



    @io.netty.channel.ChannelHandler.Sharable
    public class NettyClientHandler extends ChannelDuplexHandler {

        private final URL url;

        private final ChannelHandler handler;

        public NettyClientHandler(URL url, ChannelHandler handler) {
            if (url == null) {
                throw new IllegalArgumentException("url == null");
            }
            if (handler == null) {
                throw new IllegalArgumentException("handler == null");
            }
            this.url = url;
            // 此处的handler就是NettyClient对象
            this.handler = handler;
        }

        @Override
        public void channelRead(ChannelHandlerContext ctx, Object msg) throws Exception {
            NettyChannel channel = NettyChannel.getOrAddChannel(ctx.channel(), url, handler);
            try {
                handler.received(channel, msg);
            } finally {
                NettyChannel.removeChannelIfDisconnected(ctx.channel());
            }
        }

        @Override
        public void write(ChannelHandlerContext ctx, Object msg, ChannelPromise promise) throws Exception {
            super.write(ctx, msg, promise);
            NettyChannel channel = NettyChannel.getOrAddChannel(ctx.channel(), url, handler);
            try {
                handler.sent(channel, msg);
            } finally {
                NettyChannel.removeChannelIfDisconnected(ctx.channel());
            }
        }
    }

    public class ChannelEventRunnable implements Runnable {

        public ChannelEventRunnable(Channel channel, ChannelHandler handler, ChannelState state, Object message,
                Throwable exception) {
            this.channel = channel;
            this.handler = handler;
            this.state = state;
            this.message = message;
            this.exception = exception;
        }

        public void run() {
            // 检测通道状态，对于请求或者响应消息，此时 state = RECEIVED
            switch (state) {
                case CONNECTED:
                    // 省略代码
                case DISCONNECTED:
                    // 省略代码
                case SENT:
                    // 省略代码
                case RECEIVED:
                    try {
                        // 将 channel 和 message 传给 ChannelHandler 对象，进行后续的调用，这里的 handler 是 DecodeHandler 类型
                        handler.received(channel, message);
                    } catch (Exception e) {
                        logger.warn("ChannelEventRunnable handle " + state + " operation error, channel is " + channel
                                + ", message is " + message, e);
                    }
                    break;
                case CAUGHT:
                    // 省略代码
                default:
                    logger.warn("unknown state: " + state + ", message is " + message);
            }
        }

    }

    public class DecodeHandler extends AbstractChannelHandlerDelegate {

        private static final Logger log = LoggerFactory.getLogger(DecodeHandler.class);

        public DecodeHandler(ChannelHandler handler) {
            super(handler);
        }

        public void received(Channel channel, Object message) throws RemotingException {
            // 对 Decodeable 接口实现类对象进行解码
            if (message instanceof Decodeable) {
                decode(message);
            }

            if (message instanceof Request) {
                // 对 Request 的 data 字段进行解码
                // Request 的 data 字段为 DecodeableRpcInvocation 类型的对象
                decode(((Request) message).getData());
            }

            if (message instanceof Response) {
                // 对 Request 的 result 字段进行解码
                decode(((Response) message).getResult());
            }
            // 解码完成后，Request 对象会继续向后传递，这里的 handler 为 HeaderExchangeHandler
            handler.received(channel, message);
        }

        private void decode(Object message) {
            // Decodeable 接口目前有两个实现类：DecodeableRpcInvocation 和 DecodeableRpcResult
            if (message != null && message instanceof Decodeable) {
                try {
                    ((Decodeable) message).decode();
                    if (log.isDebugEnabled()) {
                        log.debug(new StringBuilder(32).append("Decode decodeable message ")
                                .append(message.getClass().getName()).toString());
                    }
                } catch (Throwable e) {
                    // 省略代码
                }
            }
        }

    }

    /**
     * 在Exchange层引入了Request和Response语义
     */
    public static class HeaderExchangeHandler implements ChannelHandlerDelegate {

        public void received(Channel channel, Object message) throws RemotingException {
            channel.setAttribute(KEY_READ_TIMESTAMP, System.currentTimeMillis());
            ExchangeChannel exchangeChannel = HeaderExchangeChannel.getOrAddChannel(channel);
            try {
                // 处理请求对象
                if (message instanceof Request) {
                    // handle request.
                    Request request = (Request) message;
                    // 处理事件
                    if (request.isEvent()) {
                        handlerEvent(channel, request);
                    
                    // 处理普通请求
                    } else {
                        // 双向通信
                        if (request.isTwoWay()) {
                            // 真正的调用服务，并且将调用得到的结果封装到 Response 对象中，最后再将该对象返回给服务消费方。
                            // 如果服务调用的过程中发生错误，则将错误信息封装到 Response 对象中，并返回给服务消费方。
                            Response response = handleRequest(exchangeChannel, request);
                            // 将调用结果返回给服务消费端，channel的类型为NettyChannel
                            channel.send(response);
                        }
                        // 如果是单向通信，仅向后调用指定服务即可，无需返回调用结果
                        else {
                            handler.received(exchangeChannel, request.getData());
                        }
                    }
                    // 处理响应对象，服务消费方会执行此处逻辑，后面分析
                } else if (message instanceof Response) {
                    handleResponse(channel, (Response) message);
                } else if (message instanceof String) {
                    // telnet 相关，忽略
                } else {
                    handler.received(exchangeChannel, message);
                }
            } finally {
                HeaderExchangeChannel.removeChannelIfDisconnected(channel);
            }
        }

        Response handleRequest(ExchangeChannel channel, Request req) throws RemotingException {
            Response res = new Response(req.getId(), req.getVersion());
            // 检测请求是否合法，不合法则返回状态码为 BAD_REQUEST 的响应
            if (req.isBroken()) {
                Object data = req.getData();

                String msg;
                if (data == null)
                    msg = null;
                else if (data instanceof Throwable)
                    msg = StringUtils.toString((Throwable) data);
                else
                    msg = data.toString();
                res.setErrorMessage("Fail to decode request due to: " + msg);
                // 设置 BAD_REQUEST 状态
                res.setStatus(Response.BAD_REQUEST);
                return res;
            }
            // 获取 Data 字段量的值，也就是 RpcInvocation 对象
            Object msg = req.getData();
            try {
                // 继续向下调用，这里的 handler 是 DubboProtocol 的内部类，也就是 DubboProtocol$1
                Object result = handler.reply(channel, msg);
                // 设置 OK 状态码
                res.setStatus(Response.OK);
                // 设置调用结果
                res.setResult(result);
            } catch (Throwable e) {
                // 若调用过程出现异常，则设置 SERVICE_ERROR，表示服务端异常
                res.setStatus(Response.SERVICE_ERROR);
                res.setErrorMessage(StringUtils.toString(e));
            }
            return res;
        }

        static void handleResponse(Channel channel, Response response) throws RemotingException {
            // 如果服务器端发送过来的 response 不为 null，并且 response 不是服务器对客户端发送的心跳包的响应
            // Dubbo里面心跳包都是客户端发送给服务器端，然后服务器端返回一个响应
            if (response != null && !response.isHeartbeat()) {
                DefaultFuture.received(channel, response);
            }
        }

    }

    public abstract class AbstractServer extends AbstractEndpoint implements Server {

        protected static final String SERVER_THREAD_POOL_NAME = "DubboServerHandler";
        private static final Logger logger = LoggerFactory.getLogger(AbstractServer.class);
        ExecutorService executor;
        private InetSocketAddress localAddress;
        private InetSocketAddress bindAddress;
        private int accepts;
        private int idleTimeout = 600; // 600 seconds

        public AbstractServer(URL url, ChannelHandler handler) throws RemotingException {
            super(url, handler);
            localAddress = getUrl().toInetSocketAddress();
    
            String bindIp = getUrl().getParameter(Constants.BIND_IP_KEY, getUrl().getHost());
            int bindPort = getUrl().getParameter(Constants.BIND_PORT_KEY, getUrl().getPort());
            if (url.getParameter(Constants.ANYHOST_KEY, false) || NetUtils.isInvalidLocalHost(bindIp)) {
                bindIp = NetUtils.ANYHOST;
            }
            bindAddress = new InetSocketAddress(bindIp, bindPort);
            this.accepts = url.getParameter(Constants.ACCEPTS_KEY, Constants.DEFAULT_ACCEPTS);
            this.idleTimeout = url.getParameter(Constants.IDLE_TIMEOUT_KEY, Constants.DEFAULT_IDLE_TIMEOUT);
            try {
                doOpen();
                if (logger.isInfoEnabled()) {
                    logger.info("Start " + getClass().getSimpleName() + " bind " + getBindAddress() + ", export " + getLocalAddress());
                }
            } catch (Throwable t) {
                throw new RemotingException(url.toInetSocketAddress(), null, "Failed to bind " + getClass().getSimpleName()
                        + " on " + getLocalAddress() + ", cause: " + t.getMessage(), t);
            }
            //fixme replace this with better method
            DataStore dataStore = ExtensionLoader.getExtensionLoader(DataStore.class).getDefaultExtension();
            executor = (ExecutorService) dataStore.get(Constants.EXECUTOR_SERVICE_COMPONENT_KEY, Integer.toString(url.getPort()));
        }

        protected abstract void doOpen() throws Throwable;



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

        public Collection<Channel> getChannels() {
            Collection<Channel> chs = new HashSet<Channel>();
            for (Channel channel : this.channels.values()) {
                if (channel.isConnected()) {
                    chs.add(channel);
                } else {
                    channels.remove(NetUtils.toAddressString(channel.getRemoteAddress()));
                }
            }
            return chs;
        }

    }

    @io.netty.channel.ChannelHandler.Sharable
    public class NettyServerHandler extends ChannelDuplexHandler {

        private final Map<String, Channel> channels = new ConcurrentHashMap<String, Channel>(); // <ip:port, channel>

        private final URL url;

        private final ChannelHandler handler;

        public NettyServerHandler(URL url, ChannelHandler handler) {
            if (url == null) {
                throw new IllegalArgumentException("url == null");
            }
            if (handler == null) {
                throw new IllegalArgumentException("handler == null");
            }
            this.url = url;
            // 这里的handler是NettyServer
            this.handler = handler;
        }

        public Map<String, Channel> getChannels() {
            return channels;
        }

        @Override
        public void channelActive(ChannelHandlerContext ctx) throws Exception {
            ctx.fireChannelActive();

            NettyChannel channel = NettyChannel.getOrAddChannel(ctx.channel(), url, handler);
            try {
                if (channel != null) {
                    channels.put(NetUtils.toAddressString((InetSocketAddress) ctx.channel().remoteAddress()), channel);
                }
                handler.connected(channel);
            } finally {
                NettyChannel.removeChannelIfDisconnected(ctx.channel());
            }
        }

        @Override
        public void channelRead(ChannelHandlerContext ctx, Object msg) throws Exception {
            NettyChannel channel = NettyChannel.getOrAddChannel(ctx.channel(), url, handler);
            try {
                handler.received(channel, msg);
            } finally {
                NettyChannel.removeChannelIfDisconnected(ctx.channel());
            }
        }

        @Override
        public void write(ChannelHandlerContext ctx, Object msg, ChannelPromise promise) throws Exception {
            super.write(ctx, msg, promise);
            NettyChannel channel = NettyChannel.getOrAddChannel(ctx.channel(), url, handler);
            try {
                handler.sent(channel, msg);
            } finally {
                NettyChannel.removeChannelIfDisconnected(ctx.channel());
            }
        }

        @Override
        public void channelInactive(ChannelHandlerContext ctx) throws Exception {
            NettyChannel channel = NettyChannel.getOrAddChannel(ctx.channel(), url, handler);
            try {
                channels.remove(NetUtils.toAddressString((InetSocketAddress) ctx.channel().remoteAddress()));
                handler.disconnected(channel);
            } finally {
                NettyChannel.removeChannelIfDisconnected(ctx.channel());
            }
        }

    }

    public class DubboProtocol extends AbstractProtocol {

        private ExchangeHandler requestHandler = new ExchangeHandlerAdapter() {

            public Object reply(ExchangeChannel channel, Object message) throws RemotingException {
                if (message instanceof Invocation) {
                    Invocation inv = (Invocation) message;
                    // 获取 Invoker 实例
                    Invoker<?> invoker = getInvoker(channel, inv);
                    // need to consider backward-compatibility if it's a callback
                    if (Boolean.TRUE.toString().equals(inv.getAttachments().get(IS_CALLBACK_SERVICE_INVOKE))) {
                        // 回调相关，忽略
                    }
                    RpcContext.getContext().setRemoteAddress(channel.getRemoteAddress());
                    // 通过 Invoker 调用具体的服务
                    return invoker.invoke(inv);
                }
                throw new RemotingException(channel,
                        "Unsupported request: " + message == null ? null
                                : (message.getClass().getName() + ": " + message) + ", channel: consumer: "
                                        + channel.getRemoteAddress() + " --> provider: " + channel.getLocalAddress());
            }
        };

        Invoker<?> getInvoker(Channel channel, Invocation inv) throws RemotingException {
            boolean isCallBackServiceInvoke = false;
            boolean isStubServiceInvoke = false;
            int port = channel.getLocalAddress().getPort();
            String path = inv.getAttachments().get(Constants.PATH_KEY);

            // 省略存根与回调的逻辑

            // 计算 service key，格式为 groupName/serviceName:serviceVersion:port。比如：
            // dubbo/com.alibaba.dubbo.demo.DemoService:1.0.0:20880
            String serviceKey = serviceKey(port, path, inv.getAttachments().get(Constants.VERSION_KEY),
                    inv.getAttachments().get(Constants.GROUP_KEY));

            // 从 exporterMap 查找与 serviceKey 相对应的 DubboExporter 对象，服务导出过程中会将 <serviceKey, DubboExporter> 映射关系存储到 exporterMap 集合中
            DubboExporter<?> exporter = (DubboExporter<?>) exporterMap.get(serviceKey);

            if (exporter == null)
                throw new RemotingException(channel,
                        "Not found exported service: " + serviceKey + " in " + exporterMap.keySet()
                                + ", may be version or group mismatch " + ", channel: consumer: "
                                + channel.getRemoteAddress() + " --> provider: " + channel.getLocalAddress()
                                + ", message:" + inv);

            // 获取 Invoker 对象，并返回
            return exporter.getInvoker();
        }

    }

    public abstract class AbstractProxyInvoker<T> implements Invoker<T> {

        @Override
        public Result invoke(Invocation invocation) throws RpcException {
            try {
                // 调用 doInvoke 执行后续的调用，并将调用结果封装到 RpcResult 中
                // this 为 JavassistProxyFactory 类型的对象
                return new RpcResult(doInvoke(proxy, invocation.getMethodName(), invocation.getParameterTypes(),
                        invocation.getArguments()));
            } catch (InvocationTargetException e) {
                return new RpcResult(e.getTargetException());
            } catch (Throwable e) {
                throw new RpcException("Failed to invoke remote proxy method ...");
            }
        }

        protected abstract Object doInvoke(T proxy, String methodName, Class<?>[] parameterTypes, Object[] arguments)
                throws Throwable;
    }

    public class JavassistProxyFactory extends AbstractProxyFactory {

        // 省略其他方法

        @Override
        public <T> Invoker<T> getInvoker(T proxy, Class<T> type, URL url) {
            final Wrapper wrapper = Wrapper
                    .getWrapper(proxy.getClass().getName().indexOf('$') < 0 ? proxy.getClass() : type);
            // 创建匿名类对象
            return new AbstractProxyInvoker<T>(proxy, type, url) {
                @Override
                protected Object doInvoke(T proxy, String methodName, Class<?>[] parameterTypes, Object[] arguments)
                        throws Throwable {
                    // 调用 invokeMethod 方法进行后续的调用
                    return wrapper.invokeMethod(proxy, methodName, parameterTypes, arguments);
                }
            };
        }
    }

    /** Wrapper0 是在运行时生成的，大家可使用 Arthas 进行反编译 */
    public class Wrapper0 extends Wrapper implements ClassGenerator.DC {
        public static String[] pns;
        public static Map pts;
        public static String[] mns;
        public static String[] dmns;
        public static Class[] mts0;

        // 省略其他方法

        public Object invokeMethod(Object object, String string, Class[] arrclass, Object[] arrobject)
                throws InvocationTargetException {
            DemoService demoService;
            try {
                // 类型转换
                demoService = (DemoService) object;
            } catch (Throwable throwable) {
                throw new IllegalArgumentException(throwable);
            }
            try {
                // 根据方法名调用指定的方法
                if ("sayHello".equals(string) && arrclass.length == 1) {
                    return demoService.sayHello((String) arrobject[0]);
                }
            } catch (Throwable throwable) {
                throw new InvocationTargetException(throwable);
            }
            throw new NoSuchMethodException(new StringBuffer().append("Not found method \"").append(string)
                    .append("\" in class com.alibaba.dubbo.demo.DemoService.").toString());
        }
    }

}