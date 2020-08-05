public class DubboServiceInvokingProcess {

    /**
     * Dubbo服务调用的具体过程如下： 首先服务消费者通过代理对象 Proxy 发起远程调用，接着通过网络客户端 Client
     * 将编码后的请求发送给服务提供方的网络层上，也就是 Server。Server 在收到请求后，
     * 首先要做的事情是对数据包进行解码。然后将解码后的请求发送至分发器 Dispatcher，再由分发器将请求派发到指定的线程池上，最后由线程池调用具体的服务。
     * 这就是一个远程调用请求的发送与接收过程。
     * 
     * Dubbo
     * 支持同步和异步两种调用方式，其中异步调用还可细分为"有返回值"的异步调用和"无返回值"的异步调用。所谓"无返回值"异步调用是指服务消费方只管调用，但不关心调用结果，
     * 此时 Dubbo 会直接返回一个空的 RpcResult。若要使用异步特性，需要服务消费方手动进行配置。默认情况下，Dubbo 使用同步调用方式。Dubbo同步与异步的区别在于future对象的
     * get方法（此方法是阻塞方法）是由谁来调用，如果是同步的话，就是由框架来调用，否则就是由用户自己来调用。
     */

    /**
     * 本节主要分析消费者发起RPC调用的过程
     * 
     * proxy0#sayHello(String)
     *  —> InvokerInvocationHandler#invoke(Object, Method, Object[])
     *  —> MockClusterInvoker#invoke(Invocation)
     *  —> AbstractClusterInvoker#invoke(Invocation)
     *  —> FailoverClusterInvoker#doInvoke(Invocation, List<Invoker<T>>, LoadBalance)
     *  -> InvokerWrapper#invoke(Invocation)
     *  —> Filter#invoke(Invoker, Invocation)  // 包含多个 Filter 调用
     *  —> ListenerInvokerWrapper#invoke(Invocation) 
     *  —> AbstractInvoker#invoke(Invocation) 
     *  —> DubboInvoker#doInvoke(Invocation)
     *  —> ReferenceCountExchangeClient#request(Object, int)
     *  —> HeaderExchangeClient#request(Object, int)
     *  —> HeaderExchangeChannel#request(Object, int)
     *  —> AbstractPeer#send(Object)
     *  —> AbstractClient#send(Object, boolean)
     *  —> NettyChannel#send(Object, boolean)
     *  —> NioClientSocketChannel#write(Object)
     */

    public class JavassistProxyFactory extends AbstractProxyFactory {

        @SuppressWarnings("unchecked")
        public <T> T getProxy(Invoker<T> invoker, Class<?>[] interfaces) {
            // 生成 Proxy 子类（Proxy 是抽象类）。并调用 Proxy 子类的 newInstance 方法创建 Proxy 实例
            // 服务消费者通过ProxyFactory获取到一个代理对象proxy，然后调用的实际上是这个代理对象的方法。而在这个代理对象中，实际上是通过调用
            // invoker的invoke方法来真正向服务器发起调用。
            return (T) Proxy.getProxy(interfaces).newInstance(new InvokerInvocationHandler(invoker));
        }
    
        public <T> Invoker<T> getInvoker(T proxy, Class<T> type, URL url) {
            // wrapper是由javassist编译器动态生成的一个对象
            final Wrapper wrapper = Wrapper.getWrapper(proxy.getClass().getName().indexOf('$') < 0 ? proxy.getClass() : type);
            // 返回一个匿名内部类对象，这个类继承了AbstractProxyInvoker类，并且实现了doInvoke方法。当我们调用invoker的invoke方法时，最终会调用到
            // wrapper的invokeMethod方法
            return new AbstractProxyInvoker<T>(proxy, type, url) {
                @Override
                protected Object doInvoke(T proxy, String methodName,
                                          Class<?>[] parameterTypes,
                                          Object[] arguments) throws Throwable {
                    return wrapper.invokeMethod(proxy, methodName, parameterTypes, arguments);
                }
            };
        }
    }

    public abstract class AbstractProxyInvoker<T> implements Invoker<T> {

        private final T proxy;
    
        private final Class<T> type;
    
        private final URL url;
    
        public AbstractProxyInvoker(T proxy, Class<T> type, URL url) {
            // 参数检查
            this.proxy = proxy;
            this.type = type;
            this.url = url;
        }
    
        public Class<T> getInterface() {
            return type;
        }
    
        public URL getUrl() {
            return url;
        }
    
        public boolean isAvailable() {
            return true;
        }
    
        public void destroy() {
        }
    
        public Result invoke(Invocation invocation) throws RpcException {
            try {
                return new RpcResult(doInvoke(proxy, invocation.getMethodName(), invocation.getParameterTypes(), invocation.getArguments()));
            } catch (InvocationTargetException e) {
                return new RpcResult(e.getTargetException());
            } catch (Throwable e) {
                throw new RpcException("Failed to invoke remote proxy method " + invocation.getMethodName() + " to " + getUrl() + ", cause: " + e.getMessage(), e);
            }
        }
    
        protected abstract Object doInvoke(T proxy, String methodName, Class<?>[] parameterTypes, Object[] arguments) throws Throwable;
    
        @Override
        public String toString() {
            return getInterface() + " -> " + (getUrl() == null ? " " : getUrl().toString());
        }
    
    
    }

    public class proxy0 implements ClassGenerator.DC, EchoService, DemoService {
        // 方法数组
        public static Method[] methods;
        private InvocationHandler handler;

        public proxy0(InvocationHandler invocationHandler) {
            this.handler = invocationHandler;
        }

        public proxy0() {
        }

        public String sayHello(java.lang.String arg0) {
            Object[] args = new Object[1];
            // 将参数存储到 Object 数组中
            args[0] = ($w) $1;
            // 调用 InvocationHandler 实现类的 invoke 方法得到调用结果
            Object ret = handler.invoke(this, methods[0], args);
            // 返回调用结果
            return (java.lang.String) ret;
        }

        /** 回声测试方法 */
        public Object $echo(Object object) {
            Object[] arrobject = new Object[] { object };
            Object object2 = this.handler.invoke(this, methods[1], arrobject);
            return object2;
        }
    }

    public class InvokerInvocationHandler implements InvocationHandler {

        private final Invoker<?> invoker;

        // InvokerInvocationHandler 中的 invoker 成员变量类型为
        // MockClusterInvoker，MockClusterInvoker 内部封装了服务降级逻辑。
        public InvokerInvocationHandler(Invoker<?> invoker) {
            this.invoker = invoker;
        }

        @Override
        public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
            String methodName = method.getName();
            Class<?>[] parameterTypes = method.getParameterTypes();

            // 省略代码

            // 将 method 和 args 封装到 RpcInvocation 中，并执行后续的调用
            return invoker.invoke(new RpcInvocation(method, args)).recreate();
        }
    }

    /**
     * Mock 是 Stub 的一个子集，便于服务提供方在客户端执行容错逻辑，因经常需要在出现 RpcException (比如网络失败，超时等)时进行容错，而在出现业务异常(比如登录用户名密码错误)时不需要容错，
     * 如果用 Stub，可能就需要捕获并依赖 RpcException 类，而用 Mock 就可以不依赖 RpcException，因为它的约定就是只有出现 RpcException 时才执行。
     * MockClusterInvoker中包含的invoker实例是FailoverClusterInvoker（默认），也有可能是其它集群管理类。
     */
    public class MockClusterInvoker<T> implements Invoker<T> {

        private final Invoker<T> invoker;

        // 本地伪装 Mock 通常用于服务降级，比如某验权服务，当服务提供方全部挂掉后，客户端不抛出异常，而是通过 Mock 数据返回授权失败。
        public Result invoke(Invocation invocation) throws RpcException {
            Result result = null;

            // 获取 mock 配置值
            String value = directory.getUrl()
                    .getMethodParameter(invocation.getMethodName(), Constants.MOCK_KEY, Boolean.FALSE.toString())
                    .trim();
            if (value.length() == 0 || value.equalsIgnoreCase("false")) {
                // 无 mock 逻辑，直接调用其他 Invoker 对象的 invoke 方法，比如 FailoverClusterInvoker
                result = this.invoker.invoke(invocation);
            } else if (value.startsWith("force")) {
                // 在 dubbo2.6.6 版本中，可以开始在 Spring XML 中使用 fail 和 force。force 代表强制是使用 Mock
                // 的行为，在这种情况下不会使用走远程调用，
                if (logger.isWarnEnabled()) {
                    logger.info("force-mock: " + invocation.getMethodName() + " force-mock enabled , url : "
                            + directory.getUrl());
                }
                // force:direct mock
                result = doMockInvoke(invocation, null);
            } else {
                // fail-mock
                // fail: 与默认行为一致，只有当远程调用发生错误时才使用 Mock 行为
                try {
                    result = this.invoker.invoke(invocation);
                } catch (RpcException e) {
                    if (e.isBiz()) {
                        throw e;
                    } else {
                        if (logger.isWarnEnabled()) {
                            logger.info("fail-mock: " + invocation.getMethodName() + " fail-mock enabled , url : "
                                    + directory.getUrl(), e);
                        }
                        // 失败之后才是由 Mock 行为
                        result = doMockInvoke(invocation, e);
                    }
                }
            }
            return result;
        }

    }

    public abstract class AbstractInvoker<T> implements Invoker<T> {

        private final Map<String, String> attachment;

        // invoke方法的大部分代码用于添加信息到 RpcInvocation#attachment 变量中，添加完毕后，调用 doInvoke 执行后续的调用。
        // doInvoke 是一个抽象方法，需要由子类实现
        public Result invoke(Invocation inv) throws RpcException {
            if (destroyed.get()) {
                throw new RpcException("Rpc invoker for service is DESTROYED, can not be invoked any more!");
            }

            RpcInvocation invocation = (RpcInvocation) inv;
            // 设置 Invoker
            invocation.setInvoker(this);
            if (attachment != null && attachment.size() > 0) {
                // 设置 attachment
                invocation.addAttachmentsIfAbsent(attachment);
            }
            Map<String, String> context = RpcContext.getContext().getAttachments();
            if (context != null) {
                // 添加 contextAttachments 到 RpcInvocation#attachments 的 Map 变量中
                invocation.addAttachmentsIfAbsent(context);
            }
            if (getUrl().getMethodParameter(invocation.getMethodName(), Constants.ASYNC_KEY, false)) {
                // 执行到这一步，说明开启了异步调用（默认是不开启的），设置异步信息到 RpcInvocation 的 attachments 变量中
                invocation.setAttachment(Constants.ASYNC_KEY, Boolean.TRUE.toString());
            }

            RpcUtils.attachInvocationIdIfAsync(getUrl(), invocation);

            try {
                // 抽象方法，由子类实现
                return doInvoke(invocation);
            } catch (InvocationTargetException e) { // biz exception
                // 代码省略......
            } catch (RpcException e) {
                // 代码省略......
            } catch (Throwable e) {
                return new RpcResult(e);
            }
        }

        protected abstract Result doInvoke(Invocation invocation) throws Throwable;
    }

    public class DubboInvoker<T> extends AbstractInvoker<T> {

        private final ExchangeClient[] clients;

        private final AtomicPositiveInteger index = new AtomicPositiveInteger();

        /**
         * 前面说过，Dubbo 的调用方式可以细分为3种：同步调用、异步无返回值（单向通信）、异步有返回值。
         * 同步调用和异步调用的区别是由谁调用 RpcFuture 的get 方法，同步调用是由框架自身调用 get 方法，而异步调用则是由用户调用 get 方法
         */
        protected Result doInvoke(final Invocation invocation) throws Throwable {
            RpcInvocation inv = (RpcInvocation) invocation;
            final String methodName = RpcUtils.getMethodName(invocation);
            // 设置 path 和 version 到 RpcInvocation 的 attachments 变量中
            inv.setAttachment(Constants.PATH_KEY, getUrl().getPath());
            inv.setAttachment(Constants.VERSION_KEY, version);

            ExchangeClient currentClient;
            if (clients.length == 1) {
                // 从 clients 中数组中获取 ExchangeClient
                currentClient = clients[0];
            } else {
                // 如果有多个到服务器端的连接，那么按其在数组中的递增顺序从其中选一个
                currentClient = clients[index.getAndIncrement() % clients.length];
            }

            try {
                // 获取异步配置
                boolean isAsync = RpcUtils.isAsync(getUrl(), invocation);
                // isOneway 为 true，表明单向通信，也就是异步无返回值
                // isOneway其实就是根据url中return参数的值，如果return为false，则表明不关注返回值，因此在后面中不会在RpcContext中设置future
                boolean isOneway = RpcUtils.isOneway(getUrl(), invocation);
                int timeout = getUrl().getMethodParameter(methodName, Constants.TIMEOUT_KEY, Constants.DEFAULT_TIMEOUT);

                // 如果是异步无返回值的话
                // 一些特殊场景下，为了尽快调用返回，可以设置是否等待消息发出:
                // sent="true" 等待消息发出，消息发送失败将抛出异常；
                // sent="false" 不等待消息发出，将消息放入 IO 队列，即刻返回。
                // 默认为false
                if (isOneway) {
                    boolean isSent = getUrl().getMethodParameter(methodName, Constants.SENT_KEY, false);
                    // 发送请求
                    currentClient.send(inv, isSent);
                    // 将 context 中的 future 设置为 null
                    RpcContext.getContext().setFuture(null);
                    // 返回一个空的 RpcResult
                    return new RpcResult();

                // 如果是异步有返回值的话
                } else if (isAsync) {
                    // 发送请求，并得到一个 ResponseFuture 实例
                    ResponseFuture future = currentClient.request(inv, timeout);
                    // 设置 future 到上下文中
                    RpcContext.getContext().setFuture(new FutureAdapter<Object>(future));
                    // 暂时返回一个空结果
                    return new RpcResult();
                
                // 如果是同步调用的话
                } else {
                    RpcContext.getContext().setFuture(null);
                    // 发送请求，得到一个 ResponseFuture 实例，并调用该实例的 get 方法进行等待。ResponseFuture 是一个接口，而DefaultFuture 则是它的默认实现类
                    return (Result) currentClient.request(inv, timeout).get();
                }
            
            // 在抛出 TimeoutException 和 RemotingException 之后，都会将其封装成 RpcException 对象，这个 RpcException 可以被上级（比如 FailoverClusterInvoker）捕获
            } catch (TimeoutException e) {
                throw new RpcException(RpcException.TIMEOUT_EXCEPTION, "Invoke remote method timeout. method: "
                                + invocation.getMethodName() + ", provider: " + getUrl() + ", cause: " + e.getMessage(), e);
            } catch (RemotingException e) {
                throw new RpcException(RpcException.NETWORK_EXCEPTION, "Failed to invoke remote method: "
                        + invocation.getMethodName() + ", provider: " + getUrl() + ", cause: " + e.getMessage(), e);
            }
        }
    }

    /**
     * 本篇文章在多个地方都强调过调用编号很重要，但一直没有解释原因，这里简单说明一下。一般情况下，服务消费方会并发调用多个服务，每个用户线程发送请求后，会调用不同 DefaultFuture 对象的
     * get 方法进行等待。 一段时间后，服务消费方的线程池会收到多个响应对象。这个时候要考虑一个问题，如何将每个响应对象传递给相应的 DefaultFuture 对象，且不出错。答案是通过调用编号。
     * DefaultFuture 被创建时，会要求传入一个 Request 对象。此时 DefaultFuture 可从 Request 对象中获取调用编号，并将 <调用编号, DefaultFuture 对象> 映射关系存入到静态 Map 中，
     * 即 FUTURES。线程池中的线程在收到 Response 对象后，会根据 Response 对象中的调用编号到 FUTURES 集合中取出相应的 DefaultFuture 对象，然后再将 Response 对象设置到 DefaultFuture 对象中。
     * 最后再唤醒用户线程，这样用户线程即可从 DefaultFuture 对象中获取调用结果了。
     */
    public static class DefaultFuture implements ResponseFuture {

        private final int timeout;

        private final Lock lock = new ReentrantLock();

        private final Condition done = lock.newCondition();

        private volatile Response response;

        private static final Map<Long, DefaultFuture> FUTURES = new ConcurrentHashMap<Long, DefaultFuture>();

        private volatile ResponseCallback callback;

        // 类加载的时候会启动一个超时扫描线程
        static {
            Thread th = new Thread(new RemotingInvocationTimeoutScan(), "DubboResponseTimeoutScanTimer");
            th.setDaemon(true);
            th.start();
        }

        private static class RemotingInvocationTimeoutScan implements Runnable {

            @Override
            public void run() {
                while (true) {
                    try {
                        // 扫描 FUTURES 集合
                        for (DefaultFuture future : FUTURES.values()) {
                            if (future == null || future.isDone()) {
                                continue;
                            }
                            // 如果future未完成，且超时
                            if (System.currentTimeMillis() - future.getStartTimestamp() > future.getTimeout()) {
                                // 创建一个异常的Response
                                Response timeoutResponse = new Response(future.getId());
                                // set timeout status.
                                timeoutResponse.setStatus(future.isSent() ? Response.SERVER_TIMEOUT : Response.CLIENT_TIMEOUT);
                                timeoutResponse.setErrorMessage(future.getTimeoutMessage(true));
                                // 处理异常
                                DefaultFuture.received(future.getChannel(), timeoutResponse);
                            }
                        }
                        Thread.sleep(30);
                    } catch (Throwable e) {
                        logger.error("Exception when scan the timeout invocation of remoting.", e);
                    }
                }
            }
        }

        public static void received(Channel channel, Response response) {
            try {
                // 当接收到服务端的返回值时，从 FUTURES 这个 map 中移除掉此 response 对应的 DefaultFuture 对象
                DefaultFuture future = FUTURES.remove(response.getId());
                if (future != null) {
                    // 真正接收 response 对象
                    future.doReceived(response);
                } else {
                    // 省略日志代码
                }
            } finally {
                CHANNELS.remove(response.getId());
            }
        }

        public Object get() throws RemotingException {
            return get(timeout);
        }

        public Object get(int timeout) throws RemotingException {
            if (timeout <= 0) {
                timeout = Constants.DEFAULT_TIMEOUT;
            }
            // 检测服务提供方是否成功返回了调用结果
            if (!isDone()) {
                long start = System.currentTimeMillis();
                lock.lock();
                try {
                    // 循环检测服务提供方是否成功返回了调用结果
                    while (!isDone()) {
                        // 如果调用结果尚未返回，这里等待一段时间
                        done.await(timeout, TimeUnit.MILLISECONDS);
                        // 如果调用结果成功返回，或等待超时，此时跳出 while 循环，执行后续的逻辑
                        if (isDone() || System.currentTimeMillis() - start > timeout) {
                            break;
                        }
                    }
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                } finally {
                    lock.unlock();
                }
                if (!isDone()) {
                    throw new TimeoutException(sent > 0, channel, getTimeoutMessage(false));
                }
            }
            return returnFromResponse();
        }

        private Object returnFromResponse() throws RemotingException {
            Response res = response;
            if (res == null) {
                throw new IllegalStateException("response cannot be null");
            }

            // 如果调用结果的状态为 Response.OK，则表示调用过程正常，服务提供方成功返回了调用结果
            if (res.getStatus() == Response.OK) {
                return res.getResult();
            }

            // 消费者或者提供者出现超时情况的话，抛出异常
            if (res.getStatus() == Response.CLIENT_TIMEOUT || res.getStatus() == Response.SERVER_TIMEOUT) {
                throw new TimeoutException(res.getStatus() == Response.SERVER_TIMEOUT, channel, res.getErrorMessage());
            }
            throw new RemotingException(channel, res.getErrorMessage());
        }

        public boolean isDone() {
            return response != null;
        }

        private void doReceived(Response res) {
            lock.lock();
            try {
                // 接收到返回值，向在 done 变量上阻塞的线程发送信号进行唤醒操作
                response = res;
                if (done != null) {
                    done.signal();
                }
            } finally {
                lock.unlock();
            }
            // 对 callback 进行回调操作
            if (callback != null) {
                invokeCallback(callback);
            }
        }

        private void invokeCallback(ResponseCallback c) {
            ResponseCallback callbackCopy = c;
            if (callbackCopy == null) {
                throw new NullPointerException("callback cannot be null.");
            }
            c = null;
            Response res = response;
            if (res == null) {
                throw new IllegalStateException("response cannot be null. url:" + channel.getUrl());
            }

            // 如果返回的结果正常，则回调 callbackCopy 的 done 方法
            if (res.getStatus() == Response.OK) {
                try {
                    callbackCopy.done(res.getResult());
                } catch (Exception e) {
                    logger.error("callback invoke error .reasult:" + res.getResult() + ",url:" + channel.getUrl(), e);
                }

                // 如果返回的结果中显示，消费者或者提供者出现了超时异常，则构造超时异常 TimeoutException ，并且回调 callbackCopy 的
                // caught 方法
            } else if (res.getStatus() == Response.CLIENT_TIMEOUT || res.getStatus() == Response.SERVER_TIMEOUT) {
                try {
                    TimeoutException te = new TimeoutException(res.getStatus() == Response.SERVER_TIMEOUT, channel,
                            res.getErrorMessage());
                    callbackCopy.caught(te);
                } catch (Exception e) {
                    logger.error("callback invoke error ,url:" + channel.getUrl(), e);
                }

                // 出现其他异常，使用错误信构造异常，并且回调 callbackCopy 的 caught 方法
            } else {
                try {
                    RuntimeException re = new RuntimeException(res.getErrorMessage());
                    callbackCopy.caught(re);
                } catch (Exception e) {
                    logger.error("callback invoke error ,url:" + channel.getUrl(), e);
                }
            }
        }



    }

    /**
     * ReferenceCountExchangeClient 内部定义了一个引用计数变量 referenceCount，每当该对象被引用一次 referenceCount 都会进行自增。每当 close 方法被调用时，referenceCount 进行自减。
     * ReferenceCountExchangeClient 内部仅实现了一个引用计数的功能，其他方法并无复杂逻辑，均是直接调用被装饰对象的相关方法。可以说，ReferenceCountExchangeClient 专注于引用计数的逻辑
     */
    final class ReferenceCountExchangeClient implements ExchangeClient {

        private ExchangeClient client;

        private final URL url;

        private final AtomicInteger refenceCount = new AtomicInteger(0);

        public ReferenceCountExchangeClient(ExchangeClient client, ConcurrentMap<String, LazyConnectExchangeClient> ghostClientMap) {
            this.client = client;
            refenceCount.incrementAndGet();
            this.url = client.getUrl();
            if (ghostClientMap == null) {
                throw new IllegalStateException("ghostClientMap can not be null, url: " + url);
            }
            this.ghostClientMap = ghostClientMap;
        }

        public ResponseFuture request(Object request) throws RemotingException {
            // 直接调用被装饰对象 client 的同签名方法，也就是调用 HeaderExchangeClient 的 request 方法
            return client.request(request);
        }

        public ResponseFuture request(Object request, int timeout) throws RemotingException {
            // 直接调用被装饰对象 client 的同签名方法，也就是调用 HeaderExchangeClient 的 request 方法
            return client.request(request, timeout);
        }

        public void send(Object message) throws RemotingException {
            client.send(message);
        }

        public void send(Object message, boolean sent) throws RemotingException {
            client.send(message, sent);
        }

       // referenceCount 引用计数自增，该方法由外部调用
        public void incrementAndGetCount() {
            refenceCount.incrementAndGet();
        }

        public void close() {
            close(0);
        }
    
        public void close(int timeout) {
            // referenceCount 引用计数自减
            if (refenceCount.decrementAndGet() <= 0) {
                if (timeout == 0) {
                    client.close();
                } else {
                    client.close(timeout);
                }
                client = replaceWithLazyClient();
            }
        }
    }

    /**
     * 提供心跳检查功能；将send、request、close等事件转由HeaderExchangeChannel处理，HeaderExchangeChannel对象中的Channel为所选的NIO框架对应的client对象。
     * ExchangeClient 实际上并不具备通信能力，它需要基于更底层的客户端实例进行通信，比如 NettyClient。HeaderExchangeClient 中很多方法只有一行代码，
     * 即调用 HeaderExchangeChannel 对象的同签名方法。那 HeaderExchangeClient 有什么用处呢？答案是封装了一些关于心跳检测的逻辑。
     */
    public class HeaderExchangeClient implements ExchangeClient {

        private static final ScheduledThreadPoolExecutor scheduled = new ScheduledThreadPoolExecutor(2, new NamedThreadFactory("dubbo-remoting-client-heartbeat", true));
        private final Client client;
        private final ExchangeChannel channel;
        // heartbeat timer
        private ScheduledFuture<?> heartbeatTimer;
        private int heartbeat;
        // heartbeat timeout (ms), default value is 0 , won't execute a heartbeat.
        private int heartbeatTimeout;

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
            // 直接调用 HeaderExchangeChannel 对象的同签名方法
            return channel.request(request);
        }

        public ResponseFuture request(Object request, int timeout) throws RemotingException {
            // 直接调用 HeaderExchangeChannel 对象的同签名方法
            return channel.request(request, timeout);
        }

        public void send(Object message) throws RemotingException {
            channel.send(message);
        }
    
        public void send(Object message, boolean sent) throws RemotingException {
            channel.send(message, sent);
        }

        public void close() {
            doClose();
            channel.close();
        }

        private void doClose() {
            stopHeartbeatTimer();
        }

    }

    /**
     * 主要是完成同步转异步，在request(Object request,int timeout)方法中，将请求转换成Request对象，将请求消息设置到data属性上，
     * 构建DefaultFuture对象，调用NIO框架对应的Client对象（默认NettyClient）的send方法将消息发送出去，返回DefultFuture对象。
     * 
     * Exchange 层为框架引入 Request 和 Response 语义
     */
    final class HeaderExchangeChannel implements ExchangeChannel {

        private final Channel channel;

        HeaderExchangeChannel(Channel channel) {
            if (channel == null) {
                throw new IllegalArgumentException("channel == null");
            }
            // 这里的 channel 就是前面 HeaderExchangeClient 的构造函数中 client 对象，其实指向的是 NettyClient
            this.channel = channel;
        }

        public ResponseFuture request(Object request) throws RemotingException {
            return request(request, channel.getUrl().getPositiveParameter(Constants.TIMEOUT_KEY, Constants.DEFAULT_TIMEOUT));
        }

        public ResponseFuture request(Object request, int timeout) throws RemotingException {
            if (closed) {
                throw new RemotingException(this.getLocalAddress(), null,
                        "Failed to send request " + request + ", cause: The channel " + this + " is closed!");
            }
            // 创建 Request 对象
            Request req = new Request();
            req.setVersion(Version.getProtocolVersion());
            // 设置双向通信标志为 true
            req.setTwoWay(true);
            // 这里的 request 变量类型为 RpcInvocation
            req.setData(request);        
            // 创建 DefaultFuture 对象
            DefaultFuture future = new DefaultFuture(channel, req, timeout);
            try {
                // 调用 NettyClient 的 send 方法发送请求，需要说明的是，NettyClient 中并未实现 send 方法，该方法继承自父类 AbstractPeer
                channel.send(req);
            } catch (RemotingException e) {
                future.cancel();
                throw e;
            }
            // 返回 DefaultFuture 对象
            return future;
        }

        public void send(Object message) throws RemotingException {
            send(message, getUrl().getParameter(Constants.SENT_KEY, false));
        }
    
        public void send(Object message, boolean sent) throws RemotingException {
            if (closed) {
                throw new RemotingException(this.getLocalAddress(), null, "Failed to send message " + message + ", cause: The channel " + this + " is closed!");
            }
            if (message instanceof Request
                    || message instanceof Response
                    || message instanceof String) {
                channel.send(message, sent);
            } else {
                // 如果message不是Request或者Response对象的话，就将其封装到Request对象中，然后调用NettyClient的send方法进行发送
                Request request = new Request();
                request.setVersion("2.0.0");
                request.setTwoWay(false);
                request.setData(message);
                channel.send(request, sent);
            }
        }
    }

    // class:AbstractPeer
    public void send(Object message) throws RemotingException {
        send(message, url.getParameter(Constants.SENT_KEY, false));
    }

    // AbstractClient的子类有GrizzlyClient，MinaClient，NettyClient，NettyClient（分别是Netty3和Netty4）
    public abstract class AbstractClient extends AbstractEndpoint implements Client {
 
        private final boolean send_reconnect;

        private final AtomicInteger reconnect_count = new AtomicInteger(0);

        // Reconnection error log has been called before?
        private final AtomicBoolean reconnect_error_log_flag = new AtomicBoolean(false);

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

        public void send(Object message, boolean sent) throws RemotingException {
            // 在发送时，如果没有连接，那么就会调用AbstractClient中的connect方法进行连接
            if (send_reconnect && !isConnected()) {
                connect();
            }

            // 获取的并不是网络通讯框架的channel，比如Netty中的原生channel，而是Dubbo对这个channel的封装类对象，比如NettyChannel
            Channel channel = getChannel();
            if (channel == null || !channel.isConnected()) {
                throw new RemotingException(this, "message can not send, because channel is closed . url:" + getUrl());
            }

            // 调用 NettyChannel 的 send 方法
            channel.send(message, sent);
        }

        protected abstract Channel getChannel();

    }

    public class NettyClient extends AbstractClient {

        private volatile Channel channel;

        private static final NioEventLoopGroup nioEventLoopGroup = new NioEventLoopGroup(Constants.DEFAULT_IO_THREADS, new DefaultThreadFactory("NettyClientWorker", true));

        private Bootstrap bootstrap;

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

        @Override
        protected com.alibaba.dubbo.remoting.Channel getChannel() {
            // 这里的channel就是上面doConnect方法中创建并且保存的，并且是Netty中的原生channel
            Channel c = channel;
            if (c == null || !c.isConnected())
                return null;

            // 获取/创建一个 NettyChannel 类型对象
            return NettyChannel.getOrAddChannel(c, getUrl(), this);
        }

    }

    final static class NettyChannel extends AbstractChannel {

        private static final ConcurrentMap<Channel, NettyChannel> channelMap = new ConcurrentHashMap<Channel, NettyChannel>();

        // 这里的channel为Netty中的channel
        private final Channel channel;

        private NettyChannel(Channel channel, URL url, ChannelHandler handler) {
            super(url, handler);
            if (channel == null) {
                throw new IllegalArgumentException("netty channel == null;");
            }
            this.channel = channel;
        }

        static NettyChannel getOrAddChannel(org.jboss.netty.channel.Channel ch, URL url, ChannelHandler handler) {
            if (ch == null) {
                return null;
            }
            NettyChannel ret = channelMap.get(ch);
            // 如果 ret = null，则创建一个新的 NettyChannel 实例
            if (ret == null) {
                // 创建一个NettyChannel对象，并且将 <Channel, NettyChannel> 键值对存入 channelMap 集合中
                NettyChannel nc = new NettyChannel(ch, url, handler);
                if (ch.isConnected()) {
                    // return the previous value associated with the specified key, or null if there was no mapping for the key
                    ret = channelMap.putIfAbsent(ch, nc);
                }
                if (ret == null) {
                    ret = nc;
                }
            }
            return ret;
        }

        public void send(Object message, boolean sent) throws RemotingException {
            super.send(message, sent);
     
            boolea success = true;
            int timeout = 0;
            try {
                // 最终调用Netty中原生的channel将这个message发送出去
                ChannelFuture future = channel.write(message);
                // sent 的值源于 <dubbo:method sent="true/false" /> 中 sent 的配置值，有两种配置值：
                //   1. true: 等待消息发出，消息发送失败将抛出异常
                //   2. false: 不等待消息发出，将消息放入 IO 队列，即刻返回
                // 默认情况下 sent = false
                if (sent) {
                    timeout = getUrl().getPositiveParameter(Constants.TIMEOUT_KEY, Constants.DEFAULT_TIMEOUT);
                    // future 为 netty 中的 DefaultChannelFuture 类对象
                    // future 阻塞 timeout 个单位时间，如果执行完成的话，就返回 true，如果 timeout 时间内还没有完成，那么
                    // 就直接返回 false，同时 await 方法默认响应中断，如果等待任务执行完成的过程中被中断，那么会抛出中断异常
                    success = future.await(timeout);
                }
            } catch (Throwable e) {
                throw new RemotingException(this, "Failed to send message " + message + " to " + getRemoteAddress() + ", cause: " + e.getMessage(), e);
            }
    
        }
    }

    // ExchangeClient 有3个子类：HeaderExchangeClient、ReferenceCountExchangeClient、LazyConnectExchangeClient
    public interface ExchangeClient extends Client, ExchangeChannel {
    }


    // ExchangeChannel 有4个子类:HeaderExchangerChannel、HeaderExchangeClient、ReferenceCountExchangeClient、LazyConnectExchangeClient
    public interface ExchangeChannel extends Channel{

        ResponseFuture request(Object request);

        ResponseFuture request(Object request, int timeout) throws RemotingException;
    
        ExchangeHandler getExchangeHandler();
    
        void close(int timeout);
    
    }







}