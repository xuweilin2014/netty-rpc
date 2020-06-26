public class DubboServiceInvokingProcess2 {

    /**
     * 解码器将数据包解析成 Request 对象后，NettyHandler 的 messageReceived
     * 方法紧接着会收到这个对象，并将这个对象继续向下传递。这期间该对象会被依次传递给
     * NettyServer、MultiMessageHandler、HeartbeatHandler 以及 AllChannelHandler。最后由
     * AllChannelHandler 将该对象封装到 Runnable 实现类对象中， 并将 Runnable
     * 放入线程池中执行后续的调用逻辑。整个调用栈如下：
     * 
     * NettyHandler#messageReceived(ChannelHandlerContext, MessageEvent) —>
     * AbstractPeer#received(Channel, Object) —>
     * MultiMessageHandler#received(Channel, Object) —>
     * HeartbeatHandler#received(Channel, Object) —>
     * AllChannelHandler#received(Channel, Object) —>
     * ExecutorService#execute(Runnable) // 由线程池执行后续的调用逻辑
     */

    public class NettyHandler extends SimpleChannelHandler {
        public void messageReceived(ChannelHandlerContext ctx, MessageEvent e) throws Exception {
            NettyChannel channel = NettyChannel.getOrAddChannel(ctx.getChannel(), url, handler);
            try {
                // 这里的 handler 是 NettyServer 类型的对象，最终会一直调用到 AllChannelHandler 类中的 received 方法
                handler.received(channel, e.getMessage());
            } finally {
                NettyChannel.removeChannelIfDisconnected(ctx.getChannel());
            }
        }
    }

    public class AllChannelHandler extends WrappedChannelHandler {

        // 处理请求和响应消息，这里的 message 变量类型可能是 Request，也可能是 Response
        public void received(Channel channel, Object message) throws RemotingException {
            ExecutorService cexecutor = getExecutorService();
            try {
                // 将请求和响应消息封装到 ChannelEventRunnable 对象中，然后派发到线程池中处理
                cexecutor.execute(new ChannelEventRunnable(channel, handler, ChannelState.RECEIVED, message));
            } catch (Throwable t) {
                // TODO A temporary solution to the problem that the exception information can
                // not be sent to the opposite end after the thread pool is full. Need a
                // refactoring
                // fix The thread pool is full, refuses to call, does not return, and causes the
                // consumer to wait for time out
                // 如果通信方式为双向通信，此时将 Server side ... threadpool is exhausted 错误信息封装到 Response
                // 中，并返回给服务消费方。
                if (message instanceof Request && t instanceof RejectedExecutionException) {
                    Request request = (Request) message;
                    if (request.isTwoWay()) {
                        String msg = "Server side(" + url.getIp() + "," + url.getPort()
                                + ") threadpool is exhausted ,detail msg:" + t.getMessage();
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

    }

    /**
     * 服务调用过程的如下：
     * ChannelEventRunnable#run()
     * —> DecodeHandler#received(Channel, Object)
     * —> HeaderExchangeHandler#received(Channel, Object)
     * —> HeaderExchangeHandler#handleRequest(ExchangeChannel, Request)
     * —> DubboProtocol.requestHandler#reply(ExchangeChannel, Object)
     * —> Filter#invoke(Invoker, Invocation)
     * —> AbstractProxyInvoker#invoke(Invocation)
     * —> Wrapper0#invokeMethod(Object, String, Class[], Object[])
     * —> DemoServiceImpl#sayHello(String)
     */

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
                        // 将 channel 和 message 传给 ChannelHandler 对象，进行后续的调用
                        // 这里的 handler 是 DecodeHandler 类型
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

    public class HeaderExchangeHandler implements ChannelHandlerDelegate {

        public void received(Channel channel, Object message) throws RemotingException {
            channel.setAttribute(KEY_READ_TIMESTAMP, System.currentTimeMillis());
            ExchangeChannel exchangeChannel = HeaderExchangeChannel.getOrAddChannel(channel);
            try {
                // 处理请求对象
                if (message instanceof Request) {
                    // handle request.
                    Request request = (Request) message;
                    if (request.isEvent()) {
                        // 处理事件
                        handlerEvent(channel, request);
                        // 处理普通请求
                    } else {
                        // 双向通信
                        if (request.isTwoWay()) {
                            // 真正的调用服务，并且将调用得到的结果封装到 Response 对象中，最后再将该对象返回给服务消费方。
                            // 如果服务调用的过程中发生错误，则将错误信息封装到 Response 对象中，并返回给服务消费方。
                            Response response = handleRequest(exchangeChannel, request);
                            // 将调用结果返回给服务消费端
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
                // 继续向下调用
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

            // 从 exporterMap 查找与 serviceKey 相对应的 DubboExporter 对象，
            // 服务导出过程中会将 <serviceKey, DubboExporter> 映射关系存储到 exporterMap 集合中
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

    /**
     * 服务消费方接收调用结果
     * 
     * 服务消费方在收到响应数据后，首先要做的事情是对响应数据进行解码，得到 Response 对象。然后再将该对象传递给下一个入站处理器，这个入站处理器就是 NettyHandler。接下来 NettyHandler 
     * 会将这个对象继续向下传递，最后 AllChannelHandler 的 received 方法会收到这个对象，并将这个对象派发到线程池中。这个过程和服务提供方接收请求的过程是一样的
     */

    public class HeaderExchangeHandler implements ChannelHandlerDelegate {

        public void received(Channel channel, Object message) throws RemotingException {
            channel.setAttribute(KEY_READ_TIMESTAMP, System.currentTimeMillis());
            ExchangeChannel exchangeChannel = HeaderExchangeChannel.getOrAddChannel(channel);
            try {
                if (message instanceof Request) {
                    // handle request.
                } else if (message instanceof Response) {
                    handleResponse(channel, (Response) message);
                } else if (message instanceof String) {
                    // telnet 相关，直接忽略
                } else {
                    handler.received(exchangeChannel, message);
                }
            } finally {
                HeaderExchangeChannel.removeChannelIfDisconnected(channel);
            }
        }

        static void handleResponse(Channel channel, Response response) throws RemotingException {
            if (response != null && !response.isHeartbeat()) {
                DefaultFuture.received(channel, response);
            }
        }

    }

    public static class DefaultFuture implements ResponseFuture{

        public static void received(Channel channel, Response response) {
            try {
                // 根据调用编号从 FUTURES 集合中查找指定的 DefaultFuture 对象
                DefaultFuture future = FUTURES.remove(response.getId());
                if (future != null) {
                    future.doReceived(response);
                } else {
                    // 省略代码
                }
            } finally {
                CHANNELS.remove(response.getId());
            }
        }

        private void doReceived(Response res) {
            lock.lock();
            try {
                // 保存响应对象
                response = res;
                if (done != null) {
                    // 唤醒用户线程
                    done.signal();
                }
            } finally {
                lock.unlock();
            }
            if (callback != null) {
                invokeCallback(callback);
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


}