public class DubboServiceInvokingProcess{

    /**
     * Dubbo服务调用的具体过程如下：
     * 首先服务消费者通过代理对象 Proxy 发起远程调用，接着通过网络客户端 Client 将编码后的请求发送给服务提供方的网络层上，也就是 Server。Server 在收到请求后，
     * 首先要做的事情是对数据包进行解码。然后将解码后的请求发送至分发器 Dispatcher，再由分发器将请求派发到指定的线程池上，最后由线程池调用具体的服务。
     * 这就是一个远程调用请求的发送与接收过程。
     * 
     * Dubbo 支持同步和异步两种调用方式，其中异步调用还可细分为"有返回值"的异步调用和"无返回值"的异步调用。所谓"无返回值"异步调用是指服务消费方只管调用，但不关心调用结果，
     * 此时 Dubbo 会直接返回一个空的 RpcResult。若要使用异步特性，需要服务消费方手动进行配置。默认情况下，Dubbo 使用同步调用方式。
     */

    public class JavassistProxyFactory extends AbstractProxyFactory {

        @SuppressWarnings("unchecked")
        public <T> T getProxy(Invoker<T> invoker, Class<?>[] interfaces) {
            // 生成 Proxy 子类（Proxy 是抽象类）。并调用 Proxy 子类的 newInstance 方法创建 Proxy 实例
            return (T) Proxy.getProxy(interfaces).newInstance(new InvokerInvocationHandler(invoker));
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
            Object[] arrobject = new Object[]{object};
            Object object2 = this.handler.invoke(this, methods[1], arrobject);
            return object2;
        }
    }

    public class InvokerInvocationHandler implements InvocationHandler {

        private final Invoker<?> invoker;
    
        // InvokerInvocationHandler 中的 invoker 成员变量类型为 MockClusterInvoker，MockClusterInvoker 内部封装了服务降级逻辑。
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

    public class MockClusterInvoker<T> implements Invoker<T> {

        private final Invoker<T> invoker;

        // 本地伪装 Mock 通常用于服务降级，比如某验权服务，当服务提供方全部挂掉后，客户端不抛出异常，而是通过 Mock 数据返回授权失败。
        public Result invoke(Invocation invocation) throws RpcException {
            Result result = null;
    
            // 获取 mock 配置值
            String value = directory.getUrl().getMethodParameter(invocation.getMethodName(), Constants.MOCK_KEY, Boolean.FALSE.toString()).trim();
            if (value.length() == 0 || value.equalsIgnoreCase("false")) {
                // 无 mock 逻辑，直接调用其他 Invoker 对象的 invoke 方法，比如 FailoverClusterInvoker
                result = this.invoker.invoke(invocation);
            } else if (value.startsWith("force")) {
                // 在 dubbo2.6.6 版本中，可以开始在 Spring XML 中使用 fail 和 force。force 代表强制是使用 Mock 的行为，在这种情况下不会使用走远程调用，
                if (logger.isWarnEnabled()) {
                    logger.info("force-mock: " + invocation.getMethodName() + " force-mock enabled , url : " + directory.getUrl());
                }
                //force:direct mock
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
                            logger.info("fail-mock: " + invocation.getMethodName() + " fail-mock enabled , url : " + directory.getUrl(), e);
                        }
                        result = doMockInvoke(invocation, e);
                    }
                }
            }
            return result;
        }

    }

    public abstract class AbstractInvoker<T> implements Invoker<T> {

        private final Map<String, String> attachment;

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
                // 如果开启了异步调用的话（默认不开启），设置异步信息到 RpcInvocation 的 attachments 变量中
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





}