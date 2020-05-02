// 分析此项目中异步调用的逻辑
public class AsyncRpcAnalysis {
    static class AsyncRpcCallTest {
        /**
         * 介绍一下异步调用的大概逻辑：首先，通过ctx.getBean获得到的bean是通过JDK代理创建的一个动态代理对象，
         * invoker.submit把实现了AsyncCallback接口的对象再用Callable封装，传进FutureTask对象中，然后由线程池来具体的执行。
         * 在AsyncCallback接口的call方法中，可以实现用户自定义的业务逻辑。这里是调用calculate的calculate()方法，这个调用是由
         * 线程池中的线程来执行，而不是main线程。由于calculate是一个代理对象，因此我们调用calculate方法实际上会调用到
         * MessageSendProxy中的handleInvocation方法，通过MessageSendHandler把调用请求发送给RPC服务器，并且阻塞等待默认30s服务器返回的结果，
         * 最后这个结果被保存在AsyncFuture的属性中（即其父类FutureTask中的outcome属性）。
         * 
         * 另外，在main线程中，invoker.submit返回的也是一个CGLIB代理对象，即elapse。当调用elapse0.setDetail（第一次调用）时，就会被AsyncLoadResultInterceptor
         * 拦截器拦截，最终调用到AsyncCallResult中的loadFuture方法，通过future.get方法获取前面返回的结果对象，这里是CostTime对象（不是代理，就是一个原生
         * 的CostTime对象）。然后就直接调用这个CostTime对象的setDetail方法，后面再调用方法（比如toString），就直接使用前面返回的这个CostTime对象。
         * 这个过程是在main线程中进行的。
         */
        public static void main(String[] args) {
            ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext(
                    "classpath:rpc-invoke-config-client.xml");
            final CostTimeCalculate calculate = (CostTimeCalculate) context.getBean("costTime");
            long start = 0, end = 0;
            start = System.currentTimeMillis();
            AsyncInvoker invoker = new AsyncInvoker();

            // AsyncCallback中的泛型就是这个接口中的call方法返回的结果
            CostTime elapse0 = invoker.submit(new AsyncCallback<CostTime>() {
                @Override
                public CostTime call() {
                    return calculate.calculate();
                }
            });

            CostTime elapse1 = invoker.submit(new AsyncCallback<CostTime>() {
                @Override
                public CostTime call() {
                    return calculate.calculate();
                }
            });

            CostTime elapse2 = invoker.submit(new AsyncCallback<CostTime>() {
                @Override
                public CostTime call() {
                    return calculate.calculate();
                }
            });

            System.out.println("1 async nettyrpc call:[" + "result:" + elapse0 + ", status:["
                    + ((AsyncCallObject) elapse0)._getStatus() + "]");
            System.out.println("2 async nettyrpc call:[" + "result:" + elapse1 + ", status:["
                    + ((AsyncCallObject) elapse1)._getStatus() + "]");
            System.out.println("3 async nettyrpc call:[" + "result:" + elapse2 + ", status:["
                    + ((AsyncCallObject) elapse2)._getStatus() + "]");

            elapse0.setDetail("detail changed");
            System.out.println(elapse0);

            end = System.currentTimeMillis();
            System.out.println("nettyrpc async calculate time:" + (end - start));

            context.destroy();
        }
    }

    /**
     * 这个自定义类实现了InitializingBean和FactoryBean两个接口。在Spring IoC容器中实例化了Bean之后，就会对Bean进行初始化操作，
     * 比如当一个bean实现了InitializingBean接口之后，就会回调afterProperties方法。在NettyRpcReference#afterProperties方法会
     * 和RPC服务器建立一个长连接，注意，在client.xml中使用了多少次<nettyrpc:reference>标签，就会调用多少次afterProperties方法，也就会
     * 与RPC服务器建立多少个长连接，并且把这个MessageSendHandler保存到RpcServerLoader的属性中。
     * 
     * 实现FactoryBean接口的作用举例说明，比如此NettyRpcReference中保存的interfaceName为AddCalculate，那么当调用ctx.getBean()返回的时候，
     * 就会调用getObject方法，而不是返回NettyRpcReference这个Bean。在getObject方法中，返回的是实现了interfaceName的代理对象，当代理对象调用
     * 方法时，真正调用的是实现了InvocationHandler的MessageSendProxy对象的invoke方法
     */
    @Data
    public class NettyRpcReference implements FactoryBean, InitializingBean, DisposableBean {
        private String interfaceName;
        private String ipAddr;
        private String protocol;
        private EventBus eventBus = new EventBus();

        @Override
        public void destroy() throws Exception {
            eventBus.post(new ClientStopEvent(0));
        }

        @Override
        public void afterPropertiesSet() throws Exception {
            // 每一次客户端向服务器端建立一个连接时，都会发起一个长连接，比如调用RPC服务器端的addCalculate和multiCalculate方法，
            // 就会向RPC服务器发起两次连接。不过这样会有一个问题，假设在XML文件中，指定add方法向A服务器发起请求，multi方法向B服务器发起请求，
            // 接着调用下面这行代码setRpcServerLoader，最终会调用到MessageSendInitializeTask中的call方法，分别向服务器A、B建立长连接。
            // 但是由于RpcServerLoader是单例的，因此其中所包含的MessageSendHandler是唯一的。因此，最后设置的MessageSendHandler就是与A、B两台服务器
            // 连接中的一个handler。因此，发送的add方法请求和call方法请求都会通过这个handler发往同一台服务器。不过这个项目设计的只支持
            // 客户端与一个服务器端的通信，因此RpcServerLoader使用单例模式也不会发生错误（因为这样就只能通过一个连接，或者说一个MessageSendHandler
            // 与服务器端进行通信）。
            MessageSendExecutor.getInstance().setRpcServerLoader(ipAddr, RpcSerializeProtocol.valueOf(protocol));
            ClientStopEventListener listener = new ClientStopEventListener();
            eventBus.register(listener);
        }

        @Override
        public Object getObject() throws Exception {
            // 返回一个实现了interfaceName的代理对象，调用代理对象的方法时，真正调用的是实现了InvocationHandler的
            // MessageSendProxy对象的invoke方法
            return MessageSendExecutor.getInstance().execute(getObjectType());
        }

        @Override
        public Class<?> getObjectType() {
            try {
                return this.getClass().getClassLoader().loadClass(interfaceName);
            } catch (ClassNotFoundException e) {
                System.err.println("spring analyze fail!");
            }
            return null;
        }

        @Override
        public boolean isSingleton() {
            return true;
        }
    }

    public class AsyncInvoker {
        private ThreadPoolExecutor executor = (ThreadPoolExecutor) RpcThreadPool.getExecutor(
                RpcSystemConfig.SYSTEM_PROPERTY_THREADPOOL_THREAD_NUMS, RpcSystemConfig.SYSTEM_PROPERTY_THREADPOOL_QUEUE_NUMS);
    
        public <T> T submit(final AsyncCallback<T> callback) {
            // getGenericInterfaces返回对象实现的接口信息的Type数组，包含泛型信息，举例来说，
            // 调用AsyncRpcCallTest方法传进来的callback对象是AsyncCallback接口的一个实现类，即 AsyncRpcCall<CostTime>。
            // 所以调用getGenericInterfaces()[0]所得到的就是AsyncCallback<CostTime>对象（包含泛型信息）。由于使用了泛型，
            // 所以它是ParameterizedType类型。
            Type type = callback.getClass().getGenericInterfaces()[0];
            if (type instanceof ParameterizedType) {
                // getGenericClass方法返回值returnClass为泛型中实际参数类型，比如AsyncCallback<CostTime>中的CostTime.class
                Class returnClass = (Class) ReflectionUtils.getGenericClass((ParameterizedType) type, 0);
                return intercept(callback, returnClass);
            } else {
                throw new AsyncCallException("NettyRPC AsyncCallback must be parameterized type!");
            }
        }
    
        // submit方法把传进来的Callable对象，封装进AsyncFuture对象（实现了FutureTask接口），并且提交到线程池中去执行。
        // 这个future是此项目中异步调用的关键
        private <T> AsyncFuture<T> submit(Callable<T> task) {
            AsyncFuture future = new AsyncFuture<T>(task);
            executor.submit(future);
            return future;
        }
    
        private <R> R intercept(final AsyncCallback<R> callback, Class<?> returnClass) {
            if (!Modifier.isPublic(returnClass.getModifiers())) {
                return callback.call();
            } else if (Modifier.isFinal(returnClass.getModifiers())) {
                return callback.call();
            } else if (Void.TYPE.isAssignableFrom(returnClass)) {
                return callback.call();
            } else if (returnClass.isPrimitive() || returnClass.isArray()) {
                return callback.call();
            } else if (returnClass == Object.class) {
                return callback.call();
            } else {
                return submit(callback, returnClass);
            }
        }
    
        private <T> T submit(final AsyncCallback<T> callback, Class<?> returnClass) {
            Future future = submit(new Callable<T>() {
                @Override
                public T call() throws Exception {
                    return callback.call();
                }
            });
    
            // SYSTEM_PROPERTY_ASYNC_MESSAGE_CALLBACK_TIMEOUT 的值默认为60s，这里的returnClass是CostTime.class，
            // future就是AsyncFuture，也就是一个FutureTask类型，根据这个future可以得知线程池运行任务的结果。
            // 这是因为，在FutureTask在线程池中被执行结束之后，上面实现Callable接口的匿名内部类中的call方法返回的结果，
            // 会保存在AsyncFuture对象中（其实是保存在FutureTask对象中的outcome属性中），可以通过FutureTask的get方法获取到。
            AsyncCallResult result = new AsyncCallResult(returnClass, future,
                    RpcSystemConfig.SYSTEM_PROPERTY_ASYNC_MESSAGE_CALLBACK_TIMEOUT);
    
            T asyncProxy = (T) result.getResult();
            return asyncProxy;
        }
    }


    public class AsyncCallResult {
        private Class returnClass;
        private Future future;
        private Long timeout;
    
        public AsyncCallResult(Class returnClass, Future future, Long timeout) {
            this.returnClass = returnClass;
            this.future = future;
            this.timeout = timeout;
        }
    
        public Object loadFuture() throws AsyncCallException {
            try {
                // 当getResult方法返回的代理对象第一次调用其它方法时，就会调用这个loadFuture方法，从
                // future属性中得到返回的结果。如果timeout<=0，则一直阻塞等待；否则的话，就等待指定时间，直到超时。
                if (timeout <= 0L) {
                    return future.get();
                } else {
                    return future.get(timeout, TimeUnit.MILLISECONDS);
                }
            } catch (TimeoutException e) {
                future.cancel(true);
                throw new AsyncCallException(e);
            } catch (InterruptedException e) {
                throw new AsyncCallException(e);
            } catch (Exception e) {
                translateTimeoutException(e);
                throw new AsyncCallException(e);
            }
        }
    
        private void translateTimeoutException(Throwable t) {
            int index = t.getMessage().indexOf(RpcSystemConfig.TIMEOUT_RESPONSE_MSG);
            if (index != -1) {
                throw new InvokeTimeoutException(t);
            }
        }
    
        public Object getResult() {
            // 这里的returnClass是CostTime.class类型的，首先从缓存中查找，看看是否有先前进行了保存。
            Class proxyClass = AsyncProxyCache.get(returnClass.getName());
    
            // 这里的returnClass不是接口类型的。在这里我们使用CGLIB来创建一个动态代理对象，这个对象要实现AsyncCallObject接口，
            // 同时也要继承CostTime类。最后，会设置好两个拦截器AsyncCallResultInterceptor和AsyncCallObjectInterceptor，
            // 并且设置一个过滤器AsyncCallFilter来决定当这个代理对象调用一个方法时，会使用哪个拦截器来进行拦截。
            // 具体的逻辑是这样：当调用AsyncCallObject接口中的方法_getStatus时，就会被AsyncLoadStatusInterceptor拦截，
            // 返回一个AsyncCallStatus对象。如果调用的是其他的方法，则被AsyncCallResultInterceptor拦截，这拦截器实现了LazyLoader接口，
            // 当第一次调用设置了这个拦截器的动态代理对象的时候，CGLIB会调用这个loadObject()方法获取方法调用实际需要被分派的对象，
            // 之后所有拦截到的方法调用都会分派给这个返回的对象。在这里获取到的对象就是前面在线程池中运行，最后保存在FutureTask对象中的结果，
            // 在这个例子中是CostTime。
            if (proxyClass == null) {
                Enhancer enhancer = new Enhancer();
                if (returnClass.isInterface()) {
                    enhancer.setInterfaces(new Class[]{AsyncCallObject.class, returnClass});
                } else {
                    enhancer.setInterfaces(new Class[]{AsyncCallObject.class});
                    enhancer.setSuperclass(returnClass);
                }
                enhancer.setCallbackFilter(new AsyncCallFilter());
                enhancer.setCallbackTypes(new Class[]{
                        AsyncLoadResultInterceptor.class, AsyncLoadStatusInterceptor.class
                });
                proxyClass = enhancer.createClass();
                AsyncProxyCache.save(returnClass.getName(), proxyClass);
            }
    
            // Enhancer.registerCallbacks不放到if方法里面是因为，传进去的两个Interceptor需要一个新的future对象
            // 如果把这段代码放到if中，就可能会导致每次创建的代理对象的拦截方法中，future始终是同一个对象
            Enhancer.registerCallbacks(proxyClass, new Callback[]{
                    new AsyncCallResultInterceptor(this),
                    new AsyncCallObjectInterceptor(future)
                });
    
            try {
                // 根据proxyClass每次创建一个不同的代理对象返回
                return ReflectionUtils.newInstance(proxyClass);
            } finally {
                Enhancer.registerStaticCallbacks(proxyClass, null);
            }
        }
    }

    public class AsyncCallFilter implements CallbackFilter {
        @Override
        public int accept(Method method) {
            /**
             *  getDeclaringClass返回的是此method是在哪个类中进行声明的，不过如果子类重写了父类的方法，
             *  或者一个类实现了一个接口中的方法，那么getDeclaringClass返回的是子类或者实现接口的类的名称。
             *
             *  在CGLIB动态代理中，调用enhancer.createClass()方法，会把代理类要实现的接口（通过setInterfaces设置），
             *  以及要代理的目标类（通过setSuperClass实现）包括其父类（也包括Object类）中的所有方法作为参数都传入
             *  此accept方法中。判断当代理类调用此方法时，应该使用的拦截器在数组中的下标，然后保存起来。
             *  下标为0：AsyncCallResultInterceptor
             *  下标为1：AsyncCallObjectInterceptor
             */
            return AsyncCallObject.class.isAssignableFrom(method.getDeclaringClass()) ? 1 : 0;
        }
    }

    public class AsyncLoadResultInterceptor implements LazyLoader {
        private AsyncCallResult result;
        public AsyncLoadResultInterceptor(AsyncCallResult result) {
            this.result = result;
        }
        @Override
        public Object loadObject() throws Exception {
            return result.loadFuture();
        }
    }

    public class AsyncLoadStatusInterceptor implements MethodInterceptor {
        private static final String NETTYRPCSTATUS = "_getStatus";
        private Future future;
    
        public AsyncLoadStatusInterceptor(Future future) {
            this.future = future;
        }
    
        @Override
        public Object intercept(Object obj, Method method, Object[] args, MethodProxy proxy) {
            if (NETTYRPCSTATUS.equals(method.getName())) {
                return getStatus();
            }
            return null;
        }
    
        private Object getStatus() {
            long startTime = 0L;
            long endTime = 0L;
            if (future instanceof AsyncFuture) {
                startTime = ((AsyncFuture) future).getStartTime();
                endTime = ((AsyncFuture) future).getEndTime();
            }
            CallStatus status = null;
            if (future.isCancelled()) {
                status = CallStatus.TIMEOUT;
            } else if (future.isDone()) {
                status = CallStatus.DONE;
            } else {
                status = CallStatus.RUN;
                if (endTime == 0) {
                    endTime = System.currentTimeMillis();
                }
            }
            return new AsyncCallStatus(startTime, (endTime - startTime), status);
        }
    }

    public class AsyncFuture<V> extends FutureTask<V> {
        private Thread callerThread;
        private Thread runnerThread;
        private long startTime = 0L;
        private long endTime = 0L;
    
        public AsyncFuture(Callable<V> callable) {
            super(callable);
            callerThread = Thread.currentThread();
        }
    
        /**
         * 当FutureTask中的run方法运行完时，就会调用done方法，设置好endTime时间。
         */
        @Override
        protected void done() {
            endTime = System.currentTimeMillis();
        }
    
        @Override
        public void run() {
            startTime = System.currentTimeMillis();
            runnerThread = Thread.currentThread();
            super.run();
        }
        // getter and setter方法......
    }


}