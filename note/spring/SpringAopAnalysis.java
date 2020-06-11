
public class SpringAopAnalysis {
    /**
     * 首先，基于接口的代理需要准备的元素： 1.被代理的对象 2.代理对象要实现的接口 3.要对被代理对象实施的增强(额外操作)
     * Demo中的前5步都是处理准备工作
     */
    public static void main(String[] args) {
        // 1.创建代理工厂
        ProxyFactory factory = new ProxyFactory();
        // 2.设置目标对象，也就是被代理的对象
        factory.setTarget(new ChromeBrowser());
        // 3.设置代理实现接口
        factory.setInterfaces(new Class[] { Browser.class });
        // 4.添加前置增强
        factory.addAdvice(new BrowserBeforeAdvice());
        // 5.添加后置增强
        factory.addAdvice(new BrowserAfterReturningAdvice());
        // 6.获取代理对象
        Browser browser = (Browser) factory.getProxy();

        browser.visitInternet();
    }

    /**
     * 
     * ProxyConfig | AdvisedSupport | ProxyCreatorSupport | | ProxyFactoryBean
     * ProxyFactory
     * 
     * 1.ProxyConfig：代理相关的全局配置，常见的有proxyTargetClass，exposeProxy。
     * 2.AdvisedSupport：在Spring
     * AOP中，Advisor(切面)就是将Advice(增强)和Pointcut(切入点)连接起来的东西。此类主要支持切面相关的操作。
     * 3.ProxyCreatorSupport：代理创建的辅助类，主要方法就是创建代理对象。
     * 
     * 注意，MethodInterceptor接口继承了Advice接口
     */

    // class:AdvisedSupport
    // setTarget时，将target对象封装成TargetSource对象
    public void setTarget(Object target) {
        setTargetSource(new SingletonTargetSource(target));
    }

    // class:AdvisedSupport
    @Override
    public void setTargetSource(TargetSource targetSource) {
        this.targetSource = (targetSource != null ? targetSource : EMPTY_TARGET_SOURCE);
    }

    /**
     * 创建的代理对象将要实现的一些接口。这些接口是按照顺序保存在数组中 Interfaces to be implemented by the proxy.
     * Held in List to keep the order of registration, to create JDK proxy with
     * specified order of interfaces.
     */
    private List<Class<?>> interfaces = new ArrayList<Class<?>>();

    /**
     * List of Advisors. If an Advice is added, it will be wrapped in an Advisor
     * before being added to this List.
     */
    private List<Advisor> advisors = new LinkedList<Advisor>();

    // class:AdvisedSupport
    public void setInterfaces(Class<?>... interfaces) {
        Assert.notNull(interfaces, "Interfaces must not be null");
        // 先清空再添加进去
        this.interfaces.clear();
        for (Class<?> ifc : interfaces) {
            addInterface(ifc);
        }
    }

    // class:AdvisedSupport
    public void addInterface(Class<?> intf) {
        Assert.notNull(intf, "Interface must not be null");
        if (!intf.isInterface()) {
            throw new IllegalArgumentException("[" + intf.getName() + "] is not an interface");
        }
        if (!this.interfaces.contains(intf)) {
            this.interfaces.add(intf);
            adviceChanged();
        }
    }

    // class:AdvisedSupport
    public void addAdvice(Advice advice) throws AopConfigException {
        int pos = this.advisors.size();
        addAdvice(pos, advice);
    }

    class DefaultPointcutAdvisor {
        // Pointcut.TRUE表示支持任何切入点
        public DefaultPointcutAdvisor(Advice advice) {
            this(Pointcut.TRUE, advice);
        }
    }

    // class:AdvisedSupport
    @Override
    public void addAdvice(int pos, Advice advice) throws AopConfigException {
        Assert.notNull(advice, "Advice must not be null");
        // 代码省略.......
        // 将Advice封装成Advisor然后添加到advisors集合中
        addAdvisor(pos, new DefaultPointcutAdvisor(advice));
    }

    // class:AdvisedSupport
    public void addAdvisor(int pos, Advisor advisor) throws AopConfigException {
        // 代码省略
        addAdvisorInternal(pos, advisor);
    }

    // class:AdvisedSupport
    private void addAdvisorInternal(int pos, Advisor advisor) throws AopConfigException {
        Assert.notNull(advisor, "Advisor must not be null");
        if (isFrozen()) {
            throw new AopConfigException("Cannot add advisor: Configuration is frozen.");
        }
        if (pos > this.advisors.size()) {
            throw new IllegalArgumentException(
                    "Illegal position " + pos + " in advisor list with size " + this.advisors.size());
        }
        this.advisors.add(pos, advisor);
        updateAdvisorArray();
        adviceChanged();
    }

    // class:ProxyFactory
    public Object getProxy() {
        // createAopProxy根据Factory的设置生成一个AopProxy，返回的AopProxy有两种实现：
        // 一种是JDK动态代理类型的JdkDynamicAopProxy，另外一种是CGLib类型的ObjenesisCglibAopProxy。
        // 对于不同的代理方式，getProxy调用的是各自内部的实现。
        return createAopProxy().getProxy();
    }

    // class:ProxyCreatorSupport
    protected final synchronized AopProxy createAopProxy() {
        if (!this.active) {
            activate();
        }
        // 返回的aopProxyFactory对象类型为DefaultAopProxyFactory
        // ProxyCreatorSupport继承了AdvisedSupport，而AdvisedSupport继承了ProxyConfig，因而可以当做参数传进去，
        // 最终调用工厂类AopProxyFactory依据配置信息(this)创建一个AopProxy，可能是JDK类型的或者是CGLIB类型的
        return getAopProxyFactory().createAopProxy(this);
    }

    // class:ProxyCreatorSupport
    public AopProxyFactory getAopProxyFactory() {
        return this.aopProxyFactory;
    }

    public class DefaultAopProxyFactory implements AopProxyFactory, Serializable {
        // 基于外部的配置，比如设置optimize或proxyTargetClass为true，或者目标对象没有实现接口，则会返回CGLIB代理
        // 否则返回JDK代理
        @Override
        public AopProxy createAopProxy(AdvisedSupport config) throws AopConfigException {
            if (config.isOptimize() || config.isProxyTargetClass() || hasNoUserSuppliedProxyInterfaces(config)) {
                Class<?> targetClass = config.getTargetClass();
                if (targetClass == null) {
                    throw new AopConfigException("TargetSource cannot determine target class: "
                            + "Either an interface or a target is required for proxy creation.");
                }
                if (targetClass.isInterface()) {
                    return new JdkDynamicAopProxy(config);
                }
                return new ObjenesisCglibAopProxy(config);
            } else {
                return new JdkDynamicAopProxy(config);
            }
        }
    }

    final class JdkDynamicAopProxy implements AopProxy, InvocationHandler, Serializable {

        public Object getProxy(ClassLoader classLoader) {
            // 处理代理接口
            Class<?>[] proxiedInterfaces = AopProxyUtils.completeProxiedInterfaces(this.advised);
            // 判断接口定义是否有equals和hashCode方法
            findDefinedEqualsAndHashCodeMethods(proxiedInterfaces);
            // 调用JDK创建代理方法，由于JdkDynamicAopProxy类对象本身就实现了InvocationHandler接口，
            // 因此把自己作为handler，用于创建代理对象。当用户使用此代理对象调用方法时，就会调用此类中的invoke方法来真正执行
            return Proxy.newProxyInstance(classLoader, proxiedInterfaces, this);
        }

        //invoke处理真正的代理请求
        //class:JdkDynamicAopProxy
        public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
            MethodInvocation invocation;
            Object oldProxy = null;
            boolean setProxyContext = false;

            TargetSource targetSource = this.advised.targetSource;
            Class<?> targetClass = null;
            Object target = null;

            try {
                // 代码省略.......
                Object retVal;

                //如果 expose-proxy 属性为 true，则暴露代理对象
                if (this.advised.exposeProxy) {
                    //向 AopContext 中设置代理对象
                    oldProxy = AopContext.setCurrentProxy(proxy);
                    setProxyContext = true;
                }

                target = targetSource.getTarget();
                if (target != null) {
                    targetClass = target.getClass();
                }

                // 获取与当前方法相匹配的拦截器链
                List<Object> chain = this.advised.getInterceptorsAndDynamicInterceptionAdvice(method, targetClass);

                // 如果拦截器链为空，则直接反射调用用户的方法
                if (chain.isEmpty()) {
                    // 通过反射执行目标方法
                    retVal = AopUtils.invokeJoinpointUsingReflection(target, method, args);
                } else {
                    // 链式调用，将所有元素封装成ReflectiveMethodInvocation，通过方法proceed进行链式调用
                    invocation = new ReflectiveMethodInvocation(proxy, target, method, args, targetClass, chain);
                    retVal = invocation.proceed();
                }

                // 代码省略......
                return retVal;
            } finally {
                // 省略代码......
            }
        }
    }

    // class:AdvisedSupport
    // 拦截器链的获取是一个通用方法，都是调用AdvisedSupport类，并设置了缓存以重用
    public List<Object> getInterceptorsAndDynamicInterceptionAdvice(Method method, Class<?> targetClass) {
        MethodCacheKey cacheKey = new MethodCacheKey(method);
        List<Object> cached = this.methodCache.get(cacheKey);
        if (cached == null) {
            /**
             * 真正的调用在DefaultAdvisorChainFactory中，它实现了AdvisorChainFactory接口。通过遍历所有的Advisor切面，如果是PointcutAdvisor，
             * 则提取出Pointcut，然后查看Pointcut与当前类和方法（也就是切入点）是否匹配。如果匹配的话通过AdvisorAdapterRegistry切面注册适配器将Advisor中的
             * Advice都转换为MethodInteceptor对象，然后添加到List中从而形成拦截器链。
             */
            cached = this.advisorChainFactory.getInterceptorsAndDynamicInterceptionAdvice(this, method, targetClass);
            this.methodCache.put(cacheKey, cached);
        }
        return cached;
    }

    // Class:DefaultAdvisorChainFactory
    @Override
    public List<Object> getInterceptorsAndDynamicInterceptionAdvice(Advised config, Method method,
            Class<?> targetClass) {

        List<Object> interceptorList = new ArrayList<Object>(config.getAdvisors().length);
        Class<?> actualClass = (targetClass != null ? targetClass : method.getDeclaringClass());
        boolean hasIntroductions = hasMatchingIntroductions(config, actualClass);
        // 切面适配注册器，将Advisor中Advice类型的对象转换为MethodInterceptor类型的对象，注意MethodInterceptor接口
        // 继承了Advice接口。这里getInstance返回的是DefaultAdvisorAdapterRegistry类型的对象
        AdvisorAdapterRegistry registry = GlobalAdvisorAdapterRegistry.getInstance();

        for (Advisor advisor : config.getAdvisors()) {
            // 切入点切面
            if (advisor instanceof PointcutAdvisor) {
                PointcutAdvisor pointcutAdvisor = (PointcutAdvisor) advisor;
                // Advisor中的切入点Pointcut与当前类和方法是否匹配
                if (config.isPreFiltered() || pointcutAdvisor.getPointcut().getClassFilter().matches(actualClass)) {
                    // 获取所有拦截器（即Advisor中的Advice，或者说增强器），一个被代理的对象可以配置多个拦截器，
                    // 所以可能会有多个拦截器与当前切入点相匹配，比如前面的BrowserBeforeAdvice和BrowserAfterReturningAdvice拦截器
                    MethodInterceptor[] interceptors = registry.getInterceptors(advisor);
                    MethodMatcher mm = pointcutAdvisor.getPointcut().getMethodMatcher();
                    // 当前方法是否使用切入点配置
                    if (MethodMatchers.matches(mm, method, actualClass, hasIntroductions)) {
                        if (mm.isRuntime()) {
                            for (MethodInterceptor interceptor : interceptors) {
                                interceptorList.add(new InterceptorAndDynamicMethodMatcher(interceptor, mm));
                            }
                        } else {
                            // 添加普通的拦截器
                            interceptorList.addAll(Arrays.asList(interceptors));
                        }
                    }
                }
            } else if (advisor instanceof IntroductionAdvisor) {
                // 省略代码
            } else {
                Interceptor[] interceptors = registry.getInterceptors(advisor);
                interceptorList.addAll(Arrays.asList(interceptors));
            }
        }

        return interceptorList;
    }

    public class DefaultAdvisorAdapterRegistry implements AdvisorAdapterRegistry, Serializable {

        private final List<AdvisorAdapter> adapters = new ArrayList<AdvisorAdapter>(3);

        public DefaultAdvisorAdapterRegistry() {
            registerAdvisorAdapter(new MethodBeforeAdviceAdapter());
            registerAdvisorAdapter(new AfterReturningAdviceAdapter());
            registerAdvisorAdapter(new ThrowsAdviceAdapter());
        }

        // 省略代码

        @Override
        public MethodInterceptor[] getInterceptors(Advisor advisor) throws UnknownAdviceTypeException {
            List<MethodInterceptor> interceptors = new ArrayList<MethodInterceptor>(3);
            Advice advice = advisor.getAdvice();
            /**
             * MethodInterceptor和其它接口的继承链如下： 
             * MethodInterceptor -> Interceptor -> Advice
             * MethodBeforeAdvice -> BeforeAdvice -> Advice 
             * AfterReturningAdvice -> AfterAdvice -> Advice 
             * ThrowsAdvice -> AfterAdvice -> Advice
             */
            // 如果advice对象就是实现了MethodInterceptor接口的对象，那么直接添加到拦截器列表中
            if (advice instanceof MethodInterceptor) {
                interceptors.add((MethodInterceptor) advice);
            }
            for (AdvisorAdapter adapter : this.adapters) {
                // 使用adapter对象来判断advice是实现了MethodBeforeAdvice、AfterReturningAdvice、
                // ThrowsAdvice这三个接口之一，如果是的话，就将其包装成一个MethodInterceptor，加入到列表中
                if (adapter.supportsAdvice(advice)) {
                    interceptors.add(adapter.getInterceptor(advisor));
                }
            }
            if (interceptors.isEmpty()) {
                throw new UnknownAdviceTypeException(advisor.getAdvice());
            }
            return interceptors.toArray(new MethodInterceptor[interceptors.size()]);
        }

        @Override
        public void registerAdvisorAdapter(AdvisorAdapter adapter) {
            this.adapters.add(adapter);
        }

    }

    class MethodBeforeAdviceAdapter implements AdvisorAdapter, Serializable {

        @Override
        public boolean supportsAdvice(Advice advice) {
            return (advice instanceof MethodBeforeAdvice);
        }

        @Override
        public MethodInterceptor getInterceptor(Advisor advisor) {
            MethodBeforeAdvice advice = (MethodBeforeAdvice) advisor.getAdvice();
            return new MethodBeforeAdviceInterceptor(advice);
        }

    }

    class ReflectiveMethodInvocation implements ProxyMethodInvocation, Cloneable {
        // 在JdkDynamicAopProxy的invoke方法中通过
        protected ReflectiveMethodInvocation(Object proxy, Object target, Method method, Object[] arguments,
                Class<?> targetClass, List<Object> interceptorsAndDynamicMethodMatchers) {

            this.proxy = proxy;
            this.target = target;
            this.targetClass = targetClass;
            this.method = BridgeMethodResolver.findBridgedMethod(method);
            this.arguments = arguments;
            this.interceptorsAndDynamicMethodMatchers = interceptorsAndDynamicMethodMatchers;
        }

        /**
         * 此方法是一个递归调用的方法，在各个拦截器MethodInterceptor中，均会调用这个proceed方法，从而完成整个拦截链以及真正要执行的方法的执行
         */
        public Object proceed() throws Throwable {
            /**
             * 这里首先判断当前调用是否已经将所有的调用链完成，如果已经完成，则调用invokeJoinpoint，触发真实要执行的方法。
             * 大家可能比较疑惑，这里为什么是interceptorsAndDynamicMethodMatchers.size() -
             * 1，因为这里的currentInterceptorIndex是从-1开始的， 如果从0开始的话，那么，显然就没有后面的 - 1
             */
            if (this.currentInterceptorIndex == this.interceptorsAndDynamicMethodMatchers.size() - 1) {
                return invokeJoinpoint();
            }
            /**
             * 从interceptorsAndDynamicMethodMatchers列表中获取值，然后调用其invoke方法。
             * 这里我把MethodBeforeAdviceInterceptor、AfterReturningAdviceInterceptor、ThrowsAdviceInterceptor都讲解一下。
             * 1.如果这里的MethodInterceptor实际类型是MethodBeforeAdviceInterceptor，那么调用了MethodBeforeAdviceInterceptor.invoke，
             * 注意，这里的入参是ReflectiveMethodInvocation，也就是说，这里在调用了advice#before后，接着调用了ReflectiveMethodInvocation#proceed。
             * 参考下面的MethodBeforeAdviceInterceptor。
             * 2.如果这里的MethodInterceptor实际类型是AfterReturningAdviceInterceptor，这里直接就调用了ReflectiveMethodInvocation#proceed，
             * 在其调用完成后，调用了advice#afterReturning。参考下面的AfterReturningAdviceInterceptor类。
             * 3.如果这里的MethodInterceptor实际类型是ThrowsAdviceInterceptor，这里直接调用了ReflectiveMethodInvocation#proceed，不过，
             * 这里添加了异常捕获如果获取到对应的Method，则通过invokeHandlerMethod调用捕获异常的方法。然后继续将异常抛出
             */
            Object interceptorOrInterceptionAdvice = this.interceptorsAndDynamicMethodMatchers
                    .get(++this.currentInterceptorIndex);
            if (interceptorOrInterceptionAdvice instanceof InterceptorAndDynamicMethodMatcher) {
                // 省略代码.......
            } else {
                // It's an interceptor, so we just invoke it
                return ((MethodInterceptor) interceptorOrInterceptionAdvice).invoke(this);
            }
        }
    }

    // class:AopUtils
    public static Object invokeJoinpointUsingReflection(Object target, Method method, Object[] args) throws Throwable {

        try {
            ReflectionUtils.makeAccessible(method);
            // 直接利用反射来调用方法
            return method.invoke(target, args);
        } catch (InvocationTargetException ex) {
            throw ex.getTargetException();
        }
        // 代码省略.......
    }

    public class MethodBeforeAdviceInterceptor implements MethodInterceptor, Serializable {
        //前置通知
        private MethodBeforeAdvice advice;

        
        //对MethodBeforeAdvice实现进行包装成MethodInterceptor
        public MethodBeforeAdviceInterceptor(MethodBeforeAdvice advice) {
            Assert.notNull(advice, "Advice must not be null");
            this.advice = advice;
        }

        @Override
        public Object invoke(MethodInvocation mi) throws Throwable {
            //执行前置通知
            this.advice.before(mi.getMethod(), mi.getArguments(), mi.getThis());
            //通过 MethodInvocation 调用下一个拦截器，若所有拦截器均执行完，则调用目标方法
            return mi.proceed();
        }
    }

}