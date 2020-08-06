public class DubboMock {

    public class MockClusterInvoker<T> implements Invoker<T> {

        private final Directory<T> directory;

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

        private Result doMockInvoke(Invocation invocation, RpcException e) {
            Result result = null;
            Invoker<T> minvoker;

            // 查找mock invoker，我们这种情况下是没有的
            List<Invoker<T>> mockInvokers = selectMockInvoker(invocation);
            if (mockInvokers == null || mockInvokers.size() == 0) {
                // 创建一个 MockInvoker
                minvoker = (Invoker<T>) new MockInvoker(directory.getUrl());
            } else {
                minvoker = mockInvokers.get(0);
            }
            try {
                // 执行 MockInvoker 的 invoke
                result = minvoker.invoke(invocation);
            } catch (RpcException me) {
                // 省略代码
            } catch (Throwable me) {
                throw new RpcException(getMockExceptionMessage(e, me), me.getCause());
            }
            return result;
        }

        private List<Invoker<T>> selectMockInvoker(Invocation invocation) {
            List<Invoker<T>> invokers = null;
            if (invocation instanceof RpcInvocation) {
                ((RpcInvocation) invocation).setAttachment(Constants.INVOCATION_NEED_MOCK, Boolean.TRUE.toString());
                try {
                    // MockClusterInvoker 和所有的 ClusterInvoker 一样，会维护一个 Directory
                    invokers = directory.list(invocation);
                } catch (RpcException e) {
                    if (logger.isInfoEnabled()) {
                        logger.info("Exception when try to invoke mock. Get mock invokers error for service:"
                                + directory.getUrl().getServiceInterface() + ", method:" + invocation.getMethodName()
                                + ", will contruct a new mock with 'new MockInvoker()'.", e);
                    }
                }
            }
            return invokers;
        }

    }

    /**
     * 配置规则：
     * 
     * 1、使用自定义mock类（接口名+Mock）
     * 
     * mock = default => DemoServiceMock 
     * mock = true => DemoServiceMock 
     * mock = fail => DemoServiceMock 
     * mock = force => DemoServiceMock
     * 
     * 2、先普通执行，执行失败之后再执行相应的mock逻辑
     * 
     * mock = fail:throw => throw new RpcException(" mocked exception for Service degradation. "); 
     * mock = fail:throw XxxException => throw new RpcException(RpcException.BIZ_EXCEPTION, XxxException); 
     * mock = fail:return => return null 
     * mock = fail:return xxx => return xxx 
     * mock = fail:return empty => return new Car()
     * 
     * 3、直接执行相应的mock逻辑
     * 
     * mock = force:throw => throw new RpcException(" mocked exception for Service degradation. "); 
     * mock = force:throw XxxException => throw new RpcException(RpcException.BIZ_EXCEPTION, XxxException); 
     * mock = force:return => return null 
     * mock = force:return xxx => return xxx 、
     * mock = force:return empty => return new Car() Invalid type name:'com.alibaba.dubbo.common.bytecode.Wrapper1'
     */

    final public static class MockInvoker<T> implements Invoker<T> {
        private final static ProxyFactory proxyFactory = ExtensionLoader.getExtensionLoader(ProxyFactory.class)
                .getAdaptiveExtension();
        private final static Map<String, Invoker<?>> mocks = new ConcurrentHashMap<String, Invoker<?>>();
        private final static Map<String, Throwable> throwables = new ConcurrentHashMap<String, Throwable>();

        private final URL url;

        public MockInvoker(URL url) {
            this.url = url;
        }

        public static Object parseMockValue(String mock) throws Exception {
            return parseMockValue(mock, null);
        }

        public static Object parseMockValue(String mock, Type[] returnTypes) throws Exception {
            Object value = null;
            if ("empty".equals(mock)) {
                value = ReflectUtils.getEmptyObject(
                        returnTypes != null && returnTypes.length > 0 ? (Class<?>) returnTypes[0] : null);
            } else if ("null".equals(mock)) {
                value = null;
            } else if ("true".equals(mock)) {
                value = true;
            } else if ("false".equals(mock)) {
                value = false;
            } else if (mock.length() >= 2
                    && (mock.startsWith("\"") && mock.endsWith("\"") || mock.startsWith("\'") && mock.endsWith("\'"))) {
                value = mock.subSequence(1, mock.length() - 1);
            } else if (returnTypes != null && returnTypes.length > 0 && returnTypes[0] == String.class) {
                value = mock;
            } else if (StringUtils.isNumeric(mock)) {
                value = JSON.parse(mock);
            } else if (mock.startsWith("{")) {
                value = JSON.parseObject(mock, Map.class);
            } else if (mock.startsWith("[")) {
                value = JSON.parseObject(mock, List.class);
            } else {
                value = mock;
            }
            if (returnTypes != null && returnTypes.length > 0) {
                value = PojoUtils.realize(value, (Class<?>) returnTypes[0],
                        returnTypes.length > 1 ? returnTypes[1] : null);
            }
            return value;
        }

        /**
         * mock 中的 invoke 方法只处理以下几种情况： 
         * 1.如果是开头是return，比如return null，那么就尝试构建一个null并且返回。 
         * 2.如果是mock抛出异常，则尝试抛出指定的异常。
         * 3.如果都不是，那么就是用户自定义了mock的类，就像之前说的在配置文件中声明mock="XXXMock"，这种情况就尝试去加载XXXMock这个类，然后执行对应的方法进行mock。
         */
        public Result invoke(Invocation invocation) throws RpcException {
            // 获取针对方法的mock设置，因为是 methodName.mock 作为键
            String mock = getUrl().getParameter(invocation.getMethodName() + "." + Constants.MOCK_KEY);
            if (invocation instanceof RpcInvocation) {
                ((RpcInvocation) invocation).setInvoker(this);
            }
            // 如果针对方法没有mock设置，则获取接口级别的mock设置
            if (StringUtils.isBlank(mock)) {
                mock = getUrl().getParameter(Constants.MOCK_KEY);
            }
            //如果没有mock配置抛出异常
            if (StringUtils.isBlank(mock)) {
                throw new RpcException(new IllegalAccessException("mock can not be null. url :" + url));
            }
            mock = normallizeMock(URL.decode(mock));
            if (Constants.RETURN_PREFIX.trim().equalsIgnoreCase(mock.trim())) {
                RpcResult result = new RpcResult();
                result.setValue(null);
                return result;
            // 尝试构建一个return xxx后面的这个xxx的实例
            } else if (mock.startsWith(Constants.RETURN_PREFIX)) {
                // 这里获取到 return 后面的字符串内容
                mock = mock.substring(Constants.RETURN_PREFIX.length()).trim();
                mock = mock.replace('`', '"');
                try {
                    Type[] returnTypes = RpcUtils.getReturnTypes(invocation);
                    Object value = parseMockValue(mock, returnTypes);
                    return new RpcResult(value);
                } catch (Exception ew) {
                    throw new RpcException("mock return invoke error. method :" + invocation.getMethodName() + ", mock:"
                            + mock + ", url: " + url, ew);
                }
            } else if (mock.startsWith(Constants.THROW_PREFIX)) {
                // 如果mock不是返回而是throw异常
                mock = mock.substring(Constants.THROW_PREFIX.length()).trim();
                mock = mock.replace('`', '"');
                if (StringUtils.isBlank(mock)) {
                    throw new RpcException(" mocked exception for Service degradation. ");
                } else { // user customized class
                    // 如果是一个用户自定义异常
                    Throwable t = getThrowable(mock);
                    throw new RpcException(RpcException.BIZ_EXCEPTION, t);
                }
            } else { // impl mock
                try {
                    // 这种情况是我们之前说的，自定义一个XXXMock类的情况，这里会去加载这个类
                    Invoker<T> invoker = getInvoker(mock);
                    return invoker.invoke(invocation);
                } catch (Throwable t) {
                    throw new RpcException("Failed to create mock implemention class " + mock, t);
                }
            }
        }

        @SuppressWarnings("unchecked")
        private Invoker<T> getInvoker(String mockService) {
            // 首先从缓存中去获取 mockService 对应的 invoker 对象，如果命中，就直接返回 invoker
            // 如果没有命中，就通过 JavassistProxyFactory 通过 javassist 编译器动态创建一个 invoker
            Invoker<T> invoker = (Invoker<T>) mocks.get(mockService);
            if (invoker != null) {
                return invoker;
            } else {
                // 获取到接口的 Class 对象，比如 DemoService 接口
                Class<T> serviceType = (Class<T>) ReflectUtils.forName(url.getServiceInterface());
                if (ConfigUtils.isDefault(mockService)) {
                    mockService = serviceType.getName() + "Mock";
                }

                // 获取到 mockService 对应的 class 对象，这里的 mockService 是 DemoServiceMock 字符串
                Class<?> mockClass = ReflectUtils.forName(mockService);
                // 判断 mockClass 对应的类对象是否实现了 serviceType 接口，即判断 DemoServiceMock 类是否实现了
                // DemoService 接口
                if (!serviceType.isAssignableFrom(mockClass)) {
                    throw new IllegalArgumentException("The mock implemention class " + mockClass.getName()
                            + " not implement interface " + serviceType.getName());
                }
                try {
                    T mockObject = (T) mockClass.newInstance();
                    invoker = proxyFactory.getInvoker(mockObject, (Class<T>) serviceType, url);
                    if (mocks.size() < 10000) {
                        // 将创建好的 invoker 添加到缓存中
                        mocks.put(mockService, invoker);
                    }
                    return invoker;
                } catch (InstantiationException e) {
                    throw new IllegalStateException("No such empty constructor \"public " + mockClass.getSimpleName()
                            + "()\" in mock implemention class " + mockClass.getName(), e);
                } catch (IllegalAccessException e) {
                    throw new IllegalStateException(e);
                }
            }
        }

        // mock=fail:throw
        // mock=fail:return
        // mock=xx.Service
        private String normallizeMock(String mock) {
            if (mock == null || mock.trim().length() == 0) {
                return mock;
            } else if (ConfigUtils.isDefault(mock) || "fail".equalsIgnoreCase(mock.trim())
                    || "force".equalsIgnoreCase(mock.trim())) {
                mock = url.getServiceInterface() + "Mock";
            }
            // 如果 mock 是以 fail 开头的话，就获取到 fail 之后的字符串内容
            if (mock.startsWith(Constants.FAIL_PREFIX)) {
                mock = mock.substring(Constants.FAIL_PREFIX.length()).trim();
            // 如果 mock 是以 force 开头的话，就获取到 force 之后的字符串内容
            } else if (mock.startsWith(Constants.FORCE_PREFIX)) {
                mock = mock.substring(Constants.FORCE_PREFIX.length()).trim();
            }
            return mock;
        }

    }

    // class:ConfigUtils
    public static boolean isDefault(String value) {
        return "true".equalsIgnoreCase(value) || "default".equalsIgnoreCase(value);
    }

    public class Wrapper1 extends Wrapper implements ClassGenerator.DC {
        
        public static String[] pns;
        public static Map pts;
        public static String[] mns;
        public static String[] dmns;
        public static Class[] mts0;
        public static Class[] mts1;

        public Object invokeMethod(Object object, String string, Class[] arrclass, Object[] arrobject)
                throws InvocationTargetException {
            DemoServiceImpl demoServiceImpl;
            try {
                demoServiceImpl = (DemoServiceImpl) object;
            } catch (Throwable throwable) {
                throw new IllegalArgumentException(throwable);
            }
            try {
                if ("sayHello".equals(string) && arrclass.length == 1) {
                    return demoServiceImpl.sayHello((String) arrobject[0]);
                }
                if ("sayGoodBye".equals(string) && arrclass.length == 1) {
                    return demoServiceImpl.sayGoodBye((String) arrobject[0]);
                }
            } catch (Throwable throwable) {
                throw new InvocationTargetException(throwable);
            }
            throw new NoSuchMethodException(new StringBuffer().append("Not found method \"").append(string)
                    .append("\" in class com.dubbo.simple.server.DemoServiceImpl.").toString());
        }

        public Class getPropertyType(String string) {
            return (Class) pts.get(string);
        }

        @Override
        public Object getPropertyValue(Object object, String string) {
            try {
                DemoServiceImpl demoServiceImpl = (DemoServiceImpl) object;
            } catch (Throwable throwable) {
                throw new IllegalArgumentException(throwable);
            }
            throw new NoSuchPropertyException(new StringBuffer().append("Not found property \"").append(string)
                    .append("\" filed or setter method in class com.dubbo.simple.server.DemoServiceImpl.").toString());
        }

        @Override
        public void setPropertyValue(Object object, String string, Object object2) {
            try {
                DemoServiceImpl demoServiceImpl = (DemoServiceImpl) object;
            } catch (Throwable throwable) {
                throw new IllegalArgumentException(throwable);
            }
            throw new NoSuchPropertyException(new StringBuffer().append("Not found property \"").append(string)
                    .append("\" filed or setter method in class com.dubbo.simple.server.DemoServiceImpl.").toString());
        }

        @Override
        public String[] getPropertyNames() {
            return pns;
        }

        @Override
        public String[] getMethodNames() {
            return mns;
        }

        @Override
        public boolean hasProperty(String string) {
            return pts.containsKey(string);
        }

        @Override
        public String[] getDeclaredMethodNames() {
            return dmns;
        }
    }

}