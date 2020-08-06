public class DubboAdaptive {

    /**
     * Dubbo Adaptive使用示例
     */

    public interface WheelMaker {
        Wheel makeWheel(URL url);
    }

    public class AdaptiveWheelMaker implements WheelMaker {
        public Wheel makeWheel(URL url) {
            if (url == null) {
                throw new IllegalArgumentException("url == null");
            }

            // 1.从 URL 中获取 WheelMaker 名称
            String wheelMakerName = url.getParameter("Wheel.maker");
            if (wheelMakerName == null) {
                throw new IllegalArgumentException("wheelMakerName == null");
            }

            // 2.通过 SPI 加载具体的 WheelMaker
            WheelMaker wheelMaker = ExtensionLoader.getExtensionLoader(WheelMaker.class).getExtension(wheelMakerName);

            // 3.调用目标方法
            return wheelMaker.makeWheel(url);
        }
    }

    public interface CarMaker {
        Car makeCar(URL url);
    }

    public class RaceCarMaker implements CarMaker {
        WheelMaker wheelMaker;

        // 通过 setter 注入 AdaptiveWheelMaker
        public setWheelMaker(WheelMaker wheelMaker) {
            this.wheelMaker = wheelMaker;
        }

        public Car makeCar(URL url) {
            Wheel wheel = wheelMaker.makeWheel(url);
            return new RaceCar(wheel, ...);
        }
    }

    //    ***********************************源码分析***************************************
    /**
     * Adaptive 可注解在类或方法上。当 Adaptive 注解在类上时，Dubbo 不会为该类生成代理类。 注解在方法（接口方法）上时，Dubbo
     * 则会为该方法生成代理逻辑。 也就是当@Adaptive注解在接口方法上时，表示拓展的加载逻辑需由框架自动生成当@Adaptive注解在类上时，
     * 表示拓展的加载逻辑需要由人工编码完成
     */
    @Documented
    @Retention(RetentionPolicy.RUNTIME)
    @Target({ ElementType.TYPE, ElementType.METHOD })
    public @interface Adaptive {
        String[] value() default {};
    }

    public class ExtensionLoader {

        private final Holder<Object> cachedAdaptiveInstance = new Holder<Object>();

        private volatile Class<?> cachedAdaptiveClass = null;

        public T getAdaptiveExtension() {
            // 从缓存中获取自适应拓展
            Object instance = cachedAdaptiveInstance.get();
            if (instance == null) { // 缓存未命中
                if (createAdaptiveInstanceError == null) {
                    synchronized (cachedAdaptiveInstance) {
                        instance = cachedAdaptiveInstance.get();
                        if (instance == null) {
                            try {
                                // 创建自适应拓展
                                instance = createAdaptiveExtension();
                                // 设置自适应拓展到缓存中
                                cachedAdaptiveInstance.set(instance);
                            } catch (Throwable t) {
                                createAdaptiveInstanceError = t;
                                throw new IllegalStateException("fail to create adaptive instance: " + t.toString(), t);
                            }
                        }
                    }
                } else {
                    throw new IllegalStateException(
                            "fail to create adaptive instance: " + createAdaptiveInstanceError.toString(),
                            createAdaptiveInstanceError);
                }
            }

            return (T) instance;
        }

        private T createAdaptiveExtension() {
            try {
                // 获取自适应拓展类，并通过反射实例化，然后进行依赖注入
                // Dubbo 中有两种类型的自适应拓展，一种是用户自己编写的自适应拓展，一种是程序自动生成的。手工编码的自适应拓展中可能存在着一些依赖，
                // 而自动生成的 Adaptive 拓展则不会依赖其他类。这里调用 injectExtension 方法的目的是为手工编码的自适应拓展注入依赖
                return injectExtension((T) getAdaptiveExtensionClass().newInstance());
            } catch (Exception e) {
                throw new IllegalStateException(
                        "Can not create adaptive extension " + type + ", cause: " + e.getMessage(), e);
            }
        }

        private Class<?> getAdaptiveExtensionClass() {
            // 通过 SPI 获取所有的拓展类，也就是获取到某个接口的全部实现类，比如该方法可以获取 Protocol 接口的DubboProtocol、HttpProtocol、InjvmProtocol 等实现类。
            // 在获取实现类的过程中，如果某个实现类被 Adaptive 注解修饰了（就表明此拓展是用户自定义的），那么该类就会被赋值给 cachedAdaptiveClass 变量，
            // 下面一步中就可以直接返回获取到的cachedAdaptiveClass。如果如果所有的实现类均未被 Adaptive 注解修饰，那么执行下面第三步逻辑，创建自适应拓展类。
            getExtensionClasses();
            // 检查缓存，若缓存不为空，则直接返回缓存
            if (cachedAdaptiveClass != null) {
                return cachedAdaptiveClass;
            }
            // 创建自适应拓展类
            return cachedAdaptiveClass = createAdaptiveExtensionClass();
        }

        private Class<?> createAdaptiveExtensionClass() {
            // 构建自适应拓展类的源代码
            String code = createAdaptiveExtensionClassCode();
            ClassLoader classLoader = findClassLoader();
            // 获取编译器实现类，Dubbo 默认使用 javassist 作为编译器。
            // 注意，在这里会获取到的是AdaptiveCompiler，因为AdaptiveCompiler类上面有@Adaptive注解，因此返回的就是AdaptiveCompiler。
            // 在AdaptiveCompiler类中的compile方法中，由于DEFAULT_COMPILER为null，因此通过loader.getDefaultExtension来获取到
            // Compiler默认的实现。而在Compiler接口的@SPI中，指定了value为javassit编译器，因此最终获得的编译器为javassit
            com.alibaba.dubbo.common.compiler.Compiler compiler = ExtensionLoader
                    .getExtensionLoader(com.alibaba.dubbo.common.compiler.Compiler.class).getAdaptiveExtension();
            // 编译代码，生成 Class 实例
            return compiler.compile(code, classLoader);
        }

        @Adaptive
        public static class AdaptiveCompiler implements Compiler {

            private static volatile String DEFAULT_COMPILER;

            public static void setDefaultCompiler(String compiler) {
                DEFAULT_COMPILER = compiler;
            }

            @Override
            public Class<?> compile(String code, ClassLoader classLoader) {
                Compiler compiler;
                ExtensionLoader<Compiler> loader = ExtensionLoader.getExtensionLoader(Compiler.class);
                String name = DEFAULT_COMPILER; // copy reference
                if (name != null && name.length() > 0) {
                    compiler = loader.getExtension(name);
                } else {
                    compiler = loader.getDefaultExtension();
                }
                return compiler.compile(code, classLoader);
            }

        }

        @SPI("javassist")
        public interface Compiler {

            Class<?> compile(String code, ClassLoader classLoader);

        }

        private String createAdaptiveExtensionClassCode() {
            StringBuilder codeBuilder = new StringBuilder();

            // 通过反射获取所有的方法
            Method[] methods = type.getMethods();
            boolean hasAdaptiveAnnotation = false;
            // 遍历方法列表，检测是否有一个方法被Adaptive注解修饰
            // 对于要生成自适应拓展的接口，Dubbo 要求该接口至少有一个方法被 Adaptive 注解修饰。若不满足此条件，就会抛出运行时异常。
            for (Method m : methods) {
                if (m.isAnnotationPresent(Adaptive.class)) {
                    hasAdaptiveAnnotation = true;
                    break;
                }
            }

            // 若所有的方法上均无 Adaptive 注解，则抛出异常
            if (!hasAdaptiveAnnotation)
                throw new IllegalStateException(
                        "No adaptive method on extension " + type.getName() + ", refuse to create the adaptive class!");

            // 生成 package 代码：package + type 所在包
            codeBuilder.append("package ").append(type.getPackage().getName()).append(";");
            // 生成 import 代码：import + ExtensionLoader 全限定名
            codeBuilder.append("\nimport ").append(ExtensionLoader.class.getName()).append(";");
            // 生成类代码：public class + type简单名称 + $Adaptive + implements + type全限定名 + {
            codeBuilder.append("\npublic class ").append(type.getSimpleName()).append("$Adaptive")
                    .append(" implements ").append(type.getCanonicalName()).append(" {");

            for (Method method : methods) {
                Class<?> rt = method.getReturnType();
                Class<?>[] pts = method.getParameterTypes();
                Class<?>[] ets = method.getExceptionTypes();

                Adaptive adaptiveAnnotation = method.getAnnotation(Adaptive.class);
                StringBuilder code = new StringBuilder(512);

                // 如果方法上无 Adaptive 注解，则生成 throw new UnsupportedOperationException(...) 代码
                if (adaptiveAnnotation == null) {
                    // 生成的代码格式如下：
                    // throw new UnsupportedOperationException(
                    // "method " + 方法签名 + of interface + 全限定接口名 + is not adaptive method!”)
                    code.append("throw new UnsupportedOperationException(\"method ").append(method.toString())
                            .append(" of interface ").append(type.getName()).append(" is not adaptive method!\");");
                } else {
                    // 前面说过方法代理逻辑会从 URL 中提取目标拓展的名称，因此代码生成逻辑的一个重要的任务是从方法的参数列表
                    // 或者其他参数中获取 URL 数据，比如说调用 Invoker 对象的相关方法，比如getUrl，若没有相关的getter方法，则
                    // 会直接抛出异常
                    int urlTypeIndex = -1;
                    // 遍历参数列表，确定 URL 参数位置
                    for (int i = 0; i < pts.length; ++i) {
                        if (pts[i].equals(URL.class)) {
                            urlTypeIndex = i;
                            break;
                        }
                    }
                    // urlTypeIndex != -1，表示参数列表中存在 URL 参数
                    if (urlTypeIndex != -1) {
                        // 为 URL 类型参数生成判空代码，格式如下：
                        // if (arg%d == null)
                        // throw new IllegalArgumentException("url == null");
                        String s = String.format(
                                "\nif (arg%d == null) throw new IllegalArgumentException(\"url == null\");",
                                urlTypeIndex);
                        code.append(s);

                        // 为 URL 类型参数生成赋值代码，形如 URL url = arg1
                        s = String.format("\n%s url = arg%d;", URL.class.getName(), urlTypeIndex);
                        code.append(s);
                    }
                    // 参数列表中不存在 URL 类型参数
                    else {
                        String attribMethod = null;

                        // find URL getter method
                        LBL_PTS:
                        // 遍历方法的参数类型列表
                        for (int i = 0; i < pts.length; ++i) {
                            // 获取某一类型参数的全部方法
                            Method[] ms = pts[i].getMethods();
                            // 遍历方法列表，寻找可返回 URL 的 getter 方法
                            for (Method m : ms) {
                                String name = m.getName();
                                // 1. 方法名以 get 开头，或方法名大于3个字符
                                // 2. 方法的访问权限为 public
                                // 3. 非静态方法
                                // 4. 方法参数数量为0
                                // 5. 方法返回值类型为 URL
                                if ((name.startsWith("get") || name.length() > 3) && Modifier.isPublic(m.getModifiers())
                                        && !Modifier.isStatic(m.getModifiers()) && m.getParameterTypes().length == 0
                                        && m.getReturnType() == URL.class) {
                                    urlTypeIndex = i;
                                    attribMethod = name;
                                    break LBL_PTS;
                                }
                            }
                        }
                        if (attribMethod == null) {
                            // 如果所有参数中均不包含可返回 URL 的 getter 方法，则抛出异常
                            throw new IllegalStateException(
                                    "fail to create adaptive class for interface " + type.getName()
                                            + ": not found url parameter or url attribute in parameters of method "
                                            + method.getName());
                        }

                        // 为可返回 URL 的参数生成判空代码，格式如下：
                        // if (argN == null)
                        // throw new IllegalArgumentException("参数全限定名 + argument == null");
                        String s = String.format(
                                "\nif (arg%d == null) throw new IllegalArgumentException(\"%s argument == null\");",
                                urlTypeIndex, pts[urlTypeIndex].getName());
                        code.append(s);

                        // 为 getter 方法返回的 URL 生成判空代码，格式如下：
                        // if (argN.getter方法名() == null)
                        // throw new IllegalArgumentException(参数全限定名 + argument getUrl() == null);
                        s = String.format(
                                "\nif (arg%d.%s() == null) throw new IllegalArgumentException(\"%s argument %s() == null\");",
                                urlTypeIndex, attribMethod, pts[urlTypeIndex].getName(), attribMethod);
                        code.append(s);

                        // 生成赋值语句，格式为：URL全限定名 url = argN.getter方法名()，比如：
                        // com.alibaba.dubbo.common.URL url = invoker.getUrl();
                        s = String.format("%s url = arg%d.%s();", URL.class.getName(), urlTypeIndex, attribMethod);
                        code.append(s);
                    }

                    String[] value = adaptiveAnnotation.value();

                    // value is not set, use the value generated from class name as the key
                    // 若 value 为非空数组，直接获取数组内容即可。若 value 为空数组，则需进行额外处理。处理过程是将类名转换为字符数组，
                    // 然后遍历字符数组，并将字符放入 StringBuilder 中。若字符为大写字母，则向 StringBuilder 中添加点号，
                    // 随后将字符变为小写存入 StringBuilder 中。比如 LoadBalance 经过处理后，得到 load.balance。
                    if (value.length == 0) {
                        // 获取类名，并将类名转换为字符数组
                        char[] charArray = type.getSimpleName().toCharArray();
                        StringBuilder sb = new StringBuilder(128);
                        // 遍历字节数组
                        for (int i = 0; i < charArray.length; i++) {
                            if (Character.isUpperCase(charArray[i])) {
                                if (i != 0) {
                                    sb.append(".");
                                }
                                sb.append(Character.toLowerCase(charArray[i]));
                            } else {
                                sb.append(charArray[i]);
                            }
                        }
                        value = new String[] { sb.toString() };
                    }

                    // 此段逻辑是检测方法列表中是否存在 Invocation 类型的参数，若存在，则为其生成判空代码和其他一些代码。
                    boolean hasInvocation = false;
                    for (int i = 0; i < pts.length; ++i) {
                        if (pts[i].getName().equals("com.alibaba.dubbo.rpc.Invocation")) {
                            // Null Point check
                            String s = String.format(
                                    "\nif (arg%d == null) throw new IllegalArgumentException(\"invocation == null\");",
                                    i);
                            code.append(s);
                            s = String.format("\nString methodName = arg%d.getMethodName();", i);
                            code.append(s);
                            hasInvocation = true;
                            break;
                        }
                    }

                    // 设置默认拓展名，cachedDefaultName 源于 SPI 注解值，默认情况下，SPI 注解值为空串，此时 cachedDefaultName =
                    // null
                    String defaultExtName = cachedDefaultName;
                    String getNameCode = null;

                    // 遍历 value，这里的 value 是 Adaptive 的注解值，2.2.3.3 节分析过 value 变量的获取过程。此处循环目的是生成从 URL
                    // 中获取拓展名的代码，
                    // 生成的代码会赋值给 getNameCode 变量。注意这个循环的遍历顺序是由后向前遍历的。
                    for (int i = value.length - 1; i >= 0; --i) {
                        if (i == value.length - 1) {
                            // 默认拓展名非空，也就是说SPI注解中默认的拓展名非空
                            if (null != defaultExtName) {
                                // protocol 是 url 的一部分，可通过 getProtocol 方法获取，其他的则是从URL 参数中获取。因为获取方式不同，
                                // 所以这里要判断 value[i] 是否为 protocol
                                if (!"protocol".equals(value[i]))
                                    if (hasInvocation)
                                        // 生成的代码功能等价于下面的代码：
                                        // url.getMethodParameter(methodName, value[i], defaultExtName)
                                        // 以 LoadBalance 接口的 select 方法为例，最终生成的代码如下：
                                        // url.getMethodParameter(methodName, "loadbalance", "random")
                                        getNameCode = String.format(
                                                "url.getMethodParameter(methodName, \"%s\", \"%s\")", value[i],
                                                defaultExtName);
                                    else
                                        // 生成的代码功能等价于下面的代码：
                                        // url.getParameter(value[i], defaultExtName)
                                        getNameCode = String.format("url.getParameter(\"%s\", \"%s\")", value[i],
                                                defaultExtName);
                                else
                                    getNameCode = String.format(
                                            "( url.getProtocol() == null ? \"%s\" : url.getProtocol() )",
                                            defaultExtName);
                            } else {
                                if (!"protocol".equals(value[i]))
                                    if (hasInvocation)
                                        getNameCode = String.format(
                                                "url.getMethodParameter(methodName, \"%s\", \"%s\")", value[i],
                                                defaultExtName);
                                    else
                                        getNameCode = String.format("url.getParameter(\"%s\")", value[i]);
                                else
                                    getNameCode = "url.getProtocol()";
                            }
                        } else {
                            if (!"protocol".equals(value[i]))
                                if (hasInvocation)
                                    getNameCode = String.format("url.getMethodParameter(methodName, \"%s\", \"%s\")",
                                            value[i], defaultExtName);
                                else
                                    // 生成的代码功能等价于下面的代码：
                                    // url.getParameter(value[i], getNameCode)
                                    // 以 Transporter 接口的 connect 方法为例，最终生成的代码如下：
                                    // url.getParameter("client", url.getParameter("transporter", "netty"))
                                    getNameCode = String.format("url.getParameter(\"%s\", %s)", value[i], getNameCode);
                            else
                                // 生成的代码功能等价于下面的代码：
                                // url.getProtocol() == null ? getNameCode : url.getProtocol()
                                // 以 Protocol 接口的 connect 方法为例，最终生成的代码如下：
                                // url.getProtocol() == null ? "dubbo" : url.getProtocol()
                                getNameCode = String.format("url.getProtocol() == null ? (%s) : url.getProtocol()",
                                        getNameCode);
                        }
                    }
                    //生成 extName 赋值代码
                    code.append("\nString extName = ").append(getNameCode).append(";");
                    //生成 extName 判空代码
                    String s = String.format("\nif(extName == null) "
                            + "throw new IllegalStateException(\"Fail to get extension(%s) name from url(\" + url.toString() + \") use keys(%s)\");",
                            type.getName(), Arrays.toString(value));
                    code.append(s);

                    //根据拓展名加载拓展实例，并调用拓展实例的目标方法
                    //生成拓展获取代码，格式如下：
                    //type全限定名 extension = (type全限定名)ExtensionLoader全限定名
                    //.getExtensionLoader(type全限定名.class).getExtension(extName);
                    //Tips: 格式化字符串中的 %<s 表示使用前一个转换符所描述的参数，即 type 全限定名
                    s = String.format("\n%s extension = (%<s)%s.getExtensionLoader(%s.class).getExtension(extName);",
                            type.getName(), ExtensionLoader.class.getSimpleName(), type.getName());
                    code.append(s);

                    //return statement
                    if (!rt.equals(void.class)) {
                        code.append("\nreturn ");
                    }

                    //生成目标方法调用逻辑，格式为：
                    //extension.方法名(arg0, arg2, ..., argN);
                    s = String.format("extension.%s(", method.getName());
                    code.append(s);
                    for (int i = 0; i < pts.length; i++) {
                        if (i != 0)
                            code.append(", ");
                        code.append("arg").append(i);
                    }
                    code.append(");");
                }

                //public + 返回值全限定名 + 方法名 + (
                codeBuilder.append("\npublic ").append(rt.getCanonicalName()).append(" ").append(method.getName())
                        .append("(");
                //添加参数列表代码
                for (int i = 0; i < pts.length; i++) {
                    if (i > 0) {
                        codeBuilder.append(", ");
                    }
                    codeBuilder.append(pts[i].getCanonicalName());
                    codeBuilder.append(" ");
                    codeBuilder.append("arg").append(i);
                }
                codeBuilder.append(")");
                //添加异常抛出代码
                if (ets.length > 0) {
                    codeBuilder.append(" throws ");
                    for (int i = 0; i < ets.length; i++) {
                        if (i > 0) {
                            codeBuilder.append(", ");
                        }
                        codeBuilder.append(ets[i].getCanonicalName());
                    }
                }
                codeBuilder.append(" {");
                codeBuilder.append(code.toString());
                codeBuilder.append("\n}");
            }
            codeBuilder.append("\n}");
            if (logger.isDebugEnabled()) {
                logger.debug(codeBuilder.toString());
            }
            return codeBuilder.toString();
        }

    }

    /**
     * 此接口中@SPI注解中的值"zookeeper"的作用为默认值，而@Adaptive注解中的"key1"和"key2"为getParameter中的
     * 键值，如果没有在@Adaptive注解中进行指明的话，就使用小写类名（加上.号）。因此
     * register方法中获取参数为：
     * url.getParameter("registry.center", "zookeeper");
     * 在discovery方法中获取参数为：
     * url.getParameter("key1", url.getParameter("key2", "zookeeper"));
     */
    @SPI("zookeeper")
    public interface RegistryCenter {
        /**
         * 注册服务
         */
        @Adaptive()
        String register(URL url, String content);

        /**
         * 发现服务
         */
        @Adaptive({"key1", "key2"})
        String discovery(URL url, String content);
    }

    @SPI("dubbo")
    public interface Protocol {

        int getDefaultPort();

        @Adaptive
        <T> Exporter<T> export(Invoker<T> invoker) throws RpcException;

        @Adaptive
        <T> Invoker<T> refer(Class<T> type, URL url) throws RpcException;

        void destroy();
    }

    /**
     * 通过自适应扩展生成的Protocol$Adaptive对象，其中destroy和getDefaultPort方法直接抛出异常。这是因为Protocol接口中有4个方法，但只有export和refer两个方法使用了@Adaptive注解。Dubbo自动生成的自适应实例，只有@Adaptive修饰的方法才有具体的实现。
     * 所以，Protocol$Adaptive 类中，也只有export和refer这两个方法有具体的实现，其余方法都是抛出异常。
     */
    public class Protocol$Adaptive implements org.apache.dubbo.rpc.Protocol {
        public void destroy() {
            throw new UnsupportedOperationException("method public abstract void org.apache.dubbo.rpc.Protocol.destroy() of interface org.apache.dubbo.rpc.Protocol is not adaptive method!");
        }
    
        public int getDefaultPort() {
            throw new UnsupportedOperationException("method public abstract int org.apache.dubbo.rpc.Protocol.getDefaultPort() of interface org.apache.dubbo.rpc.Protocol is not adaptive method!");
        }
    
        public org.apache.dubbo.rpc.Exporter export(org.apache.dubbo.rpc.Invoker arg0) throws org.apache.dubbo.rpc.RpcException {
            if (arg0 == null) throw new IllegalArgumentException("org.apache.dubbo.rpc.Invoker argument == null");
            if (arg0.getUrl() == null)
                throw new IllegalArgumentException("org.apache.dubbo.rpc.Invoker argument getUrl() == null");
            org.apache.dubbo.common.URL url = arg0.getUrl();
            String extName = (url.getProtocol() == null ? "dubbo" : url.getProtocol());
            if (extName == null)
                throw new IllegalStateException("Fail to get extension(org.apache.dubbo.rpc.Protocol) name from url(" + url.toString() + ") use keys([protocol])");
            org.apache.dubbo.rpc.Protocol extension = (org.apache.dubbo.rpc.Protocol) ExtensionLoader.getExtensionLoader(org.apache.dubbo.rpc.Protocol.class).getExtension(extName);
            return extension.export(arg0);
        }
    
        public org.apache.dubbo.rpc.Invoker refer(java.lang.Class arg0, org.apache.dubbo.common.URL arg1) throws org.apache.dubbo.rpc.RpcException {
            if (arg1 == null) throw new IllegalArgumentException("url == null");
            org.apache.dubbo.common.URL url = arg1;
            String extName = (url.getProtocol() == null ? "dubbo" : url.getProtocol());
            if (extName == null)
                throw new IllegalStateException("Fail to get extension(org.apache.dubbo.rpc.Protocol) name from url(" + url.toString() + ") use keys([protocol])");
            org.apache.dubbo.rpc.Protocol extension = (org.apache.dubbo.rpc.Protocol) ExtensionLoader.getExtensionLoader(org.apache.dubbo.rpc.Protocol.class).getExtension(extName);
            return extension.refer(arg0, arg1);
        }
    }

}