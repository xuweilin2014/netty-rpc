import java.lang.annotation.Annotation;

public class DubboSPI {

    public static class ExtensionLoader {

        private static final ConcurrentMap<Class<?>, ExtensionLoader<?>> EXTENSION_LOADERS = new ConcurrentHashMap<Class<?>, ExtensionLoader<?>>();

        private final ConcurrentMap<String, Holder<Object>> cachedInstances = new ConcurrentHashMap<String, Holder<Object>>();

        private static final ConcurrentMap<Class<?>, Object> EXTENSION_INSTANCES = new ConcurrentHashMap<Class<?>, Object>();

        private Set<Class<?>> cachedWrapperClasses;

        //用来保存extensionClasses这个Map对象，extensionClasses保存了从名称到Class对象的映射
        private final Holder<Map<String, Class<?>>> cachedClasses = new Holder<Map<String, Class<?>>>();

        private volatile Class<?> cachedAdaptiveClass = null;

        private final ConcurrentMap<Class<?>, String> cachedNames = new ConcurrentHashMap<Class<?>, String>();

        private String cachedDefaultName;

        private ExtensionLoader(Class<?> type) {
            this.type = type;
            objectFactory = (type == ExtensionFactory.class ? null : ExtensionLoader.getExtensionLoader(ExtensionFactory.class).getAdaptiveExtension());
        }

        // getExtensionLoader方法是一个静态工厂方法，入参是一个可扩展的接口，返回一个该接口的ExtensionLoader实体类。
        // 通过这个实体类，可以根据name获得具体的扩展，也可以获得一个自适应扩展
        public static <T> ExtensionLoader<T> getExtensionLoader(Class<T> type) {
            //传进来的type是否为空
            if (type == null)
                throw new IllegalArgumentException("Extension type == null");
            //校验传进的type类是否为接口
            if (!type.isInterface()) {
                throw new IllegalArgumentException("Extension type(" + type + ") is not interface!");
            }
            //校验传进的type类是否有@SPI注解
            if (!withExtensionAnnotation(type)) {
                throw new IllegalArgumentException("Extension type(" + type + ") is not extension, because WITHOUT @"
                        + SPI.class.getSimpleName() + " Annotation!");
            }

            //从ExtensionLoader缓存中查询是否已经存在对应类型type的ExtensionLoader实例
            ExtensionLoader<T> loader = (ExtensionLoader<T>) EXTENSION_LOADERS.get(type);
            if (loader == null) {
                //没有就new一个type类型的ExtensionLoader实例，并存入本地缓存
                EXTENSION_LOADERS.putIfAbsent(type, new ExtensionLoader<T>(type));
                loader = (ExtensionLoader<T>) EXTENSION_LOADERS.get(type);
            }
            return loader;
        }

        /**
         * 这里getExtension()方法的主要作用是获取name对应的子类对象返回。其实现方式是首先读取定义文件中的子类，不同的子类对象的功能的不同，
         * 比如使用@Adaptive修饰的装饰类和用于AOP的Wrapper类，分别将其保存到不同的缓存中。最后根据传入的name获取其对应的子类对象，
         * 并且使用相应的Wrapper类对其进行包装。
         */
        public T getExtension(String name) {
            if (name == null || name.length() == 0)
                throw new IllegalArgumentException("Extension name == null");

            if ("true".equals(name)) {
                // 获取默认的拓展实现类，即@SPI注解上的默认实现类, 如@SPI("benz")
                return getDefaultExtension();
            }

            // Holder顾名思义是用来持有目标对象，也就是name对应的拓展实例
            // 查看当前是否已经缓存有保存目标对象的实例的holder对象，缓存了则直接返回，没缓存则创建一个并缓存起来
            Holder<Object> holder = cachedInstances.get(name);
            if (holder == null) {
                cachedInstances.putIfAbsent(name, new Holder<Object>());
                holder = cachedInstances.get(name);
            }
            Object instance = holder.get();

            // 如果无法从Holder中获取目标对象的实例，则使用双检查法为目标对象创建一个实例
            if (instance == null) {
                synchronized (holder) {
                    instance = holder.get();
                    if (instance == null) {
                        // 创建name对应的拓展实例
                        instance = createExtension(name);
                        // 设置实例到holder中
                        holder.set(instance);
                    }
                }
            }
            return (T) instance;
        }

        // getDefaultExtension会获取到 @SPI 注解中的value值，也就是默认使用的扩展的名字，然后根据这个名字再调用 getExtension 方法
        public T getDefaultExtension() {
            getExtensionClasses();
            if (null == cachedDefaultName || cachedDefaultName.length() == 0
                    || "true".equals(cachedDefaultName)) {
                return null;
            }
            // 其中的cachedDefaultName是在loadExtensionClasses方法中设置的，其实就是用户在 @SPI 注解中配置的value值
            return getExtension(cachedDefaultName);
        }

        /**
         * createExtension主要进行以下4个方面的操作：
         * 1.从ClassPath下META-INF文件夹下读取扩展点配置文件，并获取配置文件中的各个子类，然后将目标name对应的子类返回（返回的为Class<?>类型的对象）
         * 2.通过反射创建实例
         * 3.为实例注入依赖（通过实例中的set方法）
         * 4.获取定义文件中定义的wrapper对象，然后使用该wrapper对象封装目标对象，并且还会调用其set方法为wrapper对象注入其所依赖的属性
         * 
         * 关于wrapper对象，这里需要说明的是，其主要作用是为目标对象实现AOP。wrapper对象有两个特点：
         * a. 与目标对象实现了同一个目标接口
         * b. 有一个以目标接口为参数类型的构造函数。
         * 这也就是上述createExtension()方法最后封装wrapper对象时传入的构造函数实例始终可以为instance实例的原因
         */
        private T createExtension(String name) {
            // getExtensionClasses用来获取配置文件中所有的拓展类，可以得到 "配置项名称" -> "配置类" 的对应关系
            Class<?> clazz = getExtensionClasses().get(name);
            if (clazz == null) {
                throw findException(name);
            }
            try {
                T instance = (T) EXTENSION_INSTANCES.get(clazz);
                if (instance == null) {
                    // 通过反射创建实例，并且放入到EXTENSION_INSTANCES中保存起来
                    EXTENSION_INSTANCES.putIfAbsent(clazz, clazz.newInstance());
                    instance = (T) EXTENSION_INSTANCES.get(clazz);
                }
                // 满足创建的实例instance的依赖关系，为生成的实例通过其set方法注入对应的实例，这里实例的获取方式不仅可以通过SPI的方式也可以通过Spring的bean工厂获取
                injectExtension(instance);
                Set<Class<?>> wrapperClasses = cachedWrapperClasses;
                // 循环创建 Wrapper 实例，将真正拓展对象包裹在Wrapper对象中，实现了Dubbo AOP的功能
                if (wrapperClasses != null && !wrapperClasses.isEmpty()) {
                    for (Class<?> wrapperClass : wrapperClasses) {
                        // 将当前 instance 作为参数传给 Wrapper 的构造方法，并通过反射创建 Wrapper 实例。
                        // 循环创建 Wrapper 实例,形成Wrapper包装链
                        instance = injectExtension((T) wrapperClass.getConstructor(type).newInstance(instance));
                    }
                }
                return instance;
            } catch (Throwable t) {
                throw new IllegalStateException("Extension instance(name: " + name + ", class: " + type
                        + ")  could not be instantiated: " + t.getMessage(), t);
            }
        }


        // 我们在通过名称获取拓展类之前，首先需要根据配置文件解析出拓展项名称到拓展类的映射关系表（Map<名称, 拓展类>），
        // 之后再根据拓展项名称从映射关系表中取出相应的拓展类即可。
        private Map<String, Class<?>> getExtensionClasses() {
            Map<String, Class<?>> classes = cachedClasses.get();

            // 这里也是先检查缓存，若缓存未命中，则通过 synchronized 加锁。加锁后再次检查缓存，并判空。
            // 此时如果 classes 仍为 null，则通过 loadExtensionClasses 加载拓展类
            if (classes == null) {
                synchronized (cachedClasses) {
                    classes = cachedClasses.get();
                    if (classes == null) {
                        // 加载定义文件，并且将定义文件中的类按照功能缓存在不同的属性中，即：
                        // a. 目标class类型缓存在cachedClasses；
                        // b. wrapper的class类型缓存在cachedWrapperClasses；
                        // c. 用于装饰的class类型缓存在cachedAdaptiveClass；
                        classes = loadExtensionClasses();
                        cachedClasses.set(classes);
                    }
                }
            }
            return classes;
        }

        private Map<String, Class<?>> loadExtensionClasses() {
            // 获取目标接口上通过@SPI注解定义的默认子类对应的名称，并将其缓存在cachedDefaultName中
            final SPI defaultAnnotation = type.getAnnotation(SPI.class);
            if (defaultAnnotation != null) {
                String value = defaultAnnotation.value();
                if ((value = value.trim()).length() > 0) {
                    // 对 SPI 注解内容进行切分
                    String[] names = NAME_SEPARATOR.split(value);
                    // 检测 SPI 注解内容是否合法，不合法则抛出异常
                    if (names.length > 1) {
                        throw new IllegalStateException("more than 1 default extension name on extension "
                                + type.getName() + ": " + Arrays.toString(names));
                    }

                    // 设置默认名称，参考 getDefaultExtension 方法
                    if (names.length == 1)
                        cachedDefaultName = names[0];
                }
            }

            Map<String, Class<?>> extensionClasses = new HashMap<String, Class<?>>();
            
            // 分别在META-INF/dubbo/internal、META-INF/dubbo、META-INF/services目录下
            // 获取定义文件，并且读取定义文件中的内容，这里主要是通过META-INF/dubbo/internal
            // 获取目标定义文件
            loadDirectory(extensionClasses, DUBBO_INTERNAL_DIRECTORY);
            loadDirectory(extensionClasses, DUBBO_DIRECTORY);
            loadDirectory(extensionClasses, SERVICES_DIRECTORY);
            return extensionClasses;
        }

        // loadDirectory 方法先通过 classLoader 获取所有资源链接，然后再通过 loadResource 方法根据
        // 对应的文件URL地址来加载资源。
        private void loadDirectory(Map<String, Class<?>> extensionClasses, String dir) {
            // fileName = 文件夹路径 + type 全限定名
            String fileName = dir + type.getName();
            try {
                Enumeration<java.net.URL> urls;
                ClassLoader classLoader = findClassLoader();
                // 根据文件名加载所有的同名文件
                if (classLoader != null) {
                    urls = classLoader.getResources(fileName);
                } else {
                    urls = ClassLoader.getSystemResources(fileName);
                }
                if (urls != null) {
                    while (urls.hasMoreElements()) {
                        java.net.URL resourceURL = urls.nextElement();
                        loadResource(extensionClasses, classLoader, resourceURL);
                    }
                }
            } catch (Throwable t) {
                logger.error("Exception when load extension class(interface: " + type + ", description file: "
                        + fileName + ").", t);
            }
        }

        private void loadResource(Map<String, Class<?>> extensionClasses, ClassLoader classLoader,
                java.net.URL resourceURL) {
            try {
                BufferedReader reader = new BufferedReader(new InputStreamReader(resourceURL.openStream(), "utf-8"));
                try {
                    String line;
                    //按行读取配置内容
                    while ((line = reader.readLine()) != null) {
                        //定位 # 字符，#表示注释
                        final int ci = line.indexOf('#');
                        if (ci >= 0)
                            //截取 # 之前的字符串，# 之后的内容为注释，需要忽略
                            line = line.substring(0, ci);
                        line = line.trim();
                        if (line.length() > 0) {
                            try {
                                String name = null;
                                //以等于号 = 为界，截取键与值
                                int i = line.indexOf('=');
                                if (i > 0) {
                                    //获取键值
                                    name = line.substring(0, i).trim();
                                    //获取键对应的类的全限定类名
                                    line = line.substring(i + 1).trim();
                                }
                                if (line.length() > 0) {
                                    //加载类，并通过 loadClass 方法把加载的类放入到extensionClasses中进行缓存
                                    loadClass(extensionClasses, resourceURL, Class.forName(line, true, classLoader),
                                            name);
                                }
                            } catch (Throwable t) {
                                IllegalStateException e = new IllegalStateException(
                                        "Failed to load extension class(interface: " + type + ", class line: " + line
                                                + ") in " + resourceURL + ", cause: " + t.getMessage(),
                                        t);
                                exceptions.put(line, e);
                            }
                        }
                    }
                } finally {
                    reader.close();
                }
            } catch (Throwable t) {
                logger.error("Exception when load extension class(interface: " + type + ", class file: " + resourceURL
                        + ") in " + resourceURL, t);
            }
        }

        private void loadClass(Map<String, Class<?>> extensionClasses, java.net.URL resourceURL, Class<?> clazz,
                String name) throws NoSuchMethodException {
            //检查clazz是否是type所表示的接口的实现类，比如Car接口，这里的clazz是Tesla
            if (!type.isAssignableFrom(clazz)) {
                throw new IllegalStateException("Error when load extension class(interface: " + type + ", class line: "
                        + clazz.getName() + "), class " + clazz.getName() + "is not subtype of interface.");
            }
            //检测目标类上是否有 Adaptive 注解，如果带有@Adaptive注解的话，就会缓存到cachedAdaptiveClass中，在通过getAdaptieExtension
            //获取自适应拓展的时候，会用到。
            //Dubbo 目前只有两个扩展点使用类上Adaptive注解，Compiler 和 ExtensionFactory
            if (clazz.isAnnotationPresent((Class<? extends Annotation>) Adaptive.class)) {
                if (cachedAdaptiveClass == null) {
                    cachedAdaptiveClass = clazz;
                } else if (!cachedAdaptiveClass.equals(clazz)) {
                    throw new IllegalStateException("More than 1 adaptive class found: "
                            + cachedAdaptiveClass.getClass().getName()
                            + ", " + clazz.getClass().getName());
                }
            //检测 clazz 是否是 Wrapper 类型，检查其实就是看clazz是否有一个以type为参数的构造。
            //如果有，则说明此clazz为Wrapper类型的；如果没有，则说明此clazz不为Wrapper类型
            } else if (isWrapperClass(clazz)) {
                Set<Class<?>> wrappers = cachedWrapperClasses;
                if (wrappers == null) {
                    cachedWrapperClasses = new ConcurrentHashSet<Class<?>>();
                    wrappers = cachedWrapperClasses;
                }
                //存储 clazz 到 cachedWrapperClasses 缓存中
                wrappers.add(clazz);
            //程序进入此分支，表明 clazz 是一个普通的拓展类
            } else {
                //检测 clazz 是否有默认的构造方法，如果没有，则抛出异常
                clazz.getConstructor();
                if (name == null || name.length() == 0) {
                    //如果 name 为空，则尝试从 Extension 注解中获取 name，或使用小写的类名作为 name
                    name = findAnnotationName(clazz);
                    if (name == null || name.length() == 0) {
                        if (clazz.getSimpleName().length() > type.getSimpleName().length()
                                && clazz.getSimpleName().endsWith(type.getSimpleName())) {
                            name = clazz.getSimpleName().substring(0, clazz.getSimpleName().length() - type.getSimpleName().length()).toLowerCase();
                        } else {
                            throw new IllegalStateException("No such extension name for the class " + clazz.getName() + " in the config " + resourceURL);
                        }
                    }
                }
                String[] names = NAME_SEPARATOR.split(name);
                if (names != null && names.length > 0) {
                    Activate activate = clazz.getAnnotation(Activate.class);
                    if (activate != null) {
                        //如果类上有 Activate 注解，则使用 names 数组的第一个元素作为键，
                        //存储 name 到 Activate 注解对象的映射关系
                        cachedActivates.put(names[0], activate);
                    }
                    for (String n : names) {
                        if (!cachedNames.containsKey(clazz)) {
                            //存储 Class 到名称的映射关系
                            cachedNames.put(clazz, n);
                        }
                        Class<?> c = extensionClasses.get(n);
                        if (c == null) {
                            //存储名称到 Class 的映射关系
                            extensionClasses.put(n, clazz);
                        } else if (c != clazz) {
                            throw new IllegalStateException("Duplicate extension " + type.getName() + " name " + n + " on " + c.getName() + " and " + clazz.getName());
                        }
                    }
                }
            }
        }

        private boolean isWrapperClass(Class<?> clazz) {
            try {
                clazz.getConstructor(type);
                return true;
            } catch (NoSuchMethodException e) {
                return false;
            }
        }

        //Dubbo IOC 是通过 setter 方法注入依赖。Dubbo 首先会通过反射获取到实例的所有方法，然后再遍历方法列表，检测方法名是否具有 setter 方法特征。
        //若有，则通过 ObjectFactory 获取依赖对象，最后通过反射调用 setter 方法将依赖设置到目标对象中。
        //
        //objectFactory 变量的类型为 AdaptiveExtensionFactory，AdaptiveExtensionFactory 内部维护了一个 ExtensionFactory 列表，
        //用于存储其他类型的 ExtensionFactory。Dubbo 目前提供了两种 ExtensionFactory，分别是 SpiExtensionFactory 和 SpringExtensionFactory。
        //前者用于创建自适应的拓展，后者是用于从 Spring 的 IOC 容器中获取所需的拓展。
        private T injectExtension(T instance) {
            try {
                if (objectFactory != null) {
                    //遍历目标类的所有方法
                    for (Method method : instance.getClass().getMethods()) {
                        //检测方法是否以 set 开头，且方法仅有一个参数，且方法访问级别为 public
                        if (method.getName().startsWith("set")
                                && method.getParameterTypes().length == 1
                                && Modifier.isPublic(method.getModifiers())) {
                            Class<?> pt = method.getParameterTypes()[0];
                            try {
                                //获取属性名，比如 setName 方法对应属性名 name
                                String property = method.getName().length() > 3 ?
                                    method.getName().substring(3, 4).toLowerCase() + method.getName().substring(4) : "";
                                //从 ObjectFactory 中获取依赖对象
                                Object object = objectFactory.getExtension(pt, property);
                                if (object != null) {
                                    //通过反射调用 setter 方法设置依赖
                                    method.invoke(instance, object);
                                }
                            } catch (Exception e) {
                                logger.error("fail to inject via method " + method.getName()
                                        + " of interface " + type.getName() + ": " + e.getMessage(), e);
                            }
                        }
                    }
                }
            } catch (Exception e) {
                logger.error(e.getMessage(), e);
            }
            return instance;
        }


    }

    @SPI
    public interface ExtensionFactory {
        <T> T getExtension(Class<T> type, String name);
    }

    /**
     * AdaptiveExtensionLoader类有@Adaptive注解，如果扩展类上面有@Adaptive注解，会使用该类作为自适应类。
     * 在ExtensionLoader方法的构造函数中，ExtensionLoader.getExtensionLoader(ExtensionFactory.class).getAdaptiveExtension())
     * 会返回一个AdaptiveExtensionFactory对象，作为自适应扩展实例。
     */
    @Adaptive
    public class AdaptiveExtensionFactory implements ExtensionFactory {

        private final List<ExtensionFactory> factories;

        public AdaptiveExtensionFactory() {
            ExtensionLoader<ExtensionFactory> loader = ExtensionLoader.getExtensionLoader(ExtensionFactory.class);
            List<ExtensionFactory> list = new ArrayList<ExtensionFactory>();
            for (String name : loader.getSupportedExtensions()) {
                list.add(loader.getExtension(name));
            }
            factories = Collections.unmodifiableList(list);
        }

        /**
         * AdaptiveExtensionLoader会遍历所有的ExtensionFactory实现，尝试着去加载扩展。如果找到了，返回。如果没有，在下一个ExtensionFactory中继续找。
         * Dubbo内置了两个ExtensionFactory，分别从Dubbo自身的扩展机制（SpiExtensionFactory）和Spring容器中去寻找（SpringExtensionFactory）。
         * 由于ExtensionFactory本身也是一个扩展点，我们可以实现自己的ExtensionFactory，让Dubbo的自动装配支持我们自定义的组件。
         */
        public <T> T getExtension(Class<T> type, String name) {
            for (ExtensionFactory factory : factories) {
                T extension = factory.getExtension(type, name);
                if (extension != null) {
                    return extension;
                }
                return null;
            }
        }
    }

    public class SpiExtensionFactory implements ExtensionFactory {

        public <T> T getExtension(Class<T> type, String name) {
            if (type.isInterface() && type.isAnnotationPresent(SPI.class)) {
                ExtensionLoader<T> loader = ExtensionLoader.getExtensionLoader(type);
                if (loader.getSupportedExtensions().size() > 0) {
                    return loader.getAdaptiveExtension();
                }
            }
            return null;
        }
    
    }

    public class SpringExtensionFactory implements ExtensionFactory {

        private static final Set<ApplicationContext> contexts = new ConcurrentHashSet<ApplicationContext>();
    
        public static void addApplicationContext(ApplicationContext context) {
            contexts.add(context);
        }
    
        public static void removeApplicationContext(ApplicationContext context) {
            contexts.remove(context);
        }
    
        @SuppressWarnings("unchecked")
        public <T> T getExtension(Class<T> type, String name) {
            for (ApplicationContext context : contexts) {
                if (context.containsBean(name)) {
                    Object bean = context.getBean(name);
                    if (type.isInstance(bean)) {
                        return (T) bean;
                    }
                }
            }
            return null;
        }
    
    }

}