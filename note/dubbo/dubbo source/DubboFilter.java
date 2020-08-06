public class DubboFilter {
    
    /**
     * 在Dubbo的整体设计中，Filter是一个很重要的概念，包括Dubbo本身的大多数功能，都是基于此扩展点实现的，在每次的调用过程中，Filter的拦截都会被执行。
     */

    /**
     * 如果我们定义的Filter有Activate注解，那么就可以不需要在XML文件中进行定义，这时自定义的Filter会被当成是默认的filter
     */

    @SPI
    public interface Filter {

        /**
         * do invoke filter.
         * // before filter
         * Result result = invoker.invoke(invocation);
         * // after filter
         * return result;
         */
        Result invoke(Invoker<?> invoker, Invocation invocation) throws RpcException;

    }
    
    @Activate(group = { Constants.CONSUMER })
    public class FilterOne implements Filter {

        @Override
        public Result invoke(Invoker<?> invoker, Invocation invocation) throws RpcException {
            System.out.println("before filter one");
            Result result = invoker.invoke(invocation);
            System.out.println("after filter one");
            return result;
        }
    }

    public class FilterThree implements Filter {
        @Override
        public Result invoke(Invoker<?> invoker, Invocation invocation) throws RpcException {
            System.out.println("before filter three");
            Result result = invoker.invoke(invocation);
            System.out.println("after filter three");
            return result;
        }
    }

    public static class ProtocolFilterWrapper implements Protocol {

        public <T> Exporter<T> export(Invoker<T> invoker) throws RpcException {
            if (Constants.REGISTRY_PROTOCOL.equals(invoker.getUrl().getProtocol())) {
                return protocol.export(invoker);
            }
            return protocol.export(buildInvokerChain(invoker, Constants.SERVICE_FILTER_KEY, Constants.PROVIDER));
        }
    
        public <T> Invoker<T> refer(Class<T> type, URL url) throws RpcException {
            if (Constants.REGISTRY_PROTOCOL.equals(url.getProtocol())) {
                return protocol.refer(type, url);
            }
            return buildInvokerChain(protocol.refer(type, url), Constants.REFERENCE_FILTER_KEY, Constants.CONSUMER);
        }

        /**
         * 在buildInvokerChain中,先获取所有已经激活的调用链，这里的调用链是已经排好序的。再通过Invoker来构造出一个Filter的调用链，
         * 最后构建出的调用链大致可以表示为：Filter1->Filter2->Filter3->......->Invoker,
         */
        private static <T> Invoker<T> buildInvokerChain(final Invoker<T> invoker, String key, String group) {
            Invoker<T> last = invoker;
            List<Filter> filters = ExtensionLoader.getExtensionLoader(Filter.class).getActivateExtension(invoker.getUrl(), key, group);
            if (filters.size() > 0) {
                for (int i = filters.size() - 1; i >= 0; i--) {
                    final Filter filter = filters.get(i);
                    final Invoker<T> next = last;
                    last = new Invoker<T>() {
    
                        public Class<T> getInterface() {
                            return invoker.getInterface();
                        }
    
                        public URL getUrl() {
                            return invoker.getUrl();
                        }
    
                        public boolean isAvailable() {
                            return invoker.isAvailable();
                        }
    
                        public Result invoke(Invocation invocation) throws RpcException {
                            return filter.invoke(next, invocation);
                        }
    
                        public void destroy() {
                            invoker.destroy();
                        }
    
                        @Override
                        public String toString() {
                            return invoker.toString();
                        }
                    };
                }
            }
            return last;
        }

    }

    public static class ConfigUtils{

        public static boolean isNotEmpty(String value) {
            return !isEmpty(value);
        }
    
        public static boolean isEmpty(String value) {
            return value == null || value.length() == 0
                    || "false".equalsIgnoreCase(value)
                    || "0".equalsIgnoreCase(value)
                    || "null".equalsIgnoreCase(value)
                    || "N/A".equalsIgnoreCase(value);
        }

    }

    public class ExtensionLoader<T> {

        public List<T> getActivateExtension(URL url, String[] values, String group) {
            List<T> exts = new ArrayList<T>();
            List<String> names = values == null ? new ArrayList<String>(0) : Arrays.asList(values);

            /**
             * 用户自定义 filter 默认在内置 filter 之后，内置的 filter 在配置中由特殊值 default 来指代，
             * 特殊符号 - 表示剔除，比如：filter="-default"，剔除添加所有默认的过滤器 filter，
             * 最开始是先处理内置的filter，再处理用户自定义的filter，因此一开始就要检测names中是否包含-default，
             * 如果包含了，就说明要剔除掉所有内置的filter。
             * 
             * 这里解释一下内置的filter，所谓内置filter，其实就是添加了Activate注解的filter。我们自己定义好filter
             * 后，有两种配置方式，一种是添加Activate注解，这样我们自定义的filter也会被dubbo当成是一个内置的filter
             * 会和dubbo中自带的filter一起进行处理；第二种是直接在XML文件中进行配置，这样我们自定义的filter就会被当成
             * 用户自定义的filter
             */
            if (!names.contains(Constants.REMOVE_VALUE_PREFIX + Constants.DEFAULT_KEY)) {
                // 最终会调用到下面的loadResource以及loadClass方法，不断循环从配置文件中读取行，比如：
                // filter1=com.dubbo.filter.mfilter.FilterOne
                // filter2=com.dubbo.filter.mfilter.FilterTwo
                // 如果=右边的实现类上注释了Activate注解的话，就将其添加到缓存 cachedActivates 中
                getExtensionClasses();
                for (Map.Entry<String, Activate> entry : cachedActivates.entrySet()) {
                    // name指的是SPI读取的配置文件的key
                    String name = entry.getKey();
                    Activate activate = entry.getValue();
                    // group主要是区分 filter 是在provider端生效还是consumer端生效，有的filter可能在provider和consumer两端都能生效
                    if (isMatchGroup(group, activate.group())) {
                        T ext = getExtension(name);
                        // 下面3个判断条件的含义依次如下：
                        // 1.用户在xml配置的filter列表中不包含当前ext，举例来说，我们自己定义了一个filter1=com.dubbo.filter.mfilter.FilterOne，
                        // 但是这个filter1既在XML文件中进行配置（因此names中包含filter1），也添加了Activate注解，那么就不会被当做内置的filter，
                        // 还是会被当做是自定义的filter
                        //
                        // 2.用户配置的filter列表中不包含当前ext的加-的key
                        //
                        // 3.如果用户的配置信息（url中体现）中有可以激活的配置key并且数据不为0,false,null，N/A，也就是说有正常的使用。比如
                        // 如果是dubbo自带的CacheFilter，如果url中没有配置cache参数，或者说cache参数的值为false，那么即使前面两个条件都满足，还是
                        // 不会被添加到exts集合中。
                        if (!names.contains(name)
                                && !names.contains(Constants.REMOVE_VALUE_PREFIX + name)
                                && isActive(activate, url)) {
                            exts.add(ext);
                        }
                    }
                }

                // 根据 @Activate 注解上的order排序
                Collections.sort(exts, ActivateComparator.COMPARATOR);
            }

            // 进行到此步骤的时候内置的filter（即带有@Activate注解）已经被添加完毕了，下面处理用户自己定义的filter，或者说
            // 用户定义的并且没有带@Activate注解的filter
            List<T> usrs = new ArrayList<T>();
            for (int i = 0; i < names.size(); i++) {
                String name = names.get(i);
                // 如果单个name不是以-开头并且所有的key里面并不包含-'name'（也就是说如果配置成了"dubbo,-dubbo"这种的可以，这个if是进不去的）
                if (!name.startsWith(Constants.REMOVE_VALUE_PREFIX)
                        && !names.contains(Constants.REMOVE_VALUE_PREFIX + name)) {
                    // 可以通过default关键字替换Dubbo原生的Filter链，主要用来控制调用链顺序
                    if (Constants.DEFAULT_KEY.equals(name)) {
                        if (usrs.size() > 0) {
                            exts.addAll(0, usrs);
                            usrs.clear();
                        }
                    } else {
                        // 加入用户自己定义的扩展Filter
                        T ext = getExtension(name);
                        usrs.add(ext);
                    }
                }
            }
            if (usrs.size() > 0) {
                exts.addAll(usrs);
            }
            return exts;
        }

        /**
         * 从@Activate注解中获取value值，并且作为key到url中去寻找是否有对应的参数，以及参数的值
         * 是否为true。
         * 
         * 比如CacheFilter的@Activate注解中，value的值为cache，接下来到url中判断url是否有cache这个参数，以及
         * 参数的值是否为true，如果为true，则表明出于激活状态
         */
        private boolean isActive(Activate activate, URL url) {
            String[] keys = activate.value();
            if (keys == null || keys.length == 0) {
                return true;
            }
            for (String key : keys) {
                for (Map.Entry<String, String> entry : url.getParameters().entrySet()) {
                    String k = entry.getKey();
                    String v = entry.getValue();
                    if ((k.equals(key) || k.endsWith("." + key))
                            && ConfigUtils.isNotEmpty(v)) {
                        return true;
                    }
                }
            }
            return false;
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
            //检测目标类上是否有 Adaptive 注解，如果带有@Adaptive注解的话，就会缓存到cachedAdaptiveClass中，在通过getAdaptieExtension获取自适应拓展的时候会用到。
            if (clazz.isAnnotationPresent((Class<? extends Annotation>) Adaptive.class)) {

                // 代码省略.......

            //检测 clazz 是否是 Wrapper 类型，检查其实就是看clazz是否有一个以type为参数的构造。
            //如果有，则说明此clazz为Wrapper类型的；如果没有，则说明此clazz不为Wrapper类型
            } else if (isWrapperClass(clazz)) {

                // 代码省略......

            //程序进入此分支，表明 clazz 是一个普通的拓展类
            } else {
                //检测 clazz 是否有默认的构造方法，如果没有，则抛出异常
                clazz.getConstructor();

                // 代码省略..........

                // 如果Filter接口实现类上有 Activate 注解，则使用 names 数组的第一个元素作为键，存储 name 到 Activate 注解对象的映射关系
                // 如果Filter接口实现类上如果没有，则不会添加到 cachedActivates 中
                String[] names = NAME_SEPARATOR.split(name);
                if (names != null && names.length > 0) {
                    Activate activate = clazz.getAnnotation(Activate.class);
                    if (activate != null) {
                        cachedActivates.put(names[0], activate);
                    }
                    
                    // 代码省略........
                }
            }
        }

    }



}