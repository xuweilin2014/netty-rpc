package com.xu.rpc.core.extension;


import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.commons.URL;
import io.netty.util.internal.ConcurrentSet;
import org.apache.log4j.Logger;


import java.io.*;
import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;

public final class ExtensionLoader<T> {

    private static final Logger logger = Logger.getLogger(ExtensionLoader.class);

    private static final Map<String, Class<?>> extensionClasses = new ConcurrentHashMap<>();

    private static final Map<Class<?>, ExtensionLoader<?>> extensionLoaders = new ConcurrentHashMap<>();

    private static final Map<String, Object> extensions = new HashMap<>();

    private Map<String, Activate> cachedActivates = new ConcurrentHashMap<>();

    private Set<Class<?>> cachedWrapperClasses = new ConcurrentSet<>();

    private String defaultName;

    private Class<T> type;

    public ExtensionLoader(Class<T> type) {
        this.type = type;
    }

    @SuppressWarnings("unchecked")
    public static <T> ExtensionLoader<T> getExtensionLoader(Class<T> type){
        if (type == null) {
            logger.warn("ExtensionLoader type cannot be null!");
            throw new IllegalArgumentException("type cannot be null!");
        }

        if (!type.isInterface()){
            logger.warn(type.getName() + " is not an interface");
            throw new IllegalArgumentException(type.getName() + " is not an interface");
        }

        if (!type.isAnnotationPresent(Extension.class)){
            logger.warn(type.getName() + " does not have an extension annotation");
            throw new IllegalArgumentException(type.getName() + " does not have an extension annotation");
        }

        ExtensionLoader<?> loader = extensionLoaders.get(type);
        if (loader == null){
            // 多个线程同时创建添加 ExtensionLoader 时，只能有一个线程可以成功，其余的从 extensionLoaders 获取
            extensionLoaders.putIfAbsent(type, new ExtensionLoader<>(type));
            loader = extensionLoaders.get(type);
        }

        return (ExtensionLoader<T>) loader;
    }

    // 获取 name 对应的扩展，如果没有这个扩展，就返回 null
    @SuppressWarnings("unchecked")
    public T getExtension(String name){
        if (name == null || name.length() == 0)
            throw new IllegalArgumentException("extension name cannot be empty");

        T ext = (T) extensions.get(name);
        if (ext == null){
            synchronized (this){
                ext = (T) extensions.get(name);
                if (ext == null) {
                    extensions.putIfAbsent(name, createExtension(name));
                    ext = (T) extensions.get(name);
                }
            }
        }
        return ext;
    }

    public T getDefaultExtension(){
        loadResource();
        if (defaultName == null || defaultName.length() == 0){
            return null;
        }

        return getExtension(defaultName);
    }

    @SuppressWarnings("unchecked")
    private T createExtension(String name) {
        loadResource();
        Object instance = extensions.get(name);
        if (cachedWrapperClasses.size() > 0){
            for (Class<?> wrapper : cachedWrapperClasses) {
                try {
                    instance = wrapper.getConstructor(type).newInstance(instance);
                } catch (InstantiationException
                        | IllegalAccessException
                        | InvocationTargetException | NoSuchMethodException e) {
                    throw new IllegalStateException("cannot instantiate with wrapper class " + wrapper.getName());
                }
            }
        }

        return (T) instance;
    }

    public void loadResource(){
        loadFile(RpcConfig.SERVICES_DIRECTORY);
        loadFile(RpcConfig.RPC_INTERNAL_DIRECTORY);
    }

    public void loadFile(String dir){
        if (dir == null || "".equals(dir)){
            logger.warn(dir + " is empty path, cannot load extensions from it");
            throw new IllegalArgumentException(dir + " is empty path, cannot load extensions from it");
        }

        String dirPath = dir + type.getName();
        InputStream is = ExtensionLoader.class.getClassLoader().getResourceAsStream(dirPath);

        if (is == null){
            logger.warn(dir + " is invalid path, cannot load extensions from it");
            throw new IllegalArgumentException(dir + " is invalid path, cannot load extensions from it");
        }

        BufferedReader resource = new BufferedReader(new InputStreamReader(is));
        String copyValue = "";
        try{
            String line = resource.readLine();
            while (line != null){
                if (!line.contains("=")){
                    throw new IllegalStateException("there should be '=' in your extension file");
                }
                String key = line.split("=")[0];
                String className = line.split("=")[1];
                copyValue = className;
                Class<?> extClass =  Class.forName(className);

                if (!type.isAssignableFrom(extClass)){
                    throw new IllegalStateException("extension " + extClass.getName() + " is not a subtype of " + type.getName());
                }

                // 获取默认的扩展名
                Extension extAnnotation = type.getAnnotation(Extension.class);
                if (extAnnotation != null){
                    defaultName = extAnnotation.value();
                }

                try {
                    // 如果扩展类有 type 类型的构造函数的话，就看作是 wrapper 类，添加到缓存中
                    extClass.getConstructor(type);
                    cachedWrapperClasses.add(extClass);
                } catch (NoSuchMethodException e) {
                    // 判断 extClass 是否有 Activate 注解
                    Activate activate = extClass.getAnnotation(Activate.class);
                    if (activate != null){
                        cachedActivates.put(key, activate);
                    }
                    Object ext = extClass.newInstance();
                    extensions.put(key, ext);
                    extensionClasses.put(key, extClass);
                }

                line = resource.readLine();
            }
        }catch (IOException ex){
            logger.warn("load extensions failed, error occurs when reading extension file");
        }catch (ClassNotFoundException ex){
            logger.warn("extension class : " + copyValue + " is not found");
        } catch (IllegalAccessException | InstantiationException e) {
            logger.warn("load extensions failed, cannot instantiate extension");
        }

    }

    public boolean hasExtension(String name){
        if (name == null || name.length() == 0)
            throw new IllegalStateException("name == null.");

        try{
            return getExtension(name) != null;
        }catch (Throwable t){
            return false;
        }
    }

    public List<T> getExtensions() {
        return Collections.unmodifiableList((List<T>) extensions.values());
    }

    public List<T> getActivateExtension(URL url, String key, String group){
        String value = url.getParameter(key);
        String[] values = null;
        if (value != null && value.length() != 0){
            values = value.split(",");
        }
        return getActivateExtension(url, values, group);
    }

    public List<T> getActivateExtension(URL url, String[] values, String group){
        // 1.获取到 url 中参数的值，比如 url 中 filter 关键字的值
        List<String> names = values == null ? new ArrayList<>(0) : Arrays.asList(values);
        List<T> exts = new CopyOnWriteArrayList<>();

        // 2.获取含有 Activate 注释的扩展类对象
        if (!names.contains(RpcConfig.REMOVE_PREFIX + RpcConfig.RPC_DEFAULT)){
            for (Map.Entry<String, Activate> entry : cachedActivates.entrySet()) {
                String name = entry.getKey();
                Activate activate = entry.getValue();
                if (isGroupMatch(group, activate.group())){
                    if (!names.contains(RpcConfig.REMOVE_PREFIX + name) && isActive(url, activate)){
                        T ext = getExtension(name);
                        exts.add(ext);
                    }
                }
            }
        }

        // 3.对 exts 集合中的对象进行排序，order 值越大，排序就
        // 比如 MonitorChainFilter 一定要在 TimeoutChainFilter 后面调用
        exts.sort(ActivateComparator.COMPARATOR);

        // 3.获取用户配置的自定义的扩展对象
        List<T> users = new CopyOnWriteArrayList<>();
        for (String name : names) {
            if (!names.contains(RpcConfig.REMOVE_PREFIX + name)
                    && !name.contains(RpcConfig.REMOVE_PREFIX)){
                if (RpcConfig.RPC_DEFAULT.equals(name)){
                    if (users.size() > 0) {
                        exts.addAll(0, users);
                        users.clear();
                    }
                }else {
                    T ext = getExtension(name);
                    users.add(ext);
                }
            }
        }

        if (users.size() > 0)
            exts.addAll(users);

        return exts;
    }

    private boolean isGroupMatch(String group, String[] activateGroup) {
        if (group == null || group.length() == 0)
            return false;

        for (String ag : activateGroup) {
            if (ag.equals(group)){
                return true;
            }
        }

        return false;
    }

    private boolean isActive(URL url, Activate activate){
        String value = activate.value();
        
        if (value.length() == 0)
            return false;

        for (Map.Entry<String, String> entry : url.getParameters().entrySet()) {
            String key = entry.getKey();
            String val = entry.getValue();
            if (value.equals(key) && val != null
                    && val.length() != 0 && !RpcConfig.FALSE.equals(val)){
                return true;
            }
        }
        
        return false;
    }

}
