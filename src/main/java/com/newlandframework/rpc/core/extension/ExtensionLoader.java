package com.newlandframework.rpc.core.extension;


import org.apache.log4j.Logger;


import java.io.*;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public final class ExtensionLoader<T> {

    private static final Logger logger = Logger.getLogger(ExtensionLoader.class);

    private static final Map<String, Class<?>> extensionClasses = new ConcurrentHashMap<>();

    private static final Map<Class<?>, ExtensionLoader<?>> extensionLoaders = new ConcurrentHashMap<>();

    private static final Map<String, Object> extensions = new HashMap<>();

    private String defaultName;

    private Class<?> type;

    private static final String SERVICES_DIRECTORY = "META-INF/services/";

    public ExtensionLoader(Class<?> type) {
        this.type = type;
    }

    @SuppressWarnings("unchecked")
    public static <T> ExtensionLoader<T> getExtensionLoader(Class<T> type){
        if (type == null) {
            logger.warn("Extensionloader type cannot be null!");
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
        return (T) extensions.get(name);
    }

    public void loadResource(){
        loadFile(SERVICES_DIRECTORY);
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
                String value = line.split("=")[1];
                copyValue = value;
                Class<?> extClass = Class.forName(value);

                if (!type.isAssignableFrom(extClass)){
                    throw new IllegalStateException("extension " + extClass.getName() + " is not a subtype of " + type.getName());
                }

                // 获取默认的扩展名
                Extension extAnnotation = type.getAnnotation(Extension.class);
                if (extAnnotation != null){
                    defaultName = extAnnotation.value();
                }

                Object ext = extClass.newInstance();
                extensions.put(key, ext);
                extensionClasses.put(key, extClass);

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

}
