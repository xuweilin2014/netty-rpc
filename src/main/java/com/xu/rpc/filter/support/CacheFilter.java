package com.xu.rpc.filter.support;

import com.alibaba.fastjson.JSON;
import com.xu.rpc.commons.cache.Cache;
import com.xu.rpc.commons.cache.CacheFactory;
import com.xu.rpc.commons.cache.lru.DefaultLruSegmentCache;
import com.xu.rpc.commons.util.AdaptiveExtensionUtil;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;
import com.xu.rpc.core.extension.Activate;
import com.xu.rpc.core.extension.ExtensionLoader;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.filter.ChainFilter;
import com.xu.rpc.protocol.Invoker;
import org.apache.log4j.Logger;
import java.util.Date;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@Activate(group = {RpcConfig.PROVIDER, RpcConfig.CONSUMER}, value = RpcConfig.CACHE_KEY, order = 2)
public class CacheFilter implements ChainFilter {

    private static final Map<String, Cache<Object, Object>> caches = new ConcurrentHashMap<>();

    private static final Logger logger = Logger.getLogger(CacheFilter.class);

    @Override
    public RpcResult intercept(Invoker invoker, RpcInvocation invocation) throws RpcException {
        String cache = invoker.getUrl().getParameter(RpcConfig.CACHE_KEY);
        // 判断用户是否开启缓存
        if (cache != null && cache.length() > 0){
            // 判断缓存的类型是否支持，现在内置的缓存类型为 lru、threadlocal
            if (ExtensionLoader.getExtensionLoader(CacheFactory.class).hasExtension(cache)){
                Cache<Object, Object> segmentCache = caches.get(cache);
                if (segmentCache == null){
                    // 根据 cache 的类型获取到对应的工厂类，从而创建缓存对象
                    CacheFactory<Object, Object> cacheFactory = AdaptiveExtensionUtil.getCacheFactory(invoker.getUrl());
                    caches.put(cache, cacheFactory.createCache(invoker.getUrl()));
                    segmentCache = caches.get(cache);
                }

                String cacheKey = genCacheKey(invocation);
                Object value = segmentCache.get(cacheKey);
                if (value != null){
                    return new RpcResult(value);
                }

                RpcResult result = invoker.invoke(invocation);
                if (result.getException() == null){
                    segmentCache.put(cacheKey, result.getResult());
                }

                return result;
            }else {
                logger.error("cache type " + cache + " not supported yet.");
            }
        }
        return invoker.invoke(invocation);
    }

    // 生成缓存的键，格式为：接口名,方法名,各参数值(使用逗号进行分隔)
    // 参数如果是原始类型或者原始类型的数组，就直接添加到键中，如果是其它类型，将其转换为 json 格式的字符串，然后添加到键中
    private String genCacheKey(RpcInvocation invocation){
        StringBuilder prefix = new StringBuilder();
        // 添加接口名和方法名
        prefix.append(invocation.getServiceType().getName()).append(RpcConfig.CACHE_KEY_SEPARATOR)
                .append(invocation.getMethodName());
        StringBuilder args = new StringBuilder();
        // 添加各参数的值
        for (Object arg : invocation.getParameters()) {
            if (args.length() > 0){
                args.append(RpcConfig.CACHE_CAPACITY_KEY);
            }
            if (arg == null || isPrimitive(arg.getClass())){
                args.append(arg);
            }else{
                try {
                    args.append(JSON.toJSONString(arg));
                } catch (Throwable e) {
                    logger.warn(e.getMessage());
                    args.append(arg);
                }
            }
        }

        return prefix.append(RpcConfig.CACHE_CAPACITY_KEY).append(args).toString();
    }

    private boolean isPrimitive(Class<?> cls) {
        if (cls.isArray()){
            return isPrimitive(cls.getComponentType());
        }

        return cls.isPrimitive() || cls == String.class || cls == Boolean.class || cls == Character.class
                || Number.class.isAssignableFrom(cls) || Date.class.isAssignableFrom(cls);
    }

}
