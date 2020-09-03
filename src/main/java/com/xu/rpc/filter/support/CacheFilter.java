package com.xu.rpc.filter.support;

import com.xu.rpc.commons.cache.CacheFactory;
import com.xu.rpc.commons.cache.DefaultSegmentCache;
import com.xu.rpc.commons.cache.SegmentCache;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.extension.ExtensionLoader;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.filter.ChainFilter;
import com.xu.rpc.model.MessageRequest;
import com.xu.rpc.protocol.Invoker;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class CacheFilter implements ChainFilter {

    private static final Map<String, SegmentCache<Object, Object>> caches = new ConcurrentHashMap<>();

    @Override
    public Object intercept(Invoker invoker, MessageRequest request) throws RpcException {
        String cache = invoker.getUrl().getParameter(RpcConfig.CACHE_KEY);
        if (cache != null && cache.length() > 0){
            if (ExtensionLoader.getExtensionLoader(CacheFactory.class).hasExtension(cache)){
                SegmentCache<Object, Object> segmentCache = caches.get(cache);
                if (segmentCache == null){
                    caches.put(cache, new DefaultSegmentCache<>(invoker.getUrl()));
                    segmentCache = caches.get(cache);
                }
                segmentCache.put();
            }
        }
        return null;
    }

}
