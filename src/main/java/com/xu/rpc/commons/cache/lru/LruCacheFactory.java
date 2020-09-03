package com.xu.rpc.commons.cache.lru;

import com.xu.rpc.commons.URL;
import com.xu.rpc.commons.cache.Cache;
import com.xu.rpc.commons.cache.CacheFactory;
import com.xu.rpc.commons.cache.lru.LruCache;
import com.xu.rpc.commons.cache.threadlocal.ThreadLocalCache;
import com.xu.rpc.core.RpcConfig;
import org.aspectj.weaver.tools.cache.CacheBacking;
import org.aspectj.weaver.tools.cache.CacheKeyResolver;

public class LruCacheFactory implements CacheFactory {

    @Override
    public Cache createCache(URL url) {
        int capacity = url.getParameter(RpcConfig.CACHE_CAPACITY_KEY, 100);
        return new LruCache(capacity);
    }
}
