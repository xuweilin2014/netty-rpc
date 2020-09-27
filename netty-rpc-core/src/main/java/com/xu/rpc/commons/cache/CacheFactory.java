package com.xu.rpc.commons.cache;

import com.xu.rpc.commons.cache.lru.LruCacheFactory;
import com.xu.rpc.commons.URL;
import com.xu.rpc.core.extension.Extension;

@Extension(LruCacheFactory.NAME)
public interface CacheFactory<K, V> {

    Cache<K, V> createCache(URL url);

}
