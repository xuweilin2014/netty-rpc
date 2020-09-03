package com.xu.rpc.commons.cache;

import com.xu.rpc.commons.URL;

public interface CacheFactory<K, V> {

    Cache<K, V> createCache(URL url);

}
