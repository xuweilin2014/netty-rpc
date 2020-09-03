package com.xu.rpc.commons.cache.lru;

import com.xu.rpc.commons.URL;
import com.xu.rpc.commons.cache.Cache;
import com.xu.rpc.commons.cache.CacheFactory;

public class LruCacheFactory implements CacheFactory {

    public static final String NAME = "lru";

    @Override
    public Cache createCache(URL url) {
        return new DefaultLruSegmentCache(url);
    }

}
