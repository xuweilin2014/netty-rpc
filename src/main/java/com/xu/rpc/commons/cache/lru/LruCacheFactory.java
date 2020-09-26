package com.xu.rpc.commons.cache.lru;

import com.xu.rpc.commons.URL;
import com.xu.rpc.commons.cache.Cache;
import com.xu.rpc.commons.cache.CacheFactory;
import com.xu.rpc.core.RpcConfig;
import org.apache.commons.lang3.StringUtils;

public class LruCacheFactory implements CacheFactory {

    public static final String NAME = "lru";

    @Override
    public Cache createCache(URL url) {
        return new DefaultSegmentLruCache(url);
    }

}
