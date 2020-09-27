package com.xu.rpc.commons;

import org.apache.commons.lang3.StringUtils;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class DateStore {

    private static final DateStore INSTANCE = new DateStore();

    private static final Map<String, Object> store = new ConcurrentHashMap<>();

    public static DateStore getINSTANCE() {
        return INSTANCE;
    }

    public static void put(String key, Object obj){
        if (StringUtils.isEmpty(key) || obj == null)
            return;

        // double check
        // 保证不会重复添加同样键值的对象
        if (!store.containsKey(key)){
            synchronized (key.intern()) {
                if (!store.containsKey(key)) {
                    store.put(key, obj);
                }
            }
        }
    }

    public static Object get(String key){
        if (StringUtils.isEmpty(key))
            return null;

        return store.get(key);
    }
}
