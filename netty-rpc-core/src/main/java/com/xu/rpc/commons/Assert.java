package com.xu.rpc.commons;

import org.apache.commons.lang3.StringUtils;

public class Assert {

    public static void notNull(Object obj, String msg){
        if (obj == null)
            throw new IllegalArgumentException(msg);
    }

    public static void notNull(Object obj, RuntimeException exception) {
        if (obj == null)
            throw exception;
    }

    public static void notEmpty(String string, String msg){
        if (StringUtils.isEmpty(string))
            throw new IllegalArgumentException(msg);
    }

}
