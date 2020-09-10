package com.xu.rpc.commons;

public class Assert {

    public static void notNull(Object obj, String msg){
        if (obj == null)
            throw new IllegalArgumentException(msg);
    }

    public static void notNull(Object obj, RuntimeException exception) {
        if (obj == null)
            throw exception;
    }

}
