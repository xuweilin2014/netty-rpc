package com.xu.rpc.core.extension;

import java.lang.annotation.*;

@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface Activate {
    // 过滤器用于服务端还是客户端
    String[] group() default {};

    // 过滤器的顺序
    int order() default 0;

    // url 上对应的键值
    String value() default "";

}
