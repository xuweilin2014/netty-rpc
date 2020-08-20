package com.xu.rpc.core.extension;

import java.lang.annotation.*;

@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface Extension {

    // 默认扩展的名称
    String value() default "";

}
