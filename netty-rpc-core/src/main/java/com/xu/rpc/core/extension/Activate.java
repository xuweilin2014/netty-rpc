package com.xu.rpc.core.extension;

import java.lang.annotation.*;

@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface Activate {

    String[] group() default {};

    int order() default 0;

    String value() default "";

}
