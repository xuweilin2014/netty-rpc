package com.newlandframework.rpc.core.extension;

import java.lang.annotation.*;

@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface Activate {

    String[] group() default {};

}
