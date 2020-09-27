package com.xu.rpc.core.extension;

import java.lang.annotation.*;

/**
 * attribute注释表明类中的某一个成员是用户可以在xml中配置的属性
 */

@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Attribute {

    String defaultValue() default "";

    boolean excluded() default false;
}
