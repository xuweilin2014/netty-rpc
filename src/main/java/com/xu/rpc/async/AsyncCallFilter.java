package com.xu.rpc.async;

import net.sf.cglib.proxy.CallbackFilter;

import java.lang.reflect.Method;


public class AsyncCallFilter implements CallbackFilter {
    @Override
    public int accept(Method method) {

        /**
         *  getDeclaringClass返回的是此method是在哪个类中进行声明的，不过如果子类重写了父类的方法，
         *  或者一个类实现了一个接口中的方法，那么getDeclaringClass返回的是子类或者实现接口的类的名称。
         *
         *  在CGLIB动态代理中，调用enhancer.createClass()方法，会把代理类要实现的接口（通过setInterfaces设置），
         *  以及要代理的目标类（通过setSuperClass实现）包括其父类（也包括Object类）中的所有方法作为参数都传入
         *  此accept方法中。判断当代理类调用此方法时，应该使用的拦截器在数组中的下标，然后保存起来。
         *  下标为0：AsyncCallResultInterceptor
         *  下标为1：AsyncCallObjectInterceptor
         */
        return AsyncCallObject.class.isAssignableFrom(method.getDeclaringClass()) ? 1 : 0;
    }
}

