package com.newlandframework.rpc.filter.support;

import com.newlandframework.rpc.filter.Filter;
import org.apache.commons.lang3.StringUtils;
import java.lang.reflect.Method;


public class SimpleFilter implements Filter {
    @Override
    public boolean before(Method method, Object classObject, Object[] parameters) {
        System.out.println(StringUtils.center("[SimpleFilter##before]", 48, "*"));
        return true;
    }

    @Override
    public void after(Method method, Object classObject, Object[] parameters) {
        System.out.println(StringUtils.center("[SimpleFilter##after]", 48, "*"));
    }
}

