package com.newlandframework.rpc.jmx;

import java.util.List;

/**
 * MBean与MXBean的区别是：定义MBean接口实现类的属性时，MBean只支持Java的八种基本数据和一些简单的引用类型，如String。
 * 不能支持复杂数据类型的关联映射。而MXBean接口实现类定义属性时，支持基本数据类型，引用数据类型，和自定义的数据类型。
 */
public interface ModuleMetricsVisitorMXBean {

    //此方法表明ModuleMetricsHandler中的有属性ModuleMetricsVisitor，当通过
    //getAttribute(name, "ModuleMetricsVisitor")来获取ModuleMetricsVisitor属性时，就会调用这个方法。
    //由于返回的ModuleMetricsVisitor类型是用户自定义的，因此实际获取到的类型为CompositeData[]。
    List<ModuleMetricsVisitor> getModuleMetricsVisitor();

    //此方法表明ModuleMetricsHandler有操作addModuleMetricsVisitor
    void addModuleMetricsVisitor(ModuleMetricsVisitor visitor);
}

