package com.xu.rpc.jmx;

import java.util.List;

/**
 * MBean与MXBean的区别是：
 * 1.定义MBean接口实现类的属性时，MBean只支持Java的八种基本数据和一些简单的引用类型，如String。
 * 不能支持复杂数据类型的关联映射。而MXBean接口实现类定义属性时，支持基本数据类型，引用数据类型，和自定义的数据类型。
 * 若是在MBean中使用自定义数据类型的话，经过JConsole查看时会显示不可用。 当MXBean中使用被转换成CompositeDataSupport类
 *
 * 2.MXBean的接口与Class的命名没有约束条件
 */
public interface MetricsVisitorMXBean {

    //此方法表明ModuleMetricsHandler中的有属性ModuleMetricsVisitor，当通过
    //getAttribute(name, "ModuleMetricsVisitor")来从名为name的object中获取ModuleMetricsVisitor属性时，就会调用这个方法。
    //由于返回的ModuleMetricsVisitor类型是用户自定义的，因此实际获取到的类型为CompositeData[]。
    //通过返回的ModuleMetricsVisitor的List的集合值，就可以通过JConsole来查询方法的具体调用情况
    List<MetricsVisitor> getModuleMetricsVisitor();

    //此方法表明ModuleMetricsHandler有操作addModuleMetricsVisitor
    void addModuleMetricsVisitor(MetricsVisitor visitor);
}

