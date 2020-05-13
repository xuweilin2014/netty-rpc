package com.newlandframework.rpc.jmx;

import javax.management.*;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeDataSupport;
import java.io.IOException;
import java.util.concurrent.TimeUnit;

/**
 * 被RPC服务器中内置的Http服务器调用，用来获取方法被调用的具体数据
 */
public class ModuleMetricsHtmlBuilder {
    private static final ModuleMetricsHtmlBuilder INSTANCE = new ModuleMetricsHtmlBuilder();
    private MBeanServerConnection connection;
    private final static String TD_BEGIN = "<td>";
    private final static String TD_END = "</td>";
    private final static String TR_BEGIN = "<tr>";
    private final static String TR_END = "</tr>";
    private final static String TABLE_BEGIN = "<html><body><div class=\"table-container\"><table border=\"1\"><tr><th>模块名称</th><th>方法名称</th>" +
            "<th>调用次数</th><th>调用成功次数</th><th>调用失败次数</th><th>被过滤次数</th><th>方法耗时（毫秒）</th><th>方法最大耗时（毫秒）</th>" +
            "<th>方法最小耗时（毫秒）</th><th>最后一次失败时间</th><th>最后一次失败堆栈明细</th></tr>";
    private final static String TABLE_END = "</table></body></html>";
    private final static String JMX_METRICS_ATTR = "ModuleMetricsVisitor";

    public static ModuleMetricsHtmlBuilder getInstance() {
        return INSTANCE;
    }

    private ModuleMetricsHtmlBuilder() {
        init();
    }

    // 获取到对启动的JMXConnectorServer的连接connection
    private void init() {
        ModuleMetricsHandler handler = ModuleMetricsHandler.getInstance();
        connection = handler.connect();

        while (true) {
            if (connection != null) {
                break;
            } else {
                try {
                    TimeUnit.SECONDS.sleep(1L);
                    connection = handler.connect();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
        }
    }

    public String buildModuleMetrics() {
        StringBuilder metrics = new StringBuilder();

        metrics.append(TABLE_BEGIN);
        ObjectName name = null;
        try {
            name = new ObjectName(ModuleMetricsHandler.MBEAN_NAME);
        } catch (MalformedObjectNameException e) {
            e.printStackTrace();
        }

        try {
            /*
             * JMX_METRICS_ATTR即为"ModuleMetricsVisitor"，由于ModuleMetricsHandler实现了ModuleMetricsVisitorMXBean接口，
             * 在此接口中有getModuleMetricsVisitor方法，表明ModuleMetricsHandler这个MXBean中有ModuleMetricsVisitor属性。
             *
             * 因此，调用getAttribute时指明JMX_METRICS_ATTR，就会调用ModuleMetricsHandler（实际上是它的父类）中的getModuleMetricsVisitor方法
             * 来获取ModuleMetricsVisitor（里面保存了一个方法调用相关的监控数据）。
             *
             * 这里获取到的实际上是ModuleMetricsVisitor的一个list集合。遍历这个list集合，获取每一个visitor中相关的数据，并且转换成HTML的格式返回。
             */
            Object obj = connection.getAttribute(name, JMX_METRICS_ATTR);
            if (obj instanceof CompositeData[]) {
                for (CompositeData compositeData : (CompositeData[]) obj) {
                    CompositeData data = compositeData;
                    String moduleName = (String) (data.get("className"));
                    String methodName = (String) (data.get("methodName"));
                    long invokeCount = (Long) (data.get("invokeCount"));
                    long invokeSuccCount = (Long) (data.get("invokeSuccCount"));
                    long invokeFailCount = (Long) (data.get("invokeFailCount"));
                    long invokeFilterCount = (Long) (data.get("invokeFilterCount"));
                    long invokeTimespan = (Long) (data.get("invokeTimespan"));
                    long invokeMinTimespan = ((Long) (data.get("invokeMinTimespan")))
                            .equals(ModuleMetricsVisitor.DEFAULT_INVOKE_MIN_TIMESPAN) ?
                            Long.valueOf(0L) : (Long) (data.get("invokeMinTimespan"));
                    long invokeMaxTimespan = (Long) (data.get("invokeMaxTimespan"));
                    String lastStackTraceDetail = (String) (data.get("lastStackTraceDetail"));
                    String lastErrorTime = (String) (data.get("lastErrorTime"));
                    metrics.append(TR_BEGIN);
                    metrics.append(TD_BEGIN + moduleName + TD_END);
                    metrics.append(TD_BEGIN + methodName + TD_END);
                    metrics.append(TD_BEGIN + invokeCount + TD_END);
                    metrics.append(TD_BEGIN + invokeSuccCount + TD_END);
                    metrics.append(TD_BEGIN + invokeFailCount + TD_END);
                    metrics.append(TD_BEGIN + invokeFilterCount + TD_END);
                    metrics.append(TD_BEGIN + invokeTimespan + TD_END);
                    metrics.append(TD_BEGIN + invokeMaxTimespan + TD_END);
                    metrics.append(TD_BEGIN + invokeMinTimespan + TD_END);
                    metrics.append(TD_BEGIN + (lastErrorTime != null ? lastErrorTime : "") + TD_END);
                    metrics.append(TD_BEGIN + lastStackTraceDetail + TD_END);
                    metrics.append(TR_END);
                }
            }
            metrics.append(TABLE_END);
        } catch (MBeanException | AttributeNotFoundException |
                InstanceNotFoundException | ReflectionException | IOException e) {
            e.printStackTrace();
        }

        return metrics.toString();
    }
}

