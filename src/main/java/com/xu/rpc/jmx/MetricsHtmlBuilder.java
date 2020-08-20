package com.xu.rpc.jmx;

import org.apache.log4j.Logger;

import javax.management.*;
import javax.management.openmbean.CompositeData;
import java.util.concurrent.TimeUnit;

/**
 * 被RPC服务器中内置的Http服务器调用，用来获取方法被调用的具体数据
 */
public class MetricsHtmlBuilder {

    private static final MetricsHtmlBuilder INSTANCE = new MetricsHtmlBuilder();

    private static final Logger logger = Logger.getLogger(MetricsHtmlBuilder.class);

    private static MetricsServer metricsServer;

    private static final String JMX_METRICS_ATTR = "ModuleMetricsVisitor";

    private static final String TD_BEGIN = "<td>";

    private static final String TD_END = "</td>";

    private static final String TR_BEGIN = "<tr>";

    private static final String TR_END = "</tr>";

    private static final String TABLE_BEGIN = "<html><body><div class=\"table-container\"><table border=\"1\"><tr><th>服务名称</th><th>方法名称</th>" +
            "<th>调用次数</th><th>调用成功次数</th><th>调用失败次数</th><th>被过滤次数</th><th>方法平均耗时（毫秒）</th><th>方法最大耗时（毫秒）</th>" +
            "<th>方法最小耗时（毫秒）</th><th>最后一次失败时间</th><th>最后一次失败堆栈明细</th></tr>";

    private static final String TABLE_END = "</table></body></html>";

    // 利用饿汉模式来实现单例模式
    public static MetricsHtmlBuilder getInstance() {
        return INSTANCE;
    }

    private MetricsHtmlBuilder() {
    }

    // 获取到与已经启动的 JMXConnectorServer 的连接connection
    private MBeanServerConnection connect() {
        if (metricsServer == null){
            throw new IllegalStateException("jmx server does not exist.");
        }

        MBeanServerConnection connection = metricsServer.connect();
        while (true) {
            if (connection != null) {
                break;
            } else {
                try {
                    TimeUnit.SECONDS.sleep(1L);
                    connection = metricsServer.connect();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
        }

        return connection;
    }

    public String buildMetrics() {
        StringBuilder metrics = new StringBuilder();
        metrics.append(TABLE_BEGIN);

        MBeanServerConnection connection = connect();
        metrics.append(getMetrics(connection));

        metrics.append(TABLE_END);
        return metrics.toString();
    }

    public String getMetrics(MBeanServerConnection connection){
        ObjectName name = null;
        try {
            name = new ObjectName(MetricsServer.MBEAN_NAME);
        } catch (MalformedObjectNameException e) {
            e.printStackTrace();
        }

        StringBuilder metrics = new StringBuilder();

        /*
         * JMX_METRICS_ATTR即为"ModuleMetricsVisitor"，由于ModuleMetricsHandler实现了ModuleMetricsVisitorMXBean接口，
         * 在此接口中有getModuleMetricsVisitor方法，表明ModuleMetricsHandler这个MXBean中有ModuleMetricsVisitor属性。
         *
         * 因此，调用getAttribute时指明JMX_METRICS_ATTR，就会调用ModuleMetricsHandler（实际上是它的父类）中的getModuleMetricsVisitor方法
         * 来获取ModuleMetricsVisitor（里面保存了一个特定方法调用相关的监控数据）。这里获取到的实际上是ModuleMetricsVisitor的一个list集合。
         * 遍历这个list集合，获取每一个visitor中相关的数据，并且转换成HTML的格式返回。
         */

        Object obj = null;
        try {
            obj = connection.getAttribute(name, JMX_METRICS_ATTR);
        } catch (Throwable e) {
            logger.error("error occurs in getting ModuleMetricsVisitor");
        }

        if (obj instanceof CompositeData[]) {
            for (CompositeData compositeData : (CompositeData[]) obj) {
                CompositeData data = compositeData;
                String moduleName = (String) (data.get("className"));
                String methodName = (String) (data.get("methodName"));
                long invokeCount = (Long) (data.get("invokeCount"));
                long invokeSuccCount = (Long) (data.get("invokeSuccCount"));
                long invokeFailCount = (Long) (data.get("invokeFailCount"));
                long invokeFilterCount = (Long) (data.get("invokeFilterCount"));
                long accumulateTimespan = (Long) (data.get("accumulateTimespan"));
                long invokeMinTimespan = ((Long) (data.get("invokeMinTimespan")))
                        .equals(MetricsVisitor.DEFAULT_INVOKE_MIN_TIMESPAN) ?
                        Long.valueOf(0L) : (Long) (data.get("invokeMinTimespan"));
                long invokeMaxTimespan = (Long) (data.get("invokeMaxTimespan"));
                String lastStackTraceDetail = (String) (data.get("lastStackTraceDetail"));
                String lastErrorTime = (String) (data.get("lastErrorTime"));
                metrics.append(TR_BEGIN);
                metrics.append(TD_BEGIN).append(moduleName).append(TD_END);
                metrics.append(TD_BEGIN).append(methodName).append(TD_END);
                metrics.append(TD_BEGIN).append(invokeCount).append(TD_END);
                metrics.append(TD_BEGIN).append(invokeSuccCount).append(TD_END);
                metrics.append(TD_BEGIN).append(invokeFailCount).append(TD_END);
                metrics.append(TD_BEGIN).append(invokeFilterCount).append(TD_END);
                metrics.append(TD_BEGIN).append((double) (accumulateTimespan / invokeSuccCount)).append(TD_END);
                metrics.append(TD_BEGIN).append(invokeMaxTimespan).append(TD_END);
                metrics.append(TD_BEGIN).append(invokeMinTimespan).append(TD_END);
                metrics.append(TD_BEGIN).append(lastErrorTime != null ? lastErrorTime : "").append(TD_END);
                metrics.append(TD_BEGIN).append(lastStackTraceDetail).append(TD_END);
                metrics.append(TR_END);
            }
        }

        return metrics.toString();
    }

    public void setMetricsServer(MetricsServer metricsServer) {
        MetricsHtmlBuilder.metricsServer = metricsServer;
    }
}

