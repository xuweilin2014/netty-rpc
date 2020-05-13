package com.newlandframework.rpc.core;

/**
 * RPC服务器一些参数配置
 */
public class RpcSystemConfig {
    public static final String SYSTEM_PROPERTY_THREADPOOL_REJECTED_POLICY_ATTR = "nettyrpc.parallel.rejected.policy";
    public static final String SYSTEM_PROPERTY_THREADPOOL_QUEUE_NAME_ATTR = "nettyrpc.parallel.queue";
    public static final long SYSTEM_PROPERTY_MESSAGE_CALLBACK_TIMEOUT = Long.getLong("nettyrpc.default.msg.timeout", 30 * 1000L);
    public static final long SYSTEM_PROPERTY_ASYNC_MESSAGE_CALLBACK_TIMEOUT = Long.getLong("nettyrpc.default.asyncmsg.timeout", 60 * 1000L);
    public static final int SYSTEM_PROPERTY_THREADPOOL_THREAD_NUMS = Integer.getInteger("nettyrpc.default.thread.nums", 16);
    public static final int SYSTEM_PROPERTY_THREADPOOL_QUEUE_NUMS = Integer.getInteger("nettyrpc.default.queue.nums", -1);
    public static final int SYSTEM_PROPERTY_CLIENT_RECONNECT_DELAY = Integer.parseInt(System.getProperty("nettyrpc.default.client.reconnect.delay", "10"));
    public static final int SYSTEM_PROPERTY_PARALLEL = Math.max(2, Runtime.getRuntime().availableProcessors());
    public static final int SYSTEM_PROPERTY_JMX_INVOKE_METRICS = Integer.getInteger("nettyrpc.jmx.invoke.metrics", 1);
    public static final int SYSTEM_PROPERTY_JMX_METRICS_HASH_NUMS = Integer.getInteger("nettyrpc.jmx.metrics.hash.nums", 8);
    public static final int SYSTEM_PROPERTY_JMX_METRICS_LOCK_FAIR = Integer.getInteger("nettyrpc.jmx.metrics.lock.fair", 0);
    public static final boolean SYSTEM_PROPERTY_JMX_METRICS_HASH_SUPPORT = RpcSystemConfig.SYSTEM_PROPERTY_JMX_METRICS_HASH_NUMS != 1;
    public static final boolean SYSTEM_PROPERTY_JMX_METRICS_SUPPORT = RpcSystemConfig.SYSTEM_PROPERTY_JMX_INVOKE_METRICS != 0;
    public static final String DELIMITER = ":";
    public static final int IPADDR_OPRT_ARRAY_LENGTH = 2;
    public static final String RPC_COMPILER_SPI_ATTR = "com.newlandframework.rpc.compiler.AccessAdaptive";
    public static final String RPC_ABILITY_DETAIL_SPI_ATTR = "com.newlandframework.rpc.core.AbilityDetail";
    public static final String FILTER_RESPONSE_MSG = "Illegal request,NettyRPC server refused to respond!";
    public static final String TIMEOUT_RESPONSE_MSG = "Timeout request,NettyRPC server request timeout!";
    public static final int SERIALIZE_POOL_MAX_TOTAL = 500;
    public static final int SERIALIZE_POOL_MIN_IDLE = 10;
    public static final int SERIALIZE_POOL_MAX_WAIT_MILLIS = 5000;
    public static final int SERIALIZE_POOL_MIN_EVICTABLE_IDLE_TIME_MILLIS = 600000;

    private static boolean monitorServerSupport = false;

    public static boolean isMonitorServerSupport() {
        return monitorServerSupport;
    }

    public static void setMonitorServerSupport(boolean jmxSupport) {
        monitorServerSupport = jmxSupport;
    }
}

