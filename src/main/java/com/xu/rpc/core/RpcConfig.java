package com.xu.rpc.core;

import org.apache.commons.lang3.StringUtils;

/**
 * RPC服务器一些参数配置
 */
public class RpcConfig {

    public static final String SYSTEM_PROPERTY_THREADPOOL_REJECTED_POLICY_ATTR = "nettyrpc.parallel.rejected.policy";

    public static final String SYSTEM_PROPERTY_THREADPOOL_QUEUE_NAME_ATTR = "nettyrpc.parallel.queue";

    public static final long SYSTEM_PROPERTY_MESSAGE_CALLBACK_TIMEOUT = Long.getLong("nettyrpc.default.msg.timeout", 30 * 1000L);

    public static final long SYSTEM_PROPERTY_ASYNC_MESSAGE_CALLBACK_TIMEOUT = Long.getLong("nettyrpc.default.asyncmsg.timeout", 60 * 1000L);

    public static final int SYSTEM_PROPERTY_THREADPOOL_THREAD_NUMS = Integer.getInteger("nettyrpc.default.thread.nums", 16);

    public static final int SYSTEM_PROPERTY_THREADPOOL_QUEUE_NUMS = Integer.getInteger("nettyrpc.default.queue.nums", -1);

    public static final int SYSTEM_PROPERTY_CLIENT_RECONNECT_DELAY = Integer.parseInt(System.getProperty("nettyrpc.default.client.reconnect.delay", "10"));

    public static final int SYSTEM_PROPERTY_PARALLEL = Math.max(2, Runtime.getRuntime().availableProcessors());

    public static final String DELIMITER = ":";

    public static final int IP_PORT_ARRAY_LENGTH = 2;

    public static final String TIMEOUT_RESPONSE_MSG = "Timeout request,NettyRPC server request timeout!";

    public static final int SERIALIZE_POOL_MAX_TOTAL = 500;

    public static final int SERIALIZE_POOL_MIN_IDLE = 10;

    public static final int SERIALIZE_POOL_MAX_WAIT_MILLIS = 5000;

    public static final int SERIALIZE_POOL_MIN_EVICTABLE_IDLE_TIME_MILLIS = 600000;

    public static final String SCOPE_LOCAL = "local";

    public static final String SCOPE_REMOTE = "remote";

    public static final String INJVM = "injvm";

    public static final String EXPORT_KEY = "export";

    public static final String REGISTRY_FACTORY = "registry";

    public static final int ZOOKEEPER_TIMEOUT = 3000;

    public static final String INTERFACE_KEY = "interface";

    public static final String ROOT_DIR = "/rpc";

    public static final String DIR_SEPARATOR = "/";

    public static final int RETRY_PERIOD = 5000;

    public static final String SERIALIZE = "serialize";

    public static final String JDK_SERIALIZE = "jdk";

    public static final String HOST = "host";

    public static final String PORT = "port";

    public static final String SERVICES_DIRECTORY = "META-INF/services/";

    public static final String RPC_INTERNAL_DIRECTORY = "META-INF/rpc/internal";

    public static final String REMOVE_PREFIX = "-";

    public static final String RPC_DEFAULT = "default";

    public static final String REGISTRY_PROTOCOL = "registry";

    public static final String FILTER = "filter";

    public static final String PROVIDER = "provider";

    public static final String CONSUMER = "consumer";

    public static final String LOCALHOST = "localhost";

    public static final String METRICS = "metrics";

    public static final int ECHO_PORT = 18882;

    public static final int METRICS_PORT = 9999;

    public static final String METRICS_PORT_KEY = "metrics.port";

    public static final String MONITOR = "monitor";

    public static final String REFER = "refer";

    public static final String CLUSTER_KEY = "cluster";

    public static final String DEFAULT_REGISTRY = "zookeeper";

    public static final String REGISTRY_KEY = "registry";

    public static final String CONSUMER_HOST = "consumer.host";

    public static final String PROTOCOL_KEY = "protocol";

    public static final String METHODS_KEY = "methods";

    public static final String TIMEOUT_KEY = "timeout";

    public static final int DEFAULT_TIMEOUT = 3000;

    public static final int RECONNECT_INTERVAL = 2000;

    public static final int RECONNECT_TIMEOUT = 900000;

    public static final String LOADBALANCE_KEY = "loadbalance";

    public static final String STICKY_KEY = "sticky";

    public static final String RETRIES_KEY = "retries";

    public static final int DEFAULT_RETRIES = 2;
}

