package com.xu.rpc.core;

/**
 * netty-rpc框架的一些配置参数
 */
public class RpcConfig {

    public static final String SYSTEM_PROPERTY_THREADPOOL_REJECTED_POLICY_ATTR = "nettyrpc.parallel.rejected.policy";

    public static final String SYSTEM_PROPERTY_THREADPOOL_QUEUE_NAME_ATTR = "nettyrpc.parallel.queue";

    public static final int SYSTEM_PROPERTY_PARALLEL = Math.max(2, Runtime.getRuntime().availableProcessors());

    public static final int SERIALIZE_POOL_MAX_TOTAL = 500;

    public static final int SERIALIZE_POOL_MIN_IDLE = 10;

    public static final int SERIALIZE_POOL_MAX_WAIT_MILLIS = 5000;

    public static final int SERIALIZE_POOL_MIN_EVICTABLE_IDLE_TIME_MILLIS = 600000;

    public static final String SCOPE_LOCAL = "local";

    public static final String SCOPE_REMOTE = "remote";

    public static final String INJVM_PROTOCOL = "injvm";

    public static final String EXPORT_KEY = "export";

    public static final String REGISTRY_FACTORY = "registry";

    public static final int ZOOKEEPER_TIMEOUT = 1000;

    public static final String INTERFACE_KEY = "interfaceName";

    public static final String ROOT_DIR = "/rpc";

    public static final String DIR_SEPARATOR = "/";

    public static final int RETRY_PERIOD = 5000;

    public static final String SERIALIZE_KEY = "serialize";

    public static final String JDK_SERIALIZE = "jdk";

    public static final String HOST = "host";

    public static final String PORT = "port";

    public static final String SERVICES_DIRECTORY = "META-INF/services/";

    public static final String RPC_INTERNAL_DIRECTORY = "META-INF/rpc/internal";

    public static final String REMOVE_PREFIX = "-";

    public static final String RPC_DEFAULT = "default";

    public static final String REGISTRY_PROTOCOL = "registry";

    public static final String EMPTY_PROTOCOL = "empty";

    public static final String FILTER = "filter";

    public static final String PROVIDER = "provider";

    public static final String CONSUMER = "consumer";

    public static final String LOCALHOST = "localhost";

    public static final String METRICS_KEY = "metrics";

    public static final int ECHO_PORT = 18882;

    public static final int METRICS_PORT = 9999;

    public static final String METRICS_PORT_KEY = "metricsPort";

    public static final String MONITOR_KEY = "monitor";

    public static final String REFER_KEY = "refer";

    public static final String CLUSTER_KEY = "cluster";

    public static final String DEFAULT_REGISTRY = "zookeeper";

    public static final String REGISTRY_KEY = "registry";

    public static final String PROTOCOL_KEY = "protocol";

    public static final String METHODS_KEY = "methods";

    public static final String TIMEOUT_KEY = "timeout";

    public static final int DEFAULT_TIMEOUT = 3000;

    public static final int RECONNECT_INTERVAL = 10000;

    public static final int RECONNECT_TIMEOUT = 15000;

    public static final String LOADBALANCE_KEY = "loadbalance";

    public static final String STICKY_KEY = "sticky";

    public static final String RETRIES_KEY = "retries";

    public static final int DEFAULT_RETRIES = 3;

    public static final String WEIGHT_KEY = "weight";

    public static final int DEFAULT_WEIGHT = 100;

    public static final String ASYNC_KEY = "async";

    public static final String HEARTBEAT_KEY = "heartbeat";

    public static final int DEFAULT_HEARTBEAT = 60000;

    public static final String HEARTBEAT_TIMEOUT_KEY = "heartbeatTimeout";

    public static final String LAST_READ_TIMESTAMP = "lastReadTimestamp";

    public static final String LAST_WRITE_TIMESTAMP = "lastWriteTimestamp";

    public static final String STUB_KEY = "stub";

    public static final String SCOPE_KEY = "scope";

    public static final String LRU_CACHE = "lru";

    public static final String THREADLOCAL_CACHE = "threadlocal";

    public static final String CACHE_KEY = "cache";

    public static final String CACHE_CAPACITY_KEY = "capacity";

    public static final String COMMA_SEPARATOR = ",";

    public static final String FALSE = "false";

    public static final String TRUE = "true";

    public static final String TOKEN_KEY = "token";

    public static final String MOCK_KEY = "mock";

    public static final String MOCK_FAIL_KEY = "fail";

    public static final String MOCK_FORCE_KEY = "force";

    public static final String FILE_KEY = "file";

    public static final String APPLICATION_KEY = "application";

    public static final String RATE_KEY = "rate";

    public static final int DEFAULT_RATE_LIMIT_PER_SECOND = Integer.MAX_VALUE;

    public static final String LIMITER_KEY = "limiter";

    public static final String IP_ADDRESS = "ip";

    public static final String ADDRESS_DELIMITER = ":";

    public static final String SEMICOLON = ";";

    public static final CharSequence ABILITY_KEY = "ability";

    public static final String INVOKE_TIMESPAN_KEY = "invokeTimespan";

    public static final String STACK_TRACE_KEY = "stackTrace";

    public static final String ONE_WAY_KEY = "oneWay";

    public static final long OPERATION_RETRY_TIMEOUT = 3000;

    public static final int ZOOKEEPER_SESSION_TIMEOUT = 30000;

    public static final String HEX_SEPARATOR = "#";

    public static final String OVERRIDE_KEY = "override";

    public static final String MOCK_SET_KEY = "mockSet";

    public static final String METHOD_KEY = "methodName";

    public static final String FILTER_KEY = "filter";

    public static final String ID_KEY = "id";

    public static final String SEGMENTS_KEY = "segments";

    public static final String ECHO_PORT_KEY = "echoPort";

    public static final int LENGTH_FIELD_LENGTH = 4;

    public static final int LENGTH_ADJUSTMENT = 0;

    public static final int MAX_FRAME_LENGTH = 1024 * 1024;

    public static final int LENGTH_FIELD_OFFSET = 0;

    public static final int INITIAL_BYTES_TO_STRIP = 4;

    public static final String PROTOSTUFF_SERIALIZE = "protostuff";

    public static final String SERIALIZER_FACTORY = "serialize";

    public static final byte REQUEST = 1;

    public static final byte RESPONSE = 1 << 1;
}

