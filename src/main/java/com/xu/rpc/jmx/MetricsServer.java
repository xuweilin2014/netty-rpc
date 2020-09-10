package com.xu.rpc.jmx;

import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.parallel.SemaphoreWrapper;
import com.xu.rpc.commons.URL;
import com.xu.rpc.remoting.server.Server;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

import javax.management.*;
import javax.management.remote.*;
import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.rmi.registry.LocateRegistry;
import java.util.concurrent.Semaphore;

/**
 * ModuleMetricsHandler是一个MXBean，或者说一个管理构件，用来代表一个被管理的资源实例。
 * 拥有属性：ModuleMetricsVisitor
 * 拥有行为：addModuleMetricsVisitor
 */
public class MetricsServer {

    private static final Logger logger = Logger.getLogger(MetricsServer.class);

    public final static String MBEAN_NAME = "com.newlandframework.rpc:type=MetricsServer";

    private String moduleMetricsJmxUrl = "";

    private final Semaphore semaphore = new Semaphore(0);

    private final SemaphoreWrapper semaphoreWrapper = new SemaphoreWrapper(semaphore);

    private JMXConnectorServer jmxServer;

    private final MetricsListener listener = new MetricsListener();

    private String host;

    private int port;

    public MetricsServer(URL url) {
        host = url.getHost();
        // 获取 jmx server 的端口
        port = url.getParameter(RpcConfig.METRICS_PORT_KEY, RpcConfig.METRICS_PORT);
    }

    // 创建并且启动 jmx 服务器
    public void start() {
        // 让客户端可以连接到JMXConnectorServer服务器，然后可以通过JConsole或者通过网页，对NettyRPC服务端的调用情况进行监控
        MBeanServer mbs = ManagementFactory.getPlatformMBeanServer();
        try {
            // 这个步骤很重要，注册一个端口，绑定url后用于客户端通过rmi方式连接JMXConnectorServer
            LocateRegistry.createRegistry(port);

            host = StringUtils.isNotEmpty(host) ? host : RpcConfig.LOCALHOST;

            // URL路径的结尾可以随意指定，但如果需要用JConsole来进行连接，则必须使用jmx:rmi
            // 这里最后拼接成为：service:jmx:rmi:///jndi/rmi://host:9999/NettyRPCServer
            moduleMetricsJmxUrl = "service:jmx:rmi:///jndi/rmi://" + host + ":" + port + "/NettyRPCServer";
            JMXServiceURL url = new JMXServiceURL(moduleMetricsJmxUrl);
            jmxServer = JMXConnectorServerFactory.newJMXConnectorServer(url, null, mbs);

            ObjectName name = new ObjectName(MBEAN_NAME);

            // 表达式OuterClass.this表示外围类的引用
            mbs.registerMBean(MetricsServer.this, name);

            // 在此ModuleMetricsHandler上注册一个NotificationListener，用来处理发生的事件。
            mbs.addNotificationListener(name, listener, null, null);

            jmxServer.start();
            semaphoreWrapper.release();

            logger.info("netty-rpc jmx server starts successfully! jmx-url : %s" + moduleMetricsJmxUrl);
        } catch (IOException | MalformedObjectNameException | InstanceNotFoundException
                | InstanceAlreadyExistsException | NotCompliantMBeanException
                | MBeanRegistrationException e) {
            logger.error("cannot start jmx server, error occurs: " + e.getMessage());
        }
    }

    public void stop() {
        MBeanServer mbs = ManagementFactory.getPlatformMBeanServer();
        try {
            ObjectName name = new ObjectName(MBEAN_NAME);
            // 取消注册 mbean
            mbs.unregisterMBean(name);
            // 关闭 jmx server
            jmxServer.stop();
        } catch (MalformedObjectNameException
                | InstanceNotFoundException | MBeanRegistrationException | IOException e) {
            logger.error("error occurs when unregistering mbean and stopping jmx server.");
        }
    }

    public MBeanServerConnection connect() {
        MBeanServerConnection connection = null;
        try {
            // semaphore在初始化的时候为0，只有start方法中将JMXConnectorServer启动之后，才会调用semaphore的release方法，使semaphore变为1。
            // 由于此方法只在ModuleMetricsHtmlBuilder的init方法中被调用，因此也就是通过semaphoreWrapper确保在JMXConnectorServer启动后，
            // ModuleMetricsHtmlBuilder才能获取到与JMXConnectorServer的连接connection
            while (!semaphoreWrapper.isRelease()) {
                semaphoreWrapper.acquire();
            }

            // 与上面start方法中的JMXConnectorServer建立连接
            JMXServiceURL url = new JMXServiceURL(moduleMetricsJmxUrl);
            JMXConnector jmxc = JMXConnectorFactory.connect(url, null);
            connection = jmxc.getMBeanServerConnection();
        } catch (IOException e) {
            logger.error("error occurs when trying to connect to jmx server. caused by " + e.getMessage());
        }
        return connection;
    }


}

