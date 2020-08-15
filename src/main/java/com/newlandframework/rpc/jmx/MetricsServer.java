package com.newlandframework.rpc.jmx;

import com.newlandframework.rpc.core.RpcSystemConfig;
import com.newlandframework.rpc.parallel.AbstractDaemonThread;
import com.newlandframework.rpc.parallel.SemaphoreWrapper;
import com.newlandframework.rpc.util.URL;
import org.apache.commons.collections.Predicate;
import org.apache.commons.collections.iterators.FilterIterator;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

import javax.management.*;
import javax.management.remote.*;
import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.rmi.registry.LocateRegistry;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Semaphore;

/**
 * ModuleMetricsHandler是一个MXBean，或者说一个管理构件，用来代表一个被管理的资源实例。
 * 拥有属性：ModuleMetricsVisitor
 * 拥有行为：addModuleMetricsVisitor
 */
public class MetricsServer extends AbstractMetricsServer {

    private static final Logger logger = Logger.getLogger(MetricsServer.class);

    public final static String MBEAN_NAME = "com.newlandframework.rpc:type=MetricsServer";

    private String moduleMetricsJmxUrl = "";

    private final Semaphore semaphore = new Semaphore(0);

    private final SemaphoreWrapper semaphoreWrapper = new SemaphoreWrapper(semaphore);

    private JMXConnectorServer jmxServer;

    private final MetricsListener listener = new MetricsListener();

    public MetricsServer(URL url) {
        super();
        start(url.getHost(), url.getPort());
    }

    @Override
    protected MetricsVisitor visitCriticalSection(String className, String methodName) {
        final String method = methodName.trim();
        final String cls = className.trim();

        // JMX度量临界区要注意线程间的并发竞争,否则会统计数据失真
        // iterator.next返回的元素都必须符合Predicate中的条件，也就是说使得其中的evaluate方法返回true
        // evaluate使得返回的visitor与前面的cls#method对应，如果不存在的话，就直接创建一个visitor，并且
        // 将其加入到队列visitorList中。
        Iterator iterator = new FilterIterator(visitorList.iterator(), new Predicate() {
            @Override
            public boolean evaluate(Object object) {
                String statClassName = ((MetricsVisitor) object).getClassName();
                String statMethodName = ((MetricsVisitor) object).getMethodName();
                return statClassName.compareTo(cls) == 0 && statMethodName.compareTo(method) == 0;
            }
        });

        MetricsVisitor visitor = null;
        if (iterator.hasNext()) {
            visitor = (MetricsVisitor) iterator.next();
        }

        if (visitor != null) {
            return visitor;
        } else {
            visitor = new MetricsVisitor(cls, method);
            addModuleMetricsVisitor(visitor);
            return visitor;
        }
    }

    // 创建并且启动 jmx 服务器
    public void start(String host, int port) {
        // 让客户端可以连接到JMXConnectorServer服务器，然后可以通过JConsole或者通过网页，对NettyRPC服务端的调用情况进行监控
        MBeanServer mbs = ManagementFactory.getPlatformMBeanServer();
        try {
            // 这个步骤很重要，注册一个端口，绑定url后用于客户端通过rmi方式连接JMXConnectorServer
            LocateRegistry.createRegistry(port);

            host = StringUtils.isNotEmpty(host) ? host : RpcSystemConfig.LOCALHOST;

            // URL路径的结尾可以随意指定，但如果需要用JConsole来进行连接，则必须使用jmx:rmi
            // 这里最后拼接成为：service:jmx:rmi:///jndi/rmi://host:1098/NettyRPCServer
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

            logger.info("jmx support NettyRPC JMX server starts successfully! jmx-url : %s" + moduleMetricsJmxUrl);
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
            e.printStackTrace();
        }
        return connection;
    }


}

