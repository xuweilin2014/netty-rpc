package com.newlandframework.rpc.jmx;

import com.newlandframework.rpc.netty.MessageRecvExecutor;
import com.newlandframework.rpc.parallel.AbstractDaemonThread;
import com.newlandframework.rpc.parallel.SemaphoreWrapper;
import org.apache.commons.collections.Predicate;
import org.apache.commons.collections.iterators.FilterIterator;
import org.apache.commons.lang3.StringUtils;
import javax.management.*;
import javax.management.remote.*;
import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.net.MalformedURLException;
import java.rmi.registry.LocateRegistry;
import java.util.Iterator;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Semaphore;

import static com.newlandframework.rpc.core.RpcSystemConfig.DELIMITER;

/**
 * ModuleMetricsHandler是一个MXBean，或者说一个管理构件，用来代表一个被管理的资源实例。
 * 拥有属性：ModuleMetricsVisitor
 * 拥有行为：addModuleMetricsVisitor
 */
public class ModuleMetricsHandler extends AbstractModuleMetricsHandler {
    public final static String MBEAN_NAME = "com.newlandframework.rpc:type=ModuleMetricsHandler";
    public final static int MODULE_METRICS_JMX_PORT = 1098;
    private String moduleMetricsJmxUrl = "";
    private Semaphore semaphore = new Semaphore(0);
    private SemaphoreWrapper semaphoreWrapper = new SemaphoreWrapper(semaphore);
    private static final ModuleMetricsHandler INSTANCE = new ModuleMetricsHandler();
    private MBeanServerConnection connection;
    private CountDownLatch latch = new CountDownLatch(1);
    private ModuleMetricsListener listener = new ModuleMetricsListener();

    //通过饿汉模式来实现单例模式
    public static ModuleMetricsHandler getInstance() {
        return INSTANCE;
    }

    private ModuleMetricsHandler() {
        super();
    }

    @Override
    protected ModuleMetricsVisitor visitCriticalSection(String className, String methodName) {
        final String method = methodName.trim();
        final String cls = className.trim();

        // JMX度量临界区要注意线程间的并发竞争,否则会统计数据失真
        // iterator.next返回的元素都必须符合Predicate中的条件，也就是说使得其中的evaluate方法返回true
        // evaluate使得返回的visitor与前面的cls#method对应，如果不存在的话，就直接创建一个visitor，并且
        // 将其加入到队列visitorList中。
        Iterator iterator = new FilterIterator(visitorList.iterator(), new Predicate() {
            @Override
            public boolean evaluate(Object object) {
                String statClassName = ((ModuleMetricsVisitor) object).getClassName();
                String statMethodName = ((ModuleMetricsVisitor) object).getMethodName();
                return statClassName.compareTo(cls) == 0 && statMethodName.compareTo(method) == 0;
            }
        });

        ModuleMetricsVisitor visitor = null;
        if (iterator.hasNext()) {
            visitor = (ModuleMetricsVisitor) iterator.next();
        }

        if (visitor != null) {
            return visitor;
        } else {
            visitor = new ModuleMetricsVisitor(cls, method);
            addModuleMetricsVisitor(visitor);
            return visitor;
        }
    }

    public void start() {
        //将JMX服务器的启动放入到其它的线程中去执行，使用的是AbstractDaemonThread的匿名内部类，
        //在run方法中启动JMX服务器，而在getDaemonThread方法中，返回这个线程的名字。
        //此匿名内部类会调用父类AbstractDaemonThread的无参构造方法，生成一个名为ModuleMetricsHandler的线程，来负责JMX服务器的启动
        new AbstractDaemonThread() {
            @Override
            public String getDaemonThreadName() {
                return ModuleMetricsHandler.class.getSimpleName();
            }

            /**
             * 让客户端可以连接到JMXConnectorServer服务器，然后可以通过JConsole或者通过网页，对NettyRPC服务端的调用情况进行监控
             */
            @Override
            public void run() {
                MBeanServer mbs = ManagementFactory.getPlatformMBeanServer();
                try {
                    //这个步骤很重要，注册一个端口，绑定url后用于客户端通过rmi方式(也就是JConsole)连接JMXConnectorServer
                    LocateRegistry.createRegistry(MODULE_METRICS_JMX_PORT);
                    MessageRecvExecutor ref = MessageRecvExecutor.getInstance();
                    String ipAddr = StringUtils.isNotEmpty(ref.getServerAddress()) ? StringUtils.substringBeforeLast(
                            ref.getServerAddress(), DELIMITER) : "localhost";

                    //URL路径的结尾可以随意指定，但如果需要用JConsole来进行连接，则必须使用jmx:rmi
                    //这里最后拼接成为：service:jmx:rmi:///jndi/rmi://ipAddr:1098/NettyRPCServer
                    moduleMetricsJmxUrl = "service:jmx:rmi:///jndi/rmi://" + ipAddr + ":" + MODULE_METRICS_JMX_PORT + "/NettyRPCServer";
                    JMXServiceURL url = new JMXServiceURL(moduleMetricsJmxUrl);
                    JMXConnectorServer cs = JMXConnectorServerFactory.newJMXConnectorServer(url, null, mbs);

                    ObjectName name = new ObjectName(MBEAN_NAME);
                    mbs.registerMBean(ModuleMetricsHandler.this, name);

                    //在此ModuleMetricsHandler上注册一个NotificationListener，用来处理发生的事件。
                    mbs.addNotificationListener(name, listener, null, null);

                    cs.start();
                    semaphoreWrapper.release();

                    System.err.printf("【jmx support】:NettyRPC JMX server starts successfully!\n【jmx-url】:%s\n\n", moduleMetricsJmxUrl);
                } catch (IOException | MalformedObjectNameException | InstanceNotFoundException
                        | InstanceAlreadyExistsException | NotCompliantMBeanException
                        | MBeanRegistrationException e) {
                    e.printStackTrace();
                }
            }
        }.start();
    }

    public void stop() {
        MBeanServer mbs = ManagementFactory.getPlatformMBeanServer();
        try {
            ObjectName name = new ObjectName(MBEAN_NAME);
            mbs.unregisterMBean(name);
        } catch (MalformedObjectNameException | InstanceNotFoundException
                | MBeanRegistrationException e) {
            e.printStackTrace();
        }
    }

    public MBeanServerConnection connect() {
        try {
            //semaphore在初始化的时候为0，只有start方法中将JMXConnectorServer启动之后，才会调用semaphore的release方法，使semaphore变为1。
            //由于此方法只在ModuleMetricsHtmlBuilder的init方法中被调用，因此也就是通过semaphoreWrapper确保在JMXConnectorServer启动后，
            //ModuleMetricsHtmlBuilder才能获取到与JMXConnectorServer的连接connection
            if (!semaphoreWrapper.isRelease()) {
                semaphoreWrapper.acquire();
            }

            //与上面start方法中的JMXConnectorServer建立连接
            JMXServiceURL url = new JMXServiceURL(moduleMetricsJmxUrl);
            JMXConnector jmxc = JMXConnectorFactory.connect(url, null);
            connection = jmxc.getMBeanServerConnection();
        } catch (MalformedURLException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return connection;
    }

    public MBeanServerConnection getConnection() {
        return connection;
    }

    public CountDownLatch getLatch() {
        return latch;
    }

    public void setLatch(CountDownLatch latch) {
        this.latch = latch;
    }
}

