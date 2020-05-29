public class JMXAnalysis {
    /**
     * netty-rpc项目中的JMX检测的大致流程如下所示：
     *
     * 1.JMX服务器启动
     *
     * 首先，在自定义的Spring标签registry中，它对应的bean为NettyRpcRegistery，这个bean实现了InitializingBean这个接口，因此
     * 在这个bean创建成功之后，会回调其afterProperties方法，这个方法主要完成两个功能：让服务开始监听客户端发起rpc调用的ip地址和端口号、
     * 启动JMX监控（如果用户配置了JMX监控的话）。
     * 
     * 监控的开启具体由ModuleMetricsHandler中的start方法完成。首先从JMX的角度看，ModuleMetricsHandler本身就是一个MXBean，或者
     * 说一个管理构件，用来代表一个被管理的资源实例，具体表现为它实现了ModuleMetricsVisitorMXBean接口。
     * 因此它拥有属性：ModuleMetricsVisitor；拥有行为：addModuleMetricsVisitor。在start方法中我们把ModuleMetricsHandler对象本身
     * 注册到MBeanServer中，并且在handler上添加一个监听器ModuleMetricsListener。当ModuleMetricsHandler对象调用sendNotification方法时，
     * 就会把事件发送给监听器ModuleMetricsListener。
     * 
     * 2.客户端发起Rpc调用
     *
     * 在客户端发起Rpc调用时，我们这里以调用AddCalculate#add方法为例。如果客户端有多个线程同时发送调用请求，Rpc服务器的MessageRecvHandler
     * 会接收到多个请求对象MessageRequest，然后会依据用户是否开启JMX创建不同的task。
     * 
     * 这些task的主要任务如下：
     * 1）将ModuleMetricsVisitor中和方法调用次数相关的属性invokeCount增加1，这个ModuleMetricsVisitor对应于AddCalculate#add方法。
     * 2）利用反射获取到用户要调用的方法，执行方法并且获取到调用的结果。
     * 3）调用成功后，修改ModuleMetricsVisitor中的属性：invokeSuccCount、accumulateTimespan、invokeMinTimespan、invokeMaxTimespan，
     * 分别代表方法调用成功的次数、累积耗时、最大耗时、最小耗时；当调用失败之后，修改ModuleMetricsVisitor中的属性：方法调用的失败次数、
     * 方法调用最后一次失败的时间以及最后一次失败的堆栈明细；当调用被拦截之后，修改ModuleMetricsVisitor中的属性：方法被拦截的次数。
     * 
     * 这些task创建完毕之后，放入到线程池里面去执行，
     * 这么做的原因是可能是业务方法（比如add方法）的执行时间会很长，导致Netty中的Reactor的worker线程一直忙于执行业务，从而使其它任务堆积。
     * 得不到响应。
     * 
     * 在MessageRecvHandler#channelRead方法中，创建的这些task，都是AbstractMessageRecvInitializeTask的子类，它们都实现了Callable接口。
     * 它们的具体类型如下所示：
     * 1）如果用户开启了JMX监控就返回MessageRecvInitializeTask；
     * 2）如果用户没有开启JMX监控，就直接返回MessageRecvInitializeTaskAdapter；
     * 
     * 3.通过浏览器获取方法调用的详细数据
     *
     * 用户可以通过JConsole的方法或者浏览器的方式。在服务器内部，有一个小型的HTTP服务器，可以接收HTTP请求，返回HTTP响应。在ApiHandler#buildResponseMsg
     * 中，返回构建的HTTP响应文本。具体如下：
     * 1）如果用户开启了JMX监控的话，就调用ModuleMetricsHtmlBuilder#buildModuleMetrics方法，来获取RPC服务器中方法调用的具体数据
     * 2）如果用户没有开启JMX监控的话，就会返回一个error信息
     * 
     */
}
