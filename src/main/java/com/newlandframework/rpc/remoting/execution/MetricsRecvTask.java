package com.newlandframework.rpc.remoting.execution;

import com.newlandframework.rpc.remoting.handler.ChannelHandler;
import com.newlandframework.rpc.util.ReflectionUtil;
import com.newlandframework.rpc.event.InvokeEventFacade;
import com.newlandframework.rpc.event.ModuleEvent;
import com.newlandframework.rpc.jmx.MetricsServer;
import com.newlandframework.rpc.jmx.MetricsVisitor;
import com.newlandframework.rpc.model.MessageRequest;
import com.newlandframework.rpc.observer.*;
import io.netty.channel.Channel;

import java.lang.reflect.Method;


public class MetricsRecvTask extends AbstractRecvTask {

    private MetricsVisitor visitor;

    private InvokeEventFacade facade;

    private InvokeEventTarget target = new InvokeEventTarget();

    public MetricsRecvTask(MessageRequest request, ChannelHandler handler, Channel channel) {
        super(request, handler, channel);
    }

    /**
     * 每一个客户端发起的一次Rpc请求，都会在服务器端将其包装成一个Task，然后放到线程池中去执行。这些Task的类型是MessageRecvInitializeTask
     * （开启了JMX监控）或者MessageRecvInitializeTaskAdapter（没有开启JMX）这两类，他们都实现了Callable接口。每个Task任务的执行流程如下：
     * 1.调用injectInvoke方法，增加方法的调用次数，不过如果没有开启JMX，也就是task是MessageRecvInitializeTaskAdapter的话，这个方法是个空方法，
     * 2.调用reflect来执行客户端要调用的方法，并且获取到执行的结果。
     * 3.如果调用成功的话，就会修改方法调用成功的次数、累积耗时、最大耗时、最小耗时
     * 4.如果方法调用被拦截，就会修改方法被拦截的次数
     * 5.如果方法调用时抛出异常，就会修改方法调用的失败次数、方法调用失败的时间以及失败的堆栈明细
     */
    @Override
    public void run() {
        //增加方法的调用次数
        injectInvoke();

        super.run();

        if (invokeStatus.isDone()) {
            // 增加方法调用成功的次数
            injectSuccInvoke(invokeTimespan);
        } else if (invokeStatus.isRejected()){
            //修改方法被拦截的次数
            injectFilterInvoke();
        } else if (invokeStatus.isExceptional()){
            //修改方法调用的失败次数、方法调用失败的时间以及失败的堆栈明细
            injectFailInvoke(response.getError());
        }

        channel.writeAndFlush(response);
    }


    protected void injectInvoke() {
        Class<?> cls;
        try {
            // request.getClassName返回客户端rpc调用方法所属于的接口的名字
            cls = Class.forName(request.getInterfaceName());
        } catch (ClassNotFoundException e) {
            throw new IllegalStateException(request.getInterfaceName() + " does not exist in rpc server.");
        }

        ReflectionUtil utils = new ReflectionUtil();

        try {
            // 通过反射获取客户端要调用的方法
            Method method = ReflectionUtil.getDeclaredMethod(cls, request.getMethodName(), request.getTypeParameters());
            // 通过反射获取方法的签名，保存到ReflectionUtils类的provider对象中
            utils.listMethod(method, false);
            // 获取客户端要调用的方法的方法签名的字符串
            String signatureMethod = utils.getProvider().toString();
            // 获取和此方法对应的ModuleMetricsVisitor对象，用来记录这个方法的调用情况，每一个特定的方法，都只和一个ModuleMetricsVisitor对象对应
            visitor = MetricsServer.getInstance().getVisitor(request.getInterfaceName(), signatureMethod);
            // 创建了一个InvokeEventFacade类型的对象facade，它包含了所有INVOKE类型的Event对象，并且这些Event对象中都
            // 保存了handler、visitor这两个参数
            facade = new InvokeEventFacade(MetricsServer.getInstance(), visitor);
            // target是被观察的对象，通过addObserver方法可以添加观察者对象，这里是InvokeObserver
            target.addObserver(new InvokeObserver(facade, visitor));
            // 调用notify方法，回调所有观察者对象的update方法，并且将INVOKE_EVENT事件进行传递，但是只有InvokeObserver可以被接收到
            target.notify(ModuleEvent.INVOKE_EVENT);
        } finally {
            utils.clearProvider();
        }
    }


    protected void injectSuccInvoke(long invokeTimespan) {
        target.addObserver(new InvokeSuccObserver(facade, visitor, invokeTimespan));
        target.notify(ModuleEvent.INVOKE_SUCC_EVENT);
    }

    // 当方法调用失败之后，就会调用此方法，作用是更新方法调用失败的次数、方法调用最后一次失败的时间
    // 以及最后一次失败的堆栈明细这三个参数。
    protected void injectFailInvoke(Throwable error) {
        target.addObserver(new InvokeFailObserver(facade, visitor, error));
        target.notify(ModuleEvent.INVOKE_FAIL_EVENT);
    }

    protected void injectFilterInvoke() {
        target.addObserver(new InvokeFilterObserver(facade, visitor));
        target.notify(ModuleEvent.INVOKE_FILTER_EVENT);
    }
}
