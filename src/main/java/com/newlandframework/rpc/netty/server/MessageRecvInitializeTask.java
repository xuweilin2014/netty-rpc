package com.newlandframework.rpc.netty.server;

import com.newlandframework.rpc.util.ReflectionUtil;
import com.newlandframework.rpc.event.InvokeEventFacade;
import com.newlandframework.rpc.event.ModuleEvent;
import com.newlandframework.rpc.filter.ServiceFilterBinder;
import com.newlandframework.rpc.jmx.ModuleMetricsHandler;
import com.newlandframework.rpc.jmx.ModuleMetricsVisitor;
import com.newlandframework.rpc.model.MessageRequest;
import com.newlandframework.rpc.model.MessageResponse;
import com.newlandframework.rpc.observer.*;
import java.lang.reflect.Method;
import java.util.Map;


public class MessageRecvInitializeTask extends AbstractMessageRecvInitializeTask {

    private ModuleMetricsVisitor visitor;

    private InvokeEventFacade facade;

    private InvokeEventTarget target = new InvokeEventTarget();

    public MessageRecvInitializeTask(MessageRequest request, MessageResponse response, Map<String, Object> handlerMap) {
        super(request, response, handlerMap);
    }

    @Override
    protected void injectInvoke() {
        //request.getClassName返回客户端rpc调用方法所属于的接口的名字
        Class cls = handlerMap.get(request.getClassName()).getClass();
        boolean binder = ServiceFilterBinder.class.isAssignableFrom(cls);
        if (binder) {
            //获取客户端要调用的服务的实现类，比如客户端要调用PersonManage的save方法，save方法的真正实现类为PersonManageImpl，
            //所以这里获取到的就是PersonManageImpl.class
            cls = ((ServiceFilterBinder) handlerMap.get(request.getClassName())).getObject().getClass();
        }

        ReflectionUtil utils = new ReflectionUtil();

        try {
            //通过反射获取客户端要调用的方法
            Method method = ReflectionUtil.getDeclaredMethod(cls, request.getMethodName(), request.getTypeParameters());
            //通过反射获取方法的签名，保存到ReflectionUtils类的provider对象中
            utils.listMethod(method, false);
            //获取客户端要调用的方法的方法签名的字符串
            String signatureMethod = utils.getProvider().toString();
            //获取和此方法对应的ModuleMetricsVisitor对象，用来记录这个方法的调用情况，每一个特定的方法，都只和一个ModuleMetricsVisitor对象
            //对应
            visitor = ModuleMetricsHandler.getInstance().getVisitor(request.getClassName(), signatureMethod);
            //创建了一个InvokeEventFacade类型的对象facade，它包含了所有INVOKE类型的Event对象，并且这些Event对象中都
            //保存了handler、visitor这两个参数
            facade = new InvokeEventFacade(ModuleMetricsHandler.getInstance(), visitor);
            //target是被观察的对象，通过addObserver方法可以添加观察者对象，这里是InvokeObserver
            target.addObserver(new InvokeObserver(facade, visitor));
            //调用notify方法，回调所有观察者对象的update方法，并且将INVOKE_EVENT事件进行传递，但是只有InvokeObserver可以被接收到
            target.notify(ModuleEvent.INVOKE_EVENT);
        } finally {
            utils.clearProvider();
        }
    }

    @Override
    protected void injectSuccInvoke(long invokeTimespan) {
        target.addObserver(new InvokeSuccObserver(facade, visitor, invokeTimespan));
        target.notify(ModuleEvent.INVOKE_SUCC_EVENT);
    }

    // 当方法调用失败之后，就会调用此方法，作用是更新方法调用失败的次数、方法调用最后一次失败的时间
    // 以及最后一次失败的堆栈明细这三个参数。
    @Override
    protected void injectFailInvoke(Throwable error) {
        target.addObserver(new InvokeFailObserver(facade, visitor, error));
        target.notify(ModuleEvent.INVOKE_FAIL_EVENT);
    }

    @Override
    protected void injectFilterInvoke() {
        target.addObserver(new InvokeFilterObserver(facade, visitor));
        target.notify(ModuleEvent.INVOKE_FILTER_EVENT);
    }
}
