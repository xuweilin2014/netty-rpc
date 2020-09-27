package com.xu.rpc.test;

import rpc.core.RpcConfig;
import rpc.core.RpcContext;
import rpc.services.CostTimeCalculate;
import rpc.services.PersonManage;
import rpc.services.pojo.CostTime;
import rpc.services.pojo.Person;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

/**
 * 介绍一下异步调用的大概逻辑：
 *
 * 首先，我们需要创建一个实现AsyncCallback接口的对象，在对象的call方法中写入我们具体的业务逻辑代码，通常是要求调用服务器端的某些代码，
 * 另外，AsyncCallback<T>中的泛型参数T通常是call方法的返回结果。
 *
 * 接着，通过AsyncInvoker#submit方法，把实现了AsyncCallback接口的对象封装成一个FutureTask对象，然后放入到线程池中去执行。也就是说，我们
 * 业务代码是在其它线程中被执行的，而不是在main线程中。我们以下面的calculate.calculate为例，calculate.calculate还是会进行同步调用，也就是
 * 在MessageSendProxy中，将方法调用包装成一个MessageRequest对象，发送给服务器，然后阻塞等待结果。
 *
 * 在前面把实现了AsyncCallback接口的对象submit给AsyncInvoker对象之后，就会返回一个对象，这个对象通常是业务逻辑代码返回对象的一个CGLIB代理对象。
 * 在AsyncInvoker#submit方法中，会使用CGLIB创建了一个实现了AsyncCallObject接口，继承了AsyncCallback<T>中T所表示类的代理对象，然后将这个代理对象返回。
 * 比如下面的elapse0。
 *
 * 当elapse0调用AsyncCallObject接口中的_getStatus方法时，会将调用转发给AsyncLoadStatusInterceptor中的intercept方法，然后根据封装好的FutureTask
 * 对象来获取到调用的状态是什么。
 *
 * 当elapse0调用除_getsStatus之外的其它方法（比如setDetail方法）时，就会将调用转发给AsyncLoadResultInterceptor中的intercept方法。此时调用会阻塞
 * （因为调用前面的FutureTask#get方法），以获取到我们业务代码真正的调用返回对象，然后再在这个返回对象上调用setDetail方法。
 *
 * 总结：
 *
 * 我们把要异步调用的方法写入AsyncCallback中的call方法中，然后AsyncInvoker会把此AsyncCallback对象会被封装成一个FutureTask对象，然后放入到线程池里面去执行，也就是由线程池
 * 中的其它线程来真正向服务器发起调用。然后AsyncInvoker#submit返回一个代理对象，此对象实现了AsyncCallObject接口，同时也继承了AsyncCallback<T>中的T参数，比如我们下面的CostTime
 * 对象。
 *
 * 当我们要调用代理对象的_getStatus方法和其它方法时，分开进行执行。调用_getStatus会返回客户端向服务器发起调用的状态；调用其它方法，会阻塞直到向服务器的调用返回真正的
 * 结果对象，然后再利用此结果对象来调用具体的方法。
 */
public class AsyncRpcCallTest {
    public static void main(String[] args) throws ExecutionException, InterruptedException {
        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("classpath:rpc-invoke-config-client.xml");
        //final CostTimeCalculate calculate = (CostTimeCalculate) context.getBean("costTime");
        PersonManage manage = (PersonManage) context.getBean("personManage");

        Person p = new Person();
        p.setId(20150811);
        p.setName("XiaoHaoBaby");
        p.setAge(1);

        int save = manage.save(p);
        System.out.println("person manage save: " + save);
        Future<?> future = RpcContext.getContext().getFuture();
        // System.out.println("future get result: " + future.get());

        manage.query(p);
    }
}

