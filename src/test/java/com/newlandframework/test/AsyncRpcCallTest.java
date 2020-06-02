package com.newlandframework.test;

import com.newlandframework.rpc.async.AsyncCallObject;
import com.newlandframework.rpc.async.AsyncCallback;
import com.newlandframework.rpc.async.AsyncInvoker;
import com.newlandframework.rpc.services.CostTimeCalculate;
import com.newlandframework.rpc.services.pojo.CostTime;
import org.springframework.context.support.ClassPathXmlApplicationContext;

/**
 * 介绍一下异步调用的大概逻辑：首先，通过ctx.getBean获得到的bean是通过JDK代理创建的一个动态代理对象，
 * invoker.submit把实现了AsyncCallback接口的对象再用Callable封装，传进FutureTask对象中，然后由线程池来具体的执行。
 * 在AsyncCallback接口的call方法中，可以实现用户自定义的业务逻辑。这里是调用calculate的calculate()方法，这个调用是由
 * 线程池中的线程来执行，而不是main线程。由于calculate是一个代理对象，因此我们调用calculate方法实际上会调用到
 * MessageSendProxy中的invoke方法，通过MessageSendHandler把调用请求发送给Rpc服务器，并且阻塞等待默认30s服务器返回的结果，
 * 最后这个结果被保存在AsyncFuture的属性中（即其父类FutureTask中的outcome属性）。
 *
 * 另外，在main线程中，invoker.submit返回的elapse也是一个CGLIB代理对象。当调用elapse0.setDetail（第一次调用）时，就会被AsyncLoadResultInterceptor
 * 拦截器拦截，最终调用到AsyncCallResult中的loadFuture方法，通过future.get方法获取前面返回的结果对象，这里是CostTime对象（不是代理，就是一个原生
 * 的CostTime对象）。然后就直接调用这个CostTime对象的setDetail方法，后面再调用方法（比如toString），就直接使用前面返回的这个CostTime对象。
 * 这个过程是在main线程中进行的。
 */
public class AsyncRpcCallTest {
    public static void main(String[] args) {
        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("classpath:rpc-invoke-config-client.xml");

        final CostTimeCalculate calculate = (CostTimeCalculate) context.getBean("costTime");

        long start = 0, end = 0;
        start = System.currentTimeMillis();

        AsyncInvoker invoker = new AsyncInvoker();

        CostTime elapse0 = invoker.submit(new AsyncCallback<CostTime>() {
            @Override
            public CostTime call() {
                return calculate.calculate();
            }
        });

        CostTime elapse1 = invoker.submit(new AsyncCallback<CostTime>() {
            @Override
            public CostTime call() {
                return calculate.calculate();
            }
        });

        CostTime elapse2 = invoker.submit(new AsyncCallback<CostTime>() {
            @Override
            public CostTime call() {
                return calculate.calculate();
            }
        });

        elapse0.setDetail("hhahahha");
        System.out.println(elapse0);
        System.out.println("1 async nettyrpc call:[" + "result:" + elapse0 + ", status:[" + ((AsyncCallObject) elapse0)._getStatus() + "]");
        System.out.println("2 async nettyrpc call:[" + "result:" + elapse1 + ", status:[" + ((AsyncCallObject) elapse1)._getStatus() + "]");
        System.out.println("3 async nettyrpc call:[" + "result:" + elapse2 + ", status:[" + ((AsyncCallObject) elapse2)._getStatus() + "]");


        end = System.currentTimeMillis();

        System.out.println("nettyrpc async calculate time:" + (end - start));

        context.destroy();
    }
}

