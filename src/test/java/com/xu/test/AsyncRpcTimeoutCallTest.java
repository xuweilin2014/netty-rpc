package com.xu.test;


import com.xu.rpc.exception.InvokeTimeoutException;
import com.xu.rpc.services.CostTimeCalculate;
import com.xu.rpc.services.pojo.CostTime;
import org.springframework.context.support.ClassPathXmlApplicationContext;


public class AsyncRpcTimeoutCallTest {
    public static void main(String[] args) {
//        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("classpath:rpc-invoke-config-client.xml");
//
//        final CostTimeCalculate calculate = (CostTimeCalculate) context.getBean("costTime");
//
//        AsyncInvoker invoker = new AsyncInvoker();
//
//        try {
//            CostTime elapse0 = invoker.submit(new AsyncCallback<CostTime>() {
//                @Override
//                public CostTime call() {
//                    return calculate.busy();
//                }
//            });
//
//            System.out.println(((AsyncCallObject) elapse0)._getStatus());
//            System.out.println("1 async nettyrpc call:[" + "result:" + elapse0 + ", status:[" + ((AsyncCallObject) elapse0)._getStatus() + "]");
//        } catch (InvokeTimeoutException e) {
//            System.err.println(e.getMessage());
//            context.destroy();
//            return;
//        }
//
//        context.destroy();
    }
}
