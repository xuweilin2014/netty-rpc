package com.xu.test;


import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.RpcContext;
import com.xu.rpc.exception.InvokeTimeoutException;
import com.xu.rpc.services.CostTimeCalculate;
import com.xu.rpc.services.pojo.CostTime;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;


public class AsyncRpcTimeoutCallTest {
    public static void main(String[] args) {
        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("classpath:rpc-invoke-config-client.xml");
        final CostTimeCalculate calculate = (CostTimeCalculate) context.getBean("costTime");
        CostTime busy = calculate.busy();
        Future<?> future = RpcContext.getContext().getFuture();
        try {
            System.out.println(future.get());
        } catch (InterruptedException e) {
            e.printStackTrace();
        } catch (ExecutionException e) {
            e.printStackTrace();
        }
    }
}
