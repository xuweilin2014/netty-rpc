package com.xu.rpc.test;

import com.xu.rpc.core.RpcContext;
import com.xu.rpc.services.PersonManage;
import com.xu.rpc.services.pojo.Person;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import java.util.concurrent.ExecutionException;

public class RpcReconnectTest {

    public static void main(String[] args) throws InterruptedException, ExecutionException {
        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("classpath:rpc-invoke-config-client.xml");

        PersonManage manage = (PersonManage) context.getBean("personManage");

        Person p = new Person();
        p.setId(20150811);
        p.setName("XiaoHaoBaby");
        p.setAge(1);
        long sleepTime = 60000 * 5;
        long start = System.currentTimeMillis();

        while (System.currentTimeMillis() - start <= sleepTime){
            Thread.sleep(1000);
        }

        manage.save(p);
        System.out.println(RpcContext.getContext().getFuture().get());
    }

}
