package com.xu.rpc.test;

import rpc.core.RpcContext;
import rpc.services.PersonManage;
import rpc.services.pojo.Person;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import java.io.IOException;
import java.util.concurrent.ExecutionException;

public class PojoFaultToleranceTest {

    public static void main(String[] args) throws ExecutionException, InterruptedException, IOException {
        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("classpath:rpc-invoke-config-client.xml");

        PersonManage manage = (PersonManage) context.getBean("personManage");

        Person p = new Person();
        p.setId(20150811);
        p.setName("XiaoHaoBaby");
        p.setAge(1);

        //Thread.sleep(20000);
        int counter = 1;
        try {
            while (counter <= 5) {
                System.out.println("this is the " + counter + " invoke.");
                System.out.println(manage.save(p));
                counter++;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        System.in.read();
    }

}
