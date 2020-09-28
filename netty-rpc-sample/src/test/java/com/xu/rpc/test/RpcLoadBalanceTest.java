package com.xu.rpc.test;

import com.xu.rpc.services.PersonManage;
import com.xu.rpc.services.pojo.Person;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import java.util.Random;

public class RpcLoadBalanceTest {

    public static void main(String[] args) throws InterruptedException {
        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("classpath:rpc-invoke-config-client.xml");

        PersonManage manage = (PersonManage) context.getBean("personManage");

        int globalId = 0;
        Person p = new Person();
        p.setId(20150811);
        p.setName("XiaoHaoBaby");
        p.setAge(1);

        int counter = 1;
        while (counter <= 100){
            Thread.sleep(new Random().nextInt(2000));
            manage.save(p);
            counter++;
        }
    }

}
