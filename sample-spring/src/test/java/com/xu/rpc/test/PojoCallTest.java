package com.xu.rpc.test;

import rpc.services.PersonManage;
import rpc.services.pojo.Person;
import org.springframework.context.support.ClassPathXmlApplicationContext;


public class PojoCallTest {
    public static void main(String[] args) throws InterruptedException {
        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("classpath:rpc-invoke-config-client.xml");

        PersonManage manage = (PersonManage) context.getBean("personManage");

        Person p = new Person();
        p.setId(20150811);
        p.setName("XiaoHaoBaby");
        p.setAge(1);

        int result = manage.save(p);
        manage.query(p);

        System.out.println("call pojo rpc result:" + result);
    }
}

