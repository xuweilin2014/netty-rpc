package com.newlandframework.test;

import com.newlandframework.rpc.services.PersonManage;
import com.newlandframework.rpc.services.pojo.Person;
import org.springframework.context.support.ClassPathXmlApplicationContext;


public class PojoCallTest {
    public static void main(String[] args) {
        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("classpath:rpc-invoke-config-client.xml");

        PersonManage manage = (PersonManage) context.getBean("personManage");

        Person p = new Person();
        p.setId(20150811);
        p.setName("XiaoHaoBaby");
        p.setAge(1);

        int result = manage.save(p);

        manage.query(p);

        System.out.println("call pojo rpc result:" + result);

        context.destroy();
    }
}

