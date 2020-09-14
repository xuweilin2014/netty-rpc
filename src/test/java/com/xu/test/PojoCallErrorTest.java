package com.xu.test;

import com.xu.rpc.exception.InvokeModuleException;
import com.xu.rpc.services.PersonManage;
import com.xu.rpc.services.pojo.Person;
import org.springframework.context.support.ClassPathXmlApplicationContext;


public class PojoCallErrorTest {
    public static void test1(PersonManage manage) {
        try {
            manage.check();
        } catch (InvokeModuleException e) {
            System.out.println(e.getMessage());
        }
    }

    public static void test2(PersonManage manage) {
        try {
            Person p = new Person();
            p.setId(20150811);
            p.setName("XiaoHaoBaby");
            p.setAge(1);
            manage.checkAge(p);
        } catch (InvokeModuleException e) {
            System.out.println(e.getMessage());
        }
    }

    public static void main(String[] args) {
        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("classpath:rpc-invoke-config-client.xml");

        PersonManage manage = (PersonManage) context.getBean("personManage");

        try {
            test1(manage);
            //test2(manage);
        } finally {
            context.destroy();
        }
    }
}

