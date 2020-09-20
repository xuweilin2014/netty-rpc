package com.xu.test;

import com.xu.rpc.services.PersonManage;
import com.xu.rpc.services.pojo.Person;
import org.springframework.context.support.ClassPathXmlApplicationContext;

public class PojoMockCallTest {

    public static void main(String[] args) throws InterruptedException {
        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("classpath:rpc-invoke-config-client.xml");

        PersonManage manage = (PersonManage) context.getBean("personManage");

        Person p = new Person();
        p.setId(20150811);
        p.setName("XiaoHaoBaby");
        p.setAge(1);

        int result = manage.save(p);
        manage.query(p);
        System.out.println("first call pojo rpc result:" + result);

        Thread.sleep(20000);

        result = manage.save(p);
        manage.query(p);
        System.out.println("second call pojo rpc result:" + result);
    }

}
