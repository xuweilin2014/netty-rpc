package com.xu.rpc.test;

import com.xu.rpc.commons.exception.InvokeTimeoutException;
import com.xu.rpc.services.PersonManage;
import com.xu.rpc.services.pojo.Person;
import org.springframework.context.support.ClassPathXmlApplicationContext;


public class PojoTimeoutCallTest {
    public static void main(String[] args) {
        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("classpath:rpc-invoke-config-client.xml");

        PersonManage manage = (PersonManage) context.getBean("personManage");

        // NettyRPC default timeout is 30s.you can define it by nettyrpc.default.msg.timeout environment variable.
        // if rpc call timeout,NettyRPC can throw InvokeTimeoutException.
        try {
            Person p = new Person();
            p.setId(20150811);
            p.setName("XiaoHaoBaby");
            p.setAge(1);
            manage.save(p);
        } catch (InvokeTimeoutException e) {
            System.err.println(e.getMessage());
        } finally {
            context.destroy();
        }
    }
}

