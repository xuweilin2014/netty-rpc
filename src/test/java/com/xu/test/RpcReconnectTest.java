package com.xu.test;

import com.xu.rpc.services.PersonManage;
import com.xu.rpc.services.pojo.Person;
import org.springframework.context.support.ClassPathXmlApplicationContext;

public class RpcReconnectTest {

    public static void main(String[] args) throws InterruptedException {
        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("classpath:rpc-invoke-config-client.xml");

        PersonManage manage = (PersonManage) context.getBean("personManage");

        Person p = new Person();
        p.setId(20150811);
        p.setName("XiaoHaoBaby");
        p.setAge(1);

        while (true){
            Thread.sleep(1000);
        }
    }

}
