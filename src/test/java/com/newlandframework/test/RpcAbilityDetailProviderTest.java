package com.newlandframework.test;

import com.newlandframework.rpc.core.AbilityDetail;
import org.springframework.context.support.ClassPathXmlApplicationContext;

public class RpcAbilityDetailProviderTest {
    public static void main(String[] args) {
        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("classpath:rpc-invoke-config-client.xml");

        AbilityDetail provider = (AbilityDetail) context.getBean("ability");

        StringBuilder ability = provider.listAbilityDetail(false);

        System.out.println(ability);

        context.destroy();
    }
}

