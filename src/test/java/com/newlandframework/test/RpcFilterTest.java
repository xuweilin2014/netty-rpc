package com.newlandframework.test;

import com.newlandframework.rpc.exception.RejectResponeException;
import com.newlandframework.rpc.services.Cache;
import com.newlandframework.rpc.services.Store;
import org.springframework.context.support.ClassPathXmlApplicationContext;

/**
 * 过滤器Filter测试
 */
public class RpcFilterTest {
    public static void main(String[] args) {
        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("classpath:rpc-invoke-config-client.xml");

        Cache cache = (Cache) context.getBean("cache");

        for (int i = 0; i < 1; i++) {
            String obj = String.valueOf(i);
            try {
                cache.put(obj, obj);
            } catch (RejectResponeException ex) {
                System.out.println("trace:" + ex.getMessage());
            }
        }

        for (int i = 0; i < 100; i++) {
            String obj = String.valueOf(i);
            try {
                System.out.println((String) cache.get(obj));
            } catch (RejectResponeException ex) {
                System.out.println("trace:" + ex.getMessage());
            }
        }

        Store store = (Store) context.getBean("store");

        for (int i = 0; i < 100; i++) {
            String obj = String.valueOf(i);
            try {
                store.save(obj);
                store.save(i);
            } catch (RejectResponeException ex) {
                System.out.println("trace:" + ex.getMessage());
            }
        }
        context.destroy();
    }
}

