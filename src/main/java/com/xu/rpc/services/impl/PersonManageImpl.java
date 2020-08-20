package com.xu.rpc.services.impl;

import com.xu.rpc.services.PersonManage;
import com.xu.rpc.services.pojo.Person;

import java.util.concurrent.TimeUnit;

public class PersonManageImpl implements PersonManage {
    @Override
    public int save(Person p) {
        //your business logic code here!
        System.out.println("person data[" + p + "] has save!");
        return 0;
    }

    @Override
    public void query(Person p) {
        //your business logic code here!
        try {
            TimeUnit.SECONDS.sleep(1);
            System.out.println("person data[" + p + "] has query!");
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void query(long timeout) {
        //your business logic code here!
        try {
            TimeUnit.SECONDS.sleep(timeout);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void check() {
        throw new RuntimeException("person check fail!");
    }

    @Override
    public boolean checkAge(Person p) {
        if (p.getAge() < 18) {
            throw new RuntimeException("person check age fail!");
        } else {
            System.out.println("person check age succ!");
            return true;
        }
    }
}

