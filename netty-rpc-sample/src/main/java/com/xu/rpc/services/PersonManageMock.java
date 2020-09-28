package com.xu.rpc.services;

import com.xu.rpc.services.pojo.Person;

public class PersonManageMock implements PersonManage {
    @Override
    public int save(Person p) {
        return 7;
    }

    @Override
    public void query(Person p) {
        System.out.println("query in PersonManageMock");
    }

    @Override
    public void query(long timeout) {

    }

    @Override
    public void check() {

    }

    @Override
    public boolean checkAge(Person p) {
        return false;
    }
}
