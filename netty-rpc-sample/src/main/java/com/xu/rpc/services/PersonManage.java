package com.xu.rpc.services;

import com.xu.rpc.services.pojo.Person;

/**
 *
 */
public interface PersonManage {
    int save(Person p);

    void query(Person p);

    void query(long timeout);

    void check();

    boolean checkAge(Person p);
}
