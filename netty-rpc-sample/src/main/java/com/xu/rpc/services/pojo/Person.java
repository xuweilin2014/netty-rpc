
package com.xu.rpc.services.pojo;

import java.io.Serializable;
import java.util.Date;

public class Person implements Serializable {
    private int id;
    private String name;
    private int age;
    private Date birthday;

    public Date getBirthday() {
        return birthday;
    }

    public void setBirthday(Date birthday) {
        this.birthday = birthday;
    }

    public int getId() {

        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public int getAge() {
        return age;
    }

    public void setAge(int age) {
        this.age = age;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return birthday != null ? String.format("Person <<id:%d name:%s age:%d birthday:%s>>", id, name, age, birthday) : String.format("Person <<id:%d name:%s age:%d>>", id, name, age);
    }
}

