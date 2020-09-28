package com.xu.rpc.services;

import com.xu.rpc.services.pojo.Person;
import org.apache.log4j.Logger;

public class PersonManageStub implements PersonManage {

    private PersonManage personManage;

    private static final Logger logger = Logger.getLogger(PersonManageStub.class);

    public PersonManageStub(PersonManage personManage){
        this.personManage = personManage;
    }

    @Override
    public int save(Person p) {
        logger.info("before execute save method");
        try {
            int save = personManage.save(p);
            logger.info("after executing save method.");
            return save;
        } catch (Exception e) {
            logger.error("error occurs " + e.getMessage());
            return -1;
        }
    }

    @Override
    public void query(Person p) {
        logger.info("before execute query method");
        try {
            personManage.query(p);
            logger.info("after executing query method.");
        } catch (Exception e) {
            logger.info("error occurs " + e.getMessage());
        }
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
