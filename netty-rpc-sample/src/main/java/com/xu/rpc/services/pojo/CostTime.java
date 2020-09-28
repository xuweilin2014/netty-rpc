package com.xu.rpc.services.pojo;

import java.io.Serializable;


public class CostTime implements Serializable {
    public long elapse;
    public String detail;

    public long getElapse() {
        return elapse;
    }

    public void setElapse(long elapse) {
        this.elapse = elapse;
    }

    public String getDetail() {
        return detail;
    }

    public void setDetail(String detail) {
        this.detail = detail;
    }

    @Override
    public String toString() {
        return "CostTime [elapse=" + elapse + ", detail=" + detail + "]";
    }
}
