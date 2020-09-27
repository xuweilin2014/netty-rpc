package com.xu.rpc.services.impl;

import com.xu.rpc.services.pojo.CostTime;
import com.xu.rpc.services.CostTimeCalculate;


public class CostTimeCalculateImpl implements CostTimeCalculate {
    @Override
    public CostTime calculate() {
        CostTime elapse = new CostTime();
        try {
            long start = 0, end = 0;
            start = System.currentTimeMillis();
            //模拟耗时操作
            Thread.sleep(5 * 1000);
            end = System.currentTimeMillis();

            long interval = end - start;
            elapse.setElapse(interval);
            elapse.setDetail("I'm XiaoHaoBaby,cost time operate succ!");
            System.out.println("calculate time:" + interval);
            return elapse;
        } catch (InterruptedException e) {
            e.printStackTrace();
            elapse.setDetail("I'm XiaoHaoBaby,cost time operate fail!");
            return elapse;
        }
    }

    @Override
    public CostTime busy() {
        CostTime elapse = new CostTime();
        try {
            long start = 0, end = 0;
            start = System.currentTimeMillis();
            //模拟耗时操作,超过nettyrpc.default.msg.timeout定义的上限
            Thread.sleep(10 * 1000L);
            end = System.currentTimeMillis();

            long interval = end - start;
            elapse.setElapse(interval);
            elapse.setDetail("I'm XiaoHao,I'm busy now!");
            System.out.println("calculate time:" + interval);
            return elapse;
        } catch (InterruptedException e) {
            e.printStackTrace();
            elapse.setDetail("I'm XiaoHao,I'm handle error now!");
            return elapse;
        }
    }
}

