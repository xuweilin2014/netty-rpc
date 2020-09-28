package com.xu.rpc.services;


import com.xu.rpc.services.pojo.CostTime;

public interface CostTimeCalculate {
    CostTime calculate();

    CostTime busy();
}

