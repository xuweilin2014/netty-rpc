package com.newlandframework.rpc.services;


import com.newlandframework.rpc.services.pojo.CostTime;

public interface CostTimeCalculate {
    CostTime calculate();

    CostTime busy();
}

