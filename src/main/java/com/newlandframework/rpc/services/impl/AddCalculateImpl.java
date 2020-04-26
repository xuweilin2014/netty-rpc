package com.newlandframework.rpc.services.impl;

import com.newlandframework.rpc.services.AddCalculate;


public class AddCalculateImpl implements AddCalculate {
    //两数相加
    @Override
    public int add(int a, int b) {
        return a + b;
    }
}
