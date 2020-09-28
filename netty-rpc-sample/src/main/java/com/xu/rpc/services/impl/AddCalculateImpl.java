package com.xu.rpc.services.impl;

import com.xu.rpc.services.AddCalculate;


public class AddCalculateImpl implements AddCalculate {
    //两数相加
    @Override
    public int add(int a, int b) {
        return a + b;
    }
}
