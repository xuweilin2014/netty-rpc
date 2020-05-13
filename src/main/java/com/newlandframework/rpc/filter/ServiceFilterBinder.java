package com.newlandframework.rpc.filter;

import lombok.Data;

@Data
public class ServiceFilterBinder {
    private Object object;
    private Filter filter;
}

