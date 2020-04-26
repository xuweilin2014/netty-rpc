package com.newlandframework.rpc.model;

import java.util.Map;

/**
 * RPC服务接口定义、服务接口实现绑定关系容器定义，提供给spring作为容器使用
 */
public class MessageKeyVal {

    private Map<String, Object> messageKeyVal;

    public void setMessageKeyVal(Map<String, Object> messageKeyVal) {
        this.messageKeyVal = messageKeyVal;
    }

    public Map<String, Object> getMessageKeyVal() {
        return messageKeyVal;
    }
}


