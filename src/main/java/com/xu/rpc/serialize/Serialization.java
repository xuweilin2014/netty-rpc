package com.xu.rpc.serialize;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public enum Serialization {

    JDKSERIALIZE("jdk"),

    KRYOSERIALIZE("kryo"),

    HESSIANSERIALIZE("hessian");

    private String serialize;

    private Serialization(String serialize) {
        this.serialize = serialize;
    }

    @Override
    public String toString() {
        ReflectionToStringBuilder.setDefaultStyle(ToStringStyle.SHORT_PREFIX_STYLE);
        return ReflectionToStringBuilder.toString(this);
    }

    public String getSerialize() {
        return serialize;
    }
}
