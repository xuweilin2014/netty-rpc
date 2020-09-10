package com.xu.rpc.commons.limiter;

public class TimestampHolder {

    private long timestamp;

    public TimestampHolder(long timestamp) {
        this.timestamp = timestamp;
    }

    public long getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(long timestamp) {
        this.timestamp = timestamp;
    }
}
