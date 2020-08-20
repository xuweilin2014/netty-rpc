
package com.xu.rpc.async;

import lombok.Data;

@Data
public class AsyncCallStatus {
    private long startTime;
    private long elapseTime;
    private CallStatus status;

    public AsyncCallStatus(long startTime, long elapseTime, CallStatus status) {
        this.startTime = startTime;
        this.elapseTime = elapseTime;
        this.status = status;
    }


    @Override
    public String toString() {
        return "AsyncLoadStatus [status=" + status + ", startTime=" + startTime + ", elapseTime=" + elapseTime + "]";
    }
}

