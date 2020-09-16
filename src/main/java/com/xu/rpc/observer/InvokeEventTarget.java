
package com.xu.rpc.observer;

import com.xu.rpc.event.MonitorEvent;
import com.xu.rpc.event.MonitorNotification;

import java.util.Observable;

/**
 * 被观察的对象
 */
public class InvokeEventTarget extends Observable {

    public void notify(MonitorNotification event) {
        setChanged();

        notifyObservers(event);
    }

}

