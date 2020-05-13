
package com.newlandframework.rpc.observer;

import com.newlandframework.rpc.event.ModuleEvent;

import java.util.Observable;

/**
 * 被观察的对象
 */
public class InvokeEventTarget extends Observable {
    public void notify(ModuleEvent event) {
        setChanged();
        notifyObservers(event);
    }
}

