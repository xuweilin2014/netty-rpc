
package com.xu.rpc.observer;

import com.xu.rpc.event.ModuleEvent;

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

