package com.xu.rpc.registry;

public interface KeeperStateListener {

    public static final int RECONNECTED = 0;

    public static final int CONNECTED = 1;

    public static final int DISCONNECTED = 2;

    public void stateChanged(int state);

}
