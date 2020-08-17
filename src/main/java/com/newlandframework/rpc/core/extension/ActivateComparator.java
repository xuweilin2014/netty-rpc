package com.newlandframework.rpc.core.extension;

import java.util.Comparator;

public class ActivateComparator implements Comparator<Object> {

    public static final ActivateComparator COMPARATOR = new ActivateComparator();

    @Override
    public int compare(Object o1, Object o2) {
        if (o1 == null && o2 == null)
            return 0;

        if (o1 == null)
            return -1;

        if (o2 == null)
            return 1;

        if (o1.equals(o2))
            return 0;

        Activate activate1 = o1.getClass().getAnnotation(Activate.class);
        Activate activate2 = o2.getClass().getAnnotation(Activate.class);

        return Integer.compare(activate1.order(), activate2.order());
    }
}
