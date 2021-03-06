package com.xu.rpc.commons.util;

import com.google.common.collect.ImmutableMap;
import com.xu.rpc.commons.exception.RpcException;

import java.io.Serializable;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.Constructor;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Array;
import java.lang.reflect.Executable;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class ReflectionUtils {
    private static ImmutableMap.Builder<Class, Object> builder = ImmutableMap.builder();
    private StringBuilder provider = new StringBuilder();

    public StringBuilder getProvider() {
        return provider;
    }

    public void clearProvider() {
        provider.delete(0, provider.length());
    }

    static {
        builder.put(Boolean.class, Boolean.FALSE);
        builder.put(Byte.class, Byte.valueOf((byte) 0));
        builder.put(Character.class, Character.valueOf((char) 0));
        builder.put(Short.class, Short.valueOf((short) 0));
        builder.put(Double.class, Double.valueOf(0));
        builder.put(Float.class, Float.valueOf(0));
        builder.put(Integer.class, Integer.valueOf(0));
        builder.put(Long.class, Long.valueOf(0));
        builder.put(boolean.class, Boolean.FALSE);
        builder.put(byte.class, Byte.valueOf((byte) 0));
        builder.put(char.class, Character.valueOf((char) 0));
        builder.put(short.class, Short.valueOf((short) 0));
        builder.put(double.class, Double.valueOf(0));
        builder.put(float.class, Float.valueOf(0));
        builder.put(int.class, Integer.valueOf(0));
        builder.put(long.class, Long.valueOf(0));
    }

    public static Class<?>[] filterInterfaces(Class<?>[] proxyClasses) {
        Set<Class<?>> interfaces = new HashSet<Class<?>>();
        for (Class<?> proxyClass : proxyClasses) {
            if (proxyClass.isInterface()) {
                interfaces.add(proxyClass);
            }
        }

        interfaces.add(Serializable.class);
        return interfaces.toArray(new Class[interfaces.size()]);
    }

    public static Class<?>[] filterNonInterfaces(Class<?>[] proxyClasses) {
        Set<Class<?>> superclasses = new HashSet<Class<?>>();
        for (Class<?> proxyClass : proxyClasses) {
            if (!proxyClass.isInterface()) {
                superclasses.add(proxyClass);
            }
        }

        return superclasses.toArray(new Class[superclasses.size()]);
    }

    public static boolean existDefaultConstructor(Class<?> superclass) {
        final Constructor<?>[] declaredConstructors = superclass.getDeclaredConstructors();
        for (int i = 0; i < declaredConstructors.length; i++) {
            Constructor<?> constructor = declaredConstructors[i];
            boolean exist = (constructor.getParameterTypes().length == 0 &&
                    (Modifier.isPublic(constructor.getModifiers()) || Modifier.isProtected(constructor.getModifiers())));
            if (exist) {
                return true;
            }
        }

        return false;
    }

    public static Class<?> getParentClass(Class<?>[] proxyClasses) {
        final Class<?>[] parent = filterNonInterfaces(proxyClasses);
        switch (parent.length) {
            case 0:
                return Object.class;
            case 1:
                Class<?> superclass = parent[0];
                if (Modifier.isFinal(superclass.getModifiers())) {
                    throw new RpcException(
                            "proxy can't build " + superclass.getName() + " because it is final");
                }
                if (!existDefaultConstructor(superclass)) {
                    throw new RpcException(
                            "proxy can't build " + superclass.getName() + ", because it has no default constructor");
                }

                return superclass;
            default:
                StringBuilder errorMessage = new StringBuilder("proxy class can't build");
                for (int i = 0; i < parent.length; i++) {
                    Class<?> c = parent[i];
                    errorMessage.append(c.getName());
                    if (i != parent.length - 1) {
                        errorMessage.append(", ");
                    }
                }

                errorMessage.append("; multiple implement not allowed");
                throw new RpcException(errorMessage.toString());
        }
    }

    public static boolean isHashCodeMethod(Method method) {
        return "hashCode".equals(method.getName()) && Integer.TYPE.equals(method.getReturnType())
                && method.getParameterTypes().length == 0;
    }

    public static boolean isEqualsMethod(Method method) {
        return "equals".equals(method.getName()) && Boolean.TYPE.equals(method.getReturnType())
                && method.getParameterTypes().length == 1 && Object.class.equals(method.getParameterTypes()[0]);
    }

    public static Object newInstance(Class<?> type) {
        Constructor<?> constructor = null;
        Object[] args = new Object[0];
        try {
            constructor = type.getConstructor();
        } catch (NoSuchMethodException e) {
        }

        if (constructor == null) {
            Constructor<?>[] constructors = type.getConstructors();
            if (constructors.length == 0) {
                return null;
            }
            constructor = constructors[0];
            Class<?>[] params = constructor.getParameterTypes();
            args = new Object[params.length];
            for (int i = 0; i < params.length; i++) {
                args[i] = getDefaultVal(params[i]);
            }
        }

        try {
            return constructor.newInstance(args);
        } catch (InstantiationException
                | IllegalAccessException
                | InvocationTargetException e) {
            e.printStackTrace();
        }
        return null;
    }

    public static Object getDefaultVal(Class cl) {
        if (cl.isArray()) {
            return Array.newInstance(cl.getComponentType(), 0);
        } else if (cl.isPrimitive() || builder.build().containsKey(cl)) {
            return builder.build().get(cl);
        } else {
            return newInstance(cl);
        }
    }

    public static String[] getMethodNames(Class<?> cls){
        Method[] methods = cls.getDeclaredMethods();
        String[] methodNames = new String[methods.length];
        for (int i = 0; i < methodNames.length; i++) {
            methodNames[i] = methods[i].getName();
        }

        return methodNames;
    }

    public static Class<?> getGenericClass(ParameterizedType parameterizedType, int i) {
        Object genericClass = parameterizedType.getActualTypeArguments()[i];

        if (genericClass instanceof GenericArrayType) {
            return (Class<?>) ((GenericArrayType) genericClass).getGenericComponentType();
        } else if (genericClass instanceof ParameterizedType) {
            return (Class<?>) ((ParameterizedType) genericClass).getRawType();
        } else {
            return (Class<?>) genericClass;
        }
    }

    private static String modifiers(int m) {
        return m != 0 ? Modifier.toString(m) + " " : "";
    }

    private String getType(Class<?> t) {
        StringBuilder brackets = new StringBuilder();
        while (t.isArray()) {
            brackets.append("[]");
            t = t.getComponentType();
        }
        return t.getName() + brackets;
    }

    private void listTypes(Class<?>[] types) {
        for (int i = 0; i < types.length; i++) {
            if (i > 0) {
                provider.append(", ");
            }
            provider.append(getType(types[i]));
        }
    }

    private void listField(Field f, boolean html) {
        provider.append((html ? "&nbsp&nbsp" : "  ") + modifiers(f.getModifiers()) +
                getType(f.getType()) + " " +
                f.getName() + (html ? ";<br>" : ";\n"));
    }

    public void listMethod(Executable member, boolean html) {
        provider.append(html ? "<br>&nbsp&nbsp" : "\n  " + modifiers(member.getModifiers() & (~Modifier.FINAL)));
        if (member instanceof Method) {
            provider.append(getType(((Method) member).getReturnType()) + " ");
        }
        provider.append(member.getName() + "(");
        listTypes(member.getParameterTypes());
        provider.append(")");
        Class<?>[] exceptions = member.getExceptionTypes();
        if (exceptions.length > 0) {
            provider.append(" throws ");
        }
        listTypes(exceptions);
        provider.append(";");
    }

    public void listRpcProviderDetail(Class<?> c, boolean html) {
        if (c.isInterface()) {
            provider.append(Modifier.toString(c.getModifiers()) + " " + c.getName());
            provider.append(html ? " {<br>" : " {\n");

            boolean hasFields = false;
            Field[] fields = c.getDeclaredFields();
            if (fields.length != 0) {
                provider.append(html ? "&nbsp&nbsp//&nbspFields<br>" : "  // Fields\n");
                hasFields = true;
                for (Field field : fields) {
                    listField(field, html);
                }
            }

            provider.append(hasFields ? (html ? "<br>&nbsp&nbsp//&nbspMethods" : "\n  // Methods") : (html ? "&nbsp&nbsp//&nbspMethods" : "  // Methods"));
            Method[] methods = c.getDeclaredMethods();
            for (Method method : methods) {
                listMethod(method, html);
            }
            provider.append(html ? "<br>}<p>" : "\n}\n\n");
        }
    }

    public static Method getDeclaredMethod(Class<?> cls, String methodName, Class<?>... parameterTypes) {
        Method method = null;
        Class<?> searchType = cls;
        while (searchType != null) {
            method = findDeclaredMethod(searchType, methodName, parameterTypes);
            if (method != null) {
                return method;
            }
            searchType = searchType.getSuperclass();
        }
        return method;
    }

    public static Method findDeclaredMethod(final Class<?> cls, final String methodName, final Class<?>... parameterTypes) {
        Method method = null;
        try {
            method = cls.getDeclaredMethod(methodName, parameterTypes);
            return method;
        } catch (NoSuchMethodException e) {
            if (method == null) {
                for (Method m : cls.getDeclaredMethods()) {
                    if (m.getName().equals(methodName)) {
                        boolean find = true;
                        Class[] paramType = m.getParameterTypes();
                        if (paramType.length != parameterTypes.length) {
                            continue;
                        }
                        for (int i = 0; i < parameterTypes.length; i++) {
                            if (!paramType[i].isAssignableFrom(parameterTypes[i])) {
                                find = false;
                                break;
                            }
                        }
                        if (find) {
                            method = m;
                            break;
                        }
                    }
                }
            }
        }
        return method;
    }

    private String getClassType(Class<?>[] types) {
        StringBuilder type = new StringBuilder();
        for (int i = 0; i < types.length; i++) {
            if (i > 0) {
                type.append(", ");
            }
            type.append(getType(types[i]));
        }
        return type.toString();
    }

    public List<String> getClassMethodSignature(Class<?> cls) {
        List<String> list = new ArrayList<String>();
        if (cls.isInterface()) {
            Method[] methods = cls.getDeclaredMethods();
            StringBuilder signatureMethod = new StringBuilder();
            for (Method member : methods) {
                int modifiers = member.getModifiers();
                if (Modifier.isAbstract(modifiers) && Modifier.isPublic(modifiers)) {
                    signatureMethod.append(modifiers(Modifier.PUBLIC));
                    if (Modifier.isFinal(modifiers)) {
                        signatureMethod.append(modifiers(Modifier.FINAL));
                    }
                } else {
                    signatureMethod.append(modifiers);
                }

                if (member instanceof Method) {
                    signatureMethod.append(getType(((Method) member).getReturnType()) + " ");
                }

                signatureMethod.append(member.getName() + "(");
                signatureMethod.append(getClassType(member.getParameterTypes()));
                signatureMethod.append(")");
                Class<?>[] exceptions = member.getExceptionTypes();
                if (exceptions.length > 0) {
                    signatureMethod.append(" throws ");
                }
                listTypes(exceptions);
                list.add(signatureMethod.toString());
                signatureMethod.delete(0, signatureMethod.length());
            }
        }
        return list;
    }

    public String getMethodSignature(Method method){

        StringBuilder signatureMethod = new StringBuilder();

        int modifiers = method.getModifiers();
        if (Modifier.isAbstract(modifiers) && Modifier.isPublic(modifiers)) {
            signatureMethod.append(modifiers(Modifier.PUBLIC));
            if (Modifier.isFinal(modifiers)) {
                signatureMethod.append(modifiers(Modifier.FINAL));
            }
        } else {
            signatureMethod.append(modifiers);
        }

        signatureMethod.append(getType(((Method) method).getReturnType())).append(" ");
        signatureMethod.append(method.getName()).append("(");
        signatureMethod.append(getClassType(method.getParameterTypes()));
        signatureMethod.append(")");

        Class<?>[] exceptions = method.getExceptionTypes();
        if (exceptions.length > 0) {
            signatureMethod.append(" throws ");
        }

        listTypes(exceptions);

        return signatureMethod.toString();
    }
}

