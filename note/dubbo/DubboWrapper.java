public class DubboWrapper {
    
    public static abstract class Wrapper{

        public static Wrapper getWrapper(Class<?> c) {
            while (ClassGenerator.isDynamicClass(c)) // can not wrapper on dynamic class.
                c = c.getSuperclass();
    
            if (c == Object.class)
                return OBJECT_WRAPPER;

            //访问缓存
            Wrapper ret = WRAPPER_MAP.get(c);
            if (ret == null) {
                //缓存未命中则创建Wrapper对象
                ret = makeWrapper(c);
                //写入缓存
                WRAPPER_MAP.put(c, ret);
            }
            return ret;
        }

        private static Wrapper makeWrapper(Class<?> c) {
            if (c.isPrimitive())
                throw new IllegalArgumentException("Can not create wrapper for primitive type: " + c);
    
            String name = c.getName();
            ClassLoader cl = ClassHelper.getClassLoader(c);
    
            // c1 用于存储 setPropertyValue 方法代码
            StringBuilder c1 = new StringBuilder("public void setPropertyValue(Object o, String n, Object v){ ");
            // c2 用于存储 getPropertyValue 方法代码
            StringBuilder c2 = new StringBuilder("public Object getPropertyValue(Object o, String n){ ");
            // c3 用于存储 invokeMethod 方法代码
            StringBuilder c3 = new StringBuilder("public Object invokeMethod(Object o, String n, Class[] p, Object[] v) throws " + InvocationTargetException.class.getName() + "{ ");
    
            // 生成类型转换代码及异常捕捉代码，比如：
            // DemoService w; try { w = ((DemoServcie) $1); }catch(Throwable e){ throw new IllegalArgumentException(e); }
            c1.append(name).append(" w; try{ w = ((").append(name).append(")$1); }catch(Throwable e){ throw new IllegalArgumentException(e); }");
            c2.append(name).append(" w; try{ w = ((").append(name).append(")$1); }catch(Throwable e){ throw new IllegalArgumentException(e); }");
            c3.append(name).append(" w; try{ w = ((").append(name).append(")$1); }catch(Throwable e){ throw new IllegalArgumentException(e); }");
    
            // pts 用于存储成员变量名和类型
            Map<String, Class<?>> pts = new HashMap<String, Class<?>>(); // <property name, property types>
            // ms 用于存储方法描述信息（可理解为方法签名）及 Method 实例
            Map<String, Method> ms = new LinkedHashMap<String, Method>(); // <method desc, Method instance>
            // mns 为方法名列表
            List<String> mns = new ArrayList<String>(); // method names.
            // dmns 用于存储“定义在当前类中的方法”的名称
            List<String> dmns = new ArrayList<String>(); // declaring method names.
    
            // --------------------------------✨ 分割线1 ✨-------------------------------------

            // 对于public修饰的字段（但不包括static或者transient修饰的属性），setPropertyValue方法中可以直接赋值的方式设置值，
            // 对于getPropertyValue方法中，可以直接返回的方式获取值，不需要通过getter或者setter方法。
            for (Field f : c.getFields()) {
                String fn = f.getName();
                Class<?> ft = f.getType();

                 // 忽略关键字 static 或 transient 修饰的变量
                if (Modifier.isStatic(f.getModifiers()) || Modifier.isTransient(f.getModifiers()))
                    continue;
    
                // 在setProperty方法中生成条件判断及赋值语句，比如：
                // if( $2.equals("field_name") ) { w.field_name = (java.lang.String) $3; return;}
                c1.append(" if( $2.equals(\"").append(fn).append("\") ){ w.").append(fn).append("=").append(arg(ft, "$3")).append("; return; }");

                // 在getProperty方法中生成条件判断以及返回语句，比如：
                // if( $2.equals("field_name") ) { return ($w)w.field_name; }
                c2.append(" if( $2.equals(\"").append(fn).append("\") ){ return ($w)w.").append(fn).append("; }");

                // 存储 <字段名, 字段类型> 键值对到 pts 中
                pts.put(fn, ft);
            }
    
            // --------------------------------✨ 分割线2 ✨-------------------------------------

            Method[] methods = c.getMethods();
            // 检测 c 中是否包含在当前类中声明的方法，如果methods为null，或者methods中的方法全部为Object类中的话，返回false
            boolean hasMethod = hasMethods(methods);
            if (hasMethod) {
                c3.append(" try{");
            }
            for (Method m : methods) {
                if (m.getDeclaringClass() == Object.class) //ignore Object's method.
                    // 忽略Object.class中的方法
                    continue;
    
                String mn = m.getName();

                // 生成方法名判断语句，比如：
                // if ( "sayHello".equals( $2 )
                c3.append(" if( \"").append(mn).append("\".equals( $2 ) ");
                int len = m.getParameterTypes().length;

                // 生成“运行时传入的参数数量与方法参数列表长度”判断语句，比如：
                // && $3.length == 2
                c3.append(" && ").append(" $3.length == ").append(len);
    
                boolean override = false;
                for (Method m2 : methods) {
                    // 检测方法是否存在重载情况，条件为：方法对象不同 && 方法名相同
                    if (m != m2 && m.getName().equals(m2.getName())) {
                        override = true;
                        break;
                    }
                }

                // 对重载方法进行处理，考虑下面的方法：
                //    1. void sayHello(Integer, String)
                //    2. void sayHello(Integer, Integer)
                // 方法名相同，参数列表长度也相同，因此不能仅通过这两项判断两个方法是否相等。
                // 需要进一步判断方法的每个参数类型与传入的参数类型数组中的每个参数是否相同。
                if (override) {
                    if (len > 0) {
                        for (int l = 0; l < len; l++) {
                            // 生成参数类型进行检测代码，比如：
                            // && $3[0].getName().equals("java.lang.Integer") 
                            //    && $3[1].getName().equals("java.lang.String")
                            c3.append(" && ").append(" $3[").append(l).append("].getName().equals(\"")
                                    .append(m.getParameterTypes()[l].getName()).append("\")");
                        }
                    }
                }
    
                // 添加 ) {，完成方法判断语句，此时生成的代码可能如下（已格式化）：
                // if ("sayHello".equals($2) 
                //     && $3.length == 2
                //     && $3[0].getName().equals("java.lang.Integer") 
                //     && $3[1].getName().equals("java.lang.String")) {
                c3.append(" ) { ");
    
                // 根据返回值类型生成目标方法调用语句
                if (m.getReturnType() == Void.TYPE)
                    // w.sayHello((java.lang.Integer)$4[0], (java.lang.String)$4[1]); return null;
                    c3.append(" w.").append(mn).append('(').append(args(m.getParameterTypes(), "$4")).append(");").append(" return null;");
                else
                    // return w.sayHello((java.lang.Integer)$4[0], (java.lang.String)$4[1]);
                    c3.append(" return ($w)w.").append(mn).append('(').append(args(m.getParameterTypes(), "$4")).append(");");
    
                c3.append(" }");
    
                // 添加 }, 生成的代码形如（已格式化）：
                // if ("sayHello".equals($2) 
                //     && $3.length == 2
                //     && $3[0].getName().equals("java.lang.Integer") 
                //     && $3[1].getName().equals("java.lang.String")) {
                //
                //     w.sayHello((java.lang.Integer)$4[0], (java.lang.String)$4[1]); 
                //     return null;
                // }

                // 添加方法名到 mns 集合中
                mns.add(mn);
                // 检测当前方法是否在 c 中被声明的
                if (m.getDeclaringClass() == c)
                    //若是，则将当前方法添加到dmns中
                    dmns.add(mn);
                ms.put(ReflectUtils.getDesc(m), m);
            }
            if (hasMethod) {
                c3.append(" } catch(Throwable e) { ");
                c3.append("     throw new java.lang.reflect.InvocationTargetException(e); ");
                c3.append(" }");
            }
    
            c3.append(" throw new " + NoSuchMethodException.class.getName() + "(\"Not found method \\\"\"+$2+\"\\\" in class " + c.getName() + ".\"); }");
    
            // --------------------------------✨ 分割线3 ✨-------------------------------------

            // deal with get/set method.
            Matcher matcher;
            // 处理get/set方法
            for (Map.Entry<String, Method> entry : ms.entrySet()) {
                String md = entry.getKey();
                Method method = (Method) entry.getValue();

                // 匹配以 get 开头的方法
                if ((matcher = ReflectUtils.GETTER_METHOD_DESC_PATTERN.matcher(md)).matches()) {
                    // 获取属性名
                    String pn = propertyName(matcher.group(1));
                    // 生成属性判断以及返回语句，示例如下：
                    // if( $2.equals("name") ) { return ($w).w.getName(); }
                    c2.append(" if( $2.equals(\"").append(pn).append("\") ){ return ($w)w.").append(method.getName()).append("(); }");
                    pts.put(pn, method.getReturnType());

                // 匹配以 is/has/can 开头的方法
                } else if ((matcher = ReflectUtils.IS_HAS_CAN_METHOD_DESC_PATTERN.matcher(md)).matches()) {
                    String pn = propertyName(matcher.group(1));
                    // 生成属性判断以及返回语句，示例如下：
                    // if( $2.equals("dream") ) { return ($w).w.hasDream(); }
                    c2.append(" if( $2.equals(\"").append(pn).append("\") ){ return ($w)w.").append(method.getName()).append("(); }");
                    pts.put(pn, method.getReturnType());
                
                // 匹配以 set 开头的方法
                } else if ((matcher = ReflectUtils.SETTER_METHOD_DESC_PATTERN.matcher(md)).matches()) {
                    Class<?> pt = method.getParameterTypes()[0];
                    String pn = propertyName(matcher.group(1));
                    // 生成属性判断以及 setter 调用语句，示例如下：
                    // if( $2.equals("name") ) { w.setName((java.lang.String)$3); return; }
                    c1.append(" if( $2.equals(\"").append(pn).append("\") ){ w.").append(method.getName()).append("(").append(arg(pt, "$3")).append("); return; }");
                    pts.put(pn, pt);
                }
            }

            // 添加 NoSuchPropertyException 异常抛出代码
            c1.append(" throw new " + NoSuchPropertyException.class.getName() + "(\"Not found property \\\"\"+$2+\"\\\" filed or setter method in class " + c.getName() + ".\"); }");
            c2.append(" throw new " + NoSuchPropertyException.class.getName() + "(\"Not found property \\\"\"+$2+\"\\\" filed or setter method in class " + c.getName() + ".\"); }");
    
            // --------------------------------✨ 分割线4 ✨-------------------------------------

            // make class
            long id = WRAPPER_CLASS_COUNTER.getAndIncrement();
            // 创建类生成器
            ClassGenerator cc = ClassGenerator.newInstance(cl);
            // 设置类名及超类
            cc.setClassName((Modifier.isPublic(c.getModifiers()) ? Wrapper.class.getName() : c.getName() + "$sw") + id);
            cc.setSuperClass(Wrapper.class);
            
            // 添加默认构造方法
            cc.addDefaultConstructor();

            // 添加static字段
            cc.addField("public static String[] pns;"); // property name array.
            cc.addField("public static " + Map.class.getName() + " pts;"); // property type map.
            cc.addField("public static String[] mns;"); // all method name array.
            cc.addField("public static String[] dmns;"); // declared method name array.
            for (int i = 0, len = ms.size(); i < len; i++)
                cc.addField("public static Class[] mts" + i + ";");
    
            // 添加方法代码
            cc.addMethod("public String[] getPropertyNames(){ return pns; }");
            cc.addMethod("public boolean hasProperty(String n){ return pts.containsKey($1); }");
            cc.addMethod("public Class getPropertyType(String n){ return (Class)pts.get($1); }");
            cc.addMethod("public String[] getMethodNames(){ return mns; }");
            cc.addMethod("public String[] getDeclaredMethodNames(){ return dmns; }");
            cc.addMethod(c1.toString());
            cc.addMethod(c2.toString());
            cc.addMethod(c3.toString());
    
            try {
                Class<?> wc = cc.toClass();

                // 设置静态字段的值
                wc.getField("pts").set(null, pts);
                wc.getField("pns").set(null, pts.keySet().toArray(new String[0]));
                wc.getField("mns").set(null, mns.toArray(new String[0]));
                wc.getField("dmns").set(null, dmns.toArray(new String[0]));
                int ix = 0;

                for (Method m : ms.values())
                    wc.getField("mts" + ix++).set(null, m.getParameterTypes());

                //创建Wrapper实例
                return (Wrapper) wc.newInstance();
            } catch (RuntimeException e) {
                throw e;
            } catch (Throwable e) {
                throw new RuntimeException(e.getMessage(), e);
            } finally {
                cc.release();
                ms.clear();
                mns.clear();
                dmns.clear();
            }
        }
    }

}