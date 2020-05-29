public class ThreadLocalAnalysis {
    
    /**
     * 在多线程中ThreadLocal会为每一个线程创建单独的变量副本; 当使用ThreadLocal来维护变量时, 
     * ThreadLocal会为每个线程创建单独的变量副本, 避免因多线程操作共享变量而导致的数据不一致的情况;
     * 注意，每个ThreadLocal对象只能够维护一个变量。
     */
    /**
     * ThreadLocal的使用示例如下，使用线程池创建线程，在每个线程运行时，都会执行dbConnectionLocal.get()语句。由于线程是第一次执行get操作，因此
     * 会在每个线程内部（也就是Thread对象中）创建一个ThreadLocal.ThreadLocalMap对象，赋值给Thread的threadLocals属性。ThreadLocalMap
     * 包含一个Entry数组，每个Entry保存了Key-Val键值对，Key是ThreadLocal对象。所以前面在创建好ThreadLocalMap对象之后，还要把ThreadLocal对象
     * 也就是dbConnectionLocal对象和通过initialValue获取到的值Value保存到ThreadLocalMap中的Entry数组中。
     */
    public static class DBConnectionFactory {

        final static ThreadPoolExecutor poolExecutor = 
                new ThreadPoolExecutor(5, 5, 1, TimeUnit.MINUTES, new LinkedBlockingQueue<>());

        private static final ThreadLocal<Connection> dbConnectionLocal = new ThreadLocal<Connection>() {
            @Override
            protected Connection initialValue() {
                try {
                    return DriverManager.getConnection("", "", "");
                } catch (SQLException e) {
                    e.printStackTrace();
                }
                return null;
            }
        };
    
        public Connection getConnection() {
            return dbConnectionLocal.get();
        }

        public static void main(String[] args) throws InterruptedException {
            Thread.sleep(5000 * 4);
            for (int i = 0; i < 50; ++i) {
                poolExecutor.execute(new Runnable() {
                    public void run() {
                        System.out.println("use local connection" + dbConnectionLocal.get());
                        dbConnectionLocal.remove();
                    }
                });
            }
            System.out.println("pool execute over");
        }
    }

}