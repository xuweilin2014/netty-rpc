public class ListenableFutureAnalysis {

    /**
     * ListenableFuture的继承关系如下所示：
     * TrustedListenableFutureTask -> AbstractFuture.TrustedFuture -> AbstractFuture -> ListenableFuture -> Future
     */

    public static class ListenableFutureTest {
        public static void main(String[] args) {
            testListenFuture();
        }

        public static void testListenFuture() {
            System.out.println("主线程start");
            ListeningExecutorService pool = MoreExecutors.listeningDecorator(Executors.newFixedThreadPool(5));

            Task task1 = new Task();
            task1.args = "task1";
            Task task2 = new Task();
            task2.args = "task2";
            ListenableFuture<String> future = pool.submit(task1);
            ListenableFuture<String> future2 = pool.submit(task2);

            future2.addListener(() -> System.out.println("addListener 不能带返回值"), pool);

            /**
             * FutureCallBack接口可以对每个任务的成功或失败单独做出响应
             */
            FutureCallback<String> futureCallback = new FutureCallback<String>() {
                @Override
                public void onSuccess(String result) {
                    System.out.println("Futures.addCallback 能带返回值：" + result);
                }

                @Override
                public void onFailure(Throwable t) {
                    System.out.println("出错,业务回滚或补偿");
                }
            };

            // 为任务绑定回调接口
            Futures.addCallback(future, futureCallback, pool);
            System.out.println("主线程end");
        }
    }

    class Task implements Callable<String> {
        String args;

        @Override
        public String call() throws Exception {
            Thread.sleep(1000);
            System.out.println("任务：" + args);
            return "dong";
        }
    }

    private static class ListeningDecorator extends AbstractListeningExecutorService {

        private final ExecutorService delegate;

        //MoreExecutors.listeningDecorator就是包装了一下ThreadPoolExecutor，目的是为了使用ListenableFuture
        //这里的delegate其实就是ThreadPoolExecutor
        ListeningDecorator(ExecutorService delegate) {
            this.delegate = checkNotNull(delegate);
        }

        //重写了execute，不过还是直接调用ThreadPoolExecutor里面的execute
        @Override
        public final void execute(Runnable command) {
            delegate.execute(command);
        }
    }

    public abstract class AbstractListeningExecutorService extends AbstractExecutorService
            implements ListeningExecutorService {

        @Override
        protected final <T> RunnableFuture<T> newTaskFor(Runnable runnable, T value) {
            return TrustedListenableFutureTask.create(runnable, value);
        }

        //调用TrustedListenableFutureTask类中的静态create方法，返回一个实现了RunnableFuture
        //接口的TrustedListenableFutureTask类对象，放入到线程池中去执行
        //class:AbstractListeningExecutorService
        @Override
        protected final <T> RunnableFuture<T> newTaskFor(Callable<T> callable) {
            return TrustedListenableFutureTask.create(callable);
        }

        @Override
        public ListenableFuture<?> submit(Runnable task) {
            return (ListenableFuture<?>) super.submit(task);
        }

        @Override
        public <T> ListenableFuture<T> submit(Runnable task, @Nullable T result) {
            return (ListenableFuture<T>) super.submit(task, result);
        }

        //在Test中，pool.submit(task1)，往线程池中提交任务时，就会调用下面的submit方法，
        //然后调用父类的submit方法
        //class:AbstractListeningExecutorService
        @Override
        public <T> ListenableFuture<T> submit(Callable<T> task) {
            return (ListenableFuture<T>) super.submit(task);
        }
    }

    //class:AbstractExecutorService
    public <T> Future<T> submit(Callable<T> task) {
        if (task == null)
            throw new NullPointerException();
        //在这里会调用AbstractListeningExecutorService类中重写的newTaskFor方法
        RunnableFuture<T> ftask = newTaskFor(task);
        execute(ftask);
        return ftask;
    }

    /**
     * 在AbstractExecutorService#submit方法中，会通过newTaskFor创建一个TrustedListenableFutureTask类对象，放入到
     * 线程池中去执行，接着会调用TrustedListenableFutureTask#run方法。由于this.task具体是一个TrustedFutureInterruptibleTask
     * 类对象，因此会接着调用InterruptibleTask中的run方法
     */
    static class TrustedListenableFutureTask<V> extends AbstractFuture.TrustedFuture<V> implements RunnableFuture<V> {

        static <V> TrustedListenableFutureTask<V> create(Callable<V> callable) {
            return new TrustedListenableFutureTask<V>(callable);
        }

        //省略代码

        private TrustedFutureInterruptibleTask task;

        TrustedListenableFutureTask(Callable<V> callable) {
            this.task = new TrustedFutureInterruptibleTask(callable);
        }

        @Override
        public void run() {
            TrustedFutureInterruptibleTask localTask = task;
            if (localTask != null) {
                //由于TrustedFutureInterruptibleTask是InterruptibleTask的子类，故localTask.run
                //会调用InterruptibleTask中的run方法。
                localTask.run();
            }
        }

        @GwtIncompatible("Interruption not supported")
        @Override
        protected final void interruptTask() {
            TrustedFutureInterruptibleTask localTask = task;
            if (localTask != null) {
                localTask.interruptTask();
            }
        }

        @WeakOuter
        private final class TrustedFutureInterruptibleTask extends InterruptibleTask {
            private final Callable<V> callable;

            TrustedFutureInterruptibleTask(Callable<V> callable) {
                this.callable = checkNotNull(callable);
            }

            @Override
            void runInterruptibly() {
                //Ensure we haven't been cancelled or already run.
                if (!isDone()) {
                    try {
                        //调用
                        set(callable.call());
                    } catch (Throwable t) {
                        setException(t);
                    }
                }
            }

            @Override
            boolean wasInterrupted() {
                return TrustedListenableFutureTask.this.wasInterrupted();
            }
        }
    }

    abstract class InterruptibleTask implements Runnable {
        private static final AtomicReferenceFieldUpdater<InterruptibleTask, Thread> RUNNER = newUpdater(
                InterruptibleTask.class, Thread.class, "runner");

        @Override
        public final void run() {
            if (!RUNNER.compareAndSet(this, null, Thread.currentThread())) {
                return; // someone else has run or is running.
            }
            try {
                // 会调用TrustedFutureInterruptibleTask中的runInterruptibly方法
                runInterruptibly();
            } finally {
                if (wasInterrupted()) {
                    while (!doneInterrupting) {
                        Thread.yield();
                    }
                }
            }
        }
    }

    public static abstract class AbstractFuture<V> implements ListenableFuture<V> {

        protected boolean set(@Nullable V value) {
            //value是调用我们所定义的Callable的call方法所返回的结果值
            Object valueToSet = value == null ? NULL : value;
            //使用ATOMIC_HELPER原子的将结果值设置到AbstractFuture类对象的value属性中
            if (ATOMIC_HELPER.casValue(this, null, valueToSet)) {
                complete();
                return true;
            }
            return false;
        }

        //在这里执行完所有的回调函数
        private void complete() {
            for (Waiter currentWaiter = clearWaiters(); currentWaiter != null; currentWaiter = currentWaiter.next) {
                currentWaiter.unpark();
            }
            //We need to reverse the list to handle buggy listeners that depend on
            //ordering.
            Listener currentListener = clearListeners();
            Listener reversedList = null;
            while (currentListener != null) {
                Listener tmp = currentListener;
                currentListener = currentListener.next;
                tmp.next = reversedList;
                reversedList = tmp;
            }
            for (; reversedList != null; reversedList = reversedList.next) {
                executeListener(reversedList.task, reversedList.executor);
            }

            done();
        }

        void done() {
        }

        //这里就是调用执行器的execute方法，相当于给线程池扔一个任务，毕竟回调函数也是一个任务
        private static void executeListener(Runnable runnable, Executor executor) {
            try {
                executor.execute(runnable);
            } catch (RuntimeException e) {
                log.log(Level.SEVERE,
                        "RuntimeException while executing runnable " + runnable + " with executor " + executor, e);
            }
        }
    }

        //由于TrustedListenableFutureTask继承了AbstractFuture，每一个Task都会有一个Listener链表，
        //用来保存注册在此Task上的Listener。因此调用addListener时，其实就是将这个Listener添加到Task的Listener
        //链表中。
        //class:AbstractFuture
        public void addListener(Runnable listener, Executor executor) {
            checkNotNull(listener, "Runnable was null.");
            checkNotNull(executor, "Executor was null.");
            Listener oldHead = listeners;

            //如果任务Task还没有被执行完，那么就把Listener保存到Listener链表中，并且这个链表的头结点就是
            //新创建的Listener。为什么要用链表？因为一个任务可以有多个Listener
            if (oldHead != Listener.TOMBSTONE) {
                Listener newNode = new Listener(listener, executor);
                do {
                    newNode.next = oldHead;
                    if (ATOMIC_HELPER.casListeners(this, oldHead, newNode)) {
                        return;
                    }
                    oldHead = listeners; // re-read
                } while (oldHead != Listener.TOMBSTONE);
            }

            //如果执行到这里，就表明Listener.TOMBSTONE已经被设置好了，也就是说任务Task已经被执行完了，所以回调Listener
            executeListener(listener, executor);
        }

    }


    public static final class Futures extends GwtFuturesCatchingSpecialization {
        //class:Futures
        public static <V> void addCallback(final ListenableFuture<V> future, final FutureCallback<? super V> callback,
                Executor executor) {
            Preconditions.checkNotNull(callback);
            Runnable callbackListener = new Runnable() {
                @Override
                public void run() {
                    final V value;
                    try {
                        value = getUninterruptibly(future);
                    } catch (ExecutionException e) {
                        callback.onFailure(e.getCause());
                        return;
                    } catch (RuntimeException e) {
                        callback.onFailure(e);
                        return;
                    } catch (Error e) {
                        callback.onFailure(e);
                        return;
                    }
                    callback.onSuccess(value);
                }
            };
            //这里会先把FutureCallback包装成一个Listener（封装的目的就是为了拿到任务执行的返回值），然后添加到这里的
            //future（其实也就是前面的TrustedListenableFutureTask）的Listener链表中。
            future.addListener(callbackListener, executor);
        }
    }

    abstract static class TrustedFuture<V> extends AbstractFuture<V> {
        //省略代码

        @Override
        public final void addListener(Runnable listener, Executor executor) {
            super.addListener(listener, executor);
        }
    }

    public static final class Uninterruptibles {
        //class:Uninterruptibles
        public static <V> V getUninterruptibly(Future<V> future) throws ExecutionException {
            boolean interrupted = false;
            try {
                while (true) {
                    try {
                        return future.get();
                    } catch (InterruptedException e) {
                        interrupted = true;
                    }
                }
            } finally {
                if (interrupted) {
                    Thread.currentThread().interrupt();
                }
            }
        }
    }

}