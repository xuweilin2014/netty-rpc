public class ThreadPoolExecutorAnalysis {

    /**
     * 线程池状态： 1.RUNNING：最正常的状态：接受新的任务，处理等待队列中的任务
     * 2.SHUTDOWN：不接受新的任务提交，但是会继续处理等待队列中的任务
     * 3.STOP：不接受新的任务提交，不再处理等待队列中的任务，中断正在执行任务的线程（调用shutdonNow会进入此状态）
     * 4.TIDYING：所有的任务都销毁了，workCount 为 0。线程池的状态在转换为 TIDYING 状态时，会执行钩子方法 terminated()
     * 5.TERMINATED：terminated() 方法结束后，线程池的状态就会变成这个
     */
    private boolean addWorker(Runnable firstTask, boolean core) {
        // retry 是个无限循环. 当线程池处于 RUNNING (运行)状态时, 只有在线程池中的有效线程数被成功加一以后,
        // 才会退出该循环而去执行后边的代码. 也就是说，当线程池在 RUNNING (运行)状态下退出该 retry 循环时，
        // 线程池中的有效线程数一定少于此次设定的最大线程数(可能是 corePoolSize 或 maximumPoolSize).
        retry: for (;;) {
            int c = ctl.get();
            int rs = runStateOf(c);

            // 如果线程池满足以下条件之一，那么返回false，表示既不允许创建新的线程，提交的任务也不会被执行：
            // 1. 线程池状态 > SHUTDOWN，其实也就是 STOP, TIDYING, 或 TERMINATED
            // 2. 线程池状态 = SHUTDOWN，firstTask != null
            // 3. 线程池状态 = SHUTDOWN，workQueue为空
            // 简单分析下：
            // 对于第2种情况，当线程池处于 SHUTDOWN 的时候，不允许提交新任务，所以在第2种情况中会返回false
            // 对于第1种情况，当状态大于 SHUTDOWN 时，会中断正在执行的任务的线程，因此更加不会允许创建新的线程
            // 对于第3种情况，如果状态为SHUTDOWN，且workQueue为空，同时firstTask为null，那么表明没有任务可以被执行，也就没有必要创建新的线程。
            // 多说一句：如果线程池处于 SHUTDOWN，但是 firstTask 为 null，且 workQueue 非空，那么是允许创建 worker
            // 的。这是因为，
            // 线程池处于SHUTDOWN的时候，不允许提交新的任务，但是可以继续执行任务队列中的任务，因此当任务队列非空时，可以创建
            // 新的线程来执行任务队列中的任务
            if (rs >= SHUTDOWN && !(rs == SHUTDOWN && firstTask == null && !workQueue.isEmpty()))
                return false;

            // 如果线程池内的有效线程数大于或等于了理论上的最大容量 CAPACITY 或者实际
            // 设定的最大容量, 就返回 false直接结束该方法. 这样同样没有创建新线程，新提交的任务也同样未被执行.
            // (core ? corePoolSize : maximumPoolSize) 表示如果 core为 true,
            // 那么实际设定的最大容量为 corePoolSize, 反之则为 maximumPoolSize.
            for (;;) {
                int wc = workerCountOf(c);
                if (wc >= CAPACITY || wc >= (core ? corePoolSize : maximumPoolSize))
                    return false;
                // 如果成功，那么就是所有创建线程前的条件校验都满足了，准备创建线程执行任务了
                // 这里失败的话，说明有其他线程也在尝试往线程池中创建线程
                if (compareAndIncrementWorkerCount(c))
                    break retry;
                // 由于有并发，重新再读取一下 ctl
                c = ctl.get(); // Re-read ctl
                // 正常如果是 CAS 失败的话，进到下一个里层的for循环就可以了。可是如果是因为其他线程的操作，
                // 导致线程池的状态发生了变更，比如有其他线程关闭了这个线程池。那么需要回到外层的for循环
                if (runStateOf(c) != rs)
                    continue retry;
                // else CAS failed due to workerCount change; retry inner loop
            }
        }

        // worker 中的线程是否已经启动
        boolean workerStarted = false;
        // 是否已将这个 worker 添加到 workers 这个 HashSet 中
        boolean workerAdded = false;
        Worker w = null;
        try {
            w = new Worker(firstTask);
            // 取 worker 中的线程对象，之前说了，Worker的构造方法会调用 ThreadFactory 来创建一个新的线程
            final Thread t = w.thread;
            if (t != null) {
                // mainLock 是整个类的全局锁，持有这个锁才能让下面的操作“顺理成章”，
                // 因为关闭一个线程池也需要这个锁，至少我持有锁的期间，线程池不会被关闭
                final ReentrantLock mainLock = this.mainLock;
                mainLock.lock();
                try {
                    int rs = runStateOf(ctl.get());
                    // 小于 SHUTTDOWN 那就是 RUNNING，这个自不必说，是最正常的情况
                    // 如果等于 SHUTDOWN，前面说了，不接受新的任务，但是会继续执行等待队列中的任务
                    if (rs < SHUTDOWN || (rs == SHUTDOWN && firstTask == null)) {
                        // worker 里面的 thread 可不能是已经启动的
                        if (t.isAlive()) // precheck that t is startable
                            throw new IllegalThreadStateException();
                        workers.add(w);
                        int s = workers.size();
                        // largestPoolSize 用于记录 workers 中的个数的最大值，因为 workers 是不断增加减少的，
                        // 通过这个值可以知道线程池的大小曾经达到的最大值
                        if (s > largestPoolSize)
                            largestPoolSize = s;
                        workerAdded = true;
                    }
                } finally {
                    mainLock.unlock();
                }
                // 如果线程添加成功的话，就启动线程 t. 由于 t指向 w.thread所引用的对象, 所以相当于启动的是 w.thread所引用的线程对象.
                // 而 w或者说Worker是 Runnable 的实现类, w.thread 是以 w作为 Runnable参数所创建的一个线程对象, 所以启动
                // w.thread所引用的线程对象, 也就是要执行 w 的 run()方法.
                if (workerAdded) {
                    t.start();
                    workerStarted = true;
                }
            }
        } finally {
            // 如果线程没有启动成功，需要做一些清理工作，如前面 workCount 加了 1，将其减掉
            if (!workerStarted)
                addWorkerFailed(w);
        }
        return workerStarted;
    }

    // workers 中删除掉相应的 worker
    // workCount 减 1
    private void addWorkerFailed(Worker w) {
        final ReentrantLock mainLock = this.mainLock;
        mainLock.lock();
        try {
            if (w != null)
                workers.remove(w);
            decrementWorkerCount();
            tryTerminate();
        } finally {
            mainLock.unlock();
        }
    }

    public class Worker extends AbstractQueuedSynchronizer implements Runnable {

        Worker(Runnable firstTask) {
            setState(-1); // inhibit interrupts until runWorker
            this.firstTask = firstTask;
            // Worker实现了Runnable接口，使用ThreadFactory#newThread创建新的线程使用的就是worker对象，类似于下面这样
            // this.thread = new Thread(threadName, worker);
            // 所以当在addWorker中启动Worker中的thread时，就会调用Worker类中的run方法，最后调用runWorker方法
            this.thread = getThreadFactory().newThread(this);
        }

        public void run() {
            runWorker(this);
        }
    }

    // 此方法用一个 while 循环来不断地从等待队列中获取任务并执行
    // 前面说了，worker 在初始化的时候，可以指定 firstTask，那么第一个任务也就可以不需要从队列中获取
    final void runWorker(Worker w) {
        Thread wt = Thread.currentThread();
        // 该线程的第一个任务(如果有的话)
        Runnable task = w.firstTask;
        w.firstTask = null;
        w.unlock(); // allow interrupts
        boolean completedAbruptly = true;
        try {
            // 循环调用 getTask 获取任务，而getTask()方法是个无限循环, 会从阻塞队列workQueue中不断取出任务来执行.
            // 当阻塞队列 workQueue中所有的任务都被取完之后, 就结束下面的while循环.
            while (task != null || (task = getTask()) != null) {
                w.lock();
                // If pool is stopping, ensure thread is interrupted;
                // if not, ensure thread is not interrupted. This
                // requires a recheck in second case to deal with
                // shutdownNow race while clearing interrupt
                // 如果线程池的状态 >= STOP（正在执行的线程也要被中断），所以如果线程没有被中断的话，就要中断当前线程
                // 如果线程池的状态 < STOP，有以下3种情况：
                // i.如果Thread.interrupted为true，再次检查线程池的状态依然 <
                // STOP，那么直接往下执行，这里的Thread.interrupted方法
                // 返回True表明线程处于中断状态，但是此方法的调用会清除线程的中断状态，确保线程不会被中断，继续执行
                // ii.如果Thread.interrupted为true，再次检查线程池的状态突然变化为 >=
                // STOP，表明线程池要停止，需要中断线程，而由于Thread.interrupted
                // 方法清除了线程的中断标志，所以!wt.isInterrupted返回true，因而继续中断线程
                // iii.如果Thread.interrupted为false，表明线程不处于中断状态，则继续往下执行
                if ((runStateAtLeast(ctl.get(), STOP) || (Thread.interrupted() && runStateAtLeast(ctl.get(), STOP)))
                        && !wt.isInterrupted())
                    wt.interrupt();
                try {
                    // 这是一个钩子方法，留给需要的子类实现
                    beforeExecute(wt, task);
                    Throwable thrown = null;
                    try {
                        // 到这里终于可以执行任务了
                        task.run();
                    } catch (RuntimeException x) {
                        thrown = x;
                        throw x;
                    } catch (Error x) {
                        thrown = x;
                        throw x;
                    } catch (Throwable x) {
                        thrown = x;
                        throw new Error(x);
                    } finally {
                        // 也是一个钩子方法，将 task 和异常作为参数，留给需要的子类实现
                        afterExecute(task, thrown);
                    }
                } finally {
                    // 置空 task，准备 getTask 获取下一个任务
                    task = null;
                    // 累加完成的任务数
                    w.completedTasks++;
                    w.unlock();
                }
            }
            completedAbruptly = false;
        } finally {
            // 如果到这里，需要执行线程关闭
            // 1. 说明 getTask 返回 null，也就是说，这个 worker 的使命结束了，执行关闭
            // 2. 任务执行过程中发生了异常
            processWorkerExit(w, completedAbruptly);
        }
    }

    // 此方法有三种可能：
    // 1. 阻塞直到获取到任务返回。我们知道，默认 corePoolSize 之内的线程是不会被回收的，它们会一直等待任务
    // 2. 超时退出。keepAliveTime 起作用的时候，也就是如果这么多时间内都没有任务，那么应该执行关闭
    // 3. 如果发生了以下条件，此方法必须返回 null:
    // i 池中有大于 maximumPoolSize 个 workers 存在(其它线程通过调用 setMaximumPoolSize
    // 将线程池的线程数目减小了)
    // ii 线程池处于 SHUTDOWN，而且 workQueue 是空的，当线程池处于 SHUTDOWN 时不再接受新的任务，但是任务队列中的任务会继续执行，
    // 而当任务队列也为空的话，表明没有任务需要被执行，因此getTask返回null
    // iii 线程池处于 STOP，不仅不接受新的线程，连 workQueue 中的线程也不再执行，所以直接返回null
    private Runnable getTask() {
        boolean timedOut = false; // Did the last poll() time out?

        for (;;) {
            int c = ctl.get();
            int rs = runStateOf(c);
            // 下面这个判断有两种可能返回null:
            // 1.rs == SHUTDOWN && workQueue.isEmpty()
            // 2.rs >= STOP
            if (rs >= SHUTDOWN && (rs >= STOP || workQueue.isEmpty())) {
                decrementWorkerCount();
                return null;
            }
            int wc = workerCountOf(c);

            // 如果 allowCoreThreadTimeOut 这个字段设置为 true(也就是允许核心线程受超时机制的控制), 则
            // 直接设置 timed 为 true. 反之, 则再看当前线程池中的有效线程数是否已经超过了核心线程数, 也
            // 就是是否存在非核心线程. 如果存在非核心线程, 那么也会设置 timed 为true.
            // 如果 wc <= corePoolSize (线程池中的有效线程数少于核心线程数, 即: 线程池内运行着的都是核心线程),
            // 并且 allowCoreThreadTimeOut 为 false(即: 核心线程即使空闲, 也不会受超时机制的限制),
            // 那么就设置 timed 为 false。从这个角度看，timed表示根据线程池的状态和设置是否允许超时，而
            // timedOut表示是否真正发生了超时
            boolean timed = allowCoreThreadTimeOut || wc > corePoolSize;

            // 当线程池处于 RUNNING (运行)状态但阻塞队列内已经没有任务(为空)时, 将导致有线程接下来会一直
            // 处于空闲状态。如果空闲的是核心线程并且设置核心线程不受超时机制的影响(默认情况下就是这个设置，表示timed为false),
            // 那么这些核心线程将一直在线程池中处于空闲状态, 等待着新任务的到来, 只要线程池处于 RUNNING
            // (运行)状态, 那么, 这些空闲的核心线程将一直在池子中而不会被销毁。 如果空闲的是非核心线程, 或者
            // 虽然是核心线程但是设置了核心线程受超时机制的限制（表示timed为true）, 那么当空闲达到超时时间时, 这就满足了这里的
            // if条件而去执行if内部的代码, 通过返回 null 结束掉该 getTask()方法, 也最终结束掉 runWorker()方法，关闭掉该线程。
            // 接下来分析一下if中的各个条件：
            // i.(wc > 1 || workQueue.isEmpty())：只有当线程数量为1，并且任务队列不为空的时候，才会为false。
            // 即在上面这个条件下，不允许关闭线程池中唯一的线程，因为还有任务队列中的任务需要被执行。
            // ii.(wc > maximumPoolSize || (timed && timedOut))：如果当前线程数 wc >
            // maximumPoolSize，或者超时，都返回 null。
            // 那是因为有可能开发者调用了 setMaximumPoolSize 将线程池的 maximumPoolSize 调小了，所以要关闭掉超出数量的线程。
            if ((wc > maximumPoolSize || (timed && timedOut)) && (wc > 1 || workQueue.isEmpty())) {
                if (compareAndDecrementWorkerCount(c))
                    return null;
                continue;
            }

            try {
                // 到 workQueue 中获取任务
                Runnable r = timed ? workQueue.poll(keepAliveTime, TimeUnit.NANOSECONDS) : workQueue.take();
                // 如果阻塞队列不为空并且未发生超时的情况, 那么取出的任务就不为 null, 就直接返回该任务对象.
                if (r != null)
                    return r;
                timedOut = true;
            } catch (InterruptedException retry) {
                timedOut = false;
            }
        }
    }

    /**
     * 该方法的具体执行流程为： i.如果线程池处于关闭状态，那么根据拒绝策略，对任务进行处理。 ii.如果线程池处于RUNNING状态，如果
     * workerCount < corePoolSize，则启动一个线程执行该任务； 如果 workerCount >=
     * corePoolSize，同时任务队列没有满，则task加入到任务队列中 如果任务队列满了，但是线程数 <
     * maximumPoolSize，那么创建一个线程执行任务 如果线程数 >= maximumPoolSize，那么拒绝执行该任务
     */
    public void execute(Runnable command) {
        if (command == null)
            throw new NullPointerException();

        // 前面说的那个表示 “线程池状态” 和 “线程数” 的整数
        int c = ctl.get();

        /********************************* 情况1 ************************************/

        // 根据ctl的值, 获取线程池中的有效线程数 workerCount, 如果 workerCount小于核心线程数 corePoolSize就进入
        if (workerCountOf(c) < corePoolSize) {
            // 调用addWorker()方法, 将核心线程数corePoolSize设置为线程池中线程数的上限值, 将此次提交的任务
            // command作为参数传递进去, 然后再次获取线程池中的有效线程数 workerCount（在addWorker方法中）,
            // 如果 workerCount依然小于核心线程数 corePoolSize, 就创建并启动一个线程, 然后返回 true结束整个
            // execute()方法. 如果此时的线程池已经关闭, 或者此时再次获取到的有效线程数 workerCount已经 >=
            // 核心线程数 corePoolSize, 就再继续执行后边的内容.
            if (addWorker(command, true))
                return;
            c = ctl.get();
        }

        /***** 分析1 ****/
        // 如果情况1的判断条件不满足, 则直接进入情况2. 如果情况1的判断条件满足, 但情况1中的 addWorker()方法返回 false,
        // 也同样会进入情况2.
        // 总之, 进入情况2时, 有以下3种情况：
        // i.线程池要么已经不处于RUNNING(运行)状态, workerCountOf(c) < corePoolSize
        // ii.线程池要么已经不处于RUNNING(运行)状态, workerCountOf(c) >= corePoolSize
        // iii.线程池仍处于RUNNING(运行)状态，但 workerCount >= corePoolSize

        /********************************* 情况2 ************************************/

        /***** 分析2 ****/
        // 经过上一段分析可知, 进入这个情况时, 线程池要么已经不处于RUNNING(运行)状态, 要么仍处于RUNNING(运行)状态
        // 但线程池内的有效线程数 workerCount已经 >= corePoolSize

        // 如果线程池未处于RUNNING(运行)状态, 或者虽然处于RUNNING(运行)状态但线程池内的阻塞队列 workQueue已满,
        // 则跳过此情况直接进入情况3.
        // 如果线程池处于RUNNING(运行)状态并且线程池内的阻塞队列 workQueue未满,则将提交的任务 command 添加到阻塞队列
        // workQueue中.
        if (isRunning(c) && workQueue.offer(command)) {
            int recheck = ctl.get();

            // 再次判断线程池此时的运行状态. 如果发现线程池未处于 RUNNING(运行)状态, 由于先前已将任务command加入
            // 到阻塞队列 workQueue中了, 所以需要将该任务从 workQueue中移除. 一般来说, 该移除操作都能顺利进行。
            // 所以一旦移除成功, 就再调用 handler的 rejectedExecution()方法, 根据该 handler定义的拒绝策略,
            // 对该任务进行处理.
            if (!isRunning(recheck) && remove(command))
                reject(command);

            // 再次计算线程池内的有效线程数 workerCount, 一旦发现该数量变为0, 就将线程池内的线程数上限值
            // 设置为最大线程数 maximumPoolSize, 然后只是创建一个线程。
            // 这块代码的真正意图是：担心任务提交到队列中了，但是线程都关闭了，所以保证线程池中至少有一个线程
            else if (workerCountOf(recheck) == 0)
                addWorker(null, false);
        }

        /********************************* 情况3 ************************************/

        /***** 分析3 ****/
        // 如果该方法能够执行到这里, 那么结合分析1和分析2可知, 线程池此时必定是下面两种情况中的一种:
        // ① 已经不处于RUNNING(运行)状态
        // ② 处于RUNNING(运行)状态, 并且线程池内的有效线程数 workerCount已经
        // >= 核心线程数 corePoolSize, 并且线程池内的阻塞队列 workQueue已满

        // 再次执行addWorker() 方法, 将线程池内的线程数上限值设置为最大线程数maximumPoolSize, 并将提交的任务
        // command作为被执行的对象, 尝试创建并启动一个线程来执行该任务. 如果此时线程池的状态为如下两种中的一种,
        // 就会触发 handler的 rejectedExecution()方法来拒绝该任务的执行:
        // ① 未处于RUNNING(运行)状态.
        // ② 处于RUNNING(运行)状态, 但线程池内的有效线程数已达到本次设定的最大线程数maximumPoolSize (另外根据分析3可知,
        // 此时线程池内的阻塞队列 workQueue已满).
        //
        // 如果线程池处于 RUNNING(运行)状态, 但有效线程数还未达到本次设定的最大线程数, 那么就会尝试创建并启动一个线程
        // 来执行任务 command. 如果线程的创建和启动都很顺利, 那么就直接结束掉该 execute()方法; 如果线程的创建或
        // 启动失败, 则同样会触发 handler的 rejectedExecution()方法来拒绝该任务的执行并结束掉该 execute()方法.
        else if (!addWorker(command, false))
            reject(command);
    }

}

