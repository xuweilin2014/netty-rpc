public class ExecutorCompletionServiceAnalysis {

static class Main {

//     public static void main(String[] args) throws ExecutionException, InterruptedException {
// //        case1();
// //        case2();
//         case3();
//     }


    /**
     * <一>
     * 1. 用List收集任务结果 (List记录每个submit返回的Future)
     * 2. 循环查看结果, Future不一定完成, 如果没有完成, 那么调用get会租塞
     * 3. 如果排在前面的任务没有完成, 那么就会阻塞, 这样后面已经完成的任务就没法获得结果了, 导致了不必要的等待时间.
     *    更为严重的是: 第一个任务如果几个小时或永远完成不了, 而后面的任务几秒钟就完成了, 那么后面的任务的结果都将得不到处理
     *
     * 导致: 已完成的任务可能得不到及时处理
     */
    private static void case1() throws ExecutionException, InterruptedException {
        final Random random = new Random();
        ExecutorService service = Executors.newFixedThreadPool(10);
        List<Future<String>> taskResultHolder = new ArrayList<>();
        for(int i=0; i<50; i++) {
            //搜集任务结果
            taskResultHolder.add(service.submit(new Callable<String>() {
                public String call() throws Exception {
                    Thread.sleep(random.nextInt(5000));
                    return Thread.currentThread().getName();
                }
            }));
        }
        // 处理任务结果
        int count = 0;
        System.out.println("handle result begin");
        for(Future<String> future : taskResultHolder) {
            System.out.println(future.get());
            count++;
        }
        System.out.println("handle result end");
        System.out.println(count + " task done !");

        //关闭线程池
        service.shutdown();
    }

    /**
     * <二> 只对第一种情况进行的改进
     *      1. 查看任务是否完成, 如果完成, 就获取任务的结果, 让后重任务列表中删除任务.
     *      2. 如果任务未完成, 就跳过此任务, 继续查看下一个任务结果.
     *      3. 如果到了任务列表末端, 那么就从新回到任务列表开始, 然后继续从第一步开始执行
     *
     *      这样就可以及时处理已完成任务的结果了
     */
    private static void case2() throws ExecutionException, InterruptedException {
        final Random random = new Random();
        ExecutorService service = Executors.newFixedThreadPool(10);
        List<Future<String>> results = new ArrayList<>();

        for(int i=0; i<50; i++) {
            Callable<String> task = new Callable<String>() {
                public String call() throws Exception {
                    Thread.sleep(random.nextInt(5000)); //模拟耗时操作
                    return Thread.currentThread().getName();
                }
            };
            Future<String> future = service.submit(task);
            results.add(future); // 搜集任务结果
        }

        int count = 0;
        //自旋, 获取结果
        System.out.println("handle result begin");
        for(int i=0; i<results.size(); i++) {
            Future<String> taskHolder = results.get(i);

            if(taskHolder.isDone()) { //任务完成
                String result = taskHolder.get(); //获取结果, 进行某些操作
                System.out.println("result: " + result);
                results.remove(taskHolder);
                i--;

                count++; //完成的任务的计数器
            }

            //回到列表开头, 从新获取结果
            if(i == results.size() - 1) i = -1;
        }
        System.out.println("handle result end");
        System.out.println(count + " task done !");

        //线程池使用完必须关闭
        service.shutdown();
    }


    /**
     * <三> 使用ExecutorCompletionService管理异步任务
     * 1. Java中的ExecutorCompletionService<V>本身有管理任务队列的功能
     *    i. ExecutorCompletionService内部维护列一个队列, 用于管理已完成的任务
     *    ii. 内部还维护列一个Executor, 可以执行任务
     *
     * 2. ExecutorCompletionService内部维护了一个BlockingQueue, 只有完成的任务才被加入到队列中
     *
     * 3. 任务一完成就加入到内置管理队列中, 如果队列中的数据为空时, 调用take()就会阻塞 (等待任务完成)
     *    i. 关于完成任务是如何加入到完成队列中的, 请参考ExecutorCompletionService的内部类QueueingFuture的done()方法
     *
     * 4. ExecutorCompletionService的take/poll方法是对BlockingQueue对应的方法的封装, 关于BlockingQueue的take/poll方法:
     *    i. take()方法, 如果队列中有数据, 就返回数据, 否则就一直阻塞;
     *    ii. poll()方法: 如果有值就返回, 否则返回null
     *    iii. poll(long timeout, TimeUnit unit)方法: 如果有值就返回, 否则等待指定的时间; 如果时间到了如果有值, 就返回值, 否则返回null
     *
     * 解决了已完成任务得不到及时处理的问题
     */
    static void case3() throws InterruptedException, ExecutionException {
        Random random = new Random();

        ExecutorService service = Executors.newFixedThreadPool(10);
        ExecutorCompletionService<String> completionService = new ExecutorCompletionService<String>(service);

        for(int i=0; i<50; i++) {
            completionService.submit(new Callable<String>() {
                @Override
                public String call() throws Exception {
                    Thread.sleep(random.nextInt(5000));
                    return Thread.currentThread().getName();
                }
            });
        }

        int completionTask = 0;
        while(completionTask < 50) {
            //如果完成队列中没有数据, 则阻塞; 否则返回队列中的数据
            Future<String> resultHolder = completionService.take();
            System.out.println("result: " + resultHolder.get());
            completionTask++;
        }

        System.out.println(completionTask + " task done !");

        //ExecutorService使用完一定要关闭 (回收资源, 否则系统资源耗尽! .... 呵呵...)
        service.shutdown();
    }
}

}