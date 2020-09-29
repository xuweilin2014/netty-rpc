package com.xu.rpc.benchmark;

import com.opencsv.CSVWriter;
import com.opencsv.bean.CsvBindByName;
import com.opencsv.bean.StatefulBeanToCsv;
import com.opencsv.bean.StatefulBeanToCsvBuilder;
import com.opencsv.exceptions.CsvDataTypeMismatchException;
import com.opencsv.exceptions.CsvRequiredFieldEmptyException;
import com.xu.rpc.services.AddCalculate;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Vector;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * @author sinjinsong
 * @date 2018/8/6
 */
@Slf4j
public class RpcClient {
    private int threads = 32;
    private int requestsTotal;
    private int requestsPerThread = 1000;
    private ExecutorService executorService = Executors.newFixedThreadPool(threads);
    private int measurementIterations = 10;
    private int warmupIterations = 3;

    @Getter
    @Setter
    public class BenchmarkResult {
        @CsvBindByName(column = "Time(ms)")
        private String mills;
        @CsvBindByName(column = "QPS(ms)")
        private String qps;
        @CsvBindByName(column = "AVG_RT(ms)")
        private String avgRt;
        @CsvBindByName(column = "P90(ms)")
        private String p90;
        @CsvBindByName(column = "P99(ms)")
        private String p99;
        @CsvBindByName(column = "P999(ms)")
        private String p999;
        @CsvBindByName(column = "Type")
        private String index;

        private double _mills;
        private double _qps;
        private double _avgRt;
        private double _p90;
        private double _p99;
        private double _p999;

        public BenchmarkResult() {
        }

        public BenchmarkResult(int index, long nanos, List<Long> rts) {
            this.index = "NORMAL-" + index;
            double mills = 1.0 * nanos / 1000000;
            // 每毫秒的处理请求数
            double qps = 1.0 * requestsTotal * 1000000 / nanos;
            // 毫秒
            double avgRt = 1.0 * rts.stream().mapToLong(x -> x).sum() / 1000000 / rts.size();
            Collections.sort(rts);

            this._mills = mills;
            this._qps = qps;
            this._avgRt = avgRt;
            this._p90 = 1.0 * rts.get((int) (rts.size() * 0.9)) / 1000000;
            this._p99 = 1.0 * rts.get((int) (rts.size() * 0.99)) / 1000000;
            this._p999 = 1.0 * rts.get((int) (rts.size() * 0.999)) / 1000000;

            this.mills = String.format("%.3f", _mills).trim();
            this.qps = String.format("%.3f", _qps);
            this.avgRt = String.format("%.3f ", _avgRt);
            this.p90 = String.format("%.3f", _p90);
            this.p99 = String.format("%.3f", _p99);
            this.p999 = String.format("%.3f", _p999);
        }
    }


    private void createUser() {
        try {
            Path benchmark = Paths.get(System.getProperty("user.home"), "benchmark", "createUser.csv");
            final Path parent = benchmark.getParent();
            if (parent != null) // null will be returned if the path has no parent
                Files.createDirectories(parent);
            if (!Files.exists(benchmark)) {
                Files.createFile(benchmark);
            }
            BufferedWriter writer = Files.newBufferedWriter(benchmark, StandardOpenOption.WRITE);

            StatefulBeanToCsv beanToCsv = new StatefulBeanToCsvBuilder(writer)
                    .withQuotechar(CSVWriter.NO_QUOTE_CHARACTER)
                    .withSeparator(CSVWriter.DEFAULT_SEPARATOR)
                    .withEscapechar('\\').build();
            log.info("----------------------------------------------------------------------------");
            log.info("createUser started");

            ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("classpath:rpc-invoke-config-client.xml");
            List<BenchmarkResult> results = new ArrayList<>();
            for (int i = 0; i < warmupIterations + measurementIterations; i++) {
                CountDownLatch countDownLatch = new CountDownLatch(threads);

                AddCalculate calc = (AddCalculate) context.getBean("addCalc");

                /*Person p = new Person();
                p.setId(20150811);
                p.setName("XiaoHaoBaby");
                p.setAge(1);*/

                List<Long> rts = new Vector<>(requestsTotal);
                Runnable r = () -> {
                    for (int j = 0; j < requestsPerThread; j++) {
                        long begin = System.nanoTime();
                        try {
                            calc.add(j, j);
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                        rts.add(System.nanoTime() - begin);
                    }
                    countDownLatch.countDown();
                };


                for (int k = 0; k < threads; k++) {
                    executorService.submit(r);
                }
                long benchmarkStart = System.nanoTime();
                countDownLatch.await();
                long nanos = System.nanoTime() - benchmarkStart;
                if (i >= warmupIterations) {
                    results.add(new BenchmarkResult(i - warmupIterations, nanos, rts));
                }
            }
            results.add(avgBenchmarkResult(results));
            beanToCsv.write(results);
            writer.close();
            log.info("createUser end");

        } catch (IOException | InterruptedException
                | CsvRequiredFieldEmptyException | CsvDataTypeMismatchException e) {
            e.printStackTrace();
        }
    }

    public BenchmarkResult avgBenchmarkResult(List<BenchmarkResult> benchmarkResults) {
        BenchmarkResult result = new BenchmarkResult();
        result.index = "AVG";
        result.mills = String.format("%.3f", benchmarkResults.stream().mapToDouble(BenchmarkResult::get_mills).average().getAsDouble()).trim();

        result.qps = String.format("%.3f", benchmarkResults.stream().mapToDouble(BenchmarkResult::get_qps).average().getAsDouble());

        result.avgRt = String.format("%.3f ", benchmarkResults.stream().mapToDouble(BenchmarkResult::get_avgRt).average().getAsDouble());
        result.p90 = String.format("%.3f", benchmarkResults.stream().mapToDouble(BenchmarkResult::get_p90).average().getAsDouble());
        result.p99 = String.format("%.3f", benchmarkResults.stream().mapToDouble(BenchmarkResult::get_p99).average().getAsDouble());

        result.p999 = String.format("%.3f", benchmarkResults.stream().mapToDouble(BenchmarkResult::get_p999).average().getAsDouble());
        return result;
    }

    public static void main(String[] args) {
        new RpcClient().createUser();
    }
/*
    private void existUser() {
        try {
            Path benchmark = Paths.get(System.getProperty("user.home"), "benchmark", "existUser.csv");
            final Path parent = benchmark.getParent();
            if (parent != null) // null will be returned if the path has no parent
                Files.createDirectories(parent);
            if (!Files.exists(benchmark)) {
                Files.createFile(benchmark);
            }
            BufferedWriter writer = Files.newBufferedWriter(benchmark, StandardOpenOption.WRITE);

            StatefulBeanToCsv beanToCsv = new StatefulBeanToCsvBuilder(writer)
                    .withQuotechar(CSVWriter.NO_QUOTE_CHARACTER)
                    .withSeparator(CSVWriter.DEFAULT_SEPARATOR)
                    .withEscapechar('\\').build();

            log.info("----------------------------------------------------------------------------");
            log.info("existUser started");
            List<BenchmarkResult> results = new ArrayList<>();
            for (int i = 0; i < warmupIterations + measurementIterations; i++) {
                CountDownLatch countDownLatch = new CountDownLatch(threads);
                List<Long> rts = new Vector<>(requestsTotal);
                Runnable r = () -> {
                    for (int j = 0; j < requestsPerThread; j++) {
                        long begin = System.nanoTime();
                        try {
                            userService.existUser(j + "@gmail.com");
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                        rts.add(System.nanoTime() - begin);
                    }
                    countDownLatch.countDown();
                };
                for (int k = 0; k < threads; k++) {
                    executorService.submit(r);
                }
                long benchmarkStart = System.nanoTime();
                countDownLatch.await();
                long nanos = System.nanoTime() - benchmarkStart;
                if (i >= warmupIterations) {
                    results.add(new BenchmarkResult(i - warmupIterations, nanos, rts));
                }
            }
            results.add(avgBenchmarkResult(results));
            beanToCsv.write(results);
            writer.close();
            log.info("existUser end");
        } catch (IOException e) {
            e.printStackTrace();
        } catch (InterruptedException e) {
            e.printStackTrace();
        } catch (CsvDataTypeMismatchException e) {
            e.printStackTrace();
        } catch (CsvRequiredFieldEmptyException e) {
            e.printStackTrace();
        }
    }

    private void getUser() {
        try {
            Path benchmark = Paths.get(System.getProperty("user.home"), "benchmark", "getUser.csv");
            final Path parent = benchmark.getParent();
            if (parent != null) // null will be returned if the path has no parent
                Files.createDirectories(parent);
            if (!Files.exists(benchmark)) {
                Files.createFile(benchmark);
            }
            BufferedWriter writer = Files.newBufferedWriter(benchmark, StandardOpenOption.WRITE);

            StatefulBeanToCsv beanToCsv = new StatefulBeanToCsvBuilder(writer)
                    .withQuotechar(CSVWriter.NO_QUOTE_CHARACTER)
                    .withSeparator(CSVWriter.DEFAULT_SEPARATOR)
                    .withEscapechar('\\').build();

            log.info("----------------------------------------------------------------------------");
            log.info("getUser started");
            List<BenchmarkResult> results = new ArrayList<>();
            for (int i = 0; i < warmupIterations + measurementIterations; i++) {
                CountDownLatch countDownLatch = new CountDownLatch(threads);
                List<Long> rts = new Vector<>(requestsTotal);
                Runnable r = () -> {
                    for (int j = 0; j < requestsPerThread; j++) {
                        long begin = System.nanoTime();
                        try {
                            userService.getUser(j);
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                        rts.add(System.nanoTime() - begin);
                    }
                    countDownLatch.countDown();
                };

                for (int k = 0; k < threads; k++) {
                    executorService.submit(r);
                }
                long benchmarkStart = System.nanoTime();
                countDownLatch.await();
                long nanos = System.nanoTime() - benchmarkStart;
                if (i >= warmupIterations) {
                    results.add(new BenchmarkResult(i - warmupIterations, nanos, rts));
                }
            }
            results.add(avgBenchmarkResult(results));
            beanToCsv.write(results);
            writer.close();
            log.info("getUser end");
        } catch (IOException e) {
            e.printStackTrace();
        } catch (InterruptedException e) {
            e.printStackTrace();
        } catch (CsvDataTypeMismatchException e) {
            e.printStackTrace();
        } catch (CsvRequiredFieldEmptyException e) {
            e.printStackTrace();
        }
    }

    private void listUser() {
        try {
            Path benchmark = Paths.get(System.getProperty("user.home"), "benchmark", "listUser.csv");
            final Path parent = benchmark.getParent();
            if (parent != null) // null will be returned if the path has no parent
                Files.createDirectories(parent);
            if (!Files.exists(benchmark)) {
                Files.createFile(benchmark);
            }
            BufferedWriter writer = Files.newBufferedWriter(benchmark, StandardOpenOption.WRITE);

            StatefulBeanToCsv beanToCsv = new StatefulBeanToCsvBuilder(writer)
                    .withQuotechar(CSVWriter.NO_QUOTE_CHARACTER)
                    .withSeparator(CSVWriter.DEFAULT_SEPARATOR)
                    .withEscapechar('\\').build();

            log.info("----------------------------------------------------------------------------");
            log.info("listUser started");
            List<BenchmarkResult> results = new ArrayList<>();
            for (int i = 0; i < warmupIterations + measurementIterations; i++) {
                CountDownLatch countDownLatch = new CountDownLatch(threads);
                List<Long> rts = new Vector<>(requestsTotal);
                Runnable r = () -> {
                    for (int j = 0; j < requestsPerThread; j++) {
                        long begin = System.nanoTime();
                        try {
                            userService.listUser(j);
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                        rts.add(System.nanoTime() - begin);
                    }
                    countDownLatch.countDown();
                };

                for (int k = 0; k < threads; k++) {
                    executorService.submit(r);
                }
                long benchmarkStart = System.nanoTime();
                countDownLatch.await();
                long nanos = System.nanoTime() - benchmarkStart;
                if (i >= warmupIterations) {
                    results.add(new BenchmarkResult(i - warmupIterations, nanos, rts));
                }
            }
            results.add(avgBenchmarkResult(results));
            beanToCsv.write(results);
            writer.close();
            log.info("listUser end");
        } catch (IOException e) {
            e.printStackTrace();
        } catch (InterruptedException e) {
            e.printStackTrace();
        } catch (CsvDataTypeMismatchException e) {
            e.printStackTrace();
        } catch (CsvRequiredFieldEmptyException e) {
            e.printStackTrace();
        }
    }*/
}

