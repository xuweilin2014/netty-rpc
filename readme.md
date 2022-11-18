# Netty-Rpc 项目简介

使用 Netty、Zookeeper 和 Spring，实现了一个轻量级的 RPC 框架，RPC 使用 Netty 作为网络通讯框架，Zookeeper 作
为注册中心进行服务的自动注册和发现，Spring 作为容器，经过简单测试，QPS 大概为 1K~2K。架构和设计类似于 Dubbo，用于提高自己的代码水平以及对
 Dubbo 框架源代码的理解。

netty-rpc 项目实现的功能如下所示：

+ zookeeper作为注册中心的服务注册和发现
+ 内置3种负载均衡：加权轮询(WeightedRoundRobin)、加权随机(WeightedRandom)、一致性哈希(ConsistentHash)
+ 支持多种序列化协议，比如jdk和protostuff
+ 内置4种集群容错机制：失败自动切换(Failover)、失败自动恢复(Failback)、快速失败(Failfast)、失败安全(Failsafe)
+ 心跳机制
+ 超时机制
+ 断线重连机制
+ 服务能力展示
+ 桩
+ 服务降级
+ 服务端和客户端过滤器
+ 注册中心缓存
+ url直连
+ 结果缓存
+ SPI机制
+ 令牌验证
+ 限流
+ 粘滞连接
+ TCP/InJVM协议
+ 同步与异步调用
+ JMX监控
+ 优雅停机

此项目更加详细的文档如下：

+ [netty-rpc 项目解析](https://github.com/xuweilin2014/netty-rpc/issues/10)
+ [netty-rpc 功能详解](https://github.com/xuweilin2014/netty-rpc/issues/11)





