
public class SpringIOCBootAnalysis {
    public static void main(String[] args) {
        new ClassPathXmlApplicationContext("classpath:rpc-invoke-config-server.xml");
    }

    class ClassPathXmlApplicationContext extends AbstractXmlApplicationContext {
        public ClassPathXmlApplicationContext(String configLocation) throws BeansException {
            this(new String[] { configLocation }, true, null);
        }

        public ClassPathXmlApplicationContext(String[] configLocations, boolean refresh, ApplicationContext parent)
                throws BeansException {
            super(parent);
            // 将我们程序传入的配置文件的路径保存到ClassPathXmlApplicationContext的父类中
            // 其实就是保存到AbstractRefreshableConfigApplicationContext中的configLocations属性
            setConfigLocations(configLocations);
            if (refresh) {
                refresh();
            }
        }
    }

    class AbstractApplicationContext extends DefaultResourceLoader
            implements ConfigurableApplicationContext, DisposableBean {
        //class:AbstractApplicationContext
        @Override
        public void refresh() throws BeansException, IllegalStateException {
            synchronized (this.startupShutdownMonitor) {
                // 执行创建容器前的准备工作 :记录下容器的启动时间、标记"已启动"状态、处理配置文件中的占位符
                prepareRefresh();

                // 创建Bean容器，加载XML配置信息：如果存在容器进行销毁旧容器，创建新容器，解析XML配置文件为一个个BeanDefinition定义
                // 注册到新容器(BeanFactory)中，注意Bean未初始化
                ConfigurableListableBeanFactory beanFactory = obtainFreshBeanFactory();

                // 设置 BeanFactory 的类加载器，添加几个 BeanPostProcessor，手动注册几个特殊的 bean
                prepareBeanFactory(beanFactory);

                try {
                    // 加载并执行后置处理器
                    postProcessBeanFactory(beanFactory);
                    // 回调容器中那些实现了BeanFactoryPostProcessor接口的对象中的postProcessBeanFactory()方法
                    invokeBeanFactoryPostProcessors(beanFactory);
                    // 注册所有实现BeanPostProcessor接口的bean，即将这些bean实例化，然后将其放到beanFactory中
                    registerBeanPostProcessors(beanFactory);
                    // 初始化Spring容器的消息源
                    initMessageSource();
                    // 初始化Spring容器事件广播器
                    initApplicationEventMulticaster();
                    // 空方法
                    onRefresh();
                    // 注册事件监听器
                    registerListeners();
                    // 初始化（构造）所有在XML文件中配置的单例非延迟加载的bean
                    finishBeanFactoryInitialization(beanFactory);
                    // 清理缓存,如果容器中存Bean名为lifecycleProcessor的Bean
                    // 对其进行注册,如果不存在创建一个DefaultLifecycleProcessor进行注册
                    finishRefresh();
                } catch (BeansException ex) {
                    // Destroy already created singletons to avoid dangling resources.
                    destroyBeans();
                    // 重置'有效'标志。
                    cancelRefresh(ex);
                    // 向调用者传播异常。
                    throw ex;
                }
            }
        }

        protected void prepareRefresh() {
            this.startupDate = System.currentTimeMillis();

            synchronized (this.activeMonitor) {
                this.active = true;
            }
            // 省略代码.....
            // 在上下文环境中初始化任何占位符属性源，空方法，默认情况下不执行任何操作
            initPropertySources();
            // 校验 xml 配置文件
            getEnvironment().validateRequiredProperties();
        }

        //class:AbstractApplicationContext
        protected ConfigurableListableBeanFactory obtainFreshBeanFactory() {
            // 调用AbstractRefreshableApplicationContext类中的refreshBeanFactory方法
            refreshBeanFactory();
            ConfigurableListableBeanFactory beanFactory = getBeanFactory();
            if (logger.isDebugEnabled()) {
                logger.debug("Bean factory for " + getDisplayName() + ": " + beanFactory);
            }
            return beanFactory;
        }

        //class:AbstractApplicationContext
        protected void prepareBeanFactory(ConfigurableListableBeanFactory beanFactory) {
            // 这里设置为加载当前 ApplicationContext 类的类加载器
            beanFactory.setBeanClassLoader(getClassLoader());
            // 设置 Bean的表达式解析器，默认使用EL表达式，可以使用#{bean.xxx}形式来调用相关的属性值
            beanFactory.setBeanExpressionResolver(new StandardBeanExpressionResolver());
            beanFactory.addPropertyEditorRegistrar(new ResourceEditorRegistrar(this, getEnvironment()));
            // 默认添加一个ApplicationContextAwareProcessor对象，它实现了BeanPostProcessor接口。在这个类的postProcessBeforeInitialization方法中，
            // 如果bean实现了ApplicationContextAware接口, 那么Spring会将上下文ApplicationContext注入Bean属性中。
            // 这个beanPostProcessor中的方法会在初始化bean时，被回调，主要是当被初始化的bean实现了ApplicationContextAware接口
            // 那么就会回调bean的setApplicationContext方法。
            beanFactory.addBeanPostProcessor(new ApplicationContextAwareProcessor(this));
            // 下面几行的意思就是，如果某个 bean 依赖于以下几个接口的实现类，在自动装配的时候忽略它们
            beanFactory.ignoreDependencyInterface(ResourceLoaderAware.class);
            beanFactory.ignoreDependencyInterface(ApplicationEventPublisherAware.class);
            beanFactory.ignoreDependencyInterface(MessageSourceAware.class);
            beanFactory.ignoreDependencyInterface(ApplicationContextAware.class);
            beanFactory.ignoreDependencyInterface(EnvironmentAware.class);

            // 省略代码.....
        }

        protected void invokeBeanFactoryPostProcessors(ConfigurableListableBeanFactory beanFactory) {
            PostProcessorRegistrationDelegate.invokeBeanFactoryPostProcessors(beanFactory,
                    getBeanFactoryPostProcessors());
        }

        //class:AbstractApplicationContext
        //注册Spring的事件广播器用于广播Spring的内置事件和自定义事件
        protected void initApplicationEventMulticaster() {
            ConfigurableListableBeanFactory beanFactory = getBeanFactory();
            //APPLICATION_EVENT_MULTICASTER_BEAN_NAME的值为字符串applicationEventMulticaster
            //如果beanFactory中已经有了applicationEventMulticaster的话，直接返回，
            //如果没有，则默认将其初始化为SimpleApplicationEventMulticaster，然后返回
            if (beanFactory.containsLocalBean(APPLICATION_EVENT_MULTICASTER_BEAN_NAME)) {
                this.applicationEventMulticaster =
                        beanFactory.getBean(APPLICATION_EVENT_MULTICASTER_BEAN_NAME, ApplicationEventMulticaster.class);
                //省略代码......
            }
            else {
                this.applicationEventMulticaster = new SimpleApplicationEventMulticaster(beanFactory);
                beanFactory.registerSingleton(APPLICATION_EVENT_MULTICASTER_BEAN_NAME, this.applicationEventMulticaster);
                //省略代码......
            }
        }

        //class:AbstractApplicationContext
        protected void registerListeners() {
            // 首先注册静态指定的侦听器
            for (ApplicationListener<?> listener : getApplicationListeners()) {
                getApplicationEventMulticaster().addApplicationListener(listener);
            }
            // 下面是我们自定义的监听器
            String[] listenerBeanNames = getBeanNamesForType(ApplicationListener.class, true, false);
            for (String lisName : listenerBeanNames) {
                getApplicationEventMulticaster().addApplicationListenerBean(lisName);
            }
        }

        //执行到这一步，Spring.xml配置文件中的特殊的Bean该注册的也注册了，该调用的也调用了，就剩下了普通的Bean了，
        //在这一步就都实例化了．（仅仅是非延迟实例化的单例Bean），也就是说这一步就已经完成了Bean工厂（ApplicationContext）的初始化了
        //class:AbstractApplicationContext
        protected void finishBeanFactoryInitialization(ConfigurableListableBeanFactory beanFactory) {
            // 省略代码......
            // 实例化所有剩余（非延迟初始化）单例
            beanFactory.preInstantiateSingletons();
        }

    }

    /**
     * class:AbstractRefreshableApplicationContext
     * 如果ApplicationContext中的BeanFactory属性已经有值，销毁此BeanFactory所有 Bean，关闭 BeanFactory，
     * 重新创建一个新的Bean容器设置给ApplicationContext的beanFactory属性。
     */
    protected final void refreshBeanFactory() throws BeansException {
        if (hasBeanFactory()) {
            // 销毁容器
            destroyBeans();
            closeBeanFactory();
        }
        try {
            // 创建类型为DefaultListableBeanFactory新容器赋值给BeanFactory变量，
            // 默认创建的是DefaultListableBeanFactory容器
            DefaultListableBeanFactory beanFactory = createBeanFactory();
            beanFactory.setSerializationId(getId());
            // 设置 BeanFactory 的两个配置属性：是否允许 Bean 覆盖、是否允许循环引用
            customizeBeanFactory(beanFactory);
            // 这个方法将根据配置，加载各个Bean，然后放到 BeanFactory 中，这个方法是通过XmlBeanDefinitionReader去加载配置文件中的bean定义; 
            // 注意：这里的加载并不是初始化这个Bean，而是以Key-value的形式存储在beanFactory的beanDefinitionMap中; 
            // beanDefinitionMap为beanName-> beanDefinition
            loadBeanDefinitions(beanFactory);
            synchronized (this.beanFactoryMonitor) {
                this.beanFactory = beanFactory;
            }
        } catch (IOException ex) {
            throw new ApplicationContextException("I/O error parsing bean definition source for " + getDisplayName(),
                    ex);
        }
    }

    class ApplicationContextAwareProcessor implements BeanPostProcessor {
        private final ConfigurableApplicationContext applicationContext;

        public ApplicationContextAwareProcessor(ConfigurableApplicationContext applicationContext) {
            this.applicationContext = applicationContext;
        }

        @Override
        public Object postProcessBeforeInitialization(final Object bean, String beanName) throws BeansException {
            AccessControlContext acc = null;
            // 省略代码......
            if (acc != null) {
                // 省略代码.....
            } else {
                invokeAwareInterfaces(bean);
            }
            return bean;
        }

        private void invokeAwareInterfaces(Object bean) {
            if (bean instanceof Aware) {
                // 省略代码......
                if (bean instanceof ApplicationEventPublisherAware) {
                    ((ApplicationEventPublisherAware) bean).setApplicationEventPublisher(this.applicationContext);
                }
                if (bean instanceof ApplicationContextAware) {
                    ((ApplicationContextAware) bean).setApplicationContext(this.applicationContext);
                }
            }
        }

        @Override
        public Object postProcessAfterInitialization(Object bean, String beanName) {
            return bean;
        }
    }

    static class PostProcessorRegistrationDelegate {
        // class:PostProcessorRegistrationDelegate
        public static void invokeBeanFactoryPostProcessors(ConfigurableListableBeanFactory beanFactory,
                List<BeanFactoryPostProcessor> beanFactoryPostProcessors) {

            Set<String> processedBeans = new HashSet<String>();
            // 获取spring配置文件中定义的所有实现BeanFactoryPostProcessor接口的bean，然后将得到的bean分为三类，第一类为实现了PriorityOrdered接口，
            // 第二类为实现了Ordered接口的，第三类为其它普通的仅仅实现了BeanFactoryPostProcessor接口的bean（没有实现Ordered、PriorityOrdered接口）。
            // 之后对于每个BeanFactoryPostProcessor，调用postProcessBeanFactory方法。
            String[] postProcessorNames = beanFactory.getBeanNamesForType(BeanFactoryPostProcessor.class, true, false);

            // Separate between BeanFactoryPostProcessors that implement PriorityOrdered,
            // Ordered, and the rest.
            List<BeanFactoryPostProcessor> priorityOrderedPostProcessors = new ArrayList<BeanFactoryPostProcessor>();
            List<String> orderedPostProcessorNames = new ArrayList<String>();
            List<String> nonOrderedPostProcessorNames = new ArrayList<String>();
            for (String ppName : postProcessorNames) {
                if (processedBeans.contains(ppName)) {
                } else if (beanFactory.isTypeMatch(ppName, PriorityOrdered.class)) {
                    priorityOrderedPostProcessors.add(beanFactory.getBean(ppName, BeanFactoryPostProcessor.class));
                } else if (beanFactory.isTypeMatch(ppName, Ordered.class)) {
                    orderedPostProcessorNames.add(ppName);
                } else {
                    nonOrderedPostProcessorNames.add(ppName);
                }
            }
            // 首先，回调那些实现了PriorityOrdered接口的BeanFactoryPostProcessors
            // 代码省略......

            // 接着，回调那些实现了Ordered接口的BeanFactoryPostProcessors
            // 代码省略......

            // 最后，首先实例化BeanFactoryPostProcessor，然后回调其中的方法
            List<BeanFactoryPostProcessor> nonOrderedPostProcessors = new ArrayList<BeanFactoryPostProcessor>();
            for (String postProcessorName : nonOrderedPostProcessorNames) {
                nonOrderedPostProcessors.add(beanFactory.getBean(postProcessorName, BeanFactoryPostProcessor.class));
            }
            invokeBeanFactoryPostProcessors(nonOrderedPostProcessors, beanFactory);
        }

        // class:PostProcessorRegistrationDelegate
        private static void invokeBeanFactoryPostProcessors(
                Collection<? extends BeanFactoryPostProcessor> postProcessors,
                ConfigurableListableBeanFactory beanFactory) {
            for (BeanFactoryPostProcessor postProcessor : postProcessors) {
                postProcessor.postProcessBeanFactory(beanFactory);
            }
        }

        public static void registerBeanPostProcessors(ConfigurableListableBeanFactory beanFactory,
                AbstractApplicationContext applicationContext) {

            String[] postProcessorNames = beanFactory.getBeanNamesForType(BeanPostProcessor.class, true, false);
            // 省略代码......
            // 从Spring的配置文件中获取所有实现了BeanPostProcessor接口的对象bean，然后将其同样分为3类。
            List<BeanPostProcessor> priorityOrderedPostProcessors = new ArrayList<BeanPostProcessor>();
            List<BeanPostProcessor> internalPostProcessors = new ArrayList<BeanPostProcessor>();
            List<String> orderedPostProcessorNames = new ArrayList<String>();
            List<String> nonOrderedPostProcessorNames = new ArrayList<String>();
            for (String ppName : postProcessorNames) {
                if (beanFactory.isTypeMatch(ppName, PriorityOrdered.class)) {
                    BeanPostProcessor pp = beanFactory.getBean(ppName, BeanPostProcessor.class);
                    priorityOrderedPostProcessors.add(pp);
                    if (pp instanceof MergedBeanDefinitionPostProcessor) {
                        internalPostProcessors.add(pp);
                    }
                } else if (beanFactory.isTypeMatch(ppName, Ordered.class)) {
                    orderedPostProcessorNames.add(ppName);
                } else {
                    nonOrderedPostProcessorNames.add(ppName);
                }
            }
            // 省略代码.....
            // 实例化BeanFactory中所有实现了BeanPostProcessor接口的bean，并且把这个bean放入到beanFactory中的beanPostProcessors属性里面，
            // 等到后面调用 实现了InitializingBean接口的bean 的afterPropertiesSet方法之前/之后，再通过遍历beanPostProcessors，来执行其中
            // 的方法
            List<BeanPostProcessor> nonOrderedPostProcessors = new ArrayList<BeanPostProcessor>();
            for (String ppName : nonOrderedPostProcessorNames) {
                BeanPostProcessor pp = beanFactory.getBean(ppName, BeanPostProcessor.class);
                nonOrderedPostProcessors.add(pp);
                if (pp instanceof MergedBeanDefinitionPostProcessor) {
                    internalPostProcessors.add(pp);
                }
            }
            registerBeanPostProcessors(beanFactory, nonOrderedPostProcessors);
            // 省略代码.......
            beanFactory.addBeanPostProcessor(new ApplicationListenerDetector(applicationContext));
        }

        private static void registerBeanPostProcessors(ConfigurableListableBeanFactory beanFactory,
                List<BeanPostProcessor> postProcessors) {
            // 将实现了BeanPostProcessor接口的bean加入到beanFactory中，也就是beanFactory中的beanPostProcessors属性里面
            for (BeanPostProcessor postProcessor : postProcessors) {
                beanFactory.addBeanPostProcessor(postProcessor);
            }
        }
    }

    //class:DefaultListableBeanFactory
    public void preInstantiateSingletons() throws BeansException {
        //省略代码......
		List<String> beanNames;
		synchronized (this.beanDefinitionMap) {
			beanNames = new ArrayList<String>(this.beanDefinitionNames);
		}

		for (String beanName : beanNames) {
			RootBeanDefinition bd = getMergedLocalBeanDefinition(beanName);
			if (!bd.isAbstract() && bd.isSingleton() && !bd.isLazyInit()) {
				if (isFactoryBean(beanName)) {
					// 省略代码
				}
				else {
                    // 实例化所有非延迟初始化单例
					getBean(beanName);
				}
			}
		}
		// 省略代码
    }
    
    //class:AbstractBeanFactory
    public Object getBean(String name) throws BeansException {
		return doGetBean(name, null, null, false);
    }
    
    protected <T> T doGetBean(
			final String name, final Class<T> requiredType, final Object[] args, boolean typeCheckOnly)
			throws BeansException {
		final String beanName = transformedBeanName(name);
		Object bean;
		Object sharedInstance = getSingleton(beanName);
		if (sharedInstance != null && args == null) {
			// 省略代码......
		}else {
			// 省略代码......
			try {
				final RootBeanDefinition mbd = getMergedLocalBeanDefinition(beanName);
				checkMergedBeanDefinition(mbd, beanName, args);

				// 确保把此bean依赖的bean也进行初始化
				// 代码省略

				// Create bean instance.
				if (mbd.isSingleton()) {
                    // 通过getSingleton来真正创建beanName所表示的bean，这个方法最终会调用到
                    // AbstractAutowireCapableBeanFactory中的doCreateBean方法
					sharedInstance = getSingleton(beanName, new ObjectFactory<Object>() {
						@Override
						public Object getObject() throws BeansException {
							try {
								return createBean(beanName, mbd, args);
							}
							catch (BeansException ex) {
								destroySingleton(beanName);
								throw ex;
							}
						}
					});
					bean = getObjectForBeanInstance(sharedInstance, name, beanName, mbd);
				}
			}
			catch (BeansException ex) {
				cleanupAfterBeanCreationFailure(beanName);
				throw ex;
			}
		}
        //省略代码......
    }
    
    // class:AbstractAutowireCapableBeanFactory
    protected Object doCreateBean(final String beanName, final RootBeanDefinition mbd, final Object[] args) {
		// Instantiate the bean.
		BeanWrapper instanceWrapper = null;
		if (mbd.isSingleton()) {
			instanceWrapper = this.factoryBeanInstanceCache.remove(beanName);
		}
		if (instanceWrapper == null) {
            // 首先调用createBeanInstance方法，创建bean实例对象（这个时候执行bean的构造方法）
			instanceWrapper = createBeanInstance(beanName, mbd, args);
		}
		final Object bean = (instanceWrapper != null ? instanceWrapper.getWrappedInstance() : null);
		Class<?> beanType = (instanceWrapper != null ? instanceWrapper.getWrappedClass() : null);

		// 省略代码

		// Initialize the bean instance.
		Object exposedObject = bean;
		try {
            // 调用populateBean方法，对bean进行填充，注入相关依赖
			populateBean(beanName, mbd, instanceWrapper);
			if (exposedObject != null) {
                // 调用方法initializeBean，进行相关初始化工作
				exposedObject = initializeBean(beanName, exposedObject, mbd);
			}
		}catch (Throwable ex) {
			// 省略代码
		}
        // 省略代码
		return exposedObject;
    }
    
    //class:AbstractAutowireCapableBeanFactory
    protected Object initializeBean(final String beanName, final Object bean, RootBeanDefinition mbd) {
		if (System.getSecurityManager() != null) {
			// 省略代码
		}else {
			invokeAwareMethods(beanName, bean);
		}
		Object wrappedBean = bean;
		if (mbd == null || !mbd.isSynthetic()) {
            // 先调用applyBeanPostProcessorsBeforeInitialization方法，执行每个BeanPostProcessor
            // 的postProcessBeforeInitialization
			wrappedBean = applyBeanPostProcessorsBeforeInitialization(wrappedBean, beanName);
		}
		try {
            // 执行bean的初始化方法，如果这个bean实现了InitializingBean接口，就会回调afterPropertiesSet方法，
            // 然后调用用户自己定义的初始化方法（如果有的话）
			invokeInitMethods(beanName, wrappedBean, mbd);
		}catch (Throwable ex) {
			throw new BeanCreationException(
					(mbd != null ? mbd.getResourceDescription() : null),
					beanName, "Invocation of init method failed", ex);
		}

		if (mbd == null || !mbd.isSynthetic()) {
            // 调用applyBeanPostProcessorsAfterInitialization方法，执行每个BeanPostProcessor的
            // postProcessAfterInitialization方法
			wrappedBean = applyBeanPostProcessorsAfterInitialization(wrappedBean, beanName);
		}
		return wrappedBean;
    }
    
    //class:AbstractAutowireCapableBeanFactory
    //如果这个bean实现了BeanNameAware、BeanClassLoaderAware以及BeanFactoryAware接口的话，就回调对应的方法
    private void invokeAwareMethods(final String beanName, final Object bean) {
		if (bean instanceof Aware) {
			if (bean instanceof BeanNameAware) {
				((BeanNameAware) bean).setBeanName(beanName);
			}
			if (bean instanceof BeanClassLoaderAware) {
				((BeanClassLoaderAware) bean).setBeanClassLoader(getBeanClassLoader());
			}
			if (bean instanceof BeanFactoryAware) {
				((BeanFactoryAware) bean).setBeanFactory(AbstractAutowireCapableBeanFactory.this);
			}
		}
    }
    
    //class:AbstractAutowireCapableBeanFactory
    protected void invokeInitMethods(String beanName, final Object bean, RootBeanDefinition mbd)
			throws Throwable {
		boolean isInitializingBean = (bean instanceof InitializingBean);
		if (isInitializingBean && (mbd == null || !mbd.isExternallyManagedInitMethod("afterPropertiesSet"))) {
			if (System.getSecurityManager() != null) {
				// 调用代码
			}else {
                // 如果这个bean实现类InitializingBean接口，就会回调此bean的afterPropertiesSet方法
				((InitializingBean) bean).afterPropertiesSet();
			}
		}
		if (mbd != null) {
			String initMethodName = mbd.getInitMethodName();
			if (initMethodName != null && !(isInitializingBean && "afterPropertiesSet".equals(initMethodName)) &&
					!mbd.isExternallyManagedInitMethod(initMethodName)) {
                // 回调我们自己指定的初始化方法，即在标签中通过init-method属性
				invokeCustomInitMethod(beanName, bean, mbd);
			}
		}
    }
    
    //class:AbstractAutowireCapableBeanFactory
    public Object applyBeanPostProcessorsBeforeInitialization(Object existingBean, String beanName)
			throws BeansException {
        Object result = existingBean;
        // 这里注意，我们前面添加了一个实现了BeanPostProcessor接口的bean，即ApplicationContextAwareProcessor，
        // 因此会直接调用其postProcessBeforeInitialization方法。在这个方法中，会调用bean的setApplicationContext方法
		for (BeanPostProcessor beanProcessor : getBeanPostProcessors()) {
			result = beanProcessor.postProcessBeforeInitialization(result, beanName);
			if (result == null) {
				return result;
			}
		}
		return result;
	}

}

