package com.xu.rpc.commons.cache.lru;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

// 默认的 LRU 缓存实现，采用了双向链表 + hashmap 的结构，每次访问或者增加节点时，这个节点都会被移动到第一个节点的位置
public class DefaultLruCache<K,V> implements LruCache<K,V> {

    private int capacity;

    private final Map<K, Node<K,V>> map = new ConcurrentHashMap<>();

    private int size = 0;

    private Node<K,V> dummy;

    private Node<K,V> tail;

    public DefaultLruCache(int capacity) {
        this.capacity = capacity;
        dummy = new Node<>(null, null);
        tail = new Node<>(null, null);

        // 初始化 LRU 缓存的链表
        dummy.next = tail;
        tail.next = null;
        tail.prev = dummy;
        dummy.prev = null;
    }

    @Override
    public V get(K key) {
        if (map.containsKey(key)){
            Node<K,V> node = map.get(key);
            // 将最近被访问的节点移动到链表头
            moveToHead(node);
            return node.value;
        }

        return null;
    }

    @Override
    public void put(K key, V value) {
        size++;
        // 如果容量超过了的话，直接移除掉最后一个结点
        if (size > capacity){
            removeLast();
        }

        // 如果是已经存在的节点，则更新节点值，然后移动到链表头
        if (map.containsKey(key)){
            Node<K,V> node = map.get(key);
            node.value = value;
            moveToHead(node);

        // 如果不存在的节点，则直接移动到链表头
        }else{
            Node<K,V> node = new Node<>(key, value);
            setHead(node);
            map.put(key, node);
        }
    }

    private void moveToHead(Node<K,V> node){
        node.prev.next = node.next;
        node.next.prev = node.prev;
        setHead(node);
    }

    private void setHead(Node<K, V> node){
        node.prev = dummy;
        node.next = dummy.next;
        Node<K,V> lastHead = dummy.next;
        if (lastHead != null){
            lastHead.prev = node;
        }
        dummy.next = node;
    }

    @Override
    public int size() {
        return this.size;
    }

    @Override
    public int capacity() {
        return this.capacity;
    }

    // 清空缓存
    @Override
    public void clear() {
        dummy.next = tail;
        tail.prev = dummy;
        this.size = 0;
        map.clear();
    }

    // 移除掉缓存中的最后一个结点，也就是移除最少使用的缓存值
    @Override
    public void removeLast() {
        if (!isEmpty()){
            Node<K,V> node = tail.prev;

            map.remove(tail.prev.key);
            tail.prev = tail.prev.prev;
            tail.prev.next = tail;

            // wait for gc
            node.prev = null;
            node.next = null;

            size--;
        }
    }

    private boolean isEmpty(){
        return tail.prev == dummy;
    }

    @Override
    public String toString() {
        StringBuilder cache = new StringBuilder("head");
        Node node = dummy.next;
        while (node != tail){
            cache.append(" -> ");
            cache.append(node.key);
            node = node.next;
        }
        cache.append(" -> ").append("tail");
        return cache.toString();
    }

    private static class Node<K, V>{
        K key;

        V value;

        Node<K,V> prev;

        Node<K,V> next;

        Node(K key, V value){
            this.key = key;
            this.value = value;
        }
    }

}
