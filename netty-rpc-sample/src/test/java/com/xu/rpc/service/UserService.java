package com.xu.rpc.service;


public interface UserService {
    boolean existUser(String email);
    
    boolean createUser(User user);

    User getUser(long id);

    Page<User> listUser(int pageNo);
}
