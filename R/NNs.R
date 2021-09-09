if (arch == "N1") {
  model <- keras_model_sequential() %>% 
    layer_flatten(input_shape = c(p, p, 1)) %>%
    layer_dense(units = p^2, activation = "relu") %>% 
    layer_dense(units = p^2, activation = 'relu') %>%
    layer_dropout(0.1) %>% 
    layer_dense(units = p^2, activation = 'relu') %>%
    layer_dropout(0.15) %>% 
    layer_dense(units = p^2, activation = 'relu') %>%
    layer_dropout(0.2) %>% 
    layer_dense(units = p^2, activation = 'relu') %>%
    layer_dropout(0.25) %>% 
    layer_dense(units = p^2, activation = 'relu') %>%
    layer_dropout(0.3) %>% 
    layer_dense(units = p*p, activation = 'sigmoid')  %>%
    layer_reshape(target_shape = c(p,p)) 
}


if (arch == "N2") {
  
  #define kernsizes/poolsizes so they obey dimensionality restrictions
  kernsize1 <- ceiling(p/5)
  kernsize2 <- ceiling((p + 1 - kernsize1)/2)
  poolsize1 <- ceiling((p + 2 - kernsize1 - kernsize2)/4)
  
  model <- keras_model_sequential() %>% 
    layer_conv_2d(filters = p^2, kernel_size = c(kernsize1, kernsize1), activation = "relu", 
                  input_shape = c(p,p,1)) %>% 
    layer_conv_2d(filters = p^2, kernel_size = c(kernsize2, kernsize2), activation = "relu") %>% 
    layer_max_pooling_2d(pool_size = c(poolsize1, poolsize1)) %>% 
    layer_flatten() %>% 
    layer_dropout(0.2) %>%
    layer_dense(units = 10*p*p, activation = 'relu') %>%
    layer_dropout(0.2) %>% 
    layer_dense(units = p*p, activation = 'sigmoid')  %>%
    layer_reshape(target_shape = c(p,p)) 
  
  kernsize1 <- kernsize2 <- poolsize1 <- NULL
}