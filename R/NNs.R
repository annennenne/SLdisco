
if (arch == "N6") {
  nfilt <- 2^5
  
  if (p == 20) tow1_psize <- 5
  if (p == 10) tow1_psize <- 2
  if (p == 5) tow1_psize <- 2
  
  input <- layer_input(shape = c(p, p, 1))  
  
  tower_1a <- input %>% 
    layer_conv_2d(filters = nfilt, kernel_size = c(p, 1), padding = "same", activation='relu') 
  
  tower_1b <- input %>% 
    layer_conv_2d(filters = nfilt, kernel_size = c(1, p), padding = "same", activation='relu')
  
  tower_1c <- input %>% 
    layer_conv_2d(filters = nfilt, kernel_size = c(1, 1), activation = "relu")
  
  tower_1d <- input %>% 
    layer_conv_2d(filters = nfilt, kernel_size = c(3, 3), padding = "same", activation = "relu") 
  
  concat1 <- layer_concatenate(c(tower_1a, tower_1b, tower_1c, tower_1d), axis = 3)  %>%
    layer_max_pooling_2d(pool_size = c(tow1_psize, tow1_psize)) %>% layer_flatten() %>% 
    layer_dropout(0.2) %>%
    layer_dense(units = 4*p*p, activation = 'relu') %>%
    layer_dropout(0.2) %>% 
    layer_dense(units = p*p, activation = 'sigmoid')  %>%
    layer_reshape(target_shape = c(p,p)) 
  
  model<- keras_model(input, concat1)
  
  nfilt <- tow1_psize <- tower_1a <- tower_1b <- tower_1c <- tower_1d <- NULL
  concat1 <- input <- NULL
  
}
