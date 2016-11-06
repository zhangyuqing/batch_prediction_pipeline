#### Partition of data ####
## train list
length(trn_set_lst)
length(trn_set_lst) == n_batch_train+1
length(y_trn_lst)
length(y_trn_lst) == n_batch_train+1

sapply(trn_set_lst, dim)
sapply(y_trn_lst, length)

sapply(y_trn_lst, function(y){
  print(length(y))
  print(sum(y))
})


## data.frame
colnames(test_df)[(n_genes-10):(n_genes+1)]
test_df$response

length(training_df_lst)
sapply(training_df_lst, dim)
y_lst <- lapply(training_df_lst, function(train_df){
  return(train_df$response)
})
sapply(y_lst, length)
sapply(y_lst, function(y){
  return(sum(as.numeric(as.character(y))))
})
