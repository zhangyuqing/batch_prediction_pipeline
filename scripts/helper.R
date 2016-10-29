conCat <- function(vec1, vec2){
  if(length(vec1) == 0){
    return(c())
  }
  if(length(vec1)!=length(vec2)){
    print("Two vectors must be the same length to concat in this pipeline.")
  }
  res <- c()
  for(i in 1:length(vec1)){
    res <- c(res, vec1[i], vec2[i])
  }
  return(res)
}