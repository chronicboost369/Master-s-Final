

numcol <- which(sapply(1:ncol(train4),function(x){ is.numeric(train4[,x])})==T)
lapply(numcol,function(x){ 
  iqr <- IQR(train4[,x])
})



detecting_outliers <- function(data){
  numcol <- which(sapply(1:ncol(data),function(x){ is.numeric(data[,x])})==T)
  outliers <- function(data){
    iqr <- IQR(data)
    index <- which(data<(quantile(data,.25) - IQR(data)*1.5) | data>(quantile(data,.75) + IQR(data)*1.5))
    return(index)
  }
  indexs <- unique(unlist(lapply(numcol,function(x) {outliers(data[,x])})))
  return(indexs)
}
