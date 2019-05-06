rowsum(matrix(c(1,0),ncol=8, nrow=9)[-9,], 8)

is_stochastic<- function (x) {
  if (!is.numeric(x) || !is.matrix(x)){
    stop("Must be a numeric matrix")
  }nrow(x) == ncol(x) && 
    sum(x<0)==0 && 
    all(rowsum(x)==1)
  
  }
rowsum

hilbert<- function (n) {
  matrix()
}

arr<-factor(c("Garuda","Cebu", "British", "Jetstar", "Malindo",
              "Singapore", "Singapore", "Jetstar"))

levels(arr) <-c("BA", "5J", "GA", "3K","OD","SQ")
as.integer(arr)
factor(arr,labels=c("BA", "5J", "GA", "3K","OD","SQ"))
arr
arr_mix<-c("OD", "Singapore", "Garuda", "3K", "British", 
           "5J", "GA", "Cebu", "3K", "SQ")

