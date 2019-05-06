library(microbenchmark)
load(url("http://michaelgastner.com/dowjones.Rda"))
abs_change <- diff(close_value)

#Remove first element in month so that abs_change[i] will be the
#the change in month[i]
month<-month[-1]

rel_change <- 100* abs_change/close_value[-length(close_value)]

msub<-function(){
  for (i in seq_along(month.name)) {
    rel_change[month==month.name[i]]
  }}

mseq<-function(){
  for (i in seq_along(month.name)) {
    rel_change[seq(1, length(month), 12)]
  }
}
microbenchmark(mseq(),msub())

f<-rep(1,1000)
for (i in 3:1000) {
  f[i]<-f[i-2] + f[i-1]
}
print(f)