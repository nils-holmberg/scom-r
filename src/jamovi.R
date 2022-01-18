#~ ##################################################################### 200908
#testing jamovi, spss replacement
install.packages("jmv", lib="~/lib/r-cran")
library(jmv)
library(datasets)

#get data
data = iris
head(data)

#descriptive analysis
outp = jmv::descriptives(
data = data,
vars = names(data)[c(5,1:4)],
freq=T,
dens=T,
bar=T,
box=T,
sd=T)









