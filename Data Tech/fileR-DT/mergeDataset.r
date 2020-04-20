setwd ("D:\\Sgmon\\Documents\\Magistrale\\Data Technology\\Progetto")
adult=data.frame(read.csv(file="adultPost.csv",header=TRUE, sep=';',na.string = c("")))
census=data.frame(read.csv(file="censusPost.csv",header=TRUE, sep=';',na.string = c("")))


rbind.all.columns <- function(x, y) {
  
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  
  x[, c(as.character(y.diff))] <- NA
  
  y[, c(as.character(x.diff))] <- NA
  
  return(rbind(x, y))
}

set=rbind.all.columns(census, adult)
write.csv2(set, file = "mergedAdultCensus.csv", na="",  row.names=FALSE)



