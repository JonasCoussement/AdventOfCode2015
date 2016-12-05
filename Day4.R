library(digest)
sumof5 <- 1
number <- 1
startstring <- "bgvyzdsv"
while(sumof5>0){
  string <- paste0(startstring,number)
  hash <- digest(string, algo="md5", serialize=F)
  hashstart <- suppressWarnings(sum(as.numeric(unlist(sapply(hash, function(x) strsplit(as.character(x),"")))[1:6])))
  if(is.na(hashstart)){
  } else {
    if(hashstart==0){
      print(number)
      sumof5 = 0
    }
  }
  print(number)
  number <- number+1
}
