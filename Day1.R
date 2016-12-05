input <- read.csv("Day1.txt",header=FALSE)
charinput <- unlist(strsplit(as.character(input[1,1]),""))
#solution part 1 with sorted array
intime <- Sys.time()
charsorted <- sort(charinput)
open <- sum(sapply(charsorted, function(x) x == "("))
total <- open - (length(charsorted)-open)
total
bench <- Sys.time()-intime
bench
#solution to part 1 without sort
intime <- Sys.time()
open <- sum(sapply(charinput, function(x) x == "("))
total <- open - (length(charsorted)-open)
total
bench <- Sys.time()-intime
bench
#part 2
position <- 0
instruct <- 0
while(position>=0){
  instruct <- instruct+1
  position <- position+(charinput[instruct]=="(")*2-1
}
instruct