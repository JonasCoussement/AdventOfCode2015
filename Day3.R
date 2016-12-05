input <- read.csv("Day3.txt",header=FALSE)[1,1]
inputlist <- unlist(sapply(input, function(x) strsplit(as.character(x),"")))
posities <- matrix(rep(0,length(inputlist)*2+2),nrow=length(inputlist)+1,ncol=2)
for(i in 1:length(inputlist)){
  posities[i+1,] <- posities[i,]+switch(inputlist[i],"^"=c(0,1),"v"=c(0,-1),"<"=c(-1,0),">"=c(1,0))
}
length(unique(posities)[,1])

#part 2

posities.santa <- matrix(rep(0,length(inputlist)+2),nrow=length(inputlist)/2+1,ncol=2)
posities.robo <- matrix(rep(0,length(inputlist)+2),nrow=length(inputlist)/2+1,ncol=2)
for(i in seq(1,length(inputlist),by=2)){
  posities.santa[((i+1)/2+1),] <- posities.santa[((i+1)/2),]+switch(inputlist[i],"^"=c(0,1),"v"=c(0,-1),"<"=c(-1,0),">"=c(1,0))
  posities.robo[((i+1)/2+1),] <- posities.robo[((i+1)/2),]+switch(inputlist[i+1],"^"=c(0,1),"v"=c(0,-1),"<"=c(-1,0),">"=c(1,0))
}
length(unique(rbind(posities.santa,posities.robo))[,1])
