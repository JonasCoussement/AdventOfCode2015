input <- read.csv("Day2.txt",header=FALSE)
inputlist <- sapply(input[,1], function(x) strsplit(as.character(x),"x"))
inputarray <-as.numeric(unlist(inputlist))
packing <- t(sapply(seq(1,length(inputarray),by=3), function(x) inputarray[(x+0:2)]))
packing.sort <- apply(packing, 1, sort)
packing.size <- sum(apply(packing.sort, 2, function(x) 3*x[1]*x[2]+2*x[1]*x[3]+2*x[2]*x[3]))
ribbon.size <- sum(apply(packing.sort, 2, function(x) 2*x[1]+2*x[2]+x[1]*x[2]*x[3]))
packing.size
ribbon.size
