input <- read.csv("Day5.txt",header=FALSE)[,1]
nice<-0
for(i in 1:length(input)){
  singles <- unlist(strsplit(as.character(input[i]),""))
  duos <- sapply(2:length(singles),function(x) paste0(singles[(x-1)],singles[(x)]))
  if(sum(is.na(match(c("ab","cd","pq","xy"),duos)))==4){
    if(sum(sapply(c("a","e","i","o","u"),function(x) unlist(gregexpr(x, singles)))+1)<6){
      #not enough vowels
    } else {
      if(sum(sapply(duos,function(x) length(unique(unlist(strsplit(as.character(x),""))))))<((length(singles)-1)*2)){
        nice <- nice+1
      }
    }
  }
}
nice

#part 2
nice<-0
duosc <- 0
for(i in 1:length(input)){
  singles <- unlist(strsplit(as.character(input[i]),""))
  #singles <- unlist(strsplit(as.character("sknufchjdvccccta"),""))
  duos <- sapply(2:length(singles),function(x) paste0(singles[(x-1)],singles[(x)]))
  trios <- sapply(3:length(singles),function(x) paste0(paste0(singles[(x-2)],singles[(x-1)]),singles[x]))
  matchingduos <- sapply(duos, function(x) which(duos==x))
  dowehaveanyduos <- length(which(as.vector(unlist(sapply(matchingduos,diff)))>1))>=1
  areyousure <- length(which(sapply(matchingduos,length)>2))>1
  if(dowehaveanyduos||areyousure){
    duosc <- duosc+1
    dowehaveanypatterns <- sapply(trios,function(x) unlist(strsplit(as.character(x),""))[1]==unlist(strsplit(as.character(x),""))[3])
    if(sum(dowehaveanypatterns)>0){
      nice=nice+1
    }
  }
  }
}
duosc
nice