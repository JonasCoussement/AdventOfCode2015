input <- unlist(read.csv("Day21.txt",header=FALSE))
Boss.stats <- as.numeric(sapply(as.character(input), function(x) strsplit(x,": ")[[1]][2]))
Player.stats <- c(100,0,0)
store <- as.matrix(read.table("Day21_store.txt",header=FALSE,fill=TRUE, blank.lines.skip=TRUE))
#cleanup rings
rings <- which(store=="Rings:")
for(i in seq((rings+1),length(store[,1]),by=2)){
  store[i,1] <- paste0(store[i,1],store[i,2])
  store[i,2] <- store[i,3]
  store[i,3] <- store[i,4]
  store[i,4] <- store[i+1,1]
}
#remove now empty rows
store <- store[-seq((rings+2),length(store[,1]),by=2),]
#make individual matrices and set numeric
store.weapon <- store[2:(which(store=="Armor:")-1),]
store.armor <- store[(which(store=="Armor:")+1):(which(store=="Rings:")-1),]
store.rings <- store[(which(store=="Rings:")+1):length(store[,1]),]
store.weapon.num <- apply(store.weapon[,2:4],1:2,as.numeric)
store.armor.num <- apply(store.armor[,2:4],1:2,as.numeric)
store.rings.num <- apply(store.rings[,2:4],1:2,as.numeric)
store.num <- rbind(store.weapon.num,store.armor.num,store.rings.num)
#######################
###END OF DATA INPUT###
#######################

#add options of no armor and no rings
store.armor.num <- rbind(rep(0,3),store.armor.num)
store.rings.num <- rbind(rep(0,3),store.rings.num)
#attack rings
store.rings.num.att <- store.rings.num[c(1,which(store.rings.num[,2]>0)),]
store.rings.num.def <- store.rings.num[c(1,which(store.rings.num[,3]>0)),]
#calculate damage combinations and cost matrices
################################################
#only 1 weapon possible, max of 2 rings
possibledmg <- sapply(store.weapon.num[,2], function(x) x+store.rings.num.att[,2]) #1 weapon + 0 or 1 rings
ringcombinations.2 <- combn(length(store.rings.num.att[,1])-1,2) #possible combinations of 2 attack rings
ringcombineddmg.2 <- t(sapply(seq(1,length(ringcombinations.2),by=2), function(x) 
  store.rings.num.att[(ringcombinations.2+1),][x,]+
    store.rings.num.att[(ringcombinations.2+1),][x+1,])) #dmg and cost of these combinations
possibledmg <- rbind(possibledmg, sapply(store.weapon.num[,2], function(x) x+ringcombineddmg.2[,2]))
#coin cost of dmg
dmgcost.coin <- sapply(store.weapon.num[,1], function(x) x+store.rings.num.att[,1])
dmgcost.coin <- rbind(dmgcost.coin, sapply(store.weapon.num[,1], function(x) x+ringcombineddmg.2[,1]))
#ring cost of dmg
dmgcost.ring <- matrix(rep(c(0,rep(1,length(store.rings.num.att[,1])-1),rep(2,length(ringcombinations.2[1,]))),
                           length(store.weapon[,1])),ncol=length(store.weapon[,1]))

#calculate armor combinations and cost matrices
###############################################
#minimum 0 armor, max 1, 2 rings possible
possiblearmor <- sapply(store.armor.num[,3], function(x) x+store.rings.num.def[,3]) #0 or 1 armor + 0 or 1 rings
ringcombinations.2 <- combn(length(store.rings.num.def[,1])-1,2) #possible combinations of 2 def rings
ringcombinedarmor.2 <- t(sapply(seq(1,length(ringcombinations.2),by=2), function(x) 
  store.rings.num.def[(ringcombinations.2+1),][x,]+
    store.rings.num.def[(ringcombinations.2+1),][x+1,])) #armor and cost of these combinations
possiblearmor <- rbind(possiblearmor, sapply(store.armor.num[,3], function(x) x+ringcombinedarmor.2[,3]))
#coin cost of armor
armorcost.coin <- sapply(store.armor.num[,1], function(x) x+store.rings.num.def[,1])
armorcost.coin <- rbind(armorcost.coin, sapply(store.armor.num[,1], function(x) x+ringcombinedarmor.2[,1]))
#ring cost of armor
armorcost.ring <- matrix(rep(c(0,rep(1,length(store.rings.num.def[,1])-1),rep(2,length(ringcombinations.2[1,]))),
                           length(store.armor[,1])+1),ncol=length(store.armor[,1])+1)

#simulate bossfight
###################
gold <- 1000
for(x1 in 1:length(possibledmg[,1])){
  for(x2 in 1:length(possibledmg[1,])){
    for(y1 in 1:length(possiblearmor[,1])){
      for(y2 in 1:length(possiblearmor[1,])){
        #check if we didn't use too many rings
        if(dmgcost.ring[x1,x2]+armorcost.ring[y1,y2]<=2){
          #check if current cost is lower than previous success
          if(dmgcost.coin[x1,x2]+armorcost.coin[y1,y2]<gold){
            Boss.stats.current <- Boss.stats
            Player.stats.current <- Player.stats+c(0,possibledmg[x1,x2],possiblearmor[y1,y2])
            while(Boss.stats.current[1]>0&&Player.stats.current[1]>0){
              #player attack
              Boss.stats.current[1] <- Boss.stats.current[1]-max(c(Player.stats.current[2]-Boss.stats.current[3],1))
              #if boss survived, attack back
              if(Boss.stats.current[1]>0){
                Player.stats.current[1]<-Player.stats.current[1]-max(c(Boss.stats.current[2]-Player.stats.current[3],1))
              } else { #else player wins, update lower gold cost
                gold <- dmgcost.coin[x1,x2]+armorcost.coin[y1,y2]
                print(gold)
              }
            }
          }
        }
      }
    }
  }
}
gold

#PART 2
#######
gold <- 0
for(x1 in 1:length(possibledmg[,1])){
  for(x2 in 1:length(possibledmg[1,])){
    for(y1 in 1:length(possiblearmor[,1])){
      for(y2 in 1:length(possiblearmor[1,])){
        #check if we didn't use too many rings
        if(dmgcost.ring[x1,x2]+armorcost.ring[y1,y2]<=2){
          #check if current cost is higher than previous failure
          if(dmgcost.coin[x1,x2]+armorcost.coin[y1,y2]>gold){
            Boss.stats.current <- Boss.stats
            Player.stats.current <- Player.stats+c(0,possibledmg[x1,x2],possiblearmor[y1,y2])
            while(Boss.stats.current[1]>0&&Player.stats.current[1]>0){
              #player attack
              Boss.stats.current[1] <- Boss.stats.current[1]-max(c(Player.stats.current[2]-Boss.stats.current[3],1))
              #if boss survived, attack back
              if(Boss.stats.current[1]>0){
                Player.stats.current[1]<-Player.stats.current[1]-max(c(Boss.stats.current[2]-Player.stats.current[3],1))
              } 
              if(Player.stats.current[1]<0){
                #boss wins, update higher gold value
                gold <- dmgcost.coin[x1,x2]+armorcost.coin[y1,y2]
                print(gold)
              }
            }
          }
        }
      }
    }
  }
}
gold
