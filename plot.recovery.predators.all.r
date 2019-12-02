plot.recovery.predators.all<-function(mst.plaus=1,mlt.plaus=1,nst.plaus=1,nlt.plaus=1,Aspect=1,Xlimits=c(-0.1,1.35),Ylimits=c(-0.1,1.1)){
  
  ## Created from plot.risk.predators by ESK on 09/22/2016
  
  library(lattice)
  
  # First want the 25%, 35% and 45% allocation runs (just really need option 1)   
  file.path1A<-"C:/Users/Emily Klein/Desktop/SWFSC/FOOSA/Pixelation/Pixel_foosa/risk_25/"
  load(paste(file.path1A,"mst.1.merge.rel.75.risk",sep=""))
  mst1A <- mst.1.merge.rel.75.risk

  file.path1B<-"C:/Users/Emily Klein/Desktop/SWFSC/FOOSA/Pixelation/Pixel_foosa/risk_35/"
  load(paste(file.path1B,"mst.1.merge.rel.75.risk",sep=""))
  mst1B <- mst.1.merge.rel.75.risk
  
  file.path1C<-"C:/Users/Emily Klein/Desktop/SWFSC/FOOSA/Pixelation/Pixel_foosa/risk_45/"
  load(paste(file.path1C,"mst.1.merge.rel.75.risk",sep=""))
  mst1C <- mst.1.merge.rel.75.risk

    # Next want the historical fishing patterns (focus on Opn 1 and on 7 "current" from 2015 Ecol App)
  file.path2<-"C:/Users/Emily Klein/Desktop/SWFSC/FOOSA/Pixelation/Pixel_foosa/risk_prev_updated/"
  load(paste(file.path2,"mst.1.merge.rel.75.risk",sep=""))
  mst1 <- mst.1.merge.rel.75.risk
#   load(paste(file.path2,"mst.7.merge.rel.75.risk",sep=""))
#   mst7 <- mst.7.merge.rel.75.risk
  
  # pull out prob recovered for each predator - do for all scenarios (mst, mlt, nst, nlt) x fishing options (1 2 3 7)
  # nomenclature: [predator][fishingoption].[movement][sensitivity]
  p1A.mst<-as.vector(apply(mst1A$risk$pengs$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  p1B.mst<-as.vector(apply(mst1B$risk$pengs$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  p1C.mst<-as.vector(apply(mst1C$risk$pengs$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  p1.mst<-as.vector(apply(mst1$risk$pengs$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  # p7.mst<-as.vector(apply(mst7$risk$pengs$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  #
  s1A.mst<-as.vector(apply(mst1A$risk$seals$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  s1B.mst<-as.vector(apply(mst1B$risk$seals$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  s1C.mst<-as.vector(apply(mst1C$risk$seals$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  s1.mst<-as.vector(apply(mst1$risk$seals$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  # s7.mst<-as.vector(apply(mst7$risk$seals$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  #
  w1A.mst<-as.vector(apply(mst1A$risk$whales$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  w1B.mst<-as.vector(apply(mst1B$risk$whales$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  w1C.mst<-as.vector(apply(mst1C$risk$whales$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  w1.mst<-as.vector(apply(mst1$risk$whales$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  # w7.mst<-as.vector(apply(mst7$risk$whales$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  #
  f1A.mst<-as.vector(apply(mst1A$risk$fish$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  f1B.mst<-as.vector(apply(mst1B$risk$fish$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  f1C.mst<-as.vector(apply(mst1C$risk$fish$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  f1.mst<-as.vector(apply(mst1$risk$fish$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  # f7.mst<-as.vector(apply(mst7$risk$fish$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  #
  #
  load(paste(file.path1A,"mlt.1.merge.rel.75.risk",sep=""))
  mlt1A <- mlt.1.merge.rel.75.risk
  load(paste(file.path1B,"mlt.1.merge.rel.75.risk",sep=""))
  mlt1B <- mlt.1.merge.rel.75.risk
  load(paste(file.path1C,"mlt.1.merge.rel.75.risk",sep=""))
  mlt1C <- mlt.1.merge.rel.75.risk
  load(paste(file.path2,"mlt.1.merge.rel.75.risk",sep=""))
  mlt1 <- mlt.1.merge.rel.75.risk
#   load(paste(file.path2,"mlt.7.merge.rel.75.risk",sep=""))
#   mlt7 <- mlt.7.merge.rel.75.risk
  #
  p1A.mlt<-as.vector(apply(mlt1A$risk$pengs$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  p1B.mlt<-as.vector(apply(mlt1B$risk$pengs$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  p1C.mlt<-as.vector(apply(mlt1C$risk$pengs$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  p1.mlt<-as.vector(apply(mlt1$risk$pengs$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  # p7.mlt<-as.vector(apply(mlt7$risk$pengs$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  #
  s1A.mlt<-as.vector(apply(mlt1A$risk$seals$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  s1B.mlt<-as.vector(apply(mlt1B$risk$seals$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  s1C.mlt<-as.vector(apply(mlt1C$risk$seals$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  s1.mlt<-as.vector(apply(mlt1$risk$seals$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  # s7.mlt<-as.vector(apply(mlt7$risk$seals$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  #
  w1A.mlt<-as.vector(apply(mlt1A$risk$whales$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  w1B.mlt<-as.vector(apply(mlt1B$risk$whales$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  w1C.mlt<-as.vector(apply(mlt1C$risk$whales$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  w1.mlt<-as.vector(apply(mlt1$risk$whales$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  # w7.mlt<-as.vector(apply(mlt7$risk$whales$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  #
  f1A.mlt<-as.vector(apply(mlt1A$risk$fish$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  f1B.mlt<-as.vector(apply(mlt1B$risk$fish$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  f1C.mlt<-as.vector(apply(mlt1C$risk$fish$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  f1.mlt<-as.vector(apply(mlt1$risk$fish$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  # f7.mlt<-as.vector(apply(mlt7$risk$fish$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  #
  #
  load(paste(file.path1A,"nst.1.merge.rel.75.risk",sep=""))
  nst1A <- nst.1.merge.rel.75.risk
  load(paste(file.path1B,"nst.1.merge.rel.75.risk",sep=""))
  nst1B <- nst.1.merge.rel.75.risk
  load(paste(file.path1C,"nst.1.merge.rel.75.risk",sep=""))
  nst1C <- nst.1.merge.rel.75.risk
  load(paste(file.path2,"nst.1.merge.rel.75.risk",sep=""))
  nst1 <- nst.1.merge.rel.75.risk
#   load(paste(file.path2,"nst.7.merge.rel.75.risk",sep=""))
#   nst7 <- nst.7.merge.rel.75.risk
  #
  p1A.nst<-as.vector(apply(nst1A$risk$pengs$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  p1B.nst<-as.vector(apply(nst1B$risk$pengs$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  p1C.nst<-as.vector(apply(nst1C$risk$pengs$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  p1.nst<-as.vector(apply(nst1$risk$pengs$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  # p7.nst<-as.vector(apply(nst7$risk$pengs$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  #
  s1A.nst<-as.vector(apply(nst1A$risk$seals$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  s1B.nst<-as.vector(apply(nst1B$risk$seals$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  s1C.nst<-as.vector(apply(nst1C$risk$seals$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  s1.nst<-as.vector(apply(nst1$risk$seals$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  # s7.nst<-as.vector(apply(nst7$risk$seals$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  #
  w1A.nst<-as.vector(apply(nst1A$risk$whales$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  w1B.nst<-as.vector(apply(nst1B$risk$whales$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  w1C.nst<-as.vector(apply(nst1C$risk$whales$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  w1.nst<-as.vector(apply(nst1$risk$whales$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  # w7.nst<-as.vector(apply(nst7$risk$whales$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  #   #
  f1A.nst<-as.vector(apply(nst1A$risk$fish$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  f1B.nst<-as.vector(apply(nst1B$risk$fish$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  f1C.nst<-as.vector(apply(nst1C$risk$fish$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  f1.nst<-as.vector(apply(nst1$risk$fish$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  # f7.nst<-as.vector(apply(nst7$risk$fish$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  #
  #
  load(paste(file.path1A,"nlt.1.merge.rel.75.risk",sep=""))
  nlt1A <- nlt.1.merge.rel.75.risk
  load(paste(file.path1B,"nlt.1.merge.rel.75.risk",sep=""))
  nlt1B <- nlt.1.merge.rel.75.risk
  load(paste(file.path1C,"nlt.1.merge.rel.75.risk",sep=""))
  nlt1C <- nlt.1.merge.rel.75.risk
  load(paste(file.path2,"nlt.1.merge.rel.75.risk",sep=""))
  nlt1 <- nlt.1.merge.rel.75.risk
#   load(paste(file.path2,"nlt.7.merge.rel.75.risk",sep=""))
#   nlt7 <- nlt.7.merge.rel.75.risk
#   #
  p1A.nlt<-as.vector(apply(nlt1A$risk$pengs$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  p1B.nlt<-as.vector(apply(nlt1B$risk$pengs$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  p1C.nlt<-as.vector(apply(nlt1C$risk$pengs$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  p1.nlt<-as.vector(apply(nlt1$risk$pengs$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  # p7.nlt<-as.vector(apply(nlt7$risk$pengs$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  #
  s1A.nlt<-as.vector(apply(nlt1A$risk$seals$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  s1B.nlt<-as.vector(apply(nlt1B$risk$seals$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  s1C.nlt<-as.vector(apply(nlt1C$risk$seals$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  s1.nlt<-as.vector(apply(nlt1$risk$seals$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  # s7.nlt<-as.vector(apply(nlt7$risk$seals$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  #
  w1A.nlt<-as.vector(apply(nlt1A$risk$whales$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  w1B.nlt<-as.vector(apply(nlt1B$risk$whales$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  w1C.nlt<-as.vector(apply(nlt1C$risk$whales$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  w1.nlt<-as.vector(apply(nlt1$risk$whales$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  # w7.nlt<-as.vector(apply(nlt7$risk$whales$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  #
  f1A.nlt<-as.vector(apply(nlt1A$risk$fish$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  f1B.nlt<-as.vector(apply(nlt1B$risk$fish$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  f1C.nlt<-as.vector(apply(nlt1C$risk$fish$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  f1.nlt<-as.vector(apply(nlt1$risk$fish$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  # f7.nlt<-as.vector(apply(nlt7$risk$fish$P.notrecovered,2,FUN=function(x){c(x,NA)}))
  #
  #
  # combine output for each predator x fishing option --> matrix, get weighted mean for each
  # use the plausibility weights
  # 25% in 48.1
  p1A<-matrix(c(p1A.mst,p1A.mlt,p1A.nst,p1A.nlt),ncol=4,nrow=length(p1A.mst))
  p1A.mean<-apply(p1A,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  s1A<-matrix(c(s1A.mst,s1A.mlt,s1A.nst,s1A.nlt),ncol=4,nrow=length(s1A.mst))
  s1A.mean<-apply(s1A,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  w1A<-matrix(c(w1A.mst,w1A.mlt,w1A.nst,w1A.nlt),ncol=4,nrow=length(w1A.mst))
  w1A.mean<-apply(w1A,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  f1A<-matrix(c(f1A.mst,f1A.mlt,f1A.nst,f1A.nlt),ncol=4,nrow=length(f1A.mst))
  f1A.mean<-apply(f1A,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  # 35% in 48.1
  p1B<-matrix(c(p1B.mst,p1B.mlt,p1B.nst,p1B.nlt),ncol=4,nrow=length(p1B.mst))
  p1B.mean<-apply(p1B,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  s1B<-matrix(c(s1B.mst,s1B.mlt,s1B.nst,s1B.nlt),ncol=4,nrow=length(s1B.mst))
  s1B.mean<-apply(s1B,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  w1B<-matrix(c(w1B.mst,w1B.mlt,w1B.nst,w1B.nlt),ncol=4,nrow=length(w1B.mst))
  w1B.mean<-apply(w1B,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  f1B<-matrix(c(f1B.mst,f1B.mlt,f1B.nst,f1B.nlt),ncol=4,nrow=length(f1B.mst))
  f1B.mean<-apply(f1B,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  # 45% in 48.1
  p1C<-matrix(c(p1C.mst,p1C.mlt,p1C.nst,p1C.nlt),ncol=4,nrow=length(p1C.mst))
  p1C.mean<-apply(p1C,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  s1C<-matrix(c(s1C.mst,s1C.mlt,s1C.nst,s1C.nlt),ncol=4,nrow=length(s1C.mst))
  s1C.mean<-apply(s1C,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  w1C<-matrix(c(w1C.mst,w1C.mlt,w1C.nst,w1C.nlt),ncol=4,nrow=length(w1C.mst))
  w1C.mean<-apply(w1C,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  f1C<-matrix(c(f1C.mst,f1C.mlt,f1C.nst,f1C.nlt),ncol=4,nrow=length(f1C.mst))
  f1C.mean<-apply(f1C,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  # Original option 1
  p1<-matrix(c(p1.mst,p1.mlt,p1.nst,p1.nlt),ncol=4,nrow=length(p1.mst))
  p1.mean<-apply(p1,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  s1<-matrix(c(s1.mst,s1.mlt,s1.nst,s1.nlt),ncol=4,nrow=length(s1.mst))
  s1.mean<-apply(s1,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  w1<-matrix(c(w1.mst,w1.mlt,w1.nst,w1.nlt),ncol=4,nrow=length(w1.mst))
  w1.mean<-apply(w1,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  f1<-matrix(c(f1.mst,f1.mlt,f1.nst,f1.nlt),ncol=4,nrow=length(f1.mst))
  f1.mean<-apply(f1,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  # Original optn 7
#   p7<-matrix(c(p7.mst,p7.mlt,p7.nst,p7.nlt),ncol=4,nrow=length(p7.mst))
#   p7.mean<-apply(p7,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
#   s7<-matrix(c(s7.mst,s7.mlt,s7.nst,s7.nlt),ncol=4,nrow=length(s7.mst))
#   s7.mean<-apply(s7,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
#   w7<-matrix(c(w7.mst,w7.mlt,w7.nst,w7.nlt),ncol=4,nrow=length(w7.mst))
#   w7.mean<-apply(w7,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
#   f7<-matrix(c(f7.mst,f7.mlt,f7.nst,f7.nlt),ncol=4,nrow=length(f7.mst))
#   f7.mean<-apply(f7,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  #   
  #add NA to back end of each element so that lines are separated
  
  gall <- get.gamma.fractions(1)
  g7 <- get.gamma.fractions(7)
  Lall <- length(gall)+1
  L7 <- length(g7)+1
  
  # write over data for each predator x fishing option matrix of scenario (mlt mst nlt nst) output as dataframes
  # bind into one for each predator ([pred].data)
  p1A<-data.frame(gamma=rep(c(gall,NA),15),prob=p1A.mean,ssmu=rep(1:15,each=Lall),option=rep("25% in 48.1",length(p1A.mean)),pred=rep("Penguins",length(p1A.mean)))
  p1B<-data.frame(gamma=rep(c(gall,NA),15),prob=p1B.mean,ssmu=rep(1:15,each=Lall),option=rep("35% in 48.1",length(p1B.mean)),pred=rep("Penguins",length(p1B.mean)))
  p1C<-data.frame(gamma=rep(c(gall,NA),15),prob=p1C.mean,ssmu=rep(1:15,each=Lall),option=rep("45% in 48.1",length(p1C.mean)),pred=rep("Penguins",length(p1C.mean)))
  p1<-data.frame(gamma=rep(c(gall,NA),15),prob=p1.mean,ssmu=rep(1:15,each=Lall),option=rep("Previous Effort",length(p1.mean)),pred=rep("Penguins",length(p1.mean)))
  # p7<-data.frame(gamma=rep(c(g7,NA),15),prob=p7.mean,ssmu=rep(1:15,each=L7),option=rep("Previous dist, Opn 7",length(p7.mean)),pred=rep("Penguins",length(p7.mean)))
  p.data<-rbind(p1A,p1B,p1C,p1)
  #
  s1A<-data.frame(gamma=rep(c(gall,NA),15),prob=s1A.mean,ssmu=rep(1:15,each=Lall),option=rep("25% in 48.1",length(s1A.mean)),pred=rep("Seals",length(s1A.mean)))
  s1B<-data.frame(gamma=rep(c(gall,NA),15),prob=s1B.mean,ssmu=rep(1:15,each=Lall),option=rep("35% in 48.1",length(s1B.mean)),pred=rep("Seals",length(s1B.mean)))
  s1C<-data.frame(gamma=rep(c(gall,NA),15),prob=s1C.mean,ssmu=rep(1:15,each=Lall),option=rep("45% in 48.1",length(s1C.mean)),pred=rep("Seals",length(s1C.mean)))
  s1<-data.frame(gamma=rep(c(gall,NA),15),prob=s1.mean,ssmu=rep(1:15,each=Lall),option=rep("Previous Effort",length(s1.mean)),pred=rep("Seals",length(s1.mean)))
  # s7<-data.frame(gamma=rep(c(g7,NA),15),prob=s7.mean,ssmu=rep(1:15,each=L7),option=rep("option 7, previous",length(s7.mean)),pred=rep("Seals",length(s7.mean)))
  s.data<-rbind(s1A,s1B,s1C,s1)
  #
  w1A<-data.frame(gamma=rep(c(gall,NA),15),prob=w1A.mean,ssmu=rep(1:15,each=Lall),option=rep("25% in 48.1",length(w1A.mean)),pred=rep("Whales",length(w1A.mean)))
  w1B<-data.frame(gamma=rep(c(gall,NA),15),prob=w1B.mean,ssmu=rep(1:15,each=Lall),option=rep("35% in 48.1",length(w1B.mean)),pred=rep("Whales",length(w1B.mean)))
  w1C<-data.frame(gamma=rep(c(gall,NA),15),prob=w1C.mean,ssmu=rep(1:15,each=Lall),option=rep("45% in 48.1",length(w1C.mean)),pred=rep("Whales",length(w1C.mean)))
  w1<-data.frame(gamma=rep(c(gall,NA),15),prob=w1.mean,ssmu=rep(1:15,each=Lall),option=rep("Previous Effort",length(w1.mean)),pred=rep("Whales",length(w1.mean)))
  # w7<-data.frame(gamma=rep(c(g7,NA),15),prob=w7.mean,ssmu=rep(1:15,each=L7),option=rep("option 7, previous",length(w7.mean)),pred=rep("Whales",length(w7.mean)))
  w.data<-rbind(w1A,w1B,w1C,w1)
  #
  f1A<-data.frame(gamma=rep(c(gall,NA),15),prob=f1A.mean,ssmu=rep(1:15,each=Lall),option=rep("25% in 48.1",length(f1A.mean)),pred=rep("Fish",length(f1A.mean)))
  f1B<-data.frame(gamma=rep(c(gall,NA),15),prob=f1B.mean,ssmu=rep(1:15,each=Lall),option=rep("35% in 48.1",length(f1B.mean)),pred=rep("Fish",length(f1B.mean)))
  f1C<-data.frame(gamma=rep(c(gall,NA),15),prob=f1C.mean,ssmu=rep(1:15,each=Lall),option=rep("45% in 48.1",length(f1C.mean)),pred=rep("Fish",length(f1C.mean)))
  f1<-data.frame(gamma=rep(c(gall,NA),15),prob=f1.mean,ssmu=rep(1:15,each=Lall),option=rep("Previous Effort",length(f1.mean)),pred=rep("Fish",length(f1.mean)))
  # f7<-data.frame(gamma=rep(c(g7,NA),15),prob=f7.mean,ssmu=rep(1:15,each=L7),option=rep("option 7, previous",length(f7.mean)),pred=rep("Fish",length(f7.mean)))
  f.data<-rbind(f1A,f1B,f1C,f1)

  # combine all predator output - can look at option 7, but this is currently exactly the same as option 1 - ESK 8.26.2016
  new.data<-rbind(f.data,p.data,s.data,w.data)
  # new.data$option<-ordered(new.data$option,levels=c("option 1 25%", "option 1 45%", "option 2", "option 3"))
  new.data$option<-ordered(new.data$option,levels=c("Previous Effort", "25% in 48.1", "35% in 48.1","45% in 48.1"))
  
  # for subareas
  new.data$loc <- ifelse(is.element(new.data$ssmu,c(1:8)),"48.1", ifelse(is.element(new.data$ssmu, c(9:12)),"48.2", "48.3")) 
  # for SSMUs
  # new.data$loc <- ifelse(is.element(new.data$ssmu,c(5, 6)),"SSMU 5 & 6", "Other SSMUs")
  
  ssmu.col <- c("goldenrod3","gold", "orange", "red", "red4", "maroon3","hotpink","lightpink", "blue4", "blue", "deepskyblue", "cyan", "chartreuse", "green3", "darkgreen")
  
  #plot on gamma, grouped by predator (pred), using whole dataframe - updated to run x ssmu ESK 8.26.2016
  xyplot(prob~gamma|option*pred,groups = ssmu, data=new.data,type="l", panel=function(x,y,...){
    #panel.superpose(x,y,...,col=c("black","blue","red","green"),lty=c(1,1,1,1))
    panel.superpose(x,y,...)
    panel.abline(v=c(0.026,0.11,1.0),lty=2)
    # panel.text(1, 4, labels = panel.number())
  }
  ,layout=c(4,4,1),aspect=Aspect,xlim=c(0.0, 0.13), ylim=Ylimits, xlab="yield multiplier",ylab="probability",
  # par.settings=list(superpose.line=list(col=c("red", "blue", "black"),lty=c(1,1,1,1), lwd=1)),
  par.settings=list(superpose.line=list(col=ssmu.col,lty=c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2), lwd=1)),
  # par.settings=list(superpose.line=list(col=c("blue","red"),lty=c(1,1,1,1))),
  main=list(label="Risk to Predators, Recovery",cex=1.5,fontface="plain"),
  strip=strip.custom(style=1, bg="transparent",strip.levels = rep(TRUE,16)),
  scale=list(alternating=FALSE,axs="r"),
  auto.key =list(space="right", columns=1, lines = TRUE, points = FALSE, 
                 par.settings=list(superpose.line=list(col=c("red","blue", "black"))))) 
  #  auto.key =list(space="top", columns=2, lines = TRUE, points = FALSE, 
  #                 par.settings=list(superpose.line=list(col=c("red","blue")))))
  
}
