plot.risk.predators.all<-function(mst.plaus=1,mlt.plaus=1,nst.plaus=1,nlt.plaus=1,Aspect=1,Xlimits=c(-0.1,1.35),Ylimits=c(-0.1,1.1)){
  ## George Watters
  ## last edited 19 April 2009
  ##
  ## see fig 2 WGG-EMM-09/12
  ## This code calls output from across various scenarios
  
  library(lattice)

# Call the fishing options of interest from the initial runs   
  file.path1A<-"C:/Users/emily/Desktop/SWFSC/FOOSA/SWFSC R/Testing/risk_GGP_A/"
  load(paste(file.path1A,"mst.1.merge.rel.75.risk",sep=""))
  mst1A <- mst.1.merge.rel.75.risk
#   load(paste(file.path1A,"mst.2.merge.rel.75.risk",sep=""))
#   mst2A <- mst.2.merge.rel.75.risk
  load(paste(file.path1A,"mst.3.merge.rel.75.risk",sep=""))
  mst3A <- mst.3.merge.rel.75.risk
  
  file.path1B<-"C:/Users/emily/Desktop/SWFSC/FOOSA/SWFSC R/Testing/risk_GGP_B/"
  load(paste(file.path1B,"mst.1.merge.rel.75.risk",sep=""))
  mst1B <- mst.1.merge.rel.75.risk
#   load(paste(file.path1B,"mst.2.merge.rel.75.risk",sep=""))
#   mst2B <- mst.2.merge.rel.75.risk
  load(paste(file.path1B,"mst.3.merge.rel.75.risk",sep=""))
  mst3B <- mst.3.merge.rel.75.risk
  # file.path1C<-"C:/Users/emily/Desktop/SWFSC/FOOSA/MPA_2018/MPA_FBM/risk/"
  # load(paste(file.path1C,"mst.1.merge.rel.75.risk",sep=""))
  # mst1C <- mst.1.merge.rel.75.risk
  
# Call the additional model runs from alternative scenarios for comparison
  # file.path2<- "C:/Users/emily/Desktop/SWFSC/FOOSA/MPA_2018/MPA_FBM/risk/"
  # load(paste(file.path2,"mst.1.merge.rel.75.risk",sep=""))
  # mst1 <- mst.1.merge.rel.75.risk
#   load(paste(file.path2,"mst.7.merge.rel.75.risk",sep=""))
#   mst7 <- mst.7.merge.rel.75.risk

# # Finally option to include equal distribution runs
#   file.path4<-"C:/Users/Emily Klein/Desktop/SWFSC/FOOSA/51-07 update/51_07 Foosa/risk_A/"
#   load(paste(file.path4,"mst.1.merge.rel.75.risk",sep=""))
#   mst1C <- mst.1.merge.rel.75.risk
#   
#   file.path5<-"C:/Users/Emily Klein/Desktop/SWFSC/FOOSA/51-07 update/51_07 Foosa/risk_B/"
#   load(paste(file.path5,"mst.1.merge.rel.75.risk",sep=""))
#   mst1D <- mst.1.merge.rel.75.risk
  
  ## THIS NEEDS A LOT OF CLEANING 
  # pull out prob depleted for each predator - do for all scenarios (mst, mlt, nst, nlt) x fishing options (1 2 3 7)
  # nomenclature: [predator][fishingoption].[movement][sensitivity]
  p1A.mst<-as.vector(apply(mst1A$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  p1B.mst<-as.vector(apply(mst1B$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
#   p2.mst<-as.vector(apply(mst2$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  p3A.mst<-as.vector(apply(mst3A$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  p3B.mst<-as.vector(apply(mst3B$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  # p1.mst<-as.vector(apply(mst1$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
#   p7.mst<-as.vector(apply(mst7$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
#   p1C.mst<-as.vector(apply(mst1C$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
#   p1D.mst<-as.vector(apply(mst1D$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  #
  s1A.mst<-as.vector(apply(mst1A$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  s1B.mst<-as.vector(apply(mst1B$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  # s1C.mst<-as.vector(apply(mst1C$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
#   s2.mst<-as.vector(apply(mst2$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  s3A.mst<-as.vector(apply(mst3A$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  s3B.mst<-as.vector(apply(mst3B$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
# s7.mst<-as.vector(apply(mst7$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
#   s1C.mst<-as.vector(apply(mst1C$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
#   s1D.mst<-as.vector(apply(mst1D$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  #
  w1A.mst<-as.vector(apply(mst1A$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  w1B.mst<-as.vector(apply(mst1B$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  # w1C.mst<-as.vector(apply(mst1C$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
#   w2.mst<-as.vector(apply(mst2$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  w3A.mst<-as.vector(apply(mst3A$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  w3B.mst<-as.vector(apply(mst3B$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
# w7.mst<-as.vector(apply(mst7$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
#   w1C.mst<-as.vector(apply(mst1C$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
#   w1D.mst<-as.vector(apply(mst1D$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  #
  f1A.mst<-as.vector(apply(mst1A$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  f1B.mst<-as.vector(apply(mst1B$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  # f1C.mst<-as.vector(apply(mst1C$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
#   f2.mst<-as.vector(apply(mst2$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  f3A.mst<-as.vector(apply(mst3A$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  f3B.mst<-as.vector(apply(mst3B$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
# f7.mst<-as.vector(apply(mst7$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
#   f1C.mst<-as.vector(apply(mst1C$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
#   f1D.mst<-as.vector(apply(mst1D$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  #
  load(paste(file.path1A,"mlt.1.merge.rel.75.risk",sep=""))
  mlt1A <- mlt.1.merge.rel.75.risk
#   load(paste(file.path1A,"mlt.2.merge.rel.75.risk",sep=""))
#   mlt2 <- mlt.2.merge.rel.75.risk
  load(paste(file.path1A,"mlt.3.merge.rel.75.risk",sep=""))
  mlt3A <- mlt.3.merge.rel.75.risk
  
  load(paste(file.path1B,"mlt.1.merge.rel.75.risk",sep=""))
  mlt1B <- mlt.1.merge.rel.75.risk
  load(paste(file.path1B,"mlt.3.merge.rel.75.risk",sep=""))
  mlt3B <- mlt.3.merge.rel.75.risk

  # load(paste(file.path2,"mlt.1.merge.rel.75.risk",sep=""))
  # mlt1 <- mlt.1.merge.rel.75.risk
  #   load(paste(file.path1C,"mlt.7.merge.rel.75.risk",sep=""))
  #   mlt7 <- mlt.7.merge.rel.75.risk
  #   load(paste(file.path1C,"mlt.7.merge.rel.75.risk",sep=""))
#   mlt7 <- mlt.7.merge.rel.75.risk
#  
#   load(paste(file.path4,"mlt.1.merge.rel.75.risk",sep=""))
#   mlt1C <- mlt.1.merge.rel.75.risk
#   load(paste(file.path5,"mlt.7.merge.rel.75.risk",sep=""))
#   mlt1D <- mlt.1.merge.rel.75.risk
  #
  p1A.mlt<-as.vector(apply(mlt1A$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  p1B.mlt<-as.vector(apply(mlt1B$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  # p1C.mlt<-as.vector(apply(mlt1C$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
#   p2.mlt<-as.vector(apply(mlt2$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  p3A.mlt<-as.vector(apply(mlt3A$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  p3B.mlt<-as.vector(apply(mlt3B$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
# p7.mlt<-as.vector(apply(mlt7$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
#   p1C.mlt<-as.vector(apply(mlt1C$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
#   p1D.mlt<-as.vector(apply(mlt1D$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  #
  s1A.mlt<-as.vector(apply(mlt1A$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  s1B.mlt<-as.vector(apply(mlt1B$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  # s1C.mlt<-as.vector(apply(mlt1C$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
#   s2.mlt<-as.vector(apply(mlt2$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  s3A.mlt<-as.vector(apply(mlt3A$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  s3B.mlt<-as.vector(apply(mlt3B$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
# s7.mlt<-as.vector(apply(mlt7$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
#   s1C.mlt<-as.vector(apply(mlt1C$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
#   s1D.mlt<-as.vector(apply(mlt1D$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  #
  w1A.mlt<-as.vector(apply(mlt1A$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  w1B.mlt<-as.vector(apply(mlt1B$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  # w1C.mlt<-as.vector(apply(mlt1C$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
#   w2.mlt<-as.vector(apply(mlt2$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  w3A.mlt<-as.vector(apply(mlt3A$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  w3B.mlt<-as.vector(apply(mlt3B$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
# w7.mlt<-as.vector(apply(mlt7$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
#   w1C.mlt<-as.vector(apply(mlt1C$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
#   w1D.mlt<-as.vector(apply(mlt1D$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  #
  f1A.mlt<-as.vector(apply(mlt1A$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  f1B.mlt<-as.vector(apply(mlt1B$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  # f1C.mlt<-as.vector(apply(mlt1C$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
#   f2.mlt<-as.vector(apply(mlt2$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  f3A.mlt<-as.vector(apply(mlt3A$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  f3B.mlt<-as.vector(apply(mlt3B$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
# f7.mlt<-as.vector(apply(mlt7$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
#   f1C.mlt<-as.vector(apply(mlt1C$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
#   f1D.mlt<-as.vector(apply(mlt1D$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  #
  load(paste(file.path1A,"nst.1.merge.rel.75.risk",sep=""))
  nst1A <- nst.1.merge.rel.75.risk
#   load(paste(file.path,"nst.2.merge.rel.75.risk",sep=""))
#   nst2 <- nst.2.merge.rel.75.risk
  load(paste(file.path1A,"nst.3.merge.rel.75.risk",sep=""))
  nst3A <- nst.3.merge.rel.75.risk
  #
  load(paste(file.path1B,"nst.1.merge.rel.75.risk",sep=""))
  nst1B <- nst.1.merge.rel.75.risk
  load(paste(file.path1B,"nst.3.merge.rel.75.risk",sep=""))
  nst3B <- nst.3.merge.rel.75.risk
  # #
  # load(paste(file.path2,"nst.1.merge.rel.75.risk",sep=""))
  # nst1 <- nst.1.merge.rel.75.risk
#   load(paste(file.path2,"nst.7.merge.rel.75.risk",sep=""))
#   nst7 <- nst.7.merge.rel.75.risk
  #
#   load(paste(file.path3,"nst.1.merge.rel.75.risk",sep=""))
#   nst1C <- nst.1.merge.rel.75.risk
#   load(paste(file.path4,"nst.1.merge.rel.75.risk",sep=""))
#   nst1D <- nst.1.merge.rel.75.risk
# #
  p1A.nst<-as.vector(apply(nst1A$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  p1B.nst<-as.vector(apply(nst1B$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  # p1C.nst<-as.vector(apply(nst1C$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  #   p2.nst<-as.vector(apply(nst2$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  p3A.nst<-as.vector(apply(nst3A$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  p3B.nst<-as.vector(apply(nst3B$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
# p7.nst<-as.vector(apply(nst7$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
#   p1C.nst<-as.vector(apply(nst1C$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
#   p1D.nst<-as.vector(apply(nst1D$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  #
  s1A.nst<-as.vector(apply(nst1A$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  s1B.nst<-as.vector(apply(nst1B$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  # s1C.nst<-as.vector(apply(nst1C$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
#   s2.nst<-as.vector(apply(nst2$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  s3A.nst<-as.vector(apply(nst3A$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  s3B.nst<-as.vector(apply(nst3B$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
# s7.nst<-as.vector(apply(nst7$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
#   s1C.nst<-as.vector(apply(nst1C$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
#   s1D.nst<-as.vector(apply(nst1D$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  #
  w1A.nst<-as.vector(apply(nst1A$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  w1B.nst<-as.vector(apply(nst1B$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  # w1C.nst<-as.vector(apply(nst1C$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
#   w2.nst<-as.vector(apply(nst2$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  w3A.nst<-as.vector(apply(nst3A$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  w3B.nst<-as.vector(apply(nst3B$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
# w7.nst<-as.vector(apply(nst7$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
#   w1C.nst<-as.vector(apply(nst1C$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
#   w1D.nst<-as.vector(apply(nst1D$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  #   #
  f1A.nst<-as.vector(apply(nst1A$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  f1B.nst<-as.vector(apply(nst1B$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  # f1C.nst<-as.vector(apply(nst1C$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
#   f2.nst<-as.vector(apply(nst2$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  f3A.nst<-as.vector(apply(nst3A$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  f3B.nst<-as.vector(apply(nst3B$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
# f7.nst<-as.vector(apply(nst7$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
#   f1C.nst<-as.vector(apply(nst1C$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
#   f1D.nst<-as.vector(apply(nst1D$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  #
  #
  load(paste(file.path1A,"nlt.1.merge.rel.75.risk",sep=""))
  nlt1A <- nlt.1.merge.rel.75.risk
#   load(paste(file.path1A,"nlt.2.merge.rel.75.risk",sep=""))
#   nlt2 <- nlt.2.merge.rel.75.risk
  load(paste(file.path1A,"nlt.3.merge.rel.75.risk",sep=""))
  nlt3A <- nlt.3.merge.rel.75.risk
   #
  load(paste(file.path1B,"nlt.1.merge.rel.75.risk",sep=""))
  nlt1B <- nlt.1.merge.rel.75.risk
  load(paste(file.path1B,"nlt.3.merge.rel.75.risk",sep=""))
  nlt3B <- nlt.3.merge.rel.75.risk
  #
  # load(paste(file.path2,"nlt.1.merge.rel.75.risk",sep=""))
  # nlt1 <- nlt.1.merge.rel.75.risk
#   load(paste(file.path2,"nlt.7.merge.rel.75.risk",sep=""))
#   nlt7 <- nlt.7.merge.rel.75.risk
# #
#   load(paste(file.path4,"nlt.1.merge.rel.75.risk",sep=""))
#   nlt1C <- nlt.1.merge.rel.75.risk
#   load(paste(file.path5,"nlt.1.merge.rel.75.risk",sep=""))
#   nlt1D <- nlt.1.merge.rel.75.risk
  #
  p1A.nlt<-as.vector(apply(nlt1A$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  p1B.nlt<-as.vector(apply(nlt1B$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  # p1C.nlt<-as.vector(apply(nlt1C$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
#   p2.nlt<-as.vector(apply(nlt2$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  p3A.nlt<-as.vector(apply(nlt3A$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  p3B.nlt<-as.vector(apply(nlt3B$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
# p7.nlt<-as.vector(apply(nlt7$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
#   p1C.nlt<-as.vector(apply(nlt1C$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
#   p1D.nlt<-as.vector(apply(nlt1D$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  #
  s1A.nlt<-as.vector(apply(nlt1A$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  s1B.nlt<-as.vector(apply(nlt1B$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  # s1C.nlt<-as.vector(apply(nlt1C$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
#   s2.nlt<-as.vector(apply(nlt2$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  s3A.nlt<-as.vector(apply(nlt3A$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  s3B.nlt<-as.vector(apply(nlt3B$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
# s7.nlt<-as.vector(apply(nlt7$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
#   s1C.nlt<-as.vector(apply(nlt1C$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
#   s1D.nlt<-as.vector(apply(nlt1D$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  #
  w1A.nlt<-as.vector(apply(nlt1A$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  w1B.nlt<-as.vector(apply(nlt1B$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  # w1C.nlt<-as.vector(apply(nlt1C$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
#   w2.nlt<-as.vector(apply(nlt2$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  w3A.nlt<-as.vector(apply(nlt3A$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  w3B.nlt<-as.vector(apply(nlt3B$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
# w7.nlt<-as.vector(apply(nlt7$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
#   w1C.nlt<-as.vector(apply(nlt1C$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
#   w1D.nlt<-as.vector(apply(nlt1D$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  #
  f1A.nlt<-as.vector(apply(nlt1A$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  f1B.nlt<-as.vector(apply(nlt1B$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  # f1C.nlt<-as.vector(apply(nlt1C$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
#   f2.nlt<-as.vector(apply(nlt2$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  f3A.nlt<-as.vector(apply(nlt3A$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  f3B.nlt<-as.vector(apply(nlt3B$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
# f7.nlt<-as.vector(apply(nlt7$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
#   f1C.nlt<-as.vector(apply(nlt1C$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
#   f1D.nlt<-as.vector(apply(nlt1D$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
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
#   35% in 48.1
  p1B<-matrix(c(p1B.mst,p1B.mlt,p1B.nst,p1B.nlt),ncol=4,nrow=length(p1B.mst))
  p1B.mean<-apply(p1B,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  s1B<-matrix(c(s1B.mst,s1B.mlt,s1B.nst,s1B.nlt),ncol=4,nrow=length(s1B.mst))
  s1B.mean<-apply(s1B,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  w1B<-matrix(c(w1B.mst,w1B.mlt,w1B.nst,w1B.nlt),ncol=4,nrow=length(w1B.mst))
  w1B.mean<-apply(w1B,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  f1B<-matrix(c(f1B.mst,f1B.mlt,f1B.nst,f1B.nlt),ncol=4,nrow=length(f1B.mst))
  f1B.mean<-apply(f1B,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
#   45% in 48.1
  # p1C<-matrix(c(p1C.mst,p1C.mlt,p1C.nst,p1C.nlt),ncol=4,nrow=length(p1C.mst))
  # p1C.mean<-apply(p1C,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  # s1C<-matrix(c(s1C.mst,s1C.mlt,s1C.nst,s1C.nlt),ncol=4,nrow=length(s1C.mst))
  # s1C.mean<-apply(s1C,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  # w1C<-matrix(c(w1C.mst,w1C.mlt,w1C.nst,w1C.nlt),ncol=4,nrow=length(w1C.mst))
  # w1C.mean<-apply(w1C,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  # f1C<-matrix(c(f1C.mst,f1C.mlt,f1C.nst,f1C.nlt),ncol=4,nrow=length(f1C.mst))
  # f1C.mean<-apply(f1C,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
# Option 2 - 45%
#   p2<-matrix(c(p2.mst,p2.mlt,p2.nst,p2.nlt),ncol=4,nrow=length(p2.mst))
#   p2.mean<-apply(p2,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
#   s2<-matrix(c(s2.mst,s2.mlt,s2.nst,s2.nlt),ncol=4,nrow=length(s2.mst))
#   s2.mean<-apply(s2,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
#   w2<-matrix(c(w2.mst,w2.mlt,w2.nst,w2.nlt),ncol=4,nrow=length(w2.mst))
#   w2.mean<-apply(w2,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
#   f2<-matrix(c(f2.mst,f2.mlt,f2.nst,f2.nlt),ncol=4,nrow=length(f2.mst))
#   f2.mean<-apply(f2,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
#   # Option 3 - 45%
  p3A<-matrix(c(p3A.mst,p3A.mlt,p3A.nst,p3A.nlt),ncol=4,nrow=length(p3A.mst))
  p3A.mean<-apply(p3A,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  s3A<-matrix(c(s3A.mst,s3A.mlt,s3A.nst,s3A.nlt),ncol=4,nrow=length(s3A.mst))
  s3A.mean<-apply(s3A,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  w3A<-matrix(c(w3A.mst,w3A.mlt,w3A.nst,w3A.nlt),ncol=4,nrow=length(w3A.mst))
  w3A.mean<-apply(w3A,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  f3A<-matrix(c(f3A.mst,f3A.mlt,f3A.nst,f3A.nlt),ncol=4,nrow=length(f3A.mst))
  f3A.mean<-apply(f3A,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  # Original option 1
  p3B<-matrix(c(p3B.mst,p3B.mlt,p3B.nst,p3B.nlt),ncol=4,nrow=length(p3B.mst))
  p3B.mean<-apply(p3B,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  s3B<-matrix(c(s3B.mst,s3B.mlt,s3B.nst,s3B.nlt),ncol=4,nrow=length(s3B.mst))
  s3B.mean<-apply(s3B,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  w3B<-matrix(c(w3B.mst,w3B.mlt,w3B.nst,w3B.nlt),ncol=4,nrow=length(w3B.mst))
  w3B.mean<-apply(w3B,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  f3B<-matrix(c(f3B.mst,f3B.mlt,f3B.nst,f3B.nlt),ncol=4,nrow=length(f3B.mst))
  f3B.mean<-apply(f3B,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  # Original optn 7
#   p7<-matrix(c(p7.mst,p7.mlt,p7.nst,p7.nlt),ncol=4,nrow=length(p7.mst))
#   p7.mean<-apply(p7,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
#   s7<-matrix(c(s7.mst,s7.mlt,s7.nst,s7.nlt),ncol=4,nrow=length(s7.mst))
#   s7.mean<-apply(s7,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
#   w7<-matrix(c(w7.mst,w7.mlt,w7.nst,w7.nlt),ncol=4,nrow=length(w7.mst))
#   w7.mean<-apply(w7,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
#   f7<-matrix(c(f7.mst,f7.mlt,f7.nst,f7.nlt),ncol=4,nrow=length(f7.mst))
#   f7.mean<-apply(f7,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  # Equal weghts by SSMU
#   p1C<-matrix(c(p1C.mst,p1C.mlt,p1C.nst,p1C.nlt),ncol=4,nrow=length(p1C.mst))
#   p1C.mean<-apply(p1C,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
#   s1C<-matrix(c(s1C.mst,s1C.mlt,s1C.nst,s1C.nlt),ncol=4,nrow=length(s1C.mst))
#   s1C.mean<-apply(s1C,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
#   w1C<-matrix(c(w1C.mst,w1C.mlt,w1C.nst,w1C.nlt),ncol=4,nrow=length(w1C.mst))
#   w1C.mean<-apply(w1C,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
#   f1C<-matrix(c(f1C.mst,f1C.mlt,f1C.nst,f1C.nlt),ncol=4,nrow=length(f1C.mst))
#   f1C.mean<-apply(f1C,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
#   # All equal catch X SSMUs
#   p1D<-matrix(c(p1D.mst,p1D.mlt,p1D.nst,p1D.nlt),ncol=4,nrow=length(p1D.mst))
#   p1D.mean<-apply(p1D,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
#   s1D<-matrix(c(s1D.mst,s1D.mlt,s1D.nst,s1D.nlt),ncol=4,nrow=length(s1D.mst))
#   s1D.mean<-apply(s1D,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
#   w1D<-matrix(c(w1D.mst,w1D.mlt,w1D.nst,w1D.nlt),ncol=4,nrow=length(w1D.mst))
#   w1D.mean<-apply(w1D,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
#   f1D<-matrix(c(f1D.mst,f1D.mlt,f1D.nst,f1D.nlt),ncol=4,nrow=length(f1D.mst))
#   f1D.mean<-apply(f1D,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
#   
  #add NA to back end of each element so that lines are separated

gall <- get.gamma.fractions(1)
g7 <- get.gamma.fractions(7)
Lall <- length(gall)+1
L7 <- length(g7)+1
  
# write over data for each predator x fishing option matrix of scenario (mlt mst nlt nst) output as dataframes
# bind into one for each predator ([pred].data)
  p1A<-data.frame(gamma=rep(c(gall,NA),15),prob=p1A.mean,ssmu=rep(1:15,each=Lall),option=rep("Opn 1A",length(p1A.mean)),pred=rep("Penguins",length(p1A.mean)))
  p1B<-data.frame(gamma=rep(c(gall,NA),15),prob=p1B.mean,ssmu=rep(1:15,each=Lall),option=rep("Opn 1B",length(p1B.mean)),pred=rep("Penguins",length(p1B.mean)))
  # p1C<-data.frame(gamma=rep(c(gall,NA),15),prob=p1C.mean,ssmu=rep(1:15,each=Lall),option=rep("45% in 48.1",length(p1C.mean)),pred=rep("Penguins",length(p1C.mean)))
#   p2<-data.frame(gamma=rep(c(gall,NA),15),prob=p2.mean,ssmu=rep(1:15,each=Lall),option=rep("option 2",length(p2.mean)),pred=rep("Penguins",length(p2.mean)))
  p3A<-data.frame(gamma=rep(c(gall,NA),15),prob=p3A.mean,ssmu=rep(1:15,each=Lall),option=rep("Opn 3A",length(p3A.mean)),pred=rep("Penguins",length(p3A.mean)))
  p3B<-data.frame(gamma=rep(c(gall,NA),15),prob=p3B.mean,ssmu=rep(1:15,each=Lall),option=rep("Opn 3B",length(p3B.mean)),pred=rep("Penguins",length(p3B.mean)))
  # p2<-data.frame(gamma=rep(c(gall,NA),15),prob=p2.mean,ssmu=rep(1:15,each=Lall),option=rep("Updated Foraging",length(p1.mean)),pred=rep("Penguins",length(p1.mean)))
# p7<-data.frame(gamma=rep(c(g7,NA),15),prob=p7.mean,ssmu=rep(1:15,each=L7),option=rep("option 7, previous",length(p7.mean)),pred=rep("Penguins",length(p7.mean)))
#   p1C<-data.frame(gamma=rep(c(gall,NA),15),prob=p1C.mean,ssmu=rep(1:15,each=Lall),option=rep("Equal %, subarea",length(p1C.mean)),pred=rep("Penguins",length(p1C.mean)))
#   p1D<-data.frame(gamma=rep(c(gall,NA),15),prob=p1D.mean,ssmu=rep(1:15,each=Lall),option=rep("Equal % X all",length(p1D.mean)),pred=rep("Penguins",length(p1D.mean)))
#   #p.data<-rbind(p1A,p1,p2,p3,p7)
  p.data<-rbind(p1A,p1B,p3A,p3B)
  #
  s1A<-data.frame(gamma=rep(c(gall,NA),15),prob=s1A.mean,ssmu=rep(1:15,each=Lall),option=rep("Opn 1A",length(s1A.mean)),pred=rep("Seals",length(s1A.mean)))
  s1B<-data.frame(gamma=rep(c(gall,NA),15),prob=s1B.mean,ssmu=rep(1:15,each=Lall),option=rep("Opn 1B",length(s1B.mean)),pred=rep("Seals",length(s1B.mean)))
  # s1C<-data.frame(gamma=rep(c(gall,NA),15),prob=s1C.mean,ssmu=rep(1:15,each=Lall),option=rep("45% in 48.1",length(s1C.mean)),pred=rep("Seals",length(s1C.mean)))
#   s2<-data.frame(gamma=rep(c(gall,NA),15),prob=s2.mean,ssmu=rep(1:15,each=Lall),option=rep("option 2",length(s2.mean)),pred=rep("Seals",length(s2.mean)))
  s3A<-data.frame(gamma=rep(c(gall,NA),15),prob=s3A.mean,ssmu=rep(1:15,each=Lall),option=rep("Opn 3A",length(s3A.mean)),pred=rep("Seals",length(s3A.mean)))
  s3B<-data.frame(gamma=rep(c(gall,NA),15),prob=s3B.mean,ssmu=rep(1:15,each=Lall),option=rep("Opn 3B",length(s3B.mean)),pred=rep("Seals",length(s3B.mean)))
# s7<-data.frame(gamma=rep(c(g7,NA),15),prob=s7.mean,ssmu=rep(1:15,each=L7),option=rep("option 7, previous",length(s7.mean)),pred=rep("Seals",length(s7.mean)))
#   s1C<-data.frame(gamma=rep(c(gall,NA),15),prob=s1C.mean,ssmu=rep(1:15,each=Lall),option=rep("Equal %, subarea",length(s1C.mean)),pred=rep("Seals",length(s1C.mean)))
#   s1D<-data.frame(gamma=rep(c(gall,NA),15),prob=s1D.mean,ssmu=rep(1:15,each=Lall),option=rep("Equal % X all",length(s1D.mean)),pred=rep("Seals",length(s1D.mean)))
  # s.data<-rbind(s1A,s1,s2,s3,s7)
  s.data<-rbind(s1A,s1B,s3A,s3B)
  #
  w1A<-data.frame(gamma=rep(c(gall,NA),15),prob=w1A.mean,ssmu=rep(1:15,each=Lall),option=rep("Opn 1A",length(w1A.mean)),pred=rep("Whales",length(w1A.mean)))
  w1B<-data.frame(gamma=rep(c(gall,NA),15),prob=w1B.mean,ssmu=rep(1:15,each=Lall),option=rep("Opn 1B",length(w1B.mean)),pred=rep("Whales",length(w1B.mean)))
  # w1C<-data.frame(gamma=rep(c(gall,NA),15),prob=w1C.mean,ssmu=rep(1:15,each=Lall),option=rep("45% in 48.1",length(w1C.mean)),pred=rep("Whales",length(w1C.mean)))
#   w2<-data.frame(gamma=rep(c(gall,NA),15),prob=w2.mean,ssmu=rep(1:15,each=Lall),option=rep("option 2",length(w2.mean)),pred=rep("Whales",length(w2.mean)))
  w3A<-data.frame(gamma=rep(c(gall,NA),15),prob=w3A.mean,ssmu=rep(1:15,each=Lall),option=rep("Opn 3A",length(w3A.mean)),pred=rep("Whales",length(w3A.mean)))
  w3B<-data.frame(gamma=rep(c(gall,NA),15),prob=w3B.mean,ssmu=rep(1:15,each=Lall),option=rep("Opn 3B",length(w3B.mean)),pred=rep("Whales",length(w3B.mean)))
# w7<-data.frame(gamma=rep(c(g7,NA),15),prob=w7.mean,ssmu=rep(1:15,each=L7),option=rep("option 7, previous",length(w7.mean)),pred=rep("Whales",length(w7.mean)))
#   w1C<-data.frame(gamma=rep(c(gall,NA),15),prob=w1C.mean,ssmu=rep(1:15,each=Lall),option=rep("Equal %, subarea",length(w1C.mean)),pred=rep("Whales",length(w1C.mean)))
#   w1D<-data.frame(gamma=rep(c(gall,NA),15),prob=w1D.mean,ssmu=rep(1:15,each=Lall),option=rep("Equal % X all",length(w1D.mean)),pred=rep("Whales",length(w1D.mean)))
  # w.data<-rbind(w1A,w1,w2,w3,w7)
  w.data<-rbind(w1A,w1B,w3A,w3B)
  #
  f1A<-data.frame(gamma=rep(c(gall,NA),15),prob=f1A.mean,ssmu=rep(1:15,each=Lall),option=rep("Opn 1A",length(f1A.mean)),pred=rep("Fish",length(f1A.mean)))
  f1B<-data.frame(gamma=rep(c(gall,NA),15),prob=f1B.mean,ssmu=rep(1:15,each=Lall),option=rep("Opn 1B",length(f1B.mean)),pred=rep("Fish",length(f1B.mean)))
  # f1C<-data.frame(gamma=rep(c(gall,NA),15),prob=f1C.mean,ssmu=rep(1:15,each=Lall),option=rep("45% in 48.1",length(f1C.mean)),pred=rep("Fish",length(f1C.mean)))
#   f2<-data.frame(gamma=rep(c(gall,NA),15),prob=f2.mean,ssmu=rep(1:15,each=Lall),option=rep("option 2",length(f2.mean)),pred=rep("Fish",length(f2.mean)))
  f3A<-data.frame(gamma=rep(c(gall,NA),15),prob=f3A.mean,ssmu=rep(1:15,each=Lall),option=rep("Opn 3A",length(f3A.mean)),pred=rep("Fish",length(f3A.mean)))
  f3B<-data.frame(gamma=rep(c(gall,NA),15),prob=f3B.mean,ssmu=rep(1:15,each=Lall),option=rep("Opn 3B",length(f3B.mean)),pred=rep("Fish",length(f3B.mean)))
# f7<-data.frame(gamma=rep(c(g7,NA),15),prob=f7.mean,ssmu=rep(1:15,each=L7),option=rep("option 7, previous",length(f7.mean)),pred=rep("Fish",length(f7.mean)))
#   f1C<-data.frame(gamma=rep(c(gall,NA),15),prob=f1C.mean,ssmu=rep(1:15,each=Lall),option=rep("Equal %, subarea",length(f1C.mean)),pred=rep("Fish",length(f1C.mean)))
#   f1D<-data.frame(gamma=rep(c(gall,NA),15),prob=f1D.mean,ssmu=rep(1:15,each=Lall),option=rep("Equal % X all",length(f1D.mean)),pred=rep("Fish",length(f1D.mean)))
  # f.data<-rbind(f1A,f1,f2,f3,f7)
  f.data<-rbind(f1A,f1B,f3A,f3B)
  
# combine all predator output - can look at option 7, but this is currently exactly the same as option 1 - ESK 8.26.2016
  new.data<-rbind(f.data,p.data,s.data,w.data)
  # new.data$option<-ordered(new.data$option,levels=c("option 1 25%", "option 1 45%", "option 2", "Opn 1B"))
  new.data$option<-ordered(new.data$option,levels=c("Opn 1A","Opn 1B","Opn 3A", "Opn 3B"))

# for subareas
  new.data$loc <- ifelse(is.element(new.data$ssmu,c(1:8)),"48.1", ifelse(is.element(new.data$ssmu, c(9:12)),"48.2", "48.3")) 
# for SSMUs
  # new.data$loc <- ifelse(is.element(new.data$ssmu,c(5, 6)),"SSMU 5 & 6", "Other SSMUs")
  
ssmu.col <- c("goldenrod3","gold", "orange", "red", "red4", "maroon3","hotpink","lightpink", "blue4", "blue", "deepskyblue", "cyan", "chartreuse", "green3", "darkgreen")

   #plot on gamma, grouped by predator (pred), using whole dataframe - updated to run x ssmu ESK 8.26.2016
 xyplot(prob~gamma|option*pred, groups=ssmu, data=new.data,type="l", panel=function(x,y,...){
   #panel.superpose(x,y,...,col=c("black","blue","red","green"),lty=c(1,1,1,1))
   panel.superpose(x,y,...)
   panel.abline(v=c(0.026,0.11,1.0),lty=2)
   # panel.text(1, 4, labels = panel.number())
 }
 ,layout=c(4,4,1),aspect=Aspect,xlim=c(0.0,1.1), ylim=Ylimits, xlab="yield multiplier",ylab="probability",
 # par.settings=list(superpose.line=list(col=c("red", "blue", "black"),lty=c(1,1,1,1), lwd=1)),
 par.settings=list(superpose.line=list(col=ssmu.col,lty=c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2), lwd=1)),
 # par.settings=list(superpose.line=list(col=c("blue","red"),lty=c(1,1,1,1))),
 main=list(label="Risk to Predators",cex=1.5,fontface="plain"),
 strip=strip.custom(style=1, bg="transparent",strip.levels = rep(TRUE,16)),
 scale=list(alternating=FALSE,axs="r"),
 auto.key =list(space="right", columns=1, lines = TRUE, points = FALSE, 
               par.settings=list(superpose.line=list(col=c("red","blue", "black"))))) 
#  auto.key =list(space="top", columns=2, lines = TRUE, points = FALSE, 
#                 par.settings=list(superpose.line=list(col=c("red","blue")))))

}
