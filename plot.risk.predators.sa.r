plot.risk.predators.sa<-function(mst.plaus=1,mlt.plaus=1,nst.plaus=1,nlt.plaus=1,Aspect=1,Xlimits=c(-0.1,1.35),Ylimits=c(-0.1,1.1)){
  ## George Watters
  ## last edited 19 April 2009
  ##
  ## and by JM 25/1/12
  ## see fig 2 WGG-EMM-09/12
  ##
  ## Updated by ESK for CM 51-07 in summer 2016
  
  library(lattice)
  
  file.path<-"C:/Users/emily/Desktop/SWFSC/FOOSA/51_07 Foosa/risk/"
  load(paste(file.path,"mst.1.merge.rel.75.risk",sep=""))
  mst1 <- mst.1.merge.rel.75.risk
  load(paste(file.path,"mst.2.merge.rel.75.risk",sep=""))
  mst2 <- mst.2.merge.rel.75.risk
  load(paste(file.path,"mst.3.merge.rel.75.risk",sep=""))
  mst3 <- mst.3.merge.rel.75.risk
  load(paste(file.path,"mst.7.merge.rel.75.risk",sep=""))
  mst7 <- mst.7.merge.rel.75.risk
  
  # pull out prob depleted for each predator - do for all scenarios (mst, mlt, nst, nlt) x fishing options (1 2 3 7)
  # nomenclature: [predator][fishingoption].[movement][stable]
  p1.mst<-as.vector(apply(mst1$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  p2.mst<-as.vector(apply(mst2$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  p3.mst<-as.vector(apply(mst3$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  p7.mst<-as.vector(apply(mst7$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  #
  s1.mst<-as.vector(apply(mst1$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  s2.mst<-as.vector(apply(mst2$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  s3.mst<-as.vector(apply(mst3$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  s7.mst<-as.vector(apply(mst7$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  #
  w1.mst<-as.vector(apply(mst1$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  w2.mst<-as.vector(apply(mst2$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  w3.mst<-as.vector(apply(mst3$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  w7.mst<-as.vector(apply(mst7$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  #
  f1.mst<-as.vector(apply(mst1$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  f2.mst<-as.vector(apply(mst2$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  f3.mst<-as.vector(apply(mst3$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  f7.mst<-as.vector(apply(mst7$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  #
  load(paste(file.path,"mlt.1.merge.rel.75.risk",sep=""))
  mlt1 <- mlt.1.merge.rel.75.risk
  load(paste(file.path,"mlt.2.merge.rel.75.risk",sep=""))
  mlt2 <- mlt.2.merge.rel.75.risk
  load(paste(file.path,"mlt.3.merge.rel.75.risk",sep=""))
  mlt3 <- mlt.3.merge.rel.75.risk
  load(paste(file.path,"mlt.7.merge.rel.75.risk",sep=""))
  mlt7 <- mlt.7.merge.rel.75.risk
  #
  p1.mlt<-as.vector(apply(mlt1$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  p2.mlt<-as.vector(apply(mlt2$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  p3.mlt<-as.vector(apply(mlt3$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  p7.mlt<-as.vector(apply(mlt7$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  #
  s1.mlt<-as.vector(apply(mlt1$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  s2.mlt<-as.vector(apply(mlt2$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  s3.mlt<-as.vector(apply(mlt3$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  s7.mlt<-as.vector(apply(mlt7$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  #
  w1.mlt<-as.vector(apply(mlt1$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  w2.mlt<-as.vector(apply(mlt2$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  w3.mlt<-as.vector(apply(mlt3$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  w7.mlt<-as.vector(apply(mlt7$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  #
  f1.mlt<-as.vector(apply(mlt1$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  f2.mlt<-as.vector(apply(mlt2$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  f3.mlt<-as.vector(apply(mlt3$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  f7.mlt<-as.vector(apply(mlt7$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  #
  load(paste(file.path,"nst.1.merge.rel.75.risk",sep=""))
  nst1 <- nst.1.merge.rel.75.risk
  load(paste(file.path,"nst.2.merge.rel.75.risk",sep=""))
  nst2 <- nst.2.merge.rel.75.risk
  load(paste(file.path,"nst.3.merge.rel.75.risk",sep=""))
  nst3 <- nst.3.merge.rel.75.risk
  load(paste(file.path,"nst.7.merge.rel.75.risk",sep=""))
  nst7 <- nst.7.merge.rel.75.risk
  #
  p1.nst<-as.vector(apply(nst1$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  p2.nst<-as.vector(apply(nst2$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  p3.nst<-as.vector(apply(nst3$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  p7.nst<-as.vector(apply(nst7$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  #
  s1.nst<-as.vector(apply(nst1$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  s2.nst<-as.vector(apply(nst2$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  s3.nst<-as.vector(apply(nst3$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  s7.nst<-as.vector(apply(nst7$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  #
  w1.nst<-as.vector(apply(nst1$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  w2.nst<-as.vector(apply(nst2$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  w3.nst<-as.vector(apply(nst3$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  w7.nst<-as.vector(apply(nst7$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  #
  f1.nst<-as.vector(apply(nst1$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  f2.nst<-as.vector(apply(nst2$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  f3.nst<-as.vector(apply(nst3$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  f7.nst<-as.vector(apply(nst7$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  #
  #
  load(paste(file.path,"nlt.1.merge.rel.75.risk",sep=""))
  nlt1 <- nlt.1.merge.rel.75.risk
  load(paste(file.path,"nlt.2.merge.rel.75.risk",sep=""))
  nlt2 <- nlt.2.merge.rel.75.risk
  load(paste(file.path,"nlt.3.merge.rel.75.risk",sep=""))
  nlt3 <- nlt.3.merge.rel.75.risk
  load(paste(file.path,"nlt.7.merge.rel.75.risk",sep=""))
  nlt7 <- nlt.7.merge.rel.75.risk
  #
  p1.nlt<-as.vector(apply(nlt1$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  p2.nlt<-as.vector(apply(nlt2$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  p3.nlt<-as.vector(apply(nlt3$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  p7.nlt<-as.vector(apply(nlt7$risk$pengs$P.depleted,2,FUN=function(x){c(x,NA)}))
  #
  s1.nlt<-as.vector(apply(nlt1$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  s2.nlt<-as.vector(apply(nlt2$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  s3.nlt<-as.vector(apply(nlt3$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  s7.nlt<-as.vector(apply(nlt7$risk$seals$P.depleted,2,FUN=function(x){c(x,NA)}))
  #
  w1.nlt<-as.vector(apply(nlt1$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  w2.nlt<-as.vector(apply(nlt2$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  w3.nlt<-as.vector(apply(nlt3$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  w7.nlt<-as.vector(apply(nlt7$risk$whales$P.depleted,2,FUN=function(x){c(x,NA)}))
  #
  f1.nlt<-as.vector(apply(nlt1$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  f2.nlt<-as.vector(apply(nlt2$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  f3.nlt<-as.vector(apply(nlt3$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  f7.nlt<-as.vector(apply(nlt7$risk$fish$P.depleted,2,FUN=function(x){c(x,NA)}))
  #
  # combine output for each predator x fishing option --> matrix, get weighted mean for each
  # now use the plausibility weights
  p1<-matrix(c(p1.mst,p1.mlt,p1.nst,p1.nlt),ncol=4,nrow=length(p1.mst))
  p1.mean<-apply(p1,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  s1<-matrix(c(s1.mst,s1.mlt,s1.nst,s1.nlt),ncol=4,nrow=length(s1.mst))
  s1.mean<-apply(s1,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  w1<-matrix(c(w1.mst,w1.mlt,w1.nst,w1.nlt),ncol=4,nrow=length(w1.mst))
  w1.mean<-apply(w1,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  f1<-matrix(c(f1.mst,f1.mlt,f1.nst,f1.nlt),ncol=4,nrow=length(f1.mst))
  f1.mean<-apply(f1,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  #
  p2<-matrix(c(p2.mst,p2.mlt,p2.nst,p2.nlt),ncol=4,nrow=length(p2.mst))
  p2.mean<-apply(p2,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  s2<-matrix(c(s2.mst,s2.mlt,s2.nst,s2.nlt),ncol=4,nrow=length(s2.mst))
  s2.mean<-apply(s2,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  w2<-matrix(c(w2.mst,w2.mlt,w2.nst,w2.nlt),ncol=4,nrow=length(w2.mst))
  w2.mean<-apply(w2,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  f2<-matrix(c(f2.mst,f2.mlt,f2.nst,f2.nlt),ncol=4,nrow=length(f2.mst))
  f2.mean<-apply(f2,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  #
  p3<-matrix(c(p3.mst,p3.mlt,p3.nst,p3.nlt),ncol=4,nrow=length(p3.mst))
  p3.mean<-apply(p3,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  s3<-matrix(c(s3.mst,s3.mlt,s3.nst,s3.nlt),ncol=4,nrow=length(s3.mst))
  s3.mean<-apply(s3,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  w3<-matrix(c(w3.mst,w3.mlt,w3.nst,w3.nlt),ncol=4,nrow=length(w3.mst))
  w3.mean<-apply(w3,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  f3<-matrix(c(f3.mst,f3.mlt,f3.nst,f3.nlt),ncol=4,nrow=length(f3.mst))
  f3.mean<-apply(f3,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  #
  p7<-matrix(c(p7.mst,p7.mlt,p7.nst,p7.nlt),ncol=4,nrow=length(p7.mst))
  p7.mean<-apply(p7,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  s7<-matrix(c(s7.mst,s7.mlt,s7.nst,s7.nlt),ncol=4,nrow=length(s7.mst))
  s7.mean<-apply(s7,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  w7<-matrix(c(w7.mst,w7.mlt,w7.nst,w7.nlt),ncol=4,nrow=length(w7.mst))
  w7.mean<-apply(w7,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  f7<-matrix(c(f7.mst,f7.mlt,f7.nst,f7.nlt),ncol=4,nrow=length(f7.mst))
  f7.mean<-apply(f7,1,FUN=function(x){weighted.mean(x,c(mst.plaus,mlt.plaus,nst.plaus,nlt.plaus))})
  #
  # add NA to back end of each element so that lines are separated


gall <- get.gamma.fractions(1)
g7 <- get.gamma.fractions(7)
Lall <- length(gall)+1
L7 <- length(g7)+1
  
# write over data for each predator x fishing option matrix of scenario (mlt mst nlt nst) output as dataframes
# bind into one for each predator ([pred].data)
  p1<-data.frame(gamma=rep(c(gall,NA),15),prob=p1.mean,ssmu=rep(1:15,each=Lall),option=rep("option 1",length(p1.mean)),pred=rep("p",length(p1.mean)))
  p2<-data.frame(gamma=rep(c(gall,NA),15),prob=p2.mean,ssmu=rep(1:15,each=Lall),option=rep("option 2",length(p2.mean)),pred=rep("p",length(p2.mean)))
  p3<-data.frame(gamma=rep(c(gall,NA),15),prob=p3.mean,ssmu=rep(1:15,each=Lall),option=rep("option 3",length(p3.mean)),pred=rep("p",length(p3.mean)))
  p7<-data.frame(gamma=rep(c(g7,NA),15),prob=p7.mean,ssmu=rep(1:15,each=L7),option=rep("option 7",length(p7.mean)),pred=rep("p",length(p7.mean)))
  p.data<-rbind(p1,p2,p3,p7)
#   p.data<-rbind(p1,p7)
  #
  s1<-data.frame(gamma=rep(c(gall,NA),15),prob=s1.mean,ssmu=rep(1:15,each=Lall),option=rep("option 1",length(s1.mean)),pred=rep("s",length(s1.mean)))
  s2<-data.frame(gamma=rep(c(gall,NA),15),prob=s2.mean,ssmu=rep(1:15,each=Lall),option=rep("option 2",length(s2.mean)),pred=rep("s",length(s2.mean)))
  s3<-data.frame(gamma=rep(c(gall,NA),15),prob=s3.mean,ssmu=rep(1:15,each=Lall),option=rep("option 3",length(s3.mean)),pred=rep("s",length(s3.mean)))
  s7<-data.frame(gamma=rep(c(g7,NA),15),prob=s7.mean,ssmu=rep(1:15,each=L7),option=rep("option 7",length(s7.mean)),pred=rep("s",length(s7.mean)))
 s.data<-rbind(s1,s2,s3,s7)
 # s.data<-rbind(s1,s7)
  #
  w1<-data.frame(gamma=rep(c(gall,NA),15),prob=w1.mean,ssmu=rep(1:15,each=Lall),option=rep("option 1",length(w1.mean)),pred=rep("w",length(w1.mean)))
  w2<-data.frame(gamma=rep(c(gall,NA),15),prob=w2.mean,ssmu=rep(1:15,each=Lall),option=rep("option 2",length(w2.mean)),pred=rep("w",length(w2.mean)))
  w3<-data.frame(gamma=rep(c(gall,NA),15),prob=w3.mean,ssmu=rep(1:15,each=Lall),option=rep("option 3",length(w3.mean)),pred=rep("w",length(w3.mean)))
  w7<-data.frame(gamma=rep(c(g7,NA),15),prob=w7.mean,ssmu=rep(1:15,each=L7),option=rep("option 7",length(w7.mean)),pred=rep("w",length(w7.mean)))
  w.data<-rbind(w1,w2,w3,w7)
  #  w.data<-rbind(w1,w7)
  #
  f1<-data.frame(gamma=rep(c(gall,NA),15),prob=f1.mean,ssmu=rep(1:15,each=Lall),option=rep("option 1",length(f1.mean)),pred=rep("f",length(f1.mean)))
  f2<-data.frame(gamma=rep(c(gall,NA),15),prob=f2.mean,ssmu=rep(1:15,each=Lall),option=rep("option 2",length(f2.mean)),pred=rep("f",length(f2.mean)))
  f3<-data.frame(gamma=rep(c(gall,NA),15),prob=f3.mean,ssmu=rep(1:15,each=Lall),option=rep("option 3",length(f3.mean)),pred=rep("f",length(f3.mean)))
  f7<-data.frame(gamma=rep(c(g7,NA),15),prob=f7.mean,ssmu=rep(1:15,each=L7),option=rep("option 7",length(f7.mean)),pred=rep("f",length(f7.mean)))
 f.data<-rbind(f1,f2,f3,f7)
#  f.data<-rbind(f1,f7)
  
# combine all predator output - updated to just show optn 1 and 7, ESK 8.26.2016
 new.data<-rbind(f.data,p.data,s.data,w.data)
 new.data$option<-ordered(new.data$option,levels=c("option 1", "option 2", "option 3", "option 7"))
 #   new.data$option<-ordered(new.data$option,levels=c("option 1","option 7"))

# for subareas
 new.data$loc <- ifelse(is.element(new.data$ssmu,c(1:8)),"48.1", ifelse(is.element(new.data$ssmu, c(9:12)),"48.2", "48.3"))
# for SSMUs
 # new.data$loc <- ifelse(is.element(new.data$ssmu,c(5, 6)),"SSMU 5 & 6", "Other SSMUs")
 
 #plot on gamma, grouped by predator (pred), using whole dataframe - updated to run x ssmu ESK 8.26.2016
 xyplot(prob~gamma|option*pred,groups=loc,data=new.data,type="l", panel=function(x,y,...){
   panel.superpose(x,y,...)
   panel.abline(v=c(0.026,0.11,1.0),lty=2)
  }
 ,layout=c(4,4,1),aspect=Aspect,ylim=Ylimits, xlab="yield multiplier",ylab="probability",
 # par.settings=list(superpose.line=list(col=c("red","blue","black"),lty=c(1,1,1,1))),
 par.settings=list(superpose.line=list(col=c("blue","red"),lty=c(1,1,1,1))),
 main=list(label="48.1 at 25%", cex=1.5, fontface="plain"),
 strip=strip.custom(style=1, bg="transparent",strip.levels = rep(TRUE,16)),
 scale=list(alternating=FALSE,axs="r"),
#  auto.key =list(space="top", columns=3, lines = TRUE, points = FALSE, 
#                par.settings=list(superpose.line=list(col=c("red","blue", "black"))))) 
 auto.key =list(space="top", columns=2, lines = TRUE, points = FALSE, 
                par.settings=list(superpose.line=list(col=c("red","blue"))))) 

}

#list(Xlimits, Xlimits, Xlimits, Xlimits, Xlimits, Xlimits, Xlimits, Xlimits, Xlimits, Xlimits, Xlimits, Xlimits, Xlimits, Xlimits, Xlimits, xlim2, xlim2, xlim2, xlim2, xlim2, xlim2, xlim2, xlim2, xlim2, xlim2, xlim2, xlim2, xlim2, xlim2, xlim2)

# For subareas - MAKE SURE TO UPDATE THE % CATCH LIMITS
# # For SSMUs - MAKE SURE TO UPDATE THE % CATCH LIMITS
#   xlim=Xlimits,par.settings=list(superpose.line=list(col=c("blue","red"))),
#   main=list(label="48.1 at 45%, SSMU 5 & 6 (red), all others (blue)",cex=1.5,fontface="plain"),
#   strip=strip.custom(style=1,bg="transparent"),scale=list(relation="free"), 
#   auto.key =list(space="top", columns=2, lines = TRUE, points = FALSE,
#                  par.settings=list(superpose.line=list(col=c("blue", "red"))))) 