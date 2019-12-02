bt.abund.var<-function(sd.krill.Rdev=0.85,nyears=60,nseasons=2,ntrials=100,krec.seed=123,btmeans=bt.means.trend){
  # Function to generate variable bathtub krill abundances for no trend and trend scenarios
  # Generate nyears*nseasons*ntrials values from a normal distribution
  # original by S. Hill (provided in email dated 9 May 2008)
  # edited GMW 12 May 2008:  generalized number of rows and columns in krec.temp matrix
  var.krec<-sd.krill.Rdev^2
  mean.krec<- -var.krec/2
  set.seed(krec.seed)
  krec.temp<-matrix(data=rnorm(nyears*nseasons*ntrials, mean=mean.krec,sd=sd.krill.Rdev),nrow=nyears*nseasons,ncol=ntrials)
  bt1.n.krec<-btmeans[,1]*exp(krec.temp)
  bt2.n.krec<-btmeans[,2]*exp(krec.temp)
  bt3.n.krec<-btmeans[,3]*exp(krec.temp)
  bt.list<-list(tub1=bt1.n.krec,tub2=bt2.n.krec,tub3=bt3.n.krec)
  bt.list
}

