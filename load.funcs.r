load.funcs<-function(path.string="C:/Users/emily/Desktop/SWFSC/FOOSA/MPA_2018/MPA_GGP"){
  tt.list<-list.files(path.string)
  for(i in 1:length(tt.list)){
    source(paste(path.string,"/",tt.list[i],sep=""))
  }
}