mv.dotplot.fit.local <- function(fit, vars, xlabels=NULL,ncol=NULL){
  d<-extract(fit, vars)
  r<-NULL
  if(is.null(xlabels)){
    xlabels<-1:dim(d[[1]])[2]
  }
  for( i in 1:length(vars)){
    m=apply(d[[i]], 2, mean)
    l=apply(d[[i]], 2, hdi)[1,]
    u=apply(d[[i]], 2, hdi)[2,]
    r<-rbind(r,data.frame(dv=vars[i],ix=xlabels,mean=m, lower=l, upper=u))
  }
  ggplot(r, aes(x=ix,y=mean,ymin=lower,ymax=upper))+geom_point()+
    geom_errorbar(width=.1)+facet_wrap(~dv, ncol=ncol)+
    geom_hline(y=0,colour='red')+theme_bw()
}
