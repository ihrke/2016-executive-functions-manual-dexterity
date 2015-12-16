#
#
#
library(ProjectTemplate)
load.project()
library(rstan)
rstan_options(auto_write=TRUE) # for parallel

## for calling with Rscript
options <- commandArgs(trailingOnly = TRUE)
if( "--force" %in% options)
  uncache.all()
bname<-tools::file_path_sans_ext(basename(this.file.name()))
mod.fname=sprintf("./src/%s.stan", bname)

## for running on a server; set to low values for a desktop machine
n.chains=8
n.cores=8
n.iter=2000
n.warmup=1000

## combine the two tasks
d <- rbind(cbind(task=1, kinematics.task1), 
           cbind(task=2, kinematics.task2)) 

ztransform <- function(x){((x-mean(x))/sd(x))}
logplusone.ztransform <- function(y){x=log(y-min(y)+1); ((x-mean(x))/sd(x))}

## combine washer1 and washer2 (complex task required using two washers)
objects=c("pin", "washer", "collar")
n.objects=length(objects)

## transform kinematics data (variables for which it is necessary)
d[complete.cases(d),] %>% 
  mutate(object=factor(str_replace(as.character(object), "[12]", ""))) %>%
  mutate_each(funs(logplusone.ztransform), -task, -subj, -action, -object, -repetition,
                                      -PeakDisp, -MeanAngDisp) %>%
  mutate(PeakDisp=ztransform(PeakDisp), MeanAngDisp=ztransform(MeanAngDisp)) -> d 


## z-score cognitive variables within group
cog <- cognitive %>% select(subj=idnum, group=gruppe, one_of(cog.vars)) %>% group_by(group) %>%
  mutate_each(funs(ztransform), -subj, -group) %>% ungroup %>% data.frame

## save histograms
pdf(file=file.path('graphs', sprintf("%s_hist.pdf", bname)), width=20, height=15)
p <- d %>%
  gather(kinvar, kinval, one_of(kin.vars)) %>%
  ggplot(aes(x=kinval))+geom_histogram()+facet_wrap(~ kinvar, scales = "free")
print(p)

p <- cog %>% gather(cogvar,value, -subj,-group) %>%
  ggplot(aes(x=value))+geom_histogram()+facet_wrap(~cogvar) 
print(p)
dev.off()


## prepare data for Stan
data <- list(
  n=dim(d)[1],
  nsubj=length(levels(as.factor(d$subj))),
  ntasks=2,
  task=as.numeric(d$task),
  subj=as.numeric(as.factor(d$subj)),
  action=as.numeric(d$action),
  object=as.numeric(d$object),
  group=as.numeric(cog$group)-1,
  nkin=length(kin.vars),
  nact=length(action.vars),
  nobjects=length(objects),
  kinematics=as.matrix(d[,kin.vars]),
  tallforw=as.numeric(with(cog, Tallspenn_forlengs)),
  tallbackw=as.numeric(with(cog, Tallspenn_baklengs)),
  stroop=as.numeric(with(cog, Stroop_WC)),
  trail=as.numeric(with(cog, Trail_M_B))
)

## only run analysis when not yet cached (use --force to delete cache file in cache/var/...)
if(is.cached.var("fit")){
  printf("WARNING: loading variables from cache\n")
  fit=load.cache.var("fit")
} else {
  fit = stan(mod.fname, data = data, cores=n.cores, chains = n.chains, iter = n.iter, warmup = n.warmup)#, init=initfct)
  cache.var('fit')
}

## create diagnostics file
pdf(file=file.path('graphs', sprintf("%s.pdf", bname)), width=20, height=15)
excl.pars=c("log-posterior", "log_lik", "lp__")

# RHat
stan_rhat(fit, pars=setdiff(fit@model_pars, excl.pars))

# traceplots
#stan_trace(fit, pars=setdiff(fit@model_pars, excl.pars), inc_warmup = T)
stan_trace(fit, pars=grep("Sigma", fit@model_pars, value=T), inc_warmup = T)
stan_trace(fit, pars=grep("bsubj", fit@model_pars, value=T), inc_warmup = T)
stan_trace(fit, pars=grep("baction", fit@model_pars, value=T), inc_warmup = T)
stan_trace(fit, pars=grep("btask", fit@model_pars, value=T), inc_warmup = T)

## dotplots
stan_plot(fit, pars=setdiff(fit@model_pars, excl.pars))+geom_vline(x=0,color='red')
stan_plot(fit, pars=grep("Sigma", fit@model_pars, value=T))
stan_plot(fit, pars=grep("bsubj", fit@model_pars, value=T))
stan_plot(fit, pars=grep("baction", fit@model_pars, value=T))
stan_plot(fit, pars=grep("btask", fit@model_pars, value=T))

stan_plot(fit, pars=grep("bobject", fit@model_pars, value=T))

stan_plot(fit, pars=grep("bgroup", fit@model_pars, value=T))+geom_vline(x=0)
stan_plot(fit, pars=grep("bstroop", fit@model_pars, value=T))+geom_vline(x=0)
stan_plot(fit, pars=grep("bgstroop", fit@model_pars, value=T))+geom_vline(x=0)
stan_plot(fit, pars=grep("btallforw", fit@model_pars, value=T))+geom_vline(x=0)
stan_plot(fit, pars=grep("bgtallforw", fit@model_pars, value=T))+geom_vline(x=0)
stan_plot(fit, pars=grep("btallbackw", fit@model_pars, value=T))+geom_vline(x=0)
stan_plot(fit, pars=grep("bgtallbackw", fit@model_pars, value=T))+geom_vline(x=0)
stan_plot(fit, pars=grep("btrail", fit@model_pars, value=T))+geom_vline(x=0)
stan_plot(fit, pars=grep("bgtrail", fit@model_pars, value=T))+geom_vline(x=0)

dev.off()

## create nicer variables for interpretation
pdf(file=file.path('graphs', sprintf("%s_2.pdf", bname)), width=16, height=6)

paper.kin.vars=c("PV", "MNV", "TPV", "NCV", "PD", "MND", "TPD", "NCD")
mv.dotplot.fit.local(fit, 'bgroup', kin.vars)+ylab("regression coefficient")+xlab("Kinematic variable")+
  scale_x_discrete(labels=paper.kin.vars)+theme(strip.text=element_text())

mv.dotplot.fit.local(fit, 'btask_raw', kin.vars)

## action-covariates (are the actions different from one another?)
bact <- (extract(fit,"baction")[[1]])
bact.mean <- apply(bact, c(2,3), mean)
bact.lower <- apply(bact, c(2,3), hdi)[1,,]
bact.upper <- apply(bact, c(2,3), hdi)[2,,]

melt(bact.mean) %>% select(action=Var1,kin.var=Var2, mean=value) %>% 
  mutate(action=plyr::mapvalues(action,from=1:n.actions, to=action.vars)) %>%
  mutate(kin.var=plyr::mapvalues(kin.var, from=1:length(kin.vars), to=kin.vars)) %>%
  mutate(lower=melt(bact.lower)$value, upper=melt(bact.upper)$value) %>%
  ggplot(aes(x=kin.var,y=mean,ymin=lower,ymax=upper))+geom_point()+
  geom_errorbar(width=.1)+facet_wrap(~action,ncol=2)+geom_hline(y=0,colour='red')+theme_bw()

## object-covariates (are the objects different from one another?)
bobj <- (extract(fit,"bobject")[[1]])
bobj.mean <- apply(bobj, c(2,3), mean)
bobj.lower <- apply(bobj, c(2,3), hdi)[1,,]
bobj.upper <- apply(bobj, c(2,3), hdi)[2,,]

melt(bobj.mean) %>% select(object=Var1,kin.var=Var2, mean=value) %>% 
  mutate(object=plyr::mapvalues(object,from=1:n.objects, to=objects)) %>%
  mutate(kin.var=plyr::mapvalues(kin.var, from=1:length(kin.vars), to=kin.vars)) %>%
  mutate(lower=melt(bobj.lower)$value, upper=melt(bobj.upper)$value) %>%
  ggplot(aes(x=kin.var,y=mean,ymin=lower,ymax=upper))+geom_point()+
  geom_errorbar(width=.1)+facet_wrap(~object,ncol=4)+geom_hline(y=0,colour='red')+theme_bw()+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))

## cognitive covariates and group-interaction
mv.dotplot.fit.local(fit, vars=c('btallforw','btallbackw','bstroop', 'btrail',
                                 'bgtallforw','bgtallbackw','bgstroop', 'bgtrail'), xlabels=kin.vars,ncol=4)

#mv.dotplot.fit.local(fit, vars=c('btallforw','btallbackw','bstroop', 'btrail'), xlabels=kin.vars, ncol=1),
mv.dotplot.fit.local(fit, vars=c('bgtallforw','bgtallbackw','bgstroop', 'bgtrail'), xlabels=kin.vars,ncol=1)

## COGNITIVE X GROUP IA
plot.ia <- function(fit, varname){
  cog.young=extract(fit,sprintf("b%s",varname))[[1]]
  cog.old  =cog.young+extract(fit,sprintf("bg%s",varname))[[1]]
  data.frame(
    group=rep(c("young","old"), each=length(kin.vars)),
    var=rep(kin.vars, 2),
    mean=c( apply(cog.young, 2, mean),
            apply(cog.old, 2, mean)
            ),
    lower=c( apply(cog.young, 2, hdi)[1,],
             apply(cog.old, 2, hdi)[1,]),
    upper=c( apply(cog.young, 2, hdi)[2,],
             apply(cog.old, 2, hdi)[2,])
  ) %>% ggplot(aes(x=var,y=mean,group=group,color=group,ymin=lower,ymax=upper))+
    geom_point(position=position_dodge(width = 0.5))+
    geom_errorbar(width=.1,position = position_dodge(width = 0.5))+theme_bw()+geom_hline(y=0)+
    theme(axis.text.x = element_text(angle = 50, hjust = 1))+
    ggtitle(varname)+xlab("")+ylab("regression coefficient")
}

multiplot(
  plot.ia(fit, "tallforw")+theme(legend.position="none"),
  plot.ia(fit, "tallbackw")+theme(legend.position="none")+ylab(""),
  plot.ia(fit, "stroop")+theme(legend.position="none")+ylab(""),
  plot.ia(fit, "trail")+ylab(""),
  cols=4)

## SIGMA
mean.cov=(apply(extract(fit,"Sigma")[[1]], c(2,3), mean))

cov2cor(mean.cov) %>% melt %>% 
  mutate(Var1=plyr::mapvalues(Var1, from=1:length(kin.vars), kin.vars)) %>%
  mutate(Var2=plyr::mapvalues(Var2, from=1:length(kin.vars), kin.vars)) %>% filter(Var1>=Var2) %>% 
  mutate(value=ifelse(value==1, NA, value)) %>%
  ggplot(aes(Var1,Var2,fill=value))+geom_tile()+geom_text(aes(label=sprintf("%.2f",value)))+
  scale_fill_gradientn(colours=jet.colors(7))+theme_bw()
dev.off()



###
### Production-ready plots for paper (requires minor tweaking in inkscape)
###############################################################################


### GROUP AND TASK
bgroup <-(extract(fit,"bgroup")[[1]])
colnames(bgroup) <- paper.kin.vars
btask <- (extract(fit,"btask")[[1]])[,2,]
colnames(btask) <- paper.kin.vars

((bgroup %>% data.frame %>% gather(kin.var, value) %>% group_by(kin.var) %>%
  summarize(m=mean(value), lower=hdi(value)[1], upper=hdi(value)[2]) %>% cbind(effect="Elderly")) %>% rbind(
    (btask %>% data.frame %>% gather(kin.var, value) %>% group_by(kin.var) %>%
       summarize(m=mean(value), lower=hdi(value)[1], upper=hdi(value)[2]) %>% cbind(effect="Assembly Task")) ) )-> d 
d %>%
  ggplot(aes(y=kin.var,x=m,xmin=lower,xmax=upper))+geom_point()+geom_errorbarh(height=0.1)+
  geom_vline(x=0,color='grey')+facet_wrap(~effect, scales="free_x")+
  scale_y_discrete(labels=paper.kin.vars)+xlab("regression coefficient")+ylab("Kinematic variable")+
  theme_bw()+
  theme(strip.text.x = element_text(size=14, angle=0),                 # facet text
        strip.background = element_rect(colour="black", fill="white"), # facet BG
        panel.margin = unit(1, "lines")) # space bw facets

ggsave(filename=file.path("graphs","paper_coef_task_group.pdf"), width=9, height=4)

## ACTIONS
bact <- (extract(fit,"baction")[[1]])
bact.mean <- apply(bact, c(2,3), mean)
bact.lower <- apply(bact, c(2,3), hdi)[1,,]
bact.upper <- apply(bact, c(2,3), hdi)[2,,]

melt(bact.mean) %>% select(action=Var1,kin.var=Var2, m=value) %>% 
  mutate(action=plyr::mapvalues(action,from=1:n.actions, to=tolower(action.vars))) %>%
  mutate(kin.var=plyr::mapvalues(kin.var, from=1:length(kin.vars), to=paper.kin.vars)) %>%
  mutate(lower=melt(bact.lower)$value, upper=melt(bact.upper)$value) %>% 
  filter(action!="reaching") %>%
  ggplot(aes(y=kin.var,x=m,xmin=lower,xmax=upper))+geom_point()+geom_errorbarh(height=0.1)+
    geom_vline(x=0,color='grey')+facet_wrap(~action, scales="free_x")+
    scale_y_discrete(labels=paper.kin.vars)+xlab("regression coefficient")+ylab("Kinematic variable")+
    theme_bw()+
    theme(strip.text.x = element_text(size=14, angle=0),                 # facet text
          strip.background = element_rect(colour="black", fill="white"), # facet BG
          panel.margin = unit(1, "lines")) # space bw facets
ggsave(filename=file.path("graphs","paper_coef_action.pdf"), width=9, height=4)

## OBJECTS
bobj <- (extract(fit,"bobject")[[1]])
bobj.mean <- apply(bobj, c(2,3), mean)
bobj.lower <- apply(bobj, c(2,3), hdi)[1,,]
bobj.upper <- apply(bobj, c(2,3), hdi)[2,,]


melt(bobj.mean) %>% select(object=Var1,kin.var=Var2, m=value) %>% 
  mutate(object=plyr::mapvalues(object,from=1:n.objects, to=Hmisc::capitalize(objects))) %>%
  mutate(kin.var=plyr::mapvalues(kin.var, from=1:length(kin.vars), to=kin.vars)) %>%
  mutate(lower=melt(bobj.lower)$value, upper=melt(bobj.upper)$value) %>%
  filter(object!="Pin") %>%
  ggplot(aes(y=kin.var,x=m,xmin=lower,xmax=upper))+geom_point()+geom_errorbarh(height=0.1)+
  geom_vline(x=0,color='grey')+facet_wrap(~object, scales="free_x")+
  scale_y_discrete(labels=paper.kin.vars)+xlab("regression coefficient")+ylab("Kinematic variable")+
  theme_bw()+
  theme(strip.text.x = element_text(size=14, angle=0),                 # facet text
        strip.background = element_rect(colour="black", fill="white"), # facet BG
        panel.margin = unit(1, "lines")) # space bw facets
ggsave(filename=file.path("graphs","paper_coef_object.pdf"), width=9, height=4)

## COGNITIVE X GROUP IA
paper.cog.vars=c("tallforw"="Digits forward",
                 "tallbackw"="Digits backward",
                 "stroop"="Stroop W/C",
                 "trail"="TMT B")
d <- NULL
for(varname in c("tallforw", "tallbackw", "stroop", "trail")){
  cog.young=extract(fit,sprintf("b%s",varname))[[1]]
  cog.old  =cog.young+extract(fit,sprintf("bg%s",varname))[[1]]
  d<-rbind(d, 
        data.frame(
          cog.var=paper.cog.vars[varname],
          group=rep(c("Young","Elderly"), each=length(kin.vars)),
          kin.var=rep(paper.kin.vars, 2),
          m=c( apply(cog.young, 2, mean),
                  apply(cog.old, 2, mean)
          ),
          lower=c( apply(cog.young, 2, hdi)[1,],
                   apply(cog.old, 2, hdi)[1,]),
          upper=c( apply(cog.young, 2, hdi)[2,],
                   apply(cog.old, 2, hdi)[2,]))
  )
}
d$group = relevel(d$group, ref="Young")
d$kin.var=factor(d$kin.var, levels=paper.kin.vars)

d %>% ggplot(aes(y=m,x=kin.var,group=group,color=group,ymin=lower,ymax=upper))+
  geom_point(position=position_dodge(width = 0.5))+
  geom_errorbar(width=.2,position = position_dodge(width = 0.5))+geom_hline(y=0, color='grey')+
  facet_wrap(~cog.var, scales='fixed', ncol=4) + coord_flip()+
  scale_x_discrete(labels=paper.kin.vars)+ylab("regression coefficient")+xlab("Kinematic variable")+
  theme_bw()+scale_color_grey(start = 0.4, end = .8)+
  theme(strip.text.x = element_text(size=14, angle=0),                 # facet text
        strip.background = element_rect(colour="black", fill="white"), # facet BG
        panel.margin = unit(1, "lines")) # space bw facets

ggsave(filename=file.path("graphs","paper_coef_cog.pdf"), width=12, height=4)

cog.both <- d

## IA only
d <- NULL
for(varname in c("tallforw", "tallbackw", "stroop", "trail")){
  ia=extract(fit,sprintf("bg%s",varname))[[1]]
  d<-rbind(d, 
           data.frame(
             cog.var=paper.cog.vars[varname],
             kin.var=paper.kin.vars,
             m=apply(ia, 2, mean),
             lower=apply(ia, 2, hdi)[1,],
             upper=apply(ia, 2, hdi)[2,])
  )
}
d$kin.var=factor(d$kin.var, levels=paper.kin.vars)
cog.ia <- d  

d %>% ggplot(aes(x=m,y=kin.var,xmin=lower,xmax=upper))+
  geom_point()+geom_errorbarh(height=0.1)+geom_vline(x=0, color='grey')+
  facet_wrap(~cog.var,ncol=4, scales="free_x")+theme_bw()

