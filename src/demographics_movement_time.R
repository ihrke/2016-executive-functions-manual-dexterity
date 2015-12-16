# 
# 
#
#
library(ProjectTemplate)
load.project()

library(BayesFactor)

## table of means for all cognitive variables
cognitive %>% group_by(gruppe) %>% summarise_each(funs(mean)) %>% t

# The younger group had more years of education 
ttestBF(formula=Studieår ~ gruppe, data=cognitive)

# and scored higher on the MMSE than the older group.
ttestBF(formula=MMSE ~ gruppe, data=cognitive)

# Also, the elderly showed higher right-hand tendency in the Handedness Inventory than the younger group.
ttestBF(formula=Handedness ~ gruppe, data=cognitive)

## male/female different in groups?
gender <- data.frame(young=c(female=9, male=6),
           old=c(10,5)) 

contingencyTableBF(as.matrix(gender), sampleType = "indepMulti", fixedMargin = "cols")

## age difference (trivial)
ttestBF(formula=Alder ~ gruppe, data=cognitive)

## BDI different?
ttestBF(formula=BDI ~ gruppe, data=cognitive)

## cognitive table (creates a nice table for copy-n-pasting into word)
mean.na<-function(x) mean(x,na.rm=T)
sd.na<-function(x) sd(x,na.rm=T)

cognitive %>% group_by(gruppe) %>% summarise_each(funs(mean.na)) %>% t -> tab.m
cognitive %>% group_by(gruppe) %>% summarise_each(funs(sd.na)) %>% t -> tab.sd
tab.m <- tab.m[12:20,]
tab.sd <- tab.sd[12:20,]
vars=rownames(tab.m)
BFs=rep(0, length(vars))
names(BFs) <- vars
cohensd=rep(0, length(vars))
names(cohensd) <- vars

for(var in vars){
  x=cognitive[,var][cognitive$gruppe==1] %>% na.omit
  y=cognitive[,var][cognitive$gruppe==2] %>% na.omit
  BFs[var] <- extractBF(ttestBF(x,y))$bf
  cohensd[var] <- (mean(y)-mean(x))/((sd(x)+sd(y))/2)
}

tab <- data.frame(Variable=c("Digits forward", "Digits backward", "Stroop Word", "Stroop Color",
                             "Stroop W/C", "TMT A", "TMT B", "Right hand", "Left hand"),
                  Elderly=  sprintf("%.2f (%.2f)", tab.m[,2], tab.sd[,2]),
                  Young  =sprintf("%.2f (%.2f)", tab.m[,1], tab.sd[,1]),
                  BF = sprintf("%.2f", BFs),
                  Cohensd = sprintf("%.2f", cohensd)
                  )

htmlTable::htmlTable(format(tab),ctable=T,rnames=F)

## Overall Dexterity Performance
# As expected, younger adults inserted more pins than the older 
# on the inserting pins task and they likewise 
# completed more assemblies than the older group 
kinematics.task1 %>% filter(action=="REACHING") %>% group_by(subj) %>% 
  summarize(nrep=sum(!is.na(PeakVel))) %>% cbind(group=cognitive$gruppe) -> d
d %>% group_by(group) %>% summarize(mean(nrep), sd(nrep))
bf <- ttestBF(formula=nrep ~ group, data=d, nullInterval=c(0,Inf))
bf[1]/bf[2] # one-sided BF

kinematics.task2 %>% filter(action=="REACHING", object=="washer2") %>% group_by(subj) %>% 
  summarize(nrep=sum(!is.na(PeakVel))) %>% cbind(group=cognitive$gruppe) -> d
d %>% group_by(group) %>% summarize(mean(nrep), sd(nrep))
bf <- ttestBF(formula=nrep ~ group, data=d, nullInterval=c(0,Inf))
bf[1]/bf[2] # one-sided BF

## MOVEMENT TIMES TASK 1
## ------------------------------------
movement.times.task1 %>% group_by(subj, action) %>% summarise(mrt=mean(time, na.rm=T)) %>% 
  mutate(action=ordered(tolower(as.character(action)), levels=tolower(action.vars))) %>%
  mutate(group=(ifelse(subj<=10, "young","old"))) %>%
  mutate(group=factor(group, levels=c("young", "old"))) %>% group_by(action, group) %>%
  summarise(mean.time=mean(mrt), sem.time=sem(mrt)) %>% 
  ggplot(aes(x=action, y=mean.time, group=group, ymin=mean.time-sem.time, ymax=mean.time+sem.time, fill=group))+
    geom_errorbar(position=position_dodge(width=0.9),width=0.1)+
    geom_bar(stat="identity",position="dodge", color='black')+
    theme_bw()+ylab("Mean Movement Time")+xlab("Movement Type")+
    scale_fill_grey(start = 0.4, end = .8)+
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          legend.title=element_blank())

ggsave(filename=file.path('graphs', 'paper_time1.pdf'), width=7, height=5)

movement.times.task1 %>% group_by(subj, action) %>% summarise(mrt=mean(time, na.rm=T)) %>% 
  mutate(group=(ifelse(subj<=10, "young","old"))) %>%
  mutate(group=factor(group, levels=c("young", "old"))) %>% ungroup %>%
  mutate(subj=factor(subj)) %>% data.frame-> d2

write.table(d2, file=file.path("cache", "d2.csv"), row.names = F) ## for loading in JASP


## Bayesian ANOVA
# (redone in JASP -> src/movement_times_task1.jasp)
bf<-anovaBF(formula=mrt ~ action*group+subj, whichRandom="subj", data=d2, whichModels="all")#, posterior = TRUE, iterations=1000)
bf
plot(bf)
bfm<-max(bf)
samples=(posterior(bfm, iterations=10000))
rm(var)
summary(samples)

## pairwise contrasts
s<-data.frame(as.matrix(samples))
names(s)
## reaching
s2<-(with(s, (mu+group.old+action.REACHING+action.group.REACHING...old)-(mu+group.young+action.REACHING+action.group.REACHING...young)))
mean(s2); as.vector(hdi(s2))
## grasping
s2<-(with(s, (mu+group.old+action.GRASPING+action.group.GRASPING...old)-(mu+group.young+action.GRASPING+action.group.GRASPING...young)))
mean(s2); as.vector(hdi(s2))
## transporting
s2<-(with(s, (mu+group.old+action.TRANSPORTING+action.group.TRANSPORTING...old)-(mu+group.young+action.TRANSPORTING+action.group.TRANSPORTING...young)))
mean(s2); as.vector(hdi(s2))
## inserting
s2<-(with(s, (mu+group.old+action.INSERTING+action.group.INSERTING...old)-(mu+group.young+action.INSERTING+action.group.INSERTING...young)))
mean(s2); as.vector(hdi(s2))

## MOVEMENT TIMES TASK 2
## ------------------------------------
movement.times.task2 %>% 
  mutate(object=ifelse(str_detect(as.character(object), "washer"  ), "washer", as.character(object))) %>%
  mutate(object=ordered(object, c("pin", "washer", "collar"))) %>%  
  mutate(action=ordered(action, levels=action.vars)) %>% 
  group_by(subj, object, action) %>% summarise(mrt=mean(time, na.rm=T)) %>% 
  mutate(group=(ifelse(subj<=10, "young","old"))) %>%
  mutate(group=factor(group, levels=c("young", "old"))) %>% data.frame -> d3

d3 %>% group_by(object, action, group) %>%
  summarise(mean.time=mean(mrt, na.rm=T), sem.time=sem(mrt, na.rm=T)) %>% 
  mutate(obj.action=sprintf("%s.%s",object,action)) ->d4
d4$obj.action=tolower(as.character(d4$obj.action))
d4$obj.action=ordered(d4$obj.action, levels=unique(d4$obj.action))

d4 %>%
  ggplot(aes(x=obj.action, y=mean.time, group=interaction(group,object), 
             ymin=mean.time-sem.time, ymax=mean.time+sem.time, fill=group))+
  geom_errorbar(position=position_dodge(width=0.9),width=0.1)+
  geom_bar(stat="identity",position="dodge",color='black')+
  theme_bw()+ylab("Mean Movement Time")+xlab("Movement Type")+
  scale_fill_grey(start = 0.4, end = .8)+
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.title=element_blank())

ggsave(filename=file.path('graphs', 'paper_time2.pdf'), width=14, height=5)

write.table(na.omit(d3), file=file.path("cache", "d3.csv"), row.names = F) # for usage in JASP


## Bayesian ANOVA
# (redone in JASP -> src/movement_times_task2.jasp)
bf<-anovaBF(formula=mrt ~ object*action*group+subj, whichRandom="subj", data=na.omit(d3) %>% mutate(subj=as.factor(subj)), whichModels="all")#, posterior = TRUE, iterations=1000)
bfm <- bf[127]
samples=(posterior(bfm, iterations=10000))
rm(var)
summary(samples)

## pairwise contrasts
s<-data.frame(as.matrix(samples))
names(s)

#####################################################################################################
## Pin  - REACHING
s2<-(with(s, 
          (mu+group.old+action.REACHING+object.pin+
             object.action.pin...REACHING+action.group.REACHING...old+object.group.pin...old+
             object.action.group.pin...REACHING...old)-
          (mu+group.young+action.REACHING+object.pin+
              object.action.pin...REACHING+action.group.REACHING...young+object.group.pin...young+
              object.action.group.pin...REACHING...young)))
sprintf("(difference=%.0f ms, HDI=[%.0f, %.0f])", mean(s2), hdi(s2)[1], hdi(s2)[2])

## Pin  - GRASPING
s2<-(with(s, 
          (mu+group.old+action.GRASPING+object.pin+
             object.action.pin...GRASPING+action.group.GRASPING...old+object.group.pin...old+
             object.action.group.pin...GRASPING...old)-
            (mu+group.young+action.GRASPING+object.pin+
               object.action.pin...GRASPING+action.group.GRASPING...young+object.group.pin...young+
               object.action.group.pin...GRASPING...young)))
sprintf("(difference=%.0f ms, HDI=[%.0f, %.0f])", mean(s2), hdi(s2)[1], hdi(s2)[2])

## Pin  - TRANSPORTING
s2<-(with(s, 
          (mu+group.old+action.TRANSPORTING+object.pin+
             object.action.pin...TRANSPORTING+action.group.TRANSPORTING...old+object.group.pin...old+
             object.action.group.pin...TRANSPORTING...old)-
            (mu+group.young+action.TRANSPORTING+object.pin+
               object.action.pin...TRANSPORTING+action.group.TRANSPORTING...young+object.group.pin...young+
               object.action.group.pin...TRANSPORTING...young)))
sprintf("(difference=%.0f ms, HDI=[%.0f, %.0f])", mean(s2), hdi(s2)[1], hdi(s2)[2])

## Pin  - INSERTING
s2<-(with(s, 
          (mu+group.old+action.INSERTING+object.pin+
             object.action.pin...INSERTING+action.group.INSERTING...old+object.group.pin...old+
             object.action.group.pin...INSERTING...old)-
            (mu+group.young+action.INSERTING+object.pin+
               object.action.pin...INSERTING+action.group.INSERTING...young+object.group.pin...young+
               object.action.group.pin...INSERTING...young)))
sprintf("(difference=%.0f ms, HDI=[%.0f, %.0f])", mean(s2), hdi(s2)[1], hdi(s2)[2])

#####################################################################################################
## collar  - REACHING
s2<-(with(s, 
          (mu+group.old+action.REACHING+object.collar+
             object.action.collar...REACHING+action.group.REACHING...old+object.group.collar...old+
             object.action.group.collar...REACHING...old)-
            (mu+group.young+action.REACHING+object.collar+
               object.action.collar...REACHING+action.group.REACHING...young+object.group.collar...young+
               object.action.group.collar...REACHING...young)))
sprintf("(difference=%.0f ms, HDI=[%.0f, %.0f])", mean(s2), hdi(s2)[1], hdi(s2)[2])

## collar  - GRASPING
s2<-(with(s, 
          (mu+group.old+action.GRASPING+object.collar+
             object.action.collar...GRASPING+action.group.GRASPING...old+object.group.collar...old+
             object.action.group.collar...GRASPING...old)-
            (mu+group.young+action.GRASPING+object.collar+
               object.action.collar...GRASPING+action.group.GRASPING...young+object.group.collar...young+
               object.action.group.collar...GRASPING...young)))
sprintf("(difference=%.0f ms, HDI=[%.0f, %.0f])", mean(s2), hdi(s2)[1], hdi(s2)[2])

## collar  - TRANSPORTING
s2<-(with(s, 
          (mu+group.old+action.TRANSPORTING+object.collar+
             object.action.collar...TRANSPORTING+action.group.TRANSPORTING...old+object.group.collar...old+
             object.action.group.collar...TRANSPORTING...old)-
            (mu+group.young+action.TRANSPORTING+object.collar+
               object.action.collar...TRANSPORTING+action.group.TRANSPORTING...young+object.group.collar...young+
               object.action.group.collar...TRANSPORTING...young)))
sprintf("(difference=%.0f ms, HDI=[%.0f, %.0f])", mean(s2), hdi(s2)[1], hdi(s2)[2])

## collar  - INSERTING
s2<-(with(s, 
          (mu+group.old+action.INSERTING+object.collar+
             object.action.collar...INSERTING+action.group.INSERTING...old+object.group.collar...old+
             object.action.group.collar...INSERTING...old)-
            (mu+group.young+action.INSERTING+object.collar+
               object.action.collar...INSERTING+action.group.INSERTING...young+object.group.collar...young+
               object.action.group.collar...INSERTING...young)))
sprintf("(difference=%.0f ms, HDI=[%.0f, %.0f])", mean(s2), hdi(s2)[1], hdi(s2)[2])

#####################################################################################################
## washer  - REACHING
s2<-(with(s, 
          (mu+group.old+action.REACHING+object.washer+
             object.action.washer...REACHING+action.group.REACHING...old+object.group.washer...old+
             object.action.group.washer...REACHING...old)-
            (mu+group.young+action.REACHING+object.washer+
               object.action.washer...REACHING+action.group.REACHING...young+object.group.washer...young+
               object.action.group.washer...REACHING...young)))
sprintf("(difference=%.0f ms, HDI=[%.0f, %.0f])", mean(s2), hdi(s2)[1], hdi(s2)[2])

## washer  - GRASPING
s2<-(with(s, 
          (mu+group.old+action.GRASPING+object.washer+
             object.action.washer...GRASPING+action.group.GRASPING...old+object.group.washer...old+
             object.action.group.washer...GRASPING...old)-
            (mu+group.young+action.GRASPING+object.washer+
               object.action.washer...GRASPING+action.group.GRASPING...young+object.group.washer...young+
               object.action.group.washer...GRASPING...young)))
sprintf("(difference=%.0f ms, HDI=[%.0f, %.0f])", mean(s2), hdi(s2)[1], hdi(s2)[2])

## washer  - TRANSPORTING
s2<-(with(s, 
          (mu+group.old+action.TRANSPORTING+object.washer+
             object.action.washer...TRANSPORTING+action.group.TRANSPORTING...old+object.group.washer...old+
             object.action.group.washer...TRANSPORTING...old)-
            (mu+group.young+action.TRANSPORTING+object.washer+
               object.action.washer...TRANSPORTING+action.group.TRANSPORTING...young+object.group.washer...young+
               object.action.group.washer...TRANSPORTING...young)))
sprintf("(difference=%.0f ms, HDI=[%.0f, %.0f])", mean(s2), hdi(s2)[1], hdi(s2)[2])

## washer  - INSERTING
s2<-(with(s, 
          (mu+group.old+action.INSERTING+object.washer+
             object.action.washer...INSERTING+action.group.INSERTING...old+object.group.washer...old+
             object.action.group.washer...INSERTING...old)-
            (mu+group.young+action.INSERTING+object.washer+
               object.action.washer...INSERTING+action.group.INSERTING...young+object.group.washer...young+
               object.action.group.washer...INSERTING...young)))
sprintf("(difference=%.0f ms, HDI=[%.0f, %.0f])", mean(s2), hdi(s2)[1], hdi(s2)[2])
