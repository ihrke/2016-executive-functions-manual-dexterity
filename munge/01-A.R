
cognitive <- kincog.raw %>% select(-starts_with("ACTION"), -starts_with("fra"), -starts_with("tid"),
                                   -starts_with("til"))
#cognitive

## want the following structure for the kinematics
##
## subj action object pin time  PeakVel MeanAngVel  TimePeakVel NrChangesVel  PeakDisp  MeanAngDisp TimePeakDisp  NrChangesDisp
##
##

n.subj=nrow(kincog.raw)
n.actions=4
n.pins=12
kin.vars=c("PeakVel", "MeanAngVel",  
           "TimePeakVel", "NrChangesVel",
           "PeakDisp", "MeanAngDisp",
           "TimePeakDisp", "NrChangesDisp")
cog.vars<-c("Tallspenn_forlengs", "Tallspenn_baklengs", 
            "Stroop_WC", "Trail_M_B")
action.vars <- c("REACHING", "GRASPING", "TRANSPORTING", "INSERTING")

d<-data.frame(
  subj=rep(1:n.subj, each=n.actions*n.pins),
  action=rep(rep(1:n.actions, each=n.pins), n.subj),
  object="pin",
  pin=rep(rep(1:n.pins, n.actions), n.subj)
)

for(v in kin.vars){
  d[,v]=NA
  for(a in 1:n.actions){
    for(p in 1:n.pins){
      vname=sprintf("ACTION%i_%s_pin%i",a,v,p)
      if(!(vname %in% names(kincog.raw))){
        cat(sprintf("missing %s\n", vname))
      } else {
        d[d$action==a & d$pin==p,v]=kincog.raw[,vname]
      }
    }
  }
}
kinematics <- d
kinematics.task1 <- kinematics %>% ## this is for compatibility with task2
  rename(repetition=pin) %>% mutate(action=factor(action, labels=action.vars))


## movement times
d<-data.frame(
  subj=rep(1:n.subj, each=n.actions*n.pins),
  action=rep(rep(1:n.actions, each=n.pins), n.subj),
  object="pin",
  pin=rep(rep(1:n.pins, n.actions), n.subj)
)

FPS=16.66666
d[,"time"]=NA
for(a in 1:n.actions){
  for(p in 1:n.pins){
    vname=sprintf("ACTION%i_time_pin%i",a,p)
    if(!(vname %in% names(kincog.raw))){
      cat(sprintf("missing %s\n", vname))
    } else {
      d[d$action==a & d$pin==p,'time']=kincog.raw[,vname]*FPS
    }
  }
}

movement.times.task1 <- d %>% ## this is for compatibility with task2
  rename(repetition=pin) %>% mutate(action=factor(action, levels=1:n.actions, labels=action.vars))