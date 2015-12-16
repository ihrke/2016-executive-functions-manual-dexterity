## task 2
##
## want the following structure for the kinematics
##
## subj action object pin time  PeakVel MeanAngVel  TimePeakVel NrChangesVel  PeakDisp  MeanAngDisp TimePeakDisp  NrChangesDisp
##
##
##

## in task2, subjects build "bygg"s each consisting of 
##         a pin, a washer1, a collar and a washer2 (object)
## and for each of those, there is each action (REACHING, GRASPING, TRANSPORTING INSERTING)

n.subj=nrow(kincog.task2.raw)
n.repetitions=max(as.numeric(str_match( names(kincog.task2.raw), "([0-9])bygg")[,2]), na.rm=T)
objects=c("pin", "washer1", "collar", "washer2")
n.objects=length(objects)
d<-data.frame(
  subj=rep(1:n.subj, each=n.actions*n.repetitions*n.objects),
  action=rep(rep(action.vars, each=n.repetitions*n.objects), n.subj),
  object=rep(rep(objects, each=n.repetitions), n.actions*n.subj),
  repetition=rep(1:n.repetitions, n.actions*n.objects*n.subj) # antall bygg
)

## ok, so apparently the last bygg is number 8 and only pin and washer1 are recorded
## after reaching for collar, this is cut off
for(v in kin.vars){
  d[,v]=NA
  for(a in 1:n.actions){
    for(o in objects){
      for(r in 1:n.repetitions){
        vname=sprintf("ACTION%i_%s_%s_%ibygg",a,v,o,r)
        if(!(vname %in% names(kincog.task2.raw))){
          cat(sprintf("missing %s\n", vname))
        } else {
          d[d$action==action.vars[a] & d$object==o & d$repetition==r,v]=kincog.task2.raw[,vname]
        }
      }
    }
  }
}

kinematics.task2 <- d

## movement times
d<-data.frame(
  subj=rep(1:n.subj, each=n.actions*n.repetitions*n.objects),
  action=rep(rep(action.vars, each=n.repetitions*n.objects), n.subj),
  object=rep(rep(objects, each=n.repetitions), n.actions*n.subj),
  repetition=rep(1:n.repetitions, n.actions*n.objects*n.subj) # antall bygg
)

FPS=16.66666
d[,"time"]=NA

for(a in 1:n.actions){
  for(o in objects){
    for(r in 1:n.repetitions){
      vname=sprintf("ACTION%i_time_%s_%ibygg",a,o,r)
      if(!(vname %in% names(kincog.task2.raw))){
        cat(sprintf("missing %s\n", vname))
      } else {
        d[d$action==action.vars[a] & d$object==o & d$repetition==r,"time"]=kincog.task2.raw[,vname]*FPS
      }
    }
  }
}


movement.times.task2 <- d
