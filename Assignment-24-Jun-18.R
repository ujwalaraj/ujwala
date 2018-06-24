data(uj)
names(uj)
attach(uj)

#BOXPLOT
boxplot(work_yrs,main="Work Years",horizontal=TRUE)

#HISTOGRAM
hist(work_yrs)
hist(work_yrs,probability=TRUE) # proportions (or probabilities)
hist(work_yrs,breaks=10) # 10 breaks, or just hist(x,10)
hist(work_yrs,breaks=c(0,1,2,3,4,5,10,20,max(work_yrs))) # specify break points

#FREQUENCY CURVE
tmp = hist(work_yrs) # store the results
lines(c(min(tmp$breaks),tmp$mids,max(tmp$breaks)),c(0,tmp$counts,0),type="l")


# OGIVE 
duration=uj$work_yrs
breaks=seq(0,25,by=2)
duration.cut=cut(duration,breaks,right=TRUE)
duration.freq=table(duration.cut)
cumfreq0=c(0,cumsum(duration.freq))
cumfreq0
plot(breaks,cumfreq0,main = "OGIVE of Work Years",xlab = "work Years",ylab = "Frequency")
lines(breaks,cumfreq0)







X=mean(gmat_tpc)
Y=median(gmat_tpc)
q=sorted_gmat_tpc=sort(gmat_tpc)
#MOde
smode <-function(gmat_tpc){
  xtab<-table(gmat_tpc)
  modes<-xtab[max(xtab)==xtab]
  mag<-as.numeric(modes[1]) #in case mult. modes, this is safer
  themodes<-names(modes)
  mout<-list(themodes=themodes,modeval=mag)
  return(mout)
}

result=smode(gmat_tpc)
print(result)




summary(gmat_tpc)
var(gmat_tpc)
std=sqrt(var(gmat_tpc))


sqrt(var(gmat_tpc))
quantile(gmat_tpc,c(.25,.75))
IQR(gmat_tpc)





