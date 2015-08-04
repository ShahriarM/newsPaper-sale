rm(list=ls())

library(MASS)
library(reshape2)
library(logspline)
library(zoo)
library(rjags) # make sure you have jags program install, Ubuntu has it in its repository. 
               #http://mcmc-jags.sourceforge.net/
library(ggplot2)
library(dplyr)
library(tidyr)

time.begin <- Sys.time()
directory <- 'C:/jags-store/nbin2' #change it accordingly, could be where this file 
                                 # is (and samples_negbin2.RData in case not running jags)
setwd(directory)



sold.history <- read.delim("C:/jags-store/homework_singlecopy.tsv") # change to where data file is
sold.history <- sold.history %>%
   filter(sold > -1)

sold.history$date <- as.Date.factor(sold.history$date)


# # following to see what day of week it is, can we alreasy track it? Yes
# weekdays.tracking <- 
#   sold.history %>%
#   mutate( weekday = weekdays(date)) %>%
#   ungroup() %>%
#   select(bipad, weekday) %>%
#   distinct() 

# draws have been always greater than sold
check.draws <- 
  sold.history %>%
  filter(!is.na(draws)) %>%
  filter( draws < sold)


sold.history.weekday <- 
  sold.history %>%
  filter(!is.na(draws)) %>%
  mutate(week.day =as.numeric(c("90076"=7, "90016"=1,
                        "90026"=2, "90036"=3,
                        "90046"=4, "90056"=5, 
                        "90066"=6  )[as.character(bipad)]))
          




Starting.date <- min( sold.history$date)


sold.history.weekday.M <-
  sold.history.weekday %>%
  mutate(N.days = as.numeric( date - Starting.date )) %>%
  select( -date,-bipad) %>%
  arrange(N.days)


# draws.history.weekday.M <-
#   sold.history.weekday %>%
#   mutate(N.days = as.numeric( date - Starting.date )) %>%
#   select( -sold,-bipad) %>%
#   spread(account, draws) %>%
#   arrange(date)
 

#preparing data for jags
jags.data <- list()
 
jags.data$sold <- as.numeric( sold.history.weekday.M$sold)
jags.data$week.day <- as.numeric( sold.history.weekday.M$week.day)
jags.data$N.week.day <- length(unique(jags.data$week.day)) # expect 7, number of days in week
jags.data$N.accounts <- length(unique(sold.history.weekday.M$account))
jags.data$account <- as.numeric(sold.history.weekday.M$account)                              
jags.data$N.N.days <- length(unique(sold.history.weekday.M$N.days))
jags.data$N.days <- as.numeric(sold.history.weekday.M$N.days)              
jags.data$draws <- as.numeric( sold.history.weekday.M$draws)
jags.data$L <- dim(sold.history.weekday.M)[1]
JAGS <- FALSE # run jags or load the sample?
if (JAGS){
  set.seed(777)
  
  sh.jm <- jags.model('NYTmodel_negbin.txt',
                         data=jags.data,
                         n.chains=3,
                         n.adapt=4.0E+2) #1.0E+4

 
  
  
  jags.update(sh.jm,1.0E+3) #1.0E+4
   
 samples <- jags.samples(sh.jm,
                             c( 'r','r.a','r.b','m', 'm.a','m.b','sold_pred' ),
                             n.iter=4.0E+4, #1.0E+5
                             thin=1.0E+2)
  
   
 
 
  save(samples,file=paste("samples_negbin2.RData",sep=""))

 message('JAGS Finished')
} else{
  load("samples_negbin2.RData")
  message('Finished Loading')
}
print(Sys.time()-time.begin)
 
samples$p <- samples$r /(samples$r + samples$m)
samples$v <-samples$r * (1 - samples$p) /(samples$p)^2


p.mean<-apply(samples$p,1:2,mean)
p.median<-apply(samples$p,1:2,median)
p.sd<-apply(samples$p,1:2,sd)

r.mean<-apply(samples$r,1:2,mean)
r.median<-apply(samples$r,1:2,median)
r.sd<-apply(samples$r,1:2,sd)

m.mean<-apply(samples$m,1:2,mean)
m.median<-apply(samples$m,1:2,median)
m.sd<-apply(samples$m,1:2,sd)




sold_pred.mean<-apply(samples$sold_pred,1,mean)
sold_pred.median<-apply(samples$sold_pred,1,median)
sold_pred.sd<-apply(samples$sold_pred,1,sd)


sold_pred.ci<-array(NA,c(dim(samples$sold_pred)[c(1)],2))
for(i in 1:(dim(samples$sold_pred)[1]))
{
  sold_pred.ci[i,] <- quantile(as.vector(samples$sold_pred[i,,]),probs=c(.1,.9),na.rm=TRUE)
}
####
for(j in 1:dim(samples$r)[1])
  for(i in 1:dim(samples$r)[2]){ # warning  a lot of pictures, make sure about mixing
  
    png(paste("plots/diagnosis/r.",j,"-",i,".trace.png",sep=""), 
        width = 6.4, height = 12, units = "in", res = 600 )
    par(mfrow=c(2,1))
    matplot(samples$r[j,i,,],type='l',xlab="Iteration") 
    plot(density(samples$r[j,i,,]))
    dev.off()
  
}



for(j in 1:dim(samples$m)[1]) # warning  a lot of pictures, to make sure about mixing
  for(i in 1:dim(samples$m)[2]){
    
    png(paste("plots/diagnosis/m.",j,"-",i,".trace.png",sep=""), 
        width = 6.4, height = 12, units = "in", res = 600 )
    par(mfrow=c(2,1))
    matplot(samples$m[j,i,,],type='l',xlab="Iteration") 
    plot(density(samples$m[j,i,,]))
    dev.off()
    
  }


draw.suggestion <- cbind( sold.history.weekday.M , loww = sold_pred.ci[,1], upps= sold_pred.ci[,2])

see.0 <- sum(draw.suggestion$draws) - sum(draw.suggestion$upps) # 25813.6

see.1 <- filter(draw.suggestion , sold > upps) # dim(see.1)[1] is 1888
see.2 <- filter(draw.suggestion , draws > upps) # dim(see.2)[1] is 17441
see.3 <- filter(draw.suggestion , draws > upps & sold > upps) # # dim(see.3)[1] is 1888
see.4 <- filter(draw.suggestion , draws < upps & sold <= draws)# dim(see.4)[1] is 7662
see.5 <- filter(draw.suggestion , draws > upps & sold <= upps)# dim(see.5)[1] is 15553
see.6 <- filter(draw.suggestion , draws < upps & sold == draws)# dim(see.6)[1] is 2377


time.end <- Sys.time()
time_took <- time.end -time.start




