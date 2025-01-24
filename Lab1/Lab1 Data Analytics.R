EPI_data <- read.csv("C:/Users/dasha/Downloads/epi2024results06022024.csv") 
attach(EPI_data) # sets the ‘default’ object 
EPI.new 

NAs <- is.na(EPI.new) # records True values if the value is NA 
EPI.new.noNAs <- EPI.new[!NAs] # filters out NA values, new array 

summary(EPI.new) # stats 

'> fivenum(EPI.new,na.rm=TRUE) [1] 32.1 48.6 59.2 67.6 93.5 
> stem(EPI.new) # stem and leaf plot 
> hist(EPI.new) 
> hist(EPI.new, seq(20., 80., 1.0), prob=TRUE) 
> lines(density(EPI.new,na.rm=TRUE,bw=1.)) # or try bw=“SJ” 
> rug(EPI.new)'

fivenum(EPI.new,na.rm=TRUE)
stem(EPI.new)  
hist(EPI.new) 
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE) 
lines(density(EPI.new,na.rm=TRUE,bw=1.))  
rug(EPI.new)

boxplot(EPI.new, APO.new)
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines(density(EPI.new,na.rm=TRUE,bw=1.))
rug(EPI.new) 

hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines(density(EPI.new,na.rm=TRUE,bw="SJ")) 
rug(EPI.new) 

x<-seq(20,80,1) 
q<- dnorm(x,mean=42, sd=5,log=FALSE) 
lines(x,q)
lines(x,.4*q) 
q <-dnorm(x,mean=65, sd=5,log=FALSE) 
lines(x,.12*q) 

plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE) 

qqnorm(EPI.new)
qqline(EPI.new) 
qqplot(rnorm(250), EPI.new, xlab = "Q-Q plot for norm dsn") 
qqline(EPI.new)
qqplot(rt(250, df = 5), EPI.new, xlab = "Q-Q plot for t dsn") 
qqline(EPI.new)

#EPI Old
NAs <- is.na(EPI.old)
EPI.old.noNAs <- EPI.old[!NAs]

summary(EPI.old) 

plot(ecdf(EPI.old), do.points=FALSE, verticals=TRUE) 

qqnorm(EPI.old)
qqline(EPI.old) 
qqplot(rnorm(250), EPI.old, xlab = "Q-Q plot for norm dsn") 
qqline(EPI.old)
qqplot(rt(250, df = 5), EPI.new, xlab = "Q-Q plot for t dsn") 
qqline(EPI.old)

hist(EPI.old, seq(20., 80., 1.0), prob=TRUE)
lines(density(EPI.old,na.rm=TRUE,bw="SJ")) 
rug(EPI.old) 
