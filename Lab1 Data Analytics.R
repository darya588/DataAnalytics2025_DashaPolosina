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
