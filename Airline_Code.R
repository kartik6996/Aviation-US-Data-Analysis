bm <- read.csv(file="2008_subset.csv",header = TRUE,stringsAsFactors = FALSE)
library(dplyr)


install.packages("devtools")
library("devtools")
install_github("ndphillips/yarrr")
library("yarrr")

#Pirateplot for Arrival Delay for each Carrier

pirateplot(bm$ArrDelay~bm$UniqueCarrier, data = bm, main = "Arrival Delay ", 
           xlab = "Carrier",theme=0, pal = "pony",ylab = "Arrival Delay in minutes",
           bean.f.o = .6, # Bean fill
           point.o = .3, # Points
           inf.f.o = .7, # Inference fill
           inf.b.o = .8, # Inference border
           avg.line.o = 1, # Average line
           bar.f.o = .5, # Bar
           inf.f.col = "white", # Inf fill col
           inf.b.col = "black", # Inf border col
           avg.line.col = "black", # avg line col
           bar.f.col = gray(.8), # bar filling color
           point.pch = 21,
           point.bg = "white",
           point.col = "black",
           point.cex = .7)

##Do departure time, distance and departure delay predict arrival delay?


##remove outlier and na

arr_delay.model <- (lm(ArrDelay ~ DepTime + Distance + DepDelay, data = bm))
summary(arr_delay.model)


##There is a significant negative correlation for departure time and distance with 
##arrival delay, p < 0.01. There is a significant positive correlation between departure 
#delay and arrival delay, p < 0.01.


###If so, construct a function called "what.are.my.chances" that provides customers the service 
##of calculating their expected arrival delay according to their departure time, distance and 
##departure delay.


what.are.my.chances <- function(dep, dist, dep_delay){
  new.values <- data.frame("DepTime" = dep, "Distance" = dist, "DepDelay" = dep_delay)
  
  output <- predict(arr_delay.model,new.values)
  return(output)
}


# Testing the function
what.are.my.chances(dep = 1400, dist = 2000, dep_delay= 4)





##Best time to travel

# Number of Cancellations per month

library(dplyr)
library(treemap)
library(MASS)
inst_pkgs = load_pkgs =  c("ggplot2","ggplot2movies", "dplyr","babynames","data.table","Rcpp")
git_pkgs = git_pkgs_load = c("streamgraph","DT")
load_pkgs = c(load_pkgs, git_pkgs_load)
pkgs_loaded = lapply(load_pkgs, require, character.only=T)


data1 <- read.csv(file="2008.csv",header=TRUE,stringsAsFactors = FALSE)


##https://www.surveysystem.com/sscalc.htm for sample size calculator
##Confidence Level : 95%
##Confidence Interval :0.5
##Sampling for 38207 observations

set.seed(1)
data1 <- data1[sample(1:7009728, 38207, replace=FALSE),]
write.csv(data1,file = "2008_subset.csv")

air = read.csv('2008_subset.csv',header = TRUE,stringsAsFactors = FALSE)
save(air, file = 'air.RDdata')
load('air.RDdata')

#data <- read.csv("airlines.csv", header = TRUE)
data <- air

cancelled <- data[,"Cancelled"] == 1
cancel <- data[cancelled,]

cancel %>% 
  group_by(Month) %>% 
  tally() -> month

rcolnames(month)<- c("Month","Cancellations")
aa <- month.abb(month$Month)
month.abb
month$MonthAbb <- month.abb
treemap(month,
        index=c("MonthAbb"),
        vSize="Cancellations",
        vColor = "Cancellations",
        type="value")


treemap(air,
        index=c("Month"),
        vSize="DepDelay",
        vColor="DepDelay",
        type="value")#actual value or percentage value

##Reliable Carrier


##Cancel Percentage for each carrier
UniqueCarrier.freq <- sort(table(bm$UniqueCarrier))
UniqueCarrier.freq <- as.data.frame(UniqueCarrier.freq)
View(UniqueCarrier.freq)
colnames(UniqueCarrier.freq)<- c("UniqueCarrier", "FlightCount")

cancel %>% 
  group_by(UniqueCarrier) %>% 
  tally() -> carrier


carrier <- merge(carrier, UniqueCarrier.freq, by="UniqueCarrier")
View(carrier)

carrier$Cancel_Per <- (carrier$n/carrier$FlightCount)*100

treemap(carrier,
        index=c("UniqueCarrier"),
        vSize="Cancel_Per",
        vColor = "Cancel_Per",
        type="value")



##Arrival delay count for each carrier

air$Delay = ifelse(air$ArrDelay > 0, 1, 0)


treemap(air,
        index=c("UniqueCarrier"),
        vSize="Delay",
        vColor="Delay",
        type="value")


count <- table(air$UniqueCarrier) #flight count for each carrier
air_test <- air[which(air$Delay == 1),"UniqueCarrier"]
delaycount <- table(air[which(air$Delay == 1),"UniqueCarrier"]) # delay count for each carrier
delayprop <- delaycount/count
delayprop <- as.data.frame(delayprop)
names(delayprop)[1] <- 'UniqueCarrier'
sortedelayprop <- delayprop[order(delayprop$Freq),]
sortedelayprop

summary(sortedelayprop$Freq)


##Treemap for delay rates
treemap(delayprop,
        index=c("UniqueCarrier"),
        vSize="Freq",
        vColor="Freq",
        type="value")#actual value or percentage value




#average delay time for each carrier
DTsum <- aggregate(ArrDelay ~ UniqueCarrier, air[which(air$ArrDelay > 0),], sum)
count <- as.data.frame(count)
colnames(count)<- c("UniqueCarrier","Count")
DTsum <- merge(DTsum,count,by="UniqueCarrier")
DTsum$Average_Delay <- DTsum$ArrDelay/DTsum$Count
sortedDTsum <- DTsum[order(DTsum$ArrDelay),]

sortedDTsum
summary(sortedDTsum$ArrDelay)

treemap(DTsum,
        index=c("UniqueCarrier"),
        vSize="Average_Delay",
        vColor="Average_Delay",
        type="value")#actual value or percentage value



##Choice Of Airports

#Cancellations at Airport

cancel %>% 
  group_by(Origin) %>% 
  tally() -> Origin

treemap(Origin,
        index=c("Origin"),
        vSize="n",
        vColor = "n",
        type="value")



##Frequency/Count of Delay
treemap(air,
        index=c("Origin"),
        vSize="Delay",
        vColor="Delay",
        type="value")#actual value or percentage value
#if we are using propotion
count <- table(air$Origin) #flight count for each origin airport
delaycount <- (air[which(air$Delay == 1),]) # delay count for each origin airport

sum(count)

delaycount <- table(delaycount$Origin)

View(delaycount)
View(count)

count <- as.data.frame(count)
delaycount <- as.data.frame(delaycount)

delayprop <- merge(delaycount,count,by="Var1")
colnames(delayprop) <- c("Origin","Number_of_Delays","Number_of_Flights")

delayprop$delay_percentage <- delayprop$Number_of_Delays/delayprop$Number_of_Flights


sortedelayprop <- delayprop[order(delayprop$delay_percentage),]
sortedelayprop
summary(sortedelayprop$delay_percentage)



#Percentage of delay with total number of flights
treemap(delayprop,
        index=c("Origin"),
        vSize="delay_percentage",
        vColor="delay_percentage",
        type="value")#actual value or percentage value

#total delay time for each origin airport
DTsum <- aggregate(DepDelay ~ Origin, air[which(air$DepDelay > 0),], sum)
count <- table(air$Origin)
count <- as.data.frame(count)
colnames(count)<- c("Origin","count")

DTsum <- merge(DTsum,count,by="Origin")

View(count)

DTsum$Average_Delay <- DTsum$DepDelay/DTsum$count

DTsum <- subset(DTsum,DTsum$Average_Delay<50)




treemap(DTsum,
        index=c("Origin"),
        vSize="Average_Delay",
        vColor="Average_Delay",
        type="value")#actual value or percentage value





###Perform an analysis of variance (ANOVA) for the different mean values observed 
##for the delays in arrival time, given the interaction of 'origin' and 'dest'.




##null hypothesis that is being tested states that the origin and the destination locations 
##of a given flight route do not have a significant effect on the delay in arrival 
##time that is observed, implying that the differences in mean values of the delays 
##in arrival time for each of the origin and destination airports were solely the 
##result of randomization in this experiment. In other words, if we reject the null 
##hypothesis, we would infer that the differences in mean values of the delays in 
##arrival time for each of the corresponding origin and destination airports in this
##dataset is caused by something other than randomization, leading us to believe that 
##the variation that is observed in the mean values of the delays in arrival time 
##can be explained by the variation existent in the differentorigin and destination 
##airport locations being considered in this analysis. Alternately, if we fail to 
##reject the null hypothesis, we would infer that the variation that is observed in 
##the mean values of the delays in arrival time cannot be explained by the variation 
##existent in the different origin and destination airports being considered and, 
##as such, is likely caused by randomization.

model_annova <- aov(ArrDelay ~ Origin+Dest, data = bm)
anova(model_annova)



model_origin <- aov(ArrDelay~Origin,bm)
anova(model_origin)


model_dest <- aov(ArrDelay~Dest,bm)
anova(model_dest)


##For the analysis of variance (ANOVA) that is performed where the interation of 
##both 'origin' and 'dest' is analyzed against the response variable 'Arrival Delay', 
##a p-value < 0.05 is returned, indicating that  the interaction of these two factors 
##does have a significant effect on the response variable. 
##Therefore, based on this result, we would reject the null hypothesis, 
##leading us to believe that the variation that is observed in the mean values of 
##the delays in arrival time can be explained by the variation existent in the 
##interation of the different origin and destination airport locations being 
##considered in this analysis and, as such, is likely not caused solely by 
##randomization.





##Anova for analyzing whether the delay is related to a carrier

model_carrier <- aov(ArrDelay~UniqueCarrier,bm)
anova(model_carrier)

#less p-value indicates that carrier choice is relevant to delay and its not wholly dependent on
##randomization





