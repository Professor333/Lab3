#For Step 6-8. Steps 1-5 is already posted in the lab3code on blackboard
setwd("C:/Users/Ryan/Development/R/BIO 614/Lab3/Lab3")

library(pwr)
library(car)
library(ggplot2)

#Data 3 Analysis Step 6
data3=read.csv("lab3data3.csv", header = T)


resids1<-scale(data3[,1])
resids2<-scale(data3[,2])
qqPlot(resids1)
qqPlot(resids2)

shapiro.test(resids1)
boxplot(data3)
var.test(data3$NoC,data3$C,alternative="two.sided")
#Not Normal, equal variance, according to Qi

sample1<-data3$NoC
sample2<-data3$C
nn1<-length(data3$NoC)
nn2<-length(data3$C)
nnn<-length(data3$NoC)
out<-wilcox.test(data3$NoC,data3$C,alternative="two.sided",mu=0,paired = F, var.equal = T)
out

signal=(mean(sample1,na.rm = TRUE)-mean(sample2,na.rm = TRUE))
sp2=sum((sample1-mean(sample1,na.rm = TRUE))^2,(sample2-mean(sample2,na.rm = TRUE))^2,na.rm = TRUE)/(nn1+nn2-2)
noise=sqrt((sp2/nn1)+(sp2/nn2)) #denominator of the t statistic
tStat=signal/noise
sigT=2*pt(tStat,(nn1+nn2-2),lower.tail=FALSE)

#It is strange to calculate pwr on a non-normal dataset according to Qi
#cohen.d=signal/sqrt(sp2)
#pwr.t.test(n=nnn,d=cohen.d,sig.level=0.05,type="two.sample") 
#pwr.t.test(n=NULL,d=cohen.d,sig.level=0.05,power=0.8,type="two.sample") 


#Data 4 analysis Step 6
data4=read.csv("lab3data4.csv", header = T)

resids1<-scale(data4[,1])
resids2<-scale(data4[,2])
qqPlot(resids1)
qqPlot(resids2)

shapiro.test(resids1)
boxplot(data4)
var.test(data4$NoC,data4$C,alternative="two.sided") #Don't need to worry about var given paired design
#Normal, Not equal variance, according to Qi


sample1<-data4$NoC
sample2<-data4$C
nn1<-length(data4$NoC)
nn2<-length(data4$C)
nnn<-length(data4$NoC)
out<-t.test(data4$NoC,data4$C,alternative="two.sided",mu=0,paired = T, var.equal = F)
out

signal=(mean(sample1,na.rm = TRUE)-mean(sample2,na.rm = TRUE))
sp2=sum((sample1-mean(sample1,na.rm = TRUE))^2,(sample2-mean(sample2,na.rm = TRUE))^2,na.rm = TRUE)/(nn1+nn2-2)
noise=sqrt((sp2/nn1)+(sp2/nn2)) #denominator of the t statistic
tStat=signal/noise
sigT=2*pt(tStat,(nn1+nn2-2),lower.tail=FALSE)

cohen.d=signal/sqrt(sp2)
pwr.t.test(n=nnn,d=cohen.d,sig.level=0.05,type="paired") #Power=0.079
pwr.t.test(n=NULL,d=cohen.d,sig.level=0.05,power=0.8,type="paired") #n=313.15

#####Data Set 4 is better, answer to Step 6######

#Step 7
raw<-data.frame(sample1,sample2)
attach(raw)

means<-apply(raw,2,mean)
SE<-function(x) sd(x)/sqrt(length(x))
SEs<-apply(raw,2,SE)
trt<-as.factor(c("NoC", "C"))
summary<-data.frame(trt, means, SEs)
str(summary)
limits<-aes(ymax=means+SEs, ymin=means-SEs)

fig1<-ggplot(summary, aes(x=trt, y=means, fill=trt)) +
  labs(title = "Data Set 4") +
  geom_bar(stat="identity")+#stat="identity" to represent data values and not frequencies
  geom_errorbar(limits, width=0.1)
print(fig1)
#Collect Figure

#Step 8 part 4
pwr.t.test(n=30,d=cohen.d,sig.level=0.05,type="paired")
pwr.t.test(n=NULL,d=cohen.d,sig.level=0.05,power=0.8,type="paired")
#Answer: No
