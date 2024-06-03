#Data Visualization
#Line plot

library(ggplot2)
f<-read.csv("UNData.csv")
#Line plot
library(ggplot2)
pl1<-ggplot(data = f[f$Country.or.Area=="INDIA",], aes(x = Year, y = Value, group = Description, color = Description)) +
  geom_line() +
  ggtitle("Line Plot") +
  xlab("Year") +
  ylab("Value")
print(pl1)


#bar plot for index when year=2009
datax=subset(f,(Year==2009))
pl2<-ggplot(datax,aes(Country.or.Area,Value))+
  ggtitle("Bar Plot")+ geom_bar(stat="identity", fill="Orange")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
print(pl2)


#Data Preprocessing
f<-f[,-5] #Magnitude column removed
f$OID=as.factor(f$OID)
f$Country.or.Area=as.factor(f$Country.or.Area)
f$Description=as.factor(f$Description)


#Hypothesis
cat("\nHypothesis: Mean of Index for India from 1980 to 2005 is greater than total mean value\n")
s1=sum(f$Value)
m1 = s1/7905
s2=sum(f[f$Year>=1980 & f$Year<=2005 & f$Country.or.Area=='INDIA',]$Value)
m2 = s2/25
cat("Mean for latest decade is : ",m2)
sd1 = sd(f[f$Year>=1980 & f$Year<=2005 & f$Country.or.Area=='INDIA',]$Value)
cat("\nStandarad Deviation is : ",sd1)
n1 <- sqrt(25)
se1 <- sd1/n1
cat("\nStandard error is : ",se1)
pnd1 <- pnorm(m2,m1,se1)
cat("\nP value is :",pnd1)
if(pnd1<0.05){
  cat("\nClaim can be rejected for 0.05")
}else{
  cat("\nClaim cannot be rejected for 0.05\n")
  
}
#Anguilla value=366.279
#lowest values=0 China

cat("\nHypothesis 2: Mean of index for decade 2000-2009 for all countries is greater than total mean value\n")
sum1=sum(f$Value)
mean1 = sum1/7905
sum2=sum(f[f$Year>=2000 & f$Year<=2009 ,]$Value)
num2=nrow(f[f$Year>=2000 & f$Year<=2009 ,])
mean2 = sum2/num2
cat("Mean for latest decade is : ",mean2)
std1 = sd(f[f$Year>=2000 & f$Year<=2009,]$Value)
cat("\nStandarad Deviation is : ",std1)
num1 <- sqrt(num2)
ste1 <- std1/num1
cat("\nStandard error is : ",ste1)
pval <- pnorm(mean2,mean1,ste1)
cat("\nP value is :",pval)
if(pval<0.05){
  cat("\nClaim can be rejected for 0.05")
}else{
  cat("\nClaim cannot be rejected for 0.05\n")
}

