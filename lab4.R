#LMR ----
f<-read.csv("Toy_sales_csv.csv")

#Simple Linear Model----
l1<-lm(Unitsales~Price,f) #
s1<- summary(l1)
print(s1)

library(ggplot2)
p<- ggplot(f,aes(Price,Unitsales))+geom_point()+geom_smooth(method="lm",formula=y~x,col="red",se=F)
print(p)
pred<- predict(l1) 
print(pred)
e<- f$Unitsales-pred  
print(e)


#Muliple Linear Model----
l2<- lm(Unitsales~Price+Adexp+Promexp,f)
s2<- summary(l2)
print(s2)
df<-data.frame(Price=c(9.1,8.1),Adexp= c(52,50), Promexp=c(61,60))
pred2<- predict(l2,df) 
print(pred2)
df1<- cbind(df,pred2)
print(df1)
