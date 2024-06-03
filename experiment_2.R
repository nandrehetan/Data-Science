df=read.csv("travelled abroad_csv.csv")
p=dim(df[df$Travelledabroad=="Y",])[1]/dim(df)[1]
p=sum(df$Travelledabroad=="Y")/nrow(df)
cat("\nProbabilty of success ",p)
cat("\nPercentage of success ",p*100)
cat("\n")

#n=10 case
d=dbinom(0:10,10,p)
cat("\nProbability for k=0:10 with B.D,:",d)

#plots
plot(0:10,d,type="l")

#4th question
me=100*p
cat("\n\nmean is ",me)
s=sqrt(100*p*(1-p))
cat("\n\nstanderd deviation ",s)
p1=pnorm(59,me,s,lower.tail =FALSE)
cat("\n\nProb for n=100 and atleast 59 have travelled abroad using normal distribution ",p1)

#verification
d2=dbinom(59:100,100,p)
ans=sum(d2)
cat("\n\nBinomial distribution for n=100",ans)

d3=(dbinom(0:100,100,p))
plot(0:100,d3,type="l")