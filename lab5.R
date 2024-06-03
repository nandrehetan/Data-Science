setwd("D:/Data Science")
f1<- read.csv("knn1_csv.csv")
ed<- sqrt((f1$x-3)**2+(f1$y-2)**2)
print(ed)

f1<- cbind(f1,ed)
print(f1)

order(f1$ed) # sorts the index value wrt ed 
f1_sort<- f1[order(f1$ed),] # all rows are sorted wrt ed
print(f1_sort)

#NN algorithm----
cat("\nclass of P for NN is :", f1_sort[1,4])

#kNN algorithm for K=5----
df1<- f1_sort[1:5,]
l1<- sum(df1$class==1)
l2<- sum(df1$class==2)
l3<- sum(df1$class==3)

if(l1>l2 & l1>l3)
  cat("\nclass of P for K=5 is :", 1)
if(l2>l1 &l2>l3)
  cat("\nclass of P for K=5 is :", 2)
if(l3>l1 & l3>l2)
  cat("\nclass of P for K=5 is :", 3)

#Radius based NN----
df2<- f1_sort[f1_sort$ed<1.45,]
l11<- sum(df2$class==1)
l21<- sum(df2$class==2)
l31<- sum(df2$class==3)

if(l11>l21 & l11>l31)
  cat("\nclass of P for rnn is :", 1)
if(l21>l11 &l21>l31)
  cat("\nclass of P for rnn is :", 2)
if(l31>l11 & l31>l21)
  cat("\nclass of P for rnn is :", 3)

