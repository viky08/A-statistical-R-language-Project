
#Here we're attaching our data to our R script
attach(mdata)
library(ggplot2)
#Head displays first six entries
head(mdata)

#Multiple Plots
par(mfrow=c(1,3))
plot(mdata[,1],mdata[,3],col="#008080",type="h",pch=16,xlab="Roll nos.", ylab="Marks",main="Marks in Physics")
plot(mdata[,1],mdata[,4],col="#008080",type="h",pch=16,xlab="Roll nos.", ylab="Marks",main="Marks in English")
plot(mdata[,1],mdata[,5],col="#008080",type="h",pch=16,xlab="Roll nos.", ylab="Marks",main="Marks in Maths")

par(mfrow=c(1,3))
plot(mdata[,1],mdata[,6],col="#008080",type="h",pch=16,xlab="Roll nos.", ylab="Marks",main="Marks in Applied Mechanics")
plot(mdata[,1],mdata[,7],col="#008080",type="h",pch=16,xlab="Roll nos.", ylab="Marks",main="Marks in PEE")
plot(mdata[,1],mdata[,8],col="#008080",type="h",pch=16,xlab="Roll nos.", ylab="Marks",main="Marks in IT")

par(mfrow=c(1,1))

#Ggplot2
qplot(mdata[,1],mdata[,9],xlab="Roll Nos.", ylab="Total", color=Percentage,main="Total marks along with Percentage")

#Dot-Chart
dotchart(mdata[,4], main="Marks in English", pch=19,xlab="English Marks" ,ylab="Students")

#Barplot
barplot(mdata$IT,main="Marks in IT",border="white",col="#ffa07a",ylab="Marks in IT",xlab="Students")

#Horizontal Barplot
barplot(mdata$Ap..Mech.,main="Applied Mechanics",border="white",col=c("#008080","#fd4354"),ylab="Students",xlab="Marks in Applied Mechanics",horiz=TRUE)

#Histogram
hist(mdata$Physics,border="white", main="HISTOGRAM",xlab="Marks in Physics",ylab="No. of Students",col=c("#cc4078","#aa7645","#dd2131","green","#00ff66"))


#Scatter Plot
plot(mdata[,1],mdata[,4],col="#cd5c5c",type="o",ylab="Marks in English(brown)/Maths(Blue)", xlab="Roll nos.")
lines(mdata[,1],mdata[,5],col="#00ced1",type="o",pch=15)

#Multiple Plots
counts<-c(mdata$Physics,mdata$Ap..Mech.)
barplot(counts,main="Marks in Physics and Ap. Mech.",border="white",beside=TRUE,col=c("#9acd32","#de3143"),ylab="Marks in Physics(Green)/Ap.Mech.(Red)",xlab="Students")

#Pie Chart
v1<-length(mdata["Percentage"][mdata["Percentage"] <= 50])
v2<-length(mdata["Percentage"][mdata["Percentage"] >50] & mdata["Percentage"][mdata["Percentage"]<=60])
v3<-length(mdata["Percentage"][mdata["Percentage"] >60] & mdata["Percentage"][mdata["Percentage"]<=70])
v4<-length(mdata["Percentage"][mdata["Percentage"] >70] & mdata["Percentage"][mdata["Percentage"]<=80])
v5<-length(mdata["Percentage"][mdata["Percentage"] >80] & mdata["Percentage"][mdata["Percentage"]<=100])
v<-c(v1,v2,v3,v4,v5)

lbls<-c("Fail","Poor","Average","Good","Excellent")
pie(v,labels=lbls,main="Pie Chart showing performance of students",radius=1.1,col=c("#b22222","#006400","#483d8b","#7fffd4","#008080"))

#Boxplot
boxplot(mdata$Percentage,col="#ff2332",main="Boxplot showing median Percentage",ylab="Percentage")

#Calculations
z<-0
for(i in 1:48)
{
  z=mdata[["English"]]+mdata[["IT"]]+mdata[["Maths"]]+mdata[["Ap..Mech."]]+mdata[["Physics"]]+mdata[["PEE"]]
}
z1=z/5.4
y<-mean(mdata[,9])
x<-mean(mdata[,10])
a<-mode(mdata[,10])
o<-mdata[["Percentage"]][1]

for(i in 1:48)
{
  if(mdata[["Percentage"]][i]>o)
    o<-mdata[["Percentage"]][i]
}
c<-mdata[["Percentage"]][1]
for(i in 1:48)
{
  if(mdata[["Percentage"]][i]<c)
    c<-mdata[["Percentage"]][i]
}

cat("Mean total marks: ",y)
cat("Mean Percentage: ",x)
cat("Maximum Perecentage",o)
cat("Minimum Perecentage",c)
cat("\tMADE BY\n\tVikas Goswami\n\t402/CO/15")

