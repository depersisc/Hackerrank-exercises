# Enter your code here. Read input from STDIN. Print output to STDOUT
f<-file("stdin")
open(f)
firstline<-readLines(f,n=1)
count<-strsplit(firstline," ")[[1]]
count<-as.numeric(count)
parameter<-matrix(nrow=as.numeric(count[2]),ncol=as.numeric(count[1]))
price<-vector()
for (i in 1:as.numeric(count[2]))
{
lines<-readLines(f,n=1)
num<-strsplit(lines," ")[[1]]
for (j in 1:as.numeric(count[1]))
{parameter[i,j]<-as.numeric(num[j])}
price<-c(price,as.numeric(num[as.numeric(count[1])+1]))
}
mydata<-data.frame(price,parameter)
model<-lm(
    as.formula(paste(colnames(mydata)[1], "~",
        paste(colnames(mydata)[c(2:as.numeric(count[1]))], collapse = "+"),
        sep = ""
    )),
    data=mydata
)

n_new<-readLines(f,n=1)
n_new<-as.integer(n_new)
newparameter<-matrix(nrow=n_new,ncol=as.numeric(count[1]))
for (z in 1:n_new)
{
lines<-readLines(f,n=1)
numb<-strsplit(lines," ")[[1]]
for (x in 1:as.numeric(count[1]))
{newparameter[z,x]<-as.numeric(numb[x])}
}
newdata<-data.frame(newparameter)
write(predict(model,newdata),stdout())
close(f)
