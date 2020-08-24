Kendallcor<- function(X,Y){
n<-length(X)
c1<-max(X)+0.5
c2<-max(Y)+0.5
plot(X,Y, las=1,xlim=c(0,c1),ylim=c(0,c2));

A <- matrix(0,n,n);
for (i in 1:n) for (j in (1:n)[-i]) if ( ((Y[j]-Y[i])/(X[j]-X[i]))>0) A[i,j]<-1;
for (i in 1:n) for (j in (1:n)[-i]) if ( ((Y[j]-Y[i])/(X[j]-X[i]))<0) A[i,j]<--1;

for (i in 1:n) for (j in (1:n)[-i]){
if (A[i,j]== 1){col= 2;   lty=1};
if (A[i,j]==-1){col= 4; lty=3};
if (A[i,j] != 0) lines(c(X[i],X[j]),c(Y[i],Y[j]),col=col,lty=lty);
}

conc<-(sum(A>0)/2);
disc<-(sum(A<0)/2);
tau<-2*(conc-disc)/(n*(n-1));

text(c1-0.5,c2-0.1,paste(expression(Concordantes),conc));
text(c1-0.5,c2-0.2,paste(expression(Discordantes),disc));
text(c1-1.5,c2-0.5,expression(frac(2*(conc-disc),n(n-1))) );
text(c1-0.9,c2-0.5,adj=c(0,0.5),paste(rawToChar(as.raw(61)),round(tau,digits=3)) );
}

Kendallcor(t.d,t.impuesto)