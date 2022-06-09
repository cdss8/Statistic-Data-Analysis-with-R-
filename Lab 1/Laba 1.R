
n=150
shape=2
rate=3
p=0.688

##########################
####Gamma Distribution#### 
##########################


dgamma = rgamma(n,shape,rate)
dgamma
fitdistr(dgamma,"gamma")


mean(dgamma)
var(dgamma)
sd(dgamma)
median(dgamma)
density(dgamma)$x[which.max(density(dgamma)$y)]




skewness(dgamma)
kurtosis(dgamma)

disp= shape/(rate*rate)
disp

matexpect= shape/rate
matexpect

summary(dgamma) #???????? ????????


h1=hist(dgamma)
lines(h1$counts ~ h1$mids, col="red")
plot(ecdf(dgamma))




###############################
####Geometric Distribution #### 
###############################

dgeom = rgeom(n,prob)
dgeom
fitdistr(dgeom, "geometric")

var(dgeom)
mean(dgeom)
sd(dgeom)
median(dgeom)

density(dgeom)$x[which.max(density(dgeom)$y)]

sort(dgeom)
dgeom.t<-table(dgeom)
dgeom.t
sort(dgeom)[which.max(dgeom.t)]

skewness(dgeom)
kurtosis(dgeom)

geomatexpect=(1-prob)/prob
geomatexpect
prob
p

geodisp=(1-prob)/(prob*prob)
geodisp

summary(dgeom)

h3=hist(dgeom)
lines(h3$counts ~ h3$mids, col="red")
em1=ecdf(dgeom)
em1
plot(em1)


###############################
#### Binomial distribution #### 
###############################

size=7
dbin = rbinom(n, size, p)
dbin

var(dbin)
mean(dbin)
sd(dbin)
median(dbin)

density(dbin)$x[which.max(density(dbin)$y)]

sort(dbin)
dbin.t<-table(dbin)
dbin.t
sort(dbin)[which.max(dbin.t)]

skewness(dbin)
kurtosis(dbin)

binmatexpect=n*p
binmatexpect

bindisp=n*p*(1-p)
bindisp

summary(dbin)

h4<-hist(dbin)
lines(h4$counts ~ h4$mids, col="red")
em2=ecdf(dbin)
em2
plot(em2)


##################################
#### Exponential distribution #### 
##################################

rate=1

dexp=rexp(n, rate)
dexp

mean(dexp)
var(dexp)
sd(dexp)
median(dexp)

density(dexp)$x[which.max(density(dexp)$y)]

sort(dexp)
dexp.t<-table(dexp)
dexp.t
sort(dexp)[which.max(dexp.t)]

skewness(dexp)
kurtosis(dexp)

expmatexpect=1/rate
expmatexpect

expdisp=1/(rate*rate)
expdisp

summary(dexp)

h4=hist(dexp)
lines(h4$counts ~ h4$mids, col="red")
em1=ecdf(dexp)
em1
plot(em1)

#######################
#######################


#6.????????? ???????? ? ???? ????????????? ? ??????? ???????? X^2
#???????? ???????????
h1<-hist(dexp,freq = FALSE,col = "lightblue",xlab = "?????????? X",ylab = "????????? ???????????",main="??????????? ????? ?????????????" )
#? ?????????? ???????? ??????? ??????????
a<-h1$breaks
h1
a
#?????? ????? ???????
a[1]=0
k<-length(a)-1
k
#?????? ??????? ?????????????
a[k+1]<-Inf

#???????? ???????
d<-h1$counts
d
#???????? ???????? ???????????
d1<-pexp(a,rate=1)
d1
d2<-d1[2:(k+1)]-d1[1:k]
d2
#???????? ???????
chisq.test(d,d2)

library(moments)
library(MASS)


xho=hist(dgamma, plot=FALSE)$counts;
xho
xhb=hist(dgamma, plot=FALSE)$breaks; 
xhb
k=length(xhc)
k
xhb[1]=-1
xhb[k+1]=Inf
pnth=pgamma(xhb,shape,rate);
pnth
h1=pnth[2:(kj+1)]-pnth[1:k];
h1
chisq.test(xho, h1)

