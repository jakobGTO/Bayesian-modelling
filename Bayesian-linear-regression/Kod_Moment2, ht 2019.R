### Bayesiansk statistik, 732g43, moment 2 ###
### Kod fr?n Statistical Rethinking, Richard McElreath ###
### BW, kod fr?n Bertil Wegmann ###
library(rethinking)

### Laddar data fr?n mtcars ###
data(mtcars)
mtcars
# R kod g?llande f?rel?sningsslides 7 till 11 p? moment 2
# R kod, kvadratisk approximation med normalf?rdelade data och ok?nd varians
# br?nslef?rbrukning f?r 32 stycken bilar

# Plotta priors f?r bilexemplet med br?nslef?rbrukning, #R kod 2.1
sampleMu <- rnorm(1e4,1,1)
dens(sampleMu)
sampleSigma <- runif(1e4,0,2)
dens(sampleSigma)
sample_y <- rnorm(1e4,sampleMu,sampleSigma)
dens(sample_y)

# Transformera responsvariabeln till liter per mil
mtcars$y <- 23.52/mtcars$mpg

#R kod 2.2, kvadratisk approximation
flist <- alist(
  y ~ dnorm(mu, sigma) , # likelihood fr?n normalf?rdelning, liter per mil
  mu ~ dnorm( 1 , 1 ) , # normalf?rdelad prior
  sigma ~ dunif ( 0 , 2 )
)

resNormal <- map(flist, data=mtcars)
precis(resNormal, prob=0.909)

postSamples <- extract.samples(resNormal , n=1e4) # posterior samples

#R kod 2.3, kvadratisk approximation med prior f?r log sigma #
flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu ~ dnorm( 1 , 1 ) , # normalf?rdelad prior
  logsigma ~ dnorm ( 0 , 0.5 )
)

resNormal_logsigma <- map(flist, data=mtcars)
precis(resNormal_logsigma,prob=0.909)

# Compare sigma from prior on sigma or logsigma
dens(postSamples[,2])
postSamples_logsigma <- extract.samples(resNormal_logsigma , n=1e4) # posterior samples
dens(exp(postSamples_logsigma[,2]),type="l",col="red",add=TRUE)

#R kod 2.4, j?mf?r y med predikterat y fr?n modellen
yPred <- rnorm(1e4,postSamples[,1],postSamples[,2])
dens(mtcars$y)
dens(yPred,type="l",col="red",add=TRUE)

#R kod 2.5
Kovar <- vcov(resNormal) # kovariansmatrisen
cov2cor(Kovar) # korrelationsmatrisen

# posterior samples igen fr?n multivariat normalf?rdelningen
library(MASS)
postsamples2 <- mvrnorm( n=1e4 , mu=coef(resNormal) , Sigma=vcov(resNormal) )

### R kod g?llande f?rel?sningsslides 16 till 20 p? moment 2

# R kod 2.6, kvadratisk approximation med enkel linj?r regression
# y = br?nslef?rbrukning f?r 32 stycken bilar i liter per mil
mtcars$x1 <- 0.45*mtcars$wt # x1 = vikt i ton f?r bilarna
flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu <- b0 + b1*x1 , # hur mu ?r l?nkad till f?rklaringsvariabler.
  b0 ~ dnorm( 1 , 1 ) , # normalf?rdelad prior f?r intercept
  b1 ~ dnorm( 0.5 , 1 ) ,
  logsigma ~ dnorm ( 0 , 0.5 )
)

resNormal <- map(flist, data=mtcars)
precis(resNormal,prob=0.952)

# R kod 2.7
postSamples <- extract.samples(resNormal , n=1e6) # posterior samples
ProbNegEff <- sum(postSamples[,2] < 0)/1e6
ProbPosEff <- 1 - ProbNegEff
Odds <- ProbNegEff/ProbPosEff

# R kod 2.8
Kovar <- vcov(resNormal) # kovariansmatrisen
cov2cor(Kovar) # korrelationsmatrisen

# R kod, kvadratisk approximation med enkel linj?r regression
mtcars$x1_s <- mtcars$x1 - mean(mtcars$x1) # x1 ?r centrerad

flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, km per liter
  mu <- b0 + b1*x1_s , # hur mu ?r l?nkad till f?rklaringsvariabler.
  b0 ~ dnorm( 1 , 1 ) , # normalf?rdelad prior f?r intercept
  b1 ~ dnorm( 0.5 , 1 ) ,
  logsigma ~ dnorm ( 0 , 0.5 )
)

resNormal_s <- map(flist, data=mtcars)
precis(resNormal_s,prob=0.909)

# R kod 2.9

# Plotta map regressionslinjen
plot( mtcars$y ~ mtcars$x1 )
abline( a=coef(resNormal)["b0"] , b=coef(resNormal)["b1"] )
# Plotta 20 regressionslinjer f?r 20 dragningar fr?n posteriorn
postSamples <- extract.samples(resNormal , n=20) # posterior samples
for (i in 1:20){abline( a=postSamples[i,1] , b=postSamples[i,2])}

# Posteriorn f?r f?rv?ntad br?nslef?rbrukning, mu, ang en bil med vikt 1.5 ton
postSamples <- extract.samples(resNormal , n=1e4) # posterior samples
mu1.5 <- postSamples[,1] + postSamples[,2]*1.5
dens(mu1.5)

# R kod 2.10

# Posteriorn f?r mu f?r bilar mellan 800 och 2000 kg
# use type="n" to hide raw data
xSeq <- seq(from=0.8,to=2.0,by=0.01)
plot( mtcars$y ~ mtcars$x1 ,  type="n" )

# loop over samples and plot each mu value
for ( i in 1:1e3 ){
  mu <- postSamples[i,1] + postSamples[i,2]*xSeq
  points( xSeq , mu , pch=16 , col=col.alpha(rangi2,0.1) )
}

# 90.9 % and 95.2 % Kredibilitetsintervall f?r mu f?r bilar mellan 800 och 2000 kg
mu.ci <- sapply( xSeq , function(x) PI( postSamples[,1] + postSamples[,2]*x , prob=0.909) )
shade( mu.ci , xSeq )
mu.ci95 <- sapply( xSeq , function(x) PI( postSamples[,1] + postSamples[,2]*x , prob=0.952) )
shade( mu.ci95 , xSeq )

# 90.9 % prediktionsintervall f?r y f?r bilar mellan 800 och 2000 kg
sim.y <- matrix(0,nrow=1e4,ncol=length(xSeq))
sigma <- exp(matrix(postSamples[,3],nrow=1000,ncol=1))
for ( i in 1:length(xSeq) ){
  mu <- postSamples[,1] + postSamples[,2]*xSeq[i]
  sim.y[,i] <- rnorm(1e4,mu,sigma)
}
y.PI <- apply( sim.y , 2 , PI , prob=0.909 )

# plot raw data
plot( y ~ x , col=col.alpha(rangi2,0.5) )
# draw MAP line
abline( a=coef(resNormal)["b0"] , b=coef(resNormal)["b1"] )
# draw PI region for simulated kilometres per litre (simulated y values)
shade( y.PI , xSeq )

# R kod 2.11
# R kod g?llande f?rel?sningsslide 25 till 26 p? moment 2
# R kod, kvadratisk approximation med multipel linj?r regression
# f?rklaringsvariablerna standardiseras
mtcars$x1 <- mtcars$am
mtcars$x2 <- (mtcars$wt-mean(mtcars$wt))/sd(mtcars$wt)
mtcars$x3 <- (mtcars$hp-mean(mtcars$hp))/sd(mtcars$hp)
mtcars$x4 <- (mtcars$qsec-mean(mtcars$qsec))/sd(mtcars$qsec)
mtcars$x5 <- (mtcars$gear-mean(mtcars$gear))/sd(mtcars$gear)

flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, km per liter
  mu <- b0 + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5 , # hur mu ?r l?nkad till f?rklaringsvariabler.
  b0 ~ dnorm( 0 , 10 ) , # normalf?rdelad prior f?r interceptet
  c(b1,b2,b3,b4,b5) ~ dnorm( 0 , 5 ) , # samma normalf?rdelade prior till alla lutningspar.
  logsigma ~ dnorm ( 0 , 5 )
)

resMultReg <- map(flist, data=mtcars)
precis(resMultReg,prob=0.909)
precis(resMultReg,prob=0.952)
Kovar <- vcov(resMultReg) # kovariansmatrisen
cov2cor(Kovar) # korrelationsmatrisen

postSamples <- extract.samples(resMultReg , n=1e2) # posterior samples
plot(postSamples)
pairs(postSamples)
