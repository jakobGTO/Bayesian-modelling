### Bayesiansk statistik, 732g34, moment 3 ###
### Kod från Statistical Rethinking, Richard McElreath ###
### Kod från Bertil Wegmann ###
library(rethinking)

### Laddar data från mtcars ###
data(mtcars)

nObs <- 20

# Transformera responsvariabeln till liter per mil
mtcars$y <- 23.52/mtcars$mpg
mtcars$x <- (mtcars$wt -  mean(mtcars$wt))/sd(mtcars$wt) # x = vikt i ton för bilarna, standardiserad variabel
y <- mtcars$y[1:nObs]
x <- mtcars$x[1:nObs]

CarData <- data.frame(y,x)

#R kod 3.1
### kvadratisk approximation utan x ###
flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood från normalfördelning, liter per mil
  mu ~ dnorm( 1 , 1 ) , # normalfördelad prior
  logsigma ~ dnorm ( 0 , 2 )
)

Under_Modell <- map(flist, data=CarData)
precis(Under_Modell,prob=0.909)
postSamp <- extract.samples(Under_Modell , n=1e4) # posterior samples

# R kod 3.2
### kvadratisk approximation med polynomisk regression för x ###
flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood från normalfördelning, km per liter
  mu <- b0 + b1*x + b2*x**2 + b3*x**3 + b4*x**4 + b5*x**5 + 
    b6*x**6 + b7*x**7 + b8*x**8, # hur mu är länkad till förklaringsvariabler.
  b0 ~ dnorm( 1 , 1 ) , # normalfördelad prior för intercept
  c(b1,b2,b3,b4,b5,b6,b7,b8) ~ dnorm( 0 , 5 ) , # samma normalfördelade prior till alla lutningspar.
  logsigma ~ dnorm ( 0 , 2 )
)

Over_Modell <- map(flist, data=CarData)
precis(Over_Modell,prob=0.909)
postSampOver <- extract.samples(Over_Modell , n=1e4) # posterior samples

#R kod 3.3
### kvadratisk approximation med x ###
flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood från normalfördelning, km per liter
  mu <- b0 + b1*x , # hur mu är länkad till förklaringsvariabler.
  b0 ~ dnorm( 1 , 1 ) , # normalfördelad prior för intercept
  b1 ~ dnorm( 0 , 5 ) ,
  logsigma ~ dnorm ( 0 , 2 )
)

Balanserad_Modell <- map(flist, data=CarData)
precis(Balanserad_Modell,prob=0.909)
postSampBal <- extract.samples(Balanserad_Modell , n=1e4) # posterior samples

# Plotta regressionslinjer för posterior medelvärdet och plotta data
xSeqmin <- min(mtcars$x)
xSeqmax <- max(mtcars$x)
xSeq <- seq(from=xSeqmin,to=xSeqmax,by=0.001)

plot (mtcars$y ~ mtcars$x, lwd="4")

yHat <- rep(mean(postSamp[,1]),length(xSeq))
lines( xSeq, yHat, col="green" )

yHat <- mean(postSampBal[,1]) + mean(postSampBal[,2])*xSeq
lines( xSeq, yHat, type="l", col="red" )

yHat <- mean(postSampOver[,1]) + mean(postSampOver[,2])*xSeq + mean(postSampOver[,3])*xSeq**2 + 
  mean(postSampOver[,4])*xSeq**3  + mean(postSampOver[,5])*xSeq**4 + mean(postSampOver[,6])*xSeq**5 + 
  mean(postSampOver[,7])*xSeq**6  + mean(postSampOver[,8])*xSeq**7 + mean(postSampOver[,9])*xSeq**8
lines( xSeq, yHat, type="l", col="blue" )

points(x,y,col = "blue", lwd="4")

#R kod 3.4
### Beräkning av Deviance för training data för respektive modell ###

# extract MAP estimates
theta <- coef(Under_Modell)
# compute deviance
dev_Under <- (-2)*sum( dnorm(
  y ,
  mean=theta[1] ,
  sd=exp(theta[2]) ,
  log=TRUE ) )
dev_Under

# extract MAP estimates
theta <- coef(Balanserad_Modell)
# compute deviance
dev_Bal <- (-2)*sum( dnorm(
  y ,
  mean=theta[1] + theta[2]*x,
  sd=exp(theta[3]) ,
  log=TRUE ) )
dev_Bal

# extract MAP estimates
theta <- coef(Over_Modell)
# compute deviance
dev_Over <- (-2)*sum( dnorm(
  y ,
  mean=theta[1] + theta[2]*x + theta[3]*x**2 + theta[4]*x**3 + theta[5]*x**4 + 
    theta[6]*x**5 + theta[7]*x**6 + theta[8]*x**7 + theta[9]*x**8,
  sd=exp(theta[10]) ,
  log=TRUE ) )
dev_Over

StartTest <- nObs+1
yTest <- mtcars$y[StartTest:32]
xTest <- mtcars$x[StartTest:32]
### Beräkning av Deviance för test data för respektive modell ###
# extract MAP estimates
theta <- coef(Under_Modell)
# compute deviance
dev_Under_Test <- (-2)*sum( dnorm(
  yTest ,
  mean=theta[1] ,
  sd=exp(theta[2]) ,
  log=TRUE ) )
dev_Under_Test

# extract MAP estimates
theta <- coef(Balanserad_Modell)
# compute deviance
dev_Bal_Test <- (-2)*sum( dnorm(
  yTest ,
  mean=theta[1] + theta[2]*xTest,
  sd=exp(theta[3]) ,
  log=TRUE ) )
dev_Bal_Test

# extract MAP estimates
theta <- coef(Over_Modell)
# compute deviance
dev_Over_Test <- (-2)*sum( dnorm(
  yTest ,
  mean=theta[1] + theta[2]*xTest + theta[3]*xTest**2 + theta[4]*xTest**3 + theta[5]*xTest**4 + 
    theta[6]*xTest**5 + theta[7]*xTest**6 + theta[8]*xTest**7 + theta[9]*xTest**8,
  sd=exp(theta[10]) ,
  log=TRUE ) )
dev_Over_Test


# R kod 3.5
### Effekt från regulariseringsprior på parameterlutningar ###
### kvadratisk approximation med polynomisk regression för x ###
flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood från normalfördelning, km per liter
  mu <- b0 + b1*x + b2*x**2 + b3*x**3 + b4*x**4 + b5*x**5 + 
    b6*x**6 + b7*x**7 + b8*x**8, # hur mu är länkad till förklaringsvariabler.
  b0 ~ dnorm( 1 , 1 ) , # normalfördelad prior för intercept
  c(b1,b2,b3,b4,b5,b6,b7,b8) ~ dnorm( 0 , 0.1 ) , # samma normalfördelade prior till alla lutningspar.
  logsigma ~ dnorm ( 0 , 2 )
)

Over_Modell_RegPrior <- map(flist, data=CarData)
precis(Over_Modell_RegPrior,prob=0.909)
postSampOver_RegPrior <- extract.samples(Over_Modell_RegPrior , n=1e4) # posterior samples

### Beräkning av Deviance för training data för överanpassad modell med reg prior ###
# extract MAP estimates
theta <- coef(Over_Modell_RegPrior)
# compute deviance
dev_Over_RegPrior <- (-2)*sum( dnorm(
  y ,
  mean=theta[1] + theta[2]*x + theta[3]*x**2 + theta[4]*x**3 + theta[5]*x**4 + 
    theta[6]*x**5 + theta[7]*x**6 + theta[8]*x**7 + theta[9]*x**8,
  sd=exp(theta[10]) ,
  log=TRUE ) )
dev_Over_RegPrior

StartTest <- nObs+1
yTest <- mtcars$y[StartTest:32]
xTest <- mtcars$x[StartTest:32]

# compute deviance
dev_Over_RegPrior_Test <- (-2)*sum( dnorm(
  yTest ,
  mean=theta[1] + theta[2]*xTest + theta[3]*xTest**2 + theta[4]*xTest**3 + theta[5]*xTest**4 + 
    theta[6]*xTest**5 + theta[7]*xTest**6 + theta[8]*xTest**7 + theta[9]*xTest**8,
  sd=exp(theta[10]) ,
  log=TRUE ) )
dev_Over_RegPrior_Test

# R kod 3.6
### Beräkna AIC, DIC och WAIC för modellen utan förklaringsvar (Underanp., p=2)
### modellen med 1 förklaringsvar (bal., p=3)
### modellen med polynomisk regression (Överanp., p=10)
AIC_Under <- dev_Under + 2*2
AIC_Under
AIC_Bal <- dev_Bal + 2*3
AIC_Bal
AIC_Over <- dev_Over + 2*10
AIC_Over

DIC_Under <- DIC(Under_Modell)
DIC_Under
DIC_Bal <- DIC(Balanserad_Modell)
DIC_Bal
DIC_Over <- DIC(Over_Modell)
DIC_Over

WAIC_Under <- WAIC(Under_Modell)
WAIC_Under
WAIC_Bal <- WAIC(Balanserad_Modell)
WAIC_Bal
WAIC_Over <- WAIC(Over_Modell)
WAIC_Over


# R kod 3.7
### Modelljämförelse med y = liter per mil, x1 = hästkrafter i hundratals, x2 = antal sek på 1/4 mile
nObs <- 32
y <- mtcars$y[1:nObs]
x1 <- 0.01*mtcars$hp[1:nObs]
x2 <- mtcars$qsec[1:nObs]

CarData <- data.frame(y,x1,x2)

### kvadratisk approximation utan x ###
flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood från normalfördelning, km per liter
  mu ~ dnorm( 1 , 1 ) , # normalfördelad prior
  logsigma ~ dnorm ( 0 , 2 )
)

M_utan_x <- map(flist, data=CarData)
precis(M_utan_x,prob=0.909)

### kvadratisk approximation med x1 ###
flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood från normalfördelning, km per liter
  mu <- b0 + b1*x1 , # hur mu är länkad till förklaringsvariabler.
  b0 ~ dnorm( 1 , 1 ) , # normalfördelad prior för intercept
  b1 ~ dnorm( 0 , 5 ) ,
  logsigma ~ dnorm ( 0 , 2 )
)

M_hp <- map(flist, data=CarData)
precis(M_hp,prob=0.909)

### kvadratisk approximation med x2 ###
flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood från normalfördelning, km per liter
  mu <- b0 + b1*x2 , # hur mu är länkad till förklaringsvariabler.
  b0 ~ dnorm( 1 , 1 ) , # normalfördelad prior för intercept
  b1 ~ dnorm( 0 , 5 ) ,
  logsigma ~ dnorm ( 0 , 2 )
)

M_sek <- map(flist, data=CarData)
precis(M_sek,prob=0.909)

### kvadratisk approximation med x1 och x2 ###
flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood från normalfördelning, km per liter
  mu <- b0 + b1*x1 + b2*x2 , # hur mu är länkad till förklaringsvariabler.
  b0 ~ dnorm( 1 , 1 ) , # normalfördelad prior för intercept
  c(b1,b2) ~ dnorm( 0 , 5 ) ,
  logsigma ~ dnorm ( 0 , 2 )
)

M_hp_sek <- map(flist, data=CarData)
precis(M_hp_sek,prob=0.909)


### Jämför modeller med Akaikes vikter ###
CompMod <- compare(M_utan_x, M_hp, M_sek, M_hp_sek, n=1e3)
CompMod
### Plotta map och 90.9 % kredibilitetsintervall för resp. parameter och modell
plot( coeftab(M_utan_x,M_hp,M_sek,M_hp_sek),prob=0.909 )


# R kod 3.8
### Skattning av multipel linjär regressionsmodell med MCMC ###

# Förklaringsvariablerna standardiseras
y <- mtcars$y
x1 <- mtcars$am
x2 <- (mtcars$wt-mean(mtcars$wt))/sd(mtcars$wt)
x3 <- (mtcars$hp-mean(mtcars$hp))/sd(mtcars$hp)
x4 <- (mtcars$qsec-mean(mtcars$qsec))/sd(mtcars$qsec)
x5 <- (mtcars$gear-mean(mtcars$gear))/sd(mtcars$gear)

CarDataMCMC <- data.frame(y,x1,x2,x3,x4,x5)

# Skatta modell med map2stan
Cars.stan.modell <- alist( y ~ dnorm( mu , sigma ) ,
                         mu <- b0 + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5 ,
                         b0 ~ dnorm(1,1),
                         b1 ~ dnorm(0,5),
                         b2 ~ dnorm(0,5),
                         b3 ~ dnorm(0,5),
                         b4 ~ dnorm(0,5),
                         b5 ~ dnorm(0,5),
                         sigma ~ dcauchy(0,2)
)

Cars.stan <- map2stan(Cars.stan.modell,data=CarDataMCMC )

# Sammanfattning av de skattade parametrarna från MCMC
precis(Cars.stan,prob=0.909)

# Sammanfattning av univariata och parvisa posteriorfördelningar
pairs(Cars.stan)

# Plotta MCMC dragningarna
plot(Cars.stan)

# Ange 4 MCMC kedjor och 2 kärnor
Cars.stan_Parallell <- map2stan(Cars.stan.modell,data=CarDataMCMC,chains=4,cores=2)

# Plotta MCMC dragningar för bra mixing mellan MCMC kedjorna.
# Om det är bra mixing, så behövs bara en MCMC kedja för posterior samples
plot(Cars.stan_Parallell)

# använd kvadratisk approximation för map skattningar som bättre startvärden till MCMC.
flist <- alist(
  y ~ dnorm(mu, sigma) , # likelihood från normalfördelning, km per liter
  mu <- b0 + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5 , # hur mu är länkad till förklaringsvariabler.
  b0 ~ dnorm( 1 , 1 ) , # normalfördelad prior för interceptet
  c(b1,b2,b3,b4,b5) ~ dnorm( 0 , 5 ) , # samma normalfördelade prior till alla lutningspar.
  sigma ~ dcauchy(0,2)
)

resKvadApprox <- map(flist, data=CarDataMCMC)
Start <- coef(resKvadApprox)
Start

# Skatta modell med map2stan med map skattningar som startvärden
# Använd bättre startvärden till MCMC
Cars.stan.start <- map2stan(Cars.stan.modell,data=CarDataMCMC,
                            start=list(b0=Start[1],b1=Start[2],b2=Start[3],b3=Start[4],
                                       b4=Start[5],b5=Start[6],sigma=Start[7]),
                            chains=2,iter=4000,warmup=1000)

# Plotta MCMC dragningarna med bättre startvärden
plot(Cars.stan.start)

PostSamp <- extract.samples(Cars.stan.start)
# Undersök konvergens för posterior means
AnvPar <- PostSamp$b1
NIter <- length(PostSamp$b1)
Means <-  matrix(0,nrow=NIter,ncol=1,byrow=TRUE)
for (iter in 1:NIter){
  Means[iter] <- mean(AnvPar[1:iter])
}
plot(Means)

# Undersök konvergens för olika kvantiler
Quant <- matrix(0,nrow=NIter,ncol=4,byrow=TRUE)
for (iter in 1:NIter){
  Quant[iter,] <- quantile(AnvPar[1:iter],probs=c(0.25,0.50,0.75,0.99))
}
plot(Quant[,1])
plot(Quant[,2])
plot(Quant[,3])
plot(Quant[,4])







