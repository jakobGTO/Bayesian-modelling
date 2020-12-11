# Linjär regressionsanalys

##Data

require(rethinking)
load("C:/Users/Jakob/Desktop/Bayesian statistics/lab2/Data_Moment2.RData")

X <- as.data.frame(X)


set.seed(970922);y <- rnorm(n,A,B)

####Uppgift 1####
#==================================================================================
#==================================================================================


#a)
#Samplar från priorfördelningen för y med exp av log sigma = sigma
sampleMu <- rnorm(1e4,3,10)
sampleLogSigma <- rnorm(1e4,0,1)

sample_y <- rnorm(1e4,sampleMu,exp(sampleLogSigma))
dens(sample_y, main = "Priorfördelning för y", xlim = c(-50,50))
mean(sample_y)
#b)

#Anpassar kvadratisk approximation för linj.reg modell utan förklaringsvariabler

flist <- alist(
  y/1000 ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu ~ dnorm( 3 , 10 ) , # normalf?rdelad prior
  logsigma ~ dnorm ( 0 , 1 )
)

resNormal_logsigma <- map(flist, data=X)

precis(resNormal_logsigma)

postSamples_logsigma <- extract.samples(resNormal_logsigma , n=1e4) # posterior samples

#Kan inte bara antilogga i precis, måste extracta och räkna om
#för att få kredibilitetsintervall för sigma inte logsigma
kredintervall_sigma <-precis(exp(postSamples_logsigma$logsigma),prob=0.909)

#Plottar logsigma priorfördelningen mot logsigma posteriorfördelningen
dens(exp(sampleLogSigma), xlim = c(-1,8),ylim = c(0,3.8), main = "Sigma fördelning")
dens(exp(postSamples_logsigma[,2]),col="red",add=TRUE)
legend("topright", legend = c("Prior logsigma", "Posterior logsigma"), fill=c("black", "red"), cex = 0.8)

#Plottar my priorfördelningen mot my posteriorfördelnignen
dens(sampleMu, ylim = c(0,2.5), xlim = c(-20,25), main = "Mu fördelning")
dens(postSamples_logsigma[,1],type="l",col="red",add=TRUE)
legend("topright", legend = c("Prior mu", "Posterior mu"), fill=c("black", "red"), cex = 0.8)

mean(postSamples_logsigma[,1])
#c)

precis(resNormal_logsigma,prob=0.909)

postSamples_logsigma$sigma <- exp(postSamples_logsigma$logsigma)

#Låg korrelation ger att då vi lär oss om mu så lär vi oss
#inget om sigma och vice versa. Detta är vanligt vid gausiska
#modeller av detta slag
Kovar <-cov(postSamples_logsigma[,c(1,3)])
cov2cor(Kovar)


#d)

#Modellutvärdering genom att plotta posterior prediktiva fördelningen för y 
#mot priorfördelningen för y samt faktiska fördelningen för y.

#Posteriorprediktiva fördelningen för modellen i upg1b
yPred_1d <- rnorm(1e4,postSamples_logsigma[,1],exp(postSamples_logsigma[,2]))
#Fördelning för y i data
dens(y/1000, xlim = c(-2,8), ylim = c(0,0.5), main="Posterior fördelning")
#Posteriorprediktiv fördelning
dens(yPred_1d,type="l",col="red",add=TRUE)
#Priorfördelning från upg1a
dens(sample_y, col="blue",add=TRUE)
legend("topright", legend = c("true y", "pred y", "prior y"), fill=c("black", "red","blue"), cex = 0.8)

mean(sample_y)
mean(yPred_1d)
mean(y/1000)

#e)

#Uniform prior ska användas istället för normalprior som tidigare upg

#Beräknar my|sigma2 samt sigma2 med uniform prior genom:
#sigma2 = (n-1)*s^2 / f(sigma2) där f(sigma2) är chisq(1e4,n-1)

f_sigma <- rchisq(1e4,nrow(X) - 1)

sigma2 <- ( (nrow(X) - 1) * var(y/1000) ) / f_sigma

mu_sigma2 <- rnorm(n = 1e4, mean(y/1000), sqrt(sigma2/nrow(X)))

#Plottar sigma2s posteriorfördelning med unif prior mot normalprior
dens(sqrt(sigma2), main="Posteriorfördelning sigma")
dens(exp(postSamples_logsigma[,2]),col="red",add=TRUE)
legend("topright",inset=.05, legend = c("Uniform prior", "Normal prior"), fill=c("black", "red","blue"), cex = 0.8)

#Plottar mu|sigma2s fördelning med unif prior mot normalprior
dens(mu_sigma2, main="Betingad posterior mu")
dens(postSamples_logsigma[,1],type="l",col="red",add=TRUE)
legend("topright",inset=.05, legend = c("Uniform prior", "Normal prior"), fill=c("black", "red","blue"), cex = 0.8)


####Uppgift 2#####
#==================================================================================
#==================================================================================

#a)
data <- data.frame(y/1000,scale(X[,1:4]),X[,5:6])
colnames(data) <- c("y","area","antal_rum","avgift","trappor","cityyes","sydyes")

cor(data)

flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu <- b0 + b1*area , # hur mu ?r l?nkad till f?rklaringsvariabler.
  b0 ~ dnorm( 3 , 10 ) , # normalf?rdelad prior f?r intercept
  b1 ~ dnorm( 0 , 10 ) ,
  logsigma ~ dnorm ( 0, 1 )
)

resNormal <- map(flist, data=data)
precis(resNormal,prob=0.909)

postSamples_2a <- extract.samples(resNormal , n=1e4)
#b)

#Kredibilitetsintervall visar på  att när en lägenhetsarea
#ökar så ökar även priset. Osäkerheten i intervallet är minst
#runt 3.6 och ökar med större värden på y eftersom vi har mindre
#information runt dessa värden.

xSeq <- seq(from=min(data$area),to=max(data$area),by=0.01)
plot( data$y ~ data$area ,  col=col.alpha(rangi2,0.5) , main = "95.2% Kredibilitetsband")

# 90.9 % and 95.2 % Kredibilitetsintervall f?r mu f?r bilar mellan 800 och 2000 kg
mu.ci <- sapply( xSeq , function(x) PI( postSamples_2a[,1] + postSamples_2a[,2]*x , prob=0.952) )
shade( mu.ci , xSeq)


#c)

#Givet en _ny_ lägenhets area predikteras den med 90.9 procent
#sannolikhet inom det gråa intervallet. Skillnaden i kredibilitets
#och prediktionsintervall att i prediktionsintervall utalar vi oss bara
#om ny observaiton medan kredilibitetsintervall som ovan

sim.y <- matrix(0,nrow=1e4,ncol=length(xSeq))
sigma <- exp(matrix(postSamples_2a[,3],nrow=1000,ncol=1))
for ( i in 1:length(xSeq) ){
  mu <- postSamples_2a[,1] + postSamples_2a[,2]*xSeq[i]
  sim.y[,i] <- rnorm(1e4,mu,sigma)
}
y.PI <- apply( sim.y , 2 , PI , prob=0.909 )

# plot raw data
plot( data$y ~ data$area , col=col.alpha(rangi2,0.5) , main = "90.9% Prediktionsintervall")
# draw MAP line
abline( a=coef(resNormal)["b0"] , b=coef(resNormal)["b1"] )
# draw PI region for simulated kilometres per litre (simulated y values)
shade( y.PI , xSeq )

#d)

#Bayes förklaringsgrad tolkas som en databaserad skattnig av 
#andelen varians som förklaras för ny data. I detta fall 
#skattas med högst sannolikhet att 42 procent av variansen förklaras
#av ny data.

RjSamples <- extract.samples(resNormal , n=1e4)

SSR_sample <- rep(NA, nrow(RjSamples))
SSE_sample <- rep(NA, nrow(RjSamples))
for (j in 1:nrow(RjSamples)) {
  SSR <- rep(NA, length(data$area))
  SSE <- rep(NA, length(data$area))
  for (i in 1:length(data$area)) {
  mu_i <- RjSamples[j,1] + RjSamples[j,2] * data$area[i]
  SSR[i] <- (mu_i - mean(data$y))^2
  SSE[i] <- (data$y[i] - mu_i)^2
  }  
  SSR_sample[j] <- sum(SSR)
  SSE_sample[j] <- sum(SSE)
}

R2_bayes <- SSR_sample / (SSR_sample + SSE_sample) 
dens(R2_bayes, main = "Fördelning av Bayesian R^2")
mean(R2_bayes)
sd(R2_bayes)

#### Uppgift 3 ####
#a)



flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu <- b0 + b1*area + b2*antal_rum + b3*avgift + b4*trappor + b5*cityyes + b6*sydyes, # hur mu ?r l?nkad till f?rklaringsvariabler.
  b0 ~ dnorm( 3 , 10 ) , # normalf?rdelad prior f?r intercept
  c(b1,b2,b3,b4) ~ dnorm( 0 , 10 ) ,
  c(b5,b6) ~ dnorm( 0 , 10 ) ,
  logsigma ~ dnorm ( 0, 1 )
)

resNormal <- map(flist, data=data)

postSamples <- extract.samples(resNormal , n=1e6)

#Oddset för positiv eller negativ lutning
# ProbNeggEff/ProbPosEff = Hur många gånger mer sannolikt är det
# att en parameter påverkar modellen entydigt linjärt negativt mot positivt.

Odds <- matrix(NA, ncol = 6, nrow = 1)
colnames(Odds) <- c("b1","b2","b3","b4","b5","b6")
for (i in 1:6) {
  ProbNegEff <- sum(postSamples[,i+1] < 0)/1e6
  ProbPosEff <- 1 - ProbNegEff
  Odds[,i] <- ProbNegEff/ProbPosEff
}

Odds

#b)

precis(resNormal,prob=0.909)

#c)
postSamples <- extract.samples(resNormal , n=1e4)

mu_ij <- matrix(NA,ncol = length(data$y), nrow = nrow(postSamples))

#mu beräknas för modellen

for (i in 1:nrow(postSamples)) {
  for (j in 1:length(data$y)) {
    mu_ij[i,j] <- (postSamples[i,1] + postSamples[i,2] * data$area[j] + postSamples[i,3] * data$antal_rum[j]
                 + postSamples[i,4] * data$avgift[j] + postSamples[i,5] * data$trappor[j]
                 + postSamples[i,6] * data$cityyes[j] + postSamples[i,7] * data$sydyes[j])
  }
}

#Prediktion beräknas som i uppgift 1d, men här med 6 stycken
#variabler istället för 0 xvar. Måste således sampla från rnorm
#för varje värde på mu

#jämf: yPred_1d <- rnorm(1e4,postSamples_logsigma[,1],exp(postSamples_logsigma[,2]))


pred <- matrix(NA, ncol = length(data$y), nrow = nrow(postSamples))
for (i in 1:length(data$y)) {
  pred[,i] <- rnorm(n = 1e3, mean = mu_ij[,i], sd = exp(postSamples$logsigma))
}

#Gör om till en vektor med alla predikterade värden
pred_matrix <- matrix(pred, ncol=1)

dens(pred_matrix, main = "Posteriorförldening")
dens(yPred_1d,type="l",col="blue",add=TRUE)
dens(data$y,add=TRUE,col="red")
legend("topright", legend = c("Pred med 6 xvar", "Pred med 0 xvar", "Data"), fill=c("black", "red","blue"), cex = 0.8)

