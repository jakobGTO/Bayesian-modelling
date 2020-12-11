require(rethinking)
require(xtable)


load("C:/Users/Jakob/Desktop/Bayesian statistics/lab4/VaccinationsData.Rdata")

#### Uppgift 1 ####

#### En bayesiansk logistisk modell ska anpassas med MCMC algoritmen
#### Modellen ska utvärderas med tracerplottar, n_eff, Rhat samt 
#### plottar för varje parameters ackumulerade posteriormedelvärde

## a)



#Skapa logistisk regressions modell

Upg1_data <- Vaccination_Barn[,1:3]
str(Upg1_data)
flist <- alist( y ~ dbinom( 1 , p ) ,
                logit(p) <- b0 + b1*x1 + b2*x2,
                b0 ~ dnorm(0,10) ,
                b1 ~ dnorm(0,10) ,
                b2 ~ dnorm(0,10)
)

Modell_1a <- map2stan(flist,data=Upg1_data)

#Konvergens analys
#n_eff, Rhat, Tracerplot

precis(Modell_1a, prob = 0.909)

tracerplot(Modell_1a)

PostSamp_1a <- extract.samples(Modell_1a)

#Konvergens för parameters posteriormean b0,b1,b2
par(mfrow=c(2,2))
#b0
AnvPar <- PostSamp_1a$b0
NIter <- length(PostSamp_1a$b0)
Means <-  matrix(0,nrow=NIter,ncol=1,byrow=TRUE)
for (iter in 1:NIter){
  Means[iter] <- mean(AnvPar[1:iter])
}
plot(Means, main = "Konvergens för b0 posteriormean")

#b1
AnvPar <- PostSamp_1a$b1
NIter <- length(PostSamp_1a$b1)
Means <-  matrix(0,nrow=NIter,ncol=1,byrow=TRUE)
for (iter in 1:NIter){
  Means[iter] <- mean(AnvPar[1:iter])
}
plot(Means, main = "Konvergens för b1 posteriormean")

#b2
AnvPar <- PostSamp_1a$b2
NIter <- length(PostSamp_1a$b2)
Means <-  matrix(0,nrow=NIter,ncol=1,byrow=TRUE)
for (iter in 1:NIter){
  Means[iter] <- mean(AnvPar[1:iter])
}
plot(Means, main = "Konvergens för b2 posteriormean")


#### Binomial regression ska anpassas och data måste göras om
#### Verifiera att den logistiska och binomiala ger samma resultat
## b)

#Gör om data för att kunna anpassa binomial modell

binom_df <- data.frame("y" = rep(0,times=4),"x1" = c(0,1,0,1), "x2" = c(0,0,1,1),"Unsuccesfull" = rep(0,times=4))

#Antal lyckade utfall av y
binom_df[1,1] <- sum(Upg1_data[Upg1_data$x1 == 0 & Upg1_data$x2 == 0,1])
binom_df[2,1] <- sum(Upg1_data[Upg1_data$x1 == 1 & Upg1_data$x2 == 0,1])
binom_df[3,1] <- sum(Upg1_data[Upg1_data$x1 == 0 & Upg1_data$x2 == 1,1])
binom_df[4,1] <- sum(Upg1_data[Upg1_data$x1 == 1 & Upg1_data$x2 == 1,1])
#Antal (n) inom dom fyra grupperna
binom_df[,4] <- aggregate(x = Upg1_data$y, 
                 by = list(x1 = Upg1_data$x1, x2 = Upg1_data$x2),
                 FUN = length)[,3]

#Anpassa binomial modell
flist <- alist( y ~ dbinom(Unsuccesfull,p) ,
                         logit(p) <- b0 + b1*x1 + b2*x2,
                         b0 ~ dnorm(0,10) ,
                         b1 ~ dnorm(0,10) ,
                         b2 ~ dnorm(0,10)
)
Modell_binom <- map2stan(flist,data=binom_df)

#Jämförelse av binom och logit, samma resultat, olika struktur av data
precis(Modell_binom,prob=0.909)
precis(Modell_1a,prob=0.909)

#### Utvärdera hur bra modellen i (a) anpassar sannolikheten p för att ett barn är vaccinerat mot sjukdomen
#### enligt följande: beäkna 95.2 procent kredibilitetsintervall för p för varje kombination av värden på x1 och x2
#### dvs 4 kredibilitetsintervall och jämför respektive kredibilitetsintervall med motsvarande andel barn i data som är
#### vaccinerade mot sjukdomen. Visa en figur för jämförelsen och kommentera hura bra modellen anpassar data. 
## c)

#Gör om posteriorn till sannolikhet och gör kredibilitetsintervall

#x1 = 0, x2 = 0
p.x10.x20 <- as.vector(logistic( PostSamp_1a$b0 ))
precis(p.x10.x20, prob = 0.952)
#'Sanna' sannolikheten i data
binom_df[1,1]/binom_df[1,4]

#x1 = 1, x2 = 0
p.x11.x20 <- as.vector(logistic( PostSamp_1a$b0 + PostSamp_1a$b1 ))
precis(p.x10.x21, prob = 0.952)
binom_df[2,1]/binom_df[2,4]

#x1 = 0, x2 = 1
p.x10.x21 <- as.vector(logistic( PostSamp_1a$b0 + PostSamp_1a$b2 ))
precis(p.x10.x21, prob = 0.952)
binom_df[3,1]/binom_df[3,4]

#x1 = 1, x2 = 1
p.x11.x21 <- as.vector(logistic( PostSamp_1a$b0 + PostSamp_1a$b1 + PostSamp_1a$b2 ))
precis(p.x11.x21, prob = 0.952)
binom_df[4,1]/binom_df[4,4]

#Visualisera
plot(x = 1:4,y = c(0.38,0.6,0.26,0.46), type = "p", ylim = c(0.2,0.7),ylab="",xlab="") 
lines(x = c(1,1),y = c(0.32,0.44))
lines(x = c(2,2),y = c(0.56,0.64))
lines(x = c(3,3),y = c(0.22,0.29))
lines(x = c(4,4),y = c(0.43,0.48))
points(x = 1:4,y = c(0.39,0.6,0.25,0.46), col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5), pch=19)


precis(Modell_binom)
precis(Modell_1a)

#### Posteriorfördelningen för den faktor som oddset för ett vaccinerat barn
#### förändras med då man går frn ett barn under 2år till ett barn på minst 2år,
#### dvs från x1 = 0 till x1 = 1
## d)

#Plottar förändringen i odds då x1 går från x1 = 0 till x1 = 1

ExpChangeOdds <- exp(PostSamp_1a$b1)
dens(ExpChangeOdds)
mean(ExpChangeOdds)

#### Används informationskriteriet DIC för att avgöra om det är bättre att lägga till
#### förklaringsvariablerna x3 och x4 till modellen i (a). Avgör också om posteriorfödelingarna 
#### för parametrarna till x1 och x2 förändras nämnvärt av att 
#### x3 och x4 läggs till i modellen. 
## e)

#Anpassar logistisk regressionsmodell med fyra förklaringsvariabler

Upg1e_data <- Vaccination_Barn[,1:5]

flist <- alist( y ~ dbinom( 1 , p ) ,
                logit(p) <- b0 + b1*x1 + b2*x2 + b3*x3 + b4*x4,
                b0 ~ dnorm(0,10) ,
                b1 ~ dnorm(0,10) ,
                b2 ~ dnorm(0,10) ,
                b3 ~ dnorm(0,10) ,
                b4 ~ dnorm(0,10)
)

Modell_1e <- map2stan(flist,data=Upg1e_data)

#Jämför DIC mellan de två modellerna
DIC(Modell_1a)[1]
DIC(Modell_1e)[1]

#Kollar confounding effekter
plot( coeftab(Modell_1a,Modell_1e),prob=0.909 )


#### En bayesiansk logistisk regression med y som responsvariabel och med
#### alla förklaringsvariabler x1 till x5 ska användas. Mha DIC och WAIC
#### vilken av modellerna i a,e och f som är bäst och vad modellen är bäst på.
#### Sedan ska ett 95.2 procent kredibilitetsintervall för sannolikheten p beräknas
#### att ett barn är vaccinerat mot sjukdomen som funktion av förklaringsvariablen x5 genom
#### att använda typvärdet för respektive x1-x4 i datamaterialet. 
## f)

#Anpassar logistisk regressionsmodell med alla variabler

Upg1f_data <- Vaccination_Barn[,1:6]

flist <- alist( y ~ dbinom( 1 , p ) ,
                logit(p) <- b0 + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5,
                b0 ~ dnorm(0,10) ,
                b1 ~ dnorm(0,10) ,
                b2 ~ dnorm(0,10) ,
                b3 ~ dnorm(0,10) ,
                b4 ~ dnorm(0,10) ,
                b5 ~ dnorm(0,10)
)

Modell_1f <- map2stan(flist,data=Upg1f_data)

#jämför modeller
#DIC
DIC(Modell_1a)[1]
DIC(Modell_1e)[1]
DIC(Modell_1f)[1]
#WAIC
WAIC(Modell_1a)[1]
WAIC(Modell_1e)[1]
WAIC(Modell_1f)[1]
#Confounding check
plot( coeftab(Modell_1a,Modell_1e,Modell_1f),prob=0.909 )
#Kredbilitetsband givet typvärdet av x1-x4

#Ta fram typvärdet för x1-x4
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
modex1 <- getmode(Upg1f_data$x1)
modex2 <- getmode(Upg1f_data$x2)
modex3 <- getmode(Upg1f_data$x3)
modex4 <- getmode(Upg1f_data$x4)

#Skapa plot
plot( Upg1f_data$y ~ Upg1f_data$x5 ,ylim=c(0,1.5),type="n" )

#Ta sampels från modellen
PostSamp_1f <- extract.samples(Modell_1f)

#Skapa grid av värden från min av x5 till max av x5, eftersom det är x5 som ska vara som funk av y
x5.seq <- seq(from=min(Upg1f_data$x5),to=max(Upg1f_data$x5),by=0.01)
#Beräkna bandet för y som funktion av x5 givet typvärdet på x1-x4
#Notera Logistic eftersom parametrarna är uttryckta i logodds, vill ha som probability
mu.ci <- sapply( x5.seq , function(x) 
  PI( logistic( PostSamp_1f$b0 + PostSamp_1f$b1*modex1 + PostSamp_1f$b2*modex2 +
             PostSamp_1f$b3*modex3 + PostSamp_1f$b4*modex4 + PostSamp_1f$b5*x) , prob=0.952) )
shade( mu.ci , x5.seq )

#### Uppgift 2 ####

#### Tag hänsyn till skillnader i sannolikhet för vaccination utifrån vilket samhälle som barnet tillhör
#### En logistisk modell med olika intercept för de olika samhällena z1 ska därför anpassas
#### Tar denna modell hänsyn till pooling av information från data mellan samhällena för att 
#### skatta intercepten? Undersök om MCMC dragningarna lett till att MCMC algoritmen konvergerat till
#### posteriorn med hjälp av n_eff och Rhat. Jämför med samma modell utan olika intecept
#### i uppgift 1e

## a)

Upg2_data <- Vaccination_Barn[,c(1:5,7)]
str(Upg2_data)
#Anpassar logistisk modell med olika intercept baserat på z1
#Här har varje intercept en egen priorfördelning, Således är det endast data
#från respektive grupp som anpassar interceptet, ingne poolning mellan grupperna

flist <- alist( y ~ dbinom( 1 , p ) ,
                logit(p) <- b0[z1] + b1*x1 + b2*x2 + b3*x3 + b4*x4,
                b0[z1] ~ dnorm(0,10) ,
                b1 ~ dnorm(0,10) ,
                b2 ~ dnorm(0,10) ,
                b3 ~ dnorm(0,10) ,
                b4 ~ dnorm(0,10) 
)

Modell_2 <- map2stan(flist,data=Upg2_data,iter = 20000)

precis(Modell_2, depth=2)

DIC(Modell_2)[1]
DIC(Modell_1e)[1]
WAIC(Modell_2)[1]
WAIC(Modell_1e)[1]

#### Tag återigen hänsyn till skillnader i sannolikhet för vaccination utifrån vilket samhälle som
#### som barnet tillhör. Anpassa nu en multilevel logistisk modell med olika intercept.
#### Vad är skillnaden på denna modell och den i uppgift 2a?

## b)

#Anpassar multilevel modell
#Här poolas information mellan grupperna eftersom intercepten har en gemensam fördelning.
#detta genom att alpha_j har parametrarna alpha och sigma_alpha som har egna priors.
#På detta sätt kan information från grupper med många observationer poola information
#till de grupper med få observationer och få en bättre helhets anpassning.

flist <- alist( y ~ dbinom(1,p) ,
                    logit(p) <- a[z1] + b1*x1 + b2*x2 + b3*x3 + b4*x4,
                    a[z1] ~ dnorm(a0,sigma_a) ,
                    a0 ~ dnorm(0,10) ,
                    sigma_a ~ dcauchy(0,10) ,
                    b1 ~ dnorm(0,10) ,
                    b2 ~ dnorm(0,10) ,
                    b3 ~ dnorm(0,10) ,
                    b4 ~ dnorm(0,10) 
)

Modell_2b <- map2stan(flist,data=Upg2_data, iter = 10000)

precis(Modell_2b,prob=0.909, depth = 2)

DIC(Modell_1e)[1]
DIC(Modell_2)[1]
DIC(Modell_2b)[1]
WAIC(Modell_1e)[1]
WAIC(Modell_2)[1]
WAIC(Modell_2b)[1]

#### Uppgift 3 ####

#### Används en icke-informativ konjugerad prior för medelvärdet theta och redovisa
#### parametervärdena för priorn. Simulera sedan fram 10000 värden från posteriorn
#### med hjälp av kvadratisk approximation. Beräkna posteriorfördelningensmedelvärde
#### och standardavvikelse. Stämmer dessa öerens med värdena från föreläsningen på moment 1?

## a)

bombman <- data.frame(pois = c(rep(0, 229), rep(1, 211), rep(2, 93), rep(3, 35), rep(4, 7), rep(5, 1)))

#Konjugerade prior ska användas för poissonfördelningen eftersom data är pois fördelat.
#Gamma(alpha,beta) konjugerade prior för poisfördelning
#Icke-informativ prior -> Gamma(1 + sumxi, 1 + n)

#Anpassar kvadratisk approximation med den konjugerade priorn icke informativ
flist <- alist( pois ~ dpois(lambda) ,
                           lambda <- dgamma(1,1)
)
resBomb <- map(flist, data=bombman)

precis(resBomb)

bombSamples <- extract.samples(resBomb, n = 1e4)

#Beräkna posteriorns medelvärde och standardavvikelse

mean(bombSamples$lambda)
sd(bombSamples$lambda)

#### Använd modellen för Poisson regression från föreläsningen på moment 4, men utan 
#### förklaringsvariabler. Anapssa modellen med map2stan, beräkna följande
#### pospteriorsannolikhet för medelvärde lambda: P(lambda>1|y)

## b)

#Använder normalfördelad prior istället för konjugerad gamma prior.
#Således måste log av lambda användas för att inte negativ värden ska kunna antas.

flist <- alist( pois ~ dpois(lambda) ,
                           log(lambda) <- h,
                            h ~ dnorm(0,10) 
)

resBomb_2b <- map2stan(flist, bombman)

precis(resBomb_2b)
bombSamples_2b <- extract.samples(resBomb_2b,n=1e3)

#Beräkna sannolikhten P(Lambda > 1 | y)
sum(exp(bombSamples_2b$h) > 1)/length(bombSamples_2b$h)
 

