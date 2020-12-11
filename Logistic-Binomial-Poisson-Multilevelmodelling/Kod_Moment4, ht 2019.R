### Bayesiansk statistik, 732g34, moment 4 ###
### Kod från Statistical Rethinking, Richard McElreath ###
### Kod från Bertil Wegmann ###
library(rethinking)

# ladda eBaydata
load("C:/Users/berwe48/Dropbox/Teaching/Statistisk analys av komplexa data/eBaydata.Rda")

# data frame
y <- data$UnOpen
x1 <- data$PowerSeller
x2 <- data$LogBookValue
eBay <- data.frame(y,x1,x2)

#R kod 4.1

# Skatta modell utan förklaringsvariabler med map2stan
Utan <- alist( y ~ dbinom( 1 , p ) ,
                           logit(p) <- b0 ,
                           b0 ~ dnorm(0,10)
                           )

Modell_utan <- map2stan(Utan,data=eBay)
precis(Modell_utan,prob=0.909)
logistic(c(-2.28,-1.93)) # funktionen logistic konverterar från log-odds till sannolikhet, ca värden.

# Skatta modell med förklaringsvariabel x1 = PowerSeller med map2stan
Modx1 <- alist( y ~ dbinom( 1 , p ) ,
               logit(p) <- b0 + b1*x1,
               b0 ~ dnorm(0,10) ,
               b1 ~ dnorm(0,10)
)

Modell_x1 <- map2stan(Modx1,data=eBay)
precis(Modell_x1,prob=0.909)

# Skatta modell med förklaringsvariabel x2 = LogBookValue med map2stan
Modx2 <- alist( y ~ dbinom( 1 , p ) ,
                logit(p) <- b0 + b1*x2,
                b0 ~ dnorm(0,10) ,
                b1 ~ dnorm(0,10)
)

Modell_x2 <- map2stan(Modx2,data=eBay)
precis(Modell_x2,prob=0.909)

# Skatta modell med förklaringsvariabler x1 = Powerseller, x2 = LogBookValue, med map2stan
Modx1x2 <- alist( y ~ dbinom( 1 , p ) ,
                logit(p) <- b0 + b1*x1 + b2*x2,
                b0 ~ dnorm(0,10) ,
                b1 ~ dnorm(0,10) ,
                b2 ~ dnorm(0,10)
)

Modell_x1x2 <- map2stan(Modx1x2,data=eBay)
precis(Modell_x1x2,prob=0.909)

# Jämför modellerna med Akaikevikter
Compare_Modeller <- compare(Modell_utan,Modell_x1,Modell_x2,Modell_x1x2,n=1e4)
Compare_Modeller
plot( coeftab(Modell_utan,Modell_x1,Modell_x2,Modell_x1x2),prob=0.909 )

# Undersök resultat för modellen Modell_x1x2
plot(Modell_x1x2)
pairs(Modell_x1x2)

Samples <- extract.samples(Modell_x1x2)
# Förändring i odds från förändring i PowerSeller från 0 till 1.
ExpChangeOdds <- exp(Samples$b1)
dens(ExpChangeOdds)

# Förändring i P(Y=1) från Powerseller=0 till Powerseller=1, givet mean(x2)
LinPred1 <- Samples$b0 + Samples$b1*0 + Samples$b2*mean(x2)
LinPred2 <- Samples$b0 + Samples$b1*1 + Samples$b2*mean(x2)
ProbChange2 <- logistic(LinPred2)-logistic(LinPred1)
ProbDiffOver0 <- sum(ProbChange2>0)/length(ProbChange2) # sannolikhet att skillnad > 0
ProbDiffOver0
quantile(ProbChange2,c(0.0455,0.9545)) # 90.9 % kredibilitetsintervall för skillnad

SeqProbs <- c(logistic(LinPred1),logistic(LinPred2),ProbChange2)
xlim <- c(min(SeqProbs),max(SeqProbs))
# Plotta sannolikheter för P(Y=1) och skillnaden i P(Y=1)
dens(logistic(LinPred1),type="l",xlim=xlim,col="green")
dens(logistic(LinPred2),type="l",col="blue",add=TRUE)
dens(ProbChange2,type="l",col="red",add=TRUE)

# Jämför med andelen UnOpen=1 då Powerseller=0 och Powerseller=1
Index <- x1==1
PropUnOpen_Powerseller0 <- y[Index==0]
mean(PropUnOpen_Powerseller0)
PropUnOpen_Powerseller1 <- y[Index==1]
mean(PropUnOpen_Powerseller1)


#R kod 4.2

### Binomialregression - school admissions ###
data(UCBadmit)
d <- UCBadmit
d

y <- d$admit
Applications <- d$applications
x1 <- ifelse( d$applicant.gender=="male" , 1 , 0 ) # skapa dummyvariabeln
Admits <- data.frame(y,Applications,x1)
Mod_NoDummy_list <- alist( y ~ dbinom(Applications,p) ,
                           logit(p) <- b0,
                           b0 ~ dnorm(0,10)
                           )
Mod_NoDummy <- map2stan(Mod_NoDummy_list,data=Admits)
precis(Mod_NoDummy,prob=0.909)

Mod_Dummy_list <- alist( y ~ dbinom(Applications,p) ,
                           logit(p) <- b0 + b1*x1,
                           b0 ~ dnorm(0,10) ,
                           b1 ~ dnorm(0,10)
)
Mod_Dummy <- map2stan(Mod_Dummy_list,data=Admits)
precis(Mod_Dummy,prob=0.909)

compare(Mod_NoDummy,Mod_Dummy)

# Fördelning för skillnaden i sannolikheten för beviljad ansökan mellan könen
PostSamp <- extract.samples(Mod_Dummy)
p.admit.male <- logistic( PostSamp$b0 + PostSamp$b1 )
p.admit.female <- logistic( PostSamp$b0 )
diff.admit <- p.admit.male - p.admit.female
dens(diff.admit)

# Posterior prediktioner från modellen
postcheck(Mod_Dummy,n=1000,prob=0.909)
# draw lines connecting points from same dept
for ( i in 1:6 ) {
  x <- 1 + 2*(i-1)
  y1 <- y[x]/Applications[x]
  y2 <- y[x+1]/Applications[x+1]
  lines( c(x,x+1) , c(y1,y2) , col=rangi2 , lwd=2 )
  text( x+0.5 , (y1+y2)/2 + 0.05 , d$dept[x] , cex=0.8 , col=rangi2 )
}


### Bayesiansk binomialregression med olika intercept ###

# gör index
deptIndex <- coerce_index( d$dept )
deptIndex
Admits$inst <- deptIndex

# Modeller med unika intercept för varje institution
NoDummy_list <- alist( y ~ dbinom(Applications,p) ,
                           logit(p) <- b0[inst],
                           b0[inst] ~ dnorm(0,10)
)
Mod_NoDummy_Intercepts <- map2stan(NoDummy_list,data=Admits)
precis(Mod_NoDummy_Intercepts,depth=2,prob=0.909)

Dummy_list <- alist( y ~ dbinom(Applications,p) ,
                         logit(p) <- b0[inst] + b1*x1,
                         b0[inst] ~ dnorm(0,10) ,
                         b1 ~ dnorm(0,10)
)
Mod_Dummy_Intercepts <- map2stan(Dummy_list,data=Admits)
precis(Mod_Dummy_Intercepts,depth=2,prob=0.909)

PostSamp <- extract.samples(Mod_Dummy_Intercepts)
# förändring i odds för beviljad ansökan 
# från förändring i dummyvariabeln för män från 0 till 1.
ExpChangeOdds <- exp(PostSamp$b1)
dens(ExpChangeOdds)

# Jämför dom 4 modellerna med Akaikekriteriet för WAIC
compare(Mod_NoDummy,Mod_Dummy,Mod_NoDummy_Intercepts,Mod_Dummy_Intercepts)

# Plott med univariata och parvisa posteriorfördelningar
pairs(Mod_Dummy_Intercepts)

# Posterior prediktioner från modellen med olika intercept
postcheck(Mod_Dummy_Intercepts,n=1000,prob=0.909)
# draw lines connecting points from same dept
for ( i in 1:6 ) {
  x <- 1 + 2*(i-1)
  y1 <- y[x]/Applications[x]
  y2 <- y[x+1]/Applications[x+1]
  lines( c(x,x+1) , c(y1,y2) , col=rangi2 , lwd=2 )
  text( x+0.5 , (y1+y2)/2 + 0.05 , d$dept[x] , cex=0.8 , col=rangi2 )
}


#R kod 4.3

### Poisson regression, data exempel från UCLA ###
p <- read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")
p <- within(p, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", 
                                            "Vocational"))
  id <- factor(id)
})
summary(p)

# Gör index för olika program
program <- coerce_index(p$prog)
program
mathtest <- as.numeric(0.01*p$math)
nawards <- as.integer(p$num_awards)

Awards <- data.frame(nawards,program,mathtest)

# Modeller med unika intercept för varje program
math_list <- alist( nawards ~ dpois(lambda) ,
                       log(lambda) <- b0[program] + b1*mathtest,
                       b0[program] ~ dnorm(0,10) ,
                       b1 ~ dnorm(0,10)
)
Pois_math_Intercepts <- map2stan(math_list,data=Awards)
precis(Pois_math_Intercepts,depth=2,prob=0.909)

no_math_list <- alist( nawards ~ dpois(lambda) ,
                     log(lambda) <- b0[program],
                     b0[program] ~ dnorm(0,10)
)
Pois_no_math_Intercepts <- map2stan(no_math_list,data=Awards)
precis(Pois_no_math_Intercepts,depth=2,prob=0.909)

Awards_noprog <- data.frame(nawards,mathtest)

# Modeller utan unika intercept för varje program
math_list_noprog <- alist( nawards ~ dpois(lambda) ,
                    log(lambda) <- b0 + b1*mathtest,
                    b0 ~ dnorm(0,10) ,
                    b1 ~ dnorm(0,10)
)
Pois_math <- map2stan(math_list_noprog,data=Awards_noprog)
precis(Pois_math,prob=0.909)

no_math_noprog <- alist( nawards ~ dpois(lambda) ,
                       log(lambda) <- b0,
                       b0 ~ dnorm(0,10)
)
Pois_no_math <- map2stan(no_math_noprog,data=Awards_noprog)
precis(Pois_no_math,prob=0.909)

# Jämför dom 4 modellerna med Akaikekriteriet för WAIC
compare(Pois_math_Intercepts,Pois_no_math_Intercepts,Pois_math,Pois_no_math)

# Jämför förväntat antalet awards mot observerat antal för olika värden på math
Math.seq <- seq(from=min(mathtest),to=max(mathtest),by=0.01)

plot( nawards ~ mathtest ,ylim=c(0,10) )

PostSamp <- extract.samples(Pois_math)
# loop over samples and plot each lambda value
for ( i in 1:1e3 ){
  lambda <- exp( PostSamp$b0[i] + PostSamp$b1[i]*Math.seq )
  points( Math.seq , lambda , pch=16 , col=col.alpha(rangi2,0.1) )
}

# 90.9 % Kredibilitetsintervall för lambda för olika värden på math
mu.ci <- sapply( Math.seq , function(x) 
  PI( exp( PostSamp$b0 + PostSamp$b1*x ) , prob=0.909) )
shade( mu.ci , Math.seq )

# 90.9 % prediktionsintervall för antal awards för olika värden på math
sim.y <- matrix(0,nrow=1e3,ncol=length(Math.seq))
for ( i in 1:length(Math.seq) ){
  lambda <- exp( PostSamp$b0 + PostSamp$b1*Math.seq[i] )
  sim.y[,i] <- rpois(1e3,lambda)
}
y.PI <- apply( sim.y , 2 , PI , prob=0.909 )
shade( y.PI , Math.seq )



#### Multilevel modeller ####

# Skatta multilevel modell med intelligens som responsvariabel, och
# en varierande-intercept modell för olika skolklasser i Örebro.

### OBS!!! Data finns ej att tillgå, endast exempel på kod ###

#R kod 4.4

# Hämta data
IQdata <- read.table("C:/Users/MyTable.csv", header=TRUE, sep=";",dec=",")

# Skapa variabler
n <- nrow(IQdata)
y <- IQdata$Intelligence
x1 <- IQdata$Antalsyskon_aldre
x2 <- IQdata$Antalsyskon_yngre
x3 <- IQdata$Sex
x4 <- IQdata$Socioecstat
x5 <- IQdata$MorAlder
x6 <- IQdata$FarAlder

Grupp <- as.integer(IQdata$Skolklass)

IQ_datafr <- data.frame(y,x1,x2,x3,x4,x5,x6,Grupp)

Modell_IQ <- alist( y ~ dnorm(mu,sigma) ,
                           mu <- a[Grupp] + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5 + b6*x6,
                           a[Grupp] ~ dnorm(a0,sigma_a) ,
                           a0 ~ dnorm(0,10) ,
                           sigma_a ~ dcauchy(0,1) ,
                           b1 ~ dnorm(0,10) ,
                           b2 ~ dnorm(0,10) ,
                           b3 ~ dnorm(0,10) ,
                           b4 ~ dnorm(0,10) ,
                           b5 ~ dnorm(0,10) ,
                           b6 ~ dnorm(0,10) ,
                           sigma ~ dcauchy(0,1)
)
IQres <- map2stan(Modell_IQ,data=IQ_datafr)
precis(IQres,prob=0.909)


# Skatta multilevel modell med familjestorlek (dummyvariabel) som responsvariabel, och 
# en varierande-intercept modell för olika skolklasser i Örebro.

### OBS!!! Data finns ej att tillgå, endast exempel på kod ###

#R kod 4.5

AntBarn <- x1+x2+1

y <- ifelse( AntBarn > 3 , 1 , 0 ) # skapa dummyvariabeln

FamiljData <- data.frame(y,x1,x2,x3)

Modell_Fam <- alist( y ~ dbinom(1,p) ,
                     logit(p) <- a[Grupp] + b1*x1 + b2*x2 + b3*x3,
                     a[Grupp] ~ dnorm(a0,sigma_a) ,
                     a0 ~ dnorm(0,10) ,
                     sigma_a ~ dcauchy(0,1) ,
                     b1 ~ dnorm(0,10) ,
                     b2 ~ dnorm(0,10) ,
                     b3 ~ dnorm(0,10)
)
Fam.res <- map2stan(Modell_Fam,data=FamiljData)
precis(Fam.res,prob=0.909)


#R kod 4.6

### Jämför med Bayesiansk multilevel varierande-intercept Poisson regression ###

### Poisson regression, data exempel från UCLA ###
p <- read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")
p <- within(p, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", 
                                            "Vocational"))
  id <- factor(id)
})
summary(p)

# Gör index för olika program
program <- coerce_index(p$prog)
program
mathtest <- as.numeric(0.01*p$math)
nawards <- as.integer(p$num_awards)

Awards <- data.frame(nawards,program,mathtest)

# Bayesiansk Poisson regression utan unika intercept för varje program
math_list_noprog <- alist( nawards ~ dpois(lambda) ,
                           log(lambda) <- b0 + b1*mathtest,
                           b0 ~ dnorm(0,10) ,
                           b1 ~ dnorm(0,10)
)
Pois_math <- map2stan(math_list_noprog,data=Awards_noprog)
precis(Pois_math,prob=0.909)

# Bayesiansk Poisson regression med unika intercept för varje program
math_list <- alist( nawards ~ dpois(lambda) ,
                    log(lambda) <- b0[program] + b1*mathtest,
                    b0[program] ~ dnorm(0,10) ,
                    b1 ~ dnorm(0,10)
)
Pois_math_Intercepts <- map2stan(math_list,data=Awards)
precis(Pois_math_Intercepts,depth=2,prob=0.909)

# Bayesiansk multilevel varierande-intercept Poisson regression
math_list_mult <- alist( nawards ~ dpois(lambda) ,
                    log(lambda) <- a[program] + b1*mathtest,
                    a[program] ~ dnorm(mu,sigma) ,
                    mu <- dnorm(0,10) ,
                    sigma <- dcauchy(0,1) ,
                    b1 ~ dnorm(0,10)
)
Pois_math_Intercepts_mult <- map2stan(math_list_mult,data=Awards,chains=3,iter=4000)
precis(Pois_math_Intercepts_mult,depth=2,prob=0.909)

# Bayesiansk multilevel varierande-intercept Poisson regression utan förklaringsvariabler
math_list_mult_utan <- alist( nawards ~ dpois(lambda) ,
                         log(lambda) <- a[program],
                         a[program] ~ dnorm(mu,sigma) ,
                         mu <- dnorm(0,10) ,
                         sigma <- dcauchy(0,1) ,
                         b1 ~ dnorm(0,10)
)
Pois_math_Intercepts_mult_utan <- map2stan(math_list_mult_utan,data=Awards,chains=3,iter=4000)
precis(Pois_math_Intercepts_mult_utan,depth=2,prob=0.909)

# Jämför modellernas prediktiva förmåga
compare(Pois_math_Intercepts_mult,Pois_math_Intercepts,Pois_math)










