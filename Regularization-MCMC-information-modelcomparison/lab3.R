require(rethinking)

load("C:/Users/Jakob/Desktop/Bayesian statistics/lab2/Data_Moment2.RData")

X <- as.data.frame(X)

set.seed(970922);y <- rnorm(n,A,B)

data <- data.frame(y/1000,scale(X[,1:4]),X[,5:6])
colnames(data) <- c("y","area","antal_rum","avgift","trappor","cityyes","sydyes")


#### Uppgift 1 ####

# --- --- a)

#Null modell
flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu ~ dnorm( 3 , 10 ) , # normalf?rdelad prior
  logsigma ~ dnorm ( 0 , 1 )
)

resNormal_Null <- map(flist, data=data)

precis(resNormal_Null)

#En variabel
flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu <- b0 + b1*area , # hur mu ?r l?nkad till f?rklaringsvariabler.
  b0 ~ dnorm( 3 , 10 ) , # normalf?rdelad prior f?r intercept
  b1 ~ dnorm( 0 , 10 ) ,
  logsigma ~ dnorm ( 0, 1 )
)

resNormal_EnXvar <- map(flist, data=data)

precis(resNormal_EnXvar)

#Full modell
flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu <- b0 + b1*area + b2*antal_rum + b3*avgift + b4*trappor + b5*cityyes + b6*sydyes, # hur mu ?r l?nkad till f?rklaringsvariabler.
  b0 ~ dnorm( 3 , 10 ) , # normalf?rdelad prior f?r intercept
  c(b1,b2,b3,b4) ~ dnorm( 0 , 10 ) ,
  c(b5,b6) ~ dnorm( 0 , 10 ) ,
  logsigma ~ dnorm ( 0, 1 )
)

resNormal_Full <- map(flist, data=data)

precis(resNormal_Full)


# --- --- b)

### Beräkna deviance på respektive modell för hela datamaterialet.
### In sample deviance eftersom hela datamaterialet används

# Deviance Null model

theta_Null <- coef(resNormal_Null)
dev_Null <- (-2)*sum( dnorm(
  data$y ,
  mean=theta_Null[1] ,
  sd=exp(theta_Null[2]) ,
  log=TRUE ) )
dev_Null

# Deviance EnXvar

theta_EnXvar <- coef(resNormal_EnXvar)
dev_EnXvar <- (-2)*sum( dnorm(
  data$y ,
  mean=theta_EnXvar[1] + theta_EnXvar[2]*data$area,
  sd=exp(theta_EnXvar[3]) ,
  log=TRUE ) )
dev_EnXvar

# Deviance Full
theta_Full <- coef(resNormal_Full)
dev_Full <- (-2)*sum( dnorm(
  data$y ,
  mean=theta_Full[1] + theta_Full[2]*data$area + theta_Full[3]*data$antal_rum + 
    theta_Full[4]*data$avgift + theta_Full[5]*data$trappor + 
    theta_Full[6]*data$cityyes + theta_Full[7]*data$sydyes,
  sd=exp(theta_Full[8]) ,
  log=TRUE ) )
dev_Full

#
dev_Null
dev_EnXvar
dev_Full
#
precis(resNormal_Full)

### Delar upp i träning respektive valideringsmängd för att beräkna
### out of sample deviance
### Modellen behöver skattas om på nya training sample

# --- --- c)

train_data <- data[1:29,]
vali_data <- data[30:57,]

#Null modell traindata
flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu ~ dnorm( 3 , 10 ) , # normalf?rdelad prior
  logsigma ~ dnorm ( 0 , 1 )
)

resNormal_Null_train <- map(flist, data=train_data)

precis(resNormal_Null)

#En variabel
flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu <- b0 + b1*area , # hur mu ?r l?nkad till f?rklaringsvariabler.
  b0 ~ dnorm( 3 , 10 ) , # normalf?rdelad prior f?r intercept
  b1 ~ dnorm( 0 , 10 ) ,
  logsigma ~ dnorm ( 0, 1 )
)

resNormal_EnXvar_train <- map(flist, data=train_data)

precis(resNormal_EnXvar)

#Full modell
flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu <- b0 + b1*area + b2*antal_rum + b3*avgift + b4*trappor + b5*cityyes + b6*sydyes, # hur mu ?r l?nkad till f?rklaringsvariabler.
  b0 ~ dnorm( 3 , 10 ) , # normalf?rdelad prior f?r intercept
  c(b1,b2,b3,b4) ~ dnorm( 0 , 10 ) ,
  c(b5,b6) ~ dnorm( 0 , 10 ) ,
  logsigma ~ dnorm ( 0, 1 )
)

resNormal_Full_train <- map(flist, data=train_data)

# Train deviance
################

theta_Null <- coef(resNormal_Null_train)
dev_Null_Train <- (-2)*sum( dnorm(
  train_data$y ,
  mean=theta_Null[1] ,
  sd=exp(theta_Null[2]) ,
  log=TRUE ) )
dev_Null_Train

# Deviance EnXvar

theta_EnXvar <- coef(resNormal_EnXvar_train)
dev_EnXvar_Train <- (-2)*sum( dnorm(
  train_data$y ,
  mean=theta_EnXvar[1] + theta_EnXvar[2]*train_data$area,
  sd=exp(theta_EnXvar[3]) ,
  log=TRUE ) )
dev_EnXvar_Train

# Deviance Full
theta_Full <- coef(resNormal_Full_train)
dev_Full_Train <- (-2)*sum( dnorm(
  train_data$y ,
  mean=theta_Full[1] + theta_Full[2]*train_data$area + theta_Full[3]*train_data$antal_rum + 
    theta_Full[4]*train_data$avgift + theta_Full[5]*train_data$trappor + 
    theta_Full[6]*train_data$cityyes + theta_Full[7]*train_data$sydyes,
  sd=exp(theta_Full[8]) ,
  log=TRUE ) )
dev_Full_Train


# Validation deviance
################

theta_Null <- coef(resNormal_Null_train)
dev_Null_Vali <- (-2)*sum( dnorm(
  vali_data$y ,
  mean=theta_Null[1] ,
  sd=exp(theta_Null[2]) ,
  log=TRUE ) )
dev_Null_Vali

# Deviance EnXvar

theta_EnXvar <- coef(resNormal_EnXvar_train)
dev_EnXvar_Vali <- (-2)*sum( dnorm(
  vali_data$y ,
  mean=theta_EnXvar[1] + theta_EnXvar[2]*vali_data$area,
  sd=exp(theta_EnXvar[3]) ,
  log=TRUE ) )
dev_EnXvar_Vali

# Deviance Full

theta_Full <- coef(resNormal_Full_train)
dev_Full_Vali <- (-2)*sum( dnorm(
  vali_data$y ,
  mean=theta_Full[1] + theta_Full[2]*vali_data$area + theta_Full[3]*vali_data$antal_rum + 
    theta_Full[4]*vali_data$avgift + theta_Full[5]*vali_data$trappor + 
    theta_Full[6]*vali_data$cityyes + theta_Full[7]*vali_data$sydyes,
  sd=exp(theta_Full[8]) ,
  log=TRUE ) )
dev_Full_Vali

### Ett förväntat resultat är att deviancen för träningsmängden alltid kommer sjunka då flera parametrar
### läggs till i modellen. För valideringsmängden förväntas dock deviance öka i genomsnitt då fler variabler
### läggs till. Vi kan i detta fall se att den fulla modellen inte överanpassar data eftersom den presterar
### bäst i out-of-sample deviance, det går dock att se tendensen av ökning i träningsmängdens deviance då
### minskningen av deviance från träning till validering minskar vid mer komplex modell.

#
dev_Null_Train
dev_Null_Vali
#
dev_EnXvar_Train
dev_EnXvar_Vali
#
dev_Full_Train
dev_Full_Vali


### AIC,DIC resptketive WAIC ska beräknas
### AIC är endast en bra approximation av modellens prediktiva
### förmåga om modellen har flacka priors, posteriorn är approxmativt multivariat normalfördelad och
### antalet observationer N är mycket större än antalet parametrar p.
###
### DIC är ett mer generellt mått än AIC eftersom det inte
### kräver flacka priors
###
### WAIC är ett mer
### generellt mått än både AIC och DIC för den inte 
### kräver något av antagandena som gäller för AIC och
# --- --- d)

## AIC

AIC_Null <- dev_Null + 2 * length(coef(resNormal_Null))
AIC_EnXvar <- dev_EnXvar + 2 * length(coef(resNormal_EnXvar))
AIC_Full <- dev_Full + 2 * length(coef(resNormal_Full))

## DIC
#Null modell

samples_Null <- extract.samples(resNormal_Null, n = 1e4)

deviance_posterior_NULL <- c(rep(NA,nrow(samples_Null)))
for (i in 1:nrow(samples_Null)) {
  deviance_posterior_NULL[i] <- (-2)*sum( dnorm(
    data$y ,
    mean=samples_Null[i,1] ,
    sd=exp(samples_Null[i,2]) ,
    log=TRUE ) )
}


D_bar_null <- mean(deviance_posterior_NULL)
D_hat_null <- (-2)*sum( dnorm(
  data$y ,
  mean=mean(samples_Null[,1]) ,
  sd=exp(mean(samples_Null[,2])) ,
  log=TRUE ) )
DIC_null <- D_bar_null + (D_bar_null - D_hat_null)
DIC(resNormal_Null)

#EnXVar Modell

samples_EnXvar <- extract.samples(resNormal_EnXvar, n = 1e4)

deviance_posterior_EnXvar <- c(rep(NA,nrow(samples_EnXvar)))
for (i in 1:nrow(samples_EnXvar)) {
  deviance_posterior_EnXvar[i] <- (-2)*sum( dnorm(
    data$y ,
    mean=samples_EnXvar[i,1] + samples_EnXvar[i,2]*data$area ,
    sd=exp(samples_EnXvar[i,3]) ,
    log=TRUE ) )
}

D_bar_EnXvar <- mean(deviance_posterior_EnXvar)
D_hat_EnXvar <- (-2)*sum( dnorm(
  data$y ,
  mean=mean(samples_EnXvar[,1]) + mean(samples_EnXvar[,2]) * data$area ,
  sd=exp(mean(samples_EnXvar[,3])) ,
  log=TRUE ) )
DIC_EnXvar <- D_bar_EnXvar + (D_bar_EnXvar - D_hat_EnXvar)
DIC(resNormal_EnXvar)

#Full Modell

samples_Full <- extract.samples(resNormal_Full, n = 1e4)

deviance_posterior_Full <- c(rep(NA,nrow(samples_Full)))
for (i in 1:nrow(samples_Full)) {
  deviance_posterior_Full[i] <- (-2)*sum( dnorm(
    data$y ,
    mean=samples_Full[i,1] +  samples_Full[i,2]*data$area +
      samples_Full[i,3]*data$antal_rum + samples_Full[i,4]*data$avgift +
      samples_Full[i,5]*data$trappor + samples_Full[i,6]*data$cityyes +
      samples_Full[i,7]*data$sydyes,
    sd=exp(samples_Full[i,8]) ,
    log=TRUE ) )
}

D_bar_Full <- mean(deviance_posterior_Full)
D_hat_Full <- (-2)*sum( dnorm(
  data$y ,
  mean=mean(samples_Full[,1]) + mean(samples_Full[,2])*data$area +
    mean(samples_Full[,3])*data$antal_rum + mean(samples_Full[,4])*data$avgift +
    mean(samples_Full[,5])*data$trappor + mean(samples_Full[,6])*data$cityyes +
    mean(samples_Full[,7])*data$sydyes,
  sd=exp(mean(samples_Full[,8])) ,
  log=TRUE ) )
DIC_Full <- D_bar_Full + (D_bar_Full - D_hat_Full)
DIC(resNormal_Full)

DIC_null
DIC_EnXvar
DIC_Full
#WAIC

WAIC_Null <- WAIC(resNormal_Null)[1]
WAIC_Null
WAIC_EnXvar <- WAIC(resNormal_EnXvar)[1]
WAIC_EnXvar
WAIC_Full <- WAIC(resNormal_Full)[1]
WAIC_Full

# --- --- e)

cor(data)
#i
flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu <- b0 + b1*antal_rum , # hur mu ?r l?nkad till f?rklaringsvariabler.
  b0 ~ dnorm( 3 , 10 ) , # normalf?rdelad prior f?r intercept
  b1 ~ dnorm( 0 , 10 ) ,
  logsigma ~ dnorm ( 0, 1 )
)

resNormal_EnXvar_2ndKorr <- map(flist, data=data)
#ii
resNormal_EnXvar
#iii
resNormal_Full

### Beräkna akaike vikter för hand, compare() görs detta autoomatiskt
### Aikake-vikterna tolkas som sannolikheten att den modellen är bäst

# --- --- f)
WAIC_EnXvar_2ndKorr  <- WAIC(resNormal_EnXvar_2ndKorr)[1]

WAIC_df <- data.frame("Modell" = c(WAIC_EnXvar ,WAIC_EnXvar_2ndKorr, WAIC_Full), "Weight" = c(NA,NA,NA))
rownames(WAIC_df)<-c("EnXvar","EnXvar_2ndkorr","FULL")

for(i in 1:nrow(WAIC_df)) {
  WAIC_df[i,2] <- exp(-0.5 * (WAIC_df[i,1] - min(WAIC_df[,1]))) / 
                  sum(exp(-0.5 * (WAIC_df[,1] - min(WAIC_df[,1]))))
}
precis(resNormal_EnXvar)
precis(resNormal_EnXvar_2ndKorr)
precis(resNormal_Full)

### Använd de genomsnittliga värdena på förklaringsvariablerna för modellerna i uppgift (e).
### Beräkna ett 95.2 % kredibilitetsintervall för y genom att använda modellernas Akaike-vikter i
### uppgift (f). Kredibilitetsintervallet för y blir således ett viktat genomsnittligt kredibilitetsintervall
### över modellerna.
# --- --- g)

sample_EnXvar <- extract.samples(resNormal_EnXvar,n = 1e4)
sample_EnXvar2ndKorr <- extract.samples(resNormal_EnXvar_2ndKorr,n = 1e4)
sample_Full <- extract.samples(resNormal_Full,n=1e4)

mu <- matrix(NA,ncol=3,nrow=1e4)
colnames(mu) <- c("EnXvar","EnXvar2ndKorr","Full")
for (i in 1:1e4) {
  mu[i,1] <- sample_EnXvar[i,1] + sample_EnXvar[i,2] * mean(data$area)
  mu[i,2] <- sample_EnXvar2ndKorr[i,1] + sample_EnXvar2ndKorr[i,2] * mean(data$antal_rum)
  mu[i,3] <- sample_Full[i,1] + sample_Full[i,2]*mean(data$area) +
            sample_Full[i,3]*mean(data$antal_rum) +
            sample_Full[i,4]*mean(data$avgift) + 
            sample_Full[i,5]*mean(data$trappor) +
            sample_Full[i,6]*0 +
            sample_Full[i,7]*0
}
y <- data.frame("y_enxvar" = rnorm(1e4,mu[,1],exp(sample_EnXvar$logsigma)),
                       "y_enxvar2ndkorr" = rnorm(1e4,mu[,2],exp(sample_EnXvar2ndKorr$logsigma)),
                        "y_full" = rnorm(1e4,mu[,3],exp(sample_Full$logsigma)))
y_intervall <- WAIC_df[1,2]*y[,1] + WAIC_df[2,2]*y[,2] + WAIC_df[3,2]*y[,3]
PI(y_intervall,prob = 0.952)

# --- --- h)
##########################################
##########################################
#Full modell
grid_sd <- seq(from = 0.01,to = 2, by=0.1)
h_df <- matrix(NA, ncol=2,nrow=20)
i <- 1
flist <- alist(
    y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
    mu <- b0 + b1*area + b2*antal_rum + b3*avgift + b4*trappor + b5*cityyes + b6*sydyes, # hur mu ?r l?nkad till f?rklaringsvariabler.
    b0 ~ dnorm( 3 , 10 ) , # normalf?rdelad prior f?r intercept
    c(b1,b2,b3,b4,b5,b6) ~ dnorm( 0 , 0.01 ) ,
    logsigma ~ dnorm ( 0, 1 )
  )
  
resNormal_grid_sd <- map(flist, data=data)
DICen <- DIC(resNormal_grid_sd)

h_df[i,1] <- 0.01
h_df[i,2] <- DICen[1]

i <- i+1

flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu <- b0 + b1*area + b2*antal_rum + b3*avgift + b4*trappor + b5*cityyes + b6*sydyes, # hur mu ?r l?nkad till f?rklaringsvariabler.
  b0 ~ dnorm( 3 , 10 ) , # normalf?rdelad prior f?r intercept
  c(b1,b2,b3,b4,b5,b6) ~ dnorm( 0 , 0.11 ) ,
  logsigma ~ dnorm ( 0, 1 )
)

resNormal_grid_sd <- map(flist, data=data)
DICen <- DIC(resNormal_grid_sd)

h_df[i,1] <- 0.11
h_df[i,2] <- DICen[1]

i <- i+1

flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu <- b0 + b1*area + b2*antal_rum + b3*avgift + b4*trappor + b5*cityyes + b6*sydyes, # hur mu ?r l?nkad till f?rklaringsvariabler.
  b0 ~ dnorm( 3 , 10 ) , # normalf?rdelad prior f?r intercept
  c(b1,b2,b3,b4,b5,b6) ~ dnorm( 0 , 0.21 ) ,
  logsigma ~ dnorm ( 0, 1 )
)

resNormal_grid_sd <- map(flist, data=data)
DICen <- DIC(resNormal_grid_sd)

h_df[i,1] <- 0.21
h_df[i,2] <- DICen[1]

i <- i+1

flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu <- b0 + b1*area + b2*antal_rum + b3*avgift + b4*trappor + b5*cityyes + b6*sydyes, # hur mu ?r l?nkad till f?rklaringsvariabler.
  b0 ~ dnorm( 3 , 10 ) , # normalf?rdelad prior f?r intercept
  c(b1,b2,b3,b4,b5,b6) ~ dnorm( 0 , 0.31 ) ,
  logsigma ~ dnorm ( 0, 1 )
)

resNormal_grid_sd <- map(flist, data=data)
DICen <- DIC(resNormal_grid_sd)

h_df[i,1] <- 0.31
h_df[i,2] <- DICen[1]

i <- i+1

flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu <- b0 + b1*area + b2*antal_rum + b3*avgift + b4*trappor + b5*cityyes + b6*sydyes, # hur mu ?r l?nkad till f?rklaringsvariabler.
  b0 ~ dnorm( 3 , 10 ) , # normalf?rdelad prior f?r intercept
  c(b1,b2,b3,b4,b5,b6) ~ dnorm( 0 , 0.41 ) ,
  logsigma ~ dnorm ( 0, 1 )
)

resNormal_grid_sd <- map(flist, data=data)
DICen <- DIC(resNormal_grid_sd)

h_df[i,1] <- 0.41
h_df[i,2] <- DICen[1]

i <- i+1

flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu <- b0 + b1*area + b2*antal_rum + b3*avgift + b4*trappor + b5*cityyes + b6*sydyes, # hur mu ?r l?nkad till f?rklaringsvariabler.
  b0 ~ dnorm( 3 , 10 ) , # normalf?rdelad prior f?r intercept
  c(b1,b2,b3,b4,b5,b6) ~ dnorm( 0 , 0.51 ) ,
  logsigma ~ dnorm ( 0, 1 )
)

resNormal_grid_sd <- map(flist, data=data)
DICen <- DIC(resNormal_grid_sd)

h_df[i,1] <- 0.51
h_df[i,2] <- DICen[1]

i <- i+1

flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu <- b0 + b1*area + b2*antal_rum + b3*avgift + b4*trappor + b5*cityyes + b6*sydyes, # hur mu ?r l?nkad till f?rklaringsvariabler.
  b0 ~ dnorm( 3 , 10 ) , # normalf?rdelad prior f?r intercept
  c(b1,b2,b3,b4,b5,b6) ~ dnorm( 0 , 0.61 ) ,
  logsigma ~ dnorm ( 0, 1 )
)

resNormal_grid_sd <- map(flist, data=data)
DICen <- DIC(resNormal_grid_sd)

h_df[i,1] <- 0.61
h_df[i,2] <- DICen[1]

i <- i+1

flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu <- b0 + b1*area + b2*antal_rum + b3*avgift + b4*trappor + b5*cityyes + b6*sydyes, # hur mu ?r l?nkad till f?rklaringsvariabler.
  b0 ~ dnorm( 3 , 10 ) , # normalf?rdelad prior f?r intercept
  c(b1,b2,b3,b4,b5,b6) ~ dnorm( 0 , 0.71 ) ,
  logsigma ~ dnorm ( 0, 1 )
)

resNormal_grid_sd <- map(flist, data=data)
DICen <- DIC(resNormal_grid_sd)

h_df[i,1] <- 0.71
h_df[i,2] <- DICen[1]

i <- i+1

flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu <- b0 + b1*area + b2*antal_rum + b3*avgift + b4*trappor + b5*cityyes + b6*sydyes, # hur mu ?r l?nkad till f?rklaringsvariabler.
  b0 ~ dnorm( 3 , 10 ) , # normalf?rdelad prior f?r intercept
  c(b1,b2,b3,b4,b5,b6) ~ dnorm( 0 , 0.81 ) ,
  logsigma ~ dnorm ( 0, 1 )
)

resNormal_grid_sd <- map(flist, data=data)
DICen <- DIC(resNormal_grid_sd)

h_df[i,1] <- 0.81
h_df[i,2] <- DICen[1]

i <- i+1

flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu <- b0 + b1*area + b2*antal_rum + b3*avgift + b4*trappor + b5*cityyes + b6*sydyes, # hur mu ?r l?nkad till f?rklaringsvariabler.
  b0 ~ dnorm( 3 , 10 ) , # normalf?rdelad prior f?r intercept
  c(b1,b2,b3,b4,b5,b6) ~ dnorm( 0 , 0.91 ) ,
  logsigma ~ dnorm ( 0, 1 )
)

resNormal_grid_sd <- map(flist, data=data)
DICen <- DIC(resNormal_grid_sd)

h_df[i,1] <- 0.91
h_df[i,2] <- DICen[1]

i <- i+1

flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu <- b0 + b1*area + b2*antal_rum + b3*avgift + b4*trappor + b5*cityyes + b6*sydyes, # hur mu ?r l?nkad till f?rklaringsvariabler.
  b0 ~ dnorm( 3 , 10 ) , # normalf?rdelad prior f?r intercept
  c(b1,b2,b3,b4,b5,b6) ~ dnorm( 0 , 1.01 ) ,
  logsigma ~ dnorm ( 0, 1 )
)

resNormal_grid_sd <- map(flist, data=data)
DICen <- DIC(resNormal_grid_sd)

h_df[i,1] <- 1.01
h_df[i,2] <- DICen[1]

i <- i+1

flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu <- b0 + b1*area + b2*antal_rum + b3*avgift + b4*trappor + b5*cityyes + b6*sydyes, # hur mu ?r l?nkad till f?rklaringsvariabler.
  b0 ~ dnorm( 3 , 10 ) , # normalf?rdelad prior f?r intercept
  c(b1,b2,b3,b4,b5,b6) ~ dnorm( 0 , 1.11 ) ,
  logsigma ~ dnorm ( 0, 1 )
)

resNormal_grid_sd <- map(flist, data=data)
DICen <- DIC(resNormal_grid_sd)

h_df[i,1] <- 1.11
h_df[i,2] <- DICen[1]

i <- i+1

flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu <- b0 + b1*area + b2*antal_rum + b3*avgift + b4*trappor + b5*cityyes + b6*sydyes, # hur mu ?r l?nkad till f?rklaringsvariabler.
  b0 ~ dnorm( 3 , 10 ) , # normalf?rdelad prior f?r intercept
  c(b1,b2,b3,b4,b5,b6) ~ dnorm( 0 , 0.61 ) ,
  logsigma ~ dnorm ( 0, 1 )
)

resNormal_grid_sd <- map(flist, data=data)
DICen <- DIC(resNormal_grid_sd)

h_df[i,1] <- 1.21
h_df[i,2] <- DICen[1]

i <- i+1

flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu <- b0 + b1*area + b2*antal_rum + b3*avgift + b4*trappor + b5*cityyes + b6*sydyes, # hur mu ?r l?nkad till f?rklaringsvariabler.
  b0 ~ dnorm( 3 , 10 ) , # normalf?rdelad prior f?r intercept
  c(b1,b2,b3,b4,b5,b6) ~ dnorm( 0 , 1.31 ) ,
  logsigma ~ dnorm ( 0, 1 )
)

resNormal_grid_sd <- map(flist, data=data)
DICen <- DIC(resNormal_grid_sd)

h_df[i,1] <- 1.31
h_df[i,2] <- DICen[1]

i <- i+1

flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu <- b0 + b1*area + b2*antal_rum + b3*avgift + b4*trappor + b5*cityyes + b6*sydyes, # hur mu ?r l?nkad till f?rklaringsvariabler.
  b0 ~ dnorm( 3 , 10 ) , # normalf?rdelad prior f?r intercept
  c(b1,b2,b3,b4,b5,b6) ~ dnorm( 0 , 1.41 ) ,
  logsigma ~ dnorm ( 0, 1 )
)

resNormal_grid_sd <- map(flist, data=data)
DICen <- DIC(resNormal_grid_sd)

h_df[i,1] <- 1.41
h_df[i,2] <- DICen[1]

i <- i+1

flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu <- b0 + b1*area + b2*antal_rum + b3*avgift + b4*trappor + b5*cityyes + b6*sydyes, # hur mu ?r l?nkad till f?rklaringsvariabler.
  b0 ~ dnorm( 3 , 10 ) , # normalf?rdelad prior f?r intercept
  c(b1,b2,b3,b4,b5,b6) ~ dnorm( 0 , 1.51 ) ,
  logsigma ~ dnorm ( 0, 1 )
)

resNormal_grid_sd <- map(flist, data=data)
DICen <- DIC(resNormal_grid_sd)

h_df[i,1] <- 1.51
h_df[i,2] <- DICen[1]

i <- i+1

flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu <- b0 + b1*area + b2*antal_rum + b3*avgift + b4*trappor + b5*cityyes + b6*sydyes, # hur mu ?r l?nkad till f?rklaringsvariabler.
  b0 ~ dnorm( 3 , 10 ) , # normalf?rdelad prior f?r intercept
  c(b1,b2,b3,b4,b5,b6) ~ dnorm( 0 , 1.61 ) ,
  logsigma ~ dnorm ( 0, 1 )
)

resNormal_grid_sd <- map(flist, data=data)
DICen <- DIC(resNormal_grid_sd)

h_df[i,1] <- 1.61
h_df[i,2] <- DICen[1]

i <- i+1

flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu <- b0 + b1*area + b2*antal_rum + b3*avgift + b4*trappor + b5*cityyes + b6*sydyes, # hur mu ?r l?nkad till f?rklaringsvariabler.
  b0 ~ dnorm( 3 , 10 ) , # normalf?rdelad prior f?r intercept
  c(b1,b2,b3,b4,b5,b6) ~ dnorm( 0 , 1.71 ) ,
  logsigma ~ dnorm ( 0, 1 )
)

resNormal_grid_sd <- map(flist, data=data)
DICen <- DIC(resNormal_grid_sd)

h_df[i,1] <- 1.71
h_df[i,2] <- DICen[1]

i <- i+1

flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu <- b0 + b1*area + b2*antal_rum + b3*avgift + b4*trappor + b5*cityyes + b6*sydyes, # hur mu ?r l?nkad till f?rklaringsvariabler.
  b0 ~ dnorm( 3 , 10 ) , # normalf?rdelad prior f?r intercept
  c(b1,b2,b3,b4,b5,b6) ~ dnorm( 0 , 1.81 ) ,
  logsigma ~ dnorm ( 0, 1 )
)

resNormal_grid_sd <- map(flist, data=data)
DICen <- DIC(resNormal_grid_sd)

h_df[i,1] <- 1.81
h_df[i,2] <- DICen[1]

i <- i+1

flist <- alist(
  y ~ dnorm(mu, exp(logsigma)) , # likelihood fr?n normalf?rdelning, liter per mil
  mu <- b0 + b1*area + b2*antal_rum + b3*avgift + b4*trappor + b5*cityyes + b6*sydyes, # hur mu ?r l?nkad till f?rklaringsvariabler.
  b0 ~ dnorm( 3 , 10 ) , # normalf?rdelad prior f?r intercept
  c(b1,b2,b3,b4,b5,b6) ~ dnorm( 0 , 1.91 ) ,
  logsigma ~ dnorm ( 0, 1 )
)

resNormal_grid_sd <- map(flist, data=data)
DICen <- DIC(resNormal_grid_sd)

h_df[i,1] <- 1.91
h_df[i,2] <- DICen[1]

i <- i+1
colnames(h_df) <- c("sd","DIC")
h_df
########################################
########################################


##### Uppgift 2
#a)
data_mcmc <- data[,1:7]
colnames(data_mcmc)
# Skatta modell med map2stan
stan.modell <- alist( data_mcmc$y ~ dnorm( mu , sigma ) ,
                           mu <- b0 + b1*area + b2*antal_rum + b3*avgift + b4*trappor + b5*cityyes + b6*sydyes ,
                           b0 ~ dnorm(3,10),
                           b1 ~ dnorm(0,10),
                           b2 ~ dnorm(0,10),
                           b3 ~ dnorm(0,10),
                           b4 ~ dnorm(0,10),
                           b5 ~ dnorm(0,10),
                           b6 ~ dnorm(0,10),
                           sigma ~ dcauchy(0,2)
)

stan.man <- map2stan(stan.modell,data=data_mcmc,iter = 4000,warmup = 1000,chains = 4)
precis(stan.man,prob = 0.952)
precis(resNormal_Full,prob= 0.952)
#Logsigma -> Sigma
test_samples <- extract.samples(resNormal_Full)
sigma<-exp(test_samples$logsigma)
precis(sigma,prob=0.952)
#b)
pairs(stan.man)
pairs(resNormal_Full)


#c)
Start <- coef(resNormal_Full)
stan.man.start <- map2stan(stan.modell,data=data_mcmc,
         start=list(b0=Start[1],b1=Start[2],b2=Start[3],b3=Start[4],
                    b4=Start[5],b5=Start[6],b6=Start[7],sigma=Start[8]),
         chains=4,iter=4000,warmup=1000)
tracerplot(stan.man.start)


plot(stan.man)
PostSamp <- extract.samples(stan.man)

# Unders?k konvergens f?r posterior means
AnvPar <- PostSamp$b1
NIter <- length(PostSamp$b1)
Means <-  matrix(0,nrow=NIter,ncol=1,byrow=TRUE)
for (iter in 1:NIter){
  Means[iter] <- mean(AnvPar[1:iter])
}
plot(Means)

# Unders?k konvergens f?r olika kvantiler
Quant <- matrix(0,nrow=NIter,ncol=4,byrow=TRUE)
for (iter in 1:NIter){
  Quant[iter,] <- quantile(AnvPar[1:iter],probs=c(0.25,0.50,0.75,0.99))
}
par(mfrow=c(2,2))
plot(Quant[,1])
plot(Quant[,2])
plot(Quant[,3])
plot(Quant[,4])




