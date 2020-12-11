#Installera Rethinking packete
install.packages(c("mvtnorm","loo","coda"), repos="https://cloud.r-project.org/",dependencies=TRUE)
options(repos=c(getOption('repos'), rethinking='http://xcelab.net/R'))
install.packages('rethinking',type='source')
require(rethinking)

#Introduktion till bayesiansk statistik
#Grid approximation
#Kvadratisk approximation

### Uppgift 1 ###
#a)

#Beta konjugerad prior till binomial fördelningen

valdata <- read.csv2("C:/Users/Jakob/Desktop/Bayesian statistics/Lab1/valdata.csv")
val_andel_mp <- valdata[1:12,10]/100

alpha <- 8.389154
beta <- 112.6174

prior <- rbeta(n = 1e4, alpha,beta, ncp=0)

dens(prior,main = "Priorfördelning beta(8.39,112.62)")

#b)

#posterior hyperparameters 
#beta(alhpa + sum xi, beta + n - sum xi)

n <- 1612
sumxi <- 77.376

posterior <- rbeta(n = 1e4, alpha + sumxi, beta + n - sumxi, ncp = 0)

#Högre varians i priorn, posteriorn ganska säker eftersom
#n=1612 relativt stort. 

#Analytisk posterior
dens(posterior,col="black", main = "Posterior- och Priorfördelning")

dens(prior,col="red",add=TRUE)
legend(0.06, 70, legend = c("Posterior", "Prior"), fill=c("black", "red"), cex = 0.8)


#c)

#### Grid approximation ####
#där approximationen blir finare
#med fler värden i gridden (length.out)

p_grid <- seq( from=0 , to=1 , length.out=1000 ) # grid av v?rden

# Skapa prior baserat på värdena i gridden
prior <- dbeta(p_grid, alpha, beta, ncp = 0, log = FALSE)

#Beräkna likelihood med värdena i gridden
likelihood <- dbinom( round(sumxi) , size=n , prob=p_grid )
#Posterior till täthet
posterior <- likelihood * prior # posterior prop mot likelihood*prior
posterior <- posterior / sum(posterior) # g?r posteriorn till en t?thet

#Sampla från gridden med sannolikheterna från posteriorn
#För att erhålla posteriorfördelningen
samples_grid <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE ) # dra v?rden fr?n posteriorn

plot( samples_grid ) # plotta v?rdena

#Plotta posteriorfördelningen
dens(samples_grid, main = "Posteriorfördelning med grid approximation")

##### Kvadratisk approximation ####

flist <- alist( w ~ dbinom(1612,p) ,
                p ~ dbeta(8.389154,112.6174) 
)
KA <- map(flist, data=list(w=77))

# display summary of quadratic approximation
precis( KA , digits=5)

# Plotta kvadratiska approximationen mot grid approximationen. Anv?nd medelv?rde och st.avv. fr?n funktion precis.
samplesQ <- rnorm(1e4,0.04875,0.00517)
#Black = gridapprox, Red = kvadrat approx
dens(samples_grid,type="l",col="black", main = "Posteriorfördelning med approximation")
dens(samplesQ,type="l",col="red",add=TRUE)
legend(0.055, 80, legend = c("Grid approximation", "Kvadrat approximation"), fill=c("black", "red"), cex = 0.8)

dens(samples_grid,type="l",col="black", main = "Posteriorfördelning med gridapproximation")
dens(posterior,type="l",col="red",add=TRUE)
legend(0.055, 80, legend = c("Grid approximation", "Posterior"), fill=c("black", "red"), cex = 0.8)

dens(samplesQ,type="l",col="black", main = "Posteriorfördelning med kvadrat approximation")
dens(posterior,type="l",col="red",add=TRUE)
legend(0.053, 75, legend = c("Kvadrat approximation", "Posterior"), fill=c("black", "red"), cex = 0.8)

#e)

mean(samples_grid)
sd(samples_grid)

#f)
posterior_PI <- rbeta(n = 1e4, alpha + sumxi, beta + n - sumxi, ncp = 0)


PI( posterior_PI , prob=0.909 )

#g)
odds <- samplesQ / (1 - samplesQ)
dens(odds, main="Fördelning av odds för posteriorfördelningen")

### Uppgift 2 ###

data(chickwts)

soybeans_index <- chickwts$feed == "soybean"
meatmeal_index <- chickwts$feed == "meatmeal"

soybean_data <- chickwts[soybeans_index,1:2]
meatmeal_data <- chickwts[meatmeal_index,1:2]

ejsoy_ejmeat <-chickwts[!soybeans_index&!meatmeal_index,1:2]

#a)
#Modellera soybean kycklingars vikt
#Känd standardavvikelse om soybean kycklingar
#Prior om mu och sd används från kycklingar som ej är soy eller meat

Stavv <- sd(soybean_data[,1])

PriorMu <- mean(ejsoy_ejmeat[,1])
PriorStavv <- sd(ejsoy_ejmeat[,1])

flist <- alist(
  weight ~ dnorm(mu, Stavv) , # likelihood fr?n normalf?rdelning
  mu ~ dnorm( PriorMu , PriorStavv ) # normalf?rdelad prior
)

resNormal <- map(flist, data=soybean_data)
precis(resNormal)

samples_2a <- rnorm(1e4, 246.85, 14.27)
dens(samples_2a, type="l",col="black", main = "Posterior för kycklingar som ätit soybeans")

#b)
#Modellera meatmeal kycklingars vikt
#Känds standaravikelse om meatmeal kycklingar
#Prior om mu och sd från kycklingar som ej rä soy eller meat
Stavv_b <- sd(meatmeal_data[,1])

PriorMu_b <- mean(ejsoy_ejmeat[,1])
PriorStavv_b <- sd(ejsoy_ejmeat[,1])

flist_b <- alist(
  weight ~ dnorm(mu, Stavv_b) , # likelihood fr?n normalf?rdelning
  mu ~ dnorm( PriorMu_b , PriorStavv_b ) # normalf?rdelad prior
)

resNormal_b <- map(flist_b, data=meatmeal_data)
precis(resNormal_b)

samples_2b <- rnorm(1e4, 276.2, 19.09)
dens(samples_2b, type="l",col="black",main = "Posterior för kycklingar som ätit meatmeal")

#Plotta a och b i samma
#Lägre medelvärdes vikt och högre varians i meatmeal kycklingar än soybean kycklingar
dens(samples_2a, type="l",col="black", main = "Fördelning av kycklingar ")
dens(samples_2b, type="l",col="red",add=TRUE)
legend(260, 0.028, legend = c("Soybeans", "Meatmeal"), fill=c("black", "red"), cex = 0.8)

#c)
#Posterior för skillnaden i vikt mellan meat och soy kycklingar

posterior_diff <- samples_2b - samples_2a
dens(posterior_diff,main="Posterior flör skillnaden i medelvärde")

#Sannolikheten för att medelvärdes vikten för kycklingar som ätit
#Meatmeal är större än medelvärdes vikten för kycklingar
#som ätit soybeans.
sum( samples_2b > samples_2a) / 1e4
