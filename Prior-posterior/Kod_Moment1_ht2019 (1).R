### Bayesiansk statistik, 732g34, moment 1 ###
### Kod fr�n Statistical Rethinking, Richard McElreath ###
### BW, kod fr�n Bertil Wegmann ###

### Sampla fr�n en grid-approximativ posterior ###

# R code 3.2
p_grid <- seq( from=0 , to=1 , length.out=1000 ) # grid av v�rden

prior <- rep( 1 , 1000 ) # uniform prior
# Allm�n Beta prior, BW, se f�rel�sningsslides
alpha <- 54
beta <- 44
prior <- dbeta(p_grid, alpha, beta, ncp = 0, log = FALSE)

likelihood <- dbinom( 600 , size=1000 , prob=p_grid ) # 600 lyckade av 1000 f�rs�k
posterior <- likelihood * prior # posterior prop mot likelihood*prior
posterior <- posterior / sum(posterior) # g�r posteriorn till en t�thet

# R code 3.3
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE ) # dra v�rden fr�n posteriorn

# R code 3.4
plot( samples ) # plotta v�rdena

# R code 3.5
library(rethinking)
dens( samples ) # skattning av t�thetsfunktion fr�n de dragna posteriorv�rdena

# R code 2.6, tag fram posteriorf�rdelningen med kvadratisk approximation
typ.a.flaska <- map(
  alist(
    w ~ dbinom(1000,p) , # binomial likelihood
    p ~ dbeta(54,44) # beta prior
  ) ,
  data=list(w=600) )
# display summary of quadratic approximation
precis( typ.a.flaska , digits=5)

# Plotta kvadratiska approximationen mot grid approximationen. Anv�nd medelv�rde och st.avv. fr�n funktion precis.
samplesQ <- rnorm(1e4,0.5958,0.01482)
dens(samples,type="l",col="black")
dens(samplesQ,type="l",col="red",add=TRUE)

# R code summary, display summary of quadratic approximation
precis( typ.a.flaska , digits=5)

# R code 3.6
# add up posterior probability where p < 0.59
sum( posterior[ p_grid < 0.59 ] ) # sannolikheten f�r p < 0.59 fr�n grid av v�rden

# R code 3.7
sum( samples < 0.59) / 1e4 # sannolikheten f�r p < 0.59 fr�n samplade posteriorv�rden

# R code 3.8
sum( samples > 0.59 & samples < 0.62 ) / 1e4 # posteriorsannolikhet mellan 0.59 och 0.62

# R code 3.10
quantile( samples , c( 0.0455 , 0.9545 ) ) # ger intervallet med de 90.9 % mittersta v�rdena i posteriorn


### Visualisering av skillnader mellan kredibilitetsintervall och Highest Posterior Density Interval (HPDI) ###

# R code 3.11, samplade v�rden fr�n en v�ldigt skev posterior
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep(1,1000)
likelihood <- dbinom( 3 , size=3 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample( p_grid , size=1e4 , replace=TRUE , prob=posterior )
dens(samples)

# R code 3.12, 50 % kredibilitetsintervall (percentilintervall, PI) ger samma som quantile
PI( samples , prob=0.5 )

# R code 3.13, 50 % HPDI
HPDI( samples , prob=0.5 )

### L�gesm�tt f�r posteriorn: medelv�rde, median, typv�rde ###

# R code 3.14
# Ber�kning av typv�rdet fr�n gridden av v�rden, Maximum A Posteriori (MAP) 
p_grid[ which.max(posterior) ]

# R code 3.15
# Ber�kning av MAP fr�n samplade v�rden
chainmode( samples , adj=0.01 )

# R code 3.16
# Ber�kning av medelv�rde och median fr�n samplade v�rden
mean( samples )
median( samples )


# R kod normal, BW
# kvadratisk approximation med normalf�rdelade data
# vikter f�r kycklingar efter 6 veckors f�da
data(chickwts)
# standardavvikelse
Stavv <- sd(chickwts$weight)
Stavv
PriorMu <- 200
PriorStavv <- 100

flist <- alist(
  weight ~ dnorm(mu, Stavv) , # likelihood fr�n normalf�rdelning
  mu ~ dnorm( PriorMu , PriorStavv ) # normalf�rdelad prior
)

resNormal <- map(flist, data=chickwts)
precis(resNormal)

# Exact solution, BW
PriorPrecision <- 1/PriorStavv**2
DataPrecision <- length(chickwts$weight) / Stavv**2
w <- DataPrecision/(PriorPrecision + DataPrecision)
PostMean <- w*mean(chickwts$weight) + (1-w)*PriorMu
PostStavv <- sqrt(1/(PriorPrecision+DataPrecision))
PostMean
PostStavv

# R kod Poisson, exempel p� modellstruktur, BW
flist <- alist(
  weight ~ dpois(theta) , # likelihood fr�n Poissonf�rdelning
  theta ~ dgamma( alpha , beta ) # gammaf�rdelad prior med parametrar 
                                 # alpha och beta
)
resPoisson <- map(flist, data=PoissonData) # data frame PoissonData

# N�gra tips nedan

# create data frame
DataFrame <- data.frame(chickwts$weight) # data frame f�r kycklingvikter
# Other distributions
?Distributions















