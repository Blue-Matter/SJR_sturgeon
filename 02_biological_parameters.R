

###### Summary of biological parameters

### Growth
vb <- function(age, Linf, K, t0) Linf * (1 - exp(-K * (age - t0)))

## Bradford et al 2016
# Female
Linf <- 222
K <- 0.095
t0 <- 0

# Male
Linf <- 201
K <- 0.11
t0 <- 0

# Length-round weight conversion
# log10wR = 2.72(log10TL) – 4.70
# W = a L ^ b
a <- 10^-4.7 # 2e-5
b <- 2.72

## Stewart et al 2015 (includes juvenile unsexed individuals, implying no sexual dimorphism until maturity)
# Female
Linf <- 264
K <- 0.04
t0 <- -0.94

# Male
Linf <- 230
K <- 0.06
t0 <- -0.6

vb(1, 264, 0.04, -0.94)
vb(1, 230, 0.06, -0.6)
### Natural mortality (Then et al. 2015)
## Stewart et al. 2015
## Female max age = 51
## Male max age = 43

Then_M <- function(maxage, Linf, K) {
  if(!missing(maxage)) {
    return(4.899/(maxage^0.916))
  } else {
    return(4.118 * K ^ 0.73 / (Linf ^ 0.33))
  }
}

Then_M(51) # 0.13
Then_M(43) # 0.16

Then_M(Linf = 264, K = 0.04) # 0.06
Then_M(Linf = 230, K = 0.06) # 0.09


### Maturity

## Stewart et al 2015 citing pers comm by C. Ceapa
# Male = 140 cm
# Female = 160 cm

# Get the slope parameter given the length of 50% maturity (x50), another length (x) 
# and its corresponding maturity (m)
mat_slope <- function(x, m, x50) log((1-m)/m)/(x - x50)

mat_slope(160, 0.05, 175)
