

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

### Natural mortality (Then et al. 2015; Hoenig, 1983; Hewitt and Hoenig, 2005)
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

Hoenig_M <- function(maxage) 3/maxage
Hewitt_M <- function(maxage) 4.22/maxage

Then_M(51) # 0.13
Then_M(43) # 0.16

Then_M(Linf = 264, K = 0.04) # 0.06
Then_M(Linf = 230, K = 0.06) # 0.09

Hoenig_M(51) # 0.06
Hoenig_M(43) # 0.07

Hewitt_M(51) # 0.08
Hewitt_M(43) # 0.10




### Maturity

## Stewart et al 2015 citing pers comm by C. Ceapa
# Male = 140 cm
# Female = 160 cm

# Get the slope parameter given the length of 50% maturity (x50), another length (x) 
# and its corresponding maturity (m)
mat_slope <- function(x, m, x50) log((1-m)/m)/(x - x50)
mat_slope(160, 0.05, 175)




### Steepness
## Mangel et al. 2010 using fecundity, spawning frequency, and larval survival
## Set spawning frequency = 0.25 (once every four years)
steepness_fn <- function(seed = 1, M = 0.06, N = 200, spawn_freq = 1, 
                         wt_a = 2e-5, wt_b = 2.72, Linf = 264, K = 0.04, t0 = -0.94, 
                         sex_ratio = 0.5, surv_larval = 0.25,
                         mat_at_age) {
  maxage <- 60
  set.seed(seed)
  U <- runif(N)
  age <- -log(U)/M
  len <- Linf * (1 - exp(-K * (age - t0)))
  wt <- wt_a * len ^ wt_b
  
  age_round <- ifelse(age >= maxage, maxage, round(age))
  p_mat <- mat_at_age[age_round + 1] # Vector includes age-0
  
  tot_fecundity <- sum(p_mat * wt)
  wt_pop <- sum(wt)
  
  #w0 <- wt_a * (Linf * (1 - exp(K * t0))) ^ wt_b
  alpha <- surv_larval * spawn_freq * tot_fecundity / wt_pop
  
  calc_SPR0 <- function() {
    len_age <- Linf * (1 - exp(-K * (c(0:maxage) - t0)))
    wt_age <- wt_a * len_age ^ wt_b
    NPR <- numeric(maxage + 1)
    NPR[1] <- 1
    for(a in 2:(maxage + 1)) NPR[a] <- NPR[a-1] * exp(-M)
    NPR[maxage + 1] <- 1/(1 - exp(-M))
    SPR <- sum(NPR * wt_age * mat_at_age)
    return(SPR)
  }
  
  SPR0 <- calc_SPR0()
  h <- alpha * (1 - sex_ratio) * SPR0/(4 + alpha * (1 - sex_ratio) * SPR0)
  return(list(h = h, alpha = alpha))
}

get_mat_age <- function(replist) {
  if(missing(replist)) {
    replist <- r4ss::SS_output(file.path(getwd(), "SS", "01_base"))
  }
  Len_Mat <- dplyr::filter(replist$endgrowth, Sex == 1)$Len_Mat
  return(Len_Mat)
}

mat_at_age <- get_mat_age() 
data.frame(age = 1:length(mat_at_age) - 1, mat = mat_at_age) %>% write.csv("processed_data/mat_age.csv")
data.frame(age = 1:length(mat_at_age) - 1, mat = mat_at_age) %>% plot(mat ~ age, .)

set.seed(234)
nsim <- 200
seeds <- sample(1:4000000, size = nsim)
mean_M <- 0.06
sd_M <- 0.4
M <- rlnorm(nsim, log(mean_M) - 0.5 * sd_M^2, sd_M)


# Steepness as a function of larval survival
surv <- seq(0.01, 0.99, 0.01)
sims <- vapply(surv, function(x) {
  run_sim <- Map(steepness_fn, seed = seeds, M = M, 
                 MoreArgs = list(mat_at_age = mat_at_age, N = 500, spawn_freq = 0.25,
                                 sex_ratio = 0.5, surv_larval = x))
  h <- run_sim %>% sapply(getElement, "h")
  return(c(mean(h), sd(h)))
}, numeric(2))

plot(surv, sims[1, ], xlab = "Larval survival", ylab = "Mean steepness")
plot(surv, sims[2, ], xlab = "Larval survival", ylab = "SD steepness")
plot(surv, sims[2, ]/sims[1, ], xlab = "Larval survival", ylab = "CV steepness")

# Get distribution of steepness
run_sim <- Map(steepness_fn, seed = seeds, M = M, 
               MoreArgs = list(mat_at_age = mat_at_age, N = 500, spawn_freq = 0.25,
                               sex_ratio = 0.5, surv_larval = 0.1))
h <- run_sim %>% sapply(getElement, "h")
hist(h)

alpha <- run_sim %>% sapply(getElement, "alpha")
hist(alpha)


# Find the max F constraint given the spawning frequency
get_maxF <- function(sf = 0.25, M = 0.06) {
  
  F_solver <- function(logF, sf, M) {
    FF <- exp(logF)
    Z <- FF + M
    FF/Z * (1 - exp(-Z)) - sf
  }
  
  Fout <- uniroot(F_solver, log(c(1e-4, 3)), sf = sf, M = M)$root
  c(Baranov = exp(Fout), Simple = -log(1-sf))
}

get_maxF(0.5)
