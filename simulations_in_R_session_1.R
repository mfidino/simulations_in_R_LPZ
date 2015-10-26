#################################
#
# Code for simulation session # 1
#
# Produced by Mason Fidino
#
#


##############################################################################
###  EXAMPLE 1
##############################################################################

# Purpose: Simple example of simulating samples from a normal distribution

# population values for mean and standard deviation
# of individual lengths for snakes

mu <- 65 # population mean
sigma <- 5 # population sd

# Single sample
set.seed(44) # set this so everyone gets same results
x <- rnorm(n = 10, mean = mu, sd = sigma)

# summarize the sample

mean(x)
sd(x)

# plot out a histogram of the sample and compare 
# sample mean to population mean

hist(x, col = "gray", xlab = "Body length (cm)", las = 1, main = "")
# Fancy title stuff to add different colors
title(expression("Sample Mean "* phantom("and Population mean")), col.main = "blue")
title(expression(phantom("Sample mean ")*"and"*phantom( "Population mean")), 
      col.main = "black")
title(expression(phantom("Sample mean and")*"   Population Mean"), col.main = "red")
# line for population mean
abline(v = mu, lwd = 3, col = "red")
# line for sample mean
abline(v = mean(x), lwd = 3, col = "blue")


####################################################################################
# Example 1 end; Back to power point
####################################################################################

####################################################################################
# Example 2 start
####################################################################################

### Purpose: Taking a repeated sample of the means to determine 
###          if the difference between our sample mean
###           and our population mean is just sampling variation


# number of samples
n_iter <- 10^6

# vector for samples to go, we create this vector because it
# speeds up computation
sample_means <- rep(NA, n_iter)

# fill each element of sample_means with the mean of 10 random 
# samples from our population mean and standard deviation 

for(i in 1:n_iter){
  sample_means[i] <- mean(rnorm(n=10, mean = mu, sd = sigma))
}

# plot out the sample means and compare to the population mean

hist(sample_means, col = "gray", main = "", xlab = "Body length (cm)",
     n_class = 50, freq = FALSE, las = 1)
title(expression("Sample Mean "* phantom("and Population mean")), col.main = "blue")
title(expression(phantom("Sample mean ")*"and"*phantom( "Population mean")), 
      col.main = "black")
title(expression(phantom("Sample mean and")*"   Population Mean"), col.main = "red")
abline(v = mu, lwd = 5, col = "red")
abline(v = mean(sample_means[1:6000]), lwd = 5, col = "blue", lty = 2)

# See how estimates of the sample mean get more precise with repeated samples

to_mean <- c(1:10, seq(15, 2015, 5))
new_means <- rep(NA, length(to_mean))
for(i in 1:length(to_mean)){
  new_means[i] <- mean(sample(sample_means,size = to_mean[i], replace = FALSE))
}
plot(new_means ~ to_mean, xlab = "number of batched samples", ylab = "sample mean")
abline(h = mu, lwd = 2, col = "red", lty = 3)


####################################################################################
# Example 2 end; Back to power point
####################################################################################

####################################################################################
# Example 3 start
####################################################################################


# Purpose: simulating a linear model

# step 1: create deterministic part of linear model with 2 covariates
# 2 covariates
x1 <- rnorm(100) #100 random draws, mean 0, sd 1. 
x2 <- rnorm(100)

# Why set mean to zero and sd to 1?



# small aside
####################################################################
# z transformation is common technique, sets these constraints

unif_covar <- runif(1000, min = 5, max = 59) # random uniform,l
mean(unif_covar)
sd(unif_covar)

# apply z-transformation and look at mean and sd

scaled_unif <- scale(unif_covar)
mean(scaled_unif)
sd(scaled_unif)

### Z-transformation is ideal because it makes interpretation of 
### regression coefficients easier as they are now scale-independent
############################################################################
# back to simulation

# set parameter estimates for deterministic model
b0 <- 25 # intercept
b1 <- -5 # slope of b1
b2 <- 3 # slope of b2

# Step 2: simulate dependent variable, multiply all 100 values of x1 and x2
# by their true parameters, add those together with the intercept

y_det <- b0 + b1 * x1 + b2 * x2

# or you can do this with matrix math

B <- rbind(b0, b1, b2) # column vector
X <- cbind(1, x1, x2) # one is there for the intercept term

y_det2 <- X %*% B

# check if they are the same

identical(as.numeric(y_det2), y_det) # yep!

# Step 3: Add sampling variation

y <- rnorm(100, mean = y_det, sd = 3)

# Observe how y_det and y differ

plot(y ~y_det)


### side note: for linear models, variation can be added at step 2

set.seed(44)
y1 <- b0 + b1 * x1 + b2 * x2 + rnorm(100, mean = 0, sd = 3)
set.seed(44)
y2 <- rnorm(100, mean = y_det, sd = 3)

identical(y1, y2)


### Step 4: Analyze the model



# specify model
m1 <- lm(y ~ x1 + x2)

# look at output, b0 = 24, b1 = -5, b2 = 3, standard error = 3
summary(m1)


# can be done with grouped data as well
x <- gl(n = 2,k =  50, labels = c("control", "treatment"))

# look to see what is actually put into the model        
X <- model.matrix(y ~ x) # creates dummy variable

# but why did the control get pulled into the intercept, why not
# treatment? That is related to the levels of the factor x.

levels(x)



####################################################################################
# Example 3 end; Back to power point
####################################################################################

####################################################################################
# Example 4 start
####################################################################################

# Purpose: Compute coverage for paramter estimates

### compute coverage

# number of iterations for coverage
n_iter <- 10000

# list to put our quantile output
sample_list <- vector("list", length = n_iter)

# compute quantiles with alpha of 0.05
for(i in 1:n_iter){
  # generate 10 random samples from a normal distribution
  # with mean my and sd sigma and from those 10 random samples
  # compute 95 % confidence intervals
  
  sample_list[[i]] <- quantile(rnorm(n=10, mean = mu, sd = sigma),
                               probs = c(0.025, 0.975))
}

# put the sample_list into a matrix
s2 <- matrix(unlist(sample_list), ncol = 2, byrow = TRUE)

head(s2)

# write wrapper function for apply
ap_wrap <- function(dat = NULL, mu = 65){
  d_low <- dat[1]
  d_high <- dat[2]
  return(d_low <= mu & d_high >= mu )
}

# apply ap_wrap to every row of the s2 matrix
tf_vec <- apply(s2, 1, ap_wrap)

# compute coverage, true values = 1, false values = 0

sum(tf_vec) / length(tf_vec)

####################################################################################
# Example 4 end; Back to power point
####################################################################################

####################################################################################
# Example 5 start
####################################################################################

# Purpose: conduct a simple and more complicated power analysis


# Power analysis, example 1

# Easy example, changing only one parameter, sample size

# Number of iterations for each sample size

n_iter <- 400

# vector to store p values

p_val <- rep(NA, n_iter)

# Number of samples
n_samp <- seq(10, 100, 5)


# vector to put our power into
power_samp <- rep(NA, length(n_samp))

# set slope, intercept, x values, and sampling variation

b0 <- 1 # intercept
b1 <- 2 # slope
x <- rnorm(max(n_samp)) # maximum number of samples needed
sd <- 4 # sampling variation

# Loop through n_samp, and repeat each n_samp n_iter times

for(i in 1:length(n_samp)){
  n <- n_samp[i] # select the ith element in n_samp
  x_samp <- x[1:n] # grab the first n values from x vector
  for(k in 1:n_iter){
    y_det <- b0 + b1 * x_samp # make deterministic part
    y <- rnorm(n, mean = y_det, sd = sd) # add variation
    mod <- lm(y ~ x_samp) # linear model
    p_val[k] <- coef(summary(mod))["x_samp", "Pr(>|t|)"] # pull out only p value
    # for the parameter
  }
  power_samp[i] <- sum(p_val < 0.05)/n_iter
}

# group together power_samp and n_samp

my_ans <- data.frame(n_samp, power_samp)


# plot out the relationship here too

plot(power_samp~ n_samp, type = "l", xlab = "Number of Samples", ylab = "Power")
points(power_samp ~ n_samp)
abline(h = 0.8, lty = 2)


# How many samples to reach a power of 0.8?

head(my_ans[my_ans$power_samp >0.8,],1)

# 35 samples (or somewhere between 30 and 35)!


##########################################################

### More complex example

rm(list = ls())
# vary 1 covariates, sample size, and sampling variation

b0 <- 2 
b1 <- c(0.2, .6, 1) # set covariates

n_samp <- seq(10, 310, 20)
x1 <- rnorm(max(n_samp))


samp_sd <- c(1, 3) # low and high

n_iter <- 100 # fewer iterations here because this will take a while to run
p_val <- rep(NA, n_iter)

              

# How many different combinations are we going to have?

hmm <- expand.grid(b0, b1, n_samp, samp_sd) # 96 different combinations

# big_array

my_array <- array(NA, dim = c(length(b1), length(n_samp), length(samp_sd)))

# only inputting p values from the first regression coefficeint
  for(k in 1:length(b1)){
    b1_1 <- b1[k] # set b1
        for(l in 1:length(n_samp)){
          n <- n_samp[l] # set number of samples
          b1x <- x1[1:n] # grab covariate data
          for(p in 1:length(samp_sd)){
            my_sd <- samp_sd[p] # set sd
            for(q in 1:n_iter){
              y_det <- b0 + b1_1 * b1x # deterministic part of model
              y <- rnorm(n, mean = y_det, sd = my_sd) # add noise
              mod <- lm(y ~ b1x) # apply model
              p_val[q] <- coef(summary(mod))["b1x", "Pr(>|t|)"] # collect p value
            }
            my_array[ k, l, p] <- sum(p_val1 < 0.05)/n_iter # compute power
        }
  }
  }

# sampling variation equals 1, plotting out different values for b1 and n_samp
m1 <- t(my_array[,,-2])
# sampling variation equals 5, plotting out different values for b1 and n_samp
m2 <- t(my_array[,,-1])

windows(height = 6, width = 12)
par(mfrow = c(1, 2))
plot(m1[,1] ~ n_samp, type = "p", ylim = c(0,1), col = "blue", 
     main = "sampling variation = 1", xlab = "number of samples", ylab = "power")
points(m1[,1] ~ n_samp, type = "l", col = "blue")
points(m1[,2] ~ n_samp, type = "p", col = "red")
points(m1[,2] ~ n_samp, type = "l", col = "red")
points(m1[,3] ~ n_samp, type = "p")
points(m1[,3] ~ n_samp, type = "l")
text(c(75, 55, 120), c(0.2, 0.85, 0.95), labels = c("b1 = 0.2", "b1 = 0.6", "b1 = 1"),
     col = c("blue", "red", "black"))
abline(h = 0.8, lty = 2)



plot(m2[,1] ~ n_samp, type = "p", ylim = c(0,1), col = "blue", 
     main = "sampling variation = 3", xlab = "number of samples", ylab = "power")
points(m2[,1] ~ n_samp, type = "l", col = "blue")
points(m2[,2] ~ n_samp, type = "p", col = "red")
points(m2[,2] ~ n_samp, type = "l", col = "red")
points(m2[,3] ~ n_samp, type = "p")
points(m2[,3] ~ n_samp, type = "l")
text(c(100, 150, 100), c(0.15, 0.45, 0.85), labels = c("b1 = 0.2", "b1 = 0.6", "b1 = 1"),
     col = c("blue", "red", "black"))
abline(h = 0.8, lty = 2)


### So, how many samples should you take?











# No knowledge, 200 samples. Pilot data can be used to inform this.




# take aways from this example

# 1. It is hard to estimate parameters and test hyptotheses with noisy data.

# 2. Ecological data sets tend to be noisy, but at least with simulations
#    we can get an idea as to what we should expect.

# 3. If there are design decisions to make (# treatments vs. # replicates w/in
#    treatment), you can use these techniques to optimize power given time / money.

# 4. We did not address systematic bias or psuedo-replication,
#    but that will have a much larger impact on the quality of your data.
#    Thoughtful experimental design  (e.g.
#    measuring and accounting for mass, rainfall, etc., expand range of 
#    covariates tested, etc.) is always going to help.


