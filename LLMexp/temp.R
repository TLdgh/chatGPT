#RCD

#randomization:
trt_levels = rep(c("F1","F2","F3","Control"),times= 6)
data.frame(plant=1:24, Treatment=trt_levels)%>%mutate(Assign=sample(trt_levels))


#example:
ex1=tibble(Obs=c(575,565,600,725,
                 542,593,651,700,
                 530,590,610,715,
                 539,579,637,685,
                 570,610,629,710),ID=rep(1:4,times=5))
fitex1=lm(Obs~as.factor(ID), ex1)
anova(fitex1)
ex1%>%mutate(e=fitex1$residuals)%>%arrange(e)%>%mutate(rank=1:n(), theoq=qnorm((rank-0.5)/n()))%>%
  mutate(q0.25 = quantile(e,0.25), q0.75=quantile(e,0.75), theoq0.25=qnorm(0.25), theoq0.75=qnorm(0.75), 
         slope=(q0.75 - q0.25) / (theoq0.75 - theoq0.25), intercept=q0.25-slope*theoq0.25, qqline=intercept+slope*theoq)%>%
  plot_ly(x=~theoq, y=~e,  type='scatter')%>%
  add_trace(x=~theoq, y=~qqline, type = 'scatter', mode = 'lines', line = list(dash = 'dash'), showlegend = FALSE)







#RCBD

# randomization
trt_levels=rep(c("M1", "M2","M3","M4"), times = 6) #there are 6 blocks
data.frame(
  Block = as.factor(rep(1:6, each=4)), #there are 4 treatment levels
  Treatment = trt_levels
)%>%group_by(Block)%>%mutate(Assign=sample(Treatment))%>%print(n=24)



#example:
levels=factor(1:4)
ex2=tibble(T=rep(levels,times=6),Obs=c(90.3,92.5,85.5,82.5,89.2,89.5,90.8,89.5,
                                       98.2,90.6,89.6,85.6,93.9,94.7,86.2,87.4,
                                       87.4,87,88,78.9,97.9,95.8,93.4,90.7))%>%
  mutate(B=rep(1:6,each=4))%>%mutate(B=as.factor(B), T=as.factor(T))
fitex2=lm(Obs~T+B, ex2)
anova(fitex2)
ex2%>%mutate(e=fitex2$residuals)%>%arrange(e)%>%qqplotly()





# Example data structure
data <- tibble(
  NYear=c(5.8,15,21.5,27.5,33.5,39.5,46,51.5),
  Nsevere=c(0,1,3,8,9,8,10,5),
  Nminers=c(98,54,43,48,51,38,28,11)
)

# Fit a GLM with binomial distribution
model <- glm(cbind(Nsevere ,Nminers - Nsevere) ~ NYear, 
             family = binomial(), data = data)
nullmodel <- glm(cbind(Nsevere ,Nminers - Nsevere) ~ 1, 
                 family = binomial(), data = data)
# Summarize the model
summary(model)
anova(model)
anova(nullmodel, model, test="LRT")
qqnorm(residuals(model, type = "deviance"))
qqline(residuals(model, type = "deviance"))









### MH for Bayesian Normal-Normal where Y|mu ~ N(mu, 0.75^2) and mu~N(0,1). Given Y=6.25, we should have mu|Y=6.25 ~ N(4, 0.6^2)

MHalg<-function(initialx, e, N=10000){
  X=c(initialx, rep(initialx, N))
  
  for(t in 1:N){
    proposal=X[t]+runif(1,-e, e) #step1 propose a value 
    
    # step2: compute acceptance probability
    numerator <- dnorm(proposal, 0, 1) * dnorm(6.25, proposal, 0.75)
    denominator  <- dnorm(X[t], 0, 1) * dnorm(6.25, X[t], 0.75)
    
    alpha = min(1, numerator/denominator)
    
    # step3: accept proposal with probability = alpha otherwise Xt with 1-alpha
    u=rbinom(1,1,alpha)
    X[t+1]=proposal*u + X[t]*(1-u)
  }
  return(X)
}

y=MHalg(1,3,N=100000)

data.frame(y)%>%plot_ly()%>%
  add_histogram(x = ~y, opacity = 0.6, histnorm = 'probability density') %>%
  # Add density curve
  add_lines(
    x = ~density(y)$x,
    y = ~density(y)$y,
    name = 'Density Curve',
    line = list(color = 'red')
  )%>%add_lines(x=~density(y)$x, y = ~dnorm(density(y)$x, 4,0.6))






### MH for Bayesian beta-binomial conjugate pair
MHBayes<-function(a,b, N=10000){
  initialx=rbeta(1, a, b)
  X=c(initialx, rep(initialx, N))
  
  for(t in 1:N){
    proposal=rbeta(1, a, b)
    
    numerator=dbeta(proposal, 2,3)*dbinom(x=1, size = 2, prob = proposal)
    numeratorq=dbeta(X[t], a, b)
    
    denominator=dbeta(X[t], 2,3)*dbinom(x=1, size = 2, prob = X[t])
    denominatorq=dbeta(proposal, a, b)
    
    alpha = min(1, numerator*numeratorq/(denominator*denominatorq))
    
    u=rbinom(1,1,alpha)
    X[t+1]=proposal*u + X[t]*(1-u)
  }
  
  return(X)
}

y=MHBayes(1,1,N=100000)

data.frame(y)%>%plot_ly() %>%
  # Add histogram
  add_histogram(x = ~y, name = 'Histogram', opacity = 0.6, histnorm = 'probability density') %>%
  # Add density curve
  add_lines(
    x = ~density(y)$x,
    y = ~density(y)$y,
    name = 'Density Curve',
    line = list(color = 'red')
  )%>%add_lines(x=~density(y)$x, y = ~dbeta(density(y)$x, 3,4))












### Bayesian logistic regression simulation


set.seed(123)

# Define the levels of the single categorical predictor
X_levels <- c("A", "B")
n <- 10000  # Total number of observations

# Simulate categorical predictor X
X <- sample(X_levels, size = n, replace = TRUE)

# Define logistic regression parameters
beta_0 <- -0.54          # Intercept
beta <- c(A=0, B = 3.9)  # Coefficients for each level of X

# Compute log-odds and probabilities
log_odds <- beta_0 + beta[X]
probabilities <- 1 / (1 + exp(-log_odds))  # Logistic function

# Simulate binary response Y
Y <- rbinom(n, size = 1, prob = probabilities)

# Combine into a data frame
simulated_data <- data.frame(X = X, Y = Y)

fit <- glm(Y ~ X, data = simulated_data, family = binomial)

# Summary of the model
summary(fit)



simulated_data=simulated_data%>%group_by(X)%>%summarise(Y=sum(Y), Total=n())
fit2 <- glm(cbind(Y, Total-Y) ~ X, data = simulated_data, family = binomial(link = 'logit'))
# Summary of the model
summary(fit2)









simulated_data=simulated_data%>%mutate(X=ifelse(X == "B", 1, 0))

predictive_check <- function(posterior_samples, simulated_data) {
  n_samples <- nrow(posterior_samples)
  simulated_Y <- matrix(NA, ncol = nrow(simulated_data), nrow = n_samples)
  
  for (i in 1:n_samples) {
    beta0 <- posterior_samples[i, 1]
    beta1 <- posterior_samples[i, 2]
    
    log_odds <- beta0 + beta1 * simulated_data$X  # Compute log-odds
    p <- plogis(log_odds)  # Convert log-odds to probability
    
    # Simulate new counts of successes Ypred ~ Binomial(Total, p)
    simulated_Y[i, ] <- rbinom(n = nrow(simulated_data), size = simulated_data$Total, prob = p)
  }
  
  # Summarize observed and predicted distributions
  observed_success_rate <- simulated_data$Y / simulated_data$Total  # Observed success rates
  predicted_success_rate <- simulated_Y%>%as_tibble()%>%map2(., simulated_data$Total, ~(.x/.y))%>%as.data.frame() # Predicted success rates
  colnames(predicted_success_rate)=c("X0","X1")
  
  # Interactive Plotly histogram
  plot<-predicted_success_rate%>%pivot_longer(everything(),names_to = "XType", values_to = "Value")%>%
    plot_ly()%>%add_histogram(x =~Value, color = ~XType, histnorm = "probability density",xbins = list(size = 0.001))%>%
    add_segments(
      x = observed_success_rate[1], 
      xend = observed_success_rate[1], 
      y = 0, 
      yend = 45, 
      line = list(color = "red", dash = "dash", width = 2), 
      name = "Observed X0",
      inherit = FALSE
    ) %>%
    add_segments(
      x = observed_success_rate[2], 
      xend = observed_success_rate[2], 
      y = 0, 
      yend = 110, 
      line = list(color = "blue", dash = "dash", width = 2), 
      name = "Observed X1",
      inherit = FALSE
    ) %>%
    layout(title = "Posterior Predictive Check",
           xaxis = list(title = "Success Rate"),
           yaxis = list(title = "Frequency"))
  
  return(plot)
}


# Log-likelihood function for logistic regression
log_likelihood <- function(beta0, beta1, X, Y, n) {
  log_odds <- beta0 + beta1 * X
  p <-plogis(log_odds)
  sum(Y * log(p) + (n - Y) * log(1 - p))
}



# Prior distributions (normal priors for intercept and slope)
log_prior <- function(beta0, beta1) {
  dnorm(beta0, mean = 0, sd = 5, log = TRUE) +  # Prior for beta0
    dnorm(beta1, mean = 0, sd = 5, log = TRUE)  # Prior for beta1
}


# Posterior function: log-posterior = log-likelihood + log-prior
log_posterior <- function(beta0, beta1, X, Y, n) {
  log_likelihood(beta0, beta1, X, Y, n) + log_prior(beta0, beta1)
}



# Metropolis-Hastings algorithm
metropolis_hastings <- function(data, n_iter, init, proposal_sd){
  X=data$X
  Y=data$Y
  n=data$Total
  
  samples <- matrix(NA, ncol = 2, nrow = n_iter)
  colnames(samples) <- c("beta0", "beta1")
  current <- init
  samples[1, ] <- current
  
  for (i in 2:n_iter) {
    # Propose new values
    proposal <- rnorm(2, mean = current, sd = proposal_sd)
    
    # Compute acceptance ratio
    log_alpha <- log_posterior(proposal[1], proposal[2], X, Y, n) + log_prior(current[1], current[2]) -
      log_posterior(current[1], current[2], X, Y, n) -log_prior(proposal[1], proposal[2])
    alpha <- exp(log_alpha)
    
    # Accept or reject
    if (runif(1) < alpha) {
      current <- proposal
    }
    samples[i, ] <- current
  }
  
  return(samples)
}

set.seed(100)
# Run Metropolis-Hastings
n_iter <- 100000
init <- c(-0.5, 4)  # Initial values for beta0 and beta1
proposal_sd <- c(0.1, 0.1)  # Proposal standard deviations

samples <- metropolis_hastings(simulated_data, n_iter, init, proposal_sd)

# Analyze the results
burn_in <- 1000
posterior_samples <- samples[-(1:burn_in), ]

# Summary of posterior estimates
posterior_mean <- colMeans(posterior_samples)
posterior_sd <- apply(posterior_samples, 2, sd)
rbind(posterior_mean,posterior_sd)
predictive_check(posterior_samples,simulated_data)













# Bayesian logistic regression - Multiple predictors
set.seed(123)

# Define the levels of the categorical predictor
X_levels <- c("A", "B")
Z_levels <-paste0("L", 1:8)

n <- 10000  # Total number of observations

# Simulate categorical predictor X
X <- sample(X_levels, size = n, replace = TRUE)
Z <-sample(Z_levels, size = n, replace = TRUE)

# Define logistic regression parameters
beta0 <- c(b0=-0.54)          # Intercept
betaX <- c(A=0, B = 3.9)  # Coefficients for each level of X
betaZ <-c(L1=0, L2 =3.9, L3=2.5, L4=-0.5, L5=-2.11, L6=-0.48, L7=-0.7, L8=4.168)
betas=c(beta0, betaX, betaZ)

# Combine into a data frame
simulated_data <- tibble(X=factor(X), Z=factor(Z), b0=betas['b0'], bX=betas[as.character(X)], bZ=betas[as.character(Z)], log_odds=b0+bX+bZ, p=1/(1 + exp(-log_odds)), Y=rbinom(n=10000, size = 1, prob = p))

fit <- glm(Y ~ X+Z, data = simulated_data, family = binomial)

# Summary of the model
summary(fit)




# Log-likelihood function for logistic regression with two predictors X and Z (factor with 8 levels)
log_likelihood <- function(betas, data) {
  data%>%mutate(b0=betas['b0'], bX=ifelse(X=="B",betas[as.character(X)],0), bZ=ifelse(Z!="L1",betas[as.character(Z)],0), log_odds=b0+bX+bZ, p=plogis(log_odds), l=Y*log(p)+(Total-Y)*log(1-p))%>%
    summarise(sum(l))%>%as.numeric()
}


# Prior distributions (normal priors for intercept, X coefficient, and the 7 coefficients for Z levels)
log_prior <- function(betas) {
  dnorm(betas['b0'], mean = 0, sd = 5, log = TRUE) +  # Prior for beta0
    dnorm(betas['B'], mean = 0, sd = 5, log = TRUE) +  # Prior for beta1 (X)
    sum(dnorm(betas[c(paste0('L',2:8))], mean = 0, sd = 5, log = TRUE))  # Prior for the 7 Z coefficients
}


# Posterior function: log-posterior = log-likelihood + log-prior
log_posterior <- function(betas, data) {
  log_likelihood(betas, data) + log_prior(betas)
}


# Metropolis-Hastings algorithm for logistic regression with 1 binary response and 2 predictors (X and Z)
metropolis_hastings <- function(data, n_iter, init, proposal_sd) {
  param_names=c(levels(data$X)[-1], levels(data$Z)[-1])
  
  samples <- matrix(NA, ncol = 9, nrow = n_iter)  # 1 intercept + 1 X coefficient + 7 Z coefficients
  current <- init
  samples[1, ] <- current
  
  for (i in 2:n_iter) {
    # Propose new values (now for 9 parameters)
    proposal <- rnorm(9, mean = current, sd = proposal_sd)
    names(proposal)=names(current)
    
    # Compute acceptance ratio
    log_alpha <- log_posterior(betas = proposal, data) + log_prior(betas=current) -
      log_posterior(betas=current, data) - log_prior(betas=proposal)
    
    alpha <- exp(log_alpha)
    
    # Accept or reject
    if (runif(1) < alpha) {
      current <- proposal
    }
    samples[i, ] <- current
  }
  
  return(samples)
}



# Initial values for the parameters (beta0, beta1, beta2_1, ..., beta2_7)
init_values <- c(0, 0, rep(0, 7))  # 1 intercept, 1 X coefficient, 7 Z coefficients
names(init_values) = c('b0','B',paste0('L',2:8))
# Proposal standard deviations (you can adjust these values)
proposal_sd <- rep(0.1, 9)

# Number of iterations
n_iter <- 100000
data = simulated_data%>%group_by(X,Z)%>%summarise(Y=sum(Y), Total=n(), .groups = "drop")%>%ungroup()

set.seed(100)
# Run Metropolis-Hastings
samples <- metropolis_hastings(data, n_iter, init_values, proposal_sd)

posterior_samples <- samples[-(1:1000), ]

# Summary of posterior estimates
posterior_mean <- colMeans(posterior_samples)
posterior_sd <- apply(posterior_samples, 2, sd)
rbind(posterior_mean,posterior_sd)


