qqplotly<-function(data){
  data%>%mutate(rank=1:n(), theoq=qnorm((rank-0.5)/n()))%>%
    mutate(q0.25 = quantile(e,0.25), q0.75=quantile(e,0.75), theoq0.25=qnorm(0.25), theoq0.75=qnorm(0.75), 
           slope=(q0.75 - q0.25) / (theoq0.75 - theoq0.25), intercept=q0.25-slope*theoq0.25, qqline=intercept+slope*theoq)%>%
    plot_ly(x=~theoq, y=~e,  type='scatter', mode ='markers')%>%
    add_trace(x=~theoq, y=~qqline, type = 'scatter', mode = 'lines', line = list(dash = 'dash'), showlegend = FALSE)
}



CI_z <- function(data, threshold, alphalevel) {
  n <- length(data)
  p_hat <- sum(data <= threshold) / n
  
  # Compute the confidence interval
  error <- qnorm(alphalevel / 2) * sqrt(p_hat * (1 - p_hat) / n)
  CI <- c(p_hat + error, p_hat - error)
  
  # Format as percentages
  CI <- sprintf("%.2f%%", CI * 100)
  p_hat_formatted <- sprintf("%.2f%%", p_hat * 100)
  
  # Print the results
  cat("The maximum likelihood estimate is:", p_hat_formatted, "\n")
  cat("The", 100 * (1 - alphalevel), "%-level confidence interval for the distance measure is:", CI, "\n")
}





Bayes <- setRefClass(
  "Bayes",
  fields = list(
    data = "tbl_df", 
    alpha = "numeric", 
    beta = "numeric", 
    alphalevel = "numeric", 
    result = "data.frame"
  ),
  
  methods = list(
    initialize = function(data, alpha, beta, alphalevel) {
      .self$data <- data
      .self$alpha <- alpha
      .self$beta <- beta
      .self$alphalevel <- alphalevel
      
      .self$result <- .self$CI_Bayes()
    },
    
    CI_Bayes = function(){
      res <- list()
      for (i in 1:nrow(.self$data)) {
        model<-as.character(.self$data[[i, "Model"]])
        primarycat<-as.character(.self$data[[i, "Primary_Cat"]])
        x <- .self$data[i, "CountAuth"]%>%as.numeric()
        n <- 200
        
        
        param_1 <- .self$alpha + x
        param_2 <- .self$beta + n - x
        
        
        CI <- qbeta(c(.self$alphalevel / 2, 1 - .self$alphalevel / 2), param_1, param_2) %>% round(4)
        res[[i]] <- data.frame(
          param_1,
          param_2,
          round(param_1 / (param_1 + param_2), 4),
          round((param_1 * param_2) / ((param_1 + param_2)^2 * (param_1 + param_2 + 1)), 6),
          CI[1],
          CI[2],
          paste0("[", CI[1], ", ", CI[2], "]"),
          model,primarycat
        )
      }
      
      res <- do.call(rbind, res)%>%
        setNames(c(
          "alpha", 
          "beta", 
          "Expected Success Probability", 
          "Variance of Success Probability", 
          "CI_L",
          "CI_U",
          sprintf("%.2f%%-level Credible Interval", (1 - .self$alphalevel) * 100),
          "Model","Primary_Cat"
        ))%>%mutate(CI_L=`Expected Success Probability` - CI_L, CI_U=`Expected Success Probability` - CI_U)
      return(res)
    },
    
    Plot_Bayes = function(yrange = c(0, 1, 0.05)) {
      fig <- plot_ly(
        data = .self$result,
        x = ~Primary_Cat,
        y = ~`Expected Success Probability`,
        color = ~Model,
        colors = "Set1",
        opacity=0.5,
        type = "bar",
        text = ~paste0(round(`Expected Success Probability` * 100, 2), "%"),
        textposition = "outside",
        error_y = list(
          type = "data",
          symmetric = TRUE,
          array = ~CI_U,
          arrayminus = ~CI_L,
          color='black'
        )
      ) %>%
        add_trace(
          
        )%>%
        layout(
          yaxis = list(range = yrange[1:2], dtick = yrange[3]),
          title = list(
            text = sprintf("Expected Success Probability with %.2f%% Confidence Level", (1 - .self$alphalevel) * 100),
            x = 0.5
          )
        )
      return(fig)
    },
    
    
    effCP = function(n, p) {
      # Generate a sequence of k values from 0 to n
      k <- 0:n
      
      # Compute parameters for the Bayesian approach
      param_1 <- .self$alpha + k
      param_2 <- .self$beta + n - k
      
      # Compute Wald confidence intervals
      CI_Wald <- lapply(k, function(x) {
        phat <- x / n
        error <- qnorm(.self$alphalevel / 2) * sqrt((phat * (1 - phat)) / n)
        c(phat + error, phat - error)
      })
      
      # Compute Bayesian confidence intervals
      CI_Bayes <- mapply(function(x, y) {
        qbeta(c(.self$alphalevel / 2, 1 - .self$alphalevel / 2), x, y)
      }, param_1, param_2, SIMPLIFY = FALSE)
      
      # Determine if p is covered by each interval
      Covered_Wald <- sapply(CI_Wald, function(ci) as.integer(p >= ci[1] && p <= ci[2]))
      Covered_Bayes <- sapply(CI_Bayes, function(ci) as.integer(p >= ci[1] && p <= ci[2]))
      
      # Compute binomial probabilities
      binom_probs <- dbinom(k, size = n, prob = p)
      
      # Calculate coverage probabilities
      CP_Wald <- Covered_Wald * binom_probs
      CP_Bayes <- Covered_Bayes * binom_probs
      
      # Return the sum of coverage probabilities
      return(c(sum(CP_Wald), sum(CP_Bayes)))
    },
    
    
    MeanEffCov = function(n, p) {
      # Initialize a list to store results
      effData <- lapply(p, function(i) effCP(n, i))
      
      # Convert the list to a data frame
      effData <- do.call(rbind, effData)
      colnames(effData) <- c("CP_Wald", "CP_Bayes")
      
      # Compute the means for CP_Wald and CP_Bayes
      mean_CP_Wald <- mean(effData[, "CP_Wald"])
      mean_CP_Bayes <- mean(effData[, "CP_Bayes"])
      
      return(c(mean_CP_Wald, mean_CP_Bayes))
    },
    
    
    simBayesCoverage = function(){
      # Parameters
      n <- seq(10, 340, by = 10) # Equivalent to range(10, 350, 10)
      p <- seq(0.01, 0.99, length.out = 200) # Equivalent to np.linspace(0.01, 0.99, 200)
      
      # Calculate MeanEffCov for each sample size
      MeanEffData <- lapply(n, function(size) {
        MeanEffCov(size, p)
      })
      
      # Convert list to a data frame
      MeanEffData <- do.call(rbind, MeanEffData)
      colnames(MeanEffData) <- c("CP_Wald", "CP_Bayes")
      MeanEffData <- data.frame(n = n, MeanEffData)
      
      # Reshape the data for plotting
      MeanEffData <- MeanEffData %>%
        pivot_longer(cols = c("CP_Wald", "CP_Bayes"), names_to = "Method", values_to = "Coverage_Probability")
      
      # Create the plot with Plotly
      fig <- plot_ly(
        data = MeanEffData,
        x = ~n,
        y = ~Coverage_Probability,
        color = ~Method,
        type = "scatter",
        mode = "lines"
      )
      
      # Add layout settings
      fig <- fig %>%
        layout(
          title = list(text = "Mean Coverage Probability for 95% Confidence Level", x = 0.5),
          xaxis = list(
            title = "Sample Size"),
          yaxis = list(
            title = "Coverage Probability")
        )
      
      # Add horizontal line at y = 0.95
      fig <- fig %>%
        add_lines(
          x = ~n,
          y = ~rep(0.95, length(n)),
          line = list(dash = "dash", color = "black", width = 0.5),
          showlegend = FALSE
        )
      
      # Display the plot
      return(fig)
    }
  )
)









# Define the LogisticRegression class
BayesianLogisticRegression <- R6Class(
  "LogisticRegression",
  public = list(
    data = NULL,
    formula = NULL,
    predictor_vars = NULL,
    response_var = NULL,
    posterior_samples = NULL,
    
    initialize = function(formula, data) {
      self$data <- data
      self$formula <- formula
      terms_obj <- terms(formula)
      self$response_var <- as.character(attr(terms_obj, "variables"))[2]  # Extract response variable
      self$predictor_vars <- attr(terms_obj, "term.labels")  # Extract predictor variables
    },
    
    # Log-likelihood function
    log_likelihood = function(betas) {
      df <- self$data %>%
        mutate(
          b0 = betas["Intercept"],  # Intercept
          
          # Apply categorical encoding: remove first level as reference
          b_predictors = rowSums(sapply(self$predictor_vars, function(var) {
            if (is.factor(self$data[[var]])) {
              ifelse(self$data[[var]] != levels(self$data[[var]])[1], betas[as.character(self$data[[var]])], 0)
            } else {
              betas[var] * self$data[[var]]  # Continuous variables remain unchanged
            }
          })),
          
          log_odds = b0 + b_predictors,
          p = plogis(log_odds),
          l = self$data[[self$response_var]] * log(p) + (self$data$Total - self$data[[self$response_var]]) * log(1 - p)
        )
      
      return(sum(df$l))
    },    
    
    # Prior distribution
    log_prior = function(betas) {
      sum(dnorm(betas, mean = 0, sd = 5, log = TRUE))  # Normal priors for all coefficients
    },
    
    # Posterior function
    log_posterior = function(betas) {
      self$log_likelihood(betas) + self$log_prior(betas)
    },
    
    # Metropolis-Hastings algorithm
    metropolis_hastings = function(n_iter, init, proposal_sd) {
      param_names <- c("Intercept", unique(unlist(sapply(self$predictor_vars, function(var) levels(self$data[[var]])[-1] ))))
      names(init) <- param_names
      
      samples <- matrix(NA, ncol = length(param_names), nrow = n_iter)
      colnames(samples) <- param_names
      current <- init
      samples[1, ] <- current
      
      for (i in 2:n_iter) {
        proposal <- rnorm(length(param_names), mean = current, sd = proposal_sd)
        names(proposal) <- param_names
        
        log_alpha <- self$log_posterior(proposal) + self$log_prior(current) - self$log_posterior(current) - self$log_prior(proposal)
        
        alpha <- exp(log_alpha)
        
        if (runif(1) < alpha) {
          current <- proposal
        }
        samples[i, ] <- current
      }
      
      self$posterior_samples <- samples[-(1:1000), , drop = FALSE]  # Remove burn-in
      
      posterior_mean <- colMeans(self$posterior_samples)
      posterior_sd <- apply(self$posterior_samples, 2, sd)
      res <- data.frame(Mean = posterior_mean, SD = posterior_sd)
      
      return(res)
    },
    
    predictive_check = function(posterior_samples, data) {
      # Initialize matrix to hold simulated Y values
      simulated_Y <- matrix(NA, nrow = nrow(posterior_samples), ncol = nrow(data))
      
      legends=c()
      # Loop through each posterior sample and simulate Y values
      for (i in 1:nrow(data)) {
        lev=sapply(self$predictor_vars, function(x) as.character(data[[i,x]]))
        legends=c(legends, paste0(lev, collapse = ""))
        
        # Start with the intercept (always present)
        log_odds <- posterior_samples[, 'Intercept']
        
        for(l in lev){
          if (l %in% colnames(posterior_samples)) {
            log_odds <- log_odds + posterior_samples[, l]
          }
        }
        
        probs <- plogis(log_odds)
        # Simulate Y using binomial distribution
        simulated_Y[,i] <- sapply(probs, function(p){rbinom(n = 1, size = data$Total[i], prob=p)})
      }
      
      observed_success_rate <- data[,self$response_var] / data$Total  # Observed success rates
      predicted_success_rate <- simulated_Y%>%as_tibble()%>%map2(., data$Total, ~(.x/.y))%>%as.data.frame() # Predicted success rates
      colnames(predicted_success_rate) = legends
      
      plots <- list()
      for (j in 1:16) {
        vline=density(predicted_success_rate[,j])
        vline=max(vline$y)
        
        df=data.frame(X=predicted_success_rate[,j])
        # Create a histogram for the jth column of predicted_success_rate
        p <- df%>%plot_ly()%>%
          add_histogram(x=~X, histnorm = "probability density", name = legends[j]) %>%
          add_segments(
            x = observed_success_rate[j,], 
            xend = observed_success_rate[j,], 
            y = 0, 
            yend =vline, 
            line = list(color = 'black', dash = "dash", width = 3),
            inherit = FALSE, showlegend=FALSE
          )
        
        # Add the plot to the list
        plots[[j]] <- p
      }  
      
      # Create a subplot grid 4x4
      subplot(plots, nrows = 4 )%>%
        layout(title = "Posterior Predictive Check")
    }
    
  )
)















