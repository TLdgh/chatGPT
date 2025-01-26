



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
        n <- 50
        
        
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













