# Satoshi vs. Musk Text Analysis
# Brian Wickman
# December 2022

# Load packages
  library(tidyverse)
  library(data.table)
  library(ggthemes)
  library(ProbBayes)
  library(runjags)

# Load clean data
  data <- read.csv("~/GitProjects/Blog/MuskSatoshi/Data/Clean/SatoshiMusk.csv")

# Book Graphic
  ggplot(data %>% filter(word == 'the'), aes(author, per1000)) +
    geom_jitter(width = 0.1, size = 3, color = "royalblue") +
    coord_flip() +
    #theme_clean() +
    labs(
      y = "Rate",
      x = "Author",
      title = "Observed rates of the word 'the'"
    ) +
    theme(text = element_text(size = 18),
          plot.title = element_text(hjust = 0.5))
    

# Poisson Sampling Density (check dispersion) -----------------------------
  # Model string
    modelString = "
    model{
    ## sampling
    for (i in 1:N) {
    y[i] ~ dpois(n[i] * lambda / 1000)
    }
    ## prior
    lambda ~ dgamma(0.001, 0.001)
    }
    "
    
  # Data
    data1 <- data %>% filter(word == "and")
    y <- data1$freq
    n <- data1$total_words
    the_data <- list("y" = y, "n" = n, N = length(y))
    
  # Initial values
    initsfunction <- function(chain){
      .RNG.seed <- c(1,2)[chain]
      .RNG.name <- c("base::Super-Duper",
                     "base::Wichmann-Hill")[chain]
      return(list(.RNG.seed = .RNG.seed,
                  .RNG.name = .RNG.name))
    }
  
  # Simulated posterior draws
    posterior <- run.jags(modelString,
                          data = the_data,
                          monitor = c("lambda"),
                          n.chains = 1,
                          burnin = 2000,
                          sample = 5000,
                          inits = initsfunction)
    post <- as.mcmc(posterior)
    plot(post)
        
  # Compare sd of posterior draws to observed
    one_rep <- function(i){
      lambda <- post[i]
      sd(rpois(length(y), n * lambda / 1000))
    }
    SD <- sapply(1:5000, one_rep)
    
    ggplot(data.frame(sd = SD), aes(sd)) +
      geom_histogram(color = "black", fill = "white",
                     bins = 15) + theme(text = element_text(size = 18)) +
      geom_vline(xintercept = sd(y), size = 3 ) +
      ggplot2::annotate('text', x = 23, y = 950, label = "Observed", size = 5)
    (prob1 <- mean(SD >= sd(y))) # Reject null hypothesis
    

# Negative Binomial Model -------------------------------------------------
  # Prior
    modelString = "
    model{
    ## sampling
    for(i in 1:N){
    p[i] <- beta / (beta + n[i] / 1000)
    y[i] ~ dnegbin(p[i], alpha)
    }
    ## priors
    mu <- alpha / beta
    alpha ~ dgamma(.001, .001)
    beta ~ dgamma(.001, .001)
    }
    "
  # Likelihood
    data1 <- data %>% filter(word == "the")
    y <- data1$freq
    n <- data1$total_words
    the_data <- list("y" = y, "n" = n, N = length(y))
    
  # Initial values
    initsfunction <- function(chain){
      .RNG.seed <- c(1,2)[chain]
      .RNG.name <- c("base::Super-Duper",
                     "base::Wichmann-Hill")[chain]
      return(list(.RNG.seed = .RNG.seed,
                  .RNG.name = .RNG.name))
    }
  
  # Posterior draws  
    posterior <- run.jags(modelString,
                        data = the_data,
                        monitor = c("alpha", "beta", "mu"),
                        n.chains = 1,
                        burnin = 2000,
                        sample = 5000,
                        inits = initsfunction)
    post <- as.data.frame(as.mcmc(posterior))
    plot(posterior, vars = "mu")

  # Posterior predictive check
    one_rep <- function(i){
      p <- post$beta[i] / (post$beta[i] + n / 1000)
      sd(rnbinom(length(y), size = post$alpha[i], prob = p))
    }
    sapply(1:5000, one_rep) -> SD
      ggplot(data.frame(sd = SD), aes(sd)) +
      geom_histogram(color = "black", fill = "white",
                     bins = 15) + theme(text=element_text(size=18)) +
      geom_vline(xintercept = sd(y), size = 3 ) +
      ggplot2::annotate('text', x = 7, y = 1250,
               label = "Observed", size = 7)
    prob2 <- mean(SD >= sd(y)) # fail to reject null hypothesis
    
# Comparison of rates -----------------------------------------------------
   # Prior
      modelString = "
      model{
      ## sampling
      for(i in 1:N1){
      p1[i] <- beta1 / (beta1 + n1[i] / 1000)
      y1[i] ~ dnegbin(p1[i], alpha1)
      }
      for(i in 1:N2){
      p2[i] <- beta2 / (beta2 + n2[i] / 1000)
      y2[i] ~ dnegbin(p2[i], alpha2)
      }
      ## priors
      alpha1 ~ dgamma(.001, .001)
      beta1 ~ dgamma(.001, .001)
      alpha2 ~ dgamma(.001, .001)
      beta2 ~ dgamma(.001, .001)
      ratio <- (alpha2 / beta2) / (alpha1 / beta1)
      }
      "
  # Initial values  
    initsfunction <- function(chain){
      .RNG.seed <- c(1,2)[chain]
      .RNG.name <- c("base::Super-Duper",
                     "base::Wichmann-Hill")[chain]
      return(list(.RNG.seed = .RNG.seed,
                  .RNG.name = .RNG.name))
    }
    
  # Select 12 most used words
    pop_words <- data %>%
      group_by(word) %>%
      summarise(sum = sum(freq)) %>%
      arrange(desc(sum)) %>%
      filter(row_number() <= 12)
    
  # Define function for multiple words  
    bayes_one_word <- function(theword) {
      d1 <- filter(data, author == "musk",
                   word == theword)
      d2 <- filter(data, author == "satoshi",
                   word == theword)
      the_data <- list("y1" = d1$freq,
                       "n1" = d1$total_words,
                       "N1" = length(d1$freq),
                       "y2" = d2$freq,
                       "n2" = d2$total_words,
                       "N2" = length(d2$freq))
      posterior <- run.jags(modelString,
                            data = the_data,
                            monitor = c("ratio"),
                            n.chains = 1,
                            burnin = 2000,
                            sample = 5000,
                            inits = initsfunction)
      quantile(as.mcmc(posterior$mcmc), c(.025, .5, .975))
    }  
    
  # Apply function to pop_words
    S <- sapply(pop_words$word, bayes_one_word)
    df <- data.frame(Word = pop_words$word,
                     LO = S[1, ],
                     M = S[2, ],
                     HI = S[3, ]) %>%
      filter(Word != "could")
    
  # Plot
    ggplot(df, aes(x = Word, y = M)) +
      geom_errorbar(aes(ymin = LO, ymax = HI), width = 0.3, size = 1) +
      geom_point() + coord_flip() +
      theme_clean() +
      theme(text = element_text(size = 18),
            plot.title = element_text(hjust = 0.5)) +
      ylab("Ratio") +
      geom_hline(yintercept = 1) +
      labs(
        title = "Posterior Distribution of Ten Words"
      ) +
      ggplot2::annotate('text', x = 5.5, y = 0.45,
                        label = "Musk", size = 5) +
      ggplot2::annotate('text', x = 8.5, y = 1.4,
                        label = "Nakamoto", size = 5)
    
    

    