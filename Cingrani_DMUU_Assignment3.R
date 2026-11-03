rm(list = ls())
library(tibble)
library(dplyr)
library(ggplot2)
options(scipen = 999)

# Inputs
months <- 18
rate   <- 0.015
cost   <- 1200 * 100      # $120,000 per month

annuity_factor  <- (1 - (1 + rate)^-months) / rate   # 18 monthly payments
discount_factor <- (1 + rate)^-months                # payment at end of month 18

# Original proposal
ev_original <- (160000 - cost) * annuity_factor
ev_original

# Option A
ev_option_a <- (152000 - cost) * annuity_factor
ev_option_a

# Option B: $125K
ev_option_b <- (125000 - cost) * annuity_factor + 0.7 * 800000 * discount_factor
ev_option_b

# Option C
set.seed(15)
n_sims <- 10000

triangle <- function(n, low, mode, high) {
     u  <- runif(n)
     Fc <- (mode - low) / (high - low)
     ifelse(u < Fc,
            low + sqrt(u * (high - low) * (mode - low)),
            high - sqrt((1 - u) * (high - low) * (high - mode)))
}

sim <- tibble(savings = triangle(n_sims, low = 3.2e6, mode = 5.6e6, high = 12.8e6)) %>%
     mutate(share = case_when(
          savings < 4e6 ~ 0,
          savings < 6e6 ~ 0.20 * (savings - 4e6),
          savings < 8e6 ~ 4e5 + 0.40 * (savings - 6e6),
          TRUE          ~ 1.2e6 + 0.60 * (savings - 8e6)
     ))

mean_share   <- mean(sim$share)
median_share <- median(sim$share)
mean_share
median_share

ev_option_c <- 0.4 * ((150000 - cost) * annuity_factor + mean_share * discount_factor)
ev_option_c

# Histogram 
ggplot(sim, aes(x = share)) +
     geom_histogram(bins = 50, boundary = 0) +
     geom_vline(data = tibble(stat  = c("Mean", "Median"),
                              value = c(mean_share, median_share)),
                aes(xintercept = value, color = stat), linewidth = 1) +
     labs(
          title    = "Option C: Simulated share of savings",
          subtitle = paste("Number of simulations:", n_sims),
          x = "Share of savings ($)",
          y = "Count",
          color = NULL
     ) + theme_minimal()