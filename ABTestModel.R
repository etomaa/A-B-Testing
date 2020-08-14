

library(tidyverse)

Compute_ABTest_Results <- function(loadfile) {
  ABTest <- read.csv(loadfile, header = TRUE)
  conversion_subset_A <-
    ABTest %>% filter(variant == "A" & converted == "TRUE")
  conversions_A <- nrow(conversion_subset_A)
  visitors_A <- nrow(ABTest %>% filter(variant == "A"))
  conv_rate_A <-  (conversions_A / visitors_A)
  conversion_subset_B <-
    ABTest %>% filter(variant == "B" & converted == "TRUE")
  conversions_B <- nrow(conversion_subset_B)
  visitors_B <- nrow(ABTest %>% filter(variant == "B"))
  conv_rate_B <-  (conversions_B / visitors_B)
  uplift <-
    (conv_rate_B - conv_rate_A) / conv_rate_A * 100
  p_pool <-
    (conversions_A + conversions_B) / (visitors_A + visitors_B)
  SE_pool <-
    sqrt(p_pool * (1 - p_pool) * ((1 / visitors_A) + (1 / visitors_B)))
  MOE <- SE_pool * qnorm(0.975)
  d_hat <- conv_rate_B - conv_rate_A
  z_score <- d_hat / SE_pool
  p_value <- pnorm(q = -z_score,
                   mean = 0,
                   sd = 1) * 2
  lower   = d_hat - qnorm(0.975) * SE_pool
  upper   = d_hat + qnorm(0.975) * SE_pool
  vis_result_pool <- data.frame(
    metric = c(
      'Estimated Difference',
      'Relative Uplift(%)',
      'pooled sample proportion',
      'Standard Error of Difference',
      'z_score',
      'p-value',
      'Margin of Error',
      'CI-lower',
      'CI-upper'
    ),
    value = c(
      conv_rate_B - conv_rate_A,
      uplift,
      p_pool,
      SE_pool,
      z_score,
      p_value,
      MOE,
      ci_lower,
      ci_upper
    )
  )
  return(vis_result_pool)
}
           
   Compute_ABTest_Results("Website Results.csv")
           
           
           
          
           