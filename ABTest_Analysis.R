#Check what can be summarized or aggregated

#ABTest$session_id <- as.factor(ABTest$session_id) #Change from fact to dimension

#loading relevant packages
library(tidyverse)

#2a.Analysis  ---------------------------------------------------------------

#let's take a subset of conversions for variant_A 
conversion_subset_A <- ABTest %>% filter(variant == "A" & converted == "TRUE")

#Number of Conversions for variant_A
conversions_A <- nrow(conversion_subset_A)

#Number of Visitors for variant_A
visitors_A <- nrow(ABTest %>% filter(variant == "A"))
#Conversion_rate_A
conv_rate_A <-  (conversions_A/visitors_A)  
conv_rate_A


#let's take a subset of conversions for variant_B
conversion_subset_B <- ABTest %>% filter(variant == "B" & converted == "TRUE")

#Number of Conversions for variant_B
conversions_B <- nrow(conversion_subset_B)

#Number of Visitors for variant_B
visitors_B <- nrow(ABTest %>% filter(variant == "B"))
#Conversion_rate_A
conv_rate_B <-  (conversions_B/visitors_B)  
conv_rate_B

#2b.Analysis----------------------------------------
#1. Let's Calculate Relative uplift in Conversion Rate ----------------------

uplift <- (conv_rate_B - conv_rate_A)/ conv_rate_A * 100
uplift  #82.72%
#B is better than A by 83%. This is high enough to decide a winner.

#2. Let's compute pooled probability for variants A & B
p_pool <- (conversions_A + conversions_B)/(visitors_A + visitors_B)
p_pool  # 0.03928325

#3. Let's compute the standard error (SE_pool)
SE_pool<- sqrt(p_pool*(1-p_pool) * ((1/visitors_A) + (1/visitors_B)))
SE_pool  #0.01020014

#4. Let's compute the margin of error for the pool
MOE <- SE_pool * qnorm(0.975)
MOE <-  #0.0199919
  
  #5. Point Estimate or Difference in proportion
d_hat <- conv_rate_B - conv_rate_A
d_hat  # 0.02294568 this is the point estimate

#6. Compute the Z-score so we can determine the p-value
z_score <- d_hat/SE_pool
z_score #2.249546

# Let's calculate P-value and confidence interval -------------------------
#Method 1
#7. Let's compute p_value using the z_score value
p_value <- pnorm(q = -z_score, mean = 0, sd = 1) * 2
p_value #0.02447777

#8. Let's compute Confidence interval
#d-hat is greater than the practical significance level of 0.02. Hence i will launch the change in version B
#Method 1
ci <- c(d_hat - MOE, d_hat + MOE) #0.002953777 0.042937584
ci  
ci_lower <- d_hat - MOE
ci_lower 
ci_upper <- d_hat + MOE
ci_upper 

#Method 2
lower   = d_hat - qnorm(0.975)*SE_pool
upper   = d_hat + qnorm(0.975)*SE_pool

ci <- c(d_hat - qnorm(0.975)*SE_pool, d_hat + qnorm(0.975)*SE_pool) #0.001305794 0.039727961
ci     

#3. Let's copmpute SE and CI for test variant A and B separately
# Compute standard error of variant_B

X_hat_B <- conversions_B/visitors_B
X_hat_B  #0.05068493

se_hat_B <- sqrt(X_hat_B*(1-X_hat_B)/visitors_B) 
print(se_hat_B) #0.008118638

# Compute standard error of test_version_A

X_hat_A <- conversions_A/visitors_A
X_hat_A #0.02773925

se_hat_A <- sqrt(X_hat_A*(1-X_hat_A)/visitors_A) 
print(se_hat_A) #0.006116051


# # Compute the 95% confidence interval for B
# # Save the lower and then the upper confidence interval to a variable called `ci`.
# c(-qnorm(.975), qnorm(.975))    #95% confidence interval

#Compute 95% COnfidence Interval for Version B
ci_B <- c(X_hat_B - qnorm(0.975)*se_hat_B, X_hat_B + qnorm(0.975)*se_hat_B) 
ci_B     # 0.03477269 0.06659717

ci_lower_B <- X_hat_B - qnorm(0.975)*se_hat_B
ci_lower_B  #0.03477269
ci_lower_A <- X_hat_A - qnorm(0.975)*se_hat_A
ci_lower_A  #0.01575201
ci_upper_A <- X_hat_A + qnorm(0.975)*se_hat_A
ci_upper_A

# Compute the 95% confidence interval for A
ci_A <- c(X_hat_A - qnorm(0.975)*se_hat_A, X_hat_A + qnorm(0.975)*se_hat_A) 
ci_A   # 0.01575201 0.03972649


#Method 2
#install.packages("pwr")
library(pwr)
#Run a 1-sample test
prop.test(c(conversions_A + conversions_B),c(visitors_A + visitors_B))
# 1-sample proportions test with continuity correction
# 
# data:  c(conversions_A + conversions_B) out of c(visitors_A + visitors_B), null probability 0.5
# X-squared = 1232.4, df = 1, p-value < 2.2e-16
# alternative hypothesis: true p is not equal to 0.5
# 95 percent confidence interval:
#   0.02690636 0.04694848
# sample estimates:
#   p 
# 0.03563941 

#Run a 2-sampled test
prop.test(c(conversions_A, conversions_B), c(visitors_A,visitors_B))
#Results:
#2-sample test for equality of proportions with continuity correction
# data:  c(conversions_A, conversions_B) out of c(visitors_A, visitors_B)
# X-squared = 3.8048, df = 1, p-value = 0.05111
# alternative hypothesis: two.sided
# 95 percent confidence interval:
#   -4.106263e-02  2.887413e-05
# sample estimates:
#   prop 1     prop 2 
# 0.02531646 0.04583333 
# Results Interpretation --------------------------------------------------

#This is a Significant test result!
#Variation B's observed conversion rate (4.58%) was 81.04% higher than variation A's conversion rate (2.53%). 
#You can be 95% confident that this result is a consequence of the changes you made and not a result of random chance.
#Based on the outcomes,p-value is lower than 0.05. Hence we reject the Null Hypothesis. 

#1. Let's create a dataframe of results per test version
vis_result <- data.frame(variant = c("A","B"), visitors = c(visitors_A, visitors_B),
                         conversions = c(conversions_A,conversions_B),conversion_rate = round(c(conv_rate_A, conv_rate_B),4),
                         Standard_error = round(c(se_hat_A,se_hat_B),5), Conf_Interval_A = c(ci_A[1],ci_A[2]))
vis_result

#2. Let's create a dataframe of results for the pool
vis_result_pool <- data.frame(
  metric=c(
    'Estimated Difference',
    'Relative Uplift(%)',
    'pooled sample proportion',
    'Standard Error of Difference',
    'z_score',
    'p-value',
    'Margin of Error',
    'CI-lower',
    'CI-upper'),
  value=c(
    conv_rate_B - conv_rate_A,
    uplift,
    p_pool,
    SE_pool,
    z_score,
    p_value,
    MOE,
    ci_lower,
    ci_upper
  ))
vis_result_pool




