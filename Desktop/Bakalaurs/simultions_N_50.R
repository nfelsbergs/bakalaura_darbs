library(tidyverse)
library(data.table)
library(tableone)
library(Matching)
library(ggplot2)
library(MatchIt)
library(broom)
library(gtools)
library(survey)
library(sandwich)
library(ipw)
library(caret)
library(leaps)
library(MASS)
library(grid)
library(lattice)
library(chest)
library(scales)
setwd("C:/Users/niklavs.felsbergs/Desktop/Bakalaurs")

causal_effect <- 5
M_val <- 1000
N_val <- 50

set.seed(200)

Z <- function(z){z <- z+2}

# Attels, kur E nav D celonis (1x)pec backdoor path ilustraciajs ===============


dag <- function(N,M,s,ce){
  list <- vector(mode = "list", length = M)
  for (i in 1:M) {
    Z2 <- rnorm(N,0,s) # U
    Z1 <- rnorm(N,0,s) #U
    Z3 <- rnorm(N,0,s) + 5*Z1+(9/13)*Z2
    E <- rnorm(N,0,s) + 5*Z2
    D <- rnorm(N,0,s) + 10*Z1 
    list[[i]] <- data.frame(D,E,Z1,Z2,Z3)
  }
  return(list)
}

D <- dag(N_val,M_val,0.5,causal_effect)

###### 1) UNKNOWNS - Z3, Z4, Z6

# no adjustment
results_df <- data.frame()
for (i in 1:M_val) {
  
  m <- lm(D[[i]][,c(1,2)])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val))
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
included_in_ci <- results_df %>% filter(0 >= lower, 0 <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(0 >= lower, 0 <= upper) %>% nrow())/M_val,acc = 0.1)

results_1_no_adj <- data.frame(dag = 1, method = "no_adj" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

# 1a) backdoor-path sufficient to block backdoor path
# All backdoot paths are blocked by default - which is correct; not to select any confoudners

results_1_backdoor <- results_1_no_adj

# 1b) pre-treatment condition 
# adjusted for {Z1} - Results in hella bias, because collider

results_df <- data.frame()
for (i in 1:M_val) {
  
  m <- lm(D[[i]][,c(1,2,Z(3))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val))
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
included_in_ci <- results_df %>% filter(0 >= lower, 0 <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(0 >= lower, 0 <= upper) %>% nrow())/M_val,acc = 0.1)

results_1_pre_treatment <- data.frame(dag = 1, method = "pre-treatment" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)


# 1c) common cause criterion
# There are no common causes for E and D - which is correct; not to select any confoudners

results_1_common_cause <- results_1_no_adj

# 1d) disjunctive cause criterion
# There are no measured direct causes for E or D - which is correct; not to select any confoudners

results_1_disjunctive <- results_1_no_adj

# 1d) Kopsavilkums metodem, kas balstitas uz info
# nothing selected, which is correct.

results_1_kopsav <- results_1_no_adj


# 1e) backward selection

# train.control <- trainControl(method = "cv", number = 10)
# Train the model
results_df <- data.frame()
for (i in 1:M_val) {
  # D[[i]] <- head(D[[i]],0.3*N_val)
  # D[[i]] <- tail(D[[i]],0.7*N_val)
  step.model <- train(D ~ E+Z3, data = D[[i]],
                      method = "leapBackward", 
                      tuneGrid = data.frame(nvmax = 1:2),
                      # trControl = train.control,
                      
  )
  step.model$results
  step.model$bestTune
  factors <- (t(as.data.table(summary(step.model$finalModel)[1][[1]])[step.model$bestTune %>% as.numeric()]) %>% as.data.frame())
  factors$factor <- row.names(factors)
  factors <- factors %>% filter(V1 == TRUE & factor != 'E' & !factor %like% 'Intercept')
  factors$factor_num <- substr(factors$factor, 2,2)
  selected_factors <- factors %>% pull(factor_num) %>% as.numeric()
  # coef(step.model$finalModel, 4)
  
  m <- lm(D[[i]][,c(1,2,Z(selected_factors))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val, sum_factors = sum(selected_factors),
                                             factors_selected = paste0("Z", paste0(selected_factors, collapse = ", Z"))))
  
  
}


average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
included_in_ci <- results_df %>% filter(0 >= lower, 0 <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(0 >= lower, 0 <= upper) %>% nrow())/M_val,acc = 0.1)


results_1_backward <- data.frame(dag = 1, method = "backward" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

setDT(results_df)
results_1_backward_selected <- results_df[, .(count = .N), by = .(factors_selected)]

# 1f) forward selection

# train.control <- trainControl(method = "cv", number = 10)
# Train the model
results_df <- data.frame()
for (i in 1:M_val) {
  # D[[i]] <- head(D[[i]],0.3*N_val)
  # D[[i]] <- tail(D[[i]],0.7*N_val)
  step.model <- train(D ~ E+Z3, data = D[[i]],
                      method = "leapForward", 
                      tuneGrid = data.frame(nvmax = 1:2),
                      # trControl = train.control,
                      
  )
  step.model$results
  step.model$bestTune
  factors <- (t(as.data.table(summary(step.model$finalModel)[1][[1]])[step.model$bestTune %>% as.numeric()]) %>% as.data.frame())
  factors$factor <- row.names(factors)
  factors <- factors %>% filter(V1 == TRUE & factor != 'E' & !factor %like% 'Intercept')
  factors$factor_num <- substr(factors$factor, 2,2)
  selected_factors <- factors %>% pull(factor_num) %>% as.numeric()
  # coef(step.model$finalModel, 4)
  
  m <- lm(D[[i]][,c(1,2,Z(selected_factors))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val, sum_factors = sum(selected_factors),
                                             factors_selected = paste0("Z", paste0(selected_factors, collapse = ", Z"))))
  
  
}


average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
included_in_ci <- results_df %>% filter(0 >= lower, 0 <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(0 >= lower, 0 <= upper) %>% nrow())/M_val,acc = 0.1)


results_1_forward <- data.frame(dag = 1, method = "forward" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

setDT(results_df)
results_1_forward_selected <- results_df[, .(count = .N), by = .(factors_selected)]




# 1g) change-in-estimate approach

vlist = c("Z3")

results_df <- data.frame()
for (i in 1:M_val) {
  results <- chest_lm(crude = "D ~ E", xlist = vlist, data = D[[i]])
  # chest_plot(results)
  selected_factors <- data.table(results$data)[abs(Change) >= 10] %>% pull(variables) %>% substr(6,6) %>% as.numeric()
  
  m <- lm(D[[i]][,c(1,2,Z(selected_factors))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val, sum_factors = sum(selected_factors),
                                             factors_selected = paste0("Z", paste0(selected_factors, collapse = ", Z"))))
  
}


average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
included_in_ci <- results_df %>% filter(0 >= lower, 0 <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(0 >= lower, 0 <= upper) %>% nrow())/M_val,acc = 0.1)


results_1_change <- data.frame(dag = 1, method = "change" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

setDT(results_df)
results_1_change_selected <- results_df[, .(count = .N), by = .(factors_selected)]


















save.image(file = "whole_image_50.rda")

# Attels (2x)pec backdoor path ilustraciajs ===============

dag <- function(N,M,s,ce){
  list <- vector(mode = "list", length = M)
  for (i in 1:M) {
    Z1 <- rnorm(N,0,s) # U
    Z2 <- rnorm(N,0,s) + 5*Z1
    E <- rnorm(N,0,s) + 5*Z2
    D <- rnorm(N,0,s) + ce*E + 6*Z1 
    list[[i]] <- data.frame(D,E,Z1,Z2)
  }
  return(list)
}

D <- dag(N_val,M_val,0.5,causal_effect)

###### 1) UNKNOWNS - Z3, Z4, Z6

# no adjustment
results_df <- data.frame()
for (i in 1:M_val) {
  
  m <- lm(D[[i]][,c(1,2)])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val))
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate


included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

results_2_no_adj <- data.frame(dag = 2, method = "no_adj" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

# 1a) backdoor-path sufficient to block backdoor path
# need to block Z2 - leads to correct estimate

results_df <- data.frame()
for (i in 1:M_val) {
  
  m <- lm(D[[i]][,c(1,2,Z(2))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val))
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

results_2_backdoor <- data.frame(dag = 2, method = "backdoor" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)


# 1b) pre-treatment condition 
# adjusted for {Z2} - sme as back door


results_2_pre_treatment <- results_2_backdoor


# 1c) common cause criterion
# There are no common  \parcauses for E and D - no adjustment for Z2, when there should be


results_2_common_cause <- results_2_no_adj


# 1d) disjunctive cause criterion
# adjusted for {Z2} - sme as back door


results_2_disjunctive  <- results_2_backdoor

# 1d) Kopsavilkums metodem, kas balstitas uz info
# adjusted for {Z2} - sme as back door

results_2_kopsav <- results_2_backdoor


# 1e) backward selection
# says to adjust for Z2, which is correct

# train.control <- trainControl(method = "cv", number = 10)
# Train the model
results_df <- data.frame()
for (i in 1:M_val) {
  # D[[i]] <- head(D[[i]],0.3*N_val)
  # D[[i]] <- tail(D[[i]],0.7*N_val)
  step.model <- train(D ~ E+Z2, data = D[[i]],
                      method = "leapBackward", 
                      tuneGrid = data.frame(nvmax = 1:2),
                      # trControl = train.control,
                      
  )
  step.model$results
  step.model$bestTune
  factors <- (t(as.data.table(summary(step.model$finalModel)[1][[1]])[step.model$bestTune %>% as.numeric()]) %>% as.data.frame())
  factors$factor <- row.names(factors)
  factors <- factors %>% filter(V1 == TRUE & factor != 'E' & !factor %like% 'Intercept')
  factors$factor_num <- substr(factors$factor, 2,2)
  selected_factors <- factors %>% pull(factor_num) %>% as.numeric()
  # coef(step.model$finalModel, 4)
  
  m <- lm(D[[i]][,c(1,2,Z(selected_factors))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val, sum_factors = sum(selected_factors),
                                             factors_selected = paste0("Z", paste0(selected_factors, collapse = ", Z"))))
  
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower,causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)


results_2_backward <- data.frame(dag = 2, method = "backward" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

setDT(results_df)
results_2_backward_selected <- results_df[, .(count = .N), by = .(factors_selected)]


# 1f) forward selection
# says to adjust for Z2, which is correct

# train.control <- trainControl(method = "cv", number = 10)
# Train the model
results_df <- data.frame()
for (i in 1:M_val) {
  # D[[i]] <- head(D[[i]],0.3*N_val)
  # D[[i]] <- tail(D[[i]],0.7*N_val)
  step.model <- train(D ~ E+Z2, data = D[[i]],
                      method = "leapForward", 
                      tuneGrid = data.frame(nvmax = 1:2),
                      # trControl = train.control,
                      
  )
  step.model$results
  step.model$bestTune
  factors <- (t(as.data.table(summary(step.model$finalModel)[1][[1]])[step.model$bestTune %>% as.numeric()]) %>% as.data.frame())
  factors$factor <- row.names(factors)
  factors <- factors %>% filter(V1 == TRUE & factor != 'E' & !factor %like% 'Intercept')
  factors$factor_num <- substr(factors$factor, 2,2)
  selected_factors <- factors %>% pull(factor_num) %>% as.numeric()
  # coef(step.model$finalModel, 4)
  
  m <- lm(D[[i]][,c(1,2,Z(selected_factors))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val, sum_factors = sum(selected_factors),
                                             factors_selected = paste0("Z", paste0(selected_factors, collapse = ", Z"))))
  
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower,causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)


results_2_forward <- data.frame(dag = 2, method = "forward" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

setDT(results_df)
results_2_forward_selected <- results_df[, .(count = .N), by = .(factors_selected)]





# 1g) change-in-estimate approach
# Says not to adjust for Z2, which is wrong

vlist = c("Z2")

results_df <- data.frame()
for (i in 1:M_val) {
  results <- chest_lm(crude = "D ~ E", xlist = vlist, data = D[[i]])
  # chest_plot(results)
  selected_factors <- data.table(results$data)[abs(Change) >= 10] %>% pull(variables) %>% substr(6,6) %>% as.numeric()
  
  m <- lm(D[[i]][,c(1,2,Z(selected_factors))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val, sum_factors = sum(selected_factors),
                                             factors_selected = paste0("Z", paste0(selected_factors, collapse = ", Z"))))
  
}


average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower,causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)


results_2_change <- data.frame(dag = 2, method = "change" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

setDT(results_df)
results_2_change_selected <- results_df[, .(count = .N), by = .(factors_selected)]












save.image(file = "whole_image_50.rda")

# Attels (3x) pec backdoor ================


dag <- function(N,M,s,ce){
  list <- vector(mode = "list", length = M)
  for (i in 1:M) {
    Z1 <- rnorm(N,0,s) # U
    Z2 <- rnorm(N,0,s) 
    E <- rnorm(N,0,s) + 5*Z2 + 3*Z1
    D <- rnorm(N,0,s) + ce*E + 6*Z1 
    list[[i]] <- data.frame(D,E,Z1,Z2)
  }
  return(list)
}

D <- dag(N_val,M_val,0.5,causal_effect)

###### 1) UNKNOWNS - Z1

# no adjustment
results_df <- data.frame()
for (i in 1:M_val) {
  
  m <- lm(D[[i]][,c(1,2)])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val))
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
results_df %>% filter(0 >= lower, 0 <= upper) %>% nrow()
percent((results_df %>% filter(0 >= lower, 0 <= upper) %>% nrow())/M_val,acc = 0.1)

included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

results_3_no_adj <- data.frame(dag = 2, method = "no_adj" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

# 1a) backdoor-path sufficient to block backdoor path
# no known factors to block backdoor path

results_3_backdoor <- results_3_no_adj


# 1b) pre-treatment condition 
# adjusted for {Z2} - results in increased bias
results_df <- data.frame()
for (i in 1:M_val) {
  
  m <- lm(D[[i]][,c(1,2,Z(2))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val))
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
results_df %>% filter(0 >= lower, 0 <= upper) %>% nrow()
percent((results_df %>% filter(0 >= lower, 0 <= upper) %>% nrow())/M_val,acc = 0.1)

included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

results_3_pre_treatment <- data.frame(dag = 2, method = "pre_treatment" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)


# 1c) common cause criterion
# There are no measured common causes for E and D - no adjustment which is correct
# m <- lm(D[,c(1,2)])
# summary(m)
# confint(m,'E')
# 
# results_3_common_cause <- results_3_no_adj
# 
# 
# # 1d) disjunctive cause criterion
# # adjusted for {Z2} - results in increased bias
# 
# results_df <- data.frame()
# for (i in 1:M_val) {
#   
#   m <- lm(D[[i]][,c(1,2,Z(2))])
#   summary(m)
#   lower_val <- confint(m,'E')[1] #lower
#   upper_val <- confint(m,'E')[2] #upper
#   estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
#   
#   results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val))
#   
# }
# 
# average_lower <- results_df$lower %>% mean()
# average_upper <- results_df$upper %>% mean()
# average_estimate <- results_df$estimate %>% mean()
# average_lower
# average_upper
# average_estimate
# results_df %>% filter(0 >= lower, 0 <= upper) %>% nrow()
# percent((results_df %>% filter(0 >= lower, 0 <= upper) %>% nrow())/M_val,acc = 0.1)

results_3_disjunctive <- results_3_pre_treatment

# 1d) Kopsavilkums metodem, kas balstitas uz info
# No adjustment, which is correct


results_3_kopsav <- results_3_no_adj


# 1e) backward selection
# says to adjust for Z2, which is incorrect

# train.control <- trainControl(method = "cv", number = 10)
# Train the model
results_df <- data.frame()
for (i in 1:M_val) {
  # D[[i]] <- head(D[[i]],0.3*N_val)
  # D[[i]] <- tail(D[[i]],0.7*N_val)
  step.model <- train(D ~ E+Z2, data = D[[i]],
                      method = "leapBackward", 
                      tuneGrid = data.frame(nvmax = 1:2),
                      # trControl = train.control,
                      
  )
  step.model$results
  step.model$bestTune
  factors <- (t(as.data.table(summary(step.model$finalModel)[1][[1]])[step.model$bestTune %>% as.numeric()]) %>% as.data.frame())
  factors$factor <- row.names(factors)
  factors <- factors %>% filter(V1 == TRUE & factor != 'E' & !factor %like% 'Intercept')
  factors$factor_num <- substr(factors$factor, 2,2)
  selected_factors <- factors %>% pull(factor_num) %>% as.numeric()
  # coef(step.model$finalModel, 4)
  
  m <- lm(D[[i]][,c(1,2,Z(selected_factors))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val, sum_factors = sum(selected_factors),
                                             factors_selected = paste0("Z", paste0(selected_factors, collapse = ", Z"))))
  
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower,causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)


results_3_backward <- data.frame(dag = 3, method = "backward" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

setDT(results_df)
results_3_backward_selected <- results_df[, .(count = .N), by = .(factors_selected)]




# 1f) forward selection
# says to adjust for Z2, which is incorrect

# train.control <- trainControl(method = "cv", number = 10)
# Train the model
results_df <- data.frame()
for (i in 1:M_val) {
  # D[[i]] <- head(D[[i]],0.3*N_val)
  # D[[i]] <- tail(D[[i]],0.7*N_val)
  step.model <- train(D ~ E+Z2, data = D[[i]],
                      method = "leapForward", 
                      tuneGrid = data.frame(nvmax = 1:2),
                      # trControl = train.control,
                      
  )
  step.model$results
  step.model$bestTune
  factors <- (t(as.data.table(summary(step.model$finalModel)[1][[1]])[step.model$bestTune %>% as.numeric()]) %>% as.data.frame())
  factors$factor <- row.names(factors)
  factors <- factors %>% filter(V1 == TRUE & factor != 'E' & !factor %like% 'Intercept')
  factors$factor_num <- substr(factors$factor, 2,2)
  selected_factors <- factors %>% pull(factor_num) %>% as.numeric()
  # coef(step.model$finalModel, 4)
  
  m <- lm(D[[i]][,c(1,2,Z(selected_factors))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val, sum_factors = sum(selected_factors),
                                             factors_selected = paste0("Z", paste0(selected_factors, collapse = ", Z"))))
  
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower,causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)


results_3_forward <- data.frame(dag = 3, method = "forward" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

setDT(results_df)
results_3_forward_selected <- results_df[, .(count = .N), by = .(factors_selected)]





# 1g) change-in-estimate approach
# Says to adjust for Z2, which is incorrect

vlist = c("Z2")

results_df <- data.frame()
for (i in 1:M_val) {
  results <- chest_lm(crude = "D ~ E", xlist = vlist, data = D[[i]])
  # chest_plot(results)
  selected_factors <- data.table(results$data)[abs(Change) >= 10] %>% pull(variables) %>% substr(6,6) %>% as.numeric()
  
  m <- lm(D[[i]][,c(1,2,Z(selected_factors))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val, sum_factors = sum(selected_factors),
                                             factors_selected = paste0("Z", paste0(selected_factors, collapse = ", Z"))))
  
}


average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower,causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)


results_3_change <- data.frame(dag = 3, method = "change" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

setDT(results_df)
results_3_change_selected <- results_df[, .(count = .N), by = .(factors_selected)]






save.image(file = "whole_image_50.rda")

# The biggest system, DAG currently not in the paper ===========================
set.seed(200)

dag <- function(N,M,s,ce){
  list <- vector(mode = "list", length = M)
  for (i in 1:M) {
    Z9 <- rnorm(N,0,s) #c6
    Z6 <- rnorm(N,0,s) + 5*Z9 # U3
    Z2 <- rnorm(N,0,s) + (1/5)*Z6 #c2
    Z1 <- rnorm(N,0,s)# c1
    Z3 <- rnorm(N,0,s) # U1
    Z4 <- rnorm(N,0,s) # U2
    Z5 <- rnorm(N,0,s) + 8*Z3 + 13*Z4 #c3
    Z7 <- rnorm(N,0,s) #c4
    Z8 <- rnorm(N,0,s) #c5
    E <- rnorm(N,0,s) + 5*Z1+ 4*Z3 - (1/15)*Z8
    D <- rnorm(N,0,s) + 10 + ce*E - 14*Z1 + 18*Z6 + 50*Z7 - 8*Z4
    list[[i]] <- data.frame(D,E,Z1,Z2,Z3,Z4,Z5,Z6,Z7,Z8,Z9)
  }
  return(list)
}

D <- dag(N_val,M_val,0.5,causal_effect)

###### 1) UNKNOWNS - Z3, Z4, Z6

# no adjustment

results_df <- data.frame()
for (i in 1:M_val) {
  
  m <- lm(D[[i]][,c(1,2)])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val))
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

results_4_no_adj <- data.frame(dag = 4, method = "no_adj" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

# 1a) backdoor-path sufficient to block backdoor path
# adjusted for {Z1,Z2} - causal effect estimated around 1 - correct, Z5 not controlled, because collider

results_df <- data.frame()
for (i in 1:M_val) {
  
  m <- lm(D[[i]][,c(1,2,Z(1), Z(2))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val))
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

results_4_backdoor <- data.frame(dag = 4, method = "backdoor" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

# 1b) pre-treatment condition 
# adjusted for {Z1,2,5,8,9} - Causal effect completely wrong - adjusted for collider


results_df <- data.frame()
for (i in 1:M_val) {
  
  m <- lm(D[[i]][,c(1,2,Z(1), Z(2), Z(5), Z(8), Z(9))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val))
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

results_4_pre_treatment <- data.frame(dag = 4, method = "pre_treatment" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

# 1c) common cause criterion
# adjusted for {Z1} - causal effect estimated around 1 - correct, but larger conf intervals than backdoor


results_df <- data.frame()
for (i in 1:M_val) {
  
  m <- lm(D[[i]][,c(1,2,Z(1))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val))
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

results_4_common_cause <- data.frame(dag = 4, method = "common_cause" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

# # 1b) pre-treatment condition 
# # adjusted for {Z1,2,5,8,9} - Causal effect completely wrong - adjusted for collider
# 
# 
# results_df <- data.frame()
# for (i in 1:M_val) {
#   
#   m <- lm(D[[i]][,c(1,2,Z(1), Z(2), Z(5), Z(8), Z(9))])
#   summary(m)
#   lower_val <- confint(m,'E')[1] #lower
#   upper_val <- confint(m,'E')[2] #upper
#   estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
#   
#   results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val))
#   
# }
# 
# average_lower <- results_df$lower %>% mean()
# average_upper <- results_df$upper %>% mean()
# average_estimate <- results_df$estimate %>% mean()
# average_lower
# average_upper
# average_estimate
# results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
# percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

# 1d) disjunctive cause criterion
# adjusted for {Z1,2,8,7}


results_df <- data.frame()
for (i in 1:M_val) {
  
  m <- lm(D[[i]][,c(1,2,Z(1),Z(2),Z(7),Z(8))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val))
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

results_4_disjunctive <- data.frame(dag = 4, method = "disjunctive" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

# 1d) Kopsavilkums metodem, kas balstitas uz info
# adjusted for {Z1,2,7} - 

results_df <- data.frame()
for (i in 1:M_val) {
  
  m <- lm(D[[i]][,c(1,2,Z(1),Z(2),Z(7))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val))
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

results_4_kopsav <- data.frame(dag = 4, method = "kopsav" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

# 1e) backward selection

# train.control <- trainControl(method = "cv", number = 10)
# Train the model
results_df <- data.frame()
for (i in 1:M_val) {
  # D[[i]] <- head(D[[i]],0.3*N_val)
  # D[[i]] <- tail(D[[i]],0.7*N_val)
  step.model <- train(D ~ E+Z1+Z2+Z5+Z7+Z8+Z9, data = D[[i]],
                      method = "leapBackward", 
                      tuneGrid = data.frame(nvmax = 1:7),
                      # trControl = train.control,
                      
  )
  step.model$results
  step.model$bestTune
  factors <- (t(as.data.table(summary(step.model$finalModel)[1][[1]])[step.model$bestTune %>% as.numeric()]) %>% as.data.frame())
  factors$factor <- row.names(factors)
  factors <- factors %>% filter(V1 == TRUE & factor != 'E' & !factor %like% 'Intercept')
  factors$factor_num <- substr(factors$factor, 2,2)
  selected_factors <- factors %>% pull(factor_num) %>% as.numeric()
  # coef(step.model$finalModel, 4)
  
  m <- lm(D[[i]][,c(1,2,Z(selected_factors))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val, sum_factors = sum(selected_factors),
                                             factors_selected = paste0("Z", paste0(selected_factors, collapse = ", Z"))))
  
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

setDT(results_df)
results_df[, .(count = .N), by = .(factors_selected)]

included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower,causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)


results_4_backward <- data.frame(dag = 4, method = "backward" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

setDT(results_df)
results_4_backward_selected <- results_df[, .(count = .N), by = .(factors_selected)]


# 1f) forward selection

# train.control <- trainControl(method = "cv", number = 10)
# Train the model
results_df <- data.frame()
for (i in 1:M_val) {
  # D[[i]] <- head(D[[i]],0.3*N_val)
  # D[[i]] <- tail(D[[i]],0.7*N_val)
  step.model <- train(D ~ E+Z1+Z2+Z5+Z7+Z8+Z9, data = D[[i]],
                      method = "leapForward", 
                      tuneGrid = data.frame(nvmax = 1:7),
                      # trControl = train.control,
                      
  )
  step.model$results
  step.model$bestTune
  factors <- (t(as.data.table(summary(step.model$finalModel)[1][[1]])[step.model$bestTune %>% as.numeric()]) %>% as.data.frame())
  factors$factor <- row.names(factors)
  factors <- factors %>% filter(V1 == TRUE & factor != 'E' & !factor %like% 'Intercept')
  factors$factor_num <- substr(factors$factor, 2,2)
  selected_factors <- factors %>% pull(factor_num) %>% as.numeric()
  # coef(step.model$finalModel, 4)
  
  m <- lm(D[[i]][,c(1,2,Z(selected_factors))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val, sum_factors = sum(selected_factors),
                                             factors_selected = paste0("Z", paste0(selected_factors, collapse = ", Z"))))
  
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

setDT(results_df)
results_df[, .(count = .N), by = .(factors_selected)]


included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower,causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)


results_4_forward <- data.frame(dag = 4, method = "forward" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

setDT(results_df)
results_4_forward_selected <- results_df[, .(count = .N), by = .(factors_selected)]



# 1g) change-in-estimate approach

vlist = c("Z1", "Z2", "Z5", "Z7", "Z8", "Z9")

results_df <- data.frame()
for (i in 1:M_val) {
  results <- chest_lm(crude = "D ~ E", xlist = vlist, data = D[[i]])
  # chest_plot(results)
  selected_factors <- data.table(results$data)[abs(Change) >= 10] %>% pull(variables) %>% substr(6,6) %>% as.numeric()
  
  m <- lm(D[[i]][,c(1,2,Z(selected_factors))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val, sum_factors = sum(selected_factors),
                                             factors_selected = paste0("Z", paste0(selected_factors, collapse = ", Z"))))
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

setDT(results_df)
results_df[, .(count = .N), by = .(factors_selected)]

included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower,causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)


results_4_change <- data.frame(dag = 4, method = "change" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

setDT(results_df)
results_4_change_selected <- results_df[, .(count = .N), by = .(factors_selected)]



save.image(file = "whole_image_50.rda")

# Another newly generated system, DAG currently not in the paper ===========================

causal_effect <- 10
M_val <- 1000

set.seed(200)

dag <- function(N,M,s,ce){
  list <- vector(mode = "list", length = M)
  for (i in 1:M) {
    Z6 <- rnorm(N,0,s) #C4
    Z1 <- rnorm(N,0,s) - 5*Z6 #C1
    Z2 <- rnorm(N,0,s) + (1/5)*Z1 # U1
    Z3 <- rnorm(N,0,s) + (4/5)*Z2 # C2
    Z5 <- rnorm(N,0,s) # U2
    Z4 <- rnorm(N,0,s) +(8/9)*Z3 + 2*Z5 #C3
    Z7 <- rnorm(N,0,s) #C5
    Z8 <- rnorm(N,0,s) #C6
    E <- rnorm(N,0,s) + 5*Z8 - 3*Z3 - 9*Z1
    D <- rnorm(N,0,s) + 10 + ce*E +7*Z4+6*Z5 - 3*Z1 + Z6 -2 * Z7
    list[[i]] <- data.frame(D,E,Z1,Z2,Z3,Z4,Z5,Z6,Z7,Z8)
  }
  return(list)
}


D <- dag(N_val,M_val,0.5,causal_effect)

###### 1) UNKNOWNS - Z2, Z5
# no adjustment


results_df <- data.frame()
for (i in 1:M_val) {
  
  m <- lm(D[[i]][,c(1,2)])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val))
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)


included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

results_5_no_adj <- data.frame(dag = 5, method = "no_adj" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

# 1a) backdoor-path sufficient to block backdoor path
# adjusted for {Z1,Z3}


results_df <- data.frame()
for (i in 1:M_val) {
  
  m <- lm(D[[i]][,c(1,2,Z(1),Z(3))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val))
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

results_5_backdoor <- data.frame(dag = 5, method = "backdoor" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

# 1b) pre-treatment condition 
# adjusted for {Z1,3,4,6,8} - Causal effect completely wrong - adjusted for collider

results_df <- data.frame()
for (i in 1:M_val) {
  
  m <- lm(D[[i]][,c(1,2,Z(1), Z(3), Z(4), Z(6), Z(8))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val))
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)


included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

results_5_pre_treatment <- data.frame(dag = 5, method = "pre_treatment" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

# 1c) common cause criterion
# adjusted for {Z1}


results_df <- data.frame()
for (i in 1:M_val) {
  
  m <- lm(D[[i]][,c(1,2,Z(1))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val))
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)


included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

results_5_common_cause <- data.frame(dag = 5, method = "common_cause" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

# 1d) disjunctive cause criterion
# adjusted for {Z1,,3,4,6,7,8}


results_df <- data.frame()
for (i in 1:M_val) {
  
  m <- lm(D[[i]][,c(1,2,Z(1),Z(3),Z(4),Z(6),Z(7),Z(8))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val))
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)


included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

results_5_disjunctive <- data.frame(dag = 5, method = "disjunctive" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

# 1d) Kopsavilkums metodem, kas balstitas uz info
# adjusted for {Z1,,3,4,6,7} 


results_df <- data.frame()
for (i in 1:M_val) {
  
  m <- lm(D[[i]][,c(1,2,Z(1),Z(3),Z(4),Z(6),Z(7))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val))
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)


included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

results_5_kopsav <- data.frame(dag = 5, method = "kopsav" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

# 1e) backward selection

# train.control <- trainControl(method = "cv", number = 10)
# Train the model
results_df <- data.frame()
for (i in 1:M_val) {
  # D[[i]] <- head(D[[i]],0.3*N_val)
  # D[[i]] <- tail(D[[i]],0.7*N_val)
  step.model <- train(D ~ E+Z1+Z3+Z4+Z6+Z7+Z8, data = D[[i]],
                      method = "leapBackward", 
                      tuneGrid = data.frame(nvmax = 1:7),
                      # trControl = train.control,
                      
  )
  step.model$results
  step.model$bestTune
  factors <- (t(as.data.table(summary(step.model$finalModel)[1][[1]])[step.model$bestTune %>% as.numeric()]) %>% as.data.frame())
  factors$factor <- row.names(factors)
  factors <- factors %>% filter(V1 == TRUE & factor != 'E' & !factor %like% 'Intercept')
  factors$factor_num <- substr(factors$factor, 2,2)
  selected_factors <- factors %>% pull(factor_num) %>% as.numeric()
  # coef(step.model$finalModel, 4)
  
  m <- lm(D[[i]][,c(1,2,Z(selected_factors))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val, sum_factors = sum(selected_factors),
                                             factors_selected = paste0("Z", paste0(selected_factors, collapse = ", Z"))))
  
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

setDT(results_df)
results_df[, .(count = .N), by = .(factors_selected)]


included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower,causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)


results_5_backward <- data.frame(dag = 5, method = "backward" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

setDT(results_df)
results_5_backward_selected <- results_df[, .(count = .N), by = .(factors_selected)]



# 1f) forward selection

# train.control <- trainControl(method = "cv", number = 10)
# Train the model
results_df <- data.frame()
for (i in 1:M_val) {
  # D[[i]] <- head(D[[i]],0.3*N_val)
  # D[[i]] <- tail(D[[i]],0.7*N_val)
  step.model <- train(D ~ E+Z1+Z3+Z4+Z6+Z7+Z8, data = D[[i]],
                      method = "leapForward", 
                      tuneGrid = data.frame(nvmax = 1:7),
                      # trControl = train.control,
                      
  )
  step.model$results
  step.model$bestTune
  factors <- (t(as.data.table(summary(step.model$finalModel)[1][[1]])[step.model$bestTune %>% as.numeric()]) %>% as.data.frame())
  factors$factor <- row.names(factors)
  factors <- factors %>% filter(V1 == TRUE & factor != 'E' & !factor %like% 'Intercept')
  factors$factor_num <- substr(factors$factor, 2,2)
  selected_factors <- factors %>% pull(factor_num) %>% as.numeric()
  # coef(step.model$finalModel, 4)
  
  m <- lm(D[[i]][,c(1,2,Z(selected_factors))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val, sum_factors = sum(selected_factors),
                                             factors_selected = paste0("Z", paste0(selected_factors, collapse = ", Z"))))
  
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

setDT(results_df)
results_df[, .(count = .N), by = .(factors_selected)]



included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower,causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)


results_5_forward <- data.frame(dag = 5, method = "forward" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

setDT(results_df)
results_5_forward_selected <- results_df[, .(count = .N), by = .(factors_selected)]



# 1g) change-in-estimate approach

vlist = c("Z1", "Z3", "Z4", "Z6", "Z7", "Z8")

results_df <- data.frame()
for (i in 1:M_val) {
  results <- chest_lm(crude = "D ~ E", xlist = vlist, data = D[[i]])
  # chest_plot(results)
  selected_factors <- data.table(results$data)[abs(Change) >= 10] %>% pull(variables) %>% substr(6,6) %>% as.numeric()
  
  m <- lm(D[[i]][,c(1,2,Z(selected_factors))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val, sum_factors = sum(selected_factors),
                                             factors_selected = paste0("Z", paste0(selected_factors, collapse = ", Z"))))
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

setDT(results_df)
results_df[, .(count = .N), by = .(factors_selected)] %>% arrange(desc(count))


included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower,causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)


results_5_change <- data.frame(dag = 5, method = "change" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

setDT(results_df)
results_5_change_selected <- results_df[, .(count = .N), by = .(factors_selected)]



save.image(file = "whole_image_50.rda")

# Huge system for data based methods ================

set.seed(200)

dag <- function(N,M,s,ce){
  list <- vector(mode = "list", length = M)
  for (i in 1:M) {
    Z1 <- rnorm(N,0,s)
    Z2 <- rnorm(N,0,s) + 2*Z1 
    Z4 <- rnorm(N,0,s) + 4*Z1 
    Z3 <- rnorm(N,0,s) + Z2 - 3*Z4 
    Z6 <- rnorm(N,0,s) - (3/5)*Z4 
    Z5 <- rnorm(N,0,s) - Z6 + Z2
    Z8 <- rnorm(N,0,s) - 2*Z6 
    Z7 <- rnorm(N,0,s) - Z8 + 6*Z2 
    Z10 <- rnorm(N,0,s) - 5*Z8
    Z12 <- rnorm(N,0,s) + 5*Z8
    Z9 <- rnorm(N,0,s) + 5*Z7 - (7/9)*Z10
    Z11 <- rnorm(N,0,s) + 3*Z10 
    Z13 <- rnorm(N,0,s) + 10*Z9 
    Z14 <- rnorm(N,0,s) + 2*Z10 - 3* Z13 
    Z15 <- rnorm(N,0,s) + 6*Z11
    Z16 <- rnorm(N,0,s) + (6/7)*Z12
    Z17 <- rnorm(N,0,s) +  7*Z14
    Z18 <- rnorm(N,0,s) +  7*Z14 - 5*Z15
    Z19 <- rnorm(N,0,s) +  Z16
    E <- rnorm(N,0,s) + 4*Z9 - 3*Z17 
    D <- rnorm(N,0,s) + 3 + ce*E +7*Z17+6*Z19 - 3*Z6
    list[[i]] <- data.frame(D,E,Z1,Z2,Z3,Z4,Z5,Z6,Z7,Z8,Z9,Z10,Z11,Z12,Z13,Z14,Z15,Z16,Z17,Z18,Z19)
  }
  return(list)
}

D <- dag(N_val,M_val,0.5,causal_effect)

###### 1) UNKNOWNS - Z2, Z5
# no adjustment

results_df <- data.frame()
for (i in 1:M_val) {
  
  m <- lm(D[[i]][,c(1,2)])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val))
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

results_6_no_adj <- data.frame(dag = 6, method = "no_adj" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

# 1a) backdoor-path sufficient to block backdoor path
# adjusted for {Z17,Z19} - causal effect estimated around 1 - correct, Z5 not controlled, because collider

results_df <- data.frame()
for (i in 1:M_val) {
  
  m <- lm(D[[i]][,c(1,2,Z(17), Z(19), Z(6))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val))
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

results_6_backdoor <- data.frame(dag = 6, method = "backdoor" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)


# 1b) pre-treatment condition 


results_df <- data.frame()
for (i in 1:M_val) {
  
  m <- lm(D[[i]][,c(1,2,Z(1), Z(2), Z(3), Z(4), Z(5), Z(6), Z(7), Z(8), Z(9), Z(10), Z(11), Z(12), Z(13), Z(14), Z(15), Z(16), Z(17), Z(18), Z(19) )])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val))
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

results_6_pre_treatment <- data.frame(dag = 6, method = "pre_treatment" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)


# 1a) common cause

results_df <- data.frame()
for (i in 1:M_val) {
  
  m <- lm(D[[i]][,c(1,2,Z(17))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val))
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

results_6_common_cause <- data.frame(dag = 6, method = "common_cause" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

# 1a) disjunctive cause

results_df <- data.frame()
for (i in 1:M_val) {
  
  m <- lm(D[[i]][,c(1,2,Z(17),Z(19),Z(9),Z(6))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val))
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

results_6_disjunctive <- data.frame(dag = 6, method = "disjunctive" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)


# 1d) Using all rules from factor based methods yields a good estimate.
# adjusted for 9,17,19,6

results_df <- data.frame()
for (i in 1:M_val) {
  
  m <- lm(D[[i]][,c(1,2,Z(9),Z(17),Z(19),Z(6))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val))
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

results_6_kopsav <- data.frame(dag = 6, method = "kopsav" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)



# 1e) backward selection




results_df <- data.frame()
for (i in 1:M_val) {
  # D[[i]] <- head(D[[i]],0.3*N_val)
  # D[[i]] <- tail(D[[i]],0.7*N_val)
  step.model <- train(D ~ E+Z1+Z2+Z3+Z4+Z5+Z6+Z7+Z8+Z9+Z10+Z11+Z12+Z13+Z14+Z15+Z16+Z17+Z18+Z19, data = D[[i]],
                      method = "leapBackward", 
                      tuneGrid = data.frame(nvmax = 1:20),
                      # trControl = train.control,
                      
  )
  step.model$results
  step.model$bestTune
  factors <- (t(as.data.table(summary(step.model$finalModel)[1][[1]])[step.model$bestTune %>% as.numeric()]) %>% as.data.frame())
  factors$factor <- row.names(factors)
  factors <- factors %>% filter(V1 == TRUE & factor != 'E' & !factor %like% 'Intercept')
  factors$factor_num <- substr(factors$factor, 2,3)
  selected_factors <- factors %>% pull(factor_num) %>% as.numeric()
  # coef(step.model$finalModel, 4)
  
  m <- lm(D[[i]][,c(1,2,Z(selected_factors))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val, sum_factors = sum(selected_factors),
                                             factors_selected = paste0("Z", paste0(selected_factors, collapse = ", Z"))))
  
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

setDT(results_df)
results_df[, .(count = .N), by = .(factors_selected)]


included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower,causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)


results_6_backward <- data.frame(dag = 6, method = "backward" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

setDT(results_df)
results_6_backward_selected <- results_df[, .(count = .N), by = .(factors_selected)]


# 1f) forward selection

# train.control <- trainControl(method = "cv", number = 10)
results_df <- data.frame()
for (i in 1:M_val) {
  # D[[i]] <- head(D[[i]],0.3*N_val)
  # D[[i]] <- tail(D[[i]],0.7*N_val)
  step.model <- train(D ~ E+Z1+Z2+Z3+Z4+Z5+Z6+Z7+Z8+Z9+Z10+Z11+Z12+Z13+Z14+Z15+Z16+Z17+Z18+Z19, data = D[[i]],
                      method = "leapForward", 
                      tuneGrid = data.frame(nvmax = 1:20),
                      # trControl = train.control,
                      
  )
  step.model$results
  step.model$bestTune
  factors <- (t(as.data.table(summary(step.model$finalModel)[1][[1]])[step.model$bestTune %>% as.numeric()]) %>% as.data.frame())
  factors$factor <- row.names(factors)
  factors <- factors %>% filter(V1 == TRUE & factor != 'E' & !factor %like% 'Intercept')
  factors$factor_num <- substr(factors$factor, 2,3)
  selected_factors <- factors %>% pull(factor_num) %>% as.numeric()
  # coef(step.model$finalModel, 4)
  
  m <- lm(D[[i]][,c(1,2,Z(selected_factors))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val, sum_factors = sum(selected_factors),
                                             factors_selected = paste0("Z", paste0(selected_factors, collapse = ", Z"))))
  
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

setDT(results_df)
results_df[, .(count = .N), by = .(factors_selected)]


included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower,causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)


results_6_forward <- data.frame(dag = 6, method = "forward" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

setDT(results_df)
results_6_forward_selected <- results_df[, .(count = .N), by = .(factors_selected)]






# 1g) change-in-estimate approach

vlist = c("Z1", "Z3", "Z4", "Z5", "Z6", "Z8", "Z9", "Z10", "Z12", "Z13", "Z14", "Z15", "Z16", "Z17", "Z18", "Z19")

results_df <- data.frame()
for (i in 1:M_val) {
  results <- chest_lm(crude = "D ~ E", xlist = vlist, data = D[[i]])
  # chest_plot(results)
  selected_factors <- data.table(results$data)[abs(Change) >= 10] %>% pull(variables) %>% substr(6,6) %>% as.numeric()
  
  m <- lm(D[[i]][,c(1,2,Z(selected_factors))])
  summary(m)
  lower_val <- confint(m,'E')[1] #lower
  upper_val <- confint(m,'E')[2] #upper
  estimate_val <- m$coefficients[which(names(m$coefficients) == 'E')]
  
  results_df <- rbind(results_df, data.frame(estimate = estimate_val, lower = lower_val, upper = upper_val, sum_factors = sum(selected_factors),
                                             factors_selected = paste0("Z", paste0(selected_factors, collapse = ", Z"))))
  
}

average_lower <- results_df$lower %>% mean()
average_upper <- results_df$upper %>% mean()
average_estimate <- results_df$estimate %>% mean()
average_lower
average_upper
average_estimate
results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
percent((results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)

setDT(results_df)
results_df[, .(count = .N), by = .(factors_selected)]

included_in_ci <- results_df %>% filter(causal_effect >= lower, causal_effect <= upper) %>% nrow()
included_in_ci_prop <- percent((results_df %>% filter(causal_effect >= lower,causal_effect <= upper) %>% nrow())/M_val,acc = 0.1)


results_6_change <- data.frame(dag = 6, method = "change" ,lower = average_lower, estimate = average_estimate, upper = average_upper, in_ci = included_in_ci, in_ci_prop = included_in_ci_prop)

setDT(results_df)
results_6_change_selected <- results_df[, .(count = .N), by = .(factors_selected)]







save.image(file = "whole_image_50.rda")









save.image(file = "whole_image_50.rda")

# plotting averages and confidence intervals ==================

with_col <- data.frame(graph = "with_collider"
                       , method = factor(c("Tiesais novertejums", "Backdoor path", "Common cause", "Pre-treatment", "Disjunctive cause", "Kopsavilkums", "Backward selection", "Forward selection", "Change-in-estimate")
                                         , levels = c("Tiesais novertejums", "Backdoor path", "Common cause", "Pre-treatment", "Disjunctive cause", "Kopsavilkums", "Backward selection", "Forward selection", "Change-in-estimate"))
                       , estimate = c(3.31, 4.98, 4.98, 6.12, 4.99, 4.99, 6.13, 6.13, 6.14)
                       , lower = c(2.3, 3.75, 3.4, 5.19, 4.00, 4, 5.82, 5.82, 4.85)
                       , upper = c(4.32, 6.21, 6.56, 7.05, 5.96, 5.96, 6.44, 6.44, 7.44)
)

without_col <- data.frame(graph = "without_collider"
                          , method = factor(c("Tiesais novertejums", "Backdoor path", "Common cause", "Pre-treatment", "Disjunctive cause", "Kopsavilkums", "Backward selection", "Forward selection", "Change-in-estimate")
                                            , levels = c("Tiesais novertejums", "Backdoor path", "Common cause", "Pre-treatment", "Disjunctive cause", "Kopsavilkums", "Backward selection", "Forward selection", "Change-in-estimate"))
                          , estimate = c(10.21, 10, 9.25, 10, 10, 10, 10.01, 10, 10.41)
                          , lower = c(10.19, 9.74, 9.03, 9.78, 9.82, 9.96, 9.93, 9.83, 10.37)
                          , upper = c(10.24, 10.26, 9.46, 10.22, 10.18, 10.03, 10.1, 10.18, 10.45)
)


plot1 <- with_col %>% ggplot(aes(x=method,y = estimate)) + geom_point(lwd = 2)+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.6, lwd = 1) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab("Ricibas efekta novertejums") +
  xlab("Metode") +
  ggtitle("Novertejums celonsakaribu tiklam ar sadursmes mezglu") +
  geom_hline(yintercept = 5, linetype = "dashed")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
plot1
plot2 <- without_col %>% ggplot(aes(x=method,y = estimate)) + geom_point(lwd = 2)+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.6, lwd = 1) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab("Ricibas efekta novertejums") +
  xlab("Metode") +
  ggtitle("Novertejumi celonsakaribu tiklam bez sadursmes mezgla") +
  geom_hline(yintercept = 10, linetype = "dashed")

plot2

grid.newpage()
grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))












# stepwise theoretical===============