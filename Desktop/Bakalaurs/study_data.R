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
library(gridExtra)
library(chest)
library(scales)
setwd("C:/Users/niklavs.felsbergs/Desktop/Bakalaurs")

data_egl <- read.csv("aptaujs/EGL-dati_2020-09-04.csv")
data_csp <- read.csv("aptaujs/CSP-dati_2020-09-04.csv")

data_egl$id <- data_egl$ï..id
data_csp$id <- data_csp$ï..id

data_egl$id %>% unique() %>% length()
data_csp$id %>% unique() %>% length()

data_egl$test_grupa %>% unique()  

setDT(data_csp)  
setDT(data_egl)  

data_csp[, .(id, M1)]  



# egl_sel <- data_egl[, .(id, dzim, test_grupa, test_val = as.numeric(test_val))]
egl_sel <- data_egl[, .(id, dzim, test_grupa, test_val)]
csp_sel <- data_csp[, .(id, M1, Vecums = as.numeric(Vecums)
                        , sirdstrieka = ifelse(P2.2_9a_3 == 1, 1, 0)
                        , hipertensija = ifelse(P2.2_9a_1 == 1, 1, 0)
                        , stenokardija = ifelse(P2.2_9a_2 == 1, 1, 0)
                        , insults = ifelse(P2.2_9a_4 == 1, 1, 0)
                        , holesterins = ifelse(P2.2_9a_5 == 1, 1, 0)
                        , sirds_mazspeja = ifelse(P2.2_9a_6 == 1, 1, 0)
                        , aritmija = ifelse(P2.2_9a_7 == 1, 1, 0)
                        , vezis = ifelse(P2.2_9a_8 == 1, 1, 0)
                        , vairogdziedzeris = ifelse(P2.4_9a_10 == 1, 1, 0)
                        , astma = ifelse(P2.4_9a_11 == 1, 1, 0)
                        , bronhits = ifelse(P2.4_9a_12 == 1, 1, 0)
                        , artrits = ifelse(P2.4_9a_13 == 1, 1, 0)
                        , osteoartrits = ifelse(P2.4_9a_14 == 1, 1, 0)
                        , mugura1 = ifelse(P2.4_9a_15 == 1, 1, 0)
                        , mugura2 = ifelse(P2.4_9a_16 == 1, 1, 0)
                        , diabets = ifelse(P2.4_9a_17 == 1, 1, 0)
                        , cula = ifelse(P2.4_9a_18 == 1, 1, 0)
                        , ciroze = ifelse(P2.4_9a_19 == 1, 1, 0)
                        , galvassapes = ifelse(P2.4_9a_20 == 1, 1, 0)
                        , urins = ifelse(P2.4_9a_21 == 1, 1, 0)
                        , nieres = ifelse(P2.4_9a_22 == 1, 1, 0)
                        , depresija = ifelse(P2.4_9a_23 == 1, 1, 0)
                        , garigas = ifelse(P2.4_9a_24 == 1, 1, 0)
                        , trauma = ifelse(P2.4_9a_25 == 1, 1, 0)
                        , invaliditate = ifelse(P2.5 == 1, 1, 0)
                        , aptaukosanas = ifelse(P2.4_9a_10== 1, 1, 0))]

egl_sel_cast <- dcast(egl_sel, id + dzim ~ test_grupa, value.var = "test_val", fun.aggregate = function(x){paste(x)}, fill = NA)  


merged_data <- merge(csp_sel, egl_sel_cast, by = "id")

merged_data[, bmi := as.numeric(`Pacienta svars, kg`)/((M1*0.01)^2)]  


merged_data$asinssp_1 <- str_split(merged_data$Asinsspiediens, pattern = "/", simplify = TRUE)[,1]
merged_data$asinssp_2 <- str_split(merged_data$Asinsspiediens, pattern = "/", simplify = TRUE)[,2]

merged_data$bmi_groups <- cut(merged_data$bmi, c(0,15,25,30,35,40,70))


calc_data <- merged_data[, .(id, dzim, bmi = as.numeric(bmi), bmi_groups, asinssp_1 = as.numeric(asinssp_1), asinssp_2 = as.numeric(asinssp_2), triglic = as.numeric(`TriglicerÄ«di`), Vecums, Svars = `Pacienta svars, kg`, Augums = M1
                             , sirdstrieka, hipertensija, aptaukosanas, stenokardija, insults, holesterins, sirds_mazspeja, aritmija, vezis, vairogdziedzeris, astma, bronhits, artrits, osteoartrits, mugura1, mugura2,
                             diabets, cula, ciroze, galvassapes, urins, nieres, depresija, garigas,trauma, invaliditate)]

calc_data <- na.omit(calc_data)
calc_data[bmi >= 30, Obese := 1]
calc_data[bmi < 30, Obese := 0]
calc_data[asinssp_1 >= 140 & asinssp_2 >= 90, hyper := 1]
calc_data[(hyper != 1 | is.na(hyper)), hyper := 0]






# CHECK COLLIDER 

potential <- c(
  # "asinssp_1", "asinssp_2", "triglic", "hipertensija", 
  'aptaukosanas',
  "stenokardija", "insults", "holesterins", "sirds_mazspeja", "aritmija", "vezis", 
  "vairogdziedzeris", "astma", "bronhits", "artrits", "osteoartrits", "mugura1", "mugura2", "diabets", "cula", 
  "ciroze", "galvassapes", "urins", "nieres", "depresija", "garigas", "trauma", "invaliditate", "sirdstrieka")

# combos found:
# cause: aptaukosanas, outcome: cula

results_end <- data.frame()


for ( z in 1:length(potential)) {
  
  cause <- potential[z]
  
  results_found <- data.frame()
  
  for (j in 1:length(setdiff(potential, cause))) {
    
    
    outcome <- setdiff(potential, cause)[j]
    
    results_df <- data.frame()
    
    for (i in 1:length(setdiff(setdiff(potential, outcome), cause))) {
      
      copy_calc_data <- copy(calc_data)
      
      collider_selected <- setdiff(setdiff(potential, outcome), cause)[i]
      
      print(paste0(cause, "-", outcome, "-", collider_selected))
      
      copy_calc_data <- copy_calc_data[,.(outcome_sel = get(outcome), cause_sel = get(cause), collider_sel = get(collider_selected))]
      
      tryCatch({
      
      eff1 <- coefficients(glm(outcome_sel ~ cause_sel, data = copy_calc_data,
                               family = binomial(link = "logit")))[2]
      eff2 <- coefficients(glm(outcome_sel ~ cause_sel + collider_sel, data = copy_calc_data,
                               family = binomial(link = "logit")))[2]
      
      # if (eff1 < 10) {
        
        is_collider <- eff1 > 0 & eff2 < 0
        
        results_df <- rbind(results_df, data.frame(cause = cause, outcome = outcome, colliding_faktors = potential[i], collider = is_collider, without = eff1, with = eff2
                                                   , without_or = exp(eff1), with_or = exp(eff2)
                                                   , conf_int_lower_without_or = exp(confint(glm(outcome_sel ~ cause_sel, data = copy_calc_data,
                                                                                              family = binomial(link = "logit")))[2,1])
                                                   , conf_int_upper_without_or = exp(confint(glm(outcome_sel ~ cause_sel, data = copy_calc_data,
                                                                                              family = binomial(link = "logit")))[2,2])
                                                   , conf_int_lower_with_or = exp(confint(glm(outcome_sel ~ cause_sel + collider_sel, data = copy_calc_data,
                                                                                           family = binomial(link = "logit")))[2,1])
                                                   , conf_int_upper_with_or = exp(confint(glm(outcome_sel ~ cause_sel + collider_sel, data = copy_calc_data,
                                                                                           family = binomial(link = "logit")))[2,2])
                                                   , conf_int_lower_without = (confint(glm(outcome_sel ~ cause_sel, data = copy_calc_data,
                                                                                              family = binomial(link = "logit")))[2,1])
                                                   , conf_int_upper_without = (confint(glm(outcome_sel ~ cause_sel, data = copy_calc_data,
                                                                                              family = binomial(link = "logit")))[2,2])
                                                   , conf_int_lower_with = (confint(glm(outcome_sel ~ cause_sel + collider_sel, data = copy_calc_data,
                                                                                           family = binomial(link = "logit")))[2,1])
                                                   , conf_int_upper_with = (confint(glm(outcome_sel ~ cause_sel + collider_sel, data = copy_calc_data,
                                                                                           family = binomial(link = "logit")))[2,2])
        ))
        
        }, error=function(e){})
      # }
    }
    
    
    if (nrow(results_df) > 0) {
      
      results_found <- rbind(results_found, results_df %>% filter(sign(without) != sign(with)))
      
    }
    
  }
  
  results_end <- rbind(results_end, results_found)
  
}

save(results_end, file = "results_end.rda")

setDT(results_end)

results_end[,diff := abs(with-without)]

### 6) Regression for causal effects ------------------

model_regression <- lm(sirdstrieka ~ aptaukosanas + hipertensija,
                       data = calc_data[dzim == 'vir'])


model_tidy <- tidy(model_regression)

causal_effect <- model_tidy %>% filter(term == 'aptaukosanas') %>% pull(estimate)
causal_effect_se <- model_tidy %>% filter(term == 'aptaukosanas') %>% pull(std.error)
causal_effect

paste(causal_effect - 1.96*causal_effect_se, causal_effect, causal_effect + 1.96*causal_effect_se)

regerssion_df <- data.frame(method = "Regresijas metode", 
                            lower = causal_effect - 1.96*causal_effect_se, 
                            estimate = causal_effect,
                            upper = causal_effect + 1.96*causal_effect_se
                            , row.names = "")







