# Create ATE, ATEC, ATET error bars for irrigated and rainfed

library(ggplot2)
library(grf)
library(magrittr)
library(dplyr)

rfscratchFolder <- '/# Insert results folder from previous analysis'
# dataset
overlaprf <- readRDS(paste0(rfscratchFolder, '/overlap.rds'))

Y_hatrf <- readRDS(paste0(rfscratchFolder, '/Y_hat.rds'))
cfrf <- readRDS(paste0(rfscratchFolder, '/cf_xc1.rds'))
varImprf <- readRDS(paste0(rfscratchFolder, '/cf_xc1_varImp.rds'))

# process results ------------------------------
# add predicted treatment effects to data 
constOob_predictrf <- predict(cfrf)

# add hats to original data frame
overlapW0rf <- overlaprf %>%
  bind_cols(data.frame(Y_hat = Y_hatrf))

colVarsrf <- varImprf %>% dplyr::pull(variable)

overlapWrf <- overlapW0rf %>%
  mutate_at(colVarsrf, list(Q4 = ~ntile(., 4),
                          Q5 = ~ntile(., 5)))

overlapTaurf <- overlapWrf %>%
  bind_cols(constOob_predictrf) 

ate_cf_aipwrf = average_treatment_effect(cfrf, target.sample= 'all')
tauhat_rf_aipw = c(ATE=ate_cf_aipwrf["estimate"],
                   lower_ci=ate_cf_aipwrf["estimate"] - 1.96 * ate_cf_aipwrf["std.err"],
                   upper_ci=ate_cf_aipwrf["estimate"] + 1.96 * ate_cf_aipwrf["std.err"])
tauhat_rf_aipw

# as a percentage of yield
meanYieldrf <- mean(overlapTaurf$Y)
meanYieldrf

tauhat_rf_aipw_percent <- tauhat_rf_aipw/meanYieldrf * 100
tauhat_rf_aipw_percent

atec_cf_aipwrf = average_treatment_effect(cfrf, target.sample= 'control')
tauhat_rf_atec = c(ATE=atec_cf_aipwrf["estimate"],
                   lower_ci=atec_cf_aipwrf["estimate"] - 1.96 * atec_cf_aipwrf["std.err"],
                   upper_ci=atec_cf_aipwrf["estimate"] + 1.96 * atec_cf_aipwrf["std.err"])
tauhat_rf_atec

# as a percentage of yield
meanYieldrf <- mean(overlapTaurf$Y)
meanYieldrf

tauhat_rf_atec_percent <- tauhat_rf_atec/meanYieldrf * 100
tauhat_rf_atec_percent

atet_cf_aipwrf = average_treatment_effect(cfrf, target.sample= 'treated')
tauhat_rf_atet = c(ATE=atet_cf_aipwrf["estimate"],
                   lower_ci=atet_cf_aipwrf["estimate"] - 1.96 * atet_cf_aipwrf["std.err"],
                   upper_ci=atet_cf_aipwrf["estimate"] + 1.96 * atet_cf_aipwrf["std.err"])
tauhat_rf_atet

# as a percentage of yield
meanYieldrf <- mean(overlapTaurf$Y)
meanYieldrf

tauhat_rf_atet_percent <- tauhat_rf_atet/meanYieldrf * 100
tauhat_rf_atet_percent

#  ------------------------------------------------------------------------
irscratchFolder <- '/Volumes/LaCie/2021_Semi_Arid_Tillage/05.10Results/03.11AUG23_Irrigated_ATE_Maize_Results'
# dataset
overlapir <- readRDS(paste0(irscratchFolder, '/overlap.rds'))

Y_hatir <- readRDS(paste0(irscratchFolder, '/Y_hat.rds'))
cfir <- readRDS(paste0(irscratchFolder, '/cf_xc1.rds'))
varImpir <- readRDS(paste0(irscratchFolder, '/cf_xc1_varImp.rds'))

# process results ------------------------------
# add predicted treatment effects to data 
constOob_predictir <- predict(cfir)

# add hats to original data frame
overlapW0ir <- overlapir %>%
  bind_cols(data.frame(Y_hat = Y_hatir))

colVarsir <- varImpir %>% dplyr::pull(variable)

overlapWir <- overlapW0ir %>%
  mutate_at(colVarsir, list(Q4 = ~ntile(., 4),
                            Q5 = ~ntile(., 5)))

overlapTauir <- overlapWir %>%
  bind_cols(constOob_predictir) 

ate_cf_aipwir = average_treatment_effect(cfir, target.sample= 'all')
tauhat_ir_aipw = c(ATE=ate_cf_aipwir["estimate"],
                   lower_ci=ate_cf_aipwir["estimate"] - 1.96 * ate_cf_aipwir["std.err"],
                   upper_ci=ate_cf_aipwir["estimate"] + 1.96 * ate_cf_aipwir["std.err"])
tauhat_ir_aipw

# as a percentage of yield
meanYieldir <- mean(overlapTauir$Y)
meanYieldir

tauhat_ir_aipw_percent <- tauhat_ir_aipw/meanYieldir * 100
tauhat_ir_aipw_percent

atec_cf_aipwir = average_treatment_effect(cfir, target.sample= 'control')
tauhat_ir_atec = c(ATE=atec_cf_aipwir["estimate"],
                   lower_ci=atec_cf_aipwir["estimate"] - 1.96 * atec_cf_aipwir["std.err"],
                   upper_ci=atec_cf_aipwir["estimate"] + 1.96 * atec_cf_aipwir["std.err"])
tauhat_ir_atec

# as a percentage of yield
meanYieldir <- mean(overlapTauir$Y)
meanYieldir

tauhat_ir_atec_percent <- tauhat_ir_atec/meanYieldir * 100
tauhat_ir_atec_percent

atet_cf_aipwir = average_treatment_effect(cfir, target.sample= 'treated')
tauhat_ir_atet = c(ATE=atet_cf_aipwir["estimate"],
                   lower_ci=atet_cf_aipwir["estimate"] - 1.96 * atet_cf_aipwir["std.err"],
                   upper_ci=atet_cf_aipwir["estimate"] + 1.96 * atet_cf_aipwir["std.err"])
tauhat_ir_atet

# as a percentage of yield
meanYieldir <- mean(overlapTauir$Y)
meanYieldir

tauhat_ir_atet_percent <- tauhat_ir_atet/meanYieldir * 100
tauhat_ir_atet_percent

## now make df
df.CI <- data.frame(Parameter = c("ATE", "ATEC", "ATET", "ATE", "ATEC", "ATET"),
                    Irrigation = c("Rainfed","Rainfed","Rainfed","Irrigated","Irrigated", "Irrigated"),
                    Estimate = c(tauhat_rf_aipw_percent[1],tauhat_rf_atec_percent[1], tauhat_rf_atet_percent[1],
                                 tauhat_ir_aipw_percent[1],tauhat_ir_atec_percent[1], tauhat_ir_atet_percent[1]),
                    Lower = c(tauhat_rf_aipw_percent[2],tauhat_rf_atec_percent[2], tauhat_rf_atet_percent[2],
                              tauhat_ir_aipw_percent[2],tauhat_ir_atec_percent[2], tauhat_ir_atet_percent[2]),
                    Upper = c(tauhat_rf_aipw_percent[3],tauhat_rf_atec_percent[3], tauhat_rf_atet_percent[3],
                              tauhat_ir_aipw_percent[3],tauhat_ir_atec_percent[3], tauhat_ir_atet_percent[3]))

ggplot(df.CI, aes(x = Parameter, y = Estimate, color = Irrigation))+
  geom_point() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper)) +
  facet_wrap(~ Irrigation) +
  theme(text = element_text(size = 20)) +
  ggtitle('Average Treatment Effects From Causal Forest') +
  scale_y_continuous(name = "Estimate (% Yield)") +
  theme_bw()
  






