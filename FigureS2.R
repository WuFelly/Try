library(DALEX)          # v2.4.3
library(DALEXtra)       # v2.3.0
library(ggpubr)         # v0.6.0
library(knitr)          # v1.46
library(iBreakDown)     # v2.1.2
library(patchwork)      # v1.2.0
library(dplyr)          # v1.0.7


#Model explanation==============================================================
set.seed(123)
explainer_rf <- DALEX::explain(rf_final_fit,
                               data = rd_test[, -5],
                               y = rd_test$log_mean_mehg,
                               label = "Random Forest")

explainer_cubist <- DALEX::explain(cubist_final_fit,
                                   data = rd_test[, -5],
                                   y = rd_test$log_mean_mehg,
                                   label = "Cubist")

explainer_svmrbf <- DALEX::explain(svmrbf_final_fit,
                                   data = rd_test_transformed[, -11],
                                   y = rd_test_transformed$log_mean_mehg,
                                   label = "SVM_RBF")

explainer_glm <- DALEX::explain(glm_final_fit,
                                data = rd_test_transformed[, -11],
                                y = rd_test_transformed$log_mean_mehg,
                                label = "Glmnet")

rf_feature <- explainer_rf %>% model_parts() %>% plot(show_boxplots = F) +
  ggtitle("Feature Importance ", "")
cubist_feature <- explainer_cubist %>% model_parts() %>% plot(show_boxplots = F) +
  ggtitle("Feature Importance ", "")
svmrbf_feature <- explainer_svmrbf %>% model_parts() %>% plot(show_boxplots = F) +
  ggtitle("Feature Importance ", "")
glm_feature <- explainer_glm %>% model_parts() %>% plot(show_boxplots = F) +
  ggtitle("Feature Importance ", "")

pfi_combined <- (rf_feature|cubist_feature)/(svmrbf_feature|glm_feature)
#ggsave("pfi_combined.pdf", plot = pfi_combined, width = 16, height = 9, unit = 'in')

rf_mp <- model_parts(explainer_rf, loss_function = loss_root_mean_square)
cubist_mp <- model_parts(explainer_cubist, loss_function = loss_root_mean_square)
svmrbf_mp <- model_parts(explainer_svmrbf, loss_function = loss_root_mean_square)
glm_mp <- model_parts(explainer_glm, loss_function = loss_root_mean_square)

## Mean PFI for 1-10th permutation
rf_mean_pfi <- rf_mp %>% filter(permutation != 0) %>% group_by(variable) %>% 
  summarise(mean_pfi = mean(dropout_loss))
cubist_mean_pfi <- cubist_mp %>% filter(permutation != 0) %>% group_by(variable) %>%
  summarise(mean_pfi = mean(dropout_loss))
svmrbf_mean_pfi <- svmrbf_mp %>% filter(permutation != 0) %>% group_by(variable) %>%
  summarise(mean_pfi = mean(dropout_loss))
glm_mean_pfi <- glm_mp %>% filter(permutation != 0) %>% group_by(variable) %>%
  summarise(mean_pfi = mean(dropout_loss))

# write.csv(rf_mean_pfi, "rf_mean_pfi.csv")
# write.csv(cubist_mean_pfi, "cubist_mean_pfi.csv")
# write.csv(svmrbf_mean_pfi, "svmrbf_mean_pfi.csv")
# write.csv(glm_mean_pfi, "glm_mean_pfi.csv")

## PFI plot
rf_mean_pfi <- rf_mean_pfi %>%
  filter(!variable %in% c("_baseline_", "_full_model_"))

name_mapping <- c(
  'Tx30day' = 'Two_m_temperature_yearmax_runmean_K',
  'pev' = 'Potential_evaporation_m',
  'tp' = 'Precipitation_yearsum_mm',
  'wind' = 'Ten_metre_wind_speed',
  'CH4 emission' = 'log_ch4_em',
  'Hg emission' = 'log_hg_em_lag2',
  'sampling year' = 'Collection_year',
  'weight' = 'log_weight',
  'trophic level' = 'Trophic_level',
  'ssrd' = 'Surface_net_downward_shortwave_flux',
  'trophic class' = 'Trophic_class'
)

# PDP plot======================================================================
## Random Forest
model_profile_rf <- model_profile(explainer_rf, type = 'partial',
                                  variables = c('Trophic_level','log_weight', 
                                                'Two_m_temperature_yearmax_runmean_K',
                                                'Surface_net_downward_shortwave_flux',
                                                'Potential_evaporation_m', 
                                                'Collection_year', 'log_hg_em_lag2', 
                                                'Precipitation_yearsum_mm',
                                                'Ten_metre_wind_speed', 'log_ch4_em'
                                  ))
## Cubist
model_profile_cubist <- model_profile(explainer_cubist, type = 'partial',
                                      variables = c('Trophic_level','log_weight', 
                                                    'Two_m_temperature_yearmax_runmean_K',
                                                    'Surface_net_downward_shortwave_flux',
                                                    'Potential_evaporation_m', 
                                                    'Collection_year', 'log_hg_em_lag2', 
                                                    'Precipitation_yearsum_mm',
                                                    'Ten_metre_wind_speed', 'log_ch4_em'
                                      ))
## SVM_RBF
model_profile_svmrbf <- model_profile(explainer_svmrbf, type = 'partial',
                                      variables = c('Trophic_level','log_weight', 
                                                    'Two_m_temperature_yearmax_runmean_K',
                                                    'Surface_net_downward_shortwave_flux',
                                                    'Potential_evaporation_m', 
                                                    'Collection_year', 'log_hg_em_lag2', 
                                                    'Precipitation_yearsum_mm',
                                                    'Ten_metre_wind_speed', 'log_ch4_em'
                                      ))
## Glmnet
model_profile_glm <- model_profile(explainer_glm, type = 'partial',
                                   variables = c('Trophic_level','log_weight', 
                                                 'Two_m_temperature_yearmax_runmean_K',
                                                 'Surface_net_downward_shortwave_flux',
                                                 'Potential_evaporation_m', 
                                                 'Collection_year', 'log_hg_em_lag2', 
                                                 'Precipitation_yearsum_mm',
                                                 'Ten_metre_wind_speed', 'log_ch4_em'
                                   ))

pdp_combined <- plot(model_profile_rf, model_profile_cubist, model_profile_svmrbf,model_profile_glm, 
                     variables = c('Trophic_level','log_weight', 
                                   'Two_m_temperature_yearmax_runmean_K',
                                   'Potential_evaporation_m', 
                                   'Surface_net_downward_shortwave_flux',
                                   'Collection_year', 'log_hg_em_lag2', 
                                   'Precipitation_yearsum_mm',
                                   'log_ch4_em', 'Ten_metre_wind_speed')) + 
  ggtitle("Partial dependence profile", "") 
