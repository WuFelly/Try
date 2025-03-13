library(tidymodels)       # v1.1.1
library(caret)            # v6.0-94
library(dplyr)            # v1.1.4
library(recipes)          # v1.0.9
library(ranger)           # v0.16.0
library(randomForest)     # v4.7-1.1
library(rpart)            # v4.1.21
library(kernlab)          # v0.9-32
library(glmnet)           # v4.1-8
library(parsnip)          # v1.1.1
library(workflows)        # v1.1.3
library(workflowsets)     # v1.0.1
library(doParallel)       # v1.0.17
library(ggplot2)          # v3.5.0
library(ggpubr)           # v0.6.0
library(knitr)            # v1.46
library(gridExtra)        # v2.3
library(rules)            # v1.0.2

# Load data "ML_PDP_SHAP.RDATA"
load("ML_PDP_SHAP.RDATA")



cubist_plot <- cubist_pred %>%
  cbind(observed = rd_test$log_mean_mehg) %>%
  ggplot(aes(x=observed, y=.pred))+
  geom_point(shape = 21, size = 3, stroke = 0.5, fill = "#7fccba", color = 'black') +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#F46D75", linewidth = 0.5) +
  coord_obs_pred()+
  labs(x='Observed MeHg content (log-scaled)', y= 'Predicted MeHg content (log-scaled)', title = '') +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_text(hjust = 0.5, size = 16),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.text.y = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_blank(),
        panel.grid.major  = element_blank(),
        panel.background = element_blank())

cubist_plot

## SVM-RBF

svmrbf_plot <- svmrbf_pred %>%
  cbind(observed = rd_test_transformed$log_mean_mehg) %>%
  ggplot(aes(x=observed, y=.pred))+
  geom_point(shape = 21, size = 3, stroke = 0.5, fill = "#7fccba", color = 'black') +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#F46D75", linewidth = 0.5) +
  coord_obs_pred()+
  labs(x='Observed MeHg content (log-scaled)', y= 'Predicted MeHg content (log-scaled)', title = '') +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.title = element_text(size = 14),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.text = element_text(size = 12, color = "black"),
        panel.grid.major  = element_blank(),
        panel.background = element_blank())

svmrbf_plot

## GLM

glm_plot <- glm_pred %>%
  cbind(observed = rd_test_transformed$log_mean_mehg) %>%
  ggplot(aes(x=observed, y=.pred))+
  geom_point(shape = 21, size = 3, stroke = 0.5, fill = "#7fccba", color = 'black') +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#F46D75", linewidth = 0.5) +
  coord_obs_pred()+
  labs(x='Observed MeHg content (log-scaled)', y= 'Predicted MeHg content (log-scaled)', title = '') +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.title = element_text(size = 16),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.text = element_text(size = 14, color = "black"),
        panel.grid.major  = element_blank(),
        panel.background = element_blank())

glm_plot

## Linear regression

lm_plot <- lm_plot_data %>%
  ggplot(aes(x=observed, y=predicted))+
  geom_point(shape = 21, size = 3, stroke = 0.5, fill = "#7fccba", color = 'black') +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#F46D75", linewidth = 0.5) +
  coord_obs_pred()+
  labs(x='Observed MeHg content (log-scaled)', y= 'Predicted MeHg content (log-scaled)', title = '') +
  #xlim(c(0,3)) + 
  #ylim(c(0,3)) + 
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.title = element_text(size = 16),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_blank(),
        panel.grid.major  = element_blank(),
        panel.background = element_blank())


# Model performance metrics=====================================================
## rsq and rmse on training set
model_names_tr <- c("glm_final_fit", "svmrbf_final_fit")

results_norm_tr <- data.frame(Model = character(), R_squared = numeric(), 
                              RMSE = numeric(), Mean_Pred = numeric(), stringsAsFactors = FALSE)

for (i in seq_along(model_names_tr)) {
  model_name <- model_names_tr[i]
  predicted_values <- predict(get(model_name), new_data = rd_train_transformed)$.pred
  mean_pred <- mean(predicted_values)
  ss_total <- sum((rd_train_transformed$log_mean_mehg - mean(rd_train_transformed$log_mean_mehg))^2)
  ss_residual <- sum((rd_train_transformed$log_mean_mehg - predicted_values)^2)
  rsq <- 1 - (ss_residual / ss_total)
  rmse_value <- sqrt(mean((rd_train_transformed$log_mean_mehg - predicted_values)^2))
  results_norm_tr <- rbind(results_norm_tr, data.frame(Model = model_name, R_squared = rsq, RMSE = rmse_value, Mean_Predicted = mean_pred))
}

model_names_basic_tr <- c("rf_final_fit", "cubist_final_fit")

results_basic_tr <- data.frame(Model = character(), R_squared = numeric(), 
                               RMSE = numeric(), Mean_Pred = numeric(), stringsAsFactors = FALSE)

for (i in seq_along(model_names_basic_tr)) {
  model_name <- model_names_basic_tr[i]
  predicted_values <- predict(get(model_name), new_data = rd_train)$.pred
  mean_pred <- mean(predicted_values)
  ss_total <- sum((rd_train$log_mean_mehg - mean(rd_train$log_mean_mehg))^2)
  ss_residual <- sum((rd_train$log_mean_mehg - predicted_values)^2)
  rsq <- 1 - (ss_residual / ss_total)
  rmse_value <- sqrt(mean((rd_train$log_mean_mehg - predicted_values)^2))
  results_basic_tr <- rbind(results_basic_tr, data.frame(Model = model_name, R_squared = rsq, RMSE = rmse_value, Mean_Predicted = mean_pred))
}

## rsq and rmse on test set
model_names <- c("glm_pred", "svmrbf_pred")

results_norm <- data.frame(Model = character(), R_squared = numeric(), 
                           RMSE = numeric(), Mean_Pred = numeric(), stringsAsFactors = FALSE)

for (i in seq_along(model_names)) {
  model_name <- model_names[i]
  predicted_values <- get(model_name)$.pred
  mean_pred <- mean(predicted_values)
  ss_total <- sum((rd_test_transformed$log_mean_mehg - mean(rd_test_transformed$log_mean_mehg))^2)
  ss_residual <- sum((rd_test_transformed$log_mean_mehg - predicted_values)^2)
  rsq <- 1 - (ss_residual / ss_total)
  rmse_value <- sqrt(mean((rd_test_transformed$log_mean_mehg - predicted_values)^2))
  results_norm <- rbind(results_norm, data.frame(Model = model_name, R_squared = rsq, RMSE = rmse_value, Mean_Predicted = mean_pred))
}

model_names_basic <- c("rf_pred", "cubist_pred")

results_basic <- data.frame(Model = character(), R_squared = numeric(), 
                            RMSE = numeric(), Mean_Pred = numeric(), stringsAsFactors = FALSE)

for (i in seq_along(model_names_basic)) {
  model_name <- model_names_basic[i]
  predicted_values <- get(model_name)$.pred
  mean_pred <- mean(predicted_values)
  ss_total <- sum((rd_test$log_mean_mehg - mean(rd_test$log_mean_mehg))^2)
  ss_residual <- sum((rd_test$log_mean_mehg - predicted_values)^2)
  rsq <- 1 - (ss_residual / ss_total)
  rmse_value <- sqrt(mean((rd_test$log_mean_mehg - predicted_values)^2))
  results_basic <- rbind(results_basic, 
                         data.frame(Model = model_name, R_squared = rsq, RMSE = rmse_value, Mean_Predicted = mean_pred))
}




