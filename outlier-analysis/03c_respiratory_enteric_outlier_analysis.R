rm(list=ls())
library(here)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(MASS)

contacts_resp_ent <- readRDS(here("outlier-analysis/data/contacts_resp_ent.RDS"))

contacts_resp_ent$age <- as.numeric(contacts_resp_ent$age)
contacts_resp_ent$sex <- contacts_resp_ent$participant_sex
contacts_resp_ent$site <- contacts_resp_ent$study_site

# Daily Mean threshold outlier model -------------------------------------------------

daily_resp_mean_model <- glm(daily_resp_mean_outlier ~ participant_age + sex + site, 
                      data = contacts_resp_ent, family = binomial())
daily_resp_mean <- as.data.frame(summary(daily_resp_mean_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

daily_ent_mean_model <- glm(daily_ent_mean_outlier ~ participant_age + sex + site, 
                             data = contacts_resp_ent, family = binomial())
daily_ent_mean <- as.data.frame(summary(daily_ent_mean_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

# Daily Q75 threshold outlier model -------------------------------------------------

daily_resp_q75_model <- glm(daily_resp_q75_outlier ~ participant_age + sex + site, 
                             data = contacts_resp_ent, family = binomial())
daily_resp_q75 <- as.data.frame(summary(daily_resp_q75_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

daily_ent_q75_model <- glm(daily_ent_q75_outlier ~ participant_age + sex + site, 
                            data = contacts_resp_ent, family = binomial())
daily_ent_q75 <- as.data.frame(summary(daily_ent_q75_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

# No interaction figures --------------------------------------------------------

names(daily_resp_mean) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(daily_ent_mean) <- c("term", "estimate", "SE", "test_statistic", "p_value")

names(daily_resp_q75) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(daily_ent_q75) <- c("term", "estimate", "SE", "test_statistic", "p_value")

daily_resp_mean$log_pvalue <- log(round( daily_resp_mean$p_value, 2))
daily_resp_mean$log_pvalue[which(is.infinite(daily_resp_mean$log_pvalue))] <- -5.99

png("daily_resp_mean.png", width=3000, height=1000, res=300)
ggplot(daily_resp_mean, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Logistic: Mean Outlier Threshold for Daily Avg Respiratory Contacts ~ participant_age + Sex + Site")
dev.off()

daily_ent_mean$log_pvalue <- log(round( daily_ent_mean$p_value, 2))
daily_ent_mean$log_pvalue[which(is.infinite(daily_ent_mean$log_pvalue))] <- -5.99

png("daily_ent_mean.png", width=3000, height=1000, res=300)
ggplot(daily_ent_mean, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Logistic: Mean Outlier Threshold for Daily Avg Enteric Contacts ~ participant_age + Sex + Site")
dev.off()

daily_resp_q75$log_pvalue <- log(round( daily_resp_q75$p_value, 2))
daily_resp_q75$log_pvalue[which(is.infinite(daily_resp_q75$log_pvalue))] <- -5.99

png("daily_resp_q75.png", width=3000, height=1000, res=300)
ggplot(daily_resp_q75, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Logistic: Q75 Outlier Threshold for Daily Avg Respiratory Contacts ~ participant_age + Sex + Site")
dev.off()

daily_ent_q75$log_pvalue <- log(round( daily_ent_q75$p_value, 2))
daily_ent_q75$log_pvalue[which(is.infinite(daily_ent_q75$log_pvalue))] <- -5.99

png("daily_ent_q75.png", width=3000, height=1000, res=300)
ggplot(daily_ent_q75, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Logistic: Q75 Outlier Threshold for Daily Avg Enteric Contacts ~ participant_age + Sex + Site")
dev.off()
