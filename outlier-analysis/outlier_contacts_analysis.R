rm(list=ls())
library(here)
library(dplyr)
library(ggplot2)
library(plotly)
library(GGally)
library(scales)
library(ggpubr)
library(lme4)
require(MASS)
library(lmerTest)

# Read in Data -----------------------------------------------------------------
unique <- readRDS(here("outlier-analysis/data/contact_unique.RDS"))
daily <- readRDS(here("outlier-analysis/data/contacts_daily.RDS"))

contact_summaries <- unique %>%
  distinct() %>%
  group_by() %>%
  dplyr::summarize(unique_q50 = quantile(avg_unique_contacts, probs = 0.5),
                   unique_q75 = quantile(avg_unique_contacts, probs = 0.75),
                   unique_q90 = quantile(avg_unique_contacts, probs = 0.90),
                   unique_mean = mean(avg_unique_contacts)) %>%
  cbind(daily %>%
          distinct() %>%
          ungroup() %>%
          dplyr::summarize(daily_q50 = quantile(avg_daily_contacts, probs = 0.5),
                           daily_q75 = quantile(avg_daily_contacts, probs = 0.75),
                           daily_q90 = quantile(avg_daily_contacts, probs = 0.90),
                           daily_mean = mean(avg_daily_contacts))
        )

daily$outlier_mean <- ifelse(daily$avg_daily_contacts > contact_summaries$daily_mean, 
                             1, 0)
daily$outlier_q75 <- ifelse(daily$avg_daily_contacts > contact_summaries$daily_q75, 
                             1, 0)
unique$outlier_mean <- ifelse(unique$avg_unique_contacts > contact_summaries$unique_mean, 
                             1, 0)
unique$outlier_q75 <- ifelse(unique$avg_unique_contacts > contact_summaries$unique_q75, 
                            1, 0)

daily$age <- as.numeric(daily$age)
unique$age <- as.numeric(unique$age)

# Daily Mean threshold outlier model -------------------------------------------------

logistic_model <- glm(outlier_mean ~ age + sex + site, data = daily, family = binomial())
mean_daily_logistic <- as.data.frame(summary(logistic_model)$coefficients)

logistic_int_model <- glm(outlier_mean ~ age * sex * site, data = daily, family = binomial())
mean_daily_logistic_int <- as.data.frame(summary(logistic_int_model)$coefficients)

# Daily Q75 threshold outlier model -------------------------------------------------

logistic_model <- glm(outlier_q75 ~ age + sex + site, data = daily, family = binomial())
q75_daily_logistic <- as.data.frame(summary(logistic_model)$coefficients)

logistic_int_model <- glm(outlier_q75 ~ age * sex * site, data = daily, family = binomial())
q75_daily_logistic_int <- as.data.frame(summary(logistic_int_model)$coefficients)

# Unique Mean threshold outlier model -------------------------------------------------

logistic_model <- glm(outlier_mean ~ age + sex + site, data = unique, family = binomial())
mean_unique_logistic <- as.data.frame(summary(logistic_model)$coefficients)

logistic_int_model <- glm(outlier_mean ~ age * sex * site, data = unique, family = binomial())
mean_unique_logistic_int <- as.data.frame(summary(logistic_int_model)$coefficients)

# Unique Q75 threshold outlier model -------------------------------------------------

logistic_model <- glm(outlier_q75 ~ age + sex + site, data = unique, family = binomial())
q75_unique_logistic <- as.data.frame(summary(logistic_model)$coefficients)

logistic_int_model <- glm(outlier_q75 ~ age * sex * site, data = unique, family = binomial())
q75_unique_logistic_int <- as.data.frame(summary(logistic_int_model)$coefficients)

# No interaction logistic plots---------------------------------------------------
mean_daily_logistic <- mean_daily_logistic %>% tibble::rownames_to_column()
names(mean_daily_logistic) <- c("term", "estimate", "SE", "test_statistic", "p_value")
mean_daily_logistic_int <- mean_daily_logistic_int %>% tibble::rownames_to_column()
names(mean_daily_logistic_int) <- names(mean_daily_logistic)
mean_unique_logistic <- mean_unique_logistic %>% tibble::rownames_to_column()
names(mean_unique_logistic) <- names(mean_daily_logistic)
mean_unique_logistic_int <- mean_unique_logistic_int %>% tibble::rownames_to_column()
names(mean_unique_logistic_int) <- names(mean_daily_logistic)

mean_daily_logistic$log_pvalue <- log(round(mean_daily_logistic$p_value, 2))
mean_daily_logistic$log_pvalue[which(is.infinite(mean_daily_logistic$log_pvalue))] <- -5.99

png("outlier-analysis/mean_daily_logistic.png", width=4000, height=1500, res=300)
ggplot(mean_daily_logistic, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("logistic: Mean Outlier Threshold for Daily Avg Contacts ~ Age + Sex + Site")
dev.off()

mean_unique_logistic$log_pvalue <- log(round(mean_unique_logistic$p_value, 2))
mean_unique_logistic$log_pvalue[which(is.infinite(mean_unique_logistic$log_pvalue))] <- -5.99

png("outlier-analysis/mean_unique_logistic.png", width=4000, height=1500, res=300)
ggplot(mean_unique_logistic, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("logistic: Mean Outlier Threshold for Unique Avg Contacts ~ Age + Sex + Site")
dev.off()

# Interaction logistic plots------------------------------------------------------
# mean_daily_logistic_int$facet <- c(rep("Main", 90),
#                                  rep("Age*Sex", 80),
#                                  rep("Age*Site & Sex*Site", 73),
#                                  rep("Age*Sex*Site", 56)) ->
#   mean_unique_logistic_int$facet 

mean_daily_logistic_int$log_pvalue <- log(round(mean_daily_logistic_int$p_value, 2))
mean_daily_logistic_int$log_pvalue[which(is.infinite(mean_daily_logistic_int$log_pvalue))] <- -5.99

png("outlier-analysis/mean_daily_logistic_int.png", width=4000, height=1500, res=300)
ggplot(mean_daily_logistic_int, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("logistic: Mean Outlier Threshold for Unique Avg Contacts ~ Age * Sex * Site")
dev.off()

mean_unique_logistic_int$log_pvalue <- log(round(mean_unique_logistic_int$p_value, 2))
mean_unique_logistic_int$log_pvalue[which(is.infinite(mean_unique_logistic_int$log_pvalue))] <- -5.99

png("outlier-analysis/mean_unique_logistic_int.png", width=4000, height=1500, res=300)
ggplot(mean_unique_logistic_int, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("logistic: Mean Outlier Threshold for Unique Avg Contacts ~ Age * Sex * Site")
dev.off()

# No interaction logistic plots---------------------------------------------------
q75_daily_logistic <- q75_daily_logistic %>% tibble::rownames_to_column()
names(q75_daily_logistic) <- c("term", "estimate", "SE", "test_statistic", "p_value")
q75_daily_logistic_int <- q75_daily_logistic_int %>% tibble::rownames_to_column()
names(q75_daily_logistic_int) <- names(q75_daily_logistic)
q75_unique_logistic <- q75_unique_logistic %>% tibble::rownames_to_column()
names(q75_unique_logistic) <- names(q75_daily_logistic)
q75_unique_logistic_int <- q75_unique_logistic_int %>% tibble::rownames_to_column()
names(q75_unique_logistic_int) <- names(q75_daily_logistic)

q75_daily_logistic$log_pvalue <- log(round(q75_daily_logistic$p_value, 2))
q75_daily_logistic$log_pvalue[which(is.infinite(q75_daily_logistic$log_pvalue))] <- -5.99

png("outlier-analysis/q75_daily_logistic.png", width=4000, height=1500, res=300)
ggplot(q75_daily_logistic, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("logistic: Q75 Outlier Threshold for Daily Avg Contacts ~ Age + Sex + Site")
dev.off()

q75_unique_logistic$log_pvalue <- log(round(q75_unique_logistic$p_value, 2))
q75_unique_logistic$log_pvalue[which(is.infinite(q75_unique_logistic$log_pvalue))] <- -5.99

png("outlier-analysis/q75_unique_logistic.png", width=4000, height=1500, res=300)
ggplot(q75_unique_logistic, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("logistic: Q75 Outlier Threshold for Unique Avg Contacts ~ Age + Sex + Site")
dev.off()

# Interaction logistic plots------------------------------------------------------
# q75_daily_logistic_int$facet <- c(rep("Main", 90),
#                                  rep("Age*Sex", 80),
#                                  rep("Age*Site & Sex*Site", 73),
#                                  rep("Age*Sex*Site", 56)) ->
#   q75_unique_logistic_int$facet 

q75_daily_logistic_int$log_pvalue <- log(round(q75_daily_logistic_int$p_value, 2))
q75_daily_logistic_int$log_pvalue[which(is.infinite(q75_daily_logistic_int$log_pvalue))] <- -5.99

png("outlier-analysis/q75_daily_logistic_int.png", width=4000, height=1500, res=300)
ggplot(q75_daily_logistic_int, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("logistic: Q75 Outlier Threshold for Unique Avg Contacts ~ Age * Sex * Site")
dev.off()

q75_unique_logistic_int$log_pvalue <- log(round(q75_unique_logistic_int$p_value, 2))
q75_unique_logistic_int$log_pvalue[which(is.infinite(q75_unique_logistic_int$log_pvalue))] <- -5.99

png("outlier-analysis/q75_unique_logistic_int.png", width=4000, height=1500, res=300)
ggplot(q75_unique_logistic_int, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("logistic: Q75 Outlier Threshold for Unique Avg Contacts ~ Age * Sex * Site")
dev.off()

