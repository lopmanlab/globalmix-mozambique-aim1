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


# Models for Daily Respiratory Contacts ----------------------------------------

negbin_daily_resp_mod <- glm.nb(avg_daily_resp_contacts ~ age + sex + site,
                                data = contacts_resp_ent)
negbin_daily_resp <- as.data.frame(summary(negbin_daily_resp_mod)$coefficients)%>% 
  tibble::rownames_to_column()

negbin_daily_resp_mod_int <- glm.nb(avg_daily_resp_contacts ~ age * sex * site,
                                data = contacts_resp_ent)
negbin_daily_resp_int <- as.data.frame(summary(negbin_daily_resp_mod_int)$coefficients) %>% 
  tibble::rownames_to_column()

linear_daily_resp_mod <- glm(avg_daily_resp_contacts ~ age + sex + site,
                                data = contacts_resp_ent)
linear_daily_resp <- summary(linear_daily_resp_mod)$coefficients %>%
  as.data.frame() %>% 
  tibble::rownames_to_column()

linear_daily_resp_mod_int <- glm(avg_daily_resp_contacts ~ age * sex * site,
                                    data = contacts_resp_ent)
linear_daily_resp_int <- summary(linear_daily_resp_mod_int)$coefficients %>%
  as.data.frame() %>% 
  tibble::rownames_to_column()

poisson_daily_resp_mod <- glm(avg_daily_resp_contacts ~ age + sex + site,
                             data = contacts_resp_ent,
                             family = poisson(link = "log"))
poisson_daily_resp <- summary(poisson_daily_resp_mod)$coefficients %>%
  as.data.frame() %>% 
  tibble::rownames_to_column()

poisson_daily_resp_mod_int <- glm(avg_daily_resp_contacts ~ age * sex * site,
                                 data = contacts_resp_ent,
                                 family = poisson(link = "log"))
poisson_daily_resp_int <- summary(poisson_daily_resp_mod_int)$coefficients %>%
  as.data.frame() %>% 
  tibble::rownames_to_column()

# Models for Daily Enteric Contacts ----------------------------------------

negbin_daily_ent_mod <- glm.nb(avg_daily_ent_contacts ~ age + sex + site,
                                data = contacts_resp_ent)
negbin_daily_ent <- summary(negbin_daily_ent_mod)$coefficients %>%
  as.data.frame() %>% 
  tibble::rownames_to_column()

negbin_daily_ent_mod_int <- glm.nb(avg_daily_ent_contacts ~ age * sex * site,
                                    data = contacts_resp_ent)
negbin_daily_ent_int <- summary(negbin_daily_ent_mod_int)$coefficients %>%
  as.data.frame() %>% 
  tibble::rownames_to_column()

linear_daily_ent_mod <- glm(avg_daily_ent_contacts ~ age + sex + site,
                             data = contacts_resp_ent)
linear_daily_ent <- summary(linear_daily_ent_mod)$coefficients %>%
  as.data.frame() %>% 
  tibble::rownames_to_column()

linear_daily_ent_mod_int <- glm(avg_daily_ent_contacts ~ age * sex * site,
                                 data = contacts_resp_ent)
linear_daily_ent_int <- summary(linear_daily_ent_mod_int)$coefficients %>%
  as.data.frame() %>% 
  tibble::rownames_to_column()

poisson_daily_ent_mod <- glm(avg_daily_ent_contacts ~ age + sex + site,
                              data = contacts_resp_ent,
                             family = poisson(link = "log"))
poisson_daily_ent <- summary(poisson_daily_ent_mod)$coefficients %>%
  as.data.frame() %>% 
  tibble::rownames_to_column()

poisson_daily_ent_mod_int <- glm(avg_daily_ent_contacts ~ age * sex * site,
                                  data = contacts_resp_ent,
                                 family = poisson(link = "log"))
poisson_daily_ent_int <- summary(poisson_daily_ent_mod_int)$coefficients %>%
  as.data.frame() %>% 
  tibble::rownames_to_column()

# Models for Unique Respiratory Contacts ----------------------------------------

negbin_unique_resp_mod <- glm.nb(avg_unique_resp_contacts ~ age + sex + site,
                                data = contacts_resp_ent)
negbin_unique_resp <- summary(negbin_unique_resp_mod)$coefficients %>%
  as.data.frame() %>% 
  tibble::rownames_to_column()

negbin_unique_resp_mod_int <- glm.nb(avg_unique_resp_contacts ~ age * sex * site,
                                    data = contacts_resp_ent)
negbin_unique_resp_int <- summary(negbin_unique_resp_mod_int)$coefficients %>%
  as.data.frame() %>% 
  tibble::rownames_to_column()

linear_unique_resp_mod <- glm(avg_unique_resp_contacts ~ age + sex + site,
                             data = contacts_resp_ent)
linear_unique_resp <- summary(linear_unique_resp_mod)$coefficients %>%
  as.data.frame() %>% 
  tibble::rownames_to_column()

linear_unique_resp_mod_int <- glm(avg_unique_resp_contacts ~ age * sex * site,
                                 data = contacts_resp_ent)
linear_unique_resp_int <- summary(linear_unique_resp_mod_int)$coefficients %>%
  as.data.frame() %>% 
  tibble::rownames_to_column()

poisson_unique_resp_mod <- glm(avg_unique_resp_contacts ~ age + sex + site,
                              data = contacts_resp_ent,
                              family = poisson(link = "log"))
poisson_unique_resp <- summary(poisson_unique_resp_mod)$coefficients %>%
  as.data.frame() %>% 
  tibble::rownames_to_column()

poisson_unique_resp_mod_int <- glm(avg_unique_resp_contacts ~ age * sex * site,
                                  data = contacts_resp_ent,
                                  family = poisson(link = "log"))
poisson_unique_resp_int <- summary(poisson_unique_resp_mod_int)$coefficients %>%
  as.data.frame() %>% 
  tibble::rownames_to_column()

# Models for Unique Enteric Contacts ----------------------------------------

negbin_unique_ent_mod <- glm.nb(avg_unique_ent_contacts ~ age + sex + site,
                               data = contacts_resp_ent)
negbin_unique_ent <- summary(negbin_unique_ent_mod)$coefficients %>%
  as.data.frame() %>% 
  tibble::rownames_to_column()

negbin_unique_ent_mod_int <- glm.nb(avg_unique_ent_contacts ~ age * sex * site,
                                   data = contacts_resp_ent)
negbin_unique_ent_int <- summary(negbin_unique_ent_mod_int)$coefficients %>%
  as.data.frame() %>% 
  tibble::rownames_to_column()

linear_unique_ent_mod <- glm(avg_unique_ent_contacts ~ age + sex + site,
                            data = contacts_resp_ent)
linear_unique_ent <- summary(linear_unique_ent_mod)$coefficients %>%
  as.data.frame() %>% 
  tibble::rownames_to_column()

linear_unique_ent_mod_int <- glm(avg_unique_ent_contacts ~ age * sex * site,
                                data = contacts_resp_ent)
linear_unique_ent_int <- summary(linear_unique_ent_mod_int)$coefficients %>%
  as.data.frame() %>% 
  tibble::rownames_to_column()

poisson_unique_ent_mod <- glm(avg_unique_ent_contacts ~ age + sex + site,
                             data = contacts_resp_ent,
                             family = poisson(link = "log"))
poisson_unique_ent <- summary(poisson_unique_ent_mod)$coefficients %>%
  as.data.frame() %>% 
  tibble::rownames_to_column()

poisson_unique_ent_mod_int <- glm(avg_unique_ent_contacts ~ age * sex * site,
                                 data = contacts_resp_ent,
                                 family = poisson(link = "log"))
poisson_unique_ent_int <- summary(poisson_unique_ent_mod_int)$coefficients %>%
  as.data.frame() %>% 
  tibble::rownames_to_column()

# Daily No interaction figures ----------------------------------------------

names(negbin_daily_resp) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(linear_daily_resp) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(poisson_daily_resp) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(negbin_daily_ent) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(linear_daily_ent) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(poisson_daily_ent) <- c("term", "estimate", "SE", "test_statistic", "p_value")

negbin_daily_resp$log_pvalue <- log(round(negbin_daily_resp$p_value, 2))
negbin_daily_resp$log_pvalue[which(is.infinite(negbin_daily_resp$log_pvalue))] <- -5.99

png("outlier-analysis/negbin_daily_resp.png", width=4000, height=1500, res=300)
ggplot(negbin_daily_resp, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Negative Binomial: Daily Average Respiratory Contacts ~ Age + Sex + Site")
dev.off()

negbin_daily_ent$log_pvalue <- log(round(negbin_daily_ent$p_value, 2))
negbin_daily_ent$log_pvalue[which(is.infinite(negbin_daily_ent$log_pvalue))] <- -5.99

png("outlier-analysis/negbin_daily_ent.png", width=4000, height=1500, res=300)
ggplot(negbin_daily_ent, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Negative Binomial: Daily Average Enteric Contacts ~ Age + Sex + Site")
dev.off()


linear_daily_resp$log_pvalue <- log(round(linear_daily_resp$p_value, 2))
linear_daily_resp$log_pvalue[which(is.infinite(linear_daily_resp$log_pvalue))] <- -5.99

png("outlier-analysis/linear_daily_resp.png", width=4000, height=1500, res=300)
ggplot(linear_daily_resp, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Linear: Daily Average Respiratory Contacts ~ Age + Sex + Site")
dev.off()

linear_daily_ent$log_pvalue <- log(round(linear_daily_ent$p_value, 2))
linear_daily_ent$log_pvalue[which(is.infinite(linear_daily_ent$log_pvalue))] <- -5.99

png("outlier-analysis/linear_daily_ent.png", width=4000, height=1500, res=300)
ggplot(linear_daily_ent, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Linear: Daily Average Enteric Contacts ~ Age + Sex + Site")
dev.off()


poisson_daily_resp$log_pvalue <- log(round(poisson_daily_resp$p_value, 2))
poisson_daily_resp$log_pvalue[which(is.infinite(poisson_daily_resp$log_pvalue))] <- -5.99

png("outlier-analysis/poisson_daily_resp.png", width=4000, height=1500, res=300)
ggplot(poisson_daily_resp, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Poisson: Daily Average Respiratory Contacts ~ Age + Sex + Site")
dev.off()

poisson_daily_ent$log_pvalue <- log(round(poisson_daily_ent$p_value, 2))
poisson_daily_ent$log_pvalue[which(is.infinite(poisson_daily_ent$log_pvalue))] <- -5.99

png("outlier-analysis/poisson_daily_ent.png", width=4000, height=1500, res=300)
ggplot(poisson_daily_ent, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Poisson: Daily Average Enteric Contacts ~ Age + Sex + Site")
dev.off()

# Unique No interaction figures ----------------------------------------------

names(negbin_unique_resp) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(linear_unique_resp) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(poisson_unique_resp) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(negbin_unique_ent) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(linear_unique_ent) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(poisson_unique_ent) <- c("term", "estimate", "SE", "test_statistic", "p_value")

negbin_unique_resp$log_pvalue <- log(round(negbin_unique_resp$p_value, 2))
negbin_unique_resp$log_pvalue[which(is.infinite(negbin_unique_resp$log_pvalue))] <- -5.99

png("outlier-analysis/negbin_unique_resp.png", width=4000, height=1500, res=300)
ggplot(negbin_unique_resp, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Negative Binomial: Unique Average Respiratory Contacts ~ Age + Sex + Site")
dev.off()

negbin_unique_ent$log_pvalue <- log(round(negbin_unique_ent$p_value, 2))
negbin_unique_ent$log_pvalue[which(is.infinite(negbin_unique_ent$log_pvalue))] <- -5.99

png("outlier-analysis/negbin_unique_ent.png", width=4000, height=1500, res=300)
ggplot(negbin_unique_ent, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Negative Binomial: Unique Average Enteric Contacts ~ Age + Sex + Site")
dev.off()


linear_unique_resp$log_pvalue <- log(round(linear_unique_resp$p_value, 2))
linear_unique_resp$log_pvalue[which(is.infinite(linear_unique_resp$log_pvalue))] <- -5.99

png("outlier-analysis/linear_unique_resp.png", width=4000, height=1500, res=300)
ggplot(linear_unique_resp, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Linear: Unique Average Respiratory Contacts ~ Age + Sex + Site")
dev.off()

linear_unique_ent$log_pvalue <- log(round(linear_unique_ent$p_value, 2))
linear_unique_ent$log_pvalue[which(is.infinite(linear_unique_ent$log_pvalue))] <- -5.99

png("outlier-analysis/linear_unique_ent.png", width=4000, height=1500, res=300)
ggplot(linear_unique_ent, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Linear: Unique Average Enteric Contacts ~ Age + Sex + Site")
dev.off()


poisson_unique_resp$log_pvalue <- log(round(poisson_unique_resp$p_value, 2))
poisson_unique_resp$log_pvalue[which(is.infinite(poisson_unique_resp$log_pvalue))] <- -5.99

png("outlier-analysis/poisson_unique_resp.png", width=4000, height=1500, res=300)
ggplot(poisson_unique_resp, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Poisson: Unique Average Respiratory Contacts ~ Age + Sex + Site")
dev.off()

poisson_unique_ent$log_pvalue <- log(round(poisson_unique_ent$p_value, 2))
poisson_unique_ent$log_pvalue[which(is.infinite(poisson_unique_ent$log_pvalue))] <- -5.99

png("outlier-analysis/poisson_unique_ent.png", width=4000, height=1500, res=300)
ggplot(poisson_unique_ent, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Poisson: Unique Average Enteric Contacts ~ Age + Sex + Site")
dev.off()

# Continuous Age ---------------------------------------------------------------

contacts_resp_ent$age <- as.character(contacts_resp_ent$age)

negbin_daily_resp_mod <- glm.nb(avg_daily_resp_contacts ~ age + sex + site,
                                data = contacts_resp_ent)
negbin_daily_resp <- as.data.frame(summary(negbin_daily_resp_mod)$coefficients)%>% 
  tibble::rownames_to_column()

negbin_daily_ent_mod <- glm.nb(avg_daily_ent_contacts ~ age + sex + site,
                                data = contacts_resp_ent)
negbin_daily_ent <- as.data.frame(summary(negbin_daily_ent_mod)$coefficients)%>% 
  tibble::rownames_to_column()

names(negbin_daily_resp) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(negbin_daily_ent) <- c("term", "estimate", "SE", "test_statistic", "p_value")

negbin_daily_resp$log_pvalue <- log(round(negbin_daily_resp$p_value, 2))
negbin_daily_resp$log_pvalue[which(is.infinite(negbin_daily_resp$log_pvalue))] <- -5.99

png("outlier-analysis/negbin_daily_resp_cont.png", width=4000, height=1500, res=300)
ggplot(negbin_daily_resp, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Negative Binomial: Daily Average Respiratory Contacts ~ Age + Sex + Site")
dev.off()

negbin_daily_ent$log_pvalue <- log(round(negbin_daily_ent$p_value, 2))
negbin_daily_ent$log_pvalue[which(is.infinite(negbin_daily_ent$log_pvalue))] <- -5.99

png("outlier-analysis/negbin_daily_ent_cont.png", width=4000, height=1500, res=300)
ggplot(negbin_daily_ent, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Negative Binomial: Daily Average Enteric Contacts ~ Age + Sex + Site")
dev.off()

# Grouped Age ---------------------------------------------------------------

negbin_daily_resp_mod <- glm.nb(avg_daily_resp_contacts ~ participant_age + sex + site,
                                data = contacts_resp_ent)
negbin_daily_resp <- as.data.frame(summary(negbin_daily_resp_mod)$coefficients)%>% 
  tibble::rownames_to_column()

negbin_daily_ent_mod <- glm.nb(avg_daily_ent_contacts ~ participant_age + sex + site,
                               data = contacts_resp_ent)
negbin_daily_ent <- as.data.frame(summary(negbin_daily_ent_mod)$coefficients)%>% 
  tibble::rownames_to_column()

names(negbin_daily_resp) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(negbin_daily_ent) <- c("term", "estimate", "SE", "test_statistic", "p_value")

negbin_daily_resp$log_pvalue <- log(round(negbin_daily_resp$p_value, 2))
negbin_daily_resp$log_pvalue[which(is.infinite(negbin_daily_resp$log_pvalue))] <- -5.99

png("negbin_daily_resp_grp.png", width=4000, height=1500, res=300)
ggplot(negbin_daily_resp, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Negative Binomial: Daily Average Respiratory Contacts ~ Age + Sex + Site")
dev.off()

negbin_daily_ent$log_pvalue <- log(round(negbin_daily_ent$p_value, 2))
negbin_daily_ent$log_pvalue[which(is.infinite(negbin_daily_ent$log_pvalue))] <- -5.99

png("negbin_daily_ent_grp.png", width=4000, height=1500, res=300)
ggplot(negbin_daily_ent, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Negative Binomial: Daily Average Enteric Contacts ~ Age + Sex + Site")
dev.off()

# Save data --------------------------------------------------------------------

write.csv(negbin_daily_resp, "negbin_daily_resp.csv")
write.csv(negbin_daily_ent, "negbin_daily_ent.csv")


