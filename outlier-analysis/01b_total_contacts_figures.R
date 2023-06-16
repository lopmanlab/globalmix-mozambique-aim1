rm(list=ls())
library(here)
library(dplyr)
library(ggplot2)
library(plotly)
library(GGally)
library(scales)
library(ggpubr)

# Read in Data -----------------------------------------------------------------

unique_linear <- read.csv("outlier-analysis/data/unique_linear.csv")
unique_linear_int <- read.csv("outlier-analysis/data/unique_linear_int.csv")
unique_poisson <- read.csv("outlier-analysis/data/unique_poisson.csv")
unique_poisson_int <- read.csv("outlier-analysis/data/unique_poisson_int.csv")
unique_negbin <- read.csv("outlier-analysis/data/unique_negbin.csv")
unique_negbin_int <- read.csv("outlier-analysis/data/unique_negbin_int.csv")

daily_linear <- read.csv("outlier-analysis/data/daily_linear.csv")
daily_linear_int <- read.csv("outlier-analysis/data/daily_linear_int.csv")
daily_poisson <- read.csv("outlier-analysis/data/daily_poisson.csv")
daily_poisson_int <- read.csv("outlier-analysis/data/daily_poisson_int.csv")
daily_negbin <- read.csv("outlier-analysis/data/daily_negbin.csv")
daily_negbin_int <- read.csv("outlier-analysis/data/daily_negbin_int.csv")

# Interaction plots-------------------------------------------------------------
names(daily_linear_int) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(daily_poisson_int) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(daily_negbin_int) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(unique_linear_int) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(unique_poisson_int) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(unique_negbin_int) <- c("term", "estimate", "SE", "test_statistic", "p_value")

daily_linear_int$facet <- c(rep("Main", 90),
                            rep("Age*Sex", 80),
                            rep("Age*Site & Sex*Site", 73),
                            rep("Age*Sex*Site", 56)) ->
  daily_poisson_int$facet ->
  daily_negbin_int$facet ->
  unique_linear_int$facet ->
  unique_poisson_int$facet ->
  unique_negbin_int$facet 

daily_linear_int$log_pvalue <- log(round(daily_linear_int$p_value, 2))
daily_linear_int$log_pvalue[which(is.infinite(daily_linear_int$log_pvalue))] <- -5.99

p1 <- ggplot(daily_linear_int %>% filter(facet == "Main"), aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Linear: Daily Average Contacts ~ Age * Sex * Site\nMain")

p2 <- ggplot(daily_linear_int %>% filter(facet == "Age*Sex"), aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Age*Sex")

p3 <- ggplot(daily_linear_int %>% filter(facet == "Age*Site & Sex*Site"), aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Age*Site & Sex*Site")

p4 <- ggplot(daily_linear_int %>% filter(facet == "Age*Sex*Site"), aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Age*Sex*Site")

png("outlier-analysis/daily_linear_int.png", width=4000, height=5000, res=300)
ggarrange(p1, p2, p3, p4, nrow=4, ncol=1)
dev.off()

daily_poisson_int$log_pvalue <- log(round(daily_poisson_int$p_value, 2))
daily_poisson_int$log_pvalue[which(is.infinite(daily_poisson_int$log_pvalue))] <- -5.99

p1 <- ggplot(daily_poisson_int %>% filter(facet == "Main"), aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Poisson: Daily Average Contacts ~ Age * Sex * Site\nMain")

p2 <- ggplot(daily_poisson_int %>% filter(facet == "Age*Sex"), aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Age*Sex")

p3 <- ggplot(daily_poisson_int %>% filter(facet == "Age*Site & Sex*Site"), aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Age*Site & Sex*Site")

p4 <- ggplot(daily_poisson_int %>% filter(facet == "Age*Sex*Site"), aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Age*Sex*Site")

png("outlier-analysis/daily_poisson_int.png", width=4000, height=5000, res=300)
ggarrange(p1, p2, p3, p4, nrow=4, ncol=1)
dev.off()

daily_negbin_int$log_pvalue <- log(round(daily_negbin_int$p_value, 2))
daily_negbin_int$log_pvalue[which(is.infinite(daily_negbin_int$log_pvalue))] <- -5.99

p1 <- ggplot(daily_negbin_int %>% filter(facet == "Main"), aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Negative Binomial: Daily Average Contacts ~ Age * Sex * Site\nMain")

p2 <- ggplot(daily_negbin_int %>% filter(facet == "Age*Sex"), aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Age*Sex")

p3 <- ggplot(daily_negbin_int %>% filter(facet == "Age*Site & Sex*Site"), aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Age*Site & Sex*Site")

p4 <- ggplot(daily_negbin_int %>% filter(facet == "Age*Sex*Site"), aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Age*Sex*Site")

png("outlier-analysis/daily_negbin_int.png", width=4000, height=5000, res=300)
ggarrange(p1, p2, p3, p4, nrow=4, ncol=1)
dev.off()

unique_linear_int$log_pvalue <- log(round(unique_linear_int$p_value, 2))
unique_linear_int$log_pvalue[which(is.infinite(unique_linear_int$log_pvalue))] <- -5.99

p1 <- ggplot(unique_linear_int %>% filter(facet == "Main"), aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Linear: Unique Daily Average Contacts ~ Age + Sex + Site\nMain")

p2 <- ggplot(unique_linear_int %>% filter(facet == "Age*Sex"), aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Age*Sex")

p3 <- ggplot(unique_linear_int %>% filter(facet == "Age*Site & Sex*Site"), aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Age*Site & Sex*Site")

p4 <- ggplot(unique_linear_int %>% filter(facet == "Age*Sex*Site"), aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Age*Sex*Site")


png("outlier-analysis/unique_linear_int.png", width=4000, height=5000, res=300)
ggarrange(p1, p2, p3, p4, nrow=4, ncol=1)
dev.off()

unique_poisson_int$log_pvalue <- log(round(unique_poisson_int$p_value, 2))
unique_poisson_int$log_pvalue[which(is.infinite(unique_poisson_int$log_pvalue))] <- -5.99

p1 <- ggplot(unique_poisson_int %>% filter(facet == "Main"), aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Poisson: Unique Daily Average Contacts ~ Age + Sex + Site\nMain")

p2 <- ggplot(unique_poisson_int %>% filter(facet == "Age*Sex"), aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Age*Sex")

p3 <- ggplot(unique_poisson_int %>% filter(facet == "Age*Site & Sex*Site"), aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Age*Site & Sex*Site")

p4 <- ggplot(unique_poisson_int %>% filter(facet == "Age*Sex*Site"), aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Age*Sex*Site")

png("outlier-analysis/unique_poisson_int.png", width=4000, height=5000, res=300)
ggarrange(p1, p2, p3, p4, nrow=4, ncol=1)
dev.off()

unique_negbin_int$log_pvalue <- log(round(unique_negbin_int$p_value, 2))
unique_negbin_int$log_pvalue[which(is.infinite(unique_negbin_int$log_pvalue))] <- -5.99

p1 <- ggplot(unique_negbin_int %>% filter(facet == "Main"), aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Negative Binomial: Unique Daily Average Contacts ~ Age + Sex + Site\nMain")

p2 <- ggplot(unique_negbin_int %>% filter(facet == "Age*Sex"), aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Age*Sex")

p3 <- ggplot(unique_negbin_int %>% filter(facet == "Age*Site & Sex*Site"), aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Age*Site & Sex*Site")

p4 <- ggplot(unique_negbin_int %>% filter(facet == "Age*Sex*Site"), aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Age*Sex*Site")


png("outlier-analysis/unique_negbin_int.png", width=4000, height=5000, res=300)
ggarrange(p1, p2, p3, p4, nrow=4, ncol=1)
dev.off()

# No interaction plots----------------------------------------------------------
names(daily_linear) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(daily_poisson) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(daily_negbin) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(unique_linear) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(unique_poisson) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(unique_negbin) <- c("term", "estimate", "SE", "test_statistic", "p_value")

daily_linear$log_pvalue <- log(round(daily_linear$p_value, 2))
daily_linear$log_pvalue[which(is.infinite(daily_linear$log_pvalue))] <- -5.99

png("outlier-analysis/daily_linear.png", width=4000, height=1500, res=300)
ggplot(daily_linear, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Linear: Daily Average Contacts ~ Age + Sex + Site")
dev.off()

daily_poisson$log_pvalue <- log(round(daily_poisson$p_value, 2))
daily_poisson$log_pvalue[which(is.infinite(daily_poisson$log_pvalue))] <- -5.99

png("outlier-analysis/daily_poisson.png", width=4000, height=1500, res=300)
ggplot(daily_poisson, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Poisson: Daily Average Contacts ~ Age + Sex + Site")
dev.off()

daily_negbin$log_pvalue <- log(round(daily_negbin$p_value, 2))
daily_negbin$log_pvalue[which(is.infinite(daily_negbin$log_pvalue))] <- -5.99

png("outlier-analysis/daily_negbin.png", width=4000, height=1500, res=300)
ggplot(daily_negbin, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Negative Binomial: Daily Average Contacts ~ Age + Sex + Site")
dev.off()

unique_linear$log_pvalue <- log(round(unique_linear$p_value, 2))
unique_linear$log_pvalue[which(is.infinite(unique_linear$log_pvalue))] <- -5.99

png("outlier-analysis/unique_linear.png", width=4000, height=1500, res=300)
ggplot(unique_linear, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Linear: Unique Daily Average Contacts ~ Age + Sex + Site")
dev.off()

unique_poisson$log_pvalue <- log(round(unique_poisson$p_value, 2))
unique_poisson$log_pvalue[which(is.infinite(unique_poisson$log_pvalue))] <- -5.99

png("outlier-analysis/unique_poisson.png", width=4000, height=1500, res=300)
ggplot(unique_poisson, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Poisson: Unique Daily Average Contacts ~ Age + Sex + Site")
dev.off()

unique_negbin$log_pvalue <- log(round(unique_negbin$p_value, 2))
unique_negbin$log_pvalue[which(is.infinite(unique_negbin$log_pvalue))] <- -5.99

png("outlier-analysis/unique_negbin.png", width=4000, height=1500, res=300)
ggplot(unique_negbin, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Negative Binomial: Unique Daily Average Contacts ~ Age + Sex + Site")
dev.off()

