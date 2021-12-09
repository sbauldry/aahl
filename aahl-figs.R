### Purpose: create figures for item-response probabilities
### Author:  S Bauldry
### Date:    Dec 8, 2021

setwd("~/desktop")
library(tidyverse)
library(ggpubr)

### load estimates saved from Stata
irp <- read_csv("aahl-irp.csv")

### calculate confidence intervals
irp <- irp %>%
  mutate(ub = ifelse(irp + se < 1, irp + se, 1),
         lb = ifelse(irp - se > 0, irp - se, 0))

### graph for men
f1a <- ggplot(data = subset(irp, fem == 0 & lst == 1), mapping = aes(x = as.factor(vr), y = irp)) +
  geom_bar(stat = "identity") +
  geom_linerange(mapping = aes(x = as.factor(vr), ymin = lb, ymax = ub)) +
  scale_x_discrete(label = c("Y\nEx", "Y\nFV", "Y\nSD", "N", "P\nSmoke", "C", "A", "M\nDrink", "H"),
                   name = "") +
  scale_y_continuous(limits = c(0, 1), name = "probablity") +
  labs(title = "Unhealthy Smoker (19%)") +
  theme_light()

f1b <- ggplot(data = subset(irp, fem == 0 & lst == 2), mapping = aes(x = as.factor(vr), y = irp)) +
  geom_bar(stat = "identity") +
  geom_linerange(mapping = aes(x = as.factor(vr), ymin = lb, ymax = ub)) +
  scale_x_discrete(label = c("Y\nEx", "Y\nFV", "Y\nSD", "N", "P\nSmoke", "C", "A", "M\nDrink", "H"),
                   name = "") +
  scale_y_continuous(limits = c(0, 1), name = "probablity") +
  labs(title = "Healthy Diet (55%)") +
  theme_light()

f1c <- ggplot(data = subset(irp, fem == 0 & lst == 3), mapping = aes(x = as.factor(vr), y = irp)) +
  geom_bar(stat = "identity") +
  geom_linerange(mapping = aes(x = as.factor(vr), ymin = lb, ymax = ub)) +
  scale_x_discrete(label = c("Y\nEx", "Y\nFV", "Y\nSD", "N", "P\nSmoke", "C", "A", "M\nDrink", "H"),
                   name = "") +
  scale_y_continuous(limits = c(0, 1), name = "probablity") +
  labs(title = "Unhealthy Diet (27%)") +
  theme_light()

f1 <- ggarrange(f1a, f1c, f1b)
ggsave("~/desktop/aahl-fig1.pdf", plot = f1, dpi = 300)


### graph for women
f2a <- ggplot(data = subset(irp, fem == 1 & lst == 1), mapping = aes(x = as.factor(vr), y = irp)) +
  geom_bar(stat = "identity") +
  geom_linerange(mapping = aes(x = as.factor(vr), ymin = lb, ymax = ub)) +
  scale_x_discrete(label = c("Y\nEx", "Y\nFV", "Y\nSD", "N", "P\nSmoke", "C", "A", "M\nDrink", "H"),
                   name = "") +
  scale_y_continuous(limits = c(0, 1), name = "probablity") +
  labs(title = "Unhealthy Smoker (10%)") +
  theme_light()

f2b <- ggplot(data = subset(irp, fem == 1 & lst == 2), mapping = aes(x = as.factor(vr), y = irp)) +
  geom_bar(stat = "identity") +
  geom_linerange(mapping = aes(x = as.factor(vr), ymin = lb, ymax = ub)) +
  scale_x_discrete(label = c("Y\nEx", "Y\nFV", "Y\nSD", "N", "P\nSmoke", "C", "A", "M\nDrink", "H"),
                   name = "") +
  scale_y_continuous(limits = c(0, 1), name = "probablity") +
  labs(title = "Unhealthy Diet (11%)") +
  theme_light()

f2c <- ggplot(data = subset(irp, fem == 1 & lst == 3), mapping = aes(x = as.factor(vr), y = irp)) +
  geom_bar(stat = "identity") +
  geom_linerange(mapping = aes(x = as.factor(vr), ymin = lb, ymax = ub)) +
  scale_x_discrete(label = c("Y\nEx", "Y\nFV", "Y\nSD", "N", "P\nSmoke", "C", "A", "M\nDrink", "H"),
                   name = "") +
  scale_y_continuous(limits = c(0, 1), name = "probablity") +
  labs(title = "Healthy Diet (77%)") +
  theme_light()

f2d <- ggplot(data = subset(irp, fem == 1 & lst == 4), mapping = aes(x = as.factor(vr), y = irp)) +
  geom_bar(stat = "identity") +
  geom_linerange(mapping = aes(x = as.factor(vr), ymin = lb, ymax = ub)) +
  scale_x_discrete(label = c("Y\nEx", "Y\nFV", "Y\nSD", "N", "P\nSmoke", "C", "A", "M\nDrink", "H"),
                   name = "") +
  scale_y_continuous(limits = c(0, 1), name = "probablity") +
  labs(title = "Most Healthy (2%)") +
  theme_light()

f2 <- ggarrange(f2a, f2b, f2c, f2d)
ggsave("~/desktop/aahl-fig2.pdf", plot = f2, dpi = 300)

