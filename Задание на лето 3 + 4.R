library(dplyr)
library(ggplot2)
library(rstatix)
library(readxl)
library(ggpubr)

# Задание 3 -----------

my_data1 <- read.delim("Assignment 2.tsv")
my_data2 <- read.delim("Assignment 3.tsv")

my_data <- inner_join(my_data1, my_data2, by=c('Sample.ID'='SAMPLE_ID'))

ggplot(my_data, aes(ER.Status, KRAS)) + geom_boxplot()

ggplot(my_data, aes(HER2.Status, KRAS)) + geom_boxplot()

ggplot(my_data, aes(HER2.Status, BRCA1)) + geom_boxplot()
ggplot(my_data, aes(HER2.Status, POLE)) + geom_boxplot()
ggplot(my_data, aes(ER.Status, BRCA1)) + geom_boxplot()
ggplot(my_data, aes(ER.Status, POLE)) + geom_boxplot()

ggplot(my_data, aes(POLE, BRCA1)) + geom_point() + geom_smooth(method="lm")

ggplot(my_data, aes(BRCA1, KRAS)) + geom_point() + geom_smooth(method="lm")
ggplot(my_data, aes(POLE, KRAS)) + geom_point() + geom_smooth(method="lm")

# Задание 4 ---------------

my_data_her2 <- filter(my_data, HER2.Status %in% c("Positive", "Negative"))
data_test <- my_data_her2 %>% group_by(HER2.Status) %>% shapiro_test(KRAS)
test1 <- my_data_her2 %>% wilcox_test(KRAS ~ HER2.Status)
bxp1 <- ggboxplot(my_data_her2, x = "HER2.Status", y = "KRAS", add = "jitter") + 
  labs(subtitle = get_test_label(test1, detailed = TRUE))
bxp1

data_test2 <- my_data %>% group_by(ER.Status) %>% shapiro_test(KRAS)  # Не работает
test2 <- my_data %>% kruskal_test(KRAS ~ ER.Status)
bxp2 <- ggboxplot(my_data, x = "ER.Status", y = "KRAS", add = "jitter") +
  labs(subtitle = get_test_label(test2, detailed = TRUE))
bxp2

data_test3 <- my_data %>% shapiro_test(BRCA1, POLE)
corr1 <- my_data %>% cor_test(BRCA1, POLE, method = "spearman")
ggplot(my_data, aes(BRCA1, POLE)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)