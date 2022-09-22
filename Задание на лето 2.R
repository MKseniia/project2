library(dplyr)

my_data <- read.delim("Assignment 2.tsv")

my_data1 <- my_data %>% filter(Diagnosis.Age>65) %>% arrange(Diagnosis.Age)

my_data2 <- my_data %>% filter(Diagnosis.Age>65 & ER.Status=="Positive") %>% arrange(Diagnosis.Age)

my_data3 <- my_data %>% filter(Diagnosis.Age>65 | Diagnosis.Age<30) %>% arrange(desc(Diagnosis.Age))

my_data4 <- mutate(my_data, Fraction.Genome.Altered.Percent = Fraction.Genome.Altered * 100)

my_data5 <- my_data %>% group_by(Tumor.Stage) %>% summarize(count=n())

my_data6 <- my_data %>% group_by(ER.Status, HER2.Status) %>% summarize(count1=n())

my_data7 <- my_data %>% group_by(Tumor.Stage) %>% summarize(Mean.age=mean(Diagnosis.Age))

my_data8 <- my_data %>% group_by(Tumor.Stage) %>% summarize(max.TMB = max(TMB..nonsynonymous.))
