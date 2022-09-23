library(rstatix)
library(readr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(readxl)
library(purrr)

my_data1 <- read.delim("Assignment 2.tsv")
my_data2 <- read.delim("Assignment 3.tsv")

my_data <- inner_join(my_data1, my_data2, by=c("Sample.ID" = "SAMPLE_ID"))

fun1 <- function(a, gene) {
  test <- a %>% kruskal_test(gene ~ ER.Status)
  if (a[1, 5] < 0.05) {
    dunn_test <- a %>% dunn_test(gene ~ ER.Status)
    dunn_test
    ggplot(a, aes(x = ER.Status, y = gene)) + geom_boxplot() + 
      labs(subtitle = get_test_label(test, detailed=TRUE), caption = get_pwc_label(dunn_test))
  }
  else {
    test
    ggplot(a, aes(x = ER.Status, y = gene)) + geom_boxplot() + 
      labs(subtitle = get_test_label(test, detailed=TRUE))
  }
}

fun1(my_data, my_data$KRAS)