library(tidyverse)
library(readxl)
library(janitor)
library(naniar)
library(skimr)
library(ggpubr)
library(gt)
library(psych)

#data <- read_excel(file.choose())
data <- read_excel("Water Bill.xlsx")
View(data)
data_clean <- data %>%
  select(where(~ mean(is.na(.)) < 0.5))
View(data_clean)

data_clean <- data_clean %>%
  select(-Remark)
View(data_clean)
data_clean <- data_clean[-13, ]
View(data_clean)

data<- data_clean
View(data)

summary(data)

describe(data)
table(data$Bill)
t.test(`Consumption per Month` ~ Bill,
       data = data)
anova_model <-
  aov(`Consumption per Month` ~ Bill,
      data=data)

summary(anova_model)

TukeyHSD(anova_model)

table1 <-
  table(data$Rate,
        data$Bill)

chisq.test(table1)


cor(data$Bill,
    data$Rate,
    use="complete.obs")

ggplot(data,
       aes(Bill))+
  geom_histogram(
    bins=20,
    fill="forestgreen",
    color="black")+
  theme_bw()


ggplot(data,
       aes(Bill))+
  
  geom_bar(fill="darkgreen")+
  
  theme_bw()+
  
  labs(
    title="Bill Distribution",
    x="Bills",
    y="Amount")

data %>%
  
  count(Bill) %>%
  
  ggplot(aes(
    "",
    n,
    fill=Bill))+
  
  geom_col()+
  
  coord_polar("y")

ggplot(data,
       aes(Bill,
           Rate,
           fill=Bill))+
  
  geom_boxplot()+
  
  theme_bw()


ggplot(data,
       aes(Bill,
           Rate))+
  
  geom_point(size=3,
             color="blue")+
  
  geom_smooth(
    method="lm")+
  
  theme_bw()

ggsave(
  "TreeHeight.png",
  width=8,
  height=6,
  dpi=300)

install.packages("writexl")

library(writexl)

write_xlsx(data,
           "Cleaned_Data.xlsx")


























