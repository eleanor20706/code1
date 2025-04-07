library(readxl)
MYDATA<- read_excel("subject analysis.xlsx")

#Normality histograms
hist(MYDATA$relc)
hist(MYDATA$relb)
hist(MYDATA$relr)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(tidyr)
library(tidyr)
library(car)
install.packages("dplyr")
library(dplyr)
#long format
df_long <- MYDATA %>%
  pivot_longer(
    cols = c(relr, relc, relb),  # Columns to gather
    names_to = "shape",          # New column for shape type
    values_to = "value"          # New column for values
  ) %>%
  mutate(
    `volunteer` = as.factor(`volunteer`),  # Convert Volunteer name to factor
    shape = as.factor(shape)  # Convert shape to factor
  )


install.packages("ez")
library(ez)
repeat1 <- ezANOVA(data = df_long, 
                   dv = .(value),          
                   wid = .(volunteer),     
                   within = .(shape),      
                   detailed = TRUE,        
                   type = 3)
repeat1

#post hoc
attach(MYDATA)
t1<-t.test(relr,relc,paired=T)
t2<-t.test(relr,relb,paired=T)
t3<-t.test(relb,relc,paired=T)

pvalues <- c(t1$p.value, t2$p.value, t3$p.value)

adjusted_pvalues <- p.adjust(pvalues, method = "bonferroni")

summary_table <- data.frame(
  Comparison = c("relr vs relc", "relr vs relb", "relb vs relc"),
  p_value = round(pvalues, 4),
  p_adjusted = round(adjusted_pvalues, 4)
)

print(summary_table)  # Comparison p_value p_adjusted
#1 relr vs relc  0.5638 1
#2 relr vs relb  0.6793 1
#3 relb vs relc  0.6704 1
> 
