library(tidyverse)
# Using qwraps library for summary table (https://cran.r-project.org/web/packages/qwraps2/vignettes/summary-statistics.html#building-a-data-summary-table)
library(qwraps2)

# Part 1 (MPG Regression)
# import data
mechaCar <- read_csv(file = 'MechaCar_mpg.csv')
# check for normality
ggplot(mechaCar,aes(x=mpg)) + geom_density()
# looks good but lets do a quantitative test
shapiro.test(mechaCar$mpg)
# Null hypothesis states data follows a normal distribution
# .7869 is greater than a significance level of 0.05 so its normal distribution

# Check for variance
summary(aov(mechaCar$mpg ~ mechaCar$`vehicle weight`))
summary(aov(mechaCar$mpg ~ mechaCar$`vehicle length`))
summary(aov(mechaCar$mpg ~ mechaCar$`AWD`))
summary(aov(mechaCar$mpg ~ mechaCar$`spoiler angle`))
summary(aov(mechaCar$mpg ~ mechaCar$`ground clearance`))

# create a correlation matrix to see how close they correlate
car_matrix <- as.matrix(mechaCar[,c("mpg","vehicle weight","vehicle length","ground clearance")])

# linear model
summary(lm(formula = mpg ~ `vehicle weight` + `vehicle length` + `ground clearance`, data = mechaCar))

# Part 2 (Suspension Coil Summary)
suspensionCoil <- read_csv(file = 'Suspension_Coil.csv')
suspension_summary <-
  list("Suspension Coil Ponds-Per-Inch" =
         list("mean"       = ~ mean(PSI),
              "median"       = ~ median(PSI),
              "variance" = ~ var(PSI),
              "standard deviation" = ~ sd(PSI)
    )
)

# Final table grouping by Manufacturing Lot
final_table_by_group <- summary_table(suspensionCoil, suspension_summary, by = "Manufacturing_Lot")
final_table_by_group

# Final table without grouping by Manufacturing_Lot
final_table_total <- summary_table(suspensionCoil, suspension_summary)
final_table_total

# Part 3 (Suspension Coil T-Test)
t.test(suspensionCoil$PSI, mu=mean(1500))

# Checking again to see if Manufacturing_Lot makes a difference
group1 <- suspensionCoil %>% filter(Manufacturing_Lot == 'Lot1')
group2 <- suspensionCoil %>% filter(Manufacturing_Lot == 'Lot2')
group3 <- suspensionCoil %>% filter(Manufacturing_Lot == 'Lot3')

t.test(group1$PSI, mu=mean(1500)) # p-value = 0.9048
t.test(group2$PSI, mu=mean(1500)) # p-value = 0.3451
t.test(group3$PSI, mu=mean(1500)) # p-value = 0.637

