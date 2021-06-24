library("Rlab")
library("DescTools")
library("EnvStats")
library("summarytools")

# simulation :
height = rnorm(200, mean = 170, sd = 9.4)
sex = rbern(200, prob = 0.7)
error = runif(200, min = 85, max = 100)
weight = sapply(height, '-', error)

# amar tosifi :
k = floor(1 + log(200, 2))
weight_freq = freq(weight, breaks = k)
print(weight_freq)

male = sum(sex == 1)
female = sum(sex == 0)
sex_chart_data = c(male, female)
sex_chart_labels = c("male", "female")
pie(sex_chart_data, sex_chart_labels, main = "pie chart")
hist(height)

max_height = max(height)
min_height = min(height)
median_height = median(height)
var_height = var(height)
mean_height = mean(height)

max_sex = max(sex)
min_sex = min(sex)
median_sex = median(sex)
var_sex = var(sex)
mean_sex = mean(sex)

max_error = max(error)
min_error = min(error)
median_error = median(error)
var_error = var(error)
mean_error = mean(error)

max_weight = max(weight)
min_weight = min(weight)
median_weight = median(weight)
var_weight = var(weight)
mean_weight = mean(weight)

# azmoon haye amari :
test_1 = t.test(height, mu = 170, conf_level = 0.1)
print(test_1)
