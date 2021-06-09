library("Rlab")
library("DescTools")
library("EnvStats")
library("summarytools")

height = rnorm(200, mean = 170, sd = 9.4)
sex = rbern(200, prob = 0.7)
error = runif(200, min = 85, max = 100)
weight = sapply(height, '-', error)

k = floor(1 + log(200, 2))
weight_freq = freq(weight, breaks = k)