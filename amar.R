library("Rlab")
library("DescTools")
library("EnvStats")
library("summarytools")

# simulation :
height = rnorm(200, mean = 170, sd = 9.4)
sex = rbern(200, prob = 0.7)
error = runif(200, min = 85, max = 100)
weight = height - error
print(weight)
# amar tosifi :
k = floor(1 + log(200, 2))
print(k)

min_weight= min(weight)
max_weight= max(weight)

range = floor((max_weight - min_weight) / k)
print(range)

breaks_ = c()

for(i in seq(1, 8)){
	breaks_ = c(breaks_, min_weight + i * range);
}

print(breaks_);

weight_freq = Freq(weight, breaks = seq(min_weight, max_weight, by = range))
weight_freq_freq = weight_freq$freq

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
# amoon e aval :
# H0 : miu > 170
# H1 : miu < 170
# ebteda test ro anjam midim ta amare azmoon be dast biad
# baad nahie bohrani ro mohasebe mikonim
# baad check mikonim ke amare azmoon dar nahie bohrani gharar migire ya na
# az tarafi variance jamiat 90 e
# pas variance majhool nist
# bana bar in az azmoon e shomare 2 estefade mikonim :

test_1 = t.test(height, alternative = "less", mu = 170, conf_level = 0.9)
# az test meghdar t be dast miad
print(test_1)

# hala nahie bohrani ro hesab mikonim :

c1 = pnorm(0.9)
print(c1) 

print(-0.47 > -c1)
# natije false pass farz H rad mishe


# azmoon e dovom :
# H0 : variance < 90 
# H1 : variance > 90
# variance jamiat maloome pas az azmoon e 7 estefade mikonim :
# alpha = 0.05 va 1 - a = 0.95 va 1 - (a/2) = 0.975

test_2 = varTest(weight, sigma.squared = 90, alternative = "greater", conf.level = 0.95)
print(test_2)

# meghdar e amare az test_2 be dast miad va bayad
# meghdar e c (nahie bohrani) ro hesab konim

c2 = qchisq(0.95, 199)
print(c2)

print(258 > c2)

# azmoon e 3vom
# azmoon variance baraye ghad e mardan va zanan

ghad_mardan = c()
ghad_zanan = c()
for(i in seq(1, 200)){
	if(sex[i] == 1){
		ghad_mardan = c(ghad_mardan, height[i])
	}
	else{
		ghad_zanan = c(ghad_zanan, height[i])
	}	
}

print(ghad_mardan)
print(ghad_zanan)

# az azmoon e 17 estefade mikonim :
n1 = length(ghad_mardan)
n2 = length(ghad_zanan)

# nahie bohrani :
c3 = qf(0.99, n1, n2)
print(c3)

test_3 = var.test(ghad_mardan, ghad_zanan, ratio = 1, alternative = "two.sided" ,conf.level = 0.99)
print(test_3)

print(1.302 < c3)


# inja test e miangin anjam midim :
test_4 = t.test(ghad_mardan, ghad_zanan, mu = 0, alternative = "two.sided", paired = FALSE,var.equal = FALSE,conf.level = 0.99)
print(test_4)

#value mishe 0.219
# meghdar e C :
c4 = pnorm(0.99)
print(c4)
print(c4 < 0.219)

# test barazandegi :
# ghesmat e alef ro dar ghesmat e simulation anjam dadim
# faravani har baze dar vector e weight_freq_freq rikhte shode


