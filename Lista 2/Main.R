# Limpa as variaveis anteriores
rm(list = ls())


#  Part A

# Write a do-file that simulates 10.000 samples with 300 observations each
# of a model Yi = 10 + 0.1*Xi + ui, where Xi ~ N(2; 1) and ui ~ N(0; 1).
# Store the OLS estimate and the standard error of b1 in each simulation.
# Plot the histogram of your b1 estimates. How does it look like?

# Carreca funcoes locais
# source("./Lista 2/functions.R")

#  numero total de amostras ensaiadas
sampleTotal = 1e4

# Numero total de obseracoes em cada amosra
N_obs=300;

# declara uma matrix que vai conter os resultados
result = matrix(NA, nrow=sampleTotal, ncol=4)
colnames(result) = c("b0", "b1", "var(b0)", "var(b1)")

for (i in 1:sampleTotal)
{
  x = rnorm(N_obs, mean = 2, sd = 1)
  u = rnorm(N_obs, mean = 0, sd = 1)
  
  y = 10 + 0.1*x + u
  
  data = data.frame(y, x) 
  
  fit = lm("y ~ x", data = data)
  
  result[i, c("b0", "b1")] = fit$coefficients;
  result[i, c("var(b0)", "var(b1)")] = diag(vcov(fit));
  # result[i, c("tt")] = summary(fit)$coefficients["x","t value"];
  # a$coefficients["x","t value"]
  
}

hist(result[ ,"b0"], breaks = 20)
hist(result[ ,"b1"], breaks = 20)



# Part B
# Analyzing these 10.000 estimates of b1, what is the mean and the 
# standard error of b1? Are these values  similar to what you would predict?
colMeans(result)
diag(var(result))

# Part C
# For each simulation, calculate a test statistic to test the hypothesis 
# that b1 = 0.1 at 5% significance level. What is the critical value?
# What is the proportion of cases that you reject the null?
# Is this what you would expect?
t_0.1 = (result[ ,"b1"]- 0.1) / sqrt(result[ ,"var(b1)"])
print(sum(t_0.1 > 1.96 | t_0.1 < -1.96)/sampleTotal)


# Part D
# For each simulation, calculate a test statistic to test the hypothesis 
# that b1 = 0.95, b1 = 0.5 and b1 = 0 at 5% significance level. 
# What is the critical value? Do you reject the null more or less often 
# than in the previous item? Is this what you would expect?
t_0.95 = (result[ ,"b1"]- 0.95) / sqrt(result[ ,"var(b1)"])
print(sum(t_0.95 > 1.96 | t_0.95 < -1.96)/sampleTotal)

t_0.50 = (result[ ,"b1"]- 0.5) / sqrt(result[ ,"var(b1)"])
print(sum(t_0.50 > 1.96 | t_0.50 < -1.96)/sampleTotal)

t_0 = (result[ ,"b1"]- 0) / sqrt(result[ ,"var(b1)"])
print(sum(t_0 > 1.96 | t_0 < -1.96)/sampleTotal)

