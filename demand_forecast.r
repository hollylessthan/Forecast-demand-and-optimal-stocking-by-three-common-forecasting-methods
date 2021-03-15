#------------------#
# Forecasting
#------------------#
rm(list=ls(all.names=TRUE))
#------------------#
library(dplyr)
library(tidyr)
install.packages("skimr")
library(skimr)
library(stats)
library(MASS)
library(ggplot2)
#------------------#
#-------------------------#
# Load data
#-------------------------#
dm <- readr::read_csv("demand_data.txt")

# Add time trend
dm <- dm %>%
  mutate(t = row_number()) %>%
  as_tibble()

# Descriptives
summary(dm)
skim(dm)
head(dm, 5)

#-------------------------#
# Visualize
#-------------------------#
hist(dm$demand)

dm %>%
  ggplot(aes(x = demand)) +
  geom_histogram()

dm %>%
  ggplot(aes(x = t, y = demand)) +
  geom_line()
#-------------------------#
# Setting
#-------------------------#
#The price is $4, the unit cost is $0.80, and the salvage value is $0. 
price <- 4
cost <- 0.8
salvage <- 0

# What we know
sl<-(price - cost) / (price - salvage) # target service level
optimal_order_quantity <- as.integer(quantile(dm$demand, sl))

# Splitting the sample
dm1 <- dm[1:100,]
dm2 <- dm[101:300,] # forecast, calculate forecasting_error
dm3 <- dm[301:500,]

# Make table for summary
res_table <- data.frame(
  method = c("wo prediction",
             "moving average",
             "simple_exp_wo_tuning", "simple_exp_w_tuning",
             "holt_wo_tuning", "holt_w_tuning"),
  AvgProf = rep(0, 6)
)

dm3 %>%
  ggplot(aes(x = demand)) +
  geom_histogram()

#-------------------------#
# Find the optimal quantity using simulation with the full data
# (we can save time by using optimization methods)
#-------------------------#

# Prepare grid
stocking <- c(min(dm$demand):max(dm$demand))

# Calculate
profit <- matrix(0, nrow = nrow(dm), ncol = length(stocking))
for (i in 1:length(stocking)) { # loop for each stocking level
  q <- stocking[i]
  
  for (j in 1:nrow(dm)) { # loop for each realized demand
    profit[j,i] <- price * min(q, dm$demand[j]) + salvage * max(q - dm$demand[j], 0) - cost * q
  }
}
# make summary table
summary_table <- data.frame(
  stocking = stocking,
  AvgProf = apply(profit, 2, mean), # column means
  StdProf = apply(profit, 2, sd) # column sd
)
summary_table %>%
  as_tibble() %>%
  arrange(desc(AvgProf)) %>%
  head(1)

# This is going to be the (approx.) same as the following value
quantile(dm$demand, 0.80)

optimal_profit <- max(summary_table$AvgProf)
optimal_profit

optimal_stocking <- summary_table %>%
  as_tibble() %>%
  arrange(desc(AvgProf)) %>%
  head(1) %>%
  dplyr::select(stocking)
optimal_stocking

# Again, this value is going to be the (approx.) same as the following calculation
mean(price * pmin(optimal_order_quantity, dm$demand) - cost * optimal_order_quantity)

# Value of an oracle
(price - cost) * mean(dm$demand) - optimal_profit

# Safety Stock
safety_stock <- optimal_stocking - mean(dm$demand)
print(safety_stock)

#-------------------------#
# Find the optimal quantity using simulation only for df3
# (we can save time by using optimization methods)
#-------------------------#
# Prepare grid
stocking <- c(min(dm3$demand):max(dm3$demand))
# Calculate
profit <- matrix(0, nrow = nrow(dm3), ncol = length(stocking))
for (i in 1:length(stocking)) { # loop for each stocking level
  q <- stocking[i]
  
  for (j in 1:nrow(dm3)) { # loop for each realized demand
    profit[j,i] <- price * min(q, dm3$demand[j]) + salvage * max(q - dm3$demand[j], 0) - cost * q
  }
}
# make summary table
summary_table <- data.frame(
  stocking = stocking,
  AvgProf = apply(profit, 2, mean), # column means
  StdProf = apply(profit, 2, sd) # column sd
)
summary_table %>%
  as_tibble() %>%
  arrange(desc(AvgProf)) %>%
  head(1)

# This is going to be the (approx.) same as the following value
quantile(dm3$demand, 0.80)

optimal_profit <- max(summary_table$AvgProf)
optimal_profit

optimal_stocking <- summary_table %>%
  as_tibble() %>%
  arrange(desc(AvgProf)) %>%
  head(1) %>%
  dplyr::select(stocking) %>%
  as.numeric()

# Again, this value is going to be the (approx.) same as the following calculation
res_table$AvgProf[1] <- mean(price * pmin(optimal_stocking, dm3$demand) - cost * optimal_stocking)

# Value of an oracle
(price - cost) * mean(dm3$demand) - optimal_profit

# Safety Stock
safety_stock <- optimal_stocking - mean(dm3$demand)
print(safety_stock)

#---------------------------------------------------------------------------#
# Forecasting
#---------------------------------------------------------------------------#
# forecast for 301  --> use moving average, e.g., (D[300]+D[299]+D[298]+D[297])/4
# then, at day 301, what should be the stocking level?
# it would be: forecast + safety stock.
# then, what is the safety stock?
# Need to forecast forecasting errors:
# Forecast forecast error distribution using the forecast error from day 101 to 300
# Now, knowing the forecast error distribution --> Take the 80th percentile of the forecast error distribution.
# then: stocking = forecast + 80th_percentile of forecast_error

#-------------------------#
# method 1. moving average
#-------------------------#
# Forecast demand of df2 using df1
forecast <- c()
for (i in 1:nrow(dm2)) {
  # forecast by the last four days
  last_four_values <- c(dm$demand[(100+i-1)-3], dm$demand[(100+i-1)-2], dm$demand[(100+i-1)-1], dm$demand[(100+i-1)-0])
  forecast[i] <- mean(last_four_values)
}

# calculate stocking values using predicted forecast error in df2
forecasting_error <- dm2$demand - forecast
hist(forecasting_error)

safety_stock <- quantile(forecasting_error, 0.80)
stocking <- forecast + safety_stock

# for the remaining days 301-- 500: repeat the process
# Note that in this case, forecast changes, but safety stock remains the same
# For the third split sample: we would like to calculate the average daily profit, if we use the moving average forecasting method
forecast_301_500 <- c()
for (i in 1:nrow(dm3)) {
  # forecast by the last four days
  last_four_values <- c(dm$demand[(300+i-1)-3], dm$demand[(300+i-1)-2], dm$demand[(300+i-1)-1], dm$demand[(300+i-1)-0])
  forecast_301_500[i] <- mean(last_four_values)
}

stocking_301_500 <- forecast_301_500 + safety_stock # we use safety_stock obtained from df2

# Average profit
res_table$AvgProf[2] <- mean(price * pmin(stocking_301_500, dm3$demand) + salvage * pmax(stocking_301_500 - dm3$demand, 0) - cost * stocking_301_500)

# Oracle
(price - cost) * mean(dm3$demand)

#-------------------------#
# method 2. simple exponential smoothing
# It starts from week 101
#-------------------------#
# start from arbitrary number
alpha = 0.2

forecast_101_300 <- c()
for (i in 1:(300-101+1)) {
  if (i == 1){
    forecast_101_300[1] <- mean(dm1$demand)
  } else {
    # update the forecast with alpha
    forecast_101_300[i] <- alpha * dm$demand[100+i-1] + (1 - alpha) * forecast_101_300[i-1]
  }
}

forecast_error_simple_expo_smooth <- dm2$demand - forecast_101_300
hist(forecast_error_simple_expo_smooth)

safety_stock <- quantile(forecast_error_simple_expo_smooth, 0.80)
stocking <- forecast_101_300 + safety_stock

# for the remaining days 301-- 500: repeat the process
# Note that in this case, forecast changes, but safety stock remains the same
forecast_301_500 <- c()
for (i in 1:(300-101+1)) {
  if (i == 1){
    forecast_301_500[1] <- mean(dm$demand[1:300])
  } else {
    # update the forecast with alpha
    forecast_301_500[i] <- alpha * dm$demand[300+i-1] + (1 - alpha) * forecast_301_500[i-1]
  }
}

stocking_301_500 <- forecast_301_500 + safety_stock # we use safety_stock obtained from df2

# Average profit
res_table$AvgProf[3] <- mean(price * pmin(stocking_301_500, dm3$demand) + salvage * pmax(stocking_301_500 - dm3$demand, 0) - cost * stocking_301_500)

## finding optimal alpha
mse_simple_exponential_smoothing <- function(alpha, actual_demand) {
  forecast_101_300 <- c()
  for (i in 1:(300-101+1)) {
    if (i == 1){
      forecast_101_300[1] <- mean(dm1$demand)
    } else {
      # update the forecast with alpha
      forecast_101_300[i] <- alpha * dm$demand[100+i-1] + (1 - alpha) * forecast_101_300[i-1]
    }
  }
  
  mse <- sum((actual_demand - forecast_101_300)^2) / length(actual_demand)
  return(mse)
}

res <- optim(0.2, mse_simple_exponential_smoothing, method = "L-BFGS-B",
             actual_demand = dm2$demand,
             lower = c(0),
             upper = c(1))
## Note: bisection might be more appropriate here (https://urldefense.com/v3/__https://rpubs.com/aaronsc32/bisection-method-r__;!!Mih3wA!WKo4McW-7-dzrHSo51O5DuRNZsZC4YkYFqeXfDfe2Jlu5bLOewZdY1wOa_2w8g$ )
## There are many ways/solvers to do this, but I just sticked with the very basic.

# use the tuned values
alpha <- res$par

forecast_101_300_with_optim_alpha <- c()
for (i in 1:(300-101+1)) {
  if (i == 1){
    forecast_101_300_with_optim_alpha[1] <- mean(dm1$demand)
  } else {
    # update the forecast with alpha
    forecast_101_300_with_optim_alpha[i] <- alpha * dm$demand[100+i-1] + (1 - alpha) * forecast_101_300_with_optim_alpha[i-1]
  }
}

forecast_error_simple_expo_smooth <- dm2$demand - forecast_101_300_with_optim_alpha
hist(forecast_error_simple_expo_smooth)

safety_stock <- quantile(forecast_error_simple_expo_smooth, 0.80)
stocking <- forecast_101_300_with_optim_alpha + safety_stock

# for the remaining days 301-- 500: repeat the process
# Note that in this case, forecast changes, but safety stock remains the same
forecast_301_500 <- c()
for (i in 1:(500-301+1)) {
  if (i == 1){
    forecast_301_500[1] <- forecast_101_300[200]
  } else {
    # update the forecast with alpha
    forecast_301_500[i] <- alpha * dm$demand[300+i-1] + (1 - alpha) * forecast_301_500[i-1]
  }
}

stocking_301_500 <- forecast_301_500 + safety_stock # we use safety_stock obtained from df2

# Average profit
res_table$AvgProf[4] <- mean(price * pmin(stocking_301_500, dm3$demand) + salvage * pmax(stocking_301_500 - dm3$demand, 0) - cost * stocking_301_500)

# Another Objective Function (Maximizing Average profit to find tuning parameters)
#101_300: Average_profit
## finding optimal alpha
AveProf_simple_exponential_smoothing <- function(alpha, df, price, salvage, cost) {
  forecast_101_300 <- c()
  for (i in 1:(300-101+1)) {
    if (i == 1){
      forecast_101_300[1] <- mean(dm$demand[1:100])
    } else {
      # update the forecast with alpha
      forecast_101_300[i] <- alpha * dm$demand[100+i-1] + (1 - alpha) * forecast_101_300[i-1]
    }
  }
  
  forecast_error_simple_expo_smooth <- dm$demand[101:300] - forecast_101_300
  
  safety_stock <- quantile(forecast_error_simple_expo_smooth, 0.80)
  stocking_101_300 <- forecast_101_300 + safety_stock
  
  # Average profit
  AvgProf <- mean(price * pmin(stocking_101_300, dm$demand[101:300]) + salvage * pmax(stocking_101_300 - dm$demand[101:300], 0) - cost * stocking_101_300)
  
  print(-AvgProf)
  return(-AvgProf) # don't forget to put negative sign as we want to maximize.
}

res <- optim(0.5, AveProf_simple_exponential_smoothing, method = "L-BFGS-B",
             df = df,
             lower = c(0),
             upper = c(1),
             price = price,
             salvage = salvage,
             cost = cost)

res$par
#0.01822693

#-------------------------#
# method 3. Trend-Corrected Exponential Smoothing (Holt’s Model)
#-------------------------#
#Step 1. Use the data from Week1 to Week300 to estimate 
#the slope “a” in time (t) and the intercept “b” from the linear regression model.

#Dt = at + b
model <- lm(dm$demand[1:300] ~ dm$t[1:300])
a <- coefficients(model)[2]
b <- coefficients(model)[1]


#Step 2. Use L0=b and T0=a from the above linear regression. 
#Use alpha=0.2 and beta=0.2. Estimate the forecast from Week1 to Week 500.

#Ft+1 = Lt + Tt
#L2 = aD2 + (1-a)(L1+T1)
#t2 = b(L2 - L1)+(1-b)T1

alpha <- 0.2
beta <- 0.2
level <- b
trend <- a

forecast_1_500 <- c()
for (i in 1:500) {
  forecast_1_500[i] <- trend + level
  # update the forecast with alpha and beta
  level <- alpha * dm$demand[i] + (1 - alpha) * forecast_1_500[i]
  trend <- beta * (level - (forecast_1_500[i] - trend)) + (1 - beta) * trend
}

#Step 3. What is the stocking quantity at Week301?
forecast_error_holt <- dm$demand[101:300] - forecast_1_500[101:300]
hist(forecast_error_holt)
safety_stock <- quantile(forecast_error_holt, 0.80)

stocking_301_500 <- forecast_1_500[301:500] + safety_stock # I use safety_stock obtained from df2

stocking_301_500[1] #stocking quantity at Week301 is 3735.754

#Step 4. What is the weekly average profit from Week301 to Week500? 

res_table$AvgProf[5] <- mean(price * pmin(stocking_301_500, dm3$demand) + salvage * pmax(stocking_301_500 - dm3$demand, 0) - cost * stocking_301_500)

#Do you see an improvement over simple Exponential Smoothing?
#No
#5. Use the data from Week1 to Week300 to find an “optimal” alpha and beta values.

## finding optimal alpha & beta using MSE method
a <- coefficients(model)[2]
b <- coefficients(model)[1]


mse_holt <- function(p, actual_demand) {
  alpha <- p[1]
  beta <- p[2]
  level <- b
  trend <- a
  
  forecast_1_500 <- c()
  for (i in 1:500) {
    forecast_1_500[i] <- trend + level
    # update the forecast with alpha and beta
    level <- alpha * dm$demand[i] + (1 - alpha) * forecast_1_500[i]
    trend <- beta * (level - (forecast_1_500[i] - trend)) + (1 - beta) * trend
  }
  mse <- sum((actual_demand - forecast_1_500[101:300])^2) / length(actual_demand)
  return(mse)
}

res <- optim(c(0.2, 0.2), mse_holt, method = "L-BFGS-B",
             actual_demand = dm$demand[101:300],
             lower = c(0, 0),
             upper = c(10, 10))
print("Optimal alpha and beta using MSE method:")
print(res$par)

# Another Objective Function (Maximizing Average profit to find tuning parameters)
#101_300: Average_profit
## finding optimal alpha and beta
AveProf_holt_smoothing <- function(p, price, salvage, cost) {
  alpha <- p[1]
  #alpha <- 0.025133018
  beta <- p[2]
  level <- b
  trend <- a
  forecast_1_500 <- c()
  for (i in 1:500) {
    forecast_1_500[i] <- trend + level
    # update the forecast with alpha and beta
    level <- alpha * dm$demand[i] + (1 - alpha) * forecast_1_500[i]
    trend <- beta * (level - (forecast_1_500[i] - trend)) + (1 - beta) * trend
  }
  
  #forecast_error_holt_smooth <- dm$demand[101:300] - forecast_1_500[101:300]
  forecast_error_holt_smooth <- dm$demand[1:300] - forecast_1_500[1:300]
  
  safety_stock <- quantile(forecast_error_holt_smooth, 0.80)
  
  stocking_101_300 <- forecast_1_500[101:300] + safety_stock
  # Average profit
  AvgProf <- mean(price * pmin(stocking_101_300, dm$demand[101:300]) + salvage * pmax(stocking_101_300 - dm$demand[101:300], 0) - cost * stocking_101_300)
  
  print(-AvgProf)
  return(-AvgProf) # don't forget to put negative sign as we want to maximize.
}

res <- optim(c(0.5,0.5), AveProf_holt_smoothing, method = "L-BFGS-B",
             lower = c(0,0),
             upper = c(10, 10),
             price = price,
             salvage = salvage,
             cost = cost)
print("Optimal alpha and beta by maximizing average profit:")
res$par


#6.  use the tuned values
alpha <- res$par[1]
beta <- res$par[2]

level <- b
trend <- a

forecast_1_500_with_optim_alpha <- c()
for (i in 1:500) {
  forecast_1_500_with_optim_alpha[i] <- trend + level
  # update the forecast with alpha and beta
  level <- alpha * dm$demand[i] + (1 - alpha) * forecast_1_500_with_optim_alpha[i]
  trend <- beta * (level - (forecast_1_500_with_optim_alpha[i] - trend)) + (1 - beta) * trend
}

forecast_error_holt_smooth <- dm2$demand - forecast_1_500_with_optim_alpha[101:300]
hist(forecast_error_holt_smooth)

safety_stock <- quantile(forecast_error_holt_smooth, 0.80)
stocking_301_500 <- forecast_1_500_with_optim_alpha[301:500] + safety_stock # we use safety_stock obtained from df2


# Average profit
res_table$AvgProf[6] <- mean(price * pmin(stocking_301_500, dm3$demand) + salvage * pmax(stocking_301_500 - dm3$demand, 0) - cost * stocking_301_500)


