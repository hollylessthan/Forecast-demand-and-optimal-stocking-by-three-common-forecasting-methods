# Demand-Forecasting-by-Three-Forecasting-Methods

This is a course project from the Supply Chain Analytics course at UCSD, and the data set is obtained from the course as well.
The goal of this project is to use common forecasting methods to find optimal stocking level and optimal profit.

#### Language

* R version 4.0.3 (2020-10-10)

#### Context

Method used in this project:

1. Simulation (without prediction)
2. Moving average
3. Simple exponential smoothing (without tuning)
4. Simple exponential smoothing (tuning by mse/ avg. profit)
5. Holt's model (without tuning)
6. Holt's model (tuning by mse/ avg. profit)

#### Result

In the six methods, I found the simple exponential smoothing method tuning by mse/ avg.profit had the best performance. This result does not mean the Holt's model is not effective, but means that the egg demand data I used in this project does not contain significent time trend correlation.
