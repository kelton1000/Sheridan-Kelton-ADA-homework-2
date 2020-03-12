#Challenge 1
library(tidyverse)
library(tibble)
library(dplyr)
library(ggplot2)
library(radiant)
library(mosaic)
f <- "https://raw.githubusercontent.com/difiore/ADA-datasets/master/IMDB-movies.csv"
d <- read_csv(f, col_names = TRUE)
s <- select(d, startYear, runtimeMinutes)
head(s)

x <- filter(s, startYear %in% 1920:1979 & runtimeMinutes < 240) %>%
  mutate("decade" = case_when(startYear %in% 1920:1929 ~ "20s", 
                                     startYear %in% 1930:1939 ~ "30s",
                                     startYear %in% 1940:1949 ~ "40s",
                                     startYear %in% 1950:1959 ~ "50s",
                                     startYear %in% 1960:1969 ~ "60s",
                                     startYear %in% 1970:1979 ~ "70s"))
View(x)
nrow(x)
## need to do ggplot

#Use a one-line statement to calculate the population mean and population standard 
#deviation in runtimeMinutes for each decade and save the results in a new dataframe, results.


mean(runtimeMinutes)
mean(~mean, data = samp_dist_mean) # mean based on mean of sampling distribution
p <- ggplot(x, aes(x = runtimeMinutes)) + geom_histogram()
p
p <- p + facet_wrap(~decade, ncol = 3)
p

x <- group_by(x, decade)
x
results <- summarize(x, mean = mean(runtimeMinutes), standardDev = sdpop(runtimeMinutes))
results
#Draw a single sample of 100 movies from each decade, calculate this single sample 
#mean and sample standard deviation in runtimeMinutes, and estimate the SE around 
#the population mean runtimeMinutes for each decade based on the standard deviation 
#and sample size from these samples.
n <- 100
m <- sample_n(x, size = n, replace = FALSE)
m
mean(x)
#Compare these estimates to the actual population mean runtimeMinutes and to 
#the calculated SE in the population mean for samples of size 100 based on the 
#population standard deviation for each decade.
sample_se <- sample_sd / sqrt(n) # a vector of SEs estimated from each sample
pop_se <- sigma / (sqrt(n)) # a single value estimated from the population SD
sampling_dist_se <- sd(m) # a single value calculated from our sampling distibution SD





#Challenge 2

#What is the probability that she will hear 13 or fewer calls during any given session?
ppois(13, lambda = 18)
#What is the probability that she will hear no calls in a session?
dpois(0, lambda = 18)
#What is the probability that she will hear exactly 7 calls in a session?
dpois(7, lambda= 18)
#What is the probability that she will hear more than 20 calls in a session?
ppois(20, lambda = 18, lower.tail = FALSE)
#Plot the relevant Poisson mass function over the values in range 0 ≤ x < 40
dpois(0:40,lambda = 18)

