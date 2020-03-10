library(tidyverse)
library(tibble)
library(dplyr)
f <- "https://raw.githubusercontent.com/difiore/ADA-datasets/master/IMDB-movies.csv"
d <- read_csv(f, col_names = TRUE)
s <- select(d, startYear, runtimeMinutes)
head(s)

x <- filter(s, startYear %in% 1920:1979 & runtimeMinutes < 240)
x
nrow(x)

x2 <- mutate(x, "decade" = case_when(startYear %in% 1920:1929 ~ "20s", 
                                     startYear %in% 1930:1939 ~ "30s",
                                     startYear %in% 1940:1949 ~ "40s",
                                     startYear %in% 1950:1959 ~ "50s",
                                     startYear %in% 1960:1969 ~ "60s",
                                     startYear %in% 1970:1979 ~ "70s"))
x2
nrow(x2)
## need to do ggplot

#Use a one-line statement to calculate the population mean and population standard 
#deviation in runtimeMinutes for each decade and save the results in a new dataframe, results.


mean(runtimeMinutes)
mean(~mean, data = samp_dist_mean) # mean based on mean of sampling distribution

