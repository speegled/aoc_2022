library(tidyverse)
dd <- read.csv("data/day02", sep = " ", header = F)
dd %>% 
  mutate(V1 = sapply(V1, function(x) which(x == LETTERS)) - 1,
         V2 = sapply(V2, function(x) which(x == LETTERS)) - 24) %>% 
  mutate(win = V2 == (V1 + 1) %% 3,
         tie = V1 == V2) %>% 
  mutate(score = V2 + 1 + win * 6 + tie * 3) %>% 
  summarize(tot = sum(score))

dd %>% 
  mutate(V1 = sapply(V1, function(x) which(x == LETTERS)) - 1,
         val = case_when(
           V2 == "X" ~ (V1 + 2) %% 3 + 1,
           V2 == "Y" ~ V1 + 4,
           V2 == "Z" ~ (V1 + 1) %% 3  + 7
         )) %>% 
  summarize(tot = sum(val))