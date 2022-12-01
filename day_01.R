library(tidyverse)
data.frame(x = readLines("data/day_01")) %>% 
  mutate(y = cumsum(x == "")) %>% 
  group_by(y) %>% 
  summarize(tot = sum(as.integer(x), na.rm = T)) %>% 
  slice_max(tot)
  

data.frame(x = readLines("data/day_01")) %>% 
  mutate(y = cumsum(x == "")) %>% 
  group_by(y) %>% 
  summarize(tot = sum(as.integer(x), na.rm = T)) %>% 
  slice_max(tot, n = 3) %>% 
  summarize(sum(tot))
