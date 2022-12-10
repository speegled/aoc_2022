library(tidyverse)
dd <- data.frame(x = readLines("data/day_10"))
dd <- dd %>%
  separate_rows(x, sep = " ") %>% 
  mutate(cycle = 1:(n()), change = ifelse(str_detect(x, "[0-9]"), as.integer(x), 0)) 
dd %>% 
  mutate(val = 1 + cumsum(c(0, change[-n()]))) %>% 
  filter(cycle %in% c(20, 60, 100, 140, 180, 220)) %>% 
  summarize(ans = sum(cycle * val))

