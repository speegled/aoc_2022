dd <- data.frame(x = readLines("data/day_03"))
library(tidyverse)

dd %>% 
  mutate(len = str_length(x),
         v1 = str_sub(x, end = len/2),
         v2 = str_sub(x, start = len/2 + 1),
         common = sapply(1:n(), function(x) {
           intersect(str_split(dd$v1[x], pattern = "")[[1]], str_split(dd$v2[x], pattern = "")[[1]])
         })) %>% 
  mutate(val = sapply(1:n(), function(x) {
    which(common[x] == c(letters, LETTERS))
  })) %>% 
  summarize(tot = sum(val))

dd %>% 
  mutate(group = rep(1:100, each = 3)) %>% 
  group_by(group) %>% 
  summarize(item = intersect(intersect(str_split(first(x), pattern = "")[[1]],
                             str_split(nth(x, 2), pattern = "")[[1]]),
            str_split(last(x), pattern = "")[[1]])) %>% 
  mutate(val = sapply(1:n(), function(x) {
    which(item[x] == c(letters, LETTERS))
  })) %>% 
  summarize(tot = sum(val))
