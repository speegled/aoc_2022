library(tidyverse)

dd <- data.frame(x = readLines("data/day_05_2"))
aa <- dd %>% 
  filter(!str_detect(x, "move"))

aa <- matrix(str_split_fixed(aa[1:9,], patt = "", n = 35), nrow = 9)
aa <- apply(aa, 2, rev)[-(1),]
aa <- aa[,seq(2, 34, by = 4)]
aa <- apply(aa, 2, function(x) paste(x, collapse = "")) %>% 
  str_remove_all(" ")

dd <- dd %>% 
  filter(str_detect(x, "move")) %>% 
  separate(x, into = letters[1:6], convert = T) %>% 
  select(b, d, f) 

for(i in 1:nrow(dd)) {
  n <- dd$b[i]
  from <- dd$d[i]
  to <- dd$f[i]
  move_string <- str_sub(aa[from], start = str_length(aa[from]) - n + 1)
  aa[from] <- str_remove(aa[from], pattern = paste0(move_string, "$"))
  #'
  #' move_string <- stringi::stri_reverse(move_string)
  #'
  #' uncomment for first star
  #'
  aa[to] <- paste0(aa[to], move_string)

}  

paste0(sapply(aa, function(x) {
  str_sub(x, start = -1)
}), collapse = "")
