library(tidyverse)
dd <- data.frame(x = readLines("data/day_15_2"))
dd <- dd %>% 
  separate(x, into = c("a", "sensor_x", "sensor_y", "beacon_x", "beacon_y"), sep = "[^0-9]+", convert = T) %>% 
  select(-a)
sensors <- dd[,1:2] 
#  summarize(locs = paste(sensor_x, sensor_y))
beacons <- dd[,3:4] 
#  summarize(locs = paste(beacon_x, beacon_y))
y <- 200000
dist <- function(a, b, c = NULL, d = NULL) {
  # if(is.null(c)) {
  #   return(abs(a - c) + abs(b - d))
  # }
  as.numeric(abs(a[1] - b[1]) + abs(a[2] - b[2]))
}
n <- nrow(dd)
mindistances <- as.numeric(sapply(1:n, function(j) {dist(sensors[j, 1:2], beacons[j, 1:2])}))
vector_beacons <- paste(beacons$beacon_x, beacons$beacon_y)
vector_beacons
isnt_possible <- function(x, y) {
  # if(paste(x, y) %in% vector_beacons) {
  #   return(FALSE)
  # }
  for(i in 1:n) {
    if(dist(sensors[i, 1:2], c(x, y)) <= mindistances[i]) {
      return(TRUE)
    }
  }
  return(FALSE)
}
isnt_possible <- Vectorize(isnt_possible, vectorize.args = "y")

get_distance <- function(x, y, debug = F) {
  if(debug) {
    browser()
  }
  vals <- integer(0)
  tot <- 0
  slack <- sapply(1:n, function(i) {mindistances[i] - dist(sensors[i, 1:2], c(x, y))})
  max_slack <- max(slack[slack > 0])
  y_og <- y
  max_slack_og <- max_slack
  while(max_slack > 0) {
    vals <- union(vals, y:(y + max_slack))
    tot <- tot + max_slack + 1
    y <- y + max_slack
    slack <- sapply(1:n, function(i) {mindistances[i] - dist(sensors[i, 1:2], c(x, y))})
    max_slack <- max(slack[slack > 0])
  }
  max_slack <- max_slack_og
  y <- y_og
  while(max_slack > 0) {
    vals <- union(vals, y:(y - max_slack))
    tot <- tot + max_slack + 1
    y <- y - max_slack
    slack <- sapply(1:n, function(i) {mindistances[i] - dist(sensors[i, 1:2], c(x, y))})
    max_slack <- max(slack[slack > 0])
  }
  return(tot)
}
get_distance(10, 12, T)



slack <- sapply(1:n, function(i) {mindistances[i] - dist(sensors[i, 1:2], c(x, y))})
max_slack <- max(slack[slack > 0])
sapply(1:n, function(i) {mindistances[i] - dist(sensors[i, 1:2], c(x, y - max_slack))})
sapply(1:n, function(i) {mindistances[i] - dist(sensors[i, 1:2], c(x, y + max_slack))})
