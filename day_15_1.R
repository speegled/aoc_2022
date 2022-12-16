dd <- data.frame(x = readLines("data/day_15"))
dd <- dd %>% 
  separate(x, into = c("a", "sensor_x", "sensor_y", "beacon_x", "beacon_y"), sep = "[^0-9]+", convert = T) %>% 
  select(-a)
dd <- dd %>% 
  mutate(dist = abs(sensor_x - beacon_x) + abs(sensor_y - beacon_y))
dd
vals <- integer(0)

row <- 11
i <- 1
for(i in 1:nrow(dd)) {
  dis <- dd$dist[i]
  tol <- abs(dd$sensor_y[i] - row)
  if(tol > dis) {
    next
  } else {
    vals <- union(vals, union(dd$sensor_x[i]:(dd$sensor_x[i] - tol + dis), dd$sensor_x[i]:(dd$sensor_x[i] + tol - dis)))
    if(14 %in% vals) {
      break
    }
  }
}

setdiff(0:20, vals)

bb <- filter(dd, beacon_y == row) %>% 
  pull(beacon_x) %>% 
  unique()
length(setdiff(vals, bb))



is_subset <- function(x, y) {
  if(min(x) >= min(y) && max(x) <= max(y)) {
    return(TRUE)
  }
  if(min(y) >= min(x) && max(y) <= max(x)) {
    return(TRUE)
  }
  return(FALSE)
}
row <- 0
for(k in 1:10) {
  vals <- list(NULL)
  j <- 1
  for(i in 1:nrow(dd)) {
    dis <- dd$dist[i]
    tol <- abs(dd$sensor_y[i] - row)
    if(tol > dis) {
      next
    } else {
      vals[[j]] <- sort(union(dd$sensor_x[i]:(dd$sensor_x[i] - tol + dis), dd$sensor_x[i]:(dd$sensor_x[i] + tol - dis)))
      j <- j + 1
      #vals <- c(vals, union(dd$sensor_x[i]:(dd$sensor_x[i] - tol + dis), dd$sensor_x[i]:(dd$sensor_x[i] + tol - dis)))
    }
  }
  min <- Inf
  remove <- integer(0)
  for(i in 1:length(vals)) {
    for(j in 1:length(vals)) {
      if(is_subset(vals[[i]], vals[[j]]) && i != j) {
        remove <- c(remove, i)
      }
    }
  }
  vals
  for(i in setdiff(1:length(vals), unique(remove))) {
    for(j in setdiff(1:length(vals), c(unique(remove), i))) {
      a <- length(intersect(vals[[i]], vals[[j]]))
      if(a > 0 && a < Inf) {
        min <- a
      }
    }
  }
  if(min == Inf) {
    vals <- integer(0)
    for(i in 1:nrow(dd)) {
      dis <- dd$dist[i]
      tol <- abs(dd$sensor_y[i] - row)
      if(tol > dis) {
        next
      } else {
        vals <- union(vals, union(dd$sensor_x[i]:(dd$sensor_x[i] - tol + dis), dd$sensor_x[i]:(dd$sensor_x[i] + tol - dis)))
      }
    }
    if(length(setdiff(0:4000000, vals)) > 0) {
      break
    }
    row <- row + 1
  } else {
    change <- max(floor(min / 2), 1)
    row <- row + change
  }
}
row
