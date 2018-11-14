library(microbenchmark)

## Problem 1 from Activity 9
odd_count = function(x) {
  odd_num = 0
  for (i in 1:length(x)) {
    if (x[i] %% 2 == 1) odd_num = odd_num + 1
  }
  return(odd_num)
}

odd_count1= function(x) {
  return(sum(x %% 2))
}

x = seq(1, 10000, by = 1)

microbenchmark(odd_count(x), odd_count1(x))

## Modify the sorting function (sort_vec) from "Assignment 8" (problem 3) so that it should 
## take an additional argument ascending which causes sorting in increasing order when 
## 'ascending = TRUE'.

sort_vec = function(x, ascending = TRUE) {
  if (length(x) < 2) return (x)
    if (ascending == TRUE) {
    for(last in length(x):2) {
      for(first in 1:(last - 1)) {
        if(x[first] > x[first + 1]) {
          temp = x[first]
          x[first] = x[first + 1]
          x[first + 1] = temp
        }
      }
    }
  }
  else {
    for(last in length(x):2) {
      for(first in 1:(last - 1)) {
        if(x[first] < x[first + 1]) {
          temp = x[first]
          x[first] = x[first + 1]
          x[first + 1] = temp
        }
      }
    }
  }
  return(x)
}

## Consider a simple random walk with starting point 0 and a step -1 or 1. Below is the 
## code with dynamically allocated memory. Write your code with preallocated memory and 
## compare time for both versions using system.time() function 
## (use N = 1000, 10000 and 1000000).

N1 = 1000000

## dynamically allocated memory
data_series = 0
system.time({for (i in 2:N1){
  data_series[i] = data_series[i-1] + sample(c(-1, 1), 1)
}
})

## preallocated memory memory
data_series1 = rep(NA, N1)
system.time({for (i in 2:N1){
  data_series[i] = data_series[i-1] + sample(c(-1, 1), 1)
}
})

N = c("1000", "10000", "1000000")
dy = c("0.02", "0.09", "5.94")
pre = c("0", "0.07", "5.52")

result = rbind(N, dy, pre)
noquote(result)
