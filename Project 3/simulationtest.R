## 

install.packages("tidyverse")
install.packages("stats")
library(tidyverse)
library(stats)
library(ggplot2)
install.packages("simsalapar")
library(simsalapar)
install.packages("pastecs")
library(pastecs)
library(splines)
library(MASS)
install.packages("ggpmisc")
library(ggpmisc)

## setting overall df - Only run once at beginning
all_values <- data.frame(matrix(ncol = 0, nrow=140))
all_values$trial <- 1:nrow(all_values)

## set run ## will have to generate new df for each run
run1 <- data.frame(matrix(ncol = 0, nrow = 1)) 
##################### 

###set updating values - make sure these are saved into the workspace 
x <- 0 # set initial 'points' (this was apples in my paradigm)
score <- function(df) {
  sum(df[1,])
}
time <- 0 # set initial start time

stay <- if(x < y) { 
  stay = FALSE
} else {
  stay = TRUE
}

##sets new patch
new_patch <- function(df) {
  new <- data.frame(rnorm(1, mean = 10, sd = 1)) #this sets the inital 'richness' of the patch.
  run1 <- cbind(df, new)
}

##if exploiting patch 
exploit <- function(df, z) {
  deplete <- data.frame(z * rnorm(1, mean = 0.88, sd = .07)) ### this sets the patch depletion rate, which is also drawn from a normal distribution (cf Constantino & Daw, 2015)
  run1 <- cbind(df, deplete) 
}


## foraging for loop
for (y in seq(1,10, by = 0.001)) { # this will conduct 9001 simulations 
  run1 <- data.frame(matrix(ncol = 0, nrow = 1)) 
  x <- 0 
  time <- 0
  while(time < 420) { # this is set to 420 as we had our environments last for 7 minutes.
    stay <- 1 
    if(stay == TRUE) {
      run1 <- new_patch(run1) 
      x <- run1[,ncol(run1)] ##select the last column of the df and use this to update x
      time <- time + 3 #this is when harvest time = 3. 
      stay <- if((x* rnorm(1, mean = 0.88, sd = .07)) < y) { # the depletion rate must match that within the exploit function
        stay = FALSE
        time <- time + 6 # this is the travel time between patches, so was manipulated to be either 6 or 12 seconds
      } else {
        stay = TRUE
      }
      ##
      while(time < 420) { # if you change line 56 this should also be changed
        if(stay == TRUE) {
          run1 <- exploit(run1, x) 
          x <- run1[,ncol(run1)]
          time <- time + 3 # if you change line 61 you should also change this
          stay <- if((x* rnorm(1, mean = 0.88, sd = .07)) < y) { # the depletion rate must match that within the exploit function
            stay = FALSE
            time <- time + 6 # if you change line 64 you should also change this 
          } else {
            stay = TRUE
          }
          ##
        } else {
          break
        }
      }    
    } else {
    } 
    score(run1) ###sums total for each patch 
  }
  colnames(run1) <- seq(1, ncol(run1))
  run1 <- gather(run1, "trial", "runs")
  colnames(run1) <- c("trial", "myval")
  run1$trial <- as.numeric(run1$trial)
  all_values <- full_join(all_values, run1, by="trial" ) 
}

#calculating the values i will need for 
all_values$trial <- NULL
all_values[141,] <- colSums(all_values, na.rm = TRUE)
all_values[142,] <- seq(1, 10, by = 0.001) ## need to always change this to match sequence in line 52

figure <- data.frame(t(all_values[141:142,]))
names(figure) <- c("total_score", "leave_val")
figure$total_score <- as.numeric(figure$total_score)
figure$leave_val <- as.numeric(figure$leave_val)

#calculating the peak of the curve- this will output the optimal leaving threshold for the patch based on the above information
p1 <- ggplot2(figure, aes(x = leave_val, y = total_score)) +
  stat_smooth()
gb <- ggplot2_build(p1)
exact_x_value_of_the_curve_maximum <- gb$data[[1]]$x[which(diff(sign(diff(gb$data[[1]]$y)))==-2)+1]
p1 + geom_vline(xintercept=exact_x_value_of_the_curve_maximum)
exact_x_value_of_the_curve_maximum
