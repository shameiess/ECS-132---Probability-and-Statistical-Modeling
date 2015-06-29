# Kevin Dang Nguyen
# Rena Jing
# ECS 132 - Project 2

#byte <- sample(0:1, 8, replace = T) #to generate random 8 bit vectors

##########################################################################################
# BASIC STEP

# 1) A-collision
lappend <- function (lst, ...){
  lst <- c(lst, list(...))
  return(lst)
}

A_average = 0
A_total = 0
A_trials = 1000

for (t in 1:A_trials){
  
  array = NULL
  counter = 1
  match = 0
  
  #insert first element
  first <- sample(0:1, 8, replace = T)
  array <- lappend(array, first)
  
  repeat{

    byte <- sample(0:1, 8, replace = T)
    array <- lappend(array, byte)
    counter = counter + 1
    
   for (i in 1:(counter-1)){ 
      
      if (all(byte == array[[i]])){
        match = 1
        break
      }
   }
   if (match == 1){
     break}
        
  }
A_total = A_total + counter
}
A_average = A_total/A_trials
A_average # about 20

##########################################################################################
# 2) Assuming 100 trials

#trials <- readline(prompt = "Enter number of trials in S: ")
S_trials = 1000
S_total = 0

for (i in 1:S_trials){

random_array = NULL
counter = 0

S <- sample(0:1, 8, replace = T) #reference vector for S collision

repeat
{
  byte <- sample(0:1, 8, replace = T)
  random_array <- lappend(random_array, byte)
  counter = counter + 1
  if (all(S == byte)){
    break;}
} 
S_total = S_total + counter

}
S_average = S_total/S_trials
S_average # about 260


##########################################################################################
# GIVEN A NUMBER OF RANDOMLY GENERATED 8-BIT IVs, FIND THE PROBABILITY FOR BOTH A/S-COLLISION
A_and_S_collision <- function(N){
  p = numeric(N)
  
  for (n in 1:N){
    q = 1 - (0:(n-1))/(2^8)
    p[n] = 1- prod(q)
  }
  plot(p, main = 'Probabily of A&S collision', xlab = 'Number of random 8-bit IVs', ylab = 'Probability of collision')
  p
}
  
A_and_S_collision(50)
##########################################################################################
# SCALABILITY
S_collision <- function(sizeICV){
  
trials = 100
total = 0

for (i in 1:trials){
  
  random_array = NULL
  counter = 0
  
  S <- sample(0:1, sizeICV, replace = T) #reference vector for S collision
  
  repeat
  {
    byte <- sample(0:1, sizeICV, replace = T)
    random_array <- lappend(random_array, byte)
    counter = counter + 1
    if (all(S == byte)){
      break;}
  } 
  total = total + counter
  
}
S_average = total/trials
S_average 
}

S_collision(10) # about 1226.03
S_collision(12) # about 4178.66
S_collision(14) # about 15750.13
S_collision(16) # about 63542.38

##########################################################################################
# ESTIMATION
S_collision(24) # runs for a very long time

##########################################################################################

counter = 0


S <- sample(0:(2^8), 1, replace = T)
S
for (i in 0:(2^8)){
  if (i == S){
    break
  }
  counter = counter + 1  
}
counter
##########################################################################################
# Practical Scenario
time_8 = 416000000/(2^8) # 1625000
time_8
time_16 = 416000000/(2^16) # 6347.656
time_16

##########################################################################################
# 4 DISCUSSION

1. A possible strategy is alternating between ascending and descending counters, 
which is more efficient than ascending, A-collision, and S-collision. It is 
more efficient than S-collisions and ascending collisions because both require
traversal of the elements from the beginning of the array, whilst ascending
and descending counters can simply estimate where the IV value is centered
around and gauge based on the estimate the byte it is closer to, thus decreasing
the range that we have to search in. 



2. A-collision is the most efficient strategy because it has the shortest run
times out of ascending order and S-collision according to the simulations. Also,
since you are increasing the number of reference points, there is a higher 
probability of a collision as opposed to having just one reference point in
an S-collision. The A-collision strategy is more efficient because at the worst 
case, say if the byte was the larger than all the values in the array for the ascending order strategy,  the ascending-order strategy requires a traversal through the entire
array, which would be inefficient as opposed to having randomly generated 
bytes.