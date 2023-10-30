######################################################
# SIMPLE MARKOV MODELS (HOMOGENEIOUS)                #
######################################################
# 1. run external R: source(MyFunction_package.R)    #
# 2. set working directory                           #
#    clear global environment                        #
# 3. install and library(installr)                   #
######################################################
#1
source("C:/Users/user/OneDrive - 國立陽明交通大學/DELL R to NYCU One Drive/R 講義/MyRfunction/MyRFunction_package.R") 

#2. 
wd <- c("C:/Users/user/OneDrive - 國立陽明交通大學/DELL R to NYCU One Drive/R 講義/HEEMOD") 
set_environment(wd)

#3
packages <- c("dplyr", "ggplot2", "tidyr","heemod","diagram","shape")
install_and_load_packages(packages)

####################################################
# HIV example simple version
## Step 1: define transition for different strategy
####################################################
mat_mono <- define_transition(
  #state name vector
  state_names = c("A","B","C", "D"),
  
  C,     0.202, 0.067, 0.010,
  0,     C,     0.407, 0.012,
  0,     0,     C,     0.250,
  0,     0,     0,     1   
)
mat_mono
print(class(mat_mono))
print(c(mat_mono[1],mat_mono[5], mat_mono[16]))

#IVY's practice
mat_mono_matrix <- matrix(mat_mono)
print(class(mat_mono_matrix))

#parameter
rr <- 0.509


mat_comb <- define_transition(
  #state name vector
  state_names = c("A","B","C", "D"),
  
  C,      0.202*rr,   0.067*rr,   0.010*rr,
  0,      C,          0.407*rr,   0.012*rr,
  0,      0,          C,          0.250*rr,
  0,      0,          0,          1   
)
plot(mat_mono)
plot(mat_comb)


####################################################
## Step 2: define values for each state 
##
####################################################
#parameter: cost_drug
#NOTE: ivy thinks two medication costs can be placed in define_parameters
cost_zido <- 2278
cost_lami <- 2086.5

state_A <- define_state(
  cost_health = discount(2756,0.06),
  cost_drug   = discount(dispatch_strategy(
    mono = cost_zido,
    comb = cost_zido + cost_lami
    ), 0.06),
  cost_total  = cost_health + cost_drug,
  life_year   = 1
)
state_A

state_B <- define_state(
  cost_health = discount(3052,0.06),
  cost_drug   = discount(dispatch_strategy(
    mono = cost_zido,
    comb = cost_zido + cost_lami
  ), 0.06),
  cost_total  = cost_health + cost_drug,
  life_year   = 1
)
state_B


state_C <- define_state(
  cost_health = discount(9007,0.06),
  cost_drug   = discount(dispatch_strategy(
    mono = cost_zido,
    comb = cost_zido + cost_lami
  ), 0.06),
  cost_total  = cost_health + cost_drug,
  life_year   = 1
)
state_C

state_D <- define_state(
  cost_health = 0,
  cost_drug = 0,
  cost_total = discount(cost_health + cost_drug, .06),
  life_year   = 0
)
state_D

####################################################
## Step 4: define strategy with matrix and states
####################################################
strat_mono <- define_strategy(
  transition = mat_mono,
  A = state_A,
  B = state_B,
  C = state_C,
  D = state_D
)
strat_comb <- define_strategy(
  transition = mat_comb,
  A = state_A,
  B = state_B,
  C = state_C,
  D = state_D
) 
####################################################
## Step 5: run_model and summary
##         run_model
####################################################
res_mod <- run_model(
  mono = strat_mono,
  comb = strat_comb,
  cycles = 50,
  cost = cost_total,
  effect = life_year
)

res_mod
object_summary <- summary(res_mod, threshold = c(1000,5000,6000,10000)) 

#Ivy's practice:
#res_mod$eval_strategy_list$mono$counts
#object_summary <-summary(res_mod, threshold = c(1000,5000,6000,10000)) 
#print(object_summary$res_values$cost_health)

plot(res_mod, type = "counts", panel = "by_strategy") +
  xlab("Time") +
  theme_bw() +
  scale_color_brewer(
    name = "State",
    palette = "Set1"
  )

plot(res_mod, type = "counts", panel = "by_state") +
  xlab("Time") +
  theme_bw() +
  scale_color_brewer(
    name = "Strategy",
    palette = "Set1"
  )


plot(res_mod, type = "values", panel = "by_value",
     free_y = TRUE) +
  xlab("Time") +
  theme_bw() +
  scale_color_brewer(
    name = "Strategy",
    palette = "Set1"
  )


View(res_mod$eval_strategy_list$mono$value)