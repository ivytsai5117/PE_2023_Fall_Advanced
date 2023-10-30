######################################################
# HEEMOD:Time-varying values                         #
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
#  1. TWO time variables in HEEMOD 
#     (1) model_time: duration through the model
#     (2) state_time: duration in a given state
#  2. Apply in THREE functions
#     (1) define_parameters
#     (2) define_state
#     (3) define_transition
####################################################
#2.(1)define parameters
param <- define_parameters(
  lambda = 0.9,                          #Ivy adds         
      mr = exp(-state_time * lambda),    #Ivy: what is it?
     age = 50 + model_time)              #age increases with cycles

#2.(2)define state
state_A <- define_state(
  cost_health = 100 - state_time,
  effect      = 10)

#2.(3)define_transition
f <- function(x) abs(sin(x))

mat_mono <- define_transition(
  C,    f(state_time),
  0.1,  0.9
) 

#supplementary
run_model(
                 I = strat_1,
                II = strat_2,
            cycles = 100,
  state_time_limit = list(
                     I = c(B = 10, D = 20),
                    II = c(B = 15)
  )
)