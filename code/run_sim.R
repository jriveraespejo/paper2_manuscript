# preliminar ####

## working environment ####
rm(list=ls())

## package requirements ####
librerias = c('rethinking','rstan','coda','cmdstanr','posterior','bayesplot')
sapply(librerias, require, character.only=T)
# sapply(librerias, install.packages, character.only=T)


## loading functions ####
dir = '/home/josema/Desktop/1. Work/1 research/PhD Antwerp/#thesis/paper2/paper2_manuscript'
source( file.path(dir,'code','user-defined functions.R'))



# RQ1 ####
# 
## Design ####
#
# population simulation 
#   Interest:
#   Proportion of variability among group sub-units ( eri_s, balanced)
#   
#   NO interest:
#   Size of the effect on the units' trait ( bX$bXc )
#   Size of the trait difference between group of units ( bX$bXd )
#   Size of the effect on the units' trait ( bY$bYc )
#   Size of the trait difference between group of sub-units ( bY$bYd )
#   Size of the effect on the judges' trait ( log_bZ$bZc )
#   Size of the trait difference between group of judges ( log_bZ$bZd, fixed ) 
#   Proportion of variability among group units ( ei_s )
#   Proportion of variability among group judges ( log_ek_s )
# 
# 
# sample mechanism
#   Interest:
#   Number of sub-units ( nR )
#   Number of judges ( nK )
#   Proximity’ of the trait between units ( Ti−Tj = req_diff )
# 
#   NO interest:
#   Number of units ( nI )
#   Proportion of units per group ( pI )
#   Proportion of sub-units per group ( pR )
#   Proportion of judges per group ( pK )
#   Proximity’ of the trait between units ( Gi−Gj = req_diff )
# 
# comparison mechanism
#   Interest:
#   Number of comparisons per sub-units ( ncomp )
# 
#   NO interest:
#   -


## parameters ####

# population simulation
eir_s1 = c(1-0.3, 1-0.15) # as proportion of ei_s 

# sample mechanism
nR = c(10,20,40)
nK = c(50,100,200)
req_diff = c(0.2,0.5)

# comparison mechanism
ncomp = c(20,40,100)

# full parameter list
params = expand.grid( eir_s1, nR, nK, req_diff, ncomp)
params = cbind(params$Var1, params)
names(params) = c('eir_s1','eir_s2','nR','nK','req_diff','ncomp')
# str(params)



## simulation ####
sim_RQ1_hmc(
  nsim = 1000,
  p = 0.95,
  params,
  seed_pop = NULL,
  seed_sample = NULL,
  save_dir = file.path(dir, '_results'),
  mod_dir = file.path(dir, 'code')
)

# # running times
# (130*1000/60/60/24)*nrow(params) # warmup=2000, sampling=2000
# (100*1000/60/60/24)*nrow(params) # warmup=2000, sampling=1000
# (85*1000/60/60/24)*nrow(params) # warmup=1000, sampling=1000
# (50*1000/60/60/24)*nrow(params) # warmup=1000, sampling=1000, without adapt_delta=0.95
# (40*1000/60/60/24)*nrow(params) # draws=4000, pathfinder
# (5*1000/60/60/24)*nrow(params) # draws=4000, variational

# readRDS( file.path(dir, '_results', 'sim_RQ1.rds') )





# RQ2 ####
# 
## Design ####
#
# population simulation 
#   Interest:
#   Size of the effect on the units' trait ( bX$bXc )
#   Size of the trait difference between group of units ( bX$bXd )
#   Size of the effect on the units' trait ( bY$bYc )
#   Size of the trait difference between group of sub-units ( bY$bYd )
#   Proportion of variability among group units ( ei_s )
#   Proportion of variability among group sub-units ( eri_s )
#   
#   NO interest:
#   Size of the effect on the judges' trait ( log_bZ$bZc )
#   Size of the trait difference between group of judges ( log_bZ$bZd, fixed ) 
#   Proportion of variability among group judges ( log_ek_s)
# 
# 
# sample mechanism
#   Interest:
#   Number of sub-units ( nR )
#   Number of units ( nI )
#   Proportion of units per group ( pI )
#   Proportion of sub-units per group ( pR )
# 
#   NO interest:
#   Number of judges ( nK )
#   Proportion of judges per group ( pK )
#   Proximity’ of the trait between units ( Ti−Tj = req_diff )
#   Proximity’ of the trait between units ( Gi−Gj = req_diff )
# 
# comparison mechanism
#   Interest:
#   Number of comparisons per sub-units ( ncomp, fix at maximum )
# 
#   NO interest:
#   -


## parameters ####





## simulation ####
sim_RQ2_hmc(
  nsim = 1000,
  p = 0.95,
  params,
  seed_pop = NULL,
  seed_sample = NULL,
  save_dir = file.path(dir, '_results'),
  mod_dir = file.path(dir, 'code')
)

# readRDS( file.path(dir, '_results', 'sim_RQ2.rds') )





# RQ3 ####
# 
## Design ####
#
# population simulation 
#   Interest:
#   Size of the effect on the judges' trait ( log_bZ$bZc )
#   Size of the trait difference between group of judges ( log_bZ$bZd) 
#   Proportion of variability among group judges ( log_ek_s )
#   
#   NO interest:
#   Size of the effect on the units' trait ( bX$bXc )
#   Size of the trait difference between group of units ( bX$bXd )
#   Size of the effect on the units' trait ( bY$bYc )
#   Size of the trait difference between group of sub-units ( bY$bYd )
#   Proportion of variability among group units ( ei_s )
#   Proportion of variability among group sub-units ( eri_s )
# 
# 
# sample mechanism
#   Interest:
#   Number of judges ( nK )
#   Proportion of judges per group ( pK )
# 
#   NO interest:
#   Number of sub-units ( nR )
#   Number of units ( nI )
#   Proportion of units per group ( pI )
#   Proportion of sub-units per group ( pR )
#   Proximity’ of the trait between units ( Ti−Tj = req_diff )
#   Proximity’ of the trait between units ( Gi−Gj = req_diff )
# 
# comparison mechanism
#   Interest:
#   Number of comparisons per sub-units ( ncomp, fixed at maximum )
# 
#   NO interest:
#   -


## parameters ####





## simulation ####
sim_RQ3_hmc(
  nsim = 1000,
  p = 0.95,
  params,
  seed_pop = NULL,
  seed_sample = NULL,
  save_dir = file.path(dir, '_results'),
  mod_dir = file.path(dir, 'code')
)

# readRDS( file.path(dir, '_results', 'sim_RQ2.rds') )