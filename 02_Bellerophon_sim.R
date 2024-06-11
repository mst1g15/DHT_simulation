###############################################################################
# Date: 10 July 2024 
# Author: Mia Tackney 
###############################################################################

#Purpose: this script demonstrates how simulations are run - note that the actual 
# simulation in the manuscript uses 10,000 replications and is run on a High-Performance Computer 


#---------------------------------------------------------------------------
source("00_init.R")
source("01_simulation_functions.R")
library(ggpubr)

#treatment has 30 patients, control has 14 patients 

#Baseline MVPA is 77 (52) for treatment 
#                 70 (42) for control 

#suppose we assume our baseline is log-normal with mean 77 and sd 52
# then, the logmean of the distribution is log(77^2/(77^2+52^2)) and the logsd is log((1+52^2)/77^2)
# meanlog=4.155, sdlog=0.6129

curve(dlnorm(x, meanlog=4.155, sdlog =0.6129), from=0, to=300)

#number of repetitions
N=1000

#Results with no effect of season nor observer effect---------------------------------
results_vanilla <- sim_obtain_results(rep=1000, "vanilla")
results_vanilla$season_effect=0
results_vanilla$season_prop=0
results_vanilla$season_prop <- factor(results_vanilla$season_prop, levels=c(0, 0.05, 0.1, 0.2))
saveRDS(results_vanilla, "output/results_vanilla.RDS")
results_vanilla <- readRDS("output/results_vanilla.RDS")

#Results with varying seasonal effect and proportion of individuals who experience 
#seasonal effect---------------------------------------------------------------------------

results_season <- c()
for (i in seq(0,20, 0.2)){
  
  results1 <- sim_obtain_results(rep=N, i, season_prop=0.1, season_effect=i)
  results1$season_effect <- i
  results1$season_prop=0.1
  
  
  results2 <- sim_obtain_results(rep=N, i, season_prop=0.2, season_effect=i)
  results2$season_effect <- i
  results2$season_prop=0.2
  
  results3 <- sim_obtain_results(rep=N, i, season_prop=0.5, season_effect=i)
  results3$season_effect <- i
  results3$season_prop=0.5
  
  results_season <- rbind(results_season, results1, results2, results3)
  
}

results_season$interact=FALSE

results_season_interact <- c()
for (i in seq(0,20, 0.2)){
  
  results1 <- sim_obtain_results(rep=N, i, season_prop=0.1, season_effect=i, interact=i*0.1)
  results1$season_effect <- i
  results1$season_prop=0.1
  
  
  results2 <- sim_obtain_results(rep=N, i, season_prop=0.2, season_effect=i, interact=i*0.1)
  results2$season_effect <- i
  results2$season_prop=0.2
  
  results3 <- sim_obtain_results(rep=N, i, season_prop=0.5, season_effect=i, interact=i*0.1)
  results3$season_effect <- i
  results3$season_prop=0.5
  
  results_season_interact <- rbind(results_season_interact, results1, results2, results3)
  
}

results_season_interact$interact=TRUE
results_season <- rbind(results_season, results_season_interact)

results_season$season_prop <- factor(results_season$season_prop, levels=c(0, 0.2, 0.1, 0.5))
saveRDS(results_season, "output/results_season.RDS")


#Results with varying observer effect----------------------------------------------------

results_observer<- c()

for (i in seq(0,10, 0.25)){
  
  results1 <- sim_obtain_results(rep=N, i, observer_effect=i, weekno=4)
  results1$observer_effect <- i
  results1$interact=FALSE
  
  results_interact<- sim_obtain_results(rep=N, i, observer_effect=i, interact=i*0.1, weekno=4)
  results_interact$observer_effect <- i
  results_interact$interact=TRUE
  
  results_observer <- rbind(results_observer, results1, results_interact)
}


saveRDS(results_observer, "output/results_observer.RDS")


results_observer<- c()

for (i in seq(0,10,0.25)){
  
  results1 <- sim_obtain_results(rep=N, i, observer_effect=i, weekno=2)
  results1$observer_effect <- i
  results1$interact=FALSE
  
  results_interact<- sim_obtain_results(rep=N, i, observer_effect=i, interact=i*0.1, weekno=2)
  results_interact$observer_effect <- i
  results_interact$interact=TRUE
  
  results_observer <- rbind(results_observer, results1, results_interact)
  
  
  results_observer <- rbind(results_observer, results1)
}

saveRDS(results_observer, "output/results_observer_2wks.RDS")



#Results when data are missing --------------------------------------------------

results_mis<- c()
for (i in seq(1:14)){
  
  results1 <- sim_obtain_results(rep=1000, i, miss_type="MCAR", miss_prop_n=0.1, miss_prop_days=i/28)
  results1$miss_prop_days <- i/28
  results1$miss_prop_n=0.1
  results1$miss_type="MCAR"
  
  results2 <- sim_obtain_results(rep=1000, i, miss_type="MCAR", miss_prop_n=0.2, miss_prop_days=i/28)
  results2$miss_prop_days <- i/28
  results2$miss_prop_n=0.2
  results2$miss_type="MCAR"
  
  
  results3 <- sim_obtain_results(rep=1000, i, miss_type="MCAR", miss_prop_n=0.5, miss_prop_days=i/28)
  results3$miss_prop_days <- i/28
  results3$miss_prop_n=0.5
  results3$miss_type="MCAR"
  
  
  results4 <- sim_obtain_results(rep=1000, i, miss_type="MNAR", miss_prop_n=0.1, miss_prop_days=i/28)
  results4$miss_prop_days <- i/28
  results4$miss_prop_n=0.1
  results4$miss_type="MNAR"
  
  
  results5 <- sim_obtain_results(rep=1000, i, miss_type="MNAR", miss_prop_n=0.2, miss_prop_days=i/28)
  results5$miss_prop_days <- i/28
  results5$miss_prop_n=0.2
  results5$miss_type="MNAR"
  
  
  
  results6 <- sim_obtain_results(rep=1000, i, miss_type="MNAR", miss_prop_n=0.5, miss_prop_days=i/28)
  results6$miss_prop_days <- i/28
  results6$miss_prop_n=0.5
  results6$miss_type="MNAR"
  
  
  
  results_mis <- rbind(results_mis, results1, results2, results3, results4, results5, results6)
}

saveRDS(results_mis, "output/results_mis.RDS")

