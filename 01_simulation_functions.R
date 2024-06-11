###############################################################################
# Date: 10 July 2024 
# Author: Mia Tackney 
###############################################################################

#Purpose: this script contains functions required to run the simulation 


generate_daily_data <- function(season_prop=NULL, weekno=4, season_effect=NULL, observer_effect=NULL, 
                                miss_type=NULL, miss_prop_n=NULL, miss_prop_days=NULL){
  #Purpose: Generate daily time spent in MVPA for a setting inspired by the Bellerophon Phase II trial on INOPulse 
  #Inputs:  season_prop: fraction of participants who experience a seasonal effect 
  #         weekno: number of weeks for follow-up
  #         season_effect: scalar for the effect of season 
  #         observer effect: scalar for the effect of observer 
  #         miss_type: set either to "MCAR" (Missing Completely at Random) or "MNAR" (Missing Not at Random)
  #         miss_prop_n: fraction of participants who have missing data
  #         miss_prop_days: fraction of days in the measurement period which are non-compliant 
  #Outputs: dataset for one run of simulation with daily MVPA 
  
  
  #generate ID: 44 patients. Assume 28 days (four weeks) of data per patient 
  dayno=7*weekno
  ID <- rep(1:44, each=dayno)
  
  #assume 30 patients in treatment and 14 in control 
  treat <- c(rep(1, 30), rep(0, 14))
  
  # based on the distribution of MVPA at baseline in Table 1 of paper 
  baseline <- rlnorm(44, 4.155, 0.6129)
  
  #week number 
  week <- as.factor(rep(rep(1:weekno, each=7), 44))
  
  #vanilla version of the outcome: 
  mu <- rep(baseline+12.5*treat, each=dayno) + rep(dnorm(44,0, 2), each=dayno) 
  
  #add effect of season 
  if(!is.null(season_prop) & !is.null(season_effect)){
    winter_no = round(season_prop*44)
    sprsum_no = 44-winter_no
    season = sample(c(rep(1, winter_no*dayno), rep(0, sprsum_no*dayno)))
    mu <- mu+season_effect*season 
  }
  
  #add effect of observer  
  if(!is.null(observer_effect)){
    mu <- mu + observer_effect*(week==1) 
  }
  
  params <- calc_lognorm_param(mu, 46)
  daily_outcome <- rlnorm(n=dayno*44, params[[1]], params[[2]])
  
  #generate complete data
  dat<- tibble( ID=ID, daily_outcome=daily_outcome, 
                treat=c(rep(1, 30*dayno), rep(0, 14*dayno)), 
                baseline=rep(baseline, each=dayno),
                week=week, day=rep(1:dayno, 44)) %>% group_by(ID) 
  
  #induce missingness 
  if(!is.null(miss_type) & !is.null(miss_prop_n) & !is.null(miss_prop_days)){
    
    if(miss_type=="MCAR"){
      
      num_ind <- round(miss_prop_n*44)
      num_days <- round(miss_prop_days*dayno)
      
      select_id <- sample(1:44, num_ind)
      
      for(i in select_id){
        select_days <- sample(1:dayno, num_days)
        dat$daily_outcome[dat$ID==i & dat$day %in% select_days] <- NA
      }
    }
    
    if(miss_type=="MNAR"){
      
      num_ind <- round(miss_prop_n*44)
      num_days <- round(miss_prop_days*dayno)
      
      select_id <- sample(1:44, num_ind)
      
      dat <- dat %>% group_by(ID) %>% mutate(rank=rank(daily_outcome))
      dat$daily_outcome[dat$rank %in% 1:num_days & dat$ID %in% select_id] <- NA
    }
    
  }
  
  return(dat)
}



sim_obtain_results <- function(rep=100, scenario_name, ...){
  #Purpose: run repetitions under a simulation scenario and save results 
  #Inputs:  reps: number of repetitions 
  #         scenario_name: character for scenario name 
  #         additional inputs: include inputs to generate_daily_data
  #Outputs: mean and standard error of the treatment effect of the primary analysis,  
  #         and their Monte Carlo Errors 
   

  all_results <- c()
  
  for (i in 1:rep){
    
    #generate daily data under the given scenario 
    dat <- generate_daily_data(...)
    
    #aggregate data for analysis 
    summary <- dat %>% summarize(month_average=mean(daily_outcome, na.rm=TRUE), treat=mean(treat), baseline=mean(baseline))
    
    #primary analysis model 
    model <- lm(month_average~baseline+treat, data=summary)
    
    result <- summary(model)
    
    #obtain results 
    tmt_est <- tibble(est=result$coefficients[3,1], estse=result$coefficients[3,2])
    
    all_results <- rbind(all_results, tmt_est)
    
  }
  
  
  plot_results <- tibble(scenario=scenario_name, 
                         mean_est= mean(all_results$est), 
                         mean_se=mean(all_results$estse), 
                         mcerror_mean = sd(all_results$est),
                         mcerror_se=sd(all_results$estse))
  
  return(plot_results)
  
}
