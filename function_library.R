### Function library to be called later ###

# Return data table slice that will be the population for one instance in a Monte Carlo simulation.
montecarlo_population <- function(data_table, sample_size){
  return(data_table[sample(1:dim(data_table)[1],sample_size,replace=F)])
}

# Getting last yr's treatment distribution.
treatment_distribution <- function(data_table, treatment_col_name){
  return(as.list(table(data_table[,..treatment_col_name])/sum(table(data_table[,..treatment_col_name]))))
}

# Add treatment distribution to persistent storage using the treatment_distribution function
add_distribution_table <- function(yr, data_table, treatment_col_name){
  distribution_history[[toString(yr)]] <<- treatment_distribution(data_table, treatment_col_name)
}

# Input treatment distribution manually
# Hard coded now, should be to be soft coded but unsure how to bring in names of treatments and associated value at the same time from the input parameters list
add_distribution_manual <- function(yr, ACS=0.25, ASFR=0.2, CFF=0.15, Exam=0.1, Unworked=0.3){
  distribution_history[[toString(yr)]]<<- list('ACS' = ACS, 'ASFR' = ASFR, 'CFF' = CFF, 'Exam' = Exam, 'Unworked' = Unworked)
}

# Assign treatment based on source year's distribution
assign_treatment <- function(data_table, col_name, src_yr, treatments = c('ACS','ASFR','CFF','Exam','Unworked')){
  data_table[,col_name]<-sample(treatments,size = nrow(data_table), replace=TRUE, prob = as.vector(do.call(c, distribution_history[toString(src_yr)])))
  return(data_table)
}

### Testing and debugging starts here ###
set.seed(1337)
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)

# parameter setting. Avoid magic variables/values
sample_size= 1000
pop_size = 1e6
treatment_col_name = 'treatment'

# generate toy data table. Stand in for new yr's nf data until we get it
current_yr_data <- data.table(tin=1:pop_size,treatment=sample(c('ACS','ASFR','CFF','Exam','Unworked'), pop_size, replace= TRUE))

# Generate/load treatment distribution dictionary/list
distribution_history <- list()
load(file='distribution_history')

# Read parse data table for new distribution, add entry to distribution history
add_distribution_table(sample(1:3000,1),current_yr_data,treatment_col_name)

# Manually add distribution values to distribution_history
add_distribution_manual(sample(1:3000,1), ACS=0.25, ASFR=0.2, CFF=0.15, Exam=0.1, Unworked=0.3)

# Create Monte Carlo simulation population from a data table
monte_pop <- montecarlo_population(current_yr_data,sample_size)

# Assign treatment to monte carlo population based on source year's distribution
monte_pop <- assign_treatment(monte_pop, 'new_treatment', 1602)

# see distribution
print(table(monte_pop[,new_treatment]))
print(table(monte_pop[,new_treatment])/sum(table(monte_pop[,new_treatment])))

# Save new/updated distribution_history. I wonder if version control is necessary???
save(distribution_history,file='distribution_history')

