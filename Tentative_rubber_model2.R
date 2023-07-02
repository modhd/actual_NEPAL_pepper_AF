# Rubber and black pepper project----
#usually, packages are loaded at the beginning of a code (so you won't run the command everytime when going trough the code)
library(decisionSupport)

# Read data----
table <- read.csv("AF_input_table.csv", sep = ";")
str(table) #check whether reading data was successful


# Model function----

model_function <- function() {
  # Sceniaros
  ### As it is not known yet, the moist microclimate underneath the rubber trees can result in higher yields
  ### or, on the other hand, competition of the roots might lead to lower yields of both crops
  water_competition <- chance_event(water_competition_probability, value_if = 1, value_if_not = 0)
  

  #Natural benefits----
  ##enhanced nutrient cycle----
  mono_nutrient <- vv(var_mean = mono_nutrient,
                            var_CV = var_mono_nutrient,
                            n = n_years) * mono_nutrient_saved
  mono_nutrient_saved <- mono_nutrient
  
  AF_nutrient <- vv(var_mean = AF_nutrient,
                            var_CV = var_AF_nutrient,
                            n = n_years)
  AF_nutrient_saved <- AF_nutrient * AF_nutrient_saved
  

  # Yields----
  # account for risk in diminished yields due to knowledge gaps especially at the initial phase
  # and risk of knowledge gaps that could also lead to lower yields
  management_errors <- vv(var_mean = knowledge_gap_probability, var_CV = var_knowledge_gap_probability,
                          n = n_years, relative_trend = - 10)
  
  # Yield incomes
  ## account for water competition risk and initial knowledge gaps
  rubber_yield <- vv(var_mean = max_rubber_yield * (water_competition * yield_if_competition), 
                     var_CV = var_rubber_yield, n = n_years)
  rubber_yield <- rubber_yield * (1 - management_errors)
  rubber_income <- rubber_yield * rubber_price
    

  pepper_yield <- vv(var_mean = max_pepper_yield * (water_competition * yield_if_competition),
                     var_CV = var_pepper_yield, n = n_years)
  pepper_yield <- pepper_yield * (1 - management_errors)
  pepper_income <- pepper_yield * pepper_price
  

    #Final benefits----
  rubber_benefit <- rubber_income + mono_nutrient_saved
  AF_benefit <- pepper_income + rubber_income + AF_nutrient_saved
  

  # Costs----
  ##establishment costs in 1rst year----
  establistment_cost_mono <- c(establishment_cost_mono, rep(0, n_years))
  establistment_cost_AF <- c(establishment_cost_AF, rep(0, n_years))
  
  maintenance_cost_mono <- vv(rubber_main_cost, var_CV = var_rubber_main_cost,
                              n = n_years)
  maintenance_cost_pepper <- vv(pepper_main_cost, var_CV = var_pepper_main_cost,
                              n = n_years)
  
  
  # Water costs----
  ### pepper needs to be irrigated which is an addition cost.
  cost_water <- vv(cost_water, var_cost_water, n_years)
  
  if (water_competition) {
    AF_water <- vv(mean_AF_water, var_AF_water, n_years)
  } else {
    AF_water <- vv(competition_AF_water, var_AF_water, n_years)
  }
    
  cost_AF_water <- AF_water * cost_water
  
  
  #Substracting the costs to get final benefits----
  final_income_mono <- rubber_benefit - (maintenance_cost_mono + establishment_cost_mono)
    
  #AF----
  final_income_AF <- AF_benefit + rubber_benefit - (establishment_cost_AF + maintenance_cost_pepper
                                                   + maintenance_cost_mono + cost_AF_water)

  # discount rate set to 10 percent
  NPV_mono <- discount(final_income_mono, discount_rate = 10, calculate_NPV = TRUE)
  NPV_AF <- discount(final_income_AF, discount_rate = 10, calculate_NPV = TRUE)
  
  # calculate the overall NPV of the decision (do - don't do)
  NPV_decision <- NPV_AF - NPV_mono
  
  return(list(NPV_mono =  NPV_mono,
              NPV_AF =  NPV_AF, 
              NPV_decision = NPV_decision))
  
  
  
}
#debug(model_function)


# Montecarlo Simulation ####

AF_example_mc_simulation <- mcSimulation(as.estimate(table),
                                         model_function = model_function,
                                         numberOfModelRuns = 200,
                                         functionSyntax = "plainNames")
AF_example_mc_simulation

# Plot the distribution ####
plot_distributions(mcSimulation_object = AF_example_mc_simulation,
                   vars = c("NPV_mono" , "NPV_AF"),
                   method = 'smooth_simple_overlay',
                   base_size= 11)


# VoI
# (i.e. the NPV_decision)
mcSimulation_table <- data.frame(AF_example_mc_simulation$x, 
                                      AF_example_mc_simulation$y[3])
mcSimulation_table

evpi_AF <- multi_EVPI(mc = mcSimulation_table_hail, 
                        first_out_var = "NPV_decision")
evpi_AF

plot_evpi(evpi_AF, decision_vars = "NPV_decision")

