# Rubber and black pepper project----
#usually, packages are loaded at the beginning of a code (so you won't run the command everytime when going trough the code)
library(decisionSupport)

# Read data----
table <- read.csv("AF_input_table.csv", sep = ";")
str(table) #check whether reading data was successful


# Model function----

model_function <- function() {

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
  
  ##2 scenarios of water management and yields----
  ### generally, pepper needs to be irrigated which is an addition cost.
  ### As it is not known yet, the moist microclimate underneath the rubber trees can result in higher yields
  ### or, on the other hand, competition of the roots might lead to lower yields of both crops
  water_competition <- chance_event(water_competition_probability, value_if = 1, value_if_not = 0)
  
  
  # benefitial microclimate: enhanced yields (higher median values of the yield distributions)
  ## and lower irrigation costs (also, lower median)
  if (water_competition == 0) {
    mean_AF_water <- min_AF_water
    mean_rubber_yield <- max_rubber_yield
    mean_pepper_yield <- max_pepper_yield
    rel_trend = FALSE

  # negative competition: lower yields with negative trend
  } else {
    mean_AF_water <- max_AF_water
    mean_rubber_yield <- min_rubber_yield
    rel_trend = -10
    mean_pepper_yield <- min_pepper_yield
  }
  
  # Yields----
  #Yield incomes
  rubber_yield <- vv(var_mean = mean_rubber_yield, var_CV = var_rubber_yield, n = n_years,
                     relative_trend = rel_trend)
  rubber_income <- rubber_yield * rubber_price
  
  pepper_yield <- vv(var_mean = mean_pepper_yield, var_CV = var_pepper_yield, n = n_years,
                     relative_trend = rel_trend)
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
  cost_water <- vv(cost_water, var_cost_water, n_years)
  AF_water <- vv(mean_AF_water, var_AF_water, n_years)
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



