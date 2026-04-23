# data analysis
library(tidyverse)        # For data manipulation (dplyr, ggplot2, etc.)
library(readxl)           # For reading Excel files: read_excel()
library(plm)              # For panel regressions: pdata.frame(), plm()


# load data
data_regressions = read_excel("Data/Overview/data_pooled.xlsx")  |> #mutate(across(.cols = contains("gov_spending_tot_g"), .fns = ~ . * 100))
  mutate(total_gov_spending_cap_g_1000 = total_gov_spending_cap_g / 1000,
         gdp_cap_1000 = gdp_cap / 1000,
         popdens_1000 = popdens / 1000) |>

  select(name, year, thresholds, expect:air, contains("spending_tot_g"),
         total_gov_spending_cap_g_1000, corrupt, popdens_1000,
         gdp_cap_1000, rule,
         goveffect, total_gov_spending_cap_wid, unequal, regulatory ) 


##### regressions  (using gloria spending data)

# converting for regressions
data_regressions_plm <- pdata.frame(data_regressions,  index = c( "year" ))    # name controls too much
                                                      # time fixed control for economic crisis


# gov
model_gov_g <- plm(             #     # health_envi: 1.03*, soc = 0.64, edu_recr = 5.93***, hous: 16.05***, admin = 15.84***
  thresholds ~ soc_gov_spending_tot_g  +          # indu = 5.50***, ord = -4.72***
    gdp_cap_1000  + 
    corrupt +       
    popdens_1000    
  ,         
  model = "within", 
  data = data_regressions_plm          
)                               
summary(model_gov_g)


# hh
model_hh_g <- plm(             #   rule of law.   health_envi = 2.67, soc = - 0.988,  edu_recr = -0.972
  thresholds ~   edu_recr_hh_spending_tot_g  +   
    gdp_cap_1000  + 
     corrupt +  
    popdens_1000
  ,       
  model = "within",              
  data = data_regressions_plm             
)                               
summary(model_hh_g)

