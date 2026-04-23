# data overview
library(tidyverse)
library(sf)
library(readxl)
library(writexl)
library(ggthemes)  
library(rnaturalearth)
library(rnaturalearthdata) 
library(mapview)
library(interactions)
library(wesanderson)
library(scico)
library(ggsci)


# load absolute data
data_spending = read_excel("Data/Government spending/data_spending_absolute.xlsx") |>
  filter(!Variable %in% c("GDP", "GDP (per capita)", "Population"))  # already in aux data

data_social = read_excel("Data/Social indicators/data_soc_indi_absolute.xlsx")   
data_eco = read_excel("Data/Ecological indicators/data_eco_indi_absolute.xlsx")

data_aux = read_excel("Data/Auxiliary variables/data_aux_absolute.xlsx") 

#View(data_aux)
#View(data_social)
#View(data_eco)

# merge data
data_all_pre2 = bind_rows(data_spending, data_social) 
data_all_pre1 = bind_rows(data_all_pre2, data_eco)
data_all_pre = bind_rows(data_all_pre1, data_aux)|>
  group_by(`Country Name`) |>
  filter(!str_detect(`Country Name`, regex("Macao|Hongkong|Kerala|Hong Kong|Taiwan|FAO|Virgin Islands", ignore_case = TRUE)))   # remove them because otherwide double counting

#View(data_all_pre)
# summarize country data
data_all_pre$`Country Name` <- gsub("Korea, Dem..*", "North Korea", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Korea \\(Dem..*", "North Korea", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Democratic People's Republic of Korea", "North Korea", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Korea, Republic o.*", "South Korea", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Korea, Rep.", "South Korea", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Republic of Korea", "South Korea", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Korea \\(Republ.*", "South Korea", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("^Korea$", "South Korea", data_all_pre$`Country Name`)


data_all_pre$`Country Name` <- gsub("Bahrain.*", "Bahrain", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("China.*", "China", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Congo, Republic of.*", "Congo", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Congo, Rep.", "Congo", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Republic of the Congo", "Congo", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Republic of Congo", "Congo", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Congo, Dem. Rep.", "Democratic Congo", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Dem. Rep. C.*", "Democratic Congo", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Congo-Kinshasa", "Democratic Congo", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Congo-Brazzaville", "Congo", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Congo .*", "Congo", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Democratic Congo", "DR Congo", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Dem. Rep. of the Congo", "DR Congo", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Congo, Democratic Republic of the", "DR Congo", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Rep Congo", "Congo", data_all_pre$`Country Name`)


data_all_pre$`Country Name` <- gsub("Czech.*", "Czechia", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Hongkong.*", "Hongkong", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Honduras.*", "Honduras", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Iran.*", "Iran", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Egypt.*", "Egypt", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Lao .*", "Laos", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Panama.*", "Panama", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Russian.*", "Russia", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Republic of Serbia", "Serbia", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Slovak.*", "Slovakia", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Syria.*", "Syria", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Turkiy.*", "Turkey", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Türkiy.*", "Turkey", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Viet Nam", "Vietnam", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Republic of Vietnam", "Vietnam", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Bosnia & Herzegovina", "Bosnia and Herzegovina", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Brunei Darussalam", "Brunei", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Bulgaria.*", "Bulgaria", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Yemen.*", "Yemen", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("North Macedonia", "Macedonia", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Tanzania.*", "Tanzania", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("United Republic of Tanzania", "Tanzania", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("West Bank and Gaza", "Palestine", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("State of Palestine", "Palestine", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Palestine.*", "Palestine", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Palesti.*", "Palestine", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Central Africa Republic", "Central African Republic", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Gambia.*", "Gambia", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Kyrgyz.*", "Kyrgyzstan", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Bahamas.*", "Bahamas", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Saint Vincent and the Grenadines", "St. Vincent and the Grenadines", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Hong Kong", "Hongkong", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Hongkong.*", "Hongkong", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Taiwan.*", "Taiwan", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Macao.*", "Macao", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Micronesia.*", "Micronesia", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Republic Moldova", "Moldova", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Timor-Leste.*", "Timor-Leste", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Venezuel.*", "Venezuela", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Bahamas.*", "Bahamas", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Virgin Islands .*", "United States Virgin Islands", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Faröe Islands", "Faroe Islands", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Faeroe Islands", "Faroe Islands", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Lichtenstein", "Liechtenstein", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Armenia.*", "Armenia", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Aruba.*", "Aruba", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Azerbaijan.*", "Azerbaijan", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Belarus.*", "Belarus", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Cabo Ver.*", "Cape Verde", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Cape Verd.*", "Cape Verde", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("China.*", "China", data_all_pre$`Country Name`)


data_all_pre$`Country Name` <- gsub("C√¥te d'Ivoire", "Cote d'Ivoire", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Côte d'Ivoire", "Cote d'Ivoire", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Côte d'Ivoire", "Cote d'Ivoire", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Ivory Coast", "Cote d'Ivoire", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Central Afri.*", "Central African Republic", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Comoros.*", "Comoros", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Croatia,.*", "Croatia", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Equatorial Guinea,.*", "Equatorial Guinea", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Eritrea.*", "Eritrea", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Estonia.*", "Estonia", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Eswatini.*", "Eswatini", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Ethiopia*", "Ethiopia", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Fiji.*", "Fiji", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Franc.*", "France", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Jersey, Channel Islands*", "Channel Islands", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Jersey", "Channel Islands", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Kazakhstan,.*", "Kazakhstan", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Kyrgyz.*", "Kyrgyztan", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Lesotho.*", "Lesotho", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Macedonia.*", "Macedonia", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Madagascar.*", "Madagascar", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Marshall.*", "Marshall Islands", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Mauritania.*", "Mauritania", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Moldova.*", "Moldova", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Mozambique.*", "Mozambique", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Netherlands.*", "Netherlands", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Poland.*", "Poland", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("São Tomé.*", "Sao Tome and Principe", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Serbia.*", "Serbia", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Sint Maarten.*", "Sint Maarten (Dutch part)", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("St. Marti.*", "Saint Martin (French part)", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Slovenia.*", "Slovenia", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("South Afr.*", "South Africa", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Tajikistan.*", "Tajikistan", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Uzbekistan.*", "Uzbekistan", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("United States of America*", "United States", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("USA*", "United States", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("United King.*", "United Kingdom", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Wallis and Futuna.*", "Wallis and Futuna", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Ethiopia.*", "Ethiopia", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("South Sudan.*", "South Sudan", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Dominican.*", "Dominican Republic", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Saint Christopher (St. Kitts) and Nevis", "St. Kitts and Nevis", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Saint Kitts and Nevis", "St. Kitts and Nevis", data_all_pre$`Country Name`)



# remove world regions
data_all_pr = data_all_pre |> 
  filter(!grepl("SDG", `Country Name`) & !grepl("WHO", `Country Name`) & !grepl("WB", `Country Name`), 
         `Country Name` != "Sub Saharan Africa",
         `Country Name` != "Sub-Saharan Africa",
         `Country Name` != "European Union (27)",
         `Country Name` != "High-income countries",
         `Country Name` != "Low-income countries", 
         `Country Name` != "Lower-middle-income countries",
         `Country Name` != "Middle-income countries",
         `Country Name` != "Upper-middle-income countries",
         `Country Name` != "Land Locked Developing Countries",
         `Country Name` != "North America",
         `Country Name` != "South Asia",
         `Country Name` != "South America",
         `Country Name` != "Africa",
         `Country Name` != "Europe",
         `Country Name` != "Oceania",
         `Country Name` != "Asia",  
         `Country Name` != "World",
         `Country Name` != "Least Developed Countries",
         `Country Name` != "Small Island Developing States",
         `Country Name` != "Central Europe and the Baltics",
         `Country Name` != "Early-demographic dividend",
         `Country Name` != "East Asia & Pacific",
         `Country Name` != "East Asia & Pacific (IDA & IBRD countries)",
         `Country Name` != "Central Europe and the Baltics",
         `Country Name` != "Central Europe and the Baltics",
         `Country Name` != "East Asia & Pacific (excluding high income)",
         `Country Name` != "Heavily indebted poor countries (HIPC)",
         `Country Name` != "Latin America & Caribbean",
         `Country Name` != "Low & middle income",
         `Country Name` != "Low income",
         `Country Name` != "Lower middle income",
         `Country Name` != "Middle East & North Africa",
         `Country Name` != "Lower middle income"
  )

# summarize data for same countries       
data_all_p <- data_all_pr |>
  group_by(`Country Name`, Variable) |>
    summarise(across(`2005`:`2019`, ~ sum(.x, na.rm = TRUE)))

# change 0s to NA's a and remove them
data_all_ <- data_all_p |>
  mutate(across(where(is.numeric), ~ na_if(., 0.00000))) 

#View(data_all_)
data_all_absolute = data_all_ |>
  mutate(across(
    .cols = c(`2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`),  # year columns
    .fns = ~ if_else(Variable %in% c("Formal settlement", "Minimum income", "Adolescent birth rate", "Employment", "Social support"), . / 100, .),  # condition
    .names = "{.col}"  
  ))  |>
  rowwise() |>
  filter(!is.na(Variable)) |> ungroup()

#View(data_all_absolute)
write_xlsx(data_all_absolute, "Data/Overview/data_all_absolute.xlsx")





### load abs data
data_spending = read_excel("Data/Government spending/data_spending_abs.xlsx") 
data_social = read_excel("Data/Social indicators/data_soc_indi_abs.xlsx") 
data_eco = read_excel("Data/Ecological indicators/data_eco_indi_abs.xlsx")
data_aux = read_excel("Data/Auxiliary variables/data_aux_abs.xlsx")

#View(data_eco)
# merge data
data_all_pre2 = bind_rows(data_spending, data_social) 
data_all_pre1 = bind_rows(data_all_pre2, data_eco)
data_all_pre = bind_rows(data_all_pre1, data_aux) |>
  group_by(`Country Name`) |>
  filter(!str_detect(`Country Name`, regex("Macao|Hongkong|Kerala|Hong Kong|Taiwan|FAO|Virgin Islands", ignore_case = TRUE)))   # remove them because otherwide double counting

#View( data_all_pre)
# summarize country data
data_all_pre$`Country Name` <- gsub("Korea, Dem..*", "North Korea", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Korea \\(Dem..*", "North Korea", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Democratic People's Republic of Korea", "North Korea", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Korea, Republic o.*", "South Korea", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Korea, Rep.", "South Korea", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Republic of Korea", "South Korea", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Korea \\(Republ.*", "South Korea", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("^Korea$", "South Korea", data_all_pre$`Country Name`)


data_all_pre$`Country Name` <- gsub("Bahrain.*", "Bahrain", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("China.*", "China", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Congo, Republic of.*", "Congo", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Congo, Rep.", "Congo", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Republic of the Congo", "Congo", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Republic of Congo", "Congo", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Congo, Dem. Rep.", "Democratic Congo", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Dem. Rep. C.*", "Democratic Congo", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Congo-Kinshasa", "Democratic Congo", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Congo-Brazzaville", "Congo", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Congo .*", "Congo", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Democratic Congo", "DR Congo", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Dem. Rep. of the Congo", "DR Congo", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Congo, Democratic Republic of the", "DR Congo", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Rep Congo", "Congo", data_all_pre$`Country Name`)


data_all_pre$`Country Name` <- gsub("Czech.*", "Czechia", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("CSSR/Czechia*", "Czechia", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Hongkong.*", "Hongkong", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Honduras.*", "Honduras", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Iran.*", "Iran", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Egypt.*", "Egypt", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Lao .*", "Laos", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Panama.*", "Panama", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Russian.*", "Russia", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Republic of Serbia", "Serbia", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Yugoslavia/Serbia", "Serbia", data_all_pre$`Country Name`)


data_all_pre$`Country Name` <- gsub("Slovak.*", "Slovakia", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Syria.*", "Syria", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Turkiy.*", "Turkey", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Türkiy.*", "Turkey", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Viet Nam", "Vietnam", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Republic of Vietnam", "Vietnam", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Bosnia & Herzegovina", "Bosnia and Herzegovina", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Brunei Darussalam", "Brunei", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Bulgaria.*", "Bulgaria", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Yemen.*", "Yemen", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("North Macedonia", "Macedonia", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Tanzania.*", "Tanzania", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("United Republic of Tanzania", "Tanzania", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("West Bank and Gaza", "Palestine", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("State of Palestine", "Palestine", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Palestine.*", "Palestine", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Palesti.*", "Palestine", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Central Africa Republic", "Central African Republic", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Gambia.*", "Gambia", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Kyrgyz.*", "Kyrgyzstan", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Bahamas.*", "Bahamas", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Saint Vincent and the Grenadines", "St. Vincent and the Grenadines", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Hong Kong", "Hongkong", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Hongkong.*", "Hongkong", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Taiwan.*", "Taiwan", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Macao.*", "Macao", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Micronesia.*", "Micronesia", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Republic Moldova", "Moldova", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Timor-Leste.*", "Timor-Leste", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Venezuel.*", "Venezuela", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Bahamas.*", "Bahamas", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Virgin Islands .*", "United States Virgin Islands", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Faröe Islands", "Faroe Islands", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Faeroe Islands", "Faroe Islands", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Lichtenstein", "Liechtenstein", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Armenia.*", "Armenia", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Aruba.*", "Aruba", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Azerbaijan.*", "Azerbaijan", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Belarus.*", "Belarus", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Cabo Ver.*", "Cape Verde", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Cape Verd.*", "Cape Verde", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("China.*", "China", data_all_pre$`Country Name`)


data_all_pre$`Country Name` <- gsub("C√¥te d'Ivoire", "Cote d'Ivoire", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Côte d'Ivoire", "Cote d'Ivoire", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Côte d'Ivoire", "Cote d'Ivoire", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Ivory Coast", "Cote d'Ivoire", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Central Afri.*", "Central African Republic", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Comoros.*", "Comoros", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Croatia,.*", "Croatia", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Equatorial Guinea,.*", "Equatorial Guinea", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Eritrea.*", "Eritrea", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Estonia.*", "Estonia", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Eswatini.*", "Eswatini", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Ethiopia*", "Ethiopia", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Fiji.*", "Fiji", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Franc.*", "France", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Jersey, Channel Islands*", "Channel Islands", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Jersey", "Channel Islands", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Kazakhstan,.*", "Kazakhstan", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Kyrgyz.*", "Kyrgyztan", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Lesotho.*", "Lesotho", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Macedonia.*", "Macedonia", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Madagascar.*", "Madagascar", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Marshall.*", "Marshall Islands", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Mauritania.*", "Mauritania", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Moldova.*", "Moldova", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Mozambique.*", "Mozambique", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Netherlands.*", "Netherlands", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Poland.*", "Poland", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("São Tomé.*", "Sao Tome and Principe", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Serbia.*", "Serbia", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Sint Maarten.*", "Sint Maarten (Dutch part)", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("St. Marti.*", "Saint Martin (French part)", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Slovenia.*", "Slovenia", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("South Afr.*", "South Africa", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Tajikistan.*", "Tajikistan", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Uzbekistan.*", "Uzbekistan", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("United States of America*", "United States", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("USA*", "United States", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("United King.*", "United Kingdom", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Wallis and Futuna.*", "Wallis and Futuna", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Ethiopia.*", "Ethiopia", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("South Sudan.*", "South Sudan", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Dominican.*", "Dominican Republic", data_all_pre$`Country Name`)

data_all_pre$`Country Name` <- gsub("Saint Christopher (St. Kitts) and Nevis", "St. Kitts and Nevis", data_all_pre$`Country Name`)
data_all_pre$`Country Name` <- gsub("Saint Kitts and Nevis", "St. Kitts and Nevis", data_all_pre$`Country Name`)


#View(data_all_pre)


# remove world regions
data_all_pr = data_all_pre |> 
  filter(!grepl("SDG", `Country Name`) & !grepl("WHO", `Country Name`) & !grepl("WB", `Country Name`), 
         `Country Name` != "Sub Saharan Africa",
         `Country Name` != "Sub-Saharan Africa",
         `Country Name` != "European Union (27)",
         `Country Name` != "High-income countries",
         `Country Name` != "Low-income countries", 
         `Country Name` != "Lower-middle-income countries",
         `Country Name` != "Middle-income countries",
         `Country Name` != "Upper-middle-income countries",
         `Country Name` != "Land Locked Developing Countries",
         `Country Name` != "North America",
         `Country Name` != "South Asia",
         `Country Name` != "South America",
         `Country Name` != "Africa",
         `Country Name` != "Europe",
         `Country Name` != "Oceania",
         `Country Name` != "Asia",  
         `Country Name` != "World",
         `Country Name` != "Least Developed Countries",
         `Country Name` != "Small Island Developing States",
         `Country Name` != "Central Europe and the Baltics",
         `Country Name` != "Early-demographic dividend",
         `Country Name` != "East Asia & Pacific",
         `Country Name` != "East Asia & Pacific (IDA & IBRD countries)",
         `Country Name` != "Central Europe and the Baltics",
         `Country Name` != "Central Europe and the Baltics",
         `Country Name` != "East Asia & Pacific (excluding high income)",
         `Country Name` != "Heavily indebted poor countries (HIPC)",
         `Country Name` != "Latin America & Caribbean",
         `Country Name` != "Low & middle income",
         `Country Name` != "Low income",
         `Country Name` != "Lower middle income",
         `Country Name` != "Middle East & North Africa",
         `Country Name` != "Lower middle income"
  )

#View(data_all_pr)
# summarize data for same countries       
data_all_p <- data_all_pr |>
  group_by(`Country Name`) |>
  summarise(
    
    `Total government (wid) spending (per capita)` = sum(`Total government (wid) spending (per capita)`, na.rm = TRUE),   # government (wid) spending
    `Admin government (wid) spending (per capita)` = sum(`Admin government (wid) spending (per capita)`, na.rm = TRUE),
    `Health government (wid) spending (per capita)` = sum(`Health government (wid) spending (per capita)`, na.rm = TRUE),
    `Environment government (wid) spending (per capita)` = sum(`Environment government (wid) spending (per capita)`, na.rm = TRUE),
    `Social protection government (wid) spending (per capita)` = sum(`Social government (wid) spending (per capita)`, na.rm = TRUE),
    `Education government (wid) spending (per capita)` = sum(`Education government (wid) spending (per capita)`, na.rm = TRUE),
    `Recreation government (wid) spending (per capita)` = sum(`Recreation government (wid) spending (per capita)`, na.rm = TRUE),
    `Housing government (wid) spending (per capita)` = sum(`Housing government (wid) spending (per capita)`, na.rm = TRUE),
    `Order government (wid) spending (per capita)` = sum(`Order government (wid) spending (per capita)`, na.rm = TRUE),
    `Defense government (wid) spending (per capita)` = sum(`Defense government (wid) spending (per capita)`, na.rm = TRUE),
    `Industry government (wid) spending (per capita)` = sum(`Industry government (wid) spending (per capita)`, na.rm = TRUE),
    
    `Health government (wid) spending (%Total)` = sum(`Health government (wid) spending (%Total)`, na.rm = TRUE),
    `Environment government (wid) spending (%Total)` = sum(`Environment government (wid) spending (%Total)`, na.rm = TRUE),
    `Social protection government (wid) spending (%Total)` = sum(`Social government (wid) spending (%Total)`, na.rm = TRUE),
    `Education government (wid) spending (%Total)` = sum(`Education government (wid) spending (%Total)`, na.rm = TRUE),
    `Recreation government (wid) spending (%Total)` = sum(`Recreation government (wid) spending (%Total)`, na.rm = TRUE),
    `Housing government (wid) spending (%Total)` = sum(`Housing government (wid) spending (%Total)`, na.rm =TRUE),  
    `Industry government (wid) spending (%Total)` = sum(`Industry government (wid) spending (%Total)`, na.rm =TRUE),   
    `Order government (wid) spending (%Total)` = sum(`Order government (wid) spending (%Total)`, na.rm =TRUE),   
    `Defense government (wid) spending (%Total)` = sum(`Defense government (wid) spending (%Total)`, na.rm =TRUE),   
    `Admin government (wid) spending (%Total)` = sum(`Admin government (wid) spending (%Total)`, na.rm =TRUE),   
    
    `Total government (wid) spending (%GDP)` = sum(`Total government (wid) spending (%GDP)`, na.rm = TRUE),
    `Admin government (wid) spending (%GDP)` = sum(`Admin government (wid) spending (%GDP)`, na.rm = TRUE),
    `Health government (wid) spending (%GDP)` = sum(`Health government (wid) spending (%GDP)`, na.rm = TRUE),
    `Environment government (wid) spending (%GDP)` = sum(`Environment government (wid) spending (%GDP)`, na.rm = TRUE),
    `Social protection government (wid) spending (%GDP)` = sum(`Social government (wid) spending (%GDP)`, na.rm = TRUE),
    `Education government (wid) spending (%GDP)` = sum(`Education government (wid) spending (%GDP)`, na.rm = TRUE),
    `Housing government (wid) spending (%GDP)` = sum(`Housing government (wid) spending (%GDP)`, na.rm =TRUE),  
    `Industry government (wid) spending (%GDP)` = sum(`Industry government (wid) spending (%GDP)`, na.rm =TRUE),   
    `Order government (wid) spending (%GDP)` = sum(`Order government (wid) spending (%GDP)`, na.rm =TRUE),   
    
  #  `Total government (g) spending (per capita)` = sum(`Total government (g) spending (per capita)`, na.rm = TRUE),   # government (g) spending
    `Admin government (g) spending (per capita)` = sum(`Admin government (g) spending (per capita)`, na.rm = TRUE),
    `Health government (g) spending (per capita)` = sum(`Health government (g) spending (per capita)`, na.rm = TRUE),
    `Environemnt government (g) spending (per capita)` = sum(`Environment government (g) spending (per capita)`, na.rm = TRUE),
    `Social protection government (g) spending (per capita)` = sum(`Social government (g) spending (per capita)`, na.rm = TRUE),
    `Education government (g) spending (per capita)` = sum(`Education government (g) spending (per capita)`, na.rm = TRUE),
    `Recreation government (g) spending (per capita)` = sum(`Recreation government (g) spending (per capita)`, na.rm = TRUE),
    `Housing government (g) spending (per capita)` = sum(`Housing government (g) spending (per capita)`, na.rm = TRUE),
    `Order government (g) spending (per capita)` = sum(`Order government (g) spending (per capita)`, na.rm = TRUE),
    `Defense government (g) spending (per capita)` = sum(`Defense government (g) spending (per capita)`, na.rm = TRUE),
    `Industry government (g) spending (per capita)` = sum(`Industry government (g) spending (per capita)`, na.rm = TRUE),
    
    `Health government (g) spending (%Total)` = sum(`Health government (g) spending (%Total)`, na.rm = TRUE),
    `Environment government (g) spending (%Total)` = sum(`Environment government (g) spending (%Total)`, na.rm = TRUE),
    `Social protection government (g) spending (%Total)` = sum(`Social government (g) spending (%Total)`, na.rm = TRUE),
    `Education government (g) spending (%Total)` = sum(`Education government (g) spending (%Total)`, na.rm = TRUE),
    `Recreation government (g) spending (%Total)` = sum(`Recreation government (g) spending (%Total)`, na.rm = TRUE),
    `Housing government (g) spending (%Total)` = sum(`Housing government (g) spending (%Total)`, na.rm =TRUE),  
    `Industry government (g) spending (%Total)` = sum(`Industry government (g) spending (%Total)`, na.rm =TRUE),   
    `Order government (g) spending (%Total)` = sum(`Order government (g) spending (%Total)`, na.rm =TRUE),   
    `Defense government (g) spending (%Total)` = sum(`Defense government (g) spending (%Total)`, na.rm =TRUE),   
    `Admin government (g) spending (%Total)` = sum(`Admin government (g) spending (%Total)`, na.rm =TRUE),   
    
    `Total government (g) spending (%GDP)` = sum(`Total government (g) spending (%GDP)`, na.rm = TRUE),
    `Admin government (g) spending (%GDP)` = sum(`Admin government (g) spending (%GDP)`, na.rm = TRUE),
    `Health government (g) spending (%GDP)` = sum(`Health government (g) spending (%GDP)`, na.rm = TRUE),
    `Environment government (g) spending (%GDP)` = sum(`Environment government (g) spending (%GDP)`, na.rm = TRUE),
    `Social protection government (g) spending (%GDP)` = sum(`Social government (g) spending (%GDP)`, na.rm = TRUE),
    `Education government (g) spending (%GDP)` = sum(`Education government (g) spending (%GDP)`, na.rm = TRUE),
    `Recreation government (g) spending (%GDP)` = sum(`Recreation government (g) spending (%GDP)`, na.rm = TRUE),
    `Housing government (g) spending (%GDP)` = sum(`Housing government (g) spending (%GDP)`, na.rm =TRUE),  
    `Industry government (g) spending (%GDP)` = sum(`Industry government (g) spending (%GDP)`, na.rm =TRUE),   
    `Order government (g) spending (%GDP)` = sum(`Order government (g) spending (%GDP)`, na.rm =TRUE),   
    `Defense government (g) spending (%GDP)` = sum(`Defense government (g) spending (%GDP)`, na.rm =TRUE),   
  
    `Total household (g) spending (per capita)` = sum(`Total household (g) spending (per capita)`, na.rm = TRUE),    # household (g) spending
    `Admin household (g) spending (per capita)` = sum(`Admin household (g) spending (per capita)`, na.rm = TRUE),
    `Health household (g) spending (per capita)` = sum(`Health household (g) spending (per capita)`, na.rm = TRUE),
    `Environment household (g) spending (per capita)` = sum(`Environment household (g) spending (per capita)`, na.rm = TRUE),
    `Social protection household (g) spending (per capita)` = sum(`Social household (g) spending (per capita)`, na.rm = TRUE),
    `Education household (g) spending (per capita)` = sum(`Education household (g) spending (per capita)`, na.rm = TRUE),
    `Recreation household (g) spending (per capita)` = sum(`Recreation household (g) spending (per capita)`, na.rm = TRUE),
    `Housing household (g) spending (per capita)` = sum(`Housing household (g) spending (per capita)`, na.rm = TRUE),
    `Order household (g) spending (per capita)` = sum(`Order household (g) spending (per capita)`, na.rm = TRUE),
    `Defense household (g) spending (per capita)` = sum(`Defense household (g) spending (per capita)`, na.rm = TRUE),
    `Industry household (g) spending (per capita)` = sum(`Industry household (g) spending (per capita)`, na.rm = TRUE),
    
    `Total household (g) spending (%GDP)` = sum(`Total household (g) spending (%GDP)`, na.rm = TRUE),
    `Admin household (g) spending (%GDP)` = sum(`Admin household (g) spending (%GDP)`, na.rm = TRUE),
    `Health household (g) spending (%GDP)` = sum(`Health household (g) spending (%GDP)`, na.rm = TRUE),
    `Environment household (g) spending (%GDP)` = sum(`Environment household (g) spending (%GDP)`, na.rm = TRUE),
    `Social protection household (g) spending (%GDP)` = sum(`Social household (g) spending (%GDP)`, na.rm = TRUE),
    `Education household (g) spending (%GDP)` = sum(`Education household (g) spending (%GDP)`, na.rm = TRUE),
    `Recreation household (g) spending (%GDP)` = sum(`Recreation household (g) spending (%GDP)`, na.rm = TRUE),
    `Housing household (g) spending (%GDP)` = sum(`Housing household (g) spending (%GDP)`, na.rm =TRUE),  
    `Industry household (g) spending (%GDP)` = sum(`Industry household (g) spending (%GDP)`, na.rm =TRUE),   
    `Order household (g) spending (%GDP)` = sum(`Order household (g) spending (%GDP)`, na.rm =TRUE),   
    `Defense household (g) spending (%GDP)` = sum(`Defense household (g) spending (%GDP)`, na.rm =TRUE),   
  
    `Health household (g) spending (%Total)` = sum(`Health household (g) spending (%Total)`, na.rm = TRUE),
    `Environment household (g) spending (%Total)` = sum(`Environment household (g) spending (%Total)`, na.rm = TRUE),
    `Social protection household (g) spending (%Total)` = sum(`Social household (g) spending (%Total)`, na.rm = TRUE),
  `Education household (g) spending (%Total)` = sum(`Education household (g) spending (%Total)`, na.rm = TRUE),
  `Recreation household (g) spending (%Total)` = sum(`Recreation household (g) spending (%Total)`, na.rm = TRUE),
  `Housing household (g) spending (%Total)` = sum(`Housing household (g) spending (%Total)`, na.rm =TRUE),  
    `Industry household (g) spending (%Total)` = sum(`Industry household (g) spending (%Total)`, na.rm =TRUE),   
  `Order household (g) spending (%Total)` = sum(`Order household (g) spending (%Total)`, na.rm =TRUE),   
  `Defense household (g) spending (%Total)` = sum(`Defense household (g) spending (%Total)`, na.rm =TRUE),   
  `Admin household (g) spending (%Total)` = sum(`Admin household (g) spending (%Total)`, na.rm =TRUE),   
    
    `Life expectancy` = sum(`Life expectancy`, na.rm = TRUE) ,     # social outcomes
    `Adolescent birth rate` = sum(`Adolescent birth rate`, na.rm = TRUE),   
    `School years` = sum(`School years`, na.rm = TRUE),
    `Crime absence` = sum(`Crime absence`, na.rm = TRUE),
    `Formal settlement` = sum(`Formal settlement`, na.rm = TRUE),
    `Sufficient nutrition` = sum(`Sufficient nutrition`, na.rm = TRUE),
    `Employment` = sum(`Employment`, na.rm = TRUE),
    `Minimum income` = sum(`Minimum income`, na.rm = TRUE),
    `Social support` = sum(`Social support`, na.rm = TRUE),
    `Clean air` = sum(`Clean air`, na.rm = TRUE),
    
    `GDP` = sum(`GDP`, na.rm =TRUE),       # auxilliaries
    `GDP (per capita)` = sum(`GDP (per capita)`, na.rm =TRUE), 
  
    `Population` = sum(`Population`, na.rm = T),
    `Population density` = sum(`Population density`, na.rm = T),
    `Urbanization` = sum(`Urbanization`, na.rm = T),
  
   `Income inequality` = sum(`Income inequality`, na.rm = T),
  
  
    `Government effectiveness` = sum(`Government effectiveness`, na.rm = T),
    `Rule of law` = sum(`Rule of law`, na.rm = T),
    `Regulatory quality` = sum(`Regulatory quality`, na.rm = T),
    `Corruption control` = sum(`Corruption control`, na.rm = T),
   
    `GHG Admin (Government)` = sum(`GHG Admin (Government)`, na.rm = TRUE),  # government ghg
  `GHG Health (Government)` = sum(`GHG Health (Government)`, na.rm = TRUE),
  `GHG Environment (Government)` = sum(`GHG Environment (Government)`, na.rm = TRUE),
  `GHG Social protection (Government)` = sum(`GHG Social protection (Government)`, na.rm = TRUE),
  `GHG Education (Government)` = sum(`GHG Education (Government)`, na.rm = TRUE),
  `GHG Recreation (Government)` = sum(`GHG Recreation (Government)`, na.rm = TRUE),
  `GHG Housing (Government)` = sum(`GHG Housing (Government)`, na.rm = TRUE),
    `GHG Industry (Government)` = sum(`GHG Industry (Government)`, na.rm = TRUE),
  `GHG Order (Government)` = sum(`GHG Order (Government)`, na.rm = TRUE),
  `GHG Defense (Government)` = sum(`GHG Defense (Government)`, na.rm = TRUE),
  `GHG Total (Government)` = sum(`GHG Total (Government)`, na.rm = TRUE),
   
   `GHG Admin (Household)` = sum(`GHG Admin (Household)`, na.rm = TRUE),  # Household ghg
   `GHG Health (Household)` = sum(`GHG Health (Household)`, na.rm = TRUE),
   `GHG Environment (Household)` = sum(`GHG Environment (Household)`, na.rm = TRUE),
   `GHG Social protection (Household)` = sum(`GHG Social protection (Household)`, na.rm = TRUE),
   `GHG Education (Household)` = sum(`GHG Education (Household)`, na.rm = TRUE),
   `GHG Recreation (Household)` = sum(`GHG Recreation (Household)`, na.rm = TRUE),
   `GHG Housing (Household)` = sum(`GHG Housing (Household)`, na.rm = TRUE),
   `GHG Industry (Household)` = sum(`GHG Industry (Household)`, na.rm = TRUE),
   `GHG Order (Household)` = sum(`GHG Order (Household)`, na.rm = TRUE),
   `GHG Defense (Household)` = sum(`GHG Defense (Household)`, na.rm = TRUE),
   `GHG Total (Household)` = sum(`GHG Total (Household)`, na.rm = TRUE),
    `Carbon factor` = sum(`Carbon factor`, na.rm = T)
  )


# change 0s to NA's a and remove them
data_all_ <- data_all_p |>
  mutate(across(where(is.numeric), ~ na_if(., 0.00000)))  

#View(data_all_)

data_all_abs = data_all_ |>
  mutate(across(c( `Adolescent birth rate`), ~ . / 100)) |>  # all data in absolute values (e.g., not in percentages)
 # mutate(across(matches("%GDP|%Total"), ~ .x * 100)) |>                                     
  select(`Country Name`, matches("%GDP|%Total|per capita"), # spending data
         `GDP`, `GDP (per capita)`,  `Population`, `Population density` ,`Government effectiveness`, `Rule of law`, `Corruption control`, `Regulatory quality`,
         `Life expectancy`, `School years`, `Crime absence`, `Sufficient nutrition`, `Formal settlement`, `Minimum income`, 
         `Employment`, `Adolescent birth rate`, `Social support`,  `Clean air`, 
          `Carbon factor`, 
         matches("GHG"))  |>
  rowwise() |>
  filter(if_any(matches("GHG"), ~ !is.na(.) & . >= 0),
         if_any(matches("per capita"), ~ !is.na(.) & . >= 0)) |>
  filter(rowSums(!is.na(across(everything()))) >= 0.75 * ncol(across(everything())) ) |>   # rows that have at least 75% columns filled
  ungroup()

#View(data_all_abs)
write_xlsx(data_all_abs, "Data/Overview/data_all_abs.xlsx")





### sd data
#View(data_all_abs)
data_all_sd_filtered = data_all_abs

#View(data_all_sd_filtered)
country_names <- data_all_sd_filtered[, 1]  # separate country names and values
numeric_data <- data_all_sd_filtered[, -1]  

#View(numeric_data_scaled)
numeric_data_scaled <- scale(numeric_data)  # standardize 
numeric_data_scaled <- as.data.frame(numeric_data_scaled)
colnames(numeric_data_scaled) <- colnames(numeric_data)


data_all_sd <- cbind(country_names, numeric_data_scaled) |> # combine data
  select(-`Carbon factor`)
  
colnames(data_all_sd)[1] <- colnames(data_all_abs)[1] # rename first column

#hist(data_all_sd[[7]])
#View(data_all_sd)
write_xlsx(data_all_sd, "Data/Overview/data_all_sd.xlsx")




#View(data_all_absolute)

##### get data pooled #####

# load data
data_all_absolute = read_excel("Data/Overview/data_all_absolute.xlsx")   # absolute = panel data
data_all_abs = read_excel("Data/Overview/data_all_abs.xlsx")  # abs = 2019
data_all_sd = read_excel("Data/Overview/data_all_sd.xlsx")  # sd = standardized

#View(data_all_absolute)

#View(data_thresholds_2005_2019)

# get best countries for each year (and threshold)
data_thresholds_2005_2019 <- data_all_absolute |>
  
  select(`Country Name`, Variable, `2005`: `2019`) |>
  # Reshape to long format for years
  pivot_longer(
    cols = `2005`:`2019`,
    names_to = "Year",
    values_to = "Value"
  ) |>
  mutate(Year = as.numeric(Year)) |>
  
# Reshape to wide format for individual variables
  pivot_wider(names_from = "Variable", values_from = "Value") |>
  
  select(`Country Name`, Year, `Life expectancy`, `School years`, `Formal settlement`, `Minimum income`, `Crime absence`,
         `Adolescent birth rate`, `Sufficient nutrition`, Employment, `Social support`, `Clean air`, everything()) |>
  filter(rowSums(is.na(across(`Life expectancy`: `Clean air`))) <= 1) |>
  
  # Ensure Population is included and handled
  mutate(
    
    "Social thresholds achieved" = 
      (if_else(`Life expectancy` >= 74, 1, 0, missing = 0) +
         if_else(`School years` >= 8, 1, 0, missing = 0) +
         if_else(`Formal settlement` >= 0.95, 1, 0, missing = 0) +
         if_else(`Minimum income` >= 0.95, 1, 0, missing = 0) + 
         if_else(`Crime absence` >= 90, 1, 0, missing = 0) + 
         if_else(`Sufficient nutrition` >= 2700, 1, 0, missing = 0) +
         if_else(`Employment` >= 0.94, 1, 0, missing = 0) + 
         if_else(`Adolescent birth rate` >= 0.95, 1, 0, missing = 0) + 
         if_else(`Social support` >= 0.90, 1, 0, missing = 0) + 
         if_else(`Clean air` >= 90, 1, 0, missing = 0)),
    
    "Social thresholds achieved (-1%)" =    # identify countries are lower than actual thresholds but would be above adapted thresholds
      (if_else(`Life expectancy` < 74 & `Life expectancy` >= 74 * 0.99, 1, 0, missing = 0) + 
         if_else(`School years` < 8 & `School years` >= 8 * 0.99, 1, 0, missing = 0) +
         if_else(`Formal settlement` < 0.95 & `Formal settlement` >= 0.95 * 0.99, 1, 0, missing = 0) +
         if_else(`Minimum income` < 0.95 & `Minimum income` >= 0.95 * 0.99, 1, 0, missing = 0) + 
         if_else(`Crime absence` < 90 & `Crime absence` >= 90 * 0.99, 1, 0, missing = 0) + 
         if_else(`Sufficient nutrition` < 2700 & `Sufficient nutrition` >= 2700 * 0.99, 1, 0, missing = 0) +
         if_else(`Employment` < 0.94 & `Employment` >= 0.94 * 0.99, 1, 0, missing = 0) + 
         if_else(`Adolescent birth rate` < 95 & `Adolescent birth rate` >= 95 * 0.99, 1, 0, missing = 0) + 
         if_else(`Social support` < 0.90 & `Social support` >= 0.90 * 0.99, 1, 0, missing = 0) + 
         if_else(`Clean air` < 90 & `Clean air` >= 90 * 0.99, 1, 0, missing = 0)),
    
#    "Social thresholds achieved (-2.5%)" = 
 #     (if_else(`Life expectancy` < 74 & `Life expectancy` >= 74 * 0.975, 1, 0, missing = 0) +
  #       if_else(`School years` < 8 & `School years` >= 8 * 0.975, 1, 0, missing = 0) +
   #      if_else(`Formal settlement` < 95 & `Formal settlement` >= 95 * 0.975, 1, 0, missing = 0) +
    #     if_else(`Minimum income` < 95 & `Minimum income` >= 95 * 0.975, 1, 0, missing = 0) + 
     #    if_else(`Crime absence` < 90 & `Crime absence` >= 90 * 0.975, 1, 0, missing = 0) + 
      #   if_else(`Sufficient nutrition` < 2700 & `Sufficient nutrition` >= 2700 * 0.975, 1, 0, missing = 0) +
       #  if_else(`Employment` < 94 & `Employment` >= 94 * 0.975, 1, 0, missing = 0) + 
        # if_else(`Adolescent birth rate` < 95 & `Adolescent birth rate` >= 95 * 0.975, 1, 0, missing = 0) + 
         #if_else(`Social support` < 90 & `Social support` >= 90 * 0.975, 1, 0, missing = 0) + 
        # if_else(`Clean air` < 90 & `Clean air` >= 90 * 0.975, 1, 0, missing = 0)),
    
#    "Social thresholds achieved (-5%)" = 
 #     (if_else(`Life expectancy` < 74 & `Life expectancy` >= 74 * 0.95, 1, 0, missing = 0) +
  #       if_else(`School years` < 8 & `School years` >= 8 * 0.95, 1, 0, missing = 0) +
   #      if_else(`Formal settlement` < 95 & `Formal settlement` >= 95 * 0.95, 1, 0, missing = 0) +
    #     if_else(`Minimum income` < 95 & `Minimum income` >= 95 * 0.95, 1, 0, missing = 0) + 
     #    if_else(`Crime absence` < 90 & `Crime absence` >= 90 * 0.95, 1, 0, missing = 0) + 
      #   if_else(`Sufficient nutrition` < 2700 & `Sufficient nutrition` >= 2700 * 0.95, 1, 0, missing = 0) +
       #  if_else(`Employment` < 94 & `Employment` >= 94 * 0.95, 1, 0, missing = 0) + 
        # if_else(`Adolescent birth rate` < 95 & `Adolescent birth rate` >= 95 * 0.95, 1, 0, missing = 0) + 
         #if_else(`Social support` < 90 & `Social support` >= 90 * 0.95, 1, 0, missing = 0) + 
         #if_else(`Clean air` < 90 & `Clean air` >= 90 * 0.95, 1, 0, missing = 0)),
    
    "Social thresholds achieved (+1%)" =    # identify countries are higher than actual thresholds but would be below adapted thresholds
      (if_else(`Life expectancy` > 74 & `Life expectancy` <= 74 * 1.01, 1, 0, missing = 0) + 
         if_else(`School years` > 8 & `School years` <= 8 * 1.01, 1, 0, missing = 0) +
         if_else(`Formal settlement` > 0.95 & `Formal settlement` <= 0.95 * 1.01, 1, 0, missing = 0) +
         if_else(`Minimum income` > 0.95 & `Minimum income` <= 0.95 * 1.01, 1, 0, missing = 0) + 
         if_else(`Crime absence` > 90 & `Crime absence` <= 90 * 1.01, 1, 0, missing = 0) + 
         if_else(`Sufficient nutrition` > 2700 & `Sufficient nutrition` <= 2700 * 1.01, 1, 0, missing = 0) +
         if_else(`Employment` > 0.94 & `Employment` <= 0.94 * 1.01, 1, 0, missing = 0) + 
         if_else(`Adolescent birth rate` > 0.95 & `Adolescent birth rate` <= 0.95 * 1.01, 1, 0, missing = 0) + 
         if_else(`Social support` > 0.90 & `Social support` <= 0.90 * 1.01, 1, 0, missing = 0) + 
         if_else(`Clean air` > 90 & `Clean air` <= 90 * 1.01, 1, 0, missing = 0))
    
#    "Social thresholds achieved (+2.5%)" = 
 #     (if_else(`Life expectancy` > 74 & `Life expectancy` <= 74 * 1.025, 1, 0, missing = 0) +
  #       if_else(`School years` > 8 & `School years` <= 8 * 1.025, 1, 0, missing = 0) +
   #      if_else(`Formal settlement` > 95 & `Formal settlement` <= 95 * 1.025, 1, 0, missing = 0) +
    #     if_else(`Minimum income` > 95 & `Minimum income` <= 95 * 1.025, 1, 0, missing = 0) + 
     #    if_else(`Crime absence` > 90 & `Crime absence` <= 90 * 1.025, 1, 0, missing = 0) + 
      #   if_else(`Sufficient nutrition` > 2700 & `Sufficient nutrition` <= 2700 * 1.025, 1, 0, missing = 0) +
       #  if_else(`Employment` > 94 & `Employment` <= 94 * 1.025, 1, 0, missing = 0) + 
        # if_else(`Adolescent birth rate` > 95 & `Adolescent birth rate` <= 95 * 1.025, 1, 0, missing = 0) + 
         #if_else(`Social support` > 90 & `Social support` <= 90 * 1.025, 1, 0, missing = 0) + 
        # if_else(`Clean air` > 90 & `Clean air` <= 90 * 1.025, 1, 0, missing = 0)),
    
#    "Social thresholds achieved (+5%)" = 
 #     (if_else(`Life expectancy` > 74 & `Life expectancy` <= 74 * 1.05, 1, 0, missing = 0) +
  #       if_else(`School years` > 8 & `School years` <= 8 * 1.05, 1, 0, missing = 0) +
   #      if_else(`Formal settlement` > 95 & `Formal settlement` <= 95 * 1.05, 1, 0, missing = 0) +
    #     if_else(`Minimum income` > 95 & `Minimum income` <= 95 * 1.05, 1, 0, missing = 0) + 
     #    if_else(`Crime absence` > 90 & `Crime absence` <= 90 * 1.05, 1, 0, missing = 0) + 
      #   if_else(`Sufficient nutrition` > 2700 & `Sufficient nutrition` <= 2700 * 1.05, 1, 0, missing = 0) +
       #  if_else(`Employment` > 94 & `Employment` <= 94 * 1.05, 1, 0, missing = 0) + 
        # if_else(`Adolescent birth rate` > 95 & `Adolescent birth rate` <= 95 * 1.05, 1, 0, missing = 0) + 
         #if_else(`Social support` > 90 & `Social support` <= 90 * 1.05, 1, 0, missing = 0) + 
        # if_else(`Clean air` > 90 & `Clean air` <= 90 * 1.05, 1, 0, missing = 0))
  ) |>

  group_by(Year, `Social thresholds achieved`) |>
  
  mutate(
    
    # Cluster into low, medium, high thresholds
    `Threshold level` = case_when(
      `Social thresholds achieved` >= 0 & `Social thresholds achieved` <= 3 ~ "Low outcome",
      `Social thresholds achieved` >= 4 & `Social thresholds achieved` <= 6 ~ "Lower-middle outcome",
      `Social thresholds achieved` >= 7 & `Social thresholds achieved` <= 8 ~ "Upper-middle outcome",
      `Social thresholds achieved` >= 9 & `Social thresholds achieved` <= 10 ~ "High outcome",
      
      TRUE ~ NA_character_  # Handle unexpected cases
    )
    
  ) |>
  
  # Keep rows with no missing values for the relevant columns
  filter(rowSums(is.na(across(`Life expectancy`: `Clean air`))) <= 1) |>    # at least 9 out of 10 thresholds available
  
  ungroup() |>
  select(`Country Name`,  `Threshold level`,  everything())  # contains("(+1%)"), contains("thresholds"),
 # filter(Year == 2019 & `Social thresholds achieved (+1%)` != 0) 

#View(data_thresholds_2005_2019)

##### get data_pooled
data_pooled = data_thresholds_2005_2019 |> 
  select(`Country Name`, Year, "Life expectancy", "School years", "Formal settlement", "Minimum income", "Employment",
         "Crime absence", # "Reversed mortality rate babies", "Reversed mortality rate teenagers",
         "Clean air", "Social support", "Sufficient nutrition", "Adolescent birth rate", 
         "GDP", "GDP (per capita)", `Social thresholds achieved`, `Threshold level`, Population, `Population density`,
         `Urbanization`,  `Income inequality`, `Rule of law`, `Government effectiveness`, `Corruption control`, `Regulatory quality`,
         matches("Total|Health|Social|Education|Housing|Industry|Order|Admin|Carbon|Government|Household|thresholds")
  ) |>

  mutate(`Threshold level` = factor(`Threshold level`)) |>
  
  select(`Country Name`, Year, `Social thresholds achieved`, `Threshold level`,  everything()) 


# rename for easier use
data_pooled_renamed = data_pooled |>
  select("name" = `Country Name`,
         "year" = Year, 
         "carb_factor" = `Carbon factor`, 
         "thresholds" = `Social thresholds achieved`,
         "thresh_level" = `Threshold level`, 
         
         # --- Government spending (WID) ---
         "health_gov_spending_tot_wid" = `Health government (wid) spending (%Total)`,    
         "envi_gov_spending_tot_wid" = `Environment government (wid) spending (%Total)`,    
         "soc_gov_spending_tot_wid" = `Social government (wid) spending (%Total)`, 
         "edu_gov_spending_tot_wid" = `Education government (wid) spending (%Total)`, 
         "recr_gov_spending_tot_wid" = `Recreation government (wid) spending (%Total)`, 
         "hous_gov_spending_tot_wid" = `Housing government (wid) spending (%Total)`, 
         "indu_gov_spending_tot_wid" = `Industry government (wid) spending (%Total)`, 
         "ord_gov_spending_tot_wid" = `Order government (wid) spending (%Total)`, 
         "def_gov_spending_tot_wid" = `Defense government (wid) spending (%Total)`, 
         "admin_gov_spending_tot_wid" = `Admin government (wid) spending (%Total)`, 
         
         "total_gov_spending_gdp_wid" = `Total government (wid) spending (%GDP)` ,
         "health_gov_spending_gdp_wid" = `Health government (wid) spending (%GDP)`, 
         "envi_gov_spending_gdp_wid" = `Environment government (wid) spending (%GDP)`, 
         "soc_gov_spending_gdp_wid" = `Social government (wid) spending (%GDP)`, 
         "edu_gov_spending_gdp_wid" = `Education government (wid) spending (%GDP)`, 
         "recr_gov_spending_gdp_wid" = `Recreation government (wid) spending (%GDP)`, 
         "hous_gov_spending_gdp_wid" = `Housing government (wid) spending (%GDP)`, 
         "indu_gov_spending_gdp_wid" = `Industry government (wid) spending (%GDP)`, 
         "ord_gov_spending_gdp_wid" = `Order government (wid) spending (%GDP)`, 
         "def_gov_spending_gdp_wid" = `Defense government (wid) spending (%GDP)`, 
         "admin_gov_spending_gdp_wid" = `Admin government (wid) spending (%GDP)`,
         
         "total_gov_spending_cap_wid" = `Total government (wid) spending (per capita)`,
         "health_gov_spending_cap_wid" = `Health government (wid) spending (per capita)`, 
         "envi_gov_spending_cap_wid" = `Environment government (wid) spending (per capita)`, 
         "soc_gov_spending_cap_wid" = `Social government (wid) spending (per capita)`, 
         "edu_gov_spending_cap_wid" = `Education government (wid) spending (per capita)`, 
         "recr_gov_spending_cap_wid" = `Recreation government (wid) spending (per capita)`, 
         "hous_gov_spending_cap_wid" = `Housing government (wid) spending (per capita)`, 
         "indu_gov_spending_cap_wid" = `Industry government (wid) spending (per capita)`, 
         "ord_gov_spending_cap_wid" = `Order government (wid) spending (per capita)`, 
         "def_gov_spending_cap_wid" = `Defense government (wid) spending (per capita)`, 
         "admin_gov_spending_cap_wid" = `Admin government (wid) spending (per capita)`,
         
         # --- Government spending (Gloria) ---
         "health_gov_spending_tot_g" = `Health government (g) spending (%Total)`,    
         "envi_gov_spending_tot_g" = `Environment government (g) spending (%Total)`,    
         "soc_gov_spending_tot_g" = `Social government (g) spending (%Total)`, 
         "edu_gov_spending_tot_g" = `Education government (g) spending (%Total)`, 
         "recr_gov_spending_tot_g" = `Recreation government (g) spending (%Total)`, 
         "hous_gov_spending_tot_g" = `Housing government (g) spending (%Total)`, 
         "indu_gov_spending_tot_g" = `Industry government (g) spending (%Total)`, 
         "ord_gov_spending_tot_g" = `Order government (g) spending (%Total)`, 
         "def_gov_spending_tot_g" = `Defense government (g) spending (%Total)`, 
         "admin_gov_spending_tot_g" = `Admin government (g) spending (%Total)`, 
         
         "total_gov_spending_gdp_g" = `Total government (g) spending (%GDP)` ,
         "health_gov_spending_gdp_g" = `Health government (g) spending (%GDP)`, 
         "envi_gov_spending_gdp_g" = `Environment government (g) spending (%GDP)`, 
         "soc_gov_spending_gdp_g" = `Social government (g) spending (%GDP)`, 
         "edu_gov_spending_gdp_g" = `Education government (g) spending (%GDP)`, 
         "recr_gov_spending_gdp_g" = `Recreation government (g) spending (%GDP)`, 
         "hous_gov_spending_gdp_g" = `Housing government (g) spending (%GDP)`, 
         "indu_gov_spending_gdp_g" = `Industry government (g) spending (%GDP)`, 
         "ord_gov_spending_gdp_g" = `Order government (g) spending (%GDP)`, 
         "def_gov_spending_gdp_g" = `Defense government (g) spending (%GDP)`, 
         "admin_gov_spending_gdp_g" = `Admin government (g) spending (%GDP)`,
         
         "total_gov_spending_cap_g" = `Total government (g) spending (per capita)`,
         "health_gov_spending_cap_g" = `Health government (g) spending (per capita)`, 
         "envi_gov_spending_cap_g" = `Environment government (g) spending (per capita)`, 
         "soc_gov_spending_cap_g" = `Social government (g) spending (per capita)`, 
         "edu_gov_spending_cap_g" = `Education government (g) spending (per capita)`, 
         "recr_gov_spending_cap_g" = `Recreation government (g) spending (per capita)`, 
         "hous_gov_spending_cap_g" = `Housing government (g) spending (per capita)`, 
         "indu_gov_spending_cap_g" = `Industry government (g) spending (per capita)`, 
         "ord_gov_spending_cap_g" = `Order government (g) spending (per capita)`, 
         "def_gov_spending_cap_g" = `Defense government (g) spending (per capita)`, 
         "admin_gov_spending_cap_g" = `Admin government (g) spending (per capita)`, 
         
         # --- Household spending (Gloria) ---
         "health_hh_spending_tot_g" = `Health household (g) spending (%Total)`,    
         "envi_hh_spending_tot_g" = `Environment household (g) spending (%Total)`,    
         "soc_hh_spending_tot_g" = `Social household (g) spending (%Total)`, 
         "edu_hh_spending_tot_g" = `Education household (g) spending (%Total)`, 
         "recr_hh_spending_tot_g" = `Recreation household (g) spending (%Total)`, 
         "hous_hh_spending_tot_g" = `Housing household (g) spending (%Total)`, 
         "indu_hh_spending_tot_g" = `Industry household (g) spending (%Total)`, 
         "ord_hh_spending_tot_g" = `Order household (g) spending (%Total)`, 
         "def_hh_spending_tot_g" = `Defense household (g) spending (%Total)`, 
         "admin_hh_spending_tot_g" = `Admin household (g) spending (%Total)`, 
         
         "total_hh_spending_gdp_g" = `Total household (g) spending (%GDP)` ,
         "health_hh_spending_gdp_g" = `Health household (g) spending (%GDP)`, 
         "envi_hh_spending_gdp_g" = `Environment household (g) spending (%GDP)`, 
         "soc_hh_spending_gdp_g" = `Social household (g) spending (%GDP)`, 
         "edu_hh_spending_gdp_g" = `Education household (g) spending (%GDP)`, 
         "recr_hh_spending_gdp_g" = `Recreation household (g) spending (%GDP)`, 
         "hous_hh_spending_gdp_g" = `Housing household (g) spending (%GDP)`, 
         "indu_hh_spending_gdp_g" = `Industry household (g) spending (%GDP)`, 
         "ord_hh_spending_gdp_g" = `Order household (g) spending (%GDP)`, 
         "def_hh_spending_gdp_g" = `Defense household (g) spending (%GDP)`, 
         "admin_hh_spending_gdp_g" = `Admin household (g) spending (%GDP)`,
         
         "total_hh_spending_cap_g" = `Total household (g) spending (per capita)`,
         "health_hh_spending_cap_g" = `Health household (g) spending (per capita)`, 
         "envi_hh_spending_cap_g" = `Environment household (g) spending (per capita)`, 
         "soc_hh_spending_cap_g" = `Social household (g) spending (per capita)`, 
         "edu_hh_spending_cap_g" = `Education household (g) spending (per capita)`, 
         "recr_hh_spending_cap_g" = `Recreation household (g) spending (per capita)`, 
         "hous_hh_spending_cap_g" = `Housing household (g) spending (per capita)`, 
         "indu_hh_spending_cap_g" = `Industry household (g) spending (per capita)`, 
         "ord_hh_spending_cap_g" = `Order household (g) spending (per capita)`, 
         "def_hh_spending_cap_g" = `Defense household (g) spending (per capita)`, 
         "admin_hh_spending_cap_g" = `Admin household (g) spending (per capita)`,
         
         # --- GHG Government ---
         "ghg_total_gov_cap_g" = `GHG Total (Government)`,
         "ghg_health_gov_cap_g" = `GHG Health (Government)`,
         "ghg_envi_gov_cap_g" = `GHG Environment (Government)`,
         "ghg_soc_gov_cap_g" = `GHG Social protection (Government)`, 
         "ghg_edu_gov_cap_g" = `GHG Education (Government)`, 
         "ghg_recr_gov_cap_g" = `GHG Recreation (Government)`, 
         "ghg_hous_gov_cap_g" = `GHG Housing (Government)`, 
         "ghg_indu_gov_cap_g" = `GHG Industry (Government)`, 
         "ghg_ord_gov_cap_g" = `GHG Order (Government)`,
         "ghg_def_gov_cap_g" = `GHG Defense (Government)`,
         "ghg_admin_gov_cap_g" = `GHG Admin (Government)`,
         
         # --- GHG Household ---
         "ghg_total_hh_cap_g" = `GHG Total (Household)`,
         "ghg_health_hh_cap_g" = `GHG Health (Household)`,
         "ghg_envi_hh_cap_g" = `GHG Environment (Household)`,
         "ghg_soc_hh_cap_g" = `GHG Social protection (Household)`, 
         "ghg_edu_hh_cap_g" = `GHG Education (Household)`, 
         "ghg_recr_hh_cap_g" = `GHG Recreation (Household)`, 
         "ghg_hous_hh_cap_g" = `GHG Housing (Household)`, 
         "ghg_indu_hh_cap_g" = `GHG Industry (Household)`, 
         "ghg_ord_hh_cap_g" = `GHG Order (Household)`,
         "ghg_def_hh_cap_g" = `GHG Defense (Household)`,
         "ghg_admin_hh_cap_g" = `GHG Admin (Household)`,
         
         # --- social thresholds ---
         "expect" = `Life expectancy`,
         "school" = `School years`,
         "settle" = `Formal settlement`, 
         "crime" = `Crime absence`, 
         "income" = `Minimum income`,
         "employ" = `Employment`, 
         "support" = `Social support`, 
         "nutri" = `Sufficient nutrition`, 
         "birth" = `Adolescent birth rate`, 
         "air" = `Clean air`,
         
         # --- Aux vars ---
         "pop" = `Population`,
         "popdens" = `Population density`,
         "urban" = `Urbanization`,
         
         "gdp" = `GDP`,
         "gdp_cap" = `GDP (per capita)`,
         
         "unequal" =  `Income inequality`,
         
         "rule" = `Rule of law`,
         "goveffect" = `Government effectiveness`, 
         "regulatory" = `Regulatory quality`, 
         "corrupt"  = `Corruption control`,
         
          
         "thresh_+1%"  = `Social thresholds achieved (+1%)`,
         "thresh_-1%"  = `Social thresholds achieved (-1%)`
  )  |>
  
  mutate(  thresh_level = factor(thresh_level)) |>
  
  filter(year >= 2005 & year <= 2019)  |>       
  group_by(name,year) |>

  mutate(                                                                           # get (adjusted) intensity data
    `total_intensity_gov_wid` =  (`ghg_total_gov_cap_g` / `total_gov_spending_cap_wid`),    # government (wid)
    `health_intensity_gov_wid` = (`ghg_health_gov_cap_g` / `health_gov_spending_cap_wid`),
    `envi_intensity_gov_wid` = (`ghg_envi_gov_cap_g` / `envi_gov_spending_cap_wid`),
    `soc_intensity_gov_wid` = (`ghg_soc_gov_cap_g` / `soc_gov_spending_cap_wid`),
    `edu_intensity_gov_wid` = (`ghg_edu_gov_cap_g` / `edu_gov_spending_cap_wid`),
    `recr_intensity_gov_wid` = (`ghg_recr_gov_cap_g` / `recr_gov_spending_cap_wid`),
    `hous_intensity_gov_wid` =  (`ghg_hous_gov_cap_g` / `hous_gov_spending_cap_wid`),
    `indu_intensity_gov_wid` = (`ghg_indu_gov_cap_g` / `indu_gov_spending_cap_wid`),
    `ord_intensity_gov_wid` = (`ghg_ord_gov_cap_g` / `ord_gov_spending_cap_wid`),
    `def_intensity_gov_wid` = (`ghg_def_gov_cap_g` / `def_gov_spending_cap_wid`),
    `admin_intensity_gov_wid` = (`ghg_admin_gov_cap_g` / `admin_gov_spending_cap_wid`),
    
    `total_intensity_gov_g` =  (`ghg_total_gov_cap_g` / `total_gov_spending_cap_g`),    # government (g)
    `health_intensity_gov_g` = (`ghg_health_gov_cap_g` / `health_gov_spending_cap_g`),
    `envi_intensity_gov_g` = (`ghg_envi_gov_cap_g` / `envi_gov_spending_cap_g`),
    `soc_intensity_gov_g` = (`ghg_soc_gov_cap_g` / `soc_gov_spending_cap_g`),
    `edu_intensity_gov_g` = (`ghg_edu_gov_cap_g` / `edu_gov_spending_cap_g`),
    `recr_intensity_gov_g` = (`ghg_recr_gov_cap_g` / `recr_gov_spending_cap_g`),
    `hous_intensity_gov_g` =  (`ghg_hous_gov_cap_g` / `hous_gov_spending_cap_g`),
    `indu_intensity_gov_g` = (`ghg_indu_gov_cap_g` / `indu_gov_spending_cap_g`),
    `ord_intensity_gov_g` = (`ghg_ord_gov_cap_g` / `ord_gov_spending_cap_g`),
    `def_intensity_gov_g` = (`ghg_def_gov_cap_g` / `def_gov_spending_cap_g`),
    `admin_intensity_gov_g` = (`ghg_admin_gov_cap_g` / `admin_gov_spending_cap_g`),
    
    `total_intensity_hh_g` =  (`ghg_total_hh_cap_g` / `total_hh_spending_cap_g`),    # household
    `health_intensity_hh_g` = (`ghg_health_hh_cap_g` / `health_hh_spending_cap_g`),
    `envi_intensity_hh_g` = (`ghg_envi_hh_cap_g` / `envi_hh_spending_cap_g`),
    `soc_intensity_hh_g` = (`ghg_soc_hh_cap_g` / `soc_hh_spending_cap_g`),
    `edu_intensity_hh_g` = (`ghg_edu_hh_cap_g` / `edu_hh_spending_cap_g`),
    `recr_intensity_hh_g` = (`ghg_recr_hh_cap_g` / `recr_hh_spending_cap_g`),
    `hous_intensity_hh_g` =  (`ghg_hous_hh_cap_g` / `hous_hh_spending_cap_g`),
    `indu_intensity_hh_g` = (`ghg_indu_hh_cap_g` / `indu_hh_spending_cap_g`),
    `ord_intensity_hh_g` = (`ghg_ord_hh_cap_g` / `ord_hh_spending_cap_g`),
    `def_intensity_hh_g` = (`ghg_def_hh_cap_g` / `def_hh_spending_cap_g`),
    `admin_intensity_hh_g` = (`ghg_admin_hh_cap_g` / `admin_hh_spending_cap_g`)
  )  |>
  
  mutate(
    
    `adj_total_intensity_gov_wid` = if_else(`carb_factor` > 1,   
                                        (`ghg_total_gov_cap_g` / `total_gov_spending_cap_wid`) * `carb_factor`,    # government (wid) data
                                        `ghg_total_gov_cap_g` / `total_gov_spending_cap_wid`),
    `adj_health_intensity_gov_wid` = if_else(`carb_factor` > 1, 
                                         (`ghg_health_gov_cap_g` / `health_gov_spending_cap_wid`) * `carb_factor`, 
                                         `ghg_health_gov_cap_g` / `health_gov_spending_cap_wid`),
    `adj_envi_intensity_gov_wid` = if_else(`carb_factor` > 1, 
                                           (`ghg_envi_gov_cap_g` / `envi_gov_spending_cap_wid`) * `carb_factor`, 
                                           `ghg_envi_gov_cap_g` / `envi_gov_spending_cap_wid`),
    `adj_soc_intensity_gov_wid` = if_else(`carb_factor` > 1, 
                                      (`ghg_soc_gov_cap_g` / `soc_gov_spending_cap_wid`) * `carb_factor`, 
                                      `ghg_soc_gov_cap_g` / `soc_gov_spending_cap_wid`),
    `adj_edu_intensity_gov_wid` = if_else(`carb_factor` > 1, 
                                      (`ghg_edu_gov_cap_g` / `edu_gov_spending_cap_wid`) * `carb_factor`, 
                                      `ghg_edu_gov_cap_g` / `edu_gov_spending_cap_wid`),
    `adj_recr_intensity_gov_wid` = if_else(`carb_factor` > 1, 
                                           (`ghg_recr_gov_cap_g` / `recr_gov_spending_cap_wid`) * `carb_factor`, 
                                           `ghg_recr_gov_cap_g` / `recr_gov_spending_cap_wid`),
    `adj_hous_intensity_gov_wid` = if_else(`carb_factor` > 1, 
                                       (`ghg_hous_gov_cap_g` / `hous_gov_spending_cap_wid`) * `carb_factor`, 
                                       `ghg_hous_gov_cap_g` / `hous_gov_spending_cap_wid`),
    `adj_indu_intensity_gov_wid` = if_else(`carb_factor` > 1, 
                                       (`ghg_indu_gov_cap_g` / `indu_gov_spending_cap_wid`) * `carb_factor`, 
                                       `ghg_indu_gov_cap_g` / `indu_gov_spending_cap_wid`),
    `adj_ord_intensity_gov_wid` =  if_else(`carb_factor` > 1, 
                                       (`ghg_ord_gov_cap_g` / `ord_gov_spending_cap_wid`) * `carb_factor`, 
                                       `ghg_ord_gov_cap_g` / `ord_gov_spending_cap_wid`),
    `adj_def_intensity_gov_wid` = if_else(`carb_factor` > 1, 
                                          (`ghg_def_gov_cap_g` / `def_gov_spending_cap_wid`) * `carb_factor`, 
                                          `ghg_def_gov_cap_g` / `def_gov_spending_cap_wid`),
    `adj_admin_intensity_gov_wid` =  if_else(`carb_factor` > 1, 
                                         (`ghg_admin_gov_cap_g` / `admin_gov_spending_cap_wid`) * `carb_factor`, 
                                         `ghg_admin_gov_cap_g` / `admin_gov_spending_cap_wid`),
    
    
    `adj_total_intensity_gov_g` = if_else(`carb_factor` > 1,   
                                        (`ghg_total_gov_cap_g` / `total_gov_spending_cap_g`) * `carb_factor`,    # government (g) data
                                        `ghg_total_gov_cap_g` / `total_gov_spending_cap_g`),
    `adj_health_intensity_gov_g` = if_else(`carb_factor` > 1, 
                                         (`ghg_health_gov_cap_g` / `health_gov_spending_cap_g`) * `carb_factor`, 
                                         `ghg_health_gov_cap_g` / `health_gov_spending_cap_g`),
    `adj_envi_intensity_gov_g` = if_else(`carb_factor` > 1, 
                                         (`ghg_envi_gov_cap_g` / `envi_gov_spending_cap_g`) * `carb_factor`, 
                                         `ghg_envi_gov_cap_g` / `envi_gov_spending_cap_g`),
    `adj_soc_intensity_gov_g` = if_else(`carb_factor` > 1, 
                                      (`ghg_soc_gov_cap_g` / `soc_gov_spending_cap_g`) * `carb_factor`, 
                                      `ghg_soc_gov_cap_g` / `soc_gov_spending_cap_g`),
    `adj_edu_intensity_gov_g` = if_else(`carb_factor` > 1, 
                                      (`ghg_edu_gov_cap_g` / `edu_gov_spending_cap_g`) * `carb_factor`, 
                                      `ghg_edu_gov_cap_g` / `edu_gov_spending_cap_g`),
    `adj_recr_intensity_gov_g` = if_else(`carb_factor` > 1, 
                                         (`ghg_recr_gov_cap_g` / `recr_gov_spending_cap_g`) * `carb_factor`, 
                                         `ghg_recr_gov_cap_g` / `recr_gov_spending_cap_g`),
    `adj_hous_intensity_gov_g` = if_else(`carb_factor` > 1, 
                                       (`ghg_hous_gov_cap_g` / `hous_gov_spending_cap_g`) * `carb_factor`, 
                                       `ghg_hous_gov_cap_g` / `hous_gov_spending_cap_g`),
    `adj_indu_intensity_gov_g` = if_else(`carb_factor` > 1, 
                                       (`ghg_indu_gov_cap_g` / `indu_gov_spending_cap_g`) * `carb_factor`, 
                                       `ghg_indu_gov_cap_g` / `indu_gov_spending_cap_g`),
    `adj_ord_intensity_gov_g` =  if_else(`carb_factor` > 1, 
                                       (`ghg_ord_gov_cap_g` / `ord_gov_spending_cap_g`) * `carb_factor`, 
                                       `ghg_ord_gov_cap_g` / `ord_gov_spending_cap_g`),
    `adj_def_intensity_gov_g` = if_else(`carb_factor` > 1, 
                                        (`ghg_def_gov_cap_g` / `def_gov_spending_cap_g`) * `carb_factor`, 
                                        `ghg_def_gov_cap_g` / `def_gov_spending_cap_g`),
    `adj_admin_intensity_gov_g` =  if_else(`carb_factor` > 1, 
                                         (`ghg_admin_gov_cap_g` / `admin_gov_spending_cap_g`) * `carb_factor`, 
                                         `ghg_admin_gov_cap_g` / `admin_gov_spending_cap_g`),
    
    `adj_total_intensity_hh_g` = if_else(`carb_factor` > 1,   
                                         (`ghg_total_hh_cap_g` / `total_hh_spending_cap_g`) * `carb_factor`,    # household data
                                         `ghg_total_hh_cap_g` / `total_hh_spending_cap_g`),
    `adj_health_intensity_hh_g` = if_else(`carb_factor` > 1, 
                                          (`ghg_health_hh_cap_g` / `health_hh_spending_cap_g`) * `carb_factor`, 
                                          `ghg_health_hh_cap_g` / `health_hh_spending_cap_g`),
    `adj_envi_intensity_hh_g` = if_else(`carb_factor` > 1, 
                                        (`ghg_envi_hh_cap_g` / `envi_hh_spending_cap_g`) * `carb_factor`, 
                                        `ghg_envi_hh_cap_g` / `envi_hh_spending_cap_g`),
    `adj_soc_intensity_hh_g` = if_else(`carb_factor` > 1, 
                                       (`ghg_soc_hh_cap_g` / `soc_hh_spending_cap_g`) * `carb_factor`, 
                                       `ghg_soc_hh_cap_g` / `soc_hh_spending_cap_g`),
    `adj_edu_intensity_hh_g` = if_else(`carb_factor` > 1, 
                                       (`ghg_edu_hh_cap_g` / `edu_hh_spending_cap_g`) * `carb_factor`, 
                                       `ghg_edu_hh_cap_g` / `edu_hh_spending_cap_g`),
    `adj_recr_intensity_hh_g` = if_else(`carb_factor` > 1, 
                                        (`ghg_recr_hh_cap_g` / `recr_hh_spending_cap_g`) * `carb_factor`, 
                                        `ghg_recr_hh_cap_g` / `recr_hh_spending_cap_g`),
    `adj_hous_intensity_hh_g` = if_else(`carb_factor` > 1, 
                                        (`ghg_hous_hh_cap_g` / `hous_hh_spending_cap_g`) * `carb_factor`, 
                                        `ghg_hous_hh_cap_g` / `hous_hh_spending_cap_g`),
    `adj_indu_intensity_hh_g` = if_else(`carb_factor` > 1, 
                                        (`ghg_indu_hh_cap_g` / `indu_hh_spending_cap_g`) * `carb_factor`, 
                                        `ghg_indu_hh_cap_g` / `indu_hh_spending_cap_g`),
    `adj_ord_intensity_hh_g` =  if_else(`carb_factor` > 1, 
                                        (`ghg_ord_hh_cap_g` / `ord_hh_spending_cap_g`) * `carb_factor`, 
                                        `ghg_ord_hh_cap_g` / `ord_hh_spending_cap_g`),
    `adj_def_intensity_hh_g` = if_else(`carb_factor` > 1, 
                                       (`ghg_def_hh_cap_g` / `def_hh_spending_cap_g`) * `carb_factor`, 
                                       `ghg_def_hh_cap_g` / `def_hh_spending_cap_g`),
    `adj_admin_intensity_hh_g` =  if_else(`carb_factor` > 1, 
                                          (`ghg_admin_hh_cap_g` / `admin_hh_spending_cap_g`) * `carb_factor`, 
                                          `ghg_admin_hh_cap_g` / `admin_hh_spending_cap_g`)

  ) |>
  
  mutate(                                                                       #get relative ghg data
      ghg_share_health_gov_g   = ghg_health_gov_cap_g / ghg_total_gov_cap_g,  # gov
      ghg_share_envi_gov_g   = ghg_envi_gov_cap_g / ghg_total_gov_cap_g,
      
      ghg_share_edu_gov_g   = ghg_edu_gov_cap_g / ghg_total_gov_cap_g,
      ghg_share_recr_gov_g   = ghg_recr_gov_cap_g / ghg_total_gov_cap_g,
      
      ghg_share_soc_gov_g   = ghg_soc_gov_cap_g / ghg_total_gov_cap_g,
      ghg_share_hous_gov_g   = ghg_hous_gov_cap_g / ghg_total_gov_cap_g,
      ghg_share_indu_gov_g   = ghg_indu_gov_cap_g / ghg_total_gov_cap_g,
      
      ghg_share_ord_gov_g   = ghg_ord_gov_cap_g / ghg_total_gov_cap_g,
      ghg_share_def_gov_g   = ghg_ord_gov_cap_g / ghg_total_gov_cap_g,
      
      ghg_share_admin_gov_g   = ghg_admin_gov_cap_g / ghg_total_gov_cap_g,
      
      
      ghg_share_health_hh_g   = ghg_health_hh_cap_g / ghg_total_hh_cap_g,    # hh
      ghg_share_envi_hh_g   = ghg_envi_hh_cap_g / ghg_total_hh_cap_g,
      
      ghg_share_edu_hh_g   = ghg_edu_hh_cap_g / ghg_total_hh_cap_g,
      ghg_share_recr_hh_g   = ghg_recr_hh_cap_g / ghg_total_hh_cap_g,
      
      ghg_share_soc_hh_g   = ghg_soc_hh_cap_g / ghg_total_hh_cap_g,
      ghg_share_hous_hh_g   = ghg_hous_hh_cap_g / ghg_total_hh_cap_g,
      ghg_share_indu_hh_g   = ghg_indu_hh_cap_g / ghg_total_hh_cap_g,
      
      ghg_share_ord_hh_g   = ghg_ord_hh_cap_g / ghg_total_hh_cap_g,
      ghg_share_def_hh_g   = ghg_ord_hh_cap_g / ghg_total_hh_cap_g,
      
      ghg_share_admin_hh_g   = ghg_admin_hh_cap_g / ghg_total_hh_cap_g) |>
      
      

  filter(if_all(matches("carb_factor|GHG|spending|threshold|gdp"), ~ !is.na(.x))) |> # remove all columns where any of the five is missing
  filter(!name %in% c("Yemen", "Sri Lanka", "Venezuela", "Zimbabwe")) |>
  mutate(
    `carb_overshoot` = as.factor(case_when(          
      `carb_factor` <= 1 ~ "None",
      `carb_factor` > 1 & `carb_factor` <= 2 ~ "More than 1 time",
      `carb_factor` > 2 & `carb_factor` <= 3 ~ "More than 2 times",
      `carb_factor` > 3 ~ "More than 3 times",
      TRUE ~ NA_character_
    ))) |>
  ungroup() 


#View(data_pooled_renamed)

data_sample <- data_pooled_renamed |>
  filter(!is.na(thresholds) & year == 2019 & !is.na(carb_factor)) |> 
  select(name) |>
  distinct()

#View(data_sample)
write_xlsx(data_sample, "Data/Overview/data_sample.xlsx")


# Keep only the countries with thresholds for 2019
data_pooled_renamed_filtered <- data_pooled_renamed  |>
  filter(name %in% data_sample$name) |>
  select(name, year, thresholds, thresh_level, carb_factor, carb_overshoot, 
         pop, popdens, gdp, gdp_cap, unequal, urban, goveffect, rule, corrupt, regulatory, everything()) |>
  arrange(name, year)

 
#View(data_pooled_renamed |> filter (year == 2019))
#unique(data_pooled_renamed_filtered$name)
#write_xlsx(data_pooled_renamed_filtered, "Data/Overview/data_pooled_cofog.xlsx")


### combine health & environment data, education & recreation and order & defense !!!

data_pooled_merged = data_pooled_renamed_filtered |>
  
  mutate(
         # --- Government spending (WID) ---
         health_envi_gov_spending_tot_wid = health_gov_spending_tot_wid + envi_gov_spending_tot_wid  ,   # %total
         edu_recr_gov_spending_tot_wid = edu_gov_spending_tot_wid + recr_gov_spending_tot_wid, 
         ord_def_gov_spending_tot_wid =  ord_gov_spending_tot_wid + def_gov_spending_tot_wid, 
         
         health_envi_gov_spending_gdp_wid = health_gov_spending_gdp_wid + envi_gov_spending_gdp_wid,  # %gdp
         edu_recr_gov_spending_gdp_wid = edu_gov_spending_gdp_wid + recr_gov_spending_gdp_wid,
         ord_def_gov_spending_gdp_wid = ord_gov_spending_gdp_wid + def_gov_spending_gdp_wid, 
         
         health_envi_gov_spending_cap_wid = health_gov_spending_cap_wid + envi_gov_spending_cap_wid, # cap
         edu_recr_gov_spending_cap_wid = edu_gov_spending_cap_wid + recr_gov_spending_cap_wid,
         ord_def_gov_spending_cap_wid = ord_gov_spending_cap_wid + def_gov_spending_cap_wid,
         
         # --- Government spending (Gloria) ---
         health_envi_gov_spending_tot_g = health_gov_spending_tot_g + envi_gov_spending_tot_g,  # %total
         edu_recr_gov_spending_tot_g = edu_gov_spending_tot_g + recr_gov_spending_tot_g,
         ord_def_gov_spending_tot_g = ord_gov_spending_tot_g + def_gov_spending_tot_g,
         
         health_envi_gov_spending_gdp_g = health_gov_spending_gdp_g + envi_gov_spending_gdp_g,  # %gdp
         edu_recr_gov_spending_gdp_g = edu_gov_spending_gdp_g + recr_gov_spending_gdp_g,
         ord_def_gov_spending_gdp_g = ord_gov_spending_gdp_g + def_gov_spending_gdp_g,
         
         health_envi_gov_spending_cap_g = health_gov_spending_cap_g + envi_gov_spending_cap_g,  # cap
         edu_recr_gov_spending_cap_g = edu_gov_spending_cap_g + recr_gov_spending_cap_g,
         ord_def_gov_spending_cap_g = ord_gov_spending_cap_g + def_gov_spending_cap_g,
         
         # --- Household spending (Gloria) ---
         health_envi_hh_spending_tot_g = health_hh_spending_tot_g + envi_hh_spending_tot_g,  # %total
         edu_recr_hh_spending_tot_g = edu_hh_spending_tot_g + recr_hh_spending_tot_g,
         ord_def_hh_spending_tot_g = ord_hh_spending_tot_g + def_hh_spending_tot_g,
         
         health_envi_hh_spending_gdp_g = health_hh_spending_gdp_g + envi_hh_spending_gdp_g,  # %gdp
         edu_recr_hh_spending_gdp_g = edu_hh_spending_gdp_g + recr_hh_spending_gdp_g,
         ord_def_hh_spending_gdp_g = ord_hh_spending_gdp_g + def_hh_spending_gdp_g,
         
         health_envi_hh_spending_cap_g = health_hh_spending_cap_g + envi_hh_spending_cap_g,  # cap
         edu_recr_hh_spending_cap_g = edu_hh_spending_cap_g + recr_hh_spending_cap_g,
         ord_def_hh_spending_cap_g = ord_hh_spending_cap_g + def_hh_spending_cap_g,
         
         # --- GHG spending (Gloria) ---                                      # absolute values
         ghg_health_envi_gov_cap_g = ghg_health_gov_cap_g + ghg_envi_gov_cap_g,  # gov
         ghg_edu_recr_gov_cap_g = ghg_edu_gov_cap_g + ghg_recr_gov_cap_g,
         ghg_ord_def_gov_cap_g = ghg_ord_gov_cap_g + ghg_def_gov_cap_g,
         
         ghg_health_envi_hh_cap_g = ghg_health_hh_cap_g + ghg_envi_hh_cap_g,  # hh 
         ghg_edu_recr_hh_cap_g = ghg_edu_hh_cap_g + ghg_recr_hh_cap_g,
         ghg_ord_def_hh_cap_g = ghg_ord_hh_cap_g + ghg_def_hh_cap_g  ,
         
                                                                              # relative values (shares)
  
         ghg_share_health_envi_gov_g = ghg_share_health_gov_g + ghg_share_envi_gov_g,  # gov
         ghg_share_edu_recr_gov_g = ghg_share_edu_gov_g + ghg_share_recr_gov_g,
         ghg_share_ord_def_gov_g = ghg_share_ord_gov_g + ghg_share_def_gov_g,
         
         ghg_share_health_envi_hh_g =   ghg_share_health_hh_g +  ghg_share_envi_hh_g  ,# hh 
         ghg_share_edu_recr_hh_g =  ghg_share_edu_hh_g +  ghg_share_recr_hh_g,
         ghg_share_ord_def_hh_g =  ghg_share_ord_hh_g +  ghg_share_def_hh_g)     |>
  
  
  mutate(   # get rates for weighting intensities
  
  health_rate_gov_wid = health_gov_spending_tot_wid / (health_gov_spending_tot_wid + envi_gov_spending_tot_wid ),  # wid gov data
  envi_rate_gov_wid = envi_gov_spending_tot_wid / (health_gov_spending_tot_wid + envi_gov_spending_tot_wid ),
  
  edu_rate_gov_wid = edu_gov_spending_tot_wid / (edu_gov_spending_tot_wid + recr_gov_spending_tot_wid ),
  recr_rate_gov_wid = recr_gov_spending_tot_wid / (edu_gov_spending_tot_wid + recr_gov_spending_tot_wid ),
  
  ord_rate_gov_wid = ord_gov_spending_tot_wid / (ord_gov_spending_tot_wid + def_gov_spending_tot_wid ),
  def_rate_gov_wid = def_gov_spending_tot_wid / (ord_gov_spending_tot_wid + def_gov_spending_tot_wid ),
  
  health_rate_gov_g = health_gov_spending_tot_g / (health_gov_spending_tot_g + envi_gov_spending_tot_g ),  # g gov data
  envi_rate_gov_g = envi_gov_spending_tot_g / (health_gov_spending_tot_g + envi_gov_spending_tot_g ),
  
  edu_rate_gov_g = edu_gov_spending_tot_g / (edu_gov_spending_tot_g + recr_gov_spending_tot_g ),
  recr_rate_gov_g = recr_gov_spending_tot_g / (edu_gov_spending_tot_g + recr_gov_spending_tot_g ),
  
  ord_rate_gov_g = ord_gov_spending_tot_g / (ord_gov_spending_tot_g + def_gov_spending_tot_g ),
  def_rate_gov_g = def_gov_spending_tot_g / (ord_gov_spending_tot_g + def_gov_spending_tot_g ) ,
  
  health_rate_hh_g = health_hh_spending_tot_g / (health_hh_spending_tot_g + envi_hh_spending_tot_g ),  # g hh data
  envi_rate_hh_g = envi_hh_spending_tot_g / (health_hh_spending_tot_g + envi_hh_spending_tot_g ),
  
  edu_rate_hh_g = edu_hh_spending_tot_g / (edu_hh_spending_tot_g + recr_hh_spending_tot_g ),
  recr_rate_hh_g = recr_hh_spending_tot_g / (edu_hh_spending_tot_g + recr_hh_spending_tot_g ),
  
  ord_rate_hh_g = ord_hh_spending_tot_g / (ord_hh_spending_tot_g + def_hh_spending_tot_g ),
  def_rate_hh_g = def_hh_spending_tot_g / (ord_hh_spending_tot_g + def_hh_spending_tot_g )
  
  ) |>
  
  mutate(
    
    # gov intensities with WID spending data              #### weighting based on spending shares
    health_envi_intensity_gov_wid =  health_rate_gov_wid * health_intensity_gov_wid + envi_rate_gov_wid *envi_intensity_gov_wid, 
    edu_recr_intensity_gov_wid = edu_rate_gov_wid * edu_intensity_gov_wid + recr_rate_gov_wid * recr_intensity_gov_wid,
    ord_def_intensity_gov_wid = ord_rate_gov_wid * ord_intensity_gov_wid + def_rate_gov_wid * def_intensity_gov_wid, 
    
    # gov intensities with Gloria spending data 
    health_envi_intensity_gov_g = health_rate_gov_g * health_intensity_gov_g + envi_rate_gov_g * envi_intensity_gov_g, 
    edu_recr_intensity_gov_g = edu_rate_gov_g * edu_intensity_gov_g + recr_rate_gov_g * recr_intensity_gov_g,
    ord_def_intensity_gov_g = ord_rate_gov_g * ord_intensity_gov_g + def_rate_gov_g * def_intensity_gov_g, 
    
    # hh intensities with Gloria spending data 
    health_envi_intensity_hh_g = health_rate_hh_g * health_intensity_hh_g + envi_rate_hh_g * envi_intensity_hh_g, 
    edu_recr_intensity_hh_g = edu_rate_hh_g * edu_intensity_hh_g + recr_rate_hh_g * recr_intensity_hh_g,
    ord_def_intensity_hh_g = ord_rate_hh_g * ord_intensity_hh_g + def_rate_hh_g * def_intensity_hh_g, 
    
    
    # adj gov intensities with WID spending data
    adj_health_envi_intensity_gov_wid = health_rate_gov_wid * adj_health_intensity_gov_wid + envi_rate_gov_wid * adj_envi_intensity_gov_wid, 
    adj_edu_recr_intensity_gov_wid = edu_rate_gov_wid * adj_edu_intensity_gov_wid + recr_rate_gov_wid * adj_recr_intensity_gov_wid,
    adj_ord_def_intensity_gov_wid = ord_rate_gov_wid * adj_ord_intensity_gov_wid + def_rate_gov_wid * adj_def_intensity_gov_wid, 
    
    # adj gov intensities with Gloria spending data 
    adj_health_envi_intensity_gov_g = health_rate_gov_g * adj_health_intensity_gov_g + envi_rate_gov_g * adj_envi_intensity_gov_g, 
    adj_edu_recr_intensity_gov_g = edu_rate_gov_g * adj_edu_intensity_gov_g + recr_rate_gov_g * adj_recr_intensity_gov_g,
    adj_ord_def_intensity_gov_g = ord_rate_gov_g * adj_ord_intensity_gov_g + def_rate_gov_g * adj_def_intensity_gov_g, 
    
    # adj hh intensities with Gloria spending data 
    adj_health_envi_intensity_hh_g = health_rate_hh_g * adj_health_intensity_hh_g + envi_rate_hh_g * adj_envi_intensity_hh_g, 
    adj_edu_recr_intensity_hh_g = edu_rate_hh_g * adj_edu_intensity_hh_g + recr_rate_hh_g * adj_recr_intensity_hh_g,
    adj_ord_def_intensity_hh_g = ord_rate_hh_g * adj_ord_intensity_hh_g + def_rate_hh_g * adj_def_intensity_hh_g    )  |>
  
  select(name, year, thresholds, thresh_level, carb_factor, carb_overshoot, 
         pop, popdens, gdp, gdp_cap, unequal, urban, rule, goveffect, regulatory, corrupt,
         expect, school, birth, income, nutri, settle, crime, employ, support, air, 
         contains("spending_tot_wid"), contains("spending_gdp_wid"), contains("spending_cap_wid"), 
         contains("spending_tot_g"), contains("spending_gdp_g"), contains("spending_cap_g"),
         contains("intensity_gov_wid"), contains("intensity_gov_g"), contains("intensity_hh_g"),
         contains("ghg_"),
         everything(),  -contains("rate")) |>
  arrange(name, year)
         
#View(data_pooled_merged |> filter(year == 2019) )
write_xlsx(data_pooled_merged, "Data/Overview/data_pooled.xlsx")
write_xlsx(data_pooled_merged, "Data/SI/data_pooled.xlsx")


 
