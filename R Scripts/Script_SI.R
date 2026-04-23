
#### plots and data for supplementary information (si)

# load packages
library(tidyverse)
library(readxl)
library(openxlsx)
library(plm)
library(viridis)
library(scales)
library(writexl)


# load data
data_pooled = read_excel("Data/SI/data_pooled.xlsx")

#View(data_pooled)


#### si table 1. done in word

#### si table 2. done in word

#### si figure 1. done in python. see: "Analysis/SI/Script_SI_figure3.py"


##### si table 3. done in python. see Analysis/SI/table3.csv


#### si table 4: ghg data per cap

data_ghg_cap <- read_excel("Data/Overview/data_pooled.xlsx")  |> # ghg data already in kg and per capita

  select(name, year, thresh_level, contains("ghg_"), pop, contains("gov"), -contains("hh"),
        -ghg_health_gov_cap_g, -ghg_envi_gov_cap_g, -ghg_edu_gov_cap_g, -ghg_recr_gov_cap_g, -ghg_ord_gov_cap_g, -ghg_def_gov_cap_g ) |>
  
  pivot_longer(cols = -c("name", "year", "thresh_level"), names_to = "type", values_to = "value") |>
  arrange(year) |>
  pivot_wider(names_from = "year", values_from = "value") |>
  group_by(thresh_level, type) |>
  summarise(
    across(`2005`:`2019`, 
           list(mean = ~mean(.x, na.rm = TRUE), 
                sd = ~sd(.x, na.rm = TRUE)),  
           .names = "{col}_{fn}"),
    .groups = "drop"
  )  |>

  filter(str_detect(type, "ghg") & str_detect(type, "gov"))   |> # Keep only ghg by gov 
  
  mutate(
    type = str_replace(type, "ghg_health_envi_gov_cap_g", "Health & Environment"), 
    type = str_replace(type, "ghg_edu_recr_gov_cap_g", "Education & Recreation"),
    type = str_replace(type, "ghg_ord_def_gov_cap_g", "Order & Defense"),
    type = str_replace(type, "ghg_hous_gov_cap_g", "Housing"),
    type = str_replace(type, "ghg_soc_gov_cap_g", "Social Protection"),
    type = str_replace(type, "ghg_indu_gov_cap_g", "Industry"),
    type = str_replace(type, "ghg_admin_gov_cap_g", "Public Administration"),
    type = str_replace(type, "ghg_total_gov_cap_g", "Total"),  
    type = factor(type, levels = c( "Education & Recreation","Social Protection", "Public Administration", "Order & Defense",
                                    "Health & Environment",  "Housing", "Industry", "Total")),
    thresh_level = str_replace(thresh_level, " outcome", ""),  # Clean 'thresh_level' column
    thresh_level = factor(thresh_level, levels = c("High", "Upper-middle", "Lower-middle", "Low"))  # Order levels
  ) |>
  ungroup() |>
  rowwise() |>
  select(thresh_level, type, contains("_mean"), contains("_sd")) |>


  mutate(mean = mean(c_across(`2005_mean`:`2019_mean`), na.rm = TRUE),
         sd = mean(c_across(`2005_sd`:`2019_sd`), na.rm = TRUE),
         rsd = (sd / mean) * 100,   ) |>    # Relative SD   # pick a year or get average across period
  ungroup() |>
  select(thresh_level, type, mean, sd, rsd) |>
  group_by(type)  |>
  mutate(sd_sector = mean(sd),
         rsd_sector = mean(rsd)) |> # Average RSD for each sector 
  select(thresh_level, type, mean) |>
  pivot_wider(names_from = thresh_level, values_from = mean) |>
  select(type, High, `Upper-middle`, `Lower-middle`, Low) |>
  arrange(type) |>
  mutate(across(c(High, `Upper-middle`, `Lower-middle`, Low), ~ scales::comma(.x, accuracy = 0.01)))


#View(data_ghg_cap)
write_xlsx(data_ghg_cap, "Analysis/SI/table4_ghg_cap.xlsx")


#### si figure 2: conversion options - how to split up social protection, public order and national defense
# show three different heat maps with different social protection and order & defense adjusted intensities

# option 1
data_heat1 = read_excel("Data/SI/data_pooled_option1.xlsx") |>
  select(name, year, thresh_level, contains("spending_cap"), contains("ghg"), -contains("log"), carb_factor, contains("intensity")) |>
  group_by(thresh_level) |>
  summarise(
    total_intensity_gov_g = median(total_intensity_gov_g, na.rm = TRUE),    # median better for expontential intensity data
    health_envi_intensity_gov_g = median(health_envi_intensity_gov_g, na.rm = TRUE),
    soc_intensity_gov_g = median(soc_intensity_gov_g, na.rm = TRUE),
    edu_recr_intensity_gov_g = median(edu_recr_intensity_gov_g, na.rm = TRUE),
    hous_intensity_gov_g = median(hous_intensity_gov_g, na.rm = TRUE),
    indu_intensity_gov_g = median(indu_intensity_gov_g, na.rm = TRUE),
    ord_def_intensity_gov_g = median(ord_def_intensity_gov_g, na.rm = TRUE),
    admin_intensity_gov_g = median(admin_intensity_gov_g, na.rm = TRUE),
    
    adj_total_intensity_gov_g = median(adj_total_intensity_gov_g, na.rm = TRUE),
    adj_health_envi_intensity_gov_g = median(adj_health_envi_intensity_gov_g, na.rm = TRUE),
    adj_soc_intensity_gov_g = median(adj_soc_intensity_gov_g, na.rm = TRUE),
    adj_edu_recr_intensity_gov_g = median(adj_edu_recr_intensity_gov_g, na.rm = TRUE),
    adj_hous_intensity_gov_g = median(adj_hous_intensity_gov_g, na.rm = TRUE),
    adj_indu_intensity_gov_g = median(adj_indu_intensity_gov_g, na.rm = TRUE),
    adj_ord_def_intensity_gov_g = median(adj_ord_def_intensity_gov_g, na.rm = TRUE),
    adj_admin_intensity_gov_g = median(adj_admin_intensity_gov_g, na.rm = TRUE),
    
    total_intensity_hh_g = median(total_intensity_hh_g, na.rm = TRUE),
    health_envi_intensity_hh_g = median(health_envi_intensity_hh_g, na.rm = TRUE),
    soc_intensity_hh_g = median(soc_intensity_hh_g, na.rm = TRUE),
    edu_recr_intensity_hh_g = median(edu_recr_intensity_hh_g, na.rm = TRUE),
    hous_intensity_hh_g = median(hous_intensity_hh_g, na.rm = TRUE),
    indu_intensity_hh_g = median(indu_intensity_hh_g, na.rm = TRUE),
    ord_def_intensity_hh_g = median(ord_def_intensity_hh_g, na.rm = TRUE),
    admin_intensity_hh_g = median(admin_intensity_hh_g, na.rm = TRUE),
    
    adj_total_intensity_hh_g = median(adj_total_intensity_hh_g, na.rm = TRUE),
    adj_health_envi_intensity_hh_g = median(adj_health_envi_intensity_hh_g, na.rm = TRUE),
    adj_soc_intensity_hh_g = median(adj_soc_intensity_hh_g, na.rm = TRUE),
    adj_edu_recr_intensity_hh_g = median(adj_edu_recr_intensity_hh_g, na.rm = TRUE),
    adj_hous_intensity_hh_g = median(adj_hous_intensity_hh_g, na.rm = TRUE),
    adj_indu_intensity_hh_g = median(adj_indu_intensity_hh_g, na.rm = TRUE),
    adj_ord_def_intensity_hh_g = median(adj_ord_def_intensity_hh_g, na.rm = TRUE),
    adj_admin_intensity_hh_g =  median(adj_admin_intensity_hh_g, na.rm = TRUE)
  ) |>
  
  ungroup() |>
  mutate(
    thresh_level = str_replace(thresh_level, " outcome", ""),  # Clean 'thresh_level' column
    thresh_level = factor(thresh_level, levels = c("Low", "Lower-middle", "Upper-middle", "High")), 
    across(contains("intensity"), ~ .x * 1000)  # Convert kg to g for all intensity columns
  ) |>
  
  select(thresh_level,  contains("adj"), -contains("hh")) |>
  
  select(-contains("total")) |>
  group_by(thresh_level) |>
  summarise(across(adj_health_envi_intensity_gov_g:adj_admin_intensity_gov_g, mean, na.rm = TRUE)) |>
  pivot_longer(cols = adj_health_envi_intensity_gov_g:adj_admin_intensity_gov_g, names_to = "sector", values_to = "adj_intensity_gov_g") |>
  mutate(sector = str_replace(sector, "adj_health_envi_intensity_gov_g", "Health & Environment"),
         sector = str_replace(sector, "adj_soc_intensity_gov_g", "Social Protection"),
         sector = str_replace(sector, "adj_edu_recr_intensity_gov_g", "Education & Recreation"),
         sector = str_replace(sector, "adj_hous_intensity_gov_g", "Housing"),
         sector = str_replace(sector, "adj_indu_intensity_gov_g", "Industry"),
         sector = str_replace(sector, "adj_ord_def_intensity_gov_g", "Order & Defense"),
         sector = str_replace(sector, "adj_admin_intensity_gov_g", "Public Administration")) |>
  
  mutate(sector = factor(sector, levels = rev(c(  "Education & Recreation",
                                              "Social Protection", 
                                              "Public Administration", 
                                              "Order & Defense", 
                                              "Health & Environment",
                                              "Housing", 
                                              "Industry"))) )

# plot
x_axis =  expression(bold(paste("Adjusted carbon intensity of government spending [g CO"[bold("2")]*"e / US$]")))

ggplot(data_heat1, aes(x = sector, y = thresh_level, fill = adj_intensity_gov_g)) +
  geom_tile(color = "white", linetype = "solid", size = 0.2) +  # Thin borders for tiles
  geom_text(
    aes(label = format(round(adj_intensity_gov_g, 1), scientific = FALSE)),  # Format intensity values
    color = "black", size = 6, family = "Times New Roman", fontface = "bold"
  ) + 
  scale_fill_viridis(
    direction = 1,
    name = "",
    trans = "log",  # Apply logarithmic scale
    breaks = c(1, 10, 100, 1000, 10000),  # Adjusted breaks
    labels = function(x) {
      # Format labels based on the breaks
      sapply(x, function(value) {
        if (value < 1) {
          format(value, nsmall = 1, scientific = FALSE)  # Format smaller values with 1 decimal place
        } else {
          format(value, scientific = FALSE)  # Format values with no scientific notation
        }
      })
    },
    limits = c(0.1, 50000),  # Adjusted limits .  lowest values = 0.54
    guide = guide_colorbar(
      barwidth = 40, barheight = 0.5, direction = "horizontal"  # Customize legend dimensions
    )
  ) +
  labs(
    x = x_axis,
    y = ""
  ) + 
  theme_minimal(base_size = 14) +  # Base size for text
  theme(
    text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),   # X-axis title bold
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16),  # Rotated X-axis labels
    axis.text.y = element_text(angle = 45, hjust = 1, size = 16),  # Y-axis labels
    legend.position = "bottom",  # Legend on the bottom
    legend.title = element_text(size = 18, face = "bold", vjust = 3, hjust = 2, family = "Times New Roman"),  # Bigger and bold legend title
    
    legend.text = element_text(size = 16), 
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()
  )

ggsave(
  file = "Analysis/SI/figure2_heat_option1.png", 
  plot = last_plot(),  
  width = 8, height = 12, units = "in", bg = "transparent", dpi = 300
)


# option 2
data_heat2 = read_excel("Data/SI/data_pooled_option2.xlsx") |>
  select(name, year, thresh_level, contains("spending_cap"), contains("ghg"), -contains("log"), carb_factor, contains("intensity")) |>
  group_by(thresh_level) |>
  summarise(
    total_intensity_gov_g = median(total_intensity_gov_g, na.rm = TRUE),    # median better for expontential intensity data
    health_envi_intensity_gov_g = median(health_envi_intensity_gov_g, na.rm = TRUE),
    soc_intensity_gov_g = median(soc_intensity_gov_g, na.rm = TRUE),
    edu_recr_intensity_gov_g = median(edu_recr_intensity_gov_g, na.rm = TRUE),
    hous_intensity_gov_g = median(hous_intensity_gov_g, na.rm = TRUE),
    indu_intensity_gov_g = median(indu_intensity_gov_g, na.rm = TRUE),
    ord_def_intensity_gov_g = median(ord_def_intensity_gov_g, na.rm = TRUE),
    admin_intensity_gov_g = median(admin_intensity_gov_g, na.rm = TRUE),
    
    adj_total_intensity_gov_g = median(adj_total_intensity_gov_g, na.rm = TRUE),
    adj_health_envi_intensity_gov_g = median(adj_health_envi_intensity_gov_g, na.rm = TRUE),
    adj_soc_intensity_gov_g = median(adj_soc_intensity_gov_g, na.rm = TRUE),
    adj_edu_recr_intensity_gov_g = median(adj_edu_recr_intensity_gov_g, na.rm = TRUE),
    adj_hous_intensity_gov_g = median(adj_hous_intensity_gov_g, na.rm = TRUE),
    adj_indu_intensity_gov_g = median(adj_indu_intensity_gov_g, na.rm = TRUE),
    adj_ord_def_intensity_gov_g = median(adj_ord_def_intensity_gov_g, na.rm = TRUE),
    adj_admin_intensity_gov_g = median(adj_admin_intensity_gov_g, na.rm = TRUE),
    
    total_intensity_hh_g = median(total_intensity_hh_g, na.rm = TRUE),
    health_envi_intensity_hh_g = median(health_envi_intensity_hh_g, na.rm = TRUE),
    soc_intensity_hh_g = median(soc_intensity_hh_g, na.rm = TRUE),
    edu_recr_intensity_hh_g = median(edu_recr_intensity_hh_g, na.rm = TRUE),
    hous_intensity_hh_g = median(hous_intensity_hh_g, na.rm = TRUE),
    indu_intensity_hh_g = median(indu_intensity_hh_g, na.rm = TRUE),
    ord_def_intensity_hh_g = median(ord_def_intensity_hh_g, na.rm = TRUE),
    admin_intensity_hh_g = median(admin_intensity_hh_g, na.rm = TRUE),
    
    adj_total_intensity_hh_g = median(adj_total_intensity_hh_g, na.rm = TRUE),
    adj_health_envi_intensity_hh_g = median(adj_health_envi_intensity_hh_g, na.rm = TRUE),
    adj_soc_intensity_hh_g = median(adj_soc_intensity_hh_g, na.rm = TRUE),
    adj_edu_recr_intensity_hh_g = median(adj_edu_recr_intensity_hh_g, na.rm = TRUE),
    adj_hous_intensity_hh_g = median(adj_hous_intensity_hh_g, na.rm = TRUE),
    adj_indu_intensity_hh_g = median(adj_indu_intensity_hh_g, na.rm = TRUE),
    adj_ord_def_intensity_hh_g = median(adj_ord_def_intensity_hh_g, na.rm = TRUE),
    adj_admin_intensity_hh_g =  median(adj_admin_intensity_hh_g, na.rm = TRUE)
  ) |>
  
  ungroup() |>
  mutate(
    thresh_level = str_replace(thresh_level, " outcome", ""),  # Clean 'thresh_level' column
    thresh_level = factor(thresh_level, levels = c("Low", "Lower-middle", "Upper-middle", "High")), 
    across(contains("intensity"), ~ .x * 1000)  # Convert kg to g for all intensity columns
  ) |>
  
  select(thresh_level,  contains("adj"), -contains("hh")) |>
  
  select(-contains("total")) |>
  group_by(thresh_level) |>
  summarise(across(adj_health_envi_intensity_gov_g:adj_admin_intensity_gov_g, mean, na.rm = TRUE)) |>
  pivot_longer(cols = adj_health_envi_intensity_gov_g:adj_admin_intensity_gov_g, names_to = "sector", values_to = "adj_intensity_gov_g") |>
  mutate(sector = str_replace(sector, "adj_health_envi_intensity_gov_g", "Health & Environment"),
         sector = str_replace(sector, "adj_soc_intensity_gov_g", "Social Protection"),
         sector = str_replace(sector, "adj_edu_recr_intensity_gov_g", "Education & Recreation"),
         sector = str_replace(sector, "adj_hous_intensity_gov_g", "Housing"),
         sector = str_replace(sector, "adj_indu_intensity_gov_g", "Industry"),
         sector = str_replace(sector, "adj_ord_def_intensity_gov_g", "Order & Defense"),
         sector = str_replace(sector, "adj_admin_intensity_gov_g", "Public Administration")) |>
  
  mutate(sector = factor(sector, levels = rev(c(  "Education & Recreation",
                                              "Social Protection", 
                                              "Public Administration", 
                                              "Order & Defense", 
                                              "Health & Environment",
                                              "Housing", 
                                              "Industry"))) )

# plot
x_axis =  expression(bold(paste("Adjusted carbon intensity of government spending [g CO"[bold("2")]*"e / US$]")))

ggplot(data_heat2, aes(x = sector, y = thresh_level, fill = adj_intensity_gov_g)) +
  geom_tile(color = "white", linetype = "solid", size = 0.2) +  # Thin borders for tiles
  geom_text(
    aes(label = format(round(adj_intensity_gov_g, 1), scientific = FALSE)),  # Format intensity values
    color = "black", size = 6, family = "Times New Roman", fontface = "bold"
  ) + 
  scale_fill_viridis(
    direction = 1,
    name = "",
    trans = "log",  # Apply logarithmic scale
    breaks = c(1, 10, 100, 1000, 10000),  # Adjusted breaks
    labels = function(x) {
      # Format labels based on the breaks
      sapply(x, function(value) {
        if (value < 1) {
          format(value, nsmall = 1, scientific = FALSE)  # Format smaller values with 1 decimal place
        } else {
          format(value, scientific = FALSE)  # Format values with no scientific notation
        }
      })
    },
    limits = c(0.1, 50000),  # Adjusted limits .  lowest values = 0.54
    guide = guide_colorbar(
      barwidth = 40, barheight = 0.5, direction = "horizontal"  # Customize legend dimensions
    )
  ) +
  labs(
    x = x_axis,
    y = ""
  ) + 
  theme_minimal(base_size = 14) +  # Base size for text
  theme(
    text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),   # X-axis title bold
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16),  # Rotated X-axis labels
    axis.text.y = element_text(angle = 45, hjust = 1, size = 16),  # Y-axis labels
    legend.position = "bottom",  # Legend on the bottom
    legend.title = element_text(size = 18, face = "bold", vjust = 3, hjust = 2, family = "Times New Roman"),  # Bigger and bold legend title
    
    legend.text = element_text(size = 16), 
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()
  )

ggsave(
  file = "Analysis/SI/figure2_heat_option2.png", 
  plot = last_plot(),  
  width = 8, height = 12, units = "in", bg = "transparent", dpi = 300
)


# option 3 

data_heat_option3 = read_excel("Data/SI/data_pooled_option3.xlsx") |>
  select(name, year, thresh_level, contains("spending_cap"), contains("ghg"), -contains("log"), carb_factor, contains("intensity")) |>
  group_by(thresh_level) |>
  summarise(
    total_intensity_gov_g = median(total_intensity_gov_g, na.rm = TRUE),    # median better for expontential intensity data
    health_envi_intensity_gov_g = median(health_envi_intensity_gov_g, na.rm = TRUE),
    soc_intensity_gov_g = median(soc_intensity_gov_g, na.rm = TRUE),
    edu_recr_intensity_gov_g = median(edu_recr_intensity_gov_g, na.rm = TRUE),
    hous_intensity_gov_g = median(hous_intensity_gov_g, na.rm = TRUE),
    indu_intensity_gov_g = median(indu_intensity_gov_g, na.rm = TRUE),
    ord_def_intensity_gov_g = median(ord_def_intensity_gov_g, na.rm = TRUE),
    admin_intensity_gov_g = median(admin_intensity_gov_g, na.rm = TRUE),
    
    adj_total_intensity_gov_g = median(adj_total_intensity_gov_g, na.rm = TRUE),
    adj_health_envi_intensity_gov_g = median(adj_health_envi_intensity_gov_g, na.rm = TRUE),
    adj_soc_intensity_gov_g = median(adj_soc_intensity_gov_g, na.rm = TRUE),
    adj_edu_recr_intensity_gov_g = median(adj_edu_recr_intensity_gov_g, na.rm = TRUE),
    adj_hous_intensity_gov_g = median(adj_hous_intensity_gov_g, na.rm = TRUE),
    adj_indu_intensity_gov_g = median(adj_indu_intensity_gov_g, na.rm = TRUE),
    adj_ord_def_intensity_gov_g = median(adj_ord_def_intensity_gov_g, na.rm = TRUE),
    adj_admin_intensity_gov_g = median(adj_admin_intensity_gov_g, na.rm = TRUE),
    
    total_intensity_hh_g = median(total_intensity_hh_g, na.rm = TRUE),
    health_envi_intensity_hh_g = median(health_envi_intensity_hh_g, na.rm = TRUE),
    soc_intensity_hh_g = median(soc_intensity_hh_g, na.rm = TRUE),
    edu_recr_intensity_hh_g = median(edu_recr_intensity_hh_g, na.rm = TRUE),
    hous_intensity_hh_g = median(hous_intensity_hh_g, na.rm = TRUE),
    indu_intensity_hh_g = median(indu_intensity_hh_g, na.rm = TRUE),
    ord_def_intensity_hh_g = median(ord_def_intensity_hh_g, na.rm = TRUE),
    admin_intensity_hh_g = median(admin_intensity_hh_g, na.rm = TRUE),
    
    adj_total_intensity_hh_g = median(adj_total_intensity_hh_g, na.rm = TRUE),
    adj_health_envi_intensity_hh_g = median(adj_health_envi_intensity_hh_g, na.rm = TRUE),
    adj_soc_intensity_hh_g = median(adj_soc_intensity_hh_g, na.rm = TRUE),
    adj_edu_recr_intensity_hh_g = median(adj_edu_recr_intensity_hh_g, na.rm = TRUE),
    adj_hous_intensity_hh_g = median(adj_hous_intensity_hh_g, na.rm = TRUE),
    adj_indu_intensity_hh_g = median(adj_indu_intensity_hh_g, na.rm = TRUE),
    adj_ord_def_intensity_hh_g = median(adj_ord_def_intensity_hh_g, na.rm = TRUE),
    adj_admin_intensity_hh_g =  median(adj_admin_intensity_hh_g, na.rm = TRUE)
  ) |>
  
  ungroup() |>
  mutate(
    thresh_level = str_replace(thresh_level, " outcome", ""),  # Clean 'thresh_level' column
    thresh_level = factor(thresh_level, levels = c("Low", "Lower-middle", "Upper-middle", "High")), 
    across(contains("intensity"), ~ .x * 1000)  # Convert kg to g for all intensity columns
  ) |>
  
  select(thresh_level,  contains("adj"), -contains("hh")) |>
  
  select(-contains("total")) |>
  group_by(thresh_level) |>
  summarise(across(adj_health_envi_intensity_gov_g:adj_admin_intensity_gov_g, mean, na.rm = TRUE)) |>
  pivot_longer(cols = adj_health_envi_intensity_gov_g:adj_admin_intensity_gov_g, names_to = "sector", values_to = "adj_intensity_gov_g") |>
  mutate(sector = str_replace(sector, "adj_health_envi_intensity_gov_g", "Health & Environment"),
         sector = str_replace(sector, "adj_soc_intensity_gov_g", "Social Protection"),
         sector = str_replace(sector, "adj_edu_recr_intensity_gov_g", "Education & Recreation"),
         sector = str_replace(sector, "adj_hous_intensity_gov_g", "Housing"),
         sector = str_replace(sector, "adj_indu_intensity_gov_g", "Industry"),
         sector = str_replace(sector, "adj_ord_def_intensity_gov_g", "Order & Defense"),
         sector = str_replace(sector, "adj_admin_intensity_gov_g", "Public Administration")) |>
  
  mutate(sector = factor(sector, levels = rev(c(  "Education & Recreation",
                                              "Social Protection", 
                                              "Public Administration", 
                                              "Order & Defense", 
                                              "Health & Environment",
                                              "Housing", 
                                              "Industry"))) )

# plotting option 3 (our approach)

x_axis =  expression(bold(paste("Adjusted carbon intensity of government spending [g CO"[bold("2")]*"e / US$]")))

ggplot(data_heat_option3, aes(x = sector, y = thresh_level, fill = adj_intensity_gov_g)) +
  geom_tile(color = "white", linetype = "solid", size = 0.2) +  # Thin borders for tiles
  geom_text(
    aes(label = format(round(adj_intensity_gov_g, 1), scientific = FALSE)),  # Format intensity values
    color = "black", size = 6, family = "Times New Roman", fontface = "bold"
  ) + 
  scale_fill_viridis(
    direction = 1,
    name = "",
    trans = "log",  # Apply logarithmic scale
    breaks = c(1, 10, 100, 1000, 10000),  # Adjusted breaks
    labels = function(x) {
      # Format labels based on the breaks
      sapply(x, function(value) {
        if (value < 1) {
          format(value, nsmall = 1, scientific = FALSE)  # Format smaller values with 1 decimal place
        } else {
          format(value, scientific = FALSE)  # Format values with no scientific notation
        }
      })
    },
    limits = c(0.1, 50000),  # Adjusted limits .  lowest values = 0.54
    guide = guide_colorbar(
      barwidth = 40, barheight = 0.5, direction = "horizontal"  # Customize legend dimensions
    )
  ) +
  labs(
    x = x_axis,
    y = ""
  ) + 
  theme_minimal(base_size = 14) +  # Base size for text
  theme(
    text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),   # X-axis title bold
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16),  # Rotated X-axis labels
    axis.text.y = element_text(angle = 45, hjust = 1, size = 16),  # Y-axis labels
    legend.position = "bottom",  # Legend on the bottom
    legend.title = element_text(size = 18, face = "bold", vjust = 3, hjust = 2, family = "Times New Roman"),  # Bigger and bold legend title
    
    legend.text = element_text(size = 16), 
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()
  )

ggsave(
  file = "Analysis/SI/figure2_heat_option3.png", 
  plot = last_plot(),  
  width = 8, height = 12, units = "in", bg = "transparent", dpi = 300
)


#### si table 5. done in word


#### si figure 3 
data_figure3 <- read_excel("Data/Overview/data_pooled.xlsx") |>
  mutate(
    year = factor(
      year, 
      levels = rev(c("2005", "2006", "2007", "2008", "2009", 
                     "2010", "2011", "2012", "2013", "2014", 
                     "2015", "2016", "2017", "2018", "2019"))
    )  # Reverse the levels for proper stacking and legend order
  )

ggplot(data_figure3, aes(x = thresholds)) +
  geom_histogram(
    aes(fill = year), 
    binwidth = 1, 
    color = "grey80", 
    size = 0.2
  ) +
  labs(
    title = "",
    x = "Social Thresholds Achieved (0 - 10)",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 18, base_family = "Times New Roman") +
  theme(
    panel.grid = element_blank(),
   # plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 22, face = "bold"),
    legend.text = element_text(size = 18)
  ) +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_viridis_d(name = "Year", option = "D", direction = -1)  + # "D" option is close to your original colors
  
  guides(
    fill = guide_legend(reverse = F)  # Reverse the legend order
  )


ggsave(filename = "Analysis/SI/figure3_thresholds_distribution.png", plot = last_plot(), width = 12, height = 10, units = "in", bg = "transparent", dpi = 300)



##### si figure 4: spending shares of original cofog typology
# data for si figure 4a
data_distribution <- read_excel("Data/Overview/data_pooled.xlsx") |>    
  select(name, year, thresh_level, contains("_tot"), contains("_wid"), 
         -contains("adj_"), -contains("_intensity"),  -contains("hh"),  -contains("_cap"), -contains("_gdp")) |>
  select(-contains("tot_g"), -contains("GHG")) |>
  select(-health_envi_gov_spending_tot_wid, -edu_recr_gov_spending_tot_wid, -ord_def_gov_spending_tot_wid) |>
  
  pivot_longer(cols = -c("name", "year", "thresh_level"), names_to = "type", values_to = "value") |>
  arrange(year) |>
  pivot_wider(names_from = "year", values_from = "value") |>
  
  group_by(thresh_level, type) |>
  summarise(
    across(`2005`:`2019`, 
           list(mean = ~mean(.x, na.rm = TRUE), 
                sd = ~sd(.x, na.rm = TRUE)),
           .names = "{col}_{fn}"),
    .groups = "drop"
  ) |>
  
  mutate(
    type = str_replace(type, "health", "Health"),
    type = str_replace(type, "envi", "Environmental Protection"),
    type = str_replace(type, "edu", "Education"),
    type = str_replace(type, "recr", "Recreation"),
    type = str_replace(type, "ord", "Order"),
    type = str_replace(type, "def", "Defense"),
    type = str_replace(type, "hous", "Housing"),
    type = str_replace(type, "soc", "Social Protection"),
    type = str_replace(type, "indu", "Industry"),
    type = str_replace(type, "admin", "Public Administration"),
    type = str_replace(type, "_gov_spending_tot_wid", ""),  
    type = factor(type, levels = c( "Industry","Order",  "Defense", "Public Administration", "Housing", 
                                    "Education",  "Recreation",  "Health",  "Environmental Protection", "Social Protection")), 
    thresh_level = str_replace(thresh_level, " outcome", ""),  # Clean 'thresh_level' column
    thresh_level = factor(thresh_level, levels = c("High", "Upper-middle", "Lower-middle", "Low"))  # Order levels
  ) |>
  
  ungroup() |>
  rowwise() |>
  select(thresh_level, type, contains("_mean"), contains("_sd"), contains("lower_p"), contains("upper_p")) |>
  mutate(mean = mean(c_across(`2005_mean`:`2019_mean`), na.rm = TRUE),
         sd = mean(c_across(`2005_sd`:`2019_sd`), na.rm = TRUE),
         rsd = (sd / mean) * 100) |>   # Relative SD
  ungroup() |>
  select(thresh_level, type, mean, sd, rsd) |>
  group_by(type)  |>
  mutate(sd_sector = mean(sd),
         rsd_sector = mean(rsd))  # Average RSD for each sector

#View(data_distribution)

# data for si figure 4
data_figure4 = data_distribution |>
  select(thresh_level, type, mean)

# plot
ggplot(data = data_figure4, aes(x = `thresh_level`, y = `mean`, fill = `type`)) +
  geom_bar(stat = "identity", color = "black") +  # Use percent deviation values for the bars
  labs(title = "",
       x = "Level of social outcome",
       y = "Share of total government spending [%]",
       fill = "Spending category") + 
  # coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(labels = function(x) round(x * 100, 0)) +
  geom_text(aes(label = paste0(round(mean * 100, 1), "")), 
            position = position_stack(vjust = 0.5), 
            size = 8, color = "black", fontface = "bold", family = "Times New Roman") + 
  
  scale_fill_viridis(discrete = TRUE, option = "D", direction = -1) +  # Using "D" for the Viridis palette
  
  
  theme_bw(base_size = 15) +
  # annotate("text", x = 4.35, y = 95, label = "A", color = "black", size = 20, fontface = "bold")  +
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),  # Transparent panel background
        plot.background = element_rect(fill = "transparent", color = NA), 
        axis.title.x = element_text(size = 22, family = "Times New Roman", face = "bold", vjust = -1),  # Set x-axis label font
        axis.title.y = element_text(size = 22, family = "Times New Roman", face = "bold"),
        axis.text.x = element_text(size = 20, family = "Times New Roman"),   # Set x-axis tick label font
        axis.text.y = element_text(size = 20, family = "Times New Roman"),
        legend.title = element_text(size = 19, face = "bold", family = "Times New Roman"),
        legend.text = element_text(size = 15, family = "Times New Roman"),
        legend.background = element_rect(fill = "transparent", color = NA), 
        legend.position = "bottom") 

ggsave(file = "Analysis/SI/figure4a_shares_cofog.png", plot = last_plot(), width = 12, height = 8, units = "in")


# si figure 4b: spending shares. (from manuscript)
data_distribution <- read_excel("Data/Overview/data_pooled.xlsx") |>    
  select(name, year, thresh_level, contains("_tot"), contains("_wid"), 
         -contains("adj_"), -contains("_intensity"),  -contains("hh"),  -contains("_cap"), -contains("_gdp")) |>
  select(-contains("tot_g"), -contains("GHG")) |>
  select(-health_gov_spending_tot_wid, -envi_gov_spending_tot_wid, -edu_gov_spending_tot_wid, -recr_gov_spending_tot_wid, -ord_gov_spending_tot_wid, -def_gov_spending_tot_wid) |>
  
  pivot_longer(cols = -c("name", "year", "thresh_level"), names_to = "type", values_to = "value") |>
  arrange(year) |>
  pivot_wider(names_from = "year", values_from = "value") |>
  
  group_by(thresh_level, type) |>
  summarise(
    across(`2005`:`2019`, 
           list(mean = ~mean(.x, na.rm = TRUE), 
                sd = ~sd(.x, na.rm = TRUE)),
           .names = "{col}_{fn}"),
    .groups = "drop"
  ) |>
  
  mutate(
    type = str_replace(type, "health_envi", "Health & Environment"),
    type = str_replace(type, "edu_recr", "Education & Recreation"),
    type = str_replace(type, "ord_def", "Order & Defense"),
    type = str_replace(type, "hous", "Housing"),
    type = str_replace(type, "soc", "Social Protection"),
    type = str_replace(type, "indu", "Industry"),
    type = str_replace(type, "admin", "Public Administration"),
    type = str_replace(type, "_gov_spending_tot_wid", ""),  
    type = factor(type, levels = c( "Industry","Order & Defense", "Public Administration", "Housing", 
                                    "Education & Recreation",  "Health & Environment", "Social Protection")), 
    thresh_level = str_replace(thresh_level, " outcome", ""),  # Clean 'thresh_level' column
    thresh_level = factor(thresh_level, levels = c("High", "Upper-middle", "Lower-middle", "Low"))  # Order levels
  ) |>
  
  ungroup() |>
  rowwise() |>
  select(thresh_level, type, contains("_mean"), contains("_sd"), contains("lower_p"), contains("upper_p")) |>
  mutate(mean = mean(c_across(`2005_mean`:`2019_mean`), na.rm = TRUE),
         sd = mean(c_across(`2005_sd`:`2019_sd`), na.rm = TRUE),
         rsd = (sd / mean) * 100) |>   # Relative SD
  ungroup() |>
  select(thresh_level, type, mean, sd, rsd) |>
  group_by(type)  |>
  mutate(sd_sector = mean(sd),
         rsd_sector = mean(rsd))  # Average RSD for each sector
#View(data_distribution)

# data for figure 4b
data_figure4b = data_distribution |>
  select(thresh_level, type, mean)

# plot
ggplot(data = data_figure4b, aes(x = `thresh_level`, y = `mean`, fill = `type`)) +
  geom_bar(stat = "identity", color = "black") +  # Use percent deviation values for the bars
  labs(title = "",
       x = "Level of social outcome",
       y = "Share of total government spending [%]",
       fill = "Spending category") + 
  # coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(labels = function(x) round(x * 100, 0)) +
  geom_text(aes(label = paste0(round(mean * 100, 1), "")),  
            position = position_stack(vjust = 0.5), 
            size = 8, color = "black", fontface = "bold", family = "Times New Roman") + 
  
  scale_fill_manual(values = c(
    "Industry" = "#FDE725",  # Bright yellow 
    "Order & Defense" = "#90D743",  # Yellow-green 
    "Public Administration" = "#35B779",  # Green 
    "Housing" = "#21918C",  # Teal 
    "Education & Recreation" = "#31688E",  # Blue 
    "Health & Environment" = "#3B528B",  # Indigo 
    "Social Protection" =  "#443983")) +  # light purple
  
  theme_bw(base_size = 15) +
  # annotate("text", x = 4.35, y = 95, label = "A", color = "black", size = 20, fontface = "bold")  +
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),  # Transparent panel background
        plot.background = element_rect(fill = "transparent", color = NA), 
        axis.title.x = element_text(size = 22, family = "Times New Roman", face = "bold", vjust = -1),  # Set x-axis label font
        axis.title.y = element_text(size = 22, family = "Times New Roman", face = "bold"),
        axis.text.x = element_text(size = 20, family = "Times New Roman"),   # Set x-axis tick label font
        axis.text.y = element_text(size = 20, family = "Times New Roman"),
        legend.title = element_text(size = 19, face = "bold", family = "Times New Roman"),
        legend.text = element_text(size = 15, family = "Times New Roman"),
        legend.background = element_rect(fill = "transparent", color = NA), 
        legend.position = "bottom") 


ggsave(file = "Analysis/SI/figure4b_shares_wid_merged.png", plot = last_plot(), width = 12, height = 8, units = "in")





### running regressions for si table 6 - 11
# using model design as in manuscript 


# load data
data_regressions = read_excel("Data/Overview/data_pooled.xlsx")  |> #mutate(across(.cols = contains("gov_spending_tot_g"), .fns = ~ . * 100))
  mutate(total_gov_spending_cap_g_1000 = total_gov_spending_cap_g / 1000,
         gdp_cap_1000 = gdp_cap / 1000,
         popdens_1000 = popdens / 1000) |>
  
  select(name, year, thresholds, expect:air, contains("spending_tot_g"),
         total_gov_spending_cap_g_1000, corrupt, popdens_1000,
         gdp_cap_1000, rule,
         goveffect, total_gov_spending_cap_wid, unequal, regulatory ) 


##### regressions
# converting for regressions
data_regressions_plm <- pdata.frame(data_regressions,  index = c( "year" ))    # name controls too much
# time fixed control for economic crisis

#### adapt accoridngly

# gov
model_gov_g <- plm(             #     
  thresholds ~ soc_gov_spending_tot_g  +          
    gdp_cap_1000  +  # alternatives: total_gov_spending_cap_g_1000, total gov_spending_cap_wid
    corrupt +        # alternatives: goveffect, rule, regulatory
    popdens_1000    # alternatives: urban
  ,                 # additional controls: unequal
  model = "within", 
  data = data_regressions_plm          
)                               
summary(model_gov_g)


# hh
model_hh_g <- plm(             
  thresholds ~   edu_recr_hh_spending_tot_g  +   
    gdp_cap_1000  + 
    corrupt +  
    popdens_1000
  ,       
  model = "within",              
  data = data_regressions_plm             
)                               
summary(model_hh_g)





#### additional information not included in si ######

### figure si 5. heat maps using the cofog typology

## comparison: government vs household intensities with cofog typology

#average data used
data_heat = read_excel("Data/Overview/data_pooled.xlsx") |>
  select(name, year, thresh_level, contains("spending_cap"), contains("ghg"), -contains("log"), carb_factor, contains("intensity"),
         -contains("health_envi"), -contains("edu_recr"), -contains("ord_def")) |>
  group_by(thresh_level) |>
  summarise(
    total_intensity_gov_g = median(total_intensity_gov_g, na.rm = TRUE),    # median or mean?!  currenlty, median
    health_intensity_gov_g = median(health_intensity_gov_g, na.rm = TRUE),
    envi_intensity_gov_g = median(envi_intensity_gov_g, na.rm = TRUE),
    soc_intensity_gov_g = median(soc_intensity_gov_g, na.rm = TRUE),
    edu_intensity_gov_g = median(edu_intensity_gov_g, na.rm = TRUE),
    recr_intensity_gov_g = median(recr_intensity_gov_g, na.rm = TRUE),
    hous_intensity_gov_g = median(hous_intensity_gov_g, na.rm = TRUE),
    indu_intensity_gov_g = median(indu_intensity_gov_g, na.rm = TRUE),
    ord_intensity_gov_g = median(ord_intensity_gov_g, na.rm = TRUE),
    def_intensity_gov_g = median(def_intensity_gov_g, na.rm = TRUE),
    admin_intensity_gov_g = median(admin_intensity_gov_g, na.rm = TRUE),
    
    adj_total_intensity_gov_g = median(adj_total_intensity_gov_g, na.rm = TRUE),
    adj_health_intensity_gov_g = median(adj_health_intensity_gov_g, na.rm = TRUE),
    adj_envi_intensity_gov_g = median(adj_envi_intensity_gov_g, na.rm = TRUE),
    adj_soc_intensity_gov_g = median(adj_soc_intensity_gov_g, na.rm = TRUE),
    adj_edu_intensity_gov_g = median(adj_edu_intensity_gov_g, na.rm = TRUE),
    adj_recr_intensity_gov_g = median(adj_recr_intensity_gov_g, na.rm = TRUE),
    adj_hous_intensity_gov_g = median(adj_hous_intensity_gov_g, na.rm = TRUE),
    adj_indu_intensity_gov_g = median(adj_indu_intensity_gov_g, na.rm = TRUE),
    adj_ord_intensity_gov_g = median(adj_ord_intensity_gov_g, na.rm = TRUE),
    adj_def_intensity_gov_g = median(adj_def_intensity_gov_g, na.rm = TRUE),
    adj_admin_intensity_gov_g = median(adj_admin_intensity_gov_g, na.rm = TRUE),
    
    total_intensity_hh_g = median(total_intensity_hh_g, na.rm = TRUE),
    health_intensity_hh_g = median(health_intensity_hh_g, na.rm = TRUE),
    envi_intensity_hh_g = median(envi_intensity_hh_g, na.rm = TRUE),
    soc_intensity_hh_g = median(soc_intensity_hh_g, na.rm = TRUE),
    edu_intensity_hh_g = median(edu_intensity_hh_g, na.rm = TRUE),
    recr_intensity_hh_g = median(recr_intensity_hh_g, na.rm = TRUE),
    hous_intensity_hh_g = median(hous_intensity_hh_g, na.rm = TRUE),
    indu_intensity_hh_g = median(indu_intensity_hh_g, na.rm = TRUE),
    ord_intensity_hh_g = median(ord_intensity_hh_g, na.rm = TRUE),
    def_intensity_hh_g = median(def_intensity_hh_g, na.rm = TRUE),
    admin_intensity_hh_g = median(admin_intensity_hh_g, na.rm = TRUE),
    
    adj_total_intensity_hh_g = median(adj_total_intensity_hh_g, na.rm = TRUE),
    adj_health_intensity_hh_g = median(adj_health_intensity_hh_g, na.rm = TRUE),
    adj_envi_intensity_hh_g = median(adj_envi_intensity_hh_g, na.rm = TRUE),
    adj_soc_intensity_hh_g = median(adj_soc_intensity_hh_g, na.rm = TRUE),
    adj_edu_intensity_hh_g = median(adj_edu_intensity_hh_g, na.rm = TRUE),
    adj_recr_intensity_hh_g = median(adj_recr_intensity_hh_g, na.rm = TRUE),
    adj_hous_intensity_hh_g = median(adj_hous_intensity_hh_g, na.rm = TRUE),
    adj_indu_intensity_hh_g = median(adj_indu_intensity_hh_g, na.rm = TRUE),
    adj_ord_intensity_hh_g = median(adj_ord_intensity_hh_g, na.rm = TRUE),
    adj_def_intensity_hh_g = median(adj_def_intensity_hh_g, na.rm = TRUE),
    adj_admin_intensity_hh_g =  median(adj_admin_intensity_hh_g, na.rm = TRUE)
  ) |>
  
  ungroup() |>
  mutate(
    thresh_level = str_replace(thresh_level, " outcome", ""),  # Clean 'thresh_level' column
    thresh_level = factor(thresh_level, levels = c("Low", "Lower-middle", "Upper-middle", "High")), 
    across(contains("intensity"), ~ .x * 1000)  # Convert kg to g for all intensity columns
  ) |>
  
  mutate( 
    total_factor = adj_total_intensity_hh_g / adj_total_intensity_gov_g,
    health_factor = adj_health_intensity_hh_g / adj_health_intensity_gov_g,
    envi_factor = adj_envi_intensity_hh_g / adj_envi_intensity_gov_g,
    soc_factor = adj_soc_intensity_hh_g / adj_soc_intensity_gov_g,
    edu_factor = adj_edu_intensity_hh_g / adj_edu_intensity_gov_g,
    recr_factor = adj_recr_intensity_hh_g / adj_recr_intensity_gov_g,
    hous_factor = adj_hous_intensity_hh_g / adj_hous_intensity_gov_g,
    indu_factor = adj_indu_intensity_hh_g / adj_indu_intensity_gov_g,
    ord_factor = adj_ord_intensity_hh_g / adj_ord_intensity_gov_g,
    def_factor = adj_def_intensity_hh_g / adj_def_intensity_gov_g,
    admin_factor =  adj_admin_intensity_hh_g / adj_admin_intensity_gov_g
  ) |>
  
  select(thresh_level, contains("factor"), contains("adj"))

#View(data_heat)


#### heat maps: 1x gov, 1x hh 

## government (gloria). using adjusted intensities
data_gov_heat_adj <- data_heat |>
  select(-contains("total")) |>
  group_by(thresh_level) |>
  summarise(across(adj_health_intensity_gov_g:adj_admin_intensity_gov_g, mean, na.rm = TRUE)) |>
  pivot_longer(cols = adj_health_intensity_gov_g:adj_admin_intensity_gov_g, names_to = "sector", values_to = "adj_intensity_gov_g") |>
  mutate(sector = str_replace(sector, "adj_health_intensity_gov_g", "Health"),
         sector = str_replace(sector, "adj_envi_intensity_gov_g", "Environment"),
         sector = str_replace(sector, "adj_soc_intensity_gov_g", "Social Protection"),
         sector = str_replace(sector, "adj_edu_intensity_gov_g", "Education"),
         sector = str_replace(sector, "adj_recr_intensity_gov_g", "Recreation"),
         sector = str_replace(sector, "adj_hous_intensity_gov_g", "Housing"),
         sector = str_replace(sector, "adj_indu_intensity_gov_g", "Industry"),
         sector = str_replace(sector, "adj_ord_intensity_gov_g", "Order"),
         sector = str_replace(sector, "adj_def_intensity_gov_g", "Defense"),
         sector = str_replace(sector, "adj_admin_intensity_gov_g", "Public Administration")) |>
  
  mutate(sector = factor(sector, levels = rev(c(  "Education",
                                                  "Social Protection", 
                                                  "Public Administration",    
                                                  "Health",
                                                  "Recreation",
                                                  "Order", 
                                                  "Defense", 
                                                  "Housing", 
                                                  "Environment",
                                                  "Industry"))) )

# plot 
#View(data_gov_heat_adj)
x_axis =  expression(bold(paste("Adjusted carbon intensity of government spendings [g CO"[bold("2")]*"e / US$]")))

ggplot(data_gov_heat_adj, aes(x = sector, y = thresh_level, fill = adj_intensity_gov_g)) +
  geom_tile(color = "grey40", linetype = "solid", size = 0.5) +  # Thin borders for tiles
  geom_text(
    aes(label = format(round(adj_intensity_gov_g, 1), scientific = FALSE)),  # Format intensity values
    color = "black", size = 6, family = "Times New Roman", fontface = "bold"
  ) + 
  scale_fill_viridis(
    direction = 1,
    name = "",
    trans = "log",  # Apply logarithmic scale
    breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000),  # Adjusted breaks
    labels = function(x) {
      # Format labels based on the breaks
      sapply(x, function(value) {
        if (value < 1) {
          format(value, nsmall = 1, scientific = FALSE)  # Format smaller values with 1 decimal place
        } else {
          format(value, scientific = FALSE)  # Format values with no scientific notation
        }
      })
    },
    limits = c(0.1, 10000000),  # Adjusted limits .  lowest values = 0.54
    guide = guide_colorbar(
      barwidth = 40, barheight = 0.5, direction = "horizontal"  # Customize legend dimensions
    )
  ) +
  labs(
    x = x_axis,
    y = ""
  ) + 
  theme_minimal(base_size = 14) +  # Base size for text
  theme(
    text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
    axis.title.y = element_text(size = 20, margin = margin(r = 10), face = "bold"),   
    axis.title.x = element_text(size = 20, margin = margin(t = 10)),   # X-axis title bold
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16),  # Rotated X-axis labels
    axis.text.y = element_text(angle = 45, hjust = 1, size = 16),  # Y-axis labels
    legend.position = "bottom",  # Legend on the bottom
    legend.title = element_text(size = 18, face = "bold", vjust = 3, hjust = 2, family = "Times New Roman"),  # Bigger and bold legend title
    legend.text = element_text(size = 16), 
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()
  )

ggsave(
  file = "Analysis/SI/figure5a_heat.png", 
  plot = last_plot(),  
  width = 12, height = 12, units = "in", bg = "transparent", dpi = 300
)


#### household / private (GLORIA)

#  heat map using adjusted data
data_hh_heat_adj <- data_heat |>
  select(-contains("total")) |>
  group_by(thresh_level) |>
  summarise(across(adj_health_intensity_hh_g:adj_admin_intensity_hh_g, mean, na.rm = TRUE)) |>
  pivot_longer(cols = adj_health_intensity_hh_g:adj_admin_intensity_hh_g, names_to = "sector", values_to = "adj_intensity_hh_g") |>
  mutate(sector = str_replace(sector, "adj_health_intensity_hh_g", "Health"),
         sector = str_replace(sector, "adj_envi_intensity_hh_g", "Environmental Protection"),
         sector = str_replace(sector, "adj_soc_intensity_hh_g", "Social Protection"),
         sector = str_replace(sector, "adj_edu_intensity_hh_g", "Education"),
         sector = str_replace(sector, "adj_recr_intensity_hh_g", "Recreation"),
         sector = str_replace(sector, "adj_hous_intensity_hh_g", "Housing"),
         sector = str_replace(sector, "adj_indu_intensity_hh_g", "Industry"),
         sector = str_replace(sector, "adj_ord_intensity_hh_g", "Order"),
         sector = str_replace(sector, "adj_def_intensity_hh_g", "Defense"),
         sector = str_replace(sector, "adj_admin_intensity_hh_g", "Public Administration")) |>
  
  mutate(sector = factor(sector, levels = rev(c(  "Education",
                                                  "Social Protection", 
                                                  "Public Administration",    
                                                  "Health",
                                                  "Recreation",
                                                  "Order", 
                                                  "Defense", 
                                                  "Housing", 
                                                  "Environmental Protection",
                                                  "Industry"))) )  |>
  
  filter(sector %in% c("Education", "Recreation", "Social Protection", "Health", "Environmental Protection"))



# plot 
#View(data_hh_heat_adj)
x_axis =  expression(bold(paste("Adjusted carbon intensity of household spendings [g CO"[bold("2")]*"e / US$]")))

ggplot(data_hh_heat_adj, aes(x = sector, y = thresh_level, fill = adj_intensity_hh_g)) +
  geom_tile(color = "grey", linetype = "solid", size = 0.5) +  # Thin borders for tiles
  
  geom_text(
    aes(
      label = ifelse(is.na(adj_intensity_hh_g), "NA", format(round(adj_intensity_hh_g, 1), scientific = FALSE)),  # Replace NA with "NA"
      hjust = ifelse(is.na(adj_intensity_hh_g), 0.7, 0.5)  # Shift "NA" a bit to the left
    ),
    color = "black", size = 6, family = "Times New Roman", fontface = "bold"
  ) +
  
  scale_fill_viridis(
    direction = 1,
    name = "",
    trans = "log",  # Apply logarithmic scale
    breaks = c(1,10, 100, 1000, 10000, 100000, 1000000), # Adjusted breaks  # c(10, 100, 1000, 10000, 100000),
    labels = function(x) {
      # Format labels based on the breaks
      sapply(x, function(value) {
        if (value < 1) {
          format(value, nsmall = 1, scientific = FALSE)  # Format smaller values with 1 decimal place
        } else {
          format(value, scientific = FALSE)  # Format values with no scientific notation
        }
      })
    },
    limits = c(0.1, 10000000),  # Adjusted limits .   # c(5, 1000000),
    guide = guide_colorbar(
      barwidth = 40, barheight = 0.5, direction = "horizontal"  # Customize legend dimensions
    )
  ) +
  labs(
    x = x_axis,
    y = ""
  ) + 
  theme_minimal(base_size = 14) +  # Base size for text
  theme(
    text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
    axis.title.y = element_text(size = 20, margin = margin(r = 10), face = "bold"),   
    axis.title.x = element_text(size = 20, margin = margin(t = 10)),   # X-axis title bold
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16),  # Rotated X-axis labels
    axis.text.y = element_text(angle = 45, hjust = 1, size = 16),  # Y-axis labels
    legend.position = "bottom",  # Legend on the bottom
    legend.title = element_text(size = 18, face = "bold", vjust = 3, hjust = 2, family = "Times New Roman"),  # Bigger and bold legend title
    
    legend.text = element_text(size = 16), 
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()
  ) 


ggsave(
  file = "Analysis/SI/figure5b_heat.png", 
  plot = last_plot(),  
  width = 12, height = 12, units = "in", bg = "transparent", dpi = 300
)




