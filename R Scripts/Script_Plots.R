#  plotting

library(tidyverse)
library(sf)
library(readxl)
library(writexl)
library(ggthemes)  
library(ggpattern) #install.packages("ggtext")
library(ggtext)
library(ggrepel)
library(scales)
library(ggbreak) # install.packages("ggbreak")
library(ggalt)
library(RColorBrewer)
library(viridis)
library(openxlsx) #install.packages("openxlsx")
library(ggforce) # install.packages("ggforce")



# load data

data_pooled = read_excel("Data/Overview/data_pooled.xlsx")
data_sample = read_excel("Data/Overview/data_sample.xlsx")


#View(data_pooled)
# get threshold data for panel

data_thresholds_series <- data_pooled |>

  mutate(
    period = case_when(
      year >= 2005 & year <= 2011 ~ "2011",     
      year >= 2012 & year <= 2018 ~ "2018",
      year >= 2019 & year <= 2019 ~ "2019"
      )
  ) |>
  group_by(name, period) |>
 
  summarize(
    `total_gov_spending_cap_wid_avg` = mean(`total_gov_spending_cap_wid`, na.rm = TRUE),
    `total_intensity_gov_g_avg` = mean(`total_intensity_gov_g`, na.rm = TRUE),    
    `adj_total_intensity_gov_g_avg` = mean(`adj_total_intensity_gov_g`, na.rm = TRUE), 
    
    `total_hh_spending_cap_g_avg` = mean(`total_hh_spending_cap_g`, na.rm = TRUE),
    `total_intensity_hh_g_avg` = mean(`total_intensity_hh_g`, na.rm = TRUE),    
    `adj_total_intensity_hh_g_avg` = mean(`adj_total_intensity_hh_g`, na.rm = TRUE), 
    
    `thresholds_avg` = mean(`thresholds`, na.rm = TRUE),
    `pop_avg` = mean(pop, na.rm = TRUE),  
    `carb_factor_avg` = mean(`carb_factor`, na.rm = TRUE),

    `ghg_total_gov_cap_g_avg`=  mean(`ghg_total_gov_cap_g`, na.rm = T), 
    `ghg_total_hh_cap_g_avg`=  mean(`ghg_total_hh_cap_g`, na.rm = T) , .groups = "drop" ) |>
  
  mutate(
    `carb_overshoot_avg` = as.factor(case_when(          
      `carb_factor_avg` <= 1 ~ "None",
      `carb_factor_avg` > 1 & `carb_factor_avg` <= 2 ~ "More than 1 time",
      `carb_factor_avg` > 2 & `carb_factor_avg` <= 3 ~ "More than 2 times",
      `carb_factor_avg` > 3 ~ "More than 3 times",
      TRUE ~ NA_character_
    ))) |>
  
  group_by(name, period) |>

    mutate(
    
    # Cluster into low, medium, high thresholds
    `thresh_level_avg` = as.factor(case_when(
      `thresholds_avg` >= 0 & `thresholds_avg` <= 3 ~ "Low outcome",
      `thresholds_avg` >= 3 & `thresholds_avg` <= 6 ~ "Lower-middle outcome",
      `thresholds_avg` >= 6 & `thresholds_avg` <= 8 ~ "Upper-middle outcome",
      `thresholds_avg` > 8 & `thresholds_avg` <= 10 ~ "High outcome",
      
      TRUE ~ NA_character_  # Handle unexpected cases
    ))
  ) |>
  mutate(  `carb_overshoot_avg` = factor(`carb_overshoot_avg`,  # to get legend in plot right
                                          levels =  c("None", "More than 1 time", "More than 2 times", "More than 3 times"))) |>  
  filter(!is.na(`carb_overshoot_avg`)  ) |>    
  select(`name`, period, `carb_factor_avg`, `carb_overshoot_avg`, thresholds_avg, thresh_level_avg, everything())


#View(data_thresholds_series)

## continue from here
 
# clean data
data_thresholds_series <- na.omit(data_thresholds_series)
#unique(data_thresholds_series$`name`)

#View(data_thresholds_series)

data_figure1 = data_thresholds_series |>
  select(-contains("hh"), - contains("ghg")) |>
  group_by(`name`) |>
  filter(any(period == 2019)) |> # Include only countries with data for 2019
  ungroup() 
#View(data_figure1 |> filter(period == 2019))

#View(data_pooled)
###### figure 1a
ggplot(data_figure1) +
  geom_hline(yintercept = 3, color = "grey", linetype = "dashed", size = 0.5, alpha = 0.7) +
  geom_hline(yintercept = 6, color = "grey", linetype = "dashed", size = 0.5, alpha = 0.7) +
  geom_hline(yintercept = 8, color = "grey", linetype = "dashed", size = 0.5, alpha = 0.7) +
  # Adjusted quadrant labels (swap x and y positions)
  annotate(
    "text", x = 59000, y = 9.2, label = "High", 
    size = 5, family = "Times New Roman", color = "black", fontface = "bold"
  ) +
  annotate(
    "text", x = 59000, y = 7, label = "Upper-middle", 
    size = 5, family = "Times New Roman", color = "black", fontface = "bold"
  ) +
  annotate(
    "text", x = 59000, y = 4.5, label = "Lower-middle", 
    size = 5, family = "Times New Roman",  color = "black", fontface = "bold"
  ) +
  annotate(
    "text", x = 59000, y = 1.5, label = "Low", 
    size = 5, family = "Times New Roman", color = "black", fontface = "bold"
  ) +
  geom_point(data = data_figure1 |> filter(period == "2019"),
             mapping = aes(
               x = `total_gov_spending_cap_wid_avg`, 
               y = `thresholds_avg`, 
               fill = `carb_overshoot_avg`, 
               size = `pop_avg`
             ), 
             alpha = 0.8, 
             shape = 21
  ) +
  geom_text_repel(data = data_figure1 |> filter(period == "2019"),
                  mapping = aes(
                    x = `total_gov_spending_cap_wid_avg`, 
                    y = `thresholds_avg`, 
                    label = `name`
                  ), 
                  size = 5,  
                  family = "Times New Roman",
                  max.overlaps = 100,  # Allow up to 100 labels; adjust as needed
                  box.padding = 0.5,  # Increase space around text
                  point.padding = 0.3,  # Adjust padding between text and points
                  segment.size = 0.2,  # Reduce the size of line segments
                  segment.color = "grey70"  # Make segments less prominent
  ) + 
  geom_path(
    data = data_figure1, 
    aes(
      x = `total_gov_spending_cap_wid_avg`, 
      y = `thresholds_avg`, 
      group = `name`,
      color = `carb_overshoot_avg`  # Use color aesthetic
    ), 
    arrow = arrow(type = "closed", length = unit(0.2, "cm")),  # Reduce arrow size
    alpha = 0.5, linewidth = 1
  ) + 
  geom_point(
    data = data_figure1, 
    aes(
      x = `total_gov_spending_cap_wid_avg`, 
      y = `thresholds_avg`
    ), 
    size = 0.5
  ) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  scale_x_continuous(breaks = seq(0, 60000, by = 10000),
                     limits = c(0, 62000)) +
  scale_size_continuous(range = c(4, 18), guide = "none") +  # Adjust the size range to make all points bigger
  scale_fill_viridis_d(name = "Carbon Overshoot") + 
  scale_color_viridis_d(name = "Carbon Overshoot") +  # Ensure consistent color scales for points and paths
  
  theme_minimal() +  # Use a clean, minimal theme
  labs( 
    x = "Total government spending [US$/cap]",
    y = "Social thresholds achieved [0-10]"
  ) +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),  # Transparent panel background
    plot.background = element_rect(fill = "transparent", color = NA),  # Transparent plot background
    axis.title.x = element_text(size = 20, family = "Times New Roman"),  # Set x-axis label font
    axis.title.y = element_text(size = 20, family = "Times New Roman"), 
    axis.text.x = element_text(size = 15, family = "Times New Roman"),   # Set x-axis tick label font
    axis.text.y = element_text(size = 15, family = "Times New Roman"), 
    legend.title = element_text(size = 16, face = "bold", family = "Times New Roman"),
    legend.text = element_text(size = 16, family = "Times New Roman"),
    legend.position = "bottom"
  ) +  
  guides(
    fill = guide_legend(override.aes = list(size = 6)),  # Make legend dots bigger for fill
    color = "none"  # Remove color from geom_path from the legend
  )

ggsave(filename = "Analysis/Plots/1a_need_spending_wid.png", plot = last_plot(),  width = 20, height = 10, units = "in", bg = "transparent", dpi = 300)
                                                                           

# correlation test
correlation_test <- cor.test(
  data_figure1$`total_gov_spending_cap_wid_avg`, 
  data_figure1$`thresholds_avg`, 
  method = "spearman"
)
print(correlation_test)

shapiro_data = data_figure1  #   |> mutate(`Average government spending`= log(`Average government spending`))
shapiro.test(shapiro_data$`total_gov_spending_cap_wid_avg`)
shapiro.test(shapiro_data$`thresholds_avg`)


# getting the variation of coefficients for total government spending in figure 1a
data_total_var <- read_excel("Data/Overview/data_pooled.xlsx") |>   # dose using all years make sense here ? 
  select(name, year, thresh_level, total_gov_spending_cap_wid) |>   
 
  pivot_longer(cols = -c("name", "year", "thresh_level"), names_to = "type", values_to = "value") |>
  arrange(year) |>
  pivot_wider(names_from = "year", values_from = "value") |>
  
  group_by(thresh_level, type) |>
  summarise(
    across(`2005`:`2019`, 
           list(
             mean = ~mean(.x, na.rm = TRUE), 
             median = ~median(.x, na.rm = TRUE), ## changea rest accrodingly
             sd = ~sd(.x, na.rm = TRUE)
           ),
           .names = "{col}_{fn}"
    ),
    .groups = "drop"
  ) |>
  mutate(
    across(matches("_sd$"), 
           ~ (.x / get(sub("_sd$", "_mean", cur_column()))) * 100,       # sd/mean * 100
           .names = "{.col}_rsd" ))  |>
  rename_with(~ sub("_sd_rsd$", "_rsd", .), matches("_sd_rsd$")) |>

  mutate(
    thresh_level = str_replace(thresh_level, " outcome", ""),  # Clean 'thresh_level' column
    thresh_level = factor(thresh_level, levels = c("High", "Upper-middle", "Lower-middle", "Low"))  # Order levels
  ) |> 
  ungroup() |>
  rowwise() |>
  select(thresh_level, type, contains("_mean"), contains("_median"), contains("_sd"), contains("_rsd")) |>

  mutate(mean_avg = mean(c_across(`2005_mean`:`2019_mean`), na.rm = TRUE),    # pick a year or get average across period
         median_avg = mean(c_across(`2005_median`:`2019_median`), na.rm = TRUE),    
                
         sd_avg = mean(c_across(`2005_sd`:`2019_sd`), na.rm = TRUE),
         rsd_avg = mean(c_across(`2005_rsd`:`2019_rsd`), na.rm = TRUE)) |> 
  ungroup() |>
  select(thresh_level, type, contains("avg"), contains("2019"), everything()) |>
  arrange(thresh_level)

#View(data_total_var)

#View(data_pooled |> filter(year == 2019) |> select(name, thresh_level, contains("total_gov_spending_cap_wid")))


#### figure 1b
#View(data_figure1)
ggplot(data_figure1) +
  geom_hline(yintercept = 3, color = "grey", linetype = "dashed", size = 0.5, alpha = 0.7) +
  geom_hline(yintercept = 6, color = "grey", linetype = "dashed", size = 0.5, alpha = 0.7) +
  geom_hline(yintercept = 8, color = "grey", linetype = "dashed", size = 0.5, alpha = 0.7) +
  # Adjusted quadrant labels (swap x and y positions)
  annotate(
    "text", x = 95, y = 9.2, label = "High", 
    size = 5, family = "Times New Roman", color = "black", fontface = "bold"
  ) +
  annotate(
    "text", x = 95, y = 7, label = "Upper-middle", 
    size = 5, family = "Times New Roman", color = "black", fontface = "bold"
  ) +
  annotate(
    "text", x = 95, y = 4.5, label = "Lower-middle", 
    size = 5, family = "Times New Roman",  color = "black", fontface = "bold"
  ) +
  annotate(
    "text", x = 95, y = 1.5, label = "Low", 
    size = 5, family = "Times New Roman", color = "black", fontface = "bold"
  ) +
  
  geom_point(data = data_figure1 |> filter(period == "2019"),
             mapping = aes(
               x = `total_intensity_gov_g_avg`, 
               y = `thresholds_avg`, 
               fill = `carb_overshoot_avg`, 
               size = `pop_avg`
             ), 
             alpha = 0.8, 
             shape = 21
  ) + 
  geom_text_repel(data = data_figure1 |> filter(period == "2019"),
                  mapping = aes(
                    x = `total_intensity_gov_g_avg`, 
                    y = `thresholds_avg`, 
                    label = `name`
                  ), 
                  size = 5,  
                  family = "Times New Roman",
                  max.overlaps = 100,  # Allow up to 100 labels; adjust as needed
                  box.padding = 0.5,  # Increase space around text
                  point.padding = 0.3,  # Adjust padding between text and points
                  segment.size = 0.2,  # Reduce the size of line segments
                  segment.color = "grey70"  # Make segments less prominent
  ) + 
  geom_path(
    data = data_figure1, 
    aes(
      x = `total_intensity_gov_g_avg`, 
      y = `thresholds_avg`, 
      group = `name`,
      color = `carb_overshoot_avg`  # Match the color aesthetic with fill
    ), 
    arrow = arrow(type = "closed", length = unit(0.2, "cm")),  # Reduce arrow size
    alpha = 0.5, linewidth = 1
  ) + 
  geom_point(
    data = data_figure1, 
    aes(
      x = total_intensity_gov_g_avg, 
      y = thresholds_avg
    ),  
    size = 0.5
  ) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  scale_x_log10(breaks = c( 0.1, 1, 10, 100),      # if g co2: c(5, 50, 500, 5000),
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
                limits = c(0.05, 130)  
  )  + 
  scale_size_continuous(range = c(4, 18), guide = "none") +  # Adjust size range
  labs(
    x = expression("Carbon intensity of total government spending [kg CO"[2]*"e / US$]"),
    y = "Social thresholds achieved [0-10]"
  ) +
  scale_fill_viridis_d(name = "Carbon Overshoot") + 
  scale_color_viridis_d(name = "Carbon Overshoot") +  # Ensure consistent color scales for points and paths
  theme_minimal(base_family = "Times New Roman") +  # Use a clean, minimal theme
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),  # Transparent panel background
    plot.background = element_rect(fill = "transparent", color = NA),  # Transparent plot background
    axis.title.x = element_text(size = 20, family = "Times New Roman"),  # Set x-axis label font
    axis.title.y = element_text(size = 20, family = "Times New Roman"), 
    axis.text.x = element_text(size = 15, family = "Times New Roman"),   # Set x-axis tick label font
    axis.text.y = element_text(size = 15, family = "Times New Roman"), 
    legend.title = element_text(size = 16, face = "bold", family = "Times New Roman"),
    legend.text = element_text(size = 16, family = "Times New Roman")
  ) +
  # annotate("text", x = Inf, y = Inf, label = "A", vjust =  1.5, hjust = 2.5, color = "black", size = 20) +   # Add text to line
  theme(legend.position = "bottom") + 
  guides(
    fill = guide_legend(override.aes = list(size = 6)),  # Make legend dots bigger for fill
    color = "none"  # Remove color from geom_path from the legend
  ) 

ggsave(filename = "Analysis/Plots/1b_need_intensity_g.png", plot = last_plot(), width = 10, height = 12, units = "in", bg = "transparent", dpi = 300)

#View(data_figure1|> filter(Period == 2019) |> select(`Country Name`, `Average social thresholds achieved`, `Average carbon intensity (Government)`))
# correlation test
correlation_test <- cor.test(
  data_figure1$`total_intensity_gov_g_avg`, 
  data_figure1$`thresholds_avg`, 
  method = "spearman"
)
print(correlation_test)

data_shapiro = data_figure1 


shapiro.test(data_shapiro$`total_intensity_gov_g_avg`)
shapiro.test(data_figure1$`thresholds_avg`)


# to get descriptives of figure 1b
#View(data_pooled |> filter(year == 2019) |> select(name, thresholds, thresh_level, total_intensity_gov_g, adj_total_intensity_gov_g, everything(), -year))


 

#### figure 1c
#View(data_figure1)

ggplot(data_figure1) +
  geom_hline(yintercept = 3, color = "grey", linetype = "dashed", size = 0.5, alpha = 0.7) +
  geom_hline(yintercept = 6, color = "grey", linetype = "dashed", size = 0.5, alpha = 0.7) +
  geom_hline(yintercept = 8, color = "grey", linetype = "dashed", size = 0.5, alpha = 0.7) +
  # Adjusted quadrant labels (swap x and y positions)
  annotate(
    "text", x = 95, y = 9.2, label = "High", 
    size = 5, family = "Times New Roman", color = "black", fontface = "bold"
  ) +
  annotate(
    "text", x = 95, y = 7, label = "Upper-middle", 
    size = 5, family = "Times New Roman", color = "black", fontface = "bold"
  ) +
  annotate(
    "text", x = 95, y = 4.5, label = "Lower-middle", 
    size = 5, family = "Times New Roman",  color = "black", fontface = "bold"
  ) +
  annotate(
    "text", x = 95, y = 1.5, label = "Low", 
    size = 5, family = "Times New Roman", color = "black", fontface = "bold"
  ) +
  geom_point(data= data_figure1 |> filter(period == "2019"),
             mapping = aes(
               x = `adj_total_intensity_gov_g_avg`, 
               y = `thresholds_avg`, 
               fill = `carb_overshoot_avg`, 
               size = `pop_avg`
             ), 
             alpha = 0.8, 
             shape = 21
  ) +
  geom_text_repel(data = data_figure1 |> filter(period == "2019"),
                  mapping = aes(
                    x = `adj_total_intensity_gov_g_avg`, 
                    y = `thresholds_avg`, 
                    label = `name`
                  ), 
                  size = 5,  
                  family = "Times New Roman",
                  max.overlaps = 100,  
                  box.padding = 0.5,  
                  point.padding = 0.3,  
                  segment.size = 0.2,  
                  segment.color = "grey70"  
  ) +
  geom_path(
    data = data_figure1, 
    aes(
      x = `adj_total_intensity_gov_g_avg`, 
      y = `thresholds_avg`, 
      group = `name`,
      color = `carb_overshoot_avg`  # Ensure consistent color with geom_point
    ), 
    arrow = arrow(type = "closed", length = unit(0.2, "cm")),  
    alpha = 0.5, linewidth = 1
  ) +
  
  geom_point(
    data = data_figure1, 
    aes(
      x = `adj_total_intensity_gov_g_avg`, 
      y = `thresholds_avg`
    ),  
    size = 0.5
  ) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  scale_x_log10(breaks = c( 0.1, 1, 10, 100),      # if g co2: c(5, 50, 500, 5000),
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
                limits = c(0.05, 130)
  )  +
  scale_size_continuous(range = c(4, 18), guide = "none") +  # Adjust size range
  
  labs( 
    x = expression("Adjusted carbon intensity of total government spending [kg CO"[2]*"e / US$]"),
    y = "Social thresholds achieved [0-10]") +
  scale_fill_viridis_d(name = "Carbon Overshoot") + 
  scale_color_viridis_d(name = "Carbon Overshoot") +  # Ensure consistent color scales for points and paths
  theme_minimal(base_family = "Times New Roman") +  
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),  
        plot.background = element_rect(fill = "transparent", color = NA),  
        axis.title.x = element_text(size = 20, family = "Times New Roman", vjust = -1),  
        axis.title.y = element_text(size = 20, family = "Times New Roman"), 
        axis.text.x = element_text(size = 15, family = "Times New Roman"),   
        axis.text.y = element_text(size = 15, family = "Times New Roman"), 
        legend.title = element_text(size = 16, face = "bold", family = "Times New Roman"),
        legend.text = element_text(size = 16, family = "Times New Roman")) +
  theme(legend.position = "bottom") +   
 # annotate("text", x = Inf, y = Inf, label = "B", vjust =  1.5, hjust = 2.5, color = "black", size = 20) + 
  guides(
    fill = guide_legend(override.aes = list(size = 6)),  
    color = "none"  
  )   

 ggsave(filename = "Analysis/Plots/1c_need_adjust_intensity_g.png", plot = last_plot(), width = 10, height = 12, units = "in", bg = "transparent", dpi = 300)


# correlation test
 correlation_test <- cor.test(
   data_figure1$`adj_total_intensity_gov_g_avg`, 
   data_figure1$`thresholds_avg`, 
   method = "spearman"
 )
 print(correlation_test)
 
 shapiro.test(data_figure1$`adj_total_intensity_gov_g_avg`)
 shapiro.test(data_figure1$`thresholds_avg`)

 
#View(data_pooled |> filter(year == 2019) |> select(name, thresholds, thresh_level, total_intensity_gov_g, adj_total_intensity_gov_g, everything(), -year))



#### figure 2: show spending share averages and carbon intensity medians (Government)

# figure 2a: spending shares. 
data_distribution_share <- read_excel("Data/Overview/data_pooled.xlsx") |>    
  select(name, year, thresh_level, contains("_tot"), contains("_wid"), 
         -contains("adj_"), -contains("_intensity"),  -contains("hh"),  -contains("_cap"), -contains("_gdp")) |>
  select(-contains("tot_g"), -contains("GHG")) |>
  select(-health_gov_spending_tot_wid, -envi_gov_spending_tot_wid, -edu_gov_spending_tot_wid, -recr_gov_spending_tot_wid, -ord_gov_spending_tot_wid, -def_gov_spending_tot_wid) |>
  
  pivot_longer(cols = -c("name", "year", "thresh_level"), names_to = "category", values_to = "value") |>
  arrange(year) |>
  pivot_wider(names_from = "year", values_from = "value") |>

  group_by(thresh_level, category) |>
  summarise(
    across(`2005`:`2019`, 
           list(mean = ~mean(.x, na.rm = TRUE), 
                median = ~median(.x, na.rm = TRUE), 
                sd = ~sd(.x, na.rm = TRUE)),
           .names = "{col}_{fn}"),
    .groups = "drop"
  ) |>
  
  mutate(
    category = str_replace(category, "health_envi", "Health & Environment"),
    category = str_replace(category, "edu_recr", "Education & Recreation"),
    category = str_replace(category, "ord_def", "Order & Defense"),
    category = str_replace(category, "hous", "Housing"),
    category = str_replace(category, "soc", "Social Protection"),
    category = str_replace(category, "indu", "Industry"),
    category = str_replace(category, "admin", "Public Administration"),
    category = str_replace(category, "_gov_spending_tot_wid", ""),  
    category = factor(category, levels = c( "Industry","Order & Defense", "Public Administration", "Housing", 
                                    "Education & Recreation",  "Health & Environment", "Social Protection")), 
    thresh_level = str_replace(thresh_level, " outcome", ""),  # Clean 'thresh_level' column
    thresh_level = factor(thresh_level, levels = c("High", "Upper-middle", "Lower-middle", "Low"))  # Order levels
  ) |>
  
  ungroup() |>
  rowwise() |>
  select(thresh_level, category, contains("_mean"), contains("_median"),contains("_sd"), contains("lower_p"), contains("upper_p")) |>
  mutate(mean = mean(c_across(`2005_mean`:`2019_mean`), na.rm = TRUE),
         median = mean(c_across(`2005_median`:`2019_median`), na.rm = TRUE),
         sd = mean(c_across(`2005_sd`:`2019_sd`), na.rm = TRUE),
         rsd = (sd / mean) * 100) |>   # Relative SD
  ungroup() |>
  select(thresh_level, category, mean, median, sd, rsd) |>
  group_by(category)  |>
  mutate(median_sector = mean(median),
         sd_sector = mean(sd),
         rsd_sector = mean(rsd))  # Average RSD for each sector
#View(data_distribution_share)

#View(data_pooled |> filter(year == 2019) |> select(name, thresh_level, contains("tot_wid")))
# data for figure 2a
data_figure2a = data_distribution_share |>
  select(thresh_level, category, "median_spending_share" = median) |>
  group_by(thresh_level) |>
  mutate(median_spending_share_100 = median_spending_share / sum(median_spending_share, na.rm = TRUE)) |>
  ungroup()
  

# plot
ggplot(data = data_figure2a, aes(x = `thresh_level`, y = `median_spending_share_100`, fill = `category`)) +
  geom_bar(stat = "identity", color = "black") +  # Use percent deviation values for the bars
  labs(title = "",
       x = "Level of social outcome",
       y = "Share of total government spending [%]",
       fill = "Spending category") + 
 # coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(labels = function(x) round(x * 100, 0), limits = c(0,1.05)) +

  geom_text(aes(label = paste0(round(median_spending_share_100 * 100, 1), "")),  
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


ggsave(file = "Analysis/Plots/2a_shares_wid.png", plot = last_plot(), width = 12, height = 8, units = "in", dpi = 600)



# figure 2b

data_distribution_footprints <- read_excel("Data/Overview/data_pooled.xlsx") |>
  # Keep only the columns you need
  select(name, year, thresh_level, contains("ghg_"), -contains("hh"), contains("gov"),
         -ghg_health_gov_cap_g, -ghg_envi_gov_cap_g, -ghg_edu_gov_cap_g, 
         -ghg_recr_gov_cap_g, -ghg_ord_gov_cap_g, -ghg_def_gov_cap_g) 

# Map sector names
sector_map <- c(
  "ghg_health_envi_gov_cap_g" = "Health & Environment",
  "ghg_edu_recr_gov_cap_g" = "Education & Recreation",
  "ghg_ord_def_gov_cap_g" = "Order & Defense",
  "ghg_hous_gov_cap_g" = "Housing",
  "ghg_soc_gov_cap_g" = "Social Protection",
  "ghg_indu_gov_cap_g" = "Industry",
  "ghg_admin_gov_cap_g" = "Public Administration"
)

sector_order <- c("Industry", "Order & Defense", "Public Administration", 
                  "Housing", "Education & Recreation", "Health & Environment", "Social Protection")

sector_colors <- c(
  "Industry" = "#FDE725",
  "Order & Defense" = "#90D743",
  "Public Administration" = "#35B779",
  "Housing" = "#21918C",
  "Education & Recreation" = "#31688E",
  "Health & Environment" = "#3B528B",
  "Social Protection" = "#443983"
)

# Compute mean GHG per sector and social outcome, then relative share
data_figure2b <- data_distribution_footprints |>
  pivot_longer(cols = -c(name, year, thresh_level), names_to = "category", values_to = "ghg") |>
  filter(category %in% names(sector_map)) |>
  mutate(category = sector_map[category],
         thresh_level = factor(str_replace(thresh_level, " outcome", ""),
                               levels = c("High", "Upper-middle", "Lower-middle", "Low")),
         category = factor(category, levels = sector_order)) |>
  group_by(thresh_level, category) |>
  summarise(median_ghg = median(ghg, na.rm = TRUE), .groups = "drop") |>
  group_by(thresh_level) |>
  mutate(median_ghg_share = median_ghg / sum(median_ghg, na.rm = TRUE)) |>
  mutate(median_ghg_share_100 = median_ghg_share / sum(median_ghg_share, na.rm = TRUE)) |>
  ungroup()

#View(data_figure2b)

# Plot: governmental carbon footprint shares with styling from 2a
ggplot(data_figure2b, aes(x = thresh_level, y = median_ghg_share_100, fill = category)) +
  
  # Bars
  geom_bar(stat = "identity", color = "black") +
  
  # Labels inside bars (only if median_ghg_share_100 > 1%)
  geom_text(aes(label = ifelse(median_ghg_share_100 > 0.01, paste0(round(median_ghg_share_100*100,1), ""), "")),
            position = position_stack(vjust = 0.5),
            size = 8,
            color = "black",
            fontface = "bold",
            family = "Times New Roman") +
  
  # Fill colors
  scale_fill_manual(values = sector_colors) +
  
  # Axes labels
  labs(x = "Level of social outcome",
       y = "Share of total governmental carbon footprint [%]",
       fill = "Spending category") +
  
  # Y-axis as percent without "%" sign
  scale_y_continuous(labels = function(x) round(x * 100, 0), limits = c(0,1.05)) +
  
  # Theme: copy from 2a
  theme_bw(base_size = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(size = 22, face = "bold", family = "Times New Roman", vjust = -1),
        axis.title.y = element_text(size = 22, face = "bold", family = "Times New Roman", hjust = 1),
        axis.text.x = element_text(size = 20, family = "Times New Roman"),
        axis.text.y = element_text(size = 20, family = "Times New Roman"),
        legend.title = element_text(size = 19, face = "bold", family = "Times New Roman"),
        legend.text = element_text(size = 15, family = "Times New Roman"),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.position = "bottom")

# Save figure
ggsave("Analysis/Plots/2b_ghg_shares.png", plot = last_plot(), width = 12, height = 8, units = "in", dpi = 600)


#put data together for sd

data_figure2 = merge(data_figure2a, data_figure2b, by = c("thresh_level", "category")) 

#View(data_figure2)


##### figure 3. variety in shares and adjusted carbon intensity of government spending
# data cleaning
data_figure3a <- read_excel("Data/Overview/data_pooled.xlsx") |>
  select(name, year, thresh_level, contains("gov_spending_tot_wid")) |>
  
  pivot_longer(cols = -c(name, year, thresh_level), names_to = "category", values_to = "share") |>
  group_by(thresh_level, category) |>

  filter(!category %in% c( "health_gov_spending_tot_wid", "envi_gov_spending_tot_wid",
                       "edu_gov_spending_tot_wid",  "recr_gov_spending_tot_wid",
                       "ord_gov_spending_tot_wid", "def_gov_spending_tot_wid") ) |>
  
  mutate(
    category = str_replace(category, "_gov_spending_tot_wid", ""),
    category = str_replace(category, "health_envi", "Health & Environment"),
    category = str_replace(category, "edu_recr", "Education & Recreation"),
    category = str_replace(category, "ord_def", "Order & Defense"),
    category = str_replace(category, "hous", "Housing"),
    category = str_replace(category, "soc", "Social Protection"),
    category = str_replace(category, "indu", "Industry"),
    category = str_replace(category, "admin", "Public Administration"),
    category = str_replace(category, "total", "Total"),
    category = str_replace(category, "_g", ""),
    thresh_level = str_replace(thresh_level, " outcome", ""),
    thresh_level = factor(thresh_level, levels = c("High", "Upper-middle", "Lower-middle", "Low"))
  )  |>
  
  mutate(share = share * 100,
         category = category, 
         category = ifelse(category == "Public Administration", "Public\nAdministration", category),
         category = ifelse(category == "Education & Recreation", "Education &\nRecreation", category),
         category = ifelse(category == "Social Protection", "Social\nProtection", category),
         category = ifelse(category == "Order & Defense", "Order &\nDefense", category),
         category = ifelse(category == "Health & Environment", "Health &\nEnvironment", category),
         category = ifelse(category == "Public Administration", "Public\nAdministration", category))


#View(data_figure3a)
# plotting
# setting up some paramters
lw = 1
width = 5.7
height = 10
textsize = 13

color_list <- c('#CBC8E1' ,'#F78D81', '#F9BC74', '#A5DCD2') 

# ggplot

ggplot(data_figure3a, aes(x = share, color=thresh_level, ..count..)) + 
  geom_density(linewidth=lw, fill=NA) +
  scale_y_continuous(expand = expansion(mult = c(0, .2))) +
  facet_grid(category~., switch = "y", scales = "free_y") +
  scale_x_continuous(trans='log10', labels = function(x) format(x, scientific = FALSE)) +
  scale_color_viridis(discrete = TRUE, direction=-1) + 
  labs(x = 'Share of total government spending [%]', y = 'Frequency (Count)',   color = "Social outcome") +
  theme_minimal(
    base_family = "Times New Roman") +
  theme(legend.position = "bottom", 
        legend.text = element_text(size=textsize),
        legend.justification = 1,
        text = element_text(size = textsize),
        strip.text.x = element_blank(),
        strip.text.y.left = element_text(angle = 90, vjust = 0.01, size=textsize, face ="bold"),
        axis.text.x = element_text(size=textsize),
        axis.text.y = element_text(size=textsize),
        axis.title.x = element_text(face="bold"),
        strip.placement = "outside",
        panel.spacing.y = unit(10, "pt")) 

ggsave("Analysis/Plots/3a_ridges_shares.png", dpi = 600, width = width, height = height, units = "in")  



## figure 3b
  data_figure3b <- read_excel("Data/Overview/data_pooled.xlsx") |>
    select(name, year, thresh_level, contains("adj"), -contains("hh")) |>
    
    pivot_longer(cols = -c(name, year, thresh_level), names_to = "category", values_to = "adj_intensity") |>
    group_by(thresh_level, category) |>
    filter(str_detect(category, "gov_g")) |>
    
    filter(!category %in% c( "adj_health_intensity_gov_g", "adj_envi_intensity_gov_g",
                         "adj_edu_intensity_gov_g", "adj_recr_intensity_gov_g",
                         "adj_ord_intensity_gov_g", "adj_def_intensity_gov_g") ) |>
    
    mutate(
      category = str_replace(category, "adj_", ""),
      category = str_replace(category, "_gov", ""),
      category = str_replace(category, "_intensity", ""),
      category = str_replace(category, "health_envi", "Health & Environment"),
      category = str_replace(category, "edu_recr", "Education & Recreation"),
      category = str_replace(category, "ord_def", "Order & Defense"),
      category = str_replace(category, "hous", "Housing"),
      category = str_replace(category, "soc", "Social Protection"),
      category = str_replace(category, "indu", "Industry"),
      category = str_replace(category, "admin", "Public Administration"),
      category = str_replace(category, "total", "Total"),
      category = str_replace(category, "_g", ""),
      thresh_level = str_replace(thresh_level, " outcome", ""),
      thresh_level = factor(thresh_level, levels = c("High", "Upper-middle", "Lower-middle", "Low"))
    ) |>
    
    mutate(adj_intensity = adj_intensity*1000,
           category = category, 
           category = ifelse(category == "Public Administration", "Public\nAdministration", category),
           category = ifelse(category == "Education & Recreation", "Education &\nRecreation", category),
           category = ifelse(category == "Social Protection", "Social\nProtection", category),
           category = ifelse(category == "Order & Defense", "Order &\nDefense", category),
           category = ifelse(category == "Health & Environment", "Health &\nEnvironment", category),
           category = ifelse(category == "Public Administration", "Public\nAdministration", category)) |>
    filter(category != 'Total') %>%
    mutate(year = as.character(year))

# ggplot
ggplot(data_figure3b, aes(x = adj_intensity, color=`thresh_level`, ..count..)) + 
  geom_density(linewidth=lw, fill=NA) +
  scale_y_continuous(expand = expansion(mult = c(0, .2))) +
  facet_grid(category~., switch = "y", scales = "free_y") +
  scale_x_continuous(trans='log10', labels = function(x) format(x, scientific = FALSE)) +
  scale_color_viridis(discrete = TRUE, direction=-1) +
  labs(x = expression(bold("Adjusted carbon intensity of government spending [g CO"[bold("2")]*"e / US$]")), 
       y = 'Frequency (Count)', color = "Social outcome") +
  theme_minimal(
    base_family = "Times New Roman") +
  theme(legend.position = "bottom", 
        legend.justification = 1,
        legend.text = element_text(size=textsize),
        text = element_text(size = textsize),
        strip.text.x = element_blank(),
        strip.text.y.left = element_text(angle = 90, vjust = 0.01, size=textsize, face ="bold"),
        axis.text.x = element_text(size=textsize),
        axis.text.y = element_text(size=textsize),
        axis.title.x = element_text(hjust = 1),
        strip.placement = "outside",
        panel.spacing.y = unit(10, "pt"))

ggsave("Analysis/Plots/3b_ridges_intensity.png", dpi = 600, width = width, height = height, units = "in") 


# figure 3: put data together
data_figure3 = merge(data_figure3a, data_figure3b, by = c("name", "year", "thresh_level", "category"))
#View(data_figure3 )


# in addition, below the descriptives to describe figure 3 in manuscript. # sd and mean data for intensities 
# gov
data_intensities_gov <- read_excel("Data/Overview/data_pooled.xlsx") |>
  select(name, year, thresh_level, contains("adj"), -contains("hh")) |>
  
  pivot_longer(cols = -c(name, year, thresh_level), names_to = "category", values_to = "value") |>
  group_by(thresh_level, category) |>
  summarise(
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm  =T),
    sd = sd(value, na.rm = TRUE),
    rsd = (sd(value, na.rm = TRUE) / mean(value, na.rm = TRUE)) * 100, # Calculate RSD
    .groups = "drop"
  ) |>

  filter(str_detect(category, "gov_g")) |>
  
  filter(!category %in% c( "adj_health_intensity_gov_g", "adj_envi_intensity_gov_g",
                      "adj_edu_intensity_gov_g", "adj_recr_intensity_gov_g",
                      "adj_ord_intensity_gov_g", "adj_def_intensity_gov_g") ) |>

  mutate(
    category = str_replace(category, "adj_", ""),
    category = str_replace(category, "_gov", ""),
    category = str_replace(category, "_intensity", ""),
    category = str_replace(category, "health_envi", "Health & Environment"),
    category = str_replace(category, "edu_recr", "Education & Recreation"),
    category = str_replace(category, "ord_def", "Order & Defense"),
    category = str_replace(category, "hous", "Housing"),
    category = str_replace(category, "soc", "Social Protection"),
    category = str_replace(category, "indu", "Industry"),
    category = str_replace(category, "admin", "Public Administration"),
    category = str_replace(category, "total", "Total"),
    category = str_replace(category, "_g", ""),
    thresh_level = str_replace(thresh_level, " outcome", ""),
    thresh_level = factor(thresh_level, levels = c("High", "Upper-middle", "Lower-middle", "Low"))
  ) |>
  arrange(thresh_level, category) |>
  mutate(mean = 1000 * mean, # Convert to grams
         median = 1000* median,
         sd = 1000 * sd) |>  
  ungroup() |>
  group_by(category) |>
  mutate(mean_category = mean(mean),   # calculate average RSD for each descriptive and category
         median_category = mean(median),
         sd_category = mean(sd),
         rsd_category = mean(rsd)) 


#View(data_intensities_gov)
# put together descriptives for figure 3

#View(data_distribution_share)
data_distribution_share
data_figure3_add = data_distribution_share |> 
  rename_with(~ paste0(.x, "_shares"), .cols = 3:ncol(data_distribution_share)) |>
  group_by(thresh_level, category)

data_figure3_on = data_intensities_gov |> 
  rename_with(~ paste0(.x, "_adj_intensities"), .cols = 3:ncol(data_intensities_gov)) |>
  group_by(thresh_level, category)

data_figure3_addon = merge(data_figure3_add, data_figure3_on, by = c( "thresh_level", "category"))



#View(data_figure3_addon)
# intensities hh
data_intensities_hh <- read_excel("Data/Overview/data_pooled.xlsx") |>
  select(name, year, thresh_level, contains("adj"), -contains("gov")) |>
  
  pivot_longer(cols = -c(name, year, thresh_level), names_to = "category", values_to = "value") |>
  group_by(thresh_level, category) |>
  summarise(
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm  =T),
    sd = sd(value, na.rm = TRUE),
    rsd = (sd(value, na.rm = TRUE) / mean(value, na.rm = TRUE)) * 100, # Calculate RSD
    .groups = "drop"
  ) |>
  
  filter(str_detect(category, "hh_g")) |>
  
  filter(!category %in% c( "adj_health_intensity_hh_g", "adj_envi_intensity_hh_g",
                       "adj_edu_intensity_hh_g", "adj_recr_intensity_hh_g",
                       "adj_ord_intensity_hh_g", "adj_def_intensity_hh_g") ) |>
  mutate(
    category = str_replace(category, "adj_", ""),
    category = str_replace(category, "_hh", ""),
    category = str_replace(category, "_intensity", ""),
    category = str_replace(category, "health", "Health & Environment"),
    category = str_replace(category, "edu", "Education & Recreation"),
    category = str_replace(category, "ord", "Order & Defense"),
    category = str_replace(category, "hous", "Housing"),
    category = str_replace(category, "soc", "Social Protection"),
    category = str_replace(category, "indu", "Industry"),
    category = str_replace(category, "admin", "Public Administration"),
    category = str_replace(category, "total", "Total"),
    category = str_replace(category, "_g", ""),
    thresh_level = str_replace(thresh_level, " outcome", ""),
    thresh_level = factor(thresh_level, levels = c("High", "Upper-middle", "Lower-middle", "Low"))
  ) |>
  arrange(thresh_level, category) |>

  mutate(mean = 1000 * mean, # Convert to grams
         median = 1000* median,
         sd = 1000 * sd) |>  
  ungroup() |>
  group_by(category) |>

  mutate(mean_category = mean(mean),
         median_category = mean(median),
         sd_category = mean(sd),
         rsd_category = mean(rsd)) # Calculate average RSD for each category

#View(data_intensities_hh)



####### figure 4: heat map (gloria)
## comparison: government vs household

#average data used
data_heat = read_excel("Data/Overview/data_pooled.xlsx") |>
  select(name, year, thresh_level, contains("spending_cap"), contains("ghg"), -contains("log"), carb_factor, contains("intensity")) |>
  group_by(thresh_level) |>
  summarise(
    total_intensity_gov_g = median(total_intensity_gov_g, na.rm = TRUE),    # median or mean?!  currenlty, median
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
  
  mutate( 
    total_factor = adj_total_intensity_hh_g / adj_total_intensity_gov_g,
    health_envi_factor = adj_health_envi_intensity_hh_g / adj_health_envi_intensity_gov_g,
    soc_factor = adj_soc_intensity_hh_g / adj_soc_intensity_gov_g,
    edu_recr_factor = adj_edu_recr_intensity_hh_g / adj_edu_recr_intensity_gov_g,
    hous_factor = adj_hous_intensity_hh_g / adj_hous_intensity_gov_g,
    indu_factor = adj_indu_intensity_hh_g / adj_indu_intensity_gov_g,
    ord_def_factor = adj_ord_def_intensity_hh_g / adj_ord_def_intensity_gov_g,
    admin_factor =  adj_admin_intensity_hh_g / adj_admin_intensity_gov_g
    ) |>
  
  select(thresh_level, contains("factor"), contains("adj"))

#View(data_heat)

#### heat maps: 1x gov, 1x hh 

## government (gloria). using adjusted intensities
data_gov_heat_adj <- data_heat |>
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
#View(data_gov_heat_adj)
x_axis =  expression(bold(paste("Adjusted carbon intensity of government spendings [g CO"[bold("2")]*"e / US$]")))

ggplot(data_gov_heat_adj, aes(x = sector, y = thresh_level, fill = adj_intensity_gov_g)) +
  geom_tile(color = "grey40", linetype = "solid", size = 0.5) +  # Thin borders for tiles
  geom_text(
    aes(label = format(round(adj_intensity_gov_g, 1), scientific = FALSE)),  # Format intensity values
    color = "black", size = 7, family = "Times New Roman", fontface = "bold"
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
    axis.title.y = element_text(size = 20, margin = margin(r = 10), face = "bold"),   
    axis.title.x = element_text(size = 20, margin = margin(t = 10)),   # X-axis title bold
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16),  # Rotated X-axis labels
    axis.text.y = element_text(angle = 45, hjust = 1, size = 16),  # Y-axis labels
    legend.position = "bottom",  # Legend on the bottom
    legend.title = element_text(size = 18, face = "bold", vjust = 3, hjust = 2, family = "Times New Roman"),  # Bigger and bold legend title
    
    legend.text = element_text(size = 16), 
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()) + 

  coord_fixed(ratio = 2)

ggsave(
  file = "Analysis/Plots/4a_heat_adj_intensity_gov_g.png", 
  plot = last_plot(),  
  width = 12, height = 12, units = "in", bg = "transparent", dpi = 300
)


# heatmap for social coefficient
data_reg <- tibble(
  sector = c("Health & Environment", "Social Protection", "Education & Recreation", 
             "Housing", "Industry", "Order & Defense", "Public Administration"), 
  coefficient =  c(1.895, 0.954, 4.857, 14.308, 4.673, -4.679, 13.245)   ) |>           
  
  mutate(sector = factor(sector, levels = rev(c(  "Education & Recreation",  
                                                  "Social Protection", 
                                                  "Public Administration",    
                                                  "Order & Defense", 
                                                  "Health & Environment",
                                                  "Housing", 
                                                  "Industry"))) )

#plot
ggplot(data_reg, aes(x = sector, y = "Full sample", fill = coefficient)) +
  geom_tile(color = "grey40", size = 0.5) +  # Add borders to the tiles
  geom_text(
    aes(label = format(round(coefficient, 3), scientific = FALSE)),  # Format coefficient values
    color = "black", size = 7, family = "Times New Roman", fontface = "bold"
  ) +
  scale_fill_gradientn(
    colors = brewer.pal(9, "Oranges") , # Example palette
    values = rescale(c( -10, 0, 10, 20)),  # Adjust according to data range
    name = "",
    breaks= c(-10,0,10,20),
    limits = c(-15, 25), 
    guide = guide_colorbar(
      barwidth = 20, barheight = 0.5, direction = "horizontal"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    text = element_text(family = "Times New Roman"),  # Use Times New Roman font
    axis.title.x = element_text(size = 20, angle = 0, hjust = 0.5, vjust = 50, face = "bold"),  # Move y-axis title to the top
    axis.title.y = element_text(size = 16, face = "bold"),           # Add y-axis title with size 14
    axis.text.x = element_blank(),   
    axis.text.y = element_text(angle = 45, hjust = 1, size = 16),  # Adjust y-axis text
    legend.position = "top",                          # Legend at the top
    legend.text = element_text(size = 16),
    panel.grid.major = element_blank(),               # Remove major grid lines
    panel.grid.minor = element_blank()   
  ) +
  labs(
    x = "Association with number of achieved social thresholds",
    y = ""
  ) +
  annotate("text", x = 1.3, y = 1.1, label = "***", color = "black", size = 6, fontface = "bold", family = "Times New Roman") +
  annotate("text", x = 2.3, y = 1.1, label = "***", color = "black", size = 6, fontface = "bold", family = "Times New Roman") +
  annotate("text", x = 3.3, y = 1.1, label = "***", color = "black", size = 6, fontface = "bold", family = "Times New Roman") +
  annotate("text", x = 4.3, y = 1.1, label = "***", color = "black", size = 6, fontface = "bold", family = "Times New Roman") +
  annotate("text", x = 5.3, y = 1.1, label = "***", color = "black", size = 6, fontface = "bold", family = "Times New Roman") +
  annotate("text", x = 6.3, y = 1.1, label = "**", color = "black", size = 6, fontface = "bold", family = "Times New Roman") +
  annotate("text", x = 7.3, y = 1.1, label = "***", color = "black", size = 6, fontface = "bold", family = "Times New Roman") +
  
  coord_fixed(ratio = 2)


# Save the plot
ggsave(
  file = "Analysis/Plots/4a_heat_coefficients_gov_g.png", 
  plot = last_plot(),  
  width = 12, height = 4, bg = "transparent", units = "in", dpi = 300
)




    #### household /private (GLORIA)

#  heat map using adjusted data
data_hh_heat_adj <- data_heat |>
  select(-contains("total")) |>
  group_by(thresh_level) |>
  summarise(across(adj_health_envi_intensity_hh_g:adj_admin_intensity_hh_g, mean, na.rm = TRUE)) |>
  pivot_longer(cols = adj_health_envi_intensity_hh_g:adj_admin_intensity_hh_g, names_to = "sector", values_to = "adj_intensity_hh_g") |>
  mutate(sector = str_replace(sector, "adj_health_envi_intensity_hh_g", "Health & Environment"),
         sector = str_replace(sector, "adj_soc_intensity_hh_g", "Social Protection"),
         sector = str_replace(sector, "adj_edu_recr_intensity_hh_g", "Education & Recreation"),
         sector = str_replace(sector, "adj_hous_intensity_hh_g", "Housing"),
         sector = str_replace(sector, "adj_indu_intensity_hh_g", "Industry"),
         sector = str_replace(sector, "adj_ord_def_intensity_hh_g", "Order & Defense"),
         sector = str_replace(sector, "adj_admin_intensity_hh_g", "Public Administration")) |>
  
  mutate(sector = factor(sector, levels = rev(c(  "Education & Recreation",  
                                                  "Social Protection", 
                                                  "Public Administration",    
                                                  "Order & Defense", 
                                                  "Health & Environment",
                                                  "Housing", 
                                                  "Industry"))) ) |>
  
  filter(sector %in% c("Education & Recreation", "Social Protection", "Health & Environment"))


# plot 
#View(data_hh_heat_adj)
x_axis =  expression(bold(paste("Adjusted carbon intensity of household spendings [g CO"[bold("2")]*"e / US$]")))

ggplot(data_hh_heat_adj, aes(x = sector, y = thresh_level, fill = adj_intensity_hh_g)) +
  geom_tile(color = "grey40", linetype = "solid", size = 0.5) +  # Thin borders for tiles
  
  geom_text(
    aes(
      label = ifelse(is.na(adj_intensity_hh_g), "NA", format(round(adj_intensity_hh_g, 1), scientific = FALSE)),  # Replace NA with "NA"
      hjust = ifelse(is.na(adj_intensity_hh_g), 0.7, 0.5)  # Shift "NA" a bit to the left
    ),
    color = "black", size = 7, family = "Times New Roman", fontface = "bold"
  ) +
  
  scale_fill_viridis(
    direction = 1,
    name = "",
    trans = "log",  # Apply logarithmic scale
    breaks = c(1,10, 100, 1000, 10000), # Adjusted breaks  # c(10, 100, 1000, 10000, 100000),
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
    limits = c(0.1, 50000),  # Adjusted limits .   # c(5, 1000000),
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
    panel.grid.minor = element_blank())  +
  
  coord_fixed( ratio = 2)

ggsave(
  file = "Analysis/Plots/4b_heat_adj_intensity_hh_g.png", 
  plot = last_plot(),  
  width = 12, height = 12, units = "in", bg = "transparent", dpi = 300
)


# heatmap for social coefficient
data_reg <- tibble(
  sector = c("Health & Environment", "Social Protection", "Education & Recreation", 
             "Housing", "Industry", "Order & Defense", "Public Administration"), 
  coefficient =  c(2.678, -0.988, -0.972, 1.065, -2.386, -8.129, 4.888)         ) |>                                                                             
  mutate(sector = factor(sector, levels = rev(c(  "Education & Recreation",  
                                                  "Social Protection", 
                                                  "Public Administration",    
                                                  "Order & Defense", 
                                                  "Health & Environment",
                                                  "Housing", 
                                                  "Industry"))) )  |>
  
  filter(sector %in% c("Education & Recreation", "Social Protection", "Health & Environment"))


ggplot(data_reg, aes(x = sector, y = "Full sample", fill = coefficient)) +
  geom_tile(color = "grey40", size = 0.5) +  # Add borders to the tiles
  geom_text(
    aes(
      label = ifelse(is.na(coefficient), "NA", format(round(coefficient, 3), scientific = FALSE)),  # Replace NA with "NA"
      hjust = ifelse(is.na(coefficient), 0.7, 0.5)  # Shift "NA" a bit to the left
    ),    color = "black", size = 7, family = "Times New Roman", fontface = "bold", hjust = 0.5
  ) +
  scale_fill_gradientn(
    colors = brewer.pal(9, "Oranges") , # Example palette
    values = rescale(c( -10, 0, 10, 20)),  # Adjust according to data range
    name = "",
    breaks= c(-10,0,10,20),
    limits = c(-15, 25), 
    guide = guide_colorbar(
      barwidth = 20, barheight = 0.5, direction = "horizontal"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    text = element_text(family = "Times New Roman"),  # Use Times New Roman font
    axis.title.x = element_text(size = 20, angle = 0, hjust = 0.5, vjust = 50, face = "bold"),  # Move y-axis title to the top
    axis.title.y = element_text(size = 16, face = "bold"),           # Add y-axis title with size 14
    axis.text.x = element_blank(),   
    axis.text.y = element_text(angle = 45, hjust = 1, size = 16),  # Adjust y-axis text
    legend.position = "top",                          # Legend at the top
    legend.text = element_text(size = 16),
    panel.grid.major = element_blank(),               # Remove major grid lines
    panel.grid.minor = element_blank()   
  ) +
  labs(
    x = "Association with number of achieved social thresholds",
    y = "") +
  
  coord_fixed( ratio = 2) 


# Save the plot
ggsave(
  file = "Analysis/Plots/4b_heat_coefficients_hh_g.png", 
  plot = last_plot(),  
  width = 12, height = 4, bg = "transparent", units = "in", dpi = 300
)




###  heat map comparison 
data_heat_compare <- data_heat |>
  select(-contains("total"), -contains("gov_g"), -contains("hh_g")) |> 
  group_by(thresh_level) |>
  
  pivot_longer(cols = health_envi_factor:admin_factor, names_to = "sector", values_to = "factor") |>
  mutate(sector = str_replace(sector, "health_envi_factor", "Health & Environment"),
         sector = str_replace(sector, "soc_factor", "Social Protection"),
         sector = str_replace(sector, "edu_recr_factor", "Education & Recreation"),
         sector = str_replace(sector, "hous_factor", "Housing"),
         sector = str_replace(sector, "indu_factor", "Industry"),
         sector = str_replace(sector, "ord_def_factor", "Order & Defense"),
         sector = str_replace(sector, "admin_factor", "Public Administration")) |>
  
  mutate(sector = factor(sector, levels = rev(c(  "Education & Recreation",  
                                                  "Social Protection", 
                                                  "Public Administration",    
                                                  "Order & Defense", 
                                                  "Health & Environment",
                                                  "Housing", 
                                                  "Industry"))) ) |>
  filter(sector %in% c("Education & Recreation", "Social Protection", "Health & Environment"))


#View(data_heat_compare)
## plot 

x_axis =  expression(bold(paste("Relative Carbon Intensity of Household vs. Government Spending (Ratio)")))

ggplot(data_heat_compare, aes(x = sector, y = thresh_level, fill = factor)) +
  geom_tile(color = "grey40", linetype = "solid", size = 0.5) +  # Thin borders for tiles
  geom_text(
    aes(label = ifelse(is.na(factor), "NA", 
                       ifelse(factor < 0.01, "<0.01", 
                              ifelse(factor >= 0.01, sprintf("%.2f", factor), sprintf("%g", factor))))),
    color = "black", size = 8, family = "Times New Roman", fontface = "bold"
  ) +
  scale_fill_viridis(
    direction = 1,
    name = "",
    breaks = c(0, 5, 10, 15, 20, 25, 30),  # Adjusted breaks
    labels = function(x) {
      sapply(x, function(value) {
        if (value >= 1) {
          format(round(value, 1), nsmall = 1, scientific = FALSE)  # One decimal place for values >= 1
        } else {
          format(value, trim = TRUE, scientific = FALSE)  # Full precision for values < 1
        }
      })
    },
    limits = c(0, 30),  # Adjusted limits .  lowest values = 0.54
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
  file = "Analysis/Plots/4c_heat_intensity_compare.png", 
  plot = last_plot(),  
  width = 12, height = 12, units = "in", bg = "transparent", dpi = 300
)



### coefficients

# heatmap for social coefficient
data_reg_gov <- tibble(
  sector = c("Health & Environment", "Social Protection", "Education & Recreation", 
             "Housing", "Industry", "Order & Defense", "Public Administration"), 
  coefficient =  c(1.895, 0.954, 4.857, 14.308, 4.673, -4.679, 13.245) ) |>   # gov coefficients  
  
  mutate(sector = factor(sector, levels = rev(c(  "Education & Recreation",  
                                                  "Social Protection", 
                                                  "Public Administration",    
                                                  "Order & Defense", 
                                                  "Health & Environment",
                                                  "Housing", 
                                                  "Industry"))) ) |>
  
  filter(sector %in% c("Education & Recreation", "Social Protection", "Health & Environment"))


ggplot(data_reg_gov, aes(x = sector, y = "Full sample", fill = coefficient)) +
  geom_tile(color = "grey40", size = 0.5) +  # Add borders to the tiles
  geom_text(
    aes(label = format(round(coefficient, 2), scientific = FALSE)),  # Format coefficient values
    color = "black", size = 8, family = "Times New Roman", fontface = "bold"
  ) +
  scale_fill_gradientn(
    colors = brewer.pal(9, "Oranges") , # Example palette
    values = rescale(c(-20, -10, 0, 10, 20, 30)),  # Adjust according to data range
    name = "",
    limits = c(-35, 35), 
    guide = guide_colorbar(
      barwidth = 20, barheight = 0.5, direction = "horizontal"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    text = element_text(family = "Times New Roman"),  # Use Times New Roman font
    axis.title.x = element_text(size = 20, angle = 0, hjust = 0.5, vjust = 50, face = "bold"),  # Move y-axis title to the top
    axis.title.y = element_text(size = 16, face = "bold"),           # Add y-axis title with size 14
    axis.text.x = element_blank(),   
    axis.text.y = element_text(angle = 45, hjust = 1, size = 16),  # Adjust y-axis text
    legend.position = "top",                          # Legend at the top
    legend.text = element_text(size = 16),
    panel.grid.major = element_blank(),               # Remove major grid lines
    panel.grid.minor = element_blank()   
  ) +
  labs(
    x = "Association with number of achieved social thresholds",
    y = ""
  ) +
  annotate("text", x = 1.3, y = 1.1, label = "***", color = "black", size = 4, fontface = "bold", family = "Times New Roman") +
  annotate("text", x = 2.3, y = 1.1, label = "**", color = "black", size = 4, fontface = "bold", family = "Times New Roman") +
  annotate("text", x = 3.3, y = 1.1, label = "***", color = "black", size = 4, fontface = "bold", family = "Times New Roman") +

  coord_fixed( ratio = 2) 


# Save the plot
ggsave(
  file = "Analysis/Plots/4c_heat_gov_coef_compare.png", 
  plot = last_plot(),  
  width = 12, height = 4, bg = "transparent", units = "in", dpi = 300
)


## household (gloria)
data_reg_hh <- tibble(
  sector = c("Health & Environment", "Social Protection", "Education & Recreation", 
             "Housing", "Industry", "Order & Defense", "Public Administration"), 
  coefficient =  c(2.678, -0.988, -0.972, 1.065, -2.386, -8.129, 4.888)) |>  # hh       
                                                                         
  mutate(sector = factor(sector, levels = rev(c(  "Education & Recreation",  
                                                  "Social Protection", 
                                                  "Public Administration",    
                                                  "Order & Defense", 
                                                  "Health & Environment",
                                                  "Housing", 
                                                  "Industry"))) ) |>
  filter(sector %in% c("Education & Recreation", "Social Protection", "Health & Environment"))


ggplot(data_reg_hh, aes(x = sector, y = "Full sample", fill = coefficient)) +
  geom_tile(color = "grey40", size = 0.5) +  # Add borders to the tiles
  geom_text(
    aes(
      label = ifelse(is.na(coefficient), "NA", format(round(coefficient, 2), scientific = FALSE)),  # Replace NA with "NA"
      hjust = ifelse(is.na(coefficient), 0.7, 0.5)  # Shift "NA" a bit to the left
    ),    color = "black", size = 8, family = "Times New Roman", fontface = "bold", hjust = 0.5
  ) +
  scale_fill_gradientn(
    colors = brewer.pal(9, "Oranges") , # Example palette
    values = rescale(c(-20, -10, 0, 10, 20, 30)),  # Adjust according to data range
    name = "",
    #  breaks = seq(-0, 30, by = 10),  # Specify breaks for the legend
    limits = c(-35, 35), 
    guide = guide_colorbar(
      barwidth = 20, barheight = 0.5, direction = "horizontal"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    text = element_text(family = "Times New Roman"),  # Use Times New Roman font
    axis.title.x = element_text(size = 20, angle = 0, hjust = 0.5, vjust = 50, face = "bold"),  # Move y-axis title to the top
    axis.title.y = element_text(size = 16, face = "bold"),           # Add y-axis title with size 14
    axis.text.x = element_blank(),   
    axis.text.y = element_text(angle = 45, hjust = 1, size = 16),  # Adjust y-axis text
    legend.position = "none",                          # Legend at the top
    legend.text = element_text(size = 16),
    panel.grid.major = element_blank(),               # Remove major grid lines
    panel.grid.minor = element_blank()   
  ) +
  labs(
    x = "Association with number of achieved social thresholds",
    y = ""
  ) +
  
  coord_fixed( ratio = 2) 

# Save the plot
ggsave(
  file = "Analysis/Plots/4c_heat_hh_coef_compare.png", 
  plot = last_plot(),  
  width = 12, height = 3, bg = "transparent", units = "in", dpi = 300
)



# put data together for supplementary data. 

data_heat_pivot1 = data_heat |>

  pivot_longer(cols = 2:9, names_to = "category", values_to = "factor") |>
  mutate(category = str_remove(category, "_factor")) |>
  mutate(actor = "gov") |> # factor for gov (used in manuscript)
  select(thresh_level, category, factor, actor) |>
  filter(category %in% c("total", "health_envi", "soc", "edu_recr"))


data_heat_pivot2 = data_heat |>
  
  pivot_longer(cols = 2:9, names_to = "category", values_to = "factor") |>
  mutate(category = str_remove(category, "_factor")) |>
  mutate(actor = "hh",   # factor for hh
         factor = 1/factor) |>
  select(thresh_level, category, factor, actor) |>
  filter(category %in% c("total", "health_envi", "soc", "edu_recr"))


data_heat_pivot3 = data_heat |>
  select(thresh_level, contains("intensity") ) |>
  
  pivot_longer(cols = 2:17, names_to = "type", values_to = "adj_intensity") |>
  mutate(actor = case_when(
    str_detect(type, "gov") ~ "gov",
    str_detect(type, "hh") ~ "hh",
    TRUE ~ NA_character_  
  )) |>
    mutate(type = str_remove_all(type, "adj_|_intensity_gov_g|_intensity_hh_g")) |>
     select(thresh_level, type, actor, adj_intensity) |>
  rename(category = type) |>
     group_by(thresh_level, category) |>
  
  filter(category %in% c("total", "health_envi", "soc", "edu_recr"))


data_reg_pivot = bind_rows(data_reg_gov, data_reg_hh) |>
  mutate(actor = rep(c("gov", "hh"), each = 3), 

  signifance = c("p<0.001", "p<0.01", "p<0.001",
                  "p>0.1", "p>0.1", "p>0.01"),
  sector = c("health_envi", "soc", "edu_recr",
             "health_envi", "soc", "edu_recr") ) |>
  rename(category = sector) |>
  slice(rep(1:n(), each = 4))  |>   #replicate to match intenstiy dataset (e.g., one value for high, one for upper-middle,....)
  mutate(
    thresh_level = rep(c("High", "Upper-middle", "Lower-middle", "Low"), times = n() / 4)
  ) |>
  select(thresh_level, category, actor, everything())


# put all data together for figure 4
data_figure4 <- bind_rows(data_heat_pivot1, data_heat_pivot2) |>
  left_join(data_heat_pivot3, by = c("thresh_level", "category", "actor")) |>
  select(thresh_level, category, actor, contains("intensity"), everything()) |>
  filter(!category %in% "total") |>

 left_join(data_reg_pivot, by = c("thresh_level", "category", "actor"))

#View(data_figure4)








#### data for supplementary data

overview_text <- "\n\nHow government spending shapes eco-social outcomes: A global comparative analysis
\nby XXX
\n\nThis is the supplementary data for the analysis. It contains country-level data on spending (government, household) as well as eco-social outcomes for the period between 2005 and 2019.

\n\nAbbreviations examples:\n\nhealth_gov_spending_tot_wid: health and environment government (as a percentage of total government spending) from the World Inequality Database (WID)\nindu_gov_spending_gdp_wid: industry government spending (as a percentage of GDP) from WID\nadmin_gov_spending_cap_wid: order and defense government spending (in PPP-adjusted US$/cap in constant market prices)\nedu_gov_spending_cap_g: education and recreation government spending (in US$/cap in constant basic prices) from GLORIA (g)\ntotal_hh_spending_cap_g: total household spending (in US$/cap in constant basic prices) from GLORIA (g)
\nexpect: life expectancy at birth (in years)\nschool: average years in school (in years)\nsettle: share of urban population living in formal settlements (in percentage)\ncrime: crime rate - i.e. reserved homicide rate and rescaled (in a score between 0 and 100)\nincome: share of population above regional poverty line (in percentage)\nemploy: inverted employment rate (in percentage)\nsupport: proportion of people with friends or family they can rely on (in percentage)\nnutri: share of population with sufficient amount of food (in kilocalories)\nbirth: share of adolescent birth rate (in percentage)\nair: Inverted annual mean concentration of PM2.5 (in μg/m³)
\nghg_health_envi_gov_g: per-capita carbon footprints (i.e. carbon emission equivalents) associated with government spending in health and environmental protection from GLORIA, ghg_share_edu_recr_hh_g: of total household carbon footprints associated with education and recreation spending from GLORIA
\nord_def_intensity_gov_wid: carbon intensity of government spending in order and defense (in kg CO2e/US$) using WID spending data\nadj_edu_recr_intensity_hh_g: adjusted carbon intensity of household spending in education and recreation (in kg CO2e/US$) using GLORIA spending data

\n\nPlease do not hesitate to contact us if things are unclear: XXX"  # here all intensity data in kg co2/us$


# Split the text into lines based on newline characters
lines <- strsplit(overview_text, "\n")[[1]]

# Create a workbook
wb <- createWorkbook()

# Add a worksheet
addWorksheet(wb, "Overview")

# save 
writeData(wb, "Overview", x = lines, startCol = 1, startRow = 1, colNames = FALSE, overview_text)



#full dataset
addWorksheet(wb, "Full dataset")  # Tab name is 'Sheet1'
writeData(wb, "Full dataset", data_pooled)  # Write your data

# figure 1 + additional information
addWorksheet(wb, "Figure 1 Main Data")  # tab name
writeData(wb, "Figure 1 Main Data", data_figure1)  # save data into respective tab

addWorksheet(wb, "Figure 1 Descriptives")  
writeData(wb, "Figure 1 Descriptives", data_total_var)  

# figure 2
addWorksheet(wb, "Figure 2 Main Data")  
writeData(wb, "Figure 2 Main Data", data_figure2)  


# figure 3 + additional information
addWorksheet(wb, "Figure 3 Main Data")  # tab name
writeData(wb, "Figure 3 Main Data", data_figure3)  # save data into respective tab

addWorksheet(wb, "Figure 3 Descriptives")  
writeData(wb, "Figure 3 Descriptives", data_figure3_addon)  

# figure 4
addWorksheet(wb, "Figure 4 Main Data")  # tab name
writeData(wb, "Figure 4 Main Data", data_figure4)  # save data into respective tab


# Save the workbook to a file.  
saveWorkbook(wb, "Data/Overview/data_supplementary.xlsx", overwrite = TRUE)
saveWorkbook(wb, "Data/SI/data_supplementary.xlsx", overwrite = TRUE)
saveWorkbook(wb, "Article/Supplementary Data/data_supplementary.xlsx", overwrite = TRUE)



















      