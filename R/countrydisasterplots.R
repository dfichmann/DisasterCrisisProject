####### PLOTS PLOTS PLOTS ############

####PANEL COUNTRY PLOTS ####
plots_dir <- "plots/" 
dir.create(plots_dir, showWarnings = FALSE)
# Filter data for selected countries and years
selected_countries <- c("CHE", "ITA", "USA", "JPN")  # Change this to select your countries

# Group data by country and year
grouped_data <- filter(MyData, ISO %in% selected_countries, year >= 1970) %>% 
  group_by(ISO, year)

grouped_data$stnDAM <- grouped_data$stnDAM/10

# Create a line plot for each country and year
plot_list <- list()

for (country_name in selected_countries) {
  country_data <- grouped_data %>% 
    filter(ISO == country_name)
  
  plot3 <- ggplot(country_data, aes(x = year, y = stnDAM)) +
    geom_line() +
    geom_vline(aes(xintercept = year), data = filter(country_data, crisisJST == 1),
               color = "red", 
               linetype = "dashed", 
               linewidth = 0.8) +
    #geom_vline(aes(xintercept = year), data = filter(country_data, RC == 1),
    #           color = "blue", 
    #           linetype = "dotted", 
    #           linewidth = 0.5) +
    geom_rect(aes(xmin = year - 0.5, xmax = year + 0.5, ymin = -Inf, ymax = Inf), data = filter(country_data, dangerzoneJST == 1),
              fill = "red", alpha = 0.1) +
    labs(title = country_name,
         x = "Year",
         y = "Damage (%GDP)") +
    theme_bw()
  
  plot_list[[country_name]] <- plot3
}

panel_plot <- wrap_plots(plot_list, ncol = 2)

# Save panel graph as a file
ggsave(paste0(plots_dir, "panel_damage_crisis.png"), width = 12, height = 6)


#### ANNEX PLOTS ####
plots_dir <- "plots/" 
dir.create(plots_dir, showWarnings = FALSE)
# Filter data for selected countries and years
selected_countries <- c("AUS", "BEL", "CAN", "CHE", "DEU", "DNK", "ESP", "FIN", "FRA")  # Change this to select your countries

# Group data by country and year
grouped_data <- filter(MyData, ISO %in% selected_countries, year >= 1900) %>% 
  group_by(ISO, year)

grouped_data$stnDAM <- grouped_data$stnDAM/10

# Create a line plot for each country and year
plot_list <- list()

for (country_name in selected_countries) {
  country_data <- grouped_data %>% 
    filter(ISO == country_name)
  
  plot3 <- ggplot(country_data, aes(x = year, y = stnDAM)) +
    geom_line() +
    geom_vline(aes(xintercept = year), data = filter(country_data, crisisJST == 1),
               color = "red", 
               linetype = "dashed", 
               linewidth = 0.8) +
    #geom_vline(aes(xintercept = year), data = filter(country_data, RC == 1),
    #           color = "blue", 
    #           linetype = "dotted", 
    #           linewidth = 0.5) +
    geom_rect(aes(xmin = year - 0.5, xmax = year + 0.5, ymin = -Inf, ymax = Inf), data = filter(country_data, dangerzoneJST == 1),
              fill = "red", alpha = 0.1) +
    labs(title = country_name,
         x = "Year",
         y = "Damage (%GDP)") +
    theme_bw()
  
  plot_list[[country_name]] <- plot3
}

panel_plot <- wrap_plots(plot_list, ncol = 2)

# Save panel graph as a file
ggsave(paste0(plots_dir, "panel_damage_crisis_all1.png"), width = 12, height = 15)



#### ANNEX PLOTS ####
plots_dir <- "plots/" 
dir.create(plots_dir, showWarnings = FALSE)
# Filter data for selected countries and years
selected_countries <- c("GBR", "IRL", "ITA", "JPN", "NLD", "NOR", "PRT", "SWE", "USA")  # Change this to select your countries

# Group data by country and year
grouped_data <- filter(MyData, ISO %in% selected_countries, year >= 1900) %>% 
  group_by(ISO, year)

grouped_data$stnDAM <- grouped_data$stnDAM/10

# Create a line plot for each country and year
plot_list <- list()

for (country_name in selected_countries) {
  country_data <- grouped_data %>% 
    filter(ISO == country_name)
  
  plot3 <- ggplot(country_data, aes(x = year, y = stnDAM)) +
    geom_line() +
    geom_vline(aes(xintercept = year), data = filter(country_data, crisisJST == 1),
               color = "red", 
               linetype = "dashed", 
               size = 0.8) +
    #geom_vline(aes(xintercept = year), data = filter(country_data, RC == 1),
    #           color = "blue", 
    #           linetype = "dotted", 
    #           size = 0.5) +
    geom_rect(aes(xmin = year - 0.5, xmax = year + 0.5, ymin = -Inf, ymax = Inf), data = filter(country_data, dangerzoneJST == 1),
              fill = "red", alpha = 0.1) +
    labs(title = country_name,
         x = "Year",
         y = "Damage (%GDP)") +
    theme_bw()
  
  plot_list[[country_name]] <- plot3
}

panel_plot <- wrap_plots(plot_list, ncol = 2)

suppressWarnings(
# Save panel graph as a file
  ggsave(paste0(plots_dir, "panel_damage_crisis_all2.png"), width = 12, height = 15)
)
################################################################################################

# DISASTER BY YEAR PLOTS #####################
fill_colors <- c("Storm" = "#377EB8", "Wildfire" = "#4DAF4A", "Earthquake" = "#F781BF", "Volcanic activity" = "#A65628", 
                 "Drought" = "#E41A1C", "Flood" = "#984EA3", "Extreme temperature" = "#FFFF33", "Epidemic" = "#FF7F00")

# Convert Year column to numeric
EmDat$Year <- as.numeric(EmDat$Year)

# Filter dataset to only include specified disaster types and countries
disaster_types <- c("Earthquake", "Storm", "Volcanic activity","Wildfire")
countries <- unique(JST$ISO)
EmDat_filtered <- EmDat %>%
  filter(DisasterType %in% disaster_types & ISO %in% countries)

# Create a summary table with the number of disasters by year and disaster type
disasters_by_year_type <- EmDat_filtered %>%
  group_by(Year, DisasterType) %>%
  summarize(n = n())

# Specify the order of the levels in the DisasterType factor
disasters_by_year_type$DisasterType <- factor(disasters_by_year_type$DisasterType,
                                              levels = c("Storm","Wildfire", "Earthquake", "Volcanic activity"))

# Create a summary table with the total number of disasters by year
disasters_by_year_total <- disasters_by_year_type %>%
  group_by(Year) %>%
  summarize(total = sum(n))

# Create a scatter plot with points colored by disaster type and a smooth line showing the total number of disasters by year
p1 <- ggplot(disasters_by_year_type, aes(x = Year, y = n)) +
  geom_point(aes(color = DisasterType), alpha = 0.6, size = 2) +
  xlab("Year") + ylab("No. of Disasters") +
  theme_bw() +
  theme(legend.position = "right") +
  ggtitle("A")+
  stat_smooth(data = disasters_by_year_total, aes(x = Year, y = total), se = FALSE, color = "black", size = 0.5) +
  scale_color_manual(values = fill_colors)+
  labs(color = "")
p1
p4 <- ggplot(disasters_by_year_type, aes(x = Year, y = n)) +
  geom_area(aes(fill = DisasterType), alpha = 0.4, size = 2) +
  xlab("Year") + ylab("No. of Disasters") +
  theme_bw() +
  theme(legend.position = "right") +
  ggtitle("A")+
  stat_smooth(data = disasters_by_year_total, aes(x = Year, y = total), se = FALSE, color = "black", size = 0.5) +
  scale_fill_manual(values = fill_colors)+
  labs(color = "")
p4



# Filter dataset to only include specified disaster types and countries
disaster_types <- c("Storm","Wildfire", "Earthquake", "Volcanic activity", "Drought", "Flood", "Extreme temperature", "Epidemic")
countries <- unique(JST$ISO)
EmDat_filtered <- EmDat %>%
  filter(DisasterType %in% disaster_types & ISO %in% countries)

# Create a summary table with the number of disasters by year and disaster type
disasters_by_year_type <- EmDat_filtered %>%
  group_by(Year, DisasterType) %>%
  summarize(n = n())

# Specify the order of the levels in the DisasterType factor
disasters_by_year_type$DisasterType <- factor(disasters_by_year_type$DisasterType,
                                              levels = c("Storm","Wildfire", "Earthquake", "Volcanic activity", "Drought", "Flood", "Extreme temperature", "Epidemic"))

# Create a summary table with the total number of disasters by year
disasters_by_year_total <- disasters_by_year_type %>%
  group_by(Year) %>%
  summarize(total = sum(n))

# Create a scatter plot with points colored by disaster type and a smooth line showing the total number of disasters by year
p2 <- ggplot(disasters_by_year_type, aes(x = Year, y = n)) +
  geom_point(aes(color = DisasterType), alpha = 0.6, size = 2) +
  xlab("Year") + ylab("No. of Disasters") +
  theme_bw() +
  theme(legend.position = "right") +
  ggtitle("B")+
  stat_smooth(data = disasters_by_year_total, aes(x = Year, y = total), se = FALSE, color = "black", size = 0.5) +
  scale_color_manual(values = fill_colors)+
  labs(color = "")
p5 <- ggplot(disasters_by_year_type, aes(x = Year, y = n)) +
  geom_area(aes(fill = DisasterType), alpha = 0.4, size = 2) +
  xlab("Year") + ylab("No. of Disasters") +
  theme_bw() +
  theme(legend.position = "right") +
  ggtitle("B")+
  stat_smooth(data = disasters_by_year_total, aes(x = Year, y = total), se = FALSE, color = "black", size = 0.5) +
  scale_fill_manual(values = fill_colors)+
  labs(color = "")
p5


file_path <- "plots/disasters_overtime_dots.png"
png(file_path, width = 11, height = 5, units = "in", res = 300)
grid.arrange(p1, p2, ncol = 2)
dev.off()

file_path <- "plots/disasters_overtime_stacks.png"
png(file_path, width = 11, height = 5, units = "in", res = 300)
grid.arrange(p4, p5, ncol = 2)
dev.off()

# CRISES BY YEAR PLOTS #####################

# Filter dataset to only include specified disaster types and countries
countries <- unique(JST$ISO)
JST_filtered <- JST %>%
  filter(crisisJST == 1 & Yr>=1900)

# Create a summary table with the number of disasters by year and disaster type
crises_by_year_type <- JST_filtered %>%
  group_by(Yr) %>%
  summarize(n = n())

# Create a scatter plot with points colored by disaster type and a smooth line showing the total number of disasters by year
p3 <- ggplot(crises_by_year_type, aes(x = Yr, y = n)) +
  geom_bar(stat = "identity")+
  #geom_point(alpha = 0.6, size = 2) +
  xlab("Year") + ylab("No. of Crises") +
  theme_bw() +
  theme(legend.position = "right") +
  #stat_smooth(data = crises_by_year_type, aes(x = Yr, y = n), se = FALSE, color = "black", size = 0.5) +
  labs(color = "")

p3
ggsave("plots/JST_crises_over_time.png", width = 14, height = 7)

############ plots for each country
fill_colors <- c("Storm" = "#377EB8", "Wildfire" = "#4DAF4A", "Earthquake" = "#F781BF", "Volcanic activity" = "#A65628", 
                 "Drought" = "#E41A1C", "Flood" = "#984EA3", "Extreme temperature" = "#FFFF33", "Epidemic" = "#FF7F00")
# Convert Year column to numeric
EmDat$Year <- as.numeric(EmDat$Year)

# Filter dataset to only include specified disaster types and countries
disaster_types <- c("Storm","Wildfire", "Earthquake", "Volcanic activity", "Drought", "Flood", "Extreme temperature", "Epidemic")
countries <- unique(JST$ISO)
EmDat_filtered <- EmDat %>%
  filter(DisasterType %in% disaster_types & ISO %in% countries)

# Create a list to store the plots
plots <- list()

# Loop through each country
for (country in c("ITA", "JPN", "AUS","DEU")) {
  # Filter the data for the current country
  EmDat_country <- EmDat_filtered %>%
    filter(ISO == country)
  
  # Create a summary table with the number of disasters by year and disaster type for the current country
  disasters_by_year_type_country <- EmDat_country %>%
    group_by(Year, DisasterType) %>%
    summarize(n = n())
  
  # Specify the order of the levels in the DisasterType factor
  disasters_by_year_type_country$DisasterType <- factor(disasters_by_year_type_country$DisasterType,
                                                        levels = c("Storm","Wildfire", "Earthquake", "Volcanic activity", "Drought", "Flood", "Extreme temperature", "Epidemic"))
  
  # Create a summary table with the total number of disasters by year for the current country
  disasters_by_year_total_country <- disasters_by_year_type_country %>%
    group_by(Year) %>%
    summarize(total = sum(n))
  
  # Create a scatter plot with points colored by disaster type and a smooth line showing the total number of disasters by year for the current country
  p <- ggplot(disasters_by_year_type_country, aes(x = Year, y = n)) +
    geom_area(aes(fill = DisasterType), alpha = 0.6, size = 2) +
    xlab("Year") + ylab("No. of Disasters") +
    theme_bw() +
    theme(legend.position = "right") +
    ggtitle(country)+
    stat_smooth(data = disasters_by_year_total_country, aes(x = Year, y = total), se = FALSE, color = "black", size = 0.5) +
    scale_fill_manual(values = fill_colors)
    
  # Add the plot to the list of plots
  plots[[country]] <- p
}

panel_plot <- wrap_plots(plots, ncol = 2)
ggsave(paste0(plots_dir, "panel_damage_disaster_by_country.png"), width = 9, height = 6)
panel_plot

# Create crisis vs disaster plot

#Count everytime RC=1 by year for MyData dataset:
crisis_summary <- aggregate(RC ~ year_var, data = MyData, FUN = sum)
crisis_summary$year_var <- as.numeric(crisis_summary$year_var)

crisis_plot <- ggplot(data = crisis_summary, aes(x = year_var, y = RC)) +
  geom_col(aes(fill = RC), alpha = 0.6) + 
  labs(title = "Crises 1900 - 2016") +
  xlab("Year") + ylab("No. of crises") +
  theme_bw() +
  scale_fill_gradient(name = "# Crises   ",low = "#377EB8", high = "#E41A1C")+
  stat_smooth(se = FALSE, color = "#373232", size = 1.5)



disaster_summary <- aggregate(DisDummy ~ year_var, data = MyData, FUN = sum)
disaster_summary$year_var <- as.numeric(disaster_summary$year_var)

disaster_plot <- ggplot(data = disaster_summary, aes(x = year_var, y = DisDummy)) +
  geom_col(aes(fill = DisDummy), alpha = 0.6) + 
  labs(title = "Disasters 1900 - 2016") +
  xlab("Year") + ylab("No. of disasters") +
  theme_bw() +
  stat_smooth(se = FALSE, color = "#373232", size = 1.5) +
  scale_fill_gradient(name = "# Disaster",low = "#377EB8", high = "#E41A1C")

disaster_plot


# Combine the crisis and disaster data into a single data frame
# Arrange the crisis and disaster plots in a grid
vsplot <- grid.arrange(crisis_plot, disaster_plot, ncol = 1)
ggsave(paste0(plots_dir, "disaster_vs_crisis.png"),plot = vsplot, width = 10, height = 5)
vsplot


# Rename the DisDummy column in disaster_summary to RC
names(disaster_summary)[names(disaster_summary) == "DisDummy"] <- "RC"

# Combine the crisis and disaster data into a single data frame
combined_data <- rbind(
  cbind(crisis_summary, event_type = "Crisis"),
  cbind(disaster_summary, event_type = "Disaster")
)


# Create a plot showing the number of disasters and crises by year
combined_plot <- ggplot(data = combined_data, aes(x = year_var, y = RC)) +
  geom_point(alpha = 0.6, size = 1) + 
  labs(title = "Crises and Disasters 1900 - 2016") +
  xlab("Year") + ylab("No. of events") +
  theme_bw() +
  stat_smooth(se = FALSE, color = "darkred") +
  scale_color_brewer(palette="Set1") +
  facet_wrap(~ event_type)

combined_plot
