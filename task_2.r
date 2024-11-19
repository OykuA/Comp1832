library(readxl)
library(dplyr)
library(ggplot2)

file_path <- "cw_r.xlsx"
table_5 <- read_excel(file_path, sheet = "Table 5", skip = 9)
View(table_5)

table_5 <- table_5 %>%
  filter(!is.na(`Number of offences`) & !is.na(`Rate per 1,000 population`))

print(head(table_5))
print(colnames(table_5))
print(unique(table_5$`Area Name`))

table_5_cleaned <- table_5 %>%
  filter(!`Area Name` %in% c("ENGLAND AND WALES [note 13]", "ENGLAND"))

table_5_cleaned <- table_5_cleaned %>%
  mutate(
    `Number of offences` = as.numeric(`Number of offences`),
    `Rate per 1,000 population` = as.numeric(`Rate per 1,000 population`)
  )
print(head(table_5_cleaned))
View(table_5_cleaned)
#SEPERATE COUNTIES AND REGIONS


region_names <- c(  "North East", "North West", "Yorkshire and The Humber", 
  "East Midlands", "West Midlands", "East", 
  "London [note 14]", "South East", "South West", "WALES")
# table_5 <- table_5 %>%
#   mutate(is_region_or_county = ifelse(`Area Name` %in% region_names, TRUE, FALSE))
regions_data <- table_5_cleaned %>% filter(`Area Name` %in% region_names) # ` and ' has different purposes
counties_data <- table_5_cleaned %>% filter(!(`Area Name` %in% region_names))

print(head(regions_data))
print(head(counties_data))

#ANALYSE THE REGIONS

regions_summ <- regions_data %>%
  group_by(`Area Name`) %>%
  summarise(
    Total_Offences = sum(`Number of offences`, na.rm = TRUE),
    Avg_Rate = mean(`Rate per 1,000 population`, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Offences))

print(regions_summ)
View(regions_summ)
ggplot(regions_summ, aes(x = `Area Name`, y = `Total_Offences`, fill = `Area Name`)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_point(aes(y = Avg_Rate * 10000 ), color = "black", size = 3)+
    geom_line(aes(y = Avg_Rate * 10000, group = 1 ), color = "grey", size = 3)
    labs(title = "Distribution of Total offences by Area", 
          x = "Area Name", y = "Total_Offences") +
    
     theme_minimal()
ggsave("task_2_bar.png", width = 6, height = 4, dpi = 300)

highest_offences <- regions_summ %>%
  filter(Total_Offences == max(Total_Offences))

highest_rate <- regions_summ %>%
  filter(Avg_Rate == max(Avg_Rate))

print("Region with highest total offences:")
print(highest_offences)

print("Region with highest offence rate per 1000 population:")
print(highest_rate)


#ANALYSE THE COUNTIES

counties_summ <- counties_data %>%
  group_by(`Area Name`) %>%
  summarise(
    Total_Offences = sum(`Number of offences`, na.rm = TRUE),
    Avg_Rate = mean(`Rate per 1,000 population`, na.rm = TRUE)
  ) %>%
  arrange(Total_Offences)
print(counties_summ)
View(counties_summ)

lowest_offences <- counties_summ %>% head(3)
print("Top 3 counties with the lowest total offences:")
print(lowest_offences)


ggplot(lowest_offences, aes(x = reorder(`Area Name`, Total_Offences), y = Total_Offences, fill = `Area Name`)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_point(aes(y = Avg_Rate), color = "black", size = 3) +
  geom_line(aes(y = Avg_Rate, group = 1), color = "grey", size = 1) +
  labs(
    title = "Top 3 Counties with the Lowest Total Offences",
    x = "County Name",
    y = "Total Offences / Offence Rate (per 1000)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("task_2_lowest_counties.png", width = 6, height = 4, dpi = 300)