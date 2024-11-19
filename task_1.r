library(readxl)
library(dplyr)
library(ggplot2)

#LOADING THE XLSX FILE
file_path <- "cw_r.xlsx"
table_3d <- read_excel(file_path, sheet = "Table 3d", skip = 8)
View(table_3d)
colnames(table_3d)<- c("Fraud_Type", "2012-2013", "2013-2014", "2014-2015", "2015-2016", 
            "2016-2017", "2017-2018", "2018-2019", "2019-2020", 
            "2020-2021", "2021-2022", "total_change")
table_3d_cleaned <- table_3d %>% filter(!is.na(Fraud_Type))   
str(table_3d_cleaned)
#FIRST SUBSET
charity_frauds <- table_3d_cleaned %>%
     filter(grepl("Charity fraud", Fraud_Type))
     
#SECOND SUBSET
pension_liberation_frauds <- table_3d_cleaned %>%
     filter(grepl("Pension liberation fraud", Fraud_Type))

years <- c("2012-2013", "2013-2014", "2014-2015", "2015-2016", 
           "2016-2017", "2017-2018", "2018-2019", "2019-2020", 
           "2020-2021", "2021-2022")

pension_values <- as.numeric(pension_liberation_frauds[1, years])
charity_values <- as.numeric(charity_frauds[1, years])

#DATAFRAME FOR TWO TYPES OF FRAUDS
plot_data <- tibble(
    Year = rep(years, 2),
    Case = c(pension_values, charity_values),
    Category = rep(c("Pension Frauds", "Charity Frauds"), each = length(years))
)

#BAR PLOT
ggplot(plot_data, aes(x = Category, y = Case, fill = Category)) +
     geom_bar(stat = "identity", position = "dodge") +
     labs(title = "Distribution of Fraud Cases", 
          x = "Fraud Type", y = "Number of Cases") +
     theme_minimal()

#BOXPLOT
ggplot(plot_data, aes(x = Category, y = Case, fill = Category)) +
     geom_boxplot() +
     labs(title = "Distribution of Fraud Cases", 
          x = "Fraud Type", y = "Number of Cases") +
     theme_minimal()

#LINE PLOT
ggplot(plot_data, aes(x = Year, y = Case, color= Category, group = Category)) +
     geom_line(size = 1) +
     geom_point(size = 3)+
     labs(title = "Fraud Cases Over the Years", x = "Year", y = "Cases", color = "Fraud Type")+
     theme_minimal()+
     theme(axis.text.x = element_text(angle = 45, hjust = 1))

head(plot_data)
print(plot_data)
str(plot_data)

head(charity_frauds)
str(charity_frauds)

head(pension_liberation_frauds)
str(pension_liberation_frauds)

summary(charity_values)
summary(pension_values)
