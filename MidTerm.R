library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(scales)

data <- read_csv('/Users/kristenpechin/Desktop/MVA_Vehicle_Sales_Counts_by_Month_for_Calendar_Year_2002_through_December_2023.csv')

names(data)[names(data) == 'Year '] <- 'Year'

yearly_sales <- data %>%
  group_by(Year) %>%
  summarize(`Total Sales New` = sum(`Total Sales New`, na.rm = TRUE),
            `Total Sales Used` = sum(`Total Sales Used`, na.rm = TRUE))

years_to_plot <- c(2019, 2020, 2021, 2022)
sales_to_plot <- yearly_sales %>%
  filter(Year %in% years_to_plot)

sales_to_plot_melted <- sales_to_plot %>%
  pivot_longer(cols = c(`Total Sales New`, `Total Sales Used`),
               names_to = "Sales_Type",
               values_to = "Total_Sales")

ggplot(sales_to_plot_melted, aes(x = factor(Year), y = Total_Sales, fill = Sales_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c('skyblue', 'steelblue')) +
  ggtitle('Comparison of New and Used Vehicle Sales (2019-2022)') +
  xlab('Year') +
  ylab('Total Sales') +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
