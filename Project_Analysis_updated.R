# Define custom colors for the bars
my_colors <- c("#ff7f0e", "#1f77b4", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")



#Libraries
library(readr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyverse)
library(gridExtra)
library(highlight)
library(formattable)
library(cowplot)

#Reading csv file
tech_layoffs = read.csv("C:/Users/rcher/Documents/Humber work/Semester 4/Big Data 2/Midterm/tech_layoffs.csv")
tech_layoffs

tech_layoffs_unclear = read.csv("C:/Users/rcher/Documents/Humber work/Semester 4/Big Data 2/Midterm/tech_layoffs_unclear.csv")
tech_layoffs_unclear
#Visualizations

######################################################################################################
#1st Analysis
######################################################################################################

#1-a.) #Rank() function to calculate the rank of each company based on the number of layoffs. 

# Creating a dataset of popular technology company layoffs
tech_layoffs_1 <- data.frame(
  company = c("Alphabet", "Netflix", "Microsoft", "Amazon", "Meta"),
  total_layoffs = c(12000, 480, 10000, 18000, 11000)
)

# Converting the layoffs column to a numeric type
tech_layoffs_1$layoffs <- as.numeric(tech_layoffs_1$total_layoffs)

# Using the rank function to calculate the rank of each company based on the number of layoffs
tech_layoffs_1$rank <- rank(tech_layoffs_1$total_layoffs)

# Sort the data frame in descending order based on the layoffs column
tech_layoffs_1 <- tech_layoffs_1[order(-tech_layoffs_1$total_layoffs),]

# Create a bar chart using ggplot2
ggplot(tech_layoffs_1, aes(x = reorder(company, -total_layoffs), y = total_layoffs)) +
  geom_bar(stat = "identity", fill = "pink") +
  geom_text(aes(label = layoffs), vjust = -0.5) +
  labs(title = "Technology Company Layoffs (2022-2023)", 
       x = "Company", y = "Number of Layoffs") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))



# 1b. Industry layoffs
cols_to_select <- c("industry", "total_layoffs")
tech_layoffs_1 <- tech_layoffs[cols_to_select] %>% arrange(desc(total_layoffs)) %>%  slice(1:10)
tech_layoffs_1

# Plot the data using geom_col(), and specify custom colors with scale_fill_manual()
ggplot(tech_layoffs_1, aes(y = industry, x = total_layoffs, fill = industry)) +
  geom_col(stat="industry", fill = "#17becf")

# 2. Which company has hightest layoffs in E-commerce industry
# Filter the data for E-commerce industry, arrange the data by Total_Layoffs and select the top row
ecom_max_layoffs <- tech_layoffs %>%
  filter(industry %in% c("E-commerce, SaaS","E-commerce")) %>%
  arrange(desc(total_layoffs)) %>%
  slice_max(order_by = total_layoffs,n = 10)

# Print the result
print(ecom_max_layoffs)

# Create a bar chart
ggplot(ecom_max_layoffs, aes(x = company, y = total_layoffs, fill = company)) +
  geom_bar(stat = "identity", fill ="#bcbd22") +
  ggtitle("Company with Highest Layoffs in E-commerce Industry") +
  xlab("Company Name") +
  ylab("Total Layoffs") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5)) 

######################################################################################################
#2nd Analysis
######################################################################################################

#1.Count of companies by Status

my_colors <- c("#66cdaa", "#005f56")
status_plot <- tech_layoffs %>%
  count(status) %>%
  ggplot(aes(x = status, y = n, fill = status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = my_colors) +
  ggtitle("Count of Companies by Status") +
  xlab("Status") +
  ylab("Count") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  geom_text(aes(label = n), vjust = -0.5)

# Print the result
print(status_plot)

#2.Layoffs by year
my_colors2 <- c("#9acd32", "#228b22")
tech_layoffs$date <- mdy(tech_layoffs$reported_date)
tech_layoffs$year <- lubridate::year(tech_layoffs$date)
tech_layoffs$month <- month(tech_layoffs$date,label = TRUE, abbr = FALSE)
tech_layoffs$week <- wday(tech_layoffs$date,label = TRUE)
tech_layoffs$impacted_workforce_percentage <-as.numeric(tech_layoffs$impacted_workforce_percentage)

#layoffs by year under each status
by_year <- tech_layoffs %>%
  group_by(year, status) %>%
  summarise(total = sum(total_layoffs)) %>%
  ungroup()

#arrange both the plots
grid.arrange(status_plot,ggplot(by_year,aes(y=total, x=status,fill= status))+geom_col()+facet_wrap(~year)+
               geom_text(aes(label = total), vjust = -0.5)+
               scale_fill_manual(values = my_colors2)
             , ncol = 2)

#3.Layoff by months 

year_month_layoffs <- tech_layoffs %>%
  filter(year == 2022) %>%
  group_by(month) %>%
  summarize(total_layoffs = sum(total_layoffs))

ggplot(year_month_layoffs, aes(x = "", y = total_layoffs, fill = month)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start=0) +
  labs(title=" Total Layoffs in each month of 2022", fill = "Month") +
  theme_void()

#4.companies that had layoffs in January, 2023

df_jan_layoffs <- tech_layoffs %>% 
  filter(month == "January" & year == 2023 & total_layoffs > 0 & status == "Public") %>% 
  select(company, industry, status, total_layoffs) %>% 
arrange(desc(total_layoffs))

tech_layoffs %>% filter(month == "January" & year == 2023 & status == "Public") %>% 
arrange(desc(total_layoffs)) %>% 
slice(1:10) %>%
ggplot(aes(x=company, y=total_layoffs, group=1)) +
geom_line() +
geom_point() +
ggtitle("Top Companies with Layoffs in January, 2023") +
  ylab("Total Layoffs") +
  xlab("Company") +
 theme(axis.text.x = element_text(angle = 45, hjust = 1))

#companies that had layoffs in November, 2022

df_nov_layoffs <- tech_layoffs %>% 
  filter(month == "November" & year == 2022 & total_layoffs > 0) %>% 
  select(company, industry, status, total_layoffs) %>% 
  arrange(desc(total_layoffs))

tech_layoffs %>% filter(month == "November" & year == 2022) %>% 
  arrange(desc(total_layoffs)) %>% 
  slice(1:20) %>%
  ggplot(aes(x=company, y=total_layoffs, group=1)) +
  geom_line() +
  geom_point() +
  ggtitle("Companies with Layoffs in November, 2022") +
  ylab("Total Layoffs") +
  xlab("Company") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#5. Headquarter Locations with overall Highest Impacted Workforce  
impacted_headquarters <- tech_layoffs %>% 
  group_by(headquarter_location) %>% 
  summarise(total_impacted = sum(total_layoffs)) %>% 
  arrange(desc(total_impacted)) %>% 
  head(10)

ggplot(impacted_headquarters, 
       aes(y = reorder(headquarter_location, total_impacted), 
           x = total_impacted, fill = headquarter_location)) +
  geom_bar(stat = "identity", fill = "#005F56") +
  geom_text(aes(label = total_impacted), vjust = -0.5)+
  labs(y = "Headquarter Location", x = "Impacted Workforce", 
       title = "Headquarter Locations with Overall Highest Impacted Workforce") +
  theme(plot.title = element_text(hjust = 0.5))


#6.A new dataframe with the counts of each source
source_counts_clear <- tech_layoffs_unclear %>% 
  filter(impacted_workforce_percentage != "Unclear" & total_layoffs != "Unclear") %>%
  group_by(sources) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(15)

#chart-1
chart1 <- ggplot(source_counts_clear, aes(x = count, y = reorder(sources, count))) + 
  geom_col(fill = "#00A896") +
  geom_text(aes(label = count), vjust = -0.5)+
  labs(x = "Count", y = "Sources", title = "Top Sources for the Clear Layoff Data") +
  theme_minimal()

#7. Sources that has given atmost Unclear data about the layoffs
source_counts <- tech_layoffs_unclear %>% 
  filter(impacted_workforce_percentage == "Unclear" | total_layoffs == "Unclear") %>%
  group_by(sources) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(15)

#chart-2
chart2 <- ggplot(source_counts, aes(x = count, y = reorder(sources, count))) + 
  geom_col(fill = "orange") +
  geom_text(aes(label = count), vjust = -0.5)+
  labs(x = "Count", y = "Sources", title = "Sources giving out Unclear Layoff Data") +
  theme_minimal()

# merge the two charts together
combined_chart <- plot_grid(chart1, chart2, ncol = 2)

#7. scatterplot between impacted_workforce and total_layoffs

scatter_data <- tech_layoffs %>%
  select(company, impacted_workforce_percentage, total_layoffs) %>%
  arrange(desc(total_layoffs)) 

ggplot(scatter_data, aes(x = impacted_workforce_percentage, y = total_layoffs)) +
    geom_point() +
    labs(x = "Impacted Workforce", y = "Total Layoffs", 
         title = "Scatterplot of Impacted Workforce vs Total Layoffs")
###################################################################################################################
#Analysis4:
###################################################################################################################
# Aggregate the data by headquarters and sum the layoffs
layoffs_by_headquarters <- aggregate(total_layoffs ~ headquarter_location, data = tech_layoffs, sum)


# Install and load the treemap package
install.packages("treemap")
library(treemap)

# Create a treemap
treemap(layoffs_by_headquarters, index = "headquarter_location", vSize = "total_layoffs", 
        type = "index", palette = "Set3", title = "Layoffs by Headquarters")



#########################################################################################3
# Analysis5: To display the highest layoff as per the industry along with their status
# Load the necessary libraries
library(dplyr)
library(ggplot2)

# Group the data by industry and status, and summarize the total layoffs
layoffs_by_industry_status <- tech_layoffs %>% 
  group_by(industry, status) %>% 
  summarize(total_layoffs = sum(total_layoffs))

# Sort the data by total layoffs in descending order
layoffs_by_industry_status <- layoffs_by_industry_status %>% 
  arrange(desc(total_layoffs))

# Select the top 5 industries by layoffs
top_industries <- head(layoffs_by_industry_status, 10)

# Create a pie chart of the top industries by layoffs
ggplot(top_industries, aes(x = "", y = total_layoffs, fill = status)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start=0) +
  facet_wrap(~ industry) +
  theme_void() +
  labs(title = "Top 5 Industries by Layoffs",
       subtitle = "Pie chart showing the distribution of layoffs by status for each industry")





























  