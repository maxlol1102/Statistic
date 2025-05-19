#CLEANING DATA
# Load necessary libraries
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readxl")
install.packages("writexl")

# Load the libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(writexl)
library(tidyr)
# Load the data from an Excel file 
data <- read_excel("C:/Users/ADMIN/Downloads/MS/CLASSES/STATISTIC/experiment_data.xlsx")

# Check the structure of the data
str(data)

# Remove "total" rows (assuming they are labeled as "Over-all" in the RPM column)
data_filtered <- data %>% filter(!is.na(RPM))

# Convert Shape and RPM to factors
data_filtered$Shape <- factor(data_filtered$Shape, levels = c("Circle", "Square", "Triangle"))
data_filtered$RPM <- factor(data_filtered$RPM, levels = c(10, 15, 20, 25, 30, 35, 40, 45, 50), ordered = TRUE)



#Descriptive Statistics
# Descriptive statistics for each shape and RPM level
summary_data <- data_filtered %>%
  group_by(Shape, RPM) %>%
  summarise(Users_Mean = mean(Users_M),
            Users_SD = sd(Users_M),
            Nonusers_Mean = mean(Nonusers_M),
            Nonusers_SD = sd(Nonusers_M),
            F_value = mean(F), 
            p_value = mean(p))

# View the data
print(summary_data)



# Visualization: Plot Users' and Nonusers' Mean Performance by RPM
ggplot(data_filtered, aes(x = RPM, y = Users_M, color = Shape)) +
  geom_point() +
  geom_line() +
  labs(title = "Performance of Users by RPM and Shape",
       x = "RPM",
       y = "Mean Time on Target",
       color = "Shape") +
  theme_minimal()

ggplot(data_filtered, aes(x = RPM, y = Nonusers_M, color = Shape)) +
  geom_point() +
  geom_line() +
  labs(title = "Performance of Nonusers by RPM and Shape",
       x = "RPM",
       y = "Mean Time on Target",
       color = "Shape") +
  theme_minimal()




# Line Plot for Users and Non-users across RPM for each Shape
data_long <- data_filtered %>%
  gather(key = "Group", value = "Performance", Users_M, Nonusers_M) %>%
  mutate(Group = factor(Group, levels = c("Users_M", "Nonusers_M"), labels = c("Users", "Non-users")))

# Line Plot comparing Users and Non-users across RPM for each Shape in 1 graph
ggplot(data_long, aes(x = RPM, y = Performance, color = Group, group = interaction(Shape, Group))) +
  geom_line() +
  geom_point() +
  facet_wrap(~Shape) +  # Creates separate plots for each shape
  labs(title = "Performance Between Video Game Users and Non-users across RPMs",
       x = "RPM (Speed)",
       y = "Mean Time on Target",
       color = "Group") +
  theme_minimal()





