#import libraries
library(dplyr)
library(e1071)  # For skewness calculation
library(ggplot2)

#import data
covid_data <- read.csv("data/covid_processed.csv")

#data processing
names(covid_data) <- gsub(" ", "_", names(covid_data))
print(names(covid_data))
#cahnge data columns
# Assuming your dataframe is named df
names(covid_data) <- c(
  "Country",
  "GDP_Per_Capita",
  "Population_Density",
  "Total_Alcohol_Consumption_Per_Capita",
  "Suicide_Rate",
  "Continent",
  "Deaths_From_Smoking",
  "Drug_Legalization_Medical",  # Binary: 1 for yes, 0 for no
  "Compulsory_Military_Service", # Binary: 1 for yes, 0 for no
  "COVID_19_Cases_Mar_20_2021",
  "COVID_19_Deaths_Mar_20_2021"
)

##--------------------------------------------------------------------Q4------------------------------------------------------------##
plot_and_save <- function(df, x, y, name_file, xlabel, y_label) {
    # Function description
    'The function gets a dataset, x and y, and saves a scatter plot.'

    # Define the path and name of the output file
    png(file = paste0("output/", name_file, ".png"), width = 800, height = 600)

    # Adjust margins (bottom, left, top, right)
    par(mar = c(5, 5, 4, 2) + 0.1)  # Adjust margins; you might need to tweak these values

    # Fit a linear model
    model <- lm(df[[y]] ~ df[[x]], data = df)

    # Get the R-squared value from the summary
    summary_model <- summary(model)
    r_squared <- summary_model$r.squared

    # Calculate the correlation coefficient
    correlation_coefficient <- cor(df[[x]], df[[y]])

    # Create the plot using the specified data frame columns
    plot(df[[x]], df[[y]],
         xlab = xlabel,        # X-axis label
         ylab = y_label,       # Y-axis label
         pch = 19,             # Type of point; 19 is a solid circle
         col = rgb(0, 0, 1, 0.5),  # Blue color with transparency
         cex = 1.5,            # Size of the points
         cex.lab = 2.2,        # Increase the font size of the axis labels
         cex.axis = 1.2,       # Increase the font size of the axis tick marks
         cex.main = 2.5        # Increase the font size of the main title
    )

    # Add text to the plot with R-squared and correlation coefficient
        legend("topright", 
                legend = c(paste("R = ", format(correlation_coefficient, digits = 2))),
                cex = 3,          # Increase the size of the text in the legend
                bty = "n")          # No box around the legend

    # Close the device driver to save the file
    dev.off()
}

# alcohol cosumtion vs COVID_19_Deaths_Mar_20_2021
    plot_and_save(
        df = covid_data,         # Data frame name
        x = "Total_Alcohol_Consumption_Per_Capita", # Column for x-axis
        y = "COVID_19_Deaths_Mar_20_2021",    # Column for y-axis
        name_file = "Total_Alcohol_Consumption_Per_Capita vs COVID_19_Deaths_Mar_20_2021",     # File name for the plot image
        xlabel = "Total Alcohol Consumption Per Capita", # Label for the x-axis
        y_label = "COVID_19 Deaths Mar 2021"     # Label for the y-axis
    )



# alcohol cosumtion vs Suicide_Rate
    plot_and_save(
        df = covid_data,         # Data frame name
        x = "Total_Alcohol_Consumption_Per_Capita", # Column for x-axis
        y = "Suicide_Rate",    # Column for y-axis
        name_file = "Total_Alcohol_Consumption_Per_Capita vs Suicide_Rate",     # File name for the plot image
        xlabel = "Total Alcohol Consumption Per Capita", # Label for the x-axis
        y_label = "Suicide Rate"     # Label for the y-axis
    )


# density vs gdp
    plot_and_save(
        df = covid_data,         # Data frame name
        x = "Population_Density", # Column for x-axis
        y = "GDP_Per_Capita",    # Column for y-axis
        name_file = "Population_Density vs GDP_Per_Capita",     # File name for the plot image
        xlabel = "Population Density", # Label for the x-axis
        y_label = "GDP per Capita"     # Label for the y-axis
    )

# density vs Suicide_Rate
    plot_and_save(
        df = covid_data,         # Data frame name
        x = "COVID_19_Cases_Mar_20_2021", # Column for x-axis
        y = "Suicide_Rate",    # Column for y-axis
        name_file = "Population_Density vs Suicide_Rate",     # File name for the plot image
        xlabel = "Population Density", # Label for the x-axis
        y_label = "Suicide Rate"     # Label for the y-axis
    )

# gdp vs Suicide_Rate
    plot_and_save(
        df = covid_data,         # Data frame name
        x = "GDP_Per_Capita", # Column for x-axis
        y = "Suicide_Rate",    # Column for y-axis
        name_file = "GDP_Per_Capita vs Suicide_Rate",     # File name for the plot image
        xlabel ="GDP per Capita", # Label for the x-axis
        y_label = "Suicide Rate"     # Label for the y-axis
    )

# Deaths_From_Smoking vs Suicide_Rate
    plot_and_save(
        df = covid_data,         # Data frame name
        x = "GDP_Per_Capita", # Column for x-axis
        y = "Total_Alcohol_Consumption_Per_Capita",    # Column for y-axis
        name_file = "GDP_Per_Capita vs Total Alcohol Consumption",     # File name for the plot image
        xlabel = "GDP per Capita", # Label for the x-axis
        y_label = "Total Alcohol Consumption Per Capita"     # Label for the y-axis
    )



# GDP_Per_Capita vs COVID_19_Deaths_Mar_20_2021
    plot_and_save(
        df = covid_data,         # Data frame name
        x = "Deaths_From_Smoking", # Column for x-axis
        y = "COVID_19_Deaths_Mar_20_2021",    # Column for y-axis
        name_file = "Deaths_From_Smoking vs COVID_19_Deaths_Mar_20_2021",     # File name for the plot image
        xlabel =  "Deaths From Smoking", # Label for the x-axis
        y_label = "COVID_19 Deaths Mar 2021"     # Label for the y-axis
    )



# GDP_Per_Capita vs COVID_19_Deaths_Mar_20_2021
    plot_and_save(
        df = covid_data,         # Data frame name
        x = "GDP_Per_Capita", # Column for x-axis
        y = "COVID_19_Deaths_Mar_20_2021",    # Column for y-axis
        name_file = "GDP_Per_Capita vs COVID_19_Deaths_Mar_20_2021",     # File name for the plot image
        xlabel =  "GDP Per Capita", # Label for the x-axis
        y_label = "COVID_19 Deaths Mar 2021"     # Label for the y-axis
    )



##--------------------------------------------------------------------Q5------------------------------------------------------------##




# Selecting only continuous variables
continuous_vars <- covid_data %>% select(
  GDP_Per_Capita,
  Population_Density,
  Total_Alcohol_Consumption_Per_Capita,
  Suicide_Rate,
  Deaths_From_Smoking,
  COVID_19_Cases_Mar_20_2021,
  COVID_19_Deaths_Mar_20_2021
)

# Calculating descriptive statistics
descriptive_stats <- continuous_vars %>% summarise_all(
  list(
    mean = ~mean(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    IQR = ~IQR(., na.rm = TRUE),
    skewness = ~skewness(., na.rm = TRUE)
  )
)

# Print the results
write.csv(descriptive_stats, "output/descriptive_stats.csv", row.names = FALSE)


# Split data into two groups based on Compulsory Military Service
grouped_data <- covid_data %>%
  group_by(Compulsory_Military_Service) %>%
  summarise(
    mean = mean(COVID_19_Cases_Mar_20_2021, na.rm = TRUE),
    median = median(COVID_19_Cases_Mar_20_2021, na.rm = TRUE),
    sd = sd(COVID_19_Cases_Mar_20_2021, na.rm = TRUE),
    skewness = skewness(COVID_19_Cases_Mar_20_2021, na.rm = TRUE),
    IQR = IQR(COVID_19_Cases_Mar_20_2021, na.rm = TRUE)
  )

# Print the results
write.csv(grouped_data, "output/Compulsory_Military_Service_descriptive_stats.csv", row.names = FALSE)


##----------------------------------------------Q6----------------------------------------##
library(ggplot2)
library(dplyr)
library(tidyr)

theme(
  text = element_text(size = 16),  # Base text size
  axis.text.x = element_text(size = 18, angle = 45, hjust = 1),  # X-axis text size
  axis.text.y = element_text(size = 18),  # Y-axis text size
  axis.title = element_text(size = 20),  # Axis titles size
  plot.title = element_text(size = 22)  # Plot title size
)
library(ggplot2)
library(dplyr)
library(tidyr)

features <- c("GDP_Per_Capita", "Population_Density", 
              "Suicide_Rate", "Deaths_From_Smoking", "COVID_19_Cases_Mar_20_2021",
              "COVID_19_Deaths_Mar_20_2021", "Total_Alcohol_Consumption_Per_Capita")

outliers_info <- data.frame(Feature = character(), Outlier_Index = integer(), Outlier_Value = numeric(), Deviation = numeric(), stringsAsFactors = FALSE)

for (feature in features) {
  p <- ggplot(covid_data, aes_string(x = "factor(1)", y = feature)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
    labs(title = paste("Boxplot of", feature), x = "", y = feature) +
    theme(text = element_text(size = 16),
          axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 18),
          axis.title = element_text(size = 20),
          plot.title = element_text(size = 22))

  ggsave(paste0("output/", feature, "_boxplot.png"), plot = p, width = 8, height = 6, dpi = 300)
  
  Q1 <- quantile(covid_data[[feature]], 0.25, na.rm = TRUE)
  Q3 <- quantile(covid_data[[feature]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  outlier_indices <- which(covid_data[[feature]] < lower_bound | covid_data[[feature]] > upper_bound)
  outlier_values <- covid_data[[feature]][outlier_indices]
  
  deviations <- ifelse(outlier_values < lower_bound, abs(outlier_values - lower_bound), abs(outlier_values - upper_bound))
  
  outliers_info <- rbind(outliers_info, data.frame(Feature = feature, Outlier_Index = outlier_indices, Outlier_Value = outlier_values, Deviation = deviations, stringsAsFactors = FALSE))
}

write.csv(outliers_info, "output/outliers_info.csv", row.names = FALSE)

print(outliers_info)


#change outliers values
covid_data$Deaths_From_Smoking[c(27, 52)] <- 227477.4
covid_data$Population_Density[c(99)] <- 1718



