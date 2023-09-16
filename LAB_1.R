install.packages("readxl")
library(readxl)
EPI_data1<- read_excel("~/Desktop/Fauzan New/RPI/Sem 1/Data Analytics/R PROJECTS/2010EPI_data (1).xls")

View(EPI_data1)

attach(EPI_data1) 
fix(EPI_data1)

tf <- is.na(EPI_data1)
View(tf)

E <- EPI_data1[!tf] 
View(E)

library(readr)
EPI_data <-  read_csv("~/Desktop/Fauzan New/RPI/Sem 1/Data Analytics/R PROJECTS/2010EPI_data.csv")
View(EPI_data)

names(EPI_data) <- as.matrix(EPI_data[1,])
EPI_data <- EPI_data[-1,]
EPI_data[] <- lapply(EPI_data, function(x) type.convert(as.character()))
EPI_data
View(EPI_data)

EPI <- EPI_data$EPI
EPI<- as.numeric(EPI)
t <-is.na(EPI)

EPI[!t]

summary(EPI)
fivenum(as.numeric(EPI_data$EPI))
stem(EPI)

hist(EPI)


hist(EPI, seq(30.,95.,1.0),probability = TRUE)
lines(density(EPI, na.rm = TRUE, bw=1.))

plot(ecdf(EPI), do.points=FALSE, verticals = TRUE)

par(pty="s")
qqnorm(EPI); qqline(EPI)
x <- seq(30, 95, 1)
theoretical_quantiles <- qnorm(ppoints(length(x)))
qqplot(theoretical_quantiles, sort(x))
abline(a = 0, b = 1, col = "red")

qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for t dsn" )
qqline(x)

View(EPI_data)
## Since DALY is a column
DALY <- as.numeric(EPI_data$DALY)
t <- is.na(DALY)
DALY[!t]
summary(DALY)
fivenum(DALY)
stem(DALY)
hist(DALY)

# Create a density plot over a histogram with specified breaks
data_range <- range(DALY, na.rm = TRUE)

# Create a histogram using the breaks
hist(DALY, breaks=30, probability = TRUE)
lines(density(DALY, na.rm = TRUE, bw=1.))

# Plot the empirical cumulative distribution function (ECDF) of DALY
plot(ecdf(DALY), do.points=FALSE, verticals = TRUE)

# Create a Q-Q plot for a normal distribution
par(pty="s")
qqnorm(DALY); qqline(DALY)

# Create a Q-Q plot using theoretical quantiles from a standard normal distribution
x <- seq(30, 95, 1)
theoretical_quantiles <- qnorm(ppoints(length(x)))
qqplot(theoretical_quantiles, sort(x))
abline(a = 0, b = 1, col = "red")

# Create a Q-Q plot for a t-distribution with 5 degrees of freedom
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for t dsn" )
qqline(x)


# Convert WATER_H to numeric and handle NA values
WATER_H <- as.numeric(EPI_data$WATER_H)
t <- is.na(WATER_H)

# Displaying non-NA values of WATER_H
WATER_H[!t]

# Summarize WATER_H data
summary(WATER_H)
fivenum(WATER_H)
stem(WATER_H)

# Creating a histogram of WATER_H data
hist(WATER_H)

# Creating a density plot over a histogram with specified breaks
hist(WATER_H, seq(min(WATER_H, na.rm = TRUE), max(WATER_H, na.rm = TRUE), length.out = 30), probability = TRUE)
lines(density(WATER_H, na.rm = TRUE))

# Plotting the empirical cumulative distribution function (ECDF) of WATER_H
plot(ecdf(WATER_H), do.points=FALSE, verticals = TRUE)

# Create a Q-Q plot for a normal distribution
par(pty="s")
qqnorm(WATER_H); qqline(WATER_H)

# Create a Q-Q plot using theoretical quantiles from a standard normal distribution
x <- seq(min(WATER_H, na.rm = TRUE), max(WATER_H, na.rm = TRUE), length.out = 100)
theoretical_quantiles <- qnorm(ppoints(length(x)))
qqplot(theoretical_quantiles, sort(x))
abline(a = 0, b = 1, col = "red")

# Create a Q-Q plot for a t-distribution with 5 degrees of freedom
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

# Additional Plots

# Boxplot to visualize the central tendency and spread of WATER_H
boxplot(WATER_H, main = "Boxplot of WATER_H", ylab = "Values")

# Violin plot to visualize the distribution of WATER_H (requires ggplot2 package)
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
ggplot(EPI_data, aes(x = "", y = WATER_H)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1) +
  labs(title = "Violin plot of WATER_H", y = "Values")

##Creating boxplot of Daly and EPI
boxplot(EPI, DALY, names=c("EPI", "DALY"), main="Boxplot of EPI and DALY")

# Getting  the quantiles of EPI and DALY
qEPI <- quantile(EPI, probs=ppoints(length(EPI)), na.rm = TRUE)
qDALY <- quantile(DALY, probs=ppoints(length(DALY)), na.rm = TRUE)

# Creating a Q-Q plot of EPI and DALY
qqplot(qEPI, qDALY, xlab="Q-Q plot of EPI", ylab="Q-Q plot of DALY", main="Q-Q plot of EPI vs DALY")

###But there is more!
# Converting necessary columns to the correct data type
EPI_data <- type.convert(EPI_data, as.is = TRUE)


variables <- c("EPI", "ENVHEALTH", "ECOSYSTEM", "DALY", "AIR_H", "WATER_H", "WATER_E", "BIODIVERSITY")

for (var in variables) {
  EPI_data[[var]] <- as.numeric(EPI_data[[var]])
}

boxplot(EPI_data[, variables], las=2)
title("Combined Boxplot")

##Creating Histogram

par(mfrow=c(2, 4), mar=c(4, 4, 2, 1))

for (var in variables) {
  valid_values <- EPI_data[[var]][!is.na(EPI_data[[var]]) & !is.infinite(EPI_data[[var]])]
  hist(valid_values, main=var, xlab=var, probability = TRUE, breaks=20, col='skyblue', border='blue')
}

##Using Kernel Density Plots for the distributions
for (var in variables) {
  valid_values <- EPI_data[[var]][!is.na(EPI_data[[var]]) & !is.infinite(EPI_data[[var]])]
  dens <- density(valid_values)
  plot(dens, main=var, xlab=var, col='blue', lwd=2)
  polygon(dens, col="skyblue", border="blue")
}

##Filtering

EPILand <- EPI_data$EPI[EPI_data$Landlock == 1]
##removing NA values
ELand <- EPILand[!is.na(EPILand)]
##Plotting the histogram
hist(ELand, main = "Histogram of ELand", xlab = "ELand")
hist(ELand, breaks = seq(30., 95., 1.0), probability = TRUE, main = "Histogram of ELand with Specified Breaks", xlab = "ELand")

##Boxplot
boxplot(ELand, main = "Boxplot of ELand", ylab = "ELand")

##Density Plot
dens <- density(ELand)
plot(dens, main = "Density Plot of ELand", xlab = "ELand", col = 'blue', lwd = 2)
polygon(dens, col = "skyblue", border = "blue")
## Q-Q plot
qqnorm(ELand, main = "Q-Q Plot of ELand")
qqline(ELand, col = "red")


## Plotting for No_surface water, desert and high_population

variable_names <- c('No_surface_water', 'Desert', 'High_Population_Density')

for (variable_name in variable_names) {
  for (condition in 0:1) {
    # Filtering the values based on the condition (0 or 1)
    VarLand <- EPI_data$EPI[EPI_data[[variable_name]] == condition]
    
    # Removing NA values
    ELand <- VarLand[!is.na(VarLand)]
    
    # Boxplot
    boxplot(ELand, main = paste("Boxplot of", variable_name, "under condition", condition), ylab = variable_name)
    
    # Histogram with default breaks
    hist(ELand, main = paste("Histogram of", variable_name, "under condition", condition), xlab = variable_name)
    
    # Histogram with specified breaks and density
    hist(ELand, breaks = seq(30., 95., 1.0), probability = TRUE, main = paste("Histogram of", variable_name, "with Specified Breaks under condition", condition), xlab = variable_name)
    
    # Density plot
    dens <- density(ELand)
    plot(dens, main = paste("Density Plot of", variable_name, "under condition", condition), xlab = variable_name, col = 'blue', lwd = 2)
    polygon(dens, col = "skyblue", border = "blue")
    
    # Q-Q plot
    qqnorm(ELand, main = paste("Q-Q Plot of", variable_name, "under condition", condition))
    qqline(ELand, col = "red")
  }
}

# Filter data for an EPI region
EPI_region_data1 <- subset(EPI_data, EPI_regions == 'Sub-Saharan Africa')
View(EPI_region_data1)

# Filter data for a  GEO subregion
GEO_subregion_data <- subset(EPI_data, GEO_subregion == 'Western Europe')
View(GEO_subregion_data)

# Viewing summary statistics for the EPI column for the region and subregion
summary(EPI_region_data1$EPI)
summary(GEO_subregion_data$EPI)

# Creating a histogram for the EPI column for the region or subregion
hist(EPI_region_data1$EPI, main = "Histogram for Sub-Saharan Africa EPI Region", xlab = "EPI")
hist(GEO_subregion_data$EPI, main = "Histogram for Western Europe GEO Subregion", xlab = "EPI")

GPW3_GRUMP<- read_csv("~/Desktop/Fauzan New/RPI/Sem 1/Data Analytics/R PROJECTS/GPW3_GRUMP_SummaryInformation_2010.csv")
View(GPW3_GRUMP)
names(GPW3_GRUMP)

# Getting summary statistics for "PopulationPerUnit"
summary(GPW3_GRUMP$PopulationPerUnit)

# Creating a boxplot for "PopulationPerUnit"
boxplot(GPW3_GRUMP$PopulationPerUnit, main="Boxplot of PopulationPerUnit", ylab="Values")
# Creating a histogram for "PopulationPerUnit"
hist(GPW3_GRUMP$PopulationPerUnit, main="Histogram of PopulationPerUnit", xlab="Values")
# Creating a Q-Q plot for "PopulationPerUnit"
qqnorm(GPW3_GRUMP$PopulationPerUnit, main="Q-Q Plot of PopulationPerUnit")
qqline(GPW3_GRUMP$PopulationPerUnit, col="red")
# Creating a density plot for "PopulationPerUnit"
density_pop <- density(GPW3_GRUMP$PopulationPerUnit, na.rm=TRUE) # Remove NA values
plot(density_pop, main="Density Plot of PopulationPerUnit")
polygon(density_pop, col="skyblue", border="blue")

# Filtering 
specific_continent_data <- subset(GPW3_GRUMP, ContinentName == 'Africa')
View(specific_continent_data )

water_treatment <- read_csv("~/Desktop/Fauzan New/RPI/Sem 1/Data Analytics/R PROJECTS/water-treatment.csv")
View(water_treatment)
names(water_treatment)

# Get summary statistics for "PH-E"
summary(water_treatment$`PH-E`)
# Creating a boxplot for "PH-E"
boxplot(water_treatment$`PH-E`, main="Boxplot of PH-E", ylab="Values")

# Creating a histogram for "PH-E"
hist(water_treatment$`PH-E`, main="Histogram of PH-E", xlab="Values", breaks=20)

# Creating a Q-Q plot for "PH-E"
qqnorm(water_treatment$`PH-E`, main="Q-Q Plot of PH-E")
qqline(water_treatment$`PH-E`, col="red")

# Creating a density plot for "PH-E"
density_phe <- density(water_treatment$`PH-E`, na.rm=TRUE) # Remove NA values
plot(density_phe, main="Density Plot of PH-E")
polygon(density_phe, col="skyblue", border="blue")


# Filtering data for a specific range of PH-E values
specific_range_data <- subset(water_treatment, `PH-E` >= 7 & `PH-E` <= 8)
View(specific_range_data)
