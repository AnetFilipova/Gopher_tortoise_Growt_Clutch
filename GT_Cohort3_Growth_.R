setwd("~/Library/CloudStorage/Box-Box/TS_Lab_GopherTortoises/Data_Analysis/Growth_data/Growth_Rate_Cohort_3")

#Load necessary library
library(lme4)
library(emmeans)
library(ggplot2)

#Clear memory
rm(list=ls(all = TRUE))

datum <- read.csv("Growth_data_Working.csv")
head(datum)

# Filter out individuals with Tortoise_ID "Not_Viable" and specific IDs
datum <- subset(datum, !(Tortoise_ID %in% c("Not_Viable", "GT2023_N05.03", "GT2023_N06.01", "GT2023_N05.06", "GT2023_N15.04")))

# Confirm the filtered dataset
head(datum)

#Fit the model
# Question 1: What is the growth rate of animals before cold dormancy treatment?
# Mixed effect model with treatment as a fixed effect and Nest_ID as a random effect

model_before <- lmer(Growth_rate_Before ~ Treatment +  (1 | Nest_ID), data = datum)

# Summarize the results
summary(model_before)

# Post-hoc tests
emmeans(model_before, pairwise ~ Treatment)

#plot the data

p2=ggplot(datum, aes(x = Treatment, y = Growth_rate_Before)) +
  geom_boxplot(aes(fill = Treatment)) +
  scale_fill_manual(values = c("Constant_heat" = "red", "Cold_dormancy" = "blue")) +
  labs(title = "Growth Rate Before Cold dormancy",
       x = "Treatment Group",  # Keeps x-axis label
       y = "Growth Rate (g/day)") +
  theme_minimal() +
  guides(fill = "none") +  # Removes the legend for the fill aesthetic
  theme(plot.title = element_text(hjust = 0.5))  # Centers the title

p2

# Save file as PNG for final figure production
ggsave(p2, file="Growth_rate_Before.png", width=9, height=7, dpi=600)

# Question 2: What is the effect of treatment on growth rate during cold dormancy?
# Mixed effect model with treatment as a fixed effect and Nest_ID as a random effect

model_during <- lmer(Growth_rate_During ~ Treatment + (1 | Nest_ID), data = datum)

# Summarize the results
summary(model_during)

# Post-hoc tests
emmeans(model_during, pairwise ~ Treatment)

#plot the data

p2=ggplot(datum, aes(x = Treatment, y = Growth_rate_During)) +
  geom_boxplot(aes(fill = Treatment)) +
  scale_fill_manual(values = c("Constant_heat" = "red", "Cold_dormancy" = "blue")) +
  labs(title = "Growth Rate Before During Cold dormancy",
       x = "Treatment Group",  # Keeps x-axis label
       y = "Growth Rate (g/day)") +
  theme_minimal() +
  guides(fill = "none") +  # Removes the legend for the fill aesthetic
  theme(plot.title = element_text(hjust = 0.5))  # Centers the title

p2

# Save file as PNG for final figure production
ggsave(p2, file="Growth_rate_During.png", width=9, height=7, dpi=600)

#Question 3: What is the effect of treatment on growth rate 3 weeks post-cold dormancy?
model_3Weeks <- lmer(Growth_rate_3_Weeks_Post ~ Treatment + (1 | Nest_ID), data = datum)

# Summarize the results
summary(model_3Weeks)

# Post-hoc tests
emmeans(model_3Weeks, pairwise ~ Treatment)

#plot the data

p2=ggplot(datum, aes(x = Treatment, y = Growth_rate_3_Weeks_Post)) +
  geom_boxplot(aes(fill = Treatment)) +
  scale_fill_manual(values = c("Constant_heat" = "red", "Cold_dormancy" = "blue")) +
  labs(title = "Growth Rate 3 Weeks Post-Cold Dormnacy Treatment",
       x = "Treatment Group",  # Keeps x-axis label
       y = "Growth Rate (g/day)") +
  theme_minimal() +
  guides(fill = "none") +  # Removes the legend for the fill aesthetic
  theme(plot.title = element_text(hjust = 0.5))  # Centers the title

p2

# Save file as PNG for final figure production
ggsave(p2, file="Growth_rate_3Weeks.png", width=9, height=7, dpi=600)

##Question 4: What is the effect of treatment on growth rate 3 months post-cold dormancy?
model_3Months <- lmer(Growth_rate_3_Months_Post ~ Treatment + (1 | Nest_ID), data = datum)

# Summarize the results
summary(model_3Months)

# Post-hoc tests
emmeans(model_3Months, pairwise ~ Treatment)

#plot the data

p2=ggplot(datum, aes(x = Treatment, y = Growth_rate_3_Months_Post)) +
  geom_boxplot(aes(fill = Treatment)) +
  scale_fill_manual(values = c("Constant_heat" = "red", "Cold_dormancy" = "blue")) +
  labs(title = "Growth Rate 3 Months Post-Cold Dormnacy Treatment",
       x = "Treatment Group",  # Keeps x-axis label
       y = "Growth Rate (g/day)") +
  theme_minimal() +
  guides(fill = "none") +  # Removes the legend for the fill aesthetic
  theme(plot.title = element_text(hjust = 0.5))  # Centers the title

p2

# Save file as PNG for final figure production
ggsave(p2, file="Growth_rate_3Months.png", width=9, height=7, dpi=600)


##Combine the results from all three models into one graph
library(tidyr)

# Reshape data to long format
datum_long <- datum %>%
  pivot_longer(
    cols = c(Growth_rate_Before, Growth_rate_During, Growth_rate_3_Weeks_Post, Growth_rate_3_Months_Post),
    names_to = "Timepoint",
    values_to = "Growth_rate"
  )

# Adjust the Timepoint column for better labels
datum_long$Timepoint <- factor(
  datum_long$Timepoint,
  levels = c("Growth_rate_Before", "Growth_rate_During", "Growth_rate_3_Weeks_Post", "Growth_rate_3_Months_Post"),
  labels = c("Before Cold Dormancy", "During Cold Dormancy", "3 Weeks Post", "3 Months Post")
)

p_combined_grouped <- ggplot(datum_long, aes(x = Timepoint, y = Growth_rate, fill = Treatment)) +
  geom_boxplot(position = position_dodge()) +
  scale_fill_manual(
    values = c("Constant_heat" = "red", "Cold_dormancy" = "blue"),
    name = "Treatment Group"  # Legend title
  ) +
  labs(
    title = "Growth Rates Across Time Points and Treatments",
    x = "Time Point",  # X-axis label
    y = "Growth Rate (g/day)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.position = "right",  # Position the legend at the top (optional)
    axis.text.x = element_text(size = 10)  # Customize X-axis text size
  )

print(p_combined_grouped)
ggsave(p_combined_grouped, file = "Combined_Growth_rate.png", width = 12, height = 6, dpi = 600)

## Question 5: How does growth rate change before and after cold dormancy ONLY for the cold dormancy treatment
cold_dormancy_data <- subset(datum, Treatment == "Cold_dormancy")

#Reshape the data into a long format to get a column for Timepoint and a column for Growth_rate

datum_long <- read.csv("Growth_data_Working.csv")
datum_long <- reshape(datum_long, 
                      varying = c("Growth_rate_Before", "Growth_rate_3_Months_Post"), 
                      v.names = "Growth_rate", 
                      timevar = "Timepoint", 
                      times = c("Growth_rate_Before", "Growth_rate_3_Months_Post"), 
                      direction = "long")

# Reorder the 'Timepoint' factor to ensure correct order
datum_long$Timepoint <- factor(datum_long$Timepoint, 
                               levels = c("Growth_rate_Before", "Growth_rate_3_Months_Post"))

# Filter the data for the cold dormancy treatment
cold_dormancy_only <- datum_long[datum_long$Treatment == "Cold_dormancy", ]

model_cold_dormancy <- lmer(Growth_rate ~ Timepoint + (1 | Nest_ID), data = cold_dormancy_only)


# Summarize the model results
summary(model_cold_dormancy)

emmeans(model_cold_dormancy, pairwise ~ Timepoint)

# Create the boxplot for Growth_rate before and after cold dormancy
plot_growth <- ggplot(cold_dormancy_only, aes(x = Timepoint, y = Growth_rate, fill = Timepoint)) + 
  geom_boxplot() + 
  labs(x = "Timepoint", y = "Growth Rate (g/day)") + 
  scale_x_discrete(labels = c("Growth_rate_Before" = "Before Cold Dormancy", 
                              "Growth_rate_3_Months_Post" = "3 Months Post Cold Dormancy")) + 
  theme_minimal() + 
  theme(
    legend.position = "none",
    axis.title = element_text(size = 16),  # Increase size of axis titles
    axis.text = element_text(size = 14)    # Increase size of axis tick labels
  )

# Save the plot
ggsave(plot_growth, file = "Growth_rate_Cold_Dormancy_Boxplot.png", width = 12, height = 6, dpi = 600)


