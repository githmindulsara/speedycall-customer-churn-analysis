attach(SpeedyCall_Customer_Churn_Data)
table(Churn)/7043
boxplot(tenure ~ Churn, data = SpeedyCall_Customer_Churn_Data,main = "Boxplot of Tenure by Churn",xlab = "Churn (Yes/No)",ylab = "Tenure",col = c("lightblue", "pink"))
table(Churn,PaymentMethod)
chisq.test(table(Churn,PaymentMethod))
barplot(table(Churn,gender),beside=TRUE,col=c("skyblue","orange"),legend=TRUE,main="Churn Counts by Gender",xlab="Gender",ylab="Count")
barplot(t(prop.table(table(Churn,SeniorCitizen))),beside=TRUE,col=c("skyblue","orange"),legend=TRUE,main="Churn Counts by Seniority",xlab="Seniority",ylab="Count")

# Contingency table
churn_table <- table(SeniorCitizen, Churn)
# Calculate proportions by row (margin = 1)
churn_percentage <- prop.table(churn_table, margin = 1) * 100
# Percentage component bar chart
barplot(t(churn_percentage), beside = FALSE,col = c("skyblue", "orange"),legend.text = TRUE,args.legend = list(title = "Churn", x = "topright"),xlab = "Seniority", ylab = "Percentage",main = "Percentage Component Bar Chart of Churn by Seniority")

# Contingency table
churn_table <- table(data$Seniority, data$Churn)

# Calculate percentages
churn_percentage <- prop.table(churn_table, margin = 1) * 100

# Draw the bar chart and capture bar positions
bar_positions <- barplot(t(churn_percentage), 
                         beside = FALSE, 
                         col = c("skyblue", "orange"),
                         legend.text = TRUE,
                         args.legend = list(title = "Churn", x = "topright"),
                         xlab = "Seniority", 
                         ylab = "Percentage",
                         main = "Percentage Component Bar Chart of Churn by Seniority")

# Add percentage labels
cumulative_percentages <- apply(t(churn_percentage), 2, cumsum)  # Cumulative sum for placement
text_positions <- cumulative_percentages - t(churn_percentage) / 2  # Center of each segment
text(rep(bar_positions, each = nrow(churn_percentage)), 
     as.vector(text_positions), 
     labels = paste0(round(as.vector(t(churn_percentage)), 1), "%"), 
     cex = 0.8, col = "black")

# Adding means to the boxplot
boxplot(TotalCharges ~ Partner, 
        col = c("lightblue", "salmon"), 
        main = "Total Charges by Partner Status", 
        xlab = "Partner (No/Yes)", 
        ylab = "Total Charges", 
        notch = TRUE)

# Calculate group means
means <- tapply(TotalCharges, Partner, mean, na.rm = TRUE)

# Overlay mean points
points(1:2, means, col = "blue", pch = 16, cex = 1.5)  # Blue points for means

table_data <- table(gender, Dependents)
table_data
prop_table <- prop.table(table_data, margin = 1) * 100  # Row-wise percentages
prop_table

barplot(
  prop_table,
  beside = TRUE,                        # Grouped bars
  col = c("lightblue", "salmon"),       # Colors for genders
  legend = rownames(prop_table),        # Add legend for gender
  main = "Dependents by Gender",        # Title of the plot
  xlab = "Has Dependents",              # X-axis label
  ylab = "Percentage",                  # Y-axis label
  args.legend = list(title = "Gender", x = "topright")  # Legend position
)

# Create a contingency table
table_data <- table(gender, Dependents)

# Calculate proportions as percentages
percentages <- prop.table(table_data, margin = 1) * 100

# Draw a percentage component bar plot
bar_positions <-barplot(
  t(percentages),                  # Transpose to stack by Gender
  beside = FALSE,                  # Stacked bars
  col = c("lightblue", "salmon"),  # Colors for "No" and "Yes"
  legend = c("No", "Yes"),         # Add legend for "Has Dependents"
  main = "Percentage of Dependents by Gender",  # Title
  xlab = "Gender",                 # X-axis label
  ylab = "Percentage",             # Y-axis label
  ylim = c(0, 100)                 # Set limit to 100% for each bar
)

# Add percentage labels for each segment in the bars
# For "No" (first segment)
text(
  x = bar_positions,                             # Bar positions for "No"
  y = t(percentages)[1, ] / 2,                  # Position for "No" segment label (centered)
  labels = paste0(round(t(percentages)[1, ], 1), "%"), # Labels for "No"
  col = "black",                                # Text color
  cex = 0.8                                     # Font size
)

# For "Yes" (second segment)
text(
  x = bar_positions,                            # Bar positions for "Yes"
  y = t(percentages)[1, ] + t(percentages)[2, ] / 2,  # Position for "Yes" segment label (centered)
  labels = paste0(round(t(percentages)[2, ], 1), "%"), # Labels for "Yes"
  col = "black",                               # Text color
  cex = 0.8                                     # Font size
)

# Boxplot for Monthly Charges by Churn
boxplot(
  MonthlyCharges ~ Churn,               # Monthly Charges grouped by Churn
  col = c("lightblue", "salmon"),                # Colors for "No" and "Yes"
  main = "Monthly Charges by Churn Status",
  xlab = "Churn",
  ylab = "Monthly Charges",
                             # Add notches for confidence intervals
)










































