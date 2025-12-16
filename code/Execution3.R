library(tidyverse)
library(zoo)
library(ggplot2)
library(GGally)
library(scales)
library(patchwork)
library(glue)
# library(imputeTS)

# Load dataset
df <- read_csv("/Users/bkings/College Stuffs/Statistical Computing/Assignment/My assignment/Question/dataset_(MSC7)-80971f18-5843-4b66-8d97-d159a8118873.csv")

# Rename columns for clarity
colnames(df) <- c("bg_mean", "insulin_sum", "carbs_sum", "hr_mean", "steps_sum", "cals_sum", "bg_plus_1")

glimpse(df)
dim(df)
summary(df)
colSums(is.na(df))

hr_na_count <- sum(colSums(is.na(df)))
hr_na_count
hr_na_percent <- hr_na_count / nrow(df) * 100
hr_na_percent

# Add time column as it is not present in original dataset
df$time <- 1:nrow(df)

# Relocate 'time' column and place it at the beginning
df <- df %>% relocate(time)

# Columns
TIME <- "time"
BG <- "bg_mean"
INSULIN <- "insulin_sum"
CARBS <- "carbs_sum"
HR <- "hr_mean"
STEPS <- "steps_sum"
CALS <- "cals_sum"
BGP1 <- "bg_plus_1"

# Representation Colors
BG_COLOR <- "black"
INSULIN_COLOR <- "lightblue"
CARBS_COLOR <- "orange"
HR_COLOR <- "red"
STEPS_COLOR <- "purple"
CALS_COLOR <- "navyblue"
BGP1_COLOR <- "darkred"

# Titles
BG_TITLE <- "Blood Glucose (x1)"
INSULIN_TITLE <- "Insulin (x2)"
CARBS_TITLE <- "Carbohydrate Intake (x3)"
HR_TITLE <- "Heart Rate (x4)"
STEPS_TITLE <- "Total Steps (x5)"
CALS_TITLE <- "Calories Burned (x6)"
BGP1_TITLE <- "Blood Glucose after 1 hour (y) (Dependent Variable)"

# Labels
BG_LABEL <- "bg_mean (mmol/L)"
INSULIN_LABEL <- "insulin_sum"
CARBS_LABEL <- "carbs_sum"
HR_LABEL <- "hr_mean"
STEPS_LABEL <- "steps_sum"
CALS_LABEL <- "cals_sum"
BGP1_LABEL <- "bg+1:00 (mmol/L)"

head(df)

# df$hr_mean <- na_kalman(df$hr_mean, model = "StructTS")

# hr_mean imputation (LOCF + Backfill) Last Observation Carried Forward
df$hr_mean <- na.locf(df$hr_mean, na.rm = FALSE)
df$hr_mean <- na.locf(df$hr_mean, fromLast = TRUE)

# Optional smoothing to avoid unrealistic artificial blocks
# df$hr_mean <- zoo::rollmedian(df$hr_mean, k = 5, fill = "extend")

#df <- kNN(df, k = 5, imp_var = FALSE)

head(df)

colSums(is.na(df))

p99_carbs <- quantile(df$carbs_sum, 0.99, na.rm = TRUE)
df$carbs_sum <- ifelse(df$carbs_sum > p99_carbs, p99_carbs, df$carbs_sum)

df$steps_sum_log <- log1p(df$steps_sum)
df$cals_sum_log  <- log1p(df$cals_sum)

robust_scale <- function(x) {
  (x - median(x, na.rm = TRUE)) / IQR(x, na.rm = TRUE)
}

df_scaled <- df %>% 
  mutate(
    bg_mean_s      = robust_scale(!!sym(BG)),
    insulin_sum_s  = robust_scale(!!sym(INSULIN)),
    carbs_sum_s    = robust_scale(!!sym(CARBS)),
    hr_mean_s      = robust_scale(!!sym(HR)),
    steps_sum_s    = robust_scale(!!sym(STEPS)),
    cals_sum_s     = robust_scale(!!sym(CALS)),
    bg1_s          = robust_scale(!!sym(BGP1))
  )

# Function to create time series plot
generate_ts_plot <- function(dataset, var, color, title, y_label) {
  ggplot(dataset, aes(x = !!sym("time"), y = !!sym(var))) +
    geom_line(color = color) +
    labs(title = title,
         x = NULL,
         y = y_label) +
    theme_minimal(base_size = 14) + 
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14)
    )
}

# Time series individual plots

# Group 1
bg_mean_plt <- generate_ts_plot(df, BG, BG_COLOR, "Blood Glucose (x1)", "bg_mean (mmol/L)")
insulin_sum_plt <- generate_ts_plot(df, INSULIN, INSULIN_COLOR, "Insulin (x2)", "insulin_sum")
carbs_sum_plt <- generate_ts_plot(df, CARBS, CARBS_COLOR, "Carbohydrate Intake (x3)", "carbs_sum")
hr_mean_plt <- generate_ts_plot(df, HR, HR_COLOR, "Heart Rate (x4)", "hr_mean")
hr_mean_plt

# Add X-axis label only for the last plot
hr_mean_plt <- hr_mean_plt + labs(x = "Time")

# Combine above four plots. Dividing into two parts for clear visuals
combined_plot_1 <- bg_mean_plt / insulin_sum_plt / carbs_sum_plt / hr_mean_plt +
  plot_annotation(
    title = "Time Series"
  )

combined_plot_1

# Group 2
steps_sum_plt <- generate_ts_plot(df, STEPS, STEPS_COLOR, "Total Steps (x5)", "steps_sum")
cals_sum_plt <- generate_ts_plot(df, CALS, CALS_COLOR, "Calories Burned (x6)", "cals_sum")
bg_plus_one_plt <- generate_ts_plot(df, BGP1, BGP1_COLOR, "Blood Glucose after 1 hour (y) (Dependent Variable)", "bg+1:00 (mmol/L)")

# Add X-axis label only for the last plot
bg_plus_one_plt <- bg_plus_one_plt + labs(x = "Time (hours)")

# Combine plots for group 2.
combined_plot_2 <- steps_sum_plt / cals_sum_plt / bg_plus_one_plt +
  plot_annotation(
    title = "Time Series"
  )

combined_plot_2

# SMOOTHING DATA

# Window size for rolling average
window_size <- 200

# Apply rolling window to each variable
data_smoothed <- df %>%
  mutate(
    bg_mean_smooth = rollmean(df[[BG]], k = window_size, fill = NA, align = "center"),
    insulin_smooth = rollmean(df[[INSULIN]], k = window_size, fill = NA, align = "center"),
    carbs_smooth = rollmean(df[[CARBS]], k = window_size, fill = NA, align = "center"),
    hr_smooth = rollmean(df[[HR]], k = window_size, fill = NA, align = "center"),
    steps_smooth = rollmean(df[[STEPS]], k = window_size, fill = NA, align = "center"),
    cals_smooth = rollmean(df[[CALS]], k = window_size, fill = NA, align = "center"),
    bgplusone_smooth = rollmean(df[[BGP1]], k = window_size, fill = NA, align = "center")
  )

# Function to generate plots with both raw and smoothed data
generate_smooth_plot <- function(data, raw_var, smooth_var, color, title, y_label) {
  ggplot(data, aes(x = !!sym("time"))) +
    # Raw data in background with transparency
    geom_line(aes(y = .data[[raw_var]]), color = color, alpha = 0.2, linewidth = 0.5) +
    # Smoothed data in foreground
    geom_line(aes(y = .data[[smooth_var]]), color = color, linewidth = 1.5) +
    labs(title = title,
         y = y_label,
         x = NULL) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 13),
      axis.title = element_text(size = 14),
      plot.margin = margin(t = 5, r = 10, b = 5, l = 10)
    )
}

# Group 1
bg_mean_sm_plt <- generate_smooth_plot(data_smoothed, BG, "bg_mean_smooth", BG_COLOR, "Blood Glucose (x1)", "bg_mean (mmol/L)")
insulin_sum_sm_plt <- generate_smooth_plot(data_smoothed, INSULIN, "insulin_smooth", INSULIN_COLOR, "Insulin (x2)", "insulin_sum")
carbs_sum_sm_plt <- generate_smooth_plot(data_smoothed, CARBS, "carbs_smooth", CARBS_COLOR, "Carbohydrate Intake (x3)", "carbs_sum")

# Add x-axis only at the end of group
carbs_sum_sm_plt <- carbs_sum_sm_plt + labs(x = "Time (hours)")

generate_combined_sm_plot <- function() {
  plot_annotation(
    title = glue("Time series with {window_size} point rolling average"),
    subtitle = "Dark lines show smoothed lines and raw data with light lines",
    theme = theme(
      plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 14, hjust = 0.5)
    )
  )
}

# Combined smooth plot Group 1
combined_sm_plot_1 <- bg_mean_sm_plt / insulin_sum_sm_plt / carbs_sum_sm_plt + generate_combined_sm_plot()
  
combined_sm_plot_1

# Group 2
hr_mean_sm_plt <- generate_smooth_plot(data_smoothed, HR, "hr_smooth", HR_COLOR, "Heart Rate (x4)", "hr_mean")
steps_sum_sm_plt <- generate_smooth_plot(data_smoothed, STEPS, "steps_smooth", STEPS_COLOR, "Total Steps (x5)", "steps_sum")
cals_sum_sm_plt <- generate_smooth_plot(data_smoothed, CALS, "cals_smooth", CALS_COLOR, "Calories Burned (x6)", "cals_sum")

cals_sum_sm_plt <- cals_sum_sm_plt + labs(x = "Time (hours)")

combined_sm_plot_2 <- hr_mean_sm_plt / steps_sum_sm_plt / cals_sum_sm_plt + generate_combined_sm_plot()

combined_sm_plot_2

# Group 3
bg_plus_one_sm_plt <- generate_smooth_plot(data_smoothed, BGP1, "bgplusone_smooth", BGP1_COLOR, "Blood Glucose after 1 hour (y) (Dependent Variable)", "bg+1:00 (mmol/L)")
+ labs(x = "Time (hours)") + generate_combined_sm_plot()

bg_plus_one_sm_plt

# End of smoothing


# Data Segmentation for more clarity, noise reduction and high level view
# Number of observations per segment
segment_size <- 500

df$segment <- ceiling(df$time / segment_size)

# Calculate avg for each segment and variable
segment_avgs <- df %>%
  group_by(segment) %>%
  summarize(
    time = mean(time),
    bg_mean_avg = mean(!!sym(BG)),
    insulin_sum_avg = mean(!!sym(INSULIN)),
    carbs_sum_avg = mean(!!sym(CARBS)),
    hr_mean_avg = mean(!!sym(HR)),
    steps_sum_avg = mean(!!sym(STEPS)),
    cals_sum_avg = mean(!!sym(CALS)),
    bgplusone_avg = mean(!!sym(BGP1))
  )

# Function to create segment average plots
generate_segment_plot <- function(data, var, color, title, y_label) {
  ggplot(data, aes(x = time, y = .data[[var]])) +
    # Add points for each segment average
    geom_point(color = color, size = 3) +
    # Connect points with lines
    geom_line(color = color, linewidth = 1) +
    # Add labels
    labs(title = title,
         y = y_label,
         x = NULL) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      plot.margin = margin(t = 5, r = 10, b = 5, l = 10)
    )
}

# Generate individual plots
# Group 1
bg_seg_plt <- generate_segment_plot(segment_avgs, "bg_mean_avg",BG_COLOR, "Blood Glucose (x1)", "bg_mean (mmol/L)")
insulin_seg_plt <- generate_segment_plot(segment_avgs, "insulin_sum_avg",INSULIN_COLOR, "Insulin (x2)", "insulin_sum")
carbs_seg_plt <- generate_segment_plot(segment_avgs, "carbs_sum_avg",CARBS_COLOR, "Carbohydrate Intake (x3)", "carbs_sum")

carbs_seg_plt <- carbs_seg_plt + labs(x = "Time (hours)")

generate_combined_seg_plt <- function() {
  plot_annotation(
    title = glue("Time series with {segment_size} point segments"),
    subtitle = glue("Each point represents the average of {segment_size} consecutive observations"),
    theme = theme(
      plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 14, hjust = 0.5)
    )
  )
}

combined_seg_plt_1 <- bg_seg_plt / insulin_seg_plt / carbs_seg_plt + generate_combined_seg_plt()
  
combined_seg_plt_1

# Group 2
hr_seg_plt <- generate_segment_plot(segment_avgs, "hr_mean_avg", HR_COLOR, "Heart Rate (x4)", "hr_mean")
steps_seg_plt <- generate_segment_plot(segment_avgs, "steps_sum_avg", STEPS_COLOR, "Total Steps (x5)", "steps_sum")
cals_seg_plt <- generate_segment_plot(segment_avgs, "cals_sum_avg", CALS_COLOR, "Calories Burned (x6)", "cals_sum")

cals_seg_plt <- cals_seg_plt + labs(x = "Time (hours)")

combined_seg_plt_2 <- hr_seg_plt / steps_seg_plt / cals_seg_plt + generate_combined_seg_plt()

combined_seg_plt_2

# Group 3
bgp1_seg_plt <- generate_segment_plot(segment_avgs, "bgplusone_avg", BGP1_COLOR, "Blood Glucose after 1 hour (y) (Dependent Variable)", "bg+1:00 (mmol/L)") +
  labs(x = "Time (hours)") + generate_combined_seg_plt()

bgp1_seg_plt


# Histogram plots with density curves for independent variables

# Mode calculation function
calculate_mode <- function(x) {
  # Create frequency table
  ux <- unique(x)
  # Find the value with highest frequency
  ux[which.max(tabulate(match(x, ux)))]
}

# Function to create histogram with density plot and statistics
generate_hist_density_plot <- function(data, var, color, fill, title, x_label) {
  # Calculate statistics
  mean_val <- mean(data[[var]])
  median_val <- median(data[[var]])
  mode_val <- calculate_mode(round(data[[var]], 1))  # Round to 1 decimal for better mode calculation
  sd_val <- sd(data[[var]])
  skew_val <- moments::skewness(data[[var]])
  kurt_val <- moments::kurtosis(data[[var]]) - 3  # Excess kurtosis
  
  # Create the plot
  p <- ggplot(data, aes_string(x = var)) +
    # Add histogram
    geom_histogram(aes(y = after_stat(count)), 
                   bins = 30, 
                   fill = fill, 
                   color = "white", 
                   alpha = 0.7) +
    # Add density curve
    geom_density(aes(y = after_stat(count)), 
                 color = color, 
                 linewidth = 1) +
    # Add vertical lines for mean, median, and mode
    geom_vline(xintercept = mean_val, color = "red", linetype = "dashed", linewidth = 1) +
    geom_vline(xintercept = median_val, color = "green", linetype = "dashed", linewidth = 1) +
    geom_vline(xintercept = mode_val, color = "blue", linetype = "dotted", linewidth = 1) +
    # Add labels
    labs(title = title,
         x = x_label,
         y = "Frequency") +
    # Add statistics as text box
    annotate("text", 
             x = max(data[[var]]) * 0.5, 
             y = max(hist(data[[var]], plot = FALSE)$counts) * 0.5,
             label = sprintf("Mean: %.2f\nMedian: %.2f\nStd Dev: %.2f\nSkewness: %.2f\nKurtosis: %.2f", 
                             mean_val, median_val, sd_val, skew_val, kurt_val),
             hjust = 0, 
             vjust = 1,
             size = 3.5,
             color = "black",
             fontface = "plain",
             family = "sans") +
    # Add theme
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14)
    )
  
  return(p)
}

bg_mean_hist_plt <- generate_hist_density_plot(df, BG, BG_COLOR, BG_COLOR, BG_TITLE, BG_LABEL)
insulin_hist_plt <- generate_hist_density_plot(df, INSULIN, INSULIN_COLOR, INSULIN_COLOR, INSULIN_TITLE, INSULIN_LABEL)
carbs_hist_plt <- generate_hist_density_plot(df, CARBS, CARBS_COLOR, CARBS_COLOR, CARBS_TITLE, CARBS_LABEL)
hr_hist_plt <- generate_hist_density_plot(df, HR, HR_COLOR, HR_COLOR, HR_TITLE, HR_LABEL)
steps_hist_plt <- generate_hist_density_plot(df, STEPS, STEPS_COLOR, STEPS_COLOR, STEPS_TITLE, STEPS_LABEL)
cals_hist_plt <- generate_hist_density_plot(df, CALS, CALS_COLOR, CALS_COLOR, CALS_TITLE, CALS_LABEL)
bgp1_hist_plt <- generate_hist_density_plot(df, BGP1, BGP1_COLOR, BGP1_COLOR, BGP1_TITLE, BGP1_LABEL)

gen_hist_plot <- function() {
  plot_annotation(
    title = "Independent variables Distribution",
    theme = theme(
      plot.title = element_text(size = 20, hjust = 0.5, face = "bold")
    )
  )
}

# Arrange plots in 3x3 grid
combined_hist_plt1 <- (bg_mean_hist_plt + insulin_hist_plt) + gen_hist_plot()
combined_hist_plt1

combined_hist_plt2 <- (carbs_hist_plt + hr_hist_plt) + gen_hist_plot()
combined_hist_plt2

combined_hist_plt3 <- (steps_hist_plt + cals_hist_plt) + gen_hist_plot()
combined_hist_plt3

combined_hist_plt4 <- bgp1_hist_plt + gen_hist_plot()
combined_hist_plt4

# End of Histogram plots

# Q-Q Plots for all variables to check for their normality
variables <- c(BG, INSULIN, CARBS, HR, STEPS, CALS, BGP1)
labels <- c(BG_LABEL, INSULIN_LABEL, CARBS_LABEL, HR_LABEL, STEPS_LABEL, CALS_LABEL, BGP1_LABEL)
colors <- c(BG_COLOR, INSULIN_COLOR, CARBS_COLOR, HR_COLOR, STEPS_COLOR, CALS_COLOR, BGP1_COLOR)

layout(matrix(1:9, nrow=3, byrow = TRUE))

# Define plot parameters
par(
  mar = c(5, 5, 4.5, 2),  # Margins: bottom, left, top, right
  oma = c(0, 0, 5, 0),    # Outer margin for title
  bg = "white",
  col.axis = "black",
  col.lab = "black",
  col.main = "black",
  cex.axis = 1.3,         # Axis tick label size
  cex.lab = 1.5,          # Axis label size
  cex.main = 1.9          # Title size
)

for (i in seq_along(variables)) {
  var <- variables[i]
  label <- labels[i]
  color <- colors[i]
  
  # Create Q-Q plot
  qqnorm(df[[var]],
         main = label,
         col = adjustcolor(color, alpha.f = 0.75),
         pch = 19,          # Point character
         cex = 0.9)        # Point size
  
  # Add Q-Q line
  qqline(df[[var]], col = "red", lwd = 2)  # Q-Q line in red
  box(col = "black")                          # Box outline
}

plot.new()

mtext("All Variables Q-Q Plots:", outer = TRUE, line = 2, cex = 2.2, font = 2)

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)

# End of Q-Q Plots

# Scatter Plots

# Create a custom theme for the scatter plot matrix
custom_theme <- theme_minimal() +
  theme(
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10, face = "bold"),
    strip.text = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray95"),
    panel.border = element_rect(fill = NA, color = "gray80")
  )

# Set theme
theme_set(custom_theme)

# Select variables for the scatter plot matrix
plot_vars <- variables
plot_data <- df[, plot_vars]

# Create nicer variable labels
var_labels <- labels

names(plot_data) <- var_labels

# Create scatter plot matrix
splt <- ggpairs(
  plot_data,
  upper = list(
    continuous = wrap("cor", size = 3, color = "black")
  ),
  lower = list(
    continuous = function(data, mapping, ...) {
      ggplot(data, mapping) +
        geom_point(alpha = 0.3, size = 0.8, color = "#cf5951") +
        geom_smooth(method = "lm", color = "#100111", se = FALSE, linewidth = 0.8, message= FALSE, ...)
    }
  ),
  diag = list(
    continuous = function(data, mapping, ...) {
      ggplot(data, mapping) +
        geom_density(fill = "#cf5951", alpha = 0.7, ...)
    }
  )
) +
  ggtitle("Scatter Plot Matrix of the variables:") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 7),
    strip.text = element_text(size = 9)
  )

splt

# TASK 2 - Preprocessing for modeling
df_model <- df %>%
  select(
    time,
    bg_mean,
    insulin_sum,
    carbs_sum,
    hr_mean,
    steps_sum_log,
    cals_sum_log,
    bg_plus_1
  )

# Time based train/test data split
n <- nrow(df_model)
train_idx <- floor(0.8 * n)

train_df <- df_model[1:train_idx, ]
test_df <- df_model[(train_idx+1):n, ]

predictors <- c(
  "bg_mean",
  "insulin_sum",
  "carbs_sum",
  "hr_mean",
  "steps_sum_log",
  "cals_sum_log"
)

# Compute scaling parameters ONLY on training data
scaling_params <- lapply(train_df[predictors], function(x) {
  list(mean = mean(x), sd = sd(x))
})

scale_with_params <- function(data, params) {
  scaled <- data
  for (v in names(params)) {
    scaled[[v]] <- (data[[v]] - params[[v]]$mean) / params[[v]]$sd
  }
  scaled
}

# Optional train/test split

train_scaled <- scale_with_params(train_df, scaling_params)
test_scaled  <- scale_with_params(test_df, scaling_params)

train_scaled


X_train <- train_scaled[, predictors]
y_train <- train_scaled$bg_plus_1

X_test <- test_scaled[, predictors]
y_test <- test_scaled$bg_plus_1


# ================================
# TASK 2.1 — Preprocessing (ALL data)
# ================================

df_model_all <- df %>%
  select(
    bg_mean,
    insulin_sum,
    carbs_sum,
    hr_mean,
    steps_sum_log,
    cals_sum_log,
    bg_plus_1
  )

# Scale predictors AND target using full data
scale_vars <- c(
  "bg_mean",
  "insulin_sum",
  "carbs_sum",
  "hr_mean",
  "steps_sum_log",
  "cals_sum_log",
  "bg_plus_1"
)

scaling_params_all <- lapply(df_model_all[scale_vars], function(x) {
  list(mean = mean(x), sd = sd(x))
})

scale_all <- function(data, params) {
  scaled <- data
  for (v in names(params)) {
    scaled[[v]] <- (data[[v]] - params[[v]]$mean) / params[[v]]$sd
  }
  scaled
}

df_scaled_all <- scale_all(df_model_all, scaling_params_all)
head(df_scaled_all)

# 2.1 Model parameters estimation
# Model 1: β₁·x₁³ + β₂·x₂² + β₃·x₃² + β₄·x₄ + β₅·x₅ + β₆·x₆ + β₀

model_1 <- lm(
  bg_plus_1 ~ I(bg_mean^3) +
    I(insulin_sum^2) +
    I(carbs_sum^2) +
    hr_mean +
    steps_sum_log +
    cals_sum_log,
  data = df_scaled_all
)

# Model 2: β₁·x₁² + β₂·x₂² + β₃·x₃³ + β₄·x₄ + β₅·x₅ + β₆·x₆ + β₀
model_2 <- lm(
  bg_plus_1 ~ I(bg_mean^2) +
    I(insulin_sum^2) +
    I(carbs_sum^3) +
    hr_mean +
    steps_sum_log +
    cals_sum_log,
  data = df_scaled_all
)

# Model 3: β₁·x₁ + β₂·x₂ + β₃·x₃ + β₄·x₄² + β₅·x₅ + β₆·x₆² + β₀
model_3 <- lm(
  bg_plus_1 ~ bg_mean +
    insulin_sum +
    carbs_sum +
    I(hr_mean^2) +
    steps_sum_log +
    I(cals_sum_log^2),
  data = df_scaled_all
)

# Model 4: β₁·x₁² + β₂·x₂² + β₃·x₃² + β₄·x₄² + β₅·x₅² + β₆·x₆² + β₀
model_4 <- lm(
  bg_plus_1 ~ I(bg_mean^2) +
    I(insulin_sum^2) +
    I(carbs_sum^2) +
    I(hr_mean^2) +
    I(steps_sum_log^2) +
    I(cals_sum_log^2),
  data = df_scaled_all
)

# Model 5: β₁·x₁ + β₂·x₂ + β₃·x₃ + β₄·x₄ + β₅·x₅ + β₆·x₆ + β₇·x₁x₂ + β₈·x₃x₄ + β₉·x₂x₆ + β₀
model_5 <- lm(
  bg_plus_1 ~ bg_mean +
    insulin_sum +
    carbs_sum +
    hr_mean +
    steps_sum_log +
    cals_sum_log +
    bg_mean:insulin_sum +
    carbs_sum:hr_mean +
    insulin_sum:cals_sum_log,
  data = df_scaled_all
)

# Extracting coefficients
coef_list <- list(
  Model_1 = coef(model_1),
  Model_2 = coef(model_2),
  Model_3 = coef(model_3),
  Model_4 = coef(model_4),
  Model_5 = coef(model_5)
)

# Create unified parameter list
all_params <- unique(unlist(lapply(coef_list, names)))

# Build coefficient matrix
coef_table <- data.frame(
  Parameter = all_params,
  Model_1 = NA,
  Model_2 = NA,
  Model_3 = NA,
  Model_4 = NA,
  Model_5 = NA
)

for (i in seq_along(coef_list)) {
  model_name <- paste0("Model_", i)
  coef_table[[model_name]][
    match(names(coef_list[[i]]), coef_table$Parameter)
  ] <- round(coef_list[[i]], 4)
}

coef_table$Parameter <- gsub("\\(Intercept\\)", "Intercept (β₀)", coef_table$Parameter)
coef_table$Parameter <- gsub("bg_mean", "bg_mean (x1)", coef_table$Parameter)
coef_table$Parameter <- gsub("insulin_sum", "insulin_sum (x2)", coef_table$Parameter)
coef_table$Parameter <- gsub("carbs_sum", "carbs_sum (x3)", coef_table$Parameter)
coef_table$Parameter <- gsub("hr_mean", "hr_mean (x4)", coef_table$Parameter)
coef_table$Parameter <- gsub("steps_sum_log", "steps_sum_log (x5)", coef_table$Parameter)
coef_table$Parameter <- gsub("cals_sum_log", "cals_sum_log (x6)", coef_table$Parameter)

coef_table

coef_table_display <- coef_table
coef_table_display[,-1] <- lapply(
  coef_table_display[,-1],
  function(x) ifelse(is.na(x), "-", formatC(x, digits = 4, format = "f"))
)

# Parameter naming
library(stringr)

coef_table_display$Parameter <- coef_table_display$Parameter |>
  str_replace("\\(Intercept\\)", "β₀ (Intercept)") |>
  str_replace("I\\((.*)\\)", "\\1") |>
  str_replace("\\^2", "²") |>
  str_replace("\\^3", "³") |>
  str_replace(":", " × ")


library(gt)

coef_gt <- coef_table_display |>
  gt(rowname_col = "Parameter") |>
  tab_header(
    title = "Estimated Least Squares Coefficients",
    subtitle = "Nonlinear Regression Models (Task 2.1)"
  ) |>
  cols_label(
    Model_1 = "Model 1",
    Model_2 = "Model 2",
    Model_3 = "Model 3",
    Model_4 = "Model 4",
    Model_5 = "Model 5"
  ) |>
  opt_table_outline() |>
  tab_options(
    table.font.size = "small",
    data_row.padding = px(4)
  ) |>
  opt_all_caps()

coef_gt


# Distribution Plots
df %>%
  pivot_longer(cols = c(bg_mean, insulin_sum, carbs_sum, hr_mean, steps_sum, cals_sum, bg_plus_1),
               names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 40, fill = "grey70", color = "black") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Distributions of All Variables")

# Percentile table
percentiles <- df %>%
  summarise(across(everything(), list(
    p1  = ~ quantile(.x, 0.01, na.rm = TRUE),
    p5  = ~ quantile(.x, 0.05, na.rm = TRUE),
    p50 = ~ quantile(.x, 0.50, na.rm = TRUE),
    p95 = ~ quantile(.x, 0.95, na.rm = TRUE),
    p99 = ~ quantile(.x, 0.99, na.rm = TRUE)
  )))

print(round(percentiles, 3))



