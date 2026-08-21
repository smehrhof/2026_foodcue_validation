################################################################################################
#################------------ VALIDATING AI NUTRUTIONAL ESTIMATION ------------#################
################################################################################################

### In this script: 
# (0) Set up
# (1) Calculate error metrics
# (2) Bland-Altman plots
# (3) Confidence range analysis
# (4) Make figure 

### (0) Set up -----------------------------------------------

# Load package
librarian::shelf(tidyverse, readxl, patchwork)

# Read Excel file
data <- read_excel("data/1_ai_estimation/AI_macroestimate_test.xlsx")

data <- data %>%
  # convert point estimates to numeric
  mutate(across(c(AI_calories, AI_fat, AI_carbohydrates, AI_protein),
                ~ as.numeric(.x))) %>%
  # split each range column ("510, 560") into lower/upper numeric columns
  separate(AI_calories_range, into = c("AI_calories_lower", "AI_calories_upper"),
           sep = ",\\s*", convert = TRUE, remove = TRUE) %>%
  separate(AI_fat_range, into = c("AI_fat_lower", "AI_fat_upper"),
           sep = ",\\s*", convert = TRUE, remove = TRUE) %>%
  separate(AI_carbohydrates_range, into = c("AI_carbohydrates_lower", "AI_carbohydrates_upper"),
           sep = ",\\s*", convert = TRUE, remove = TRUE) %>%
  separate(AI_protein_range, into = c("AI_protein_lower", "AI_protein_upper"),
           sep = ",\\s*", convert = TRUE, remove = TRUE)


### (1) Calculate error metrics -----------------------------------------------

# Error metrics function 
calc_metrics <- function(actual, predicted) {
  error <- predicted - actual
  abs_error <- abs(error)
  
  tibble(
    MAE  = mean(abs_error, na.rm = TRUE),
    RMSE = sqrt(mean(error^2, na.rm = TRUE))
  )
}

# Apply across all categories 
calc_metrics(data$real_calories, data$AI_calories) %>%
  mutate(nutrient = "Calories")

calc_metrics(data$real_carbohydrates, data$AI_carbohydrates) %>%
  mutate(nutrient = "Carbohydrates")

calc_metrics(data$real_fat, data$AI_fat) %>%
  mutate(nutrient = "Fat")

calc_metrics(data$real_protein, data$AI_protein) %>%
  mutate(nutrient = "Protein")

# Separately for sweet and savoury
# Savoury
data_sa <- data %>% filter(category == "SA")

calc_metrics(data_sa$real_calories, data_sa$AI_calories) %>%
  mutate(nutrient = "Calories")

calc_metrics(data_sa$real_carbohydrates, data_sa$AI_carbohydrates) %>%
  mutate(nutrient = "Carbohydrates")

calc_metrics(data_sa$real_fat, data_sa$AI_fat) %>%
  mutate(nutrient = "Fat")

calc_metrics(data_sa$real_protein, data_sa$AI_protein) %>%
  mutate(nutrient = "Protein")

# Sweet
data_sw <- data %>% filter(category == "SW")

calc_metrics(data_sw$real_calories, data_sw$AI_calories) %>%
  mutate(nutrient = "Calories")

calc_metrics(data_sw$real_carbohydrates, data_sw$AI_carbohydrates) %>%
  mutate(nutrient = "Carbohydrates")

calc_metrics(data_sw$real_fat, data_sw$AI_fat) %>%
  mutate(nutrient = "Fat")

calc_metrics(data_sw$real_protein, data_sw$AI_protein) %>%
  mutate(nutrient = "Protein")

### (2) Bland-Altman plots -----------------------------------------------

# data in long format 
prep_bland_altman <- function(actual, predicted, nutrient_name, image_nr) {
  tibble(
    image_nr = image_nr,
    mean_val = (actual + predicted) / 2,
    diff_val = predicted - actual,
    nutrient = nutrient_name
  )
}

ba_data <- bind_rows(
  prep_bland_altman(data$real_calories, data$AI_calories, "Calories", data$image_nr),
  prep_bland_altman(data$real_fat, data$AI_fat, "Fat", data$image_nr),
  prep_bland_altman(data$real_carbohydrates, data$AI_carbohydrates, "Carbohydrates", data$image_nr),
  prep_bland_altman(data$real_protein, data$AI_protein, "Protein", data$image_nr)
) %>%
  mutate(nutrient = factor(nutrient, levels = c("Calories", "Fat", "Carbohydrates", "Protein")))

# Calculate bias (mean difference) and 95% limits of agreement
ba_stats <- ba_data %>%
  group_by(nutrient) %>%
  summarise(
    bias = mean(diff_val, na.rm = TRUE),
    sd_diff = sd(diff_val, na.rm = TRUE),
    upper_loa = bias + 1.96 * sd_diff,
    lower_loa = bias - 1.96 * sd_diff,
    .groups = "drop"
  )
ba_stats

# Bland-Altman plot 

nutrient_colours <- c(
  "Calories" = "#42049EFF",
  "Carbohydrates" = "#8405A7FF",
  "Fat" = "#BB3488FF",
  "Protein" = "#E16462FF"
)

ba_plot <- ba_data %>%
  left_join(ba_stats, by = "nutrient") %>%
  ggplot(aes(x = mean_val, y = diff_val)) +
  geom_point(
    aes(color = nutrient),
    alpha = 0.25,
    size = 1.8
  ) +
  geom_hline(
    aes(yintercept = bias, color = nutrient),
    linewidth = 0.8
  ) +
  geom_hline(
    aes(yintercept = upper_loa, color = nutrient),
    linetype = "dashed",
    linewidth = 0.5
  ) +
  geom_hline(
    aes(yintercept = lower_loa, color = nutrient),
    linetype = "dashed",
    linewidth = 0.5
  ) +
  scale_color_manual(values = nutrient_colours) +
  facet_wrap(~ nutrient, scales = "free", nrow = 1) +
  labs(
    x = "Mean of AI estimate and objective value",
    y = "Difference (AI estimate − objective value)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )

ba_plot

# What are the images falling outside limits of agreement?

outliers <- ba_data %>%
  left_join(ba_stats, by = "nutrient") %>%
  filter(diff_val > upper_loa | diff_val < lower_loa) %>%
  left_join(data %>% select(image_nr, description, AI_description), by = "image_nr") %>%
  arrange(nutrient, desc(abs(diff_val)))

print(outliers, n = Inf)

### (3) Confidence range analysis -----------------------------------------------

# Does CI width relate to estimation error? 

ci_width_data <- data %>%
  transmute(
    image_nr,
    calories_width = AI_calories_upper - AI_calories_lower,
    calories_abs_error = abs(AI_calories - real_calories),
    fat_width = AI_fat_upper - AI_fat_lower,
    fat_abs_error = abs(AI_fat - real_fat),
    carbohydrates_width = AI_carbohydrates_upper - AI_carbohydrates_lower,
    carbohydrates_abs_error = abs(AI_carbohydrates - real_carbohydrates),
    protein_width = AI_protein_upper - AI_protein_lower,
    protein_abs_error = abs(AI_protein - real_protein)
  )

# Correlate CI width with absolute error, per nutrient
cor.test(ci_width_data$calories_width, ci_width_data$calories_abs_error, use = "complete.obs")
cor.test(ci_width_data$carbohydrates_width, ci_width_data$carbohydrates_abs_error, use = "complete.obs")
cor.test(ci_width_data$fat_width, ci_width_data$fat_abs_error, use = "complete.obs")
cor.test(ci_width_data$protein_width, ci_width_data$protein_abs_error, use = "complete.obs")


# plot relationships
ci_width_long <- ci_width_data %>%
  pivot_longer(
    cols = -image_nr,
    names_to = c("nutrient", ".value"),
    names_pattern = "(.*)_(width|abs_error)"
  ) %>%
  mutate(nutrient = factor(nutrient, 
                           levels = c("calories", "fat", "carbohydrates", "protein"),
                           labels = c("Calories", "Fat", "Carbohydrates", "Protein")))

ci_width_long_jitter <- ci_width_long %>%
  group_by(nutrient) %>%
  mutate(
    width_range = diff(range(width, na.rm = TRUE)),
    width_jitter = width + runif(
      n(),
      -0.01 * width_range,
      0.01 * width_range
    )
  ) %>%
  ungroup()

ci_width_plot <- ci_width_long_jitter %>%
  ggplot(aes(x = width_jitter, y = abs_error)) +
  geom_point(
    aes(color = nutrient),
    alpha = 0.2
  ) +
  geom_smooth(
    aes(x = width, color = nutrient),
    method = "lm",
    se = TRUE
  ) +
  scale_color_manual(values = nutrient_colours) +
  facet_wrap(~ nutrient, scales = "free", nrow = 1) +
  labs(
    x = "Width of AI confidence interval",
    y = "Absolute estimation error"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )

ci_width_plot

### (4) Make figure -----------------------------------------------

final_plot <- 
  ba_plot /
  ci_width_plot

final_plot

# output
ggsave(
  "plots/ai_estimation_plots.png",
  plot = final_plot,
  width = 10,
  height = 7,
  units = "in"
)



