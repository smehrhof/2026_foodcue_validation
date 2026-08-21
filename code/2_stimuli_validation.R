##############################################################################
#################------------ VALIDATING STIMULI ------------#################
##############################################################################

### In this script: 
# (0) Set up
# (1) Data cleaning
# (2) Wanting and liking descriptives
# (3) Familiarity
# (4) Nutritional ratings
# (5) Metabolic state
# (6) Make figure

### (0) Set up -----------------------------------------------

# Load package
librarian::shelf(tidyverse, magrittr, readxl, viridis, lme4, lmerTest, emmeans, patchwork, PupillometryR)

# source data
dat <- readRDS("data/2_stimuli_validation/dat.RDS") 

prolific_dat <- readRDS("data/2_stimuli_validation/prolific_dat.RDS")

### (1) Data cleaning ----------------------------------------

# failed task attention check
# images set 1
set_1 <- dat$demographic_dat %>% filter(version == "A" | version == "B") %>% .$prolific_id
check_set_1 <- dat$task_1_attention %>%
  filter(prolific_id %in% set_1) %>% 
  mutate(check_1 = ifelse(response.attention_check_1 == "Orange", 1, 0),
         check_2 = ifelse(response.attention_check_2 == "Brown", 1, 0),
         check_3 = ifelse(response.attention_check_3 == "Green", 1, 0),
         check_4 = ifelse(response.attention_check_4 == "Blue", 1, 0),
         check = rowSums(across(check_1:check_4)))
t_excl <- check_set_1 %>% filter(check < 3) %>% .$prolific_id

# images set 2
set_2 <- dat$demographic_dat %>% filter(version == "C" | version == "D") %>% .$prolific_id
check_set_2 <- dat$task_1_attention %>%
  filter(prolific_id %in% set_2) %>% 
  mutate(check_1 = ifelse(response.attention_check_1 == "Green", 1, 0),
         check_2 = ifelse(response.attention_check_2 == "Brown", 1, 0),
         check_3 = ifelse(response.attention_check_3 == "Green", 1, 0),
         check_4 = ifelse(response.attention_check_4 == "Red", 1, 0),
         check = rowSums(across(check_1:check_4)))

t_excl <- c(t_excl, check_set_2 %>% filter(check < 3) %>% .$prolific_id)

# failed questionnaire attention check
dat$questionnaire_dat %>%
  select(prolific_id, starts_with("attention")) %>% print(n = Inf) 

q_excl <- c("7bd5c3e82f")

dat_clean <- map(dat, ~ filter(.x, !prolific_id %in% c(t_excl, q_excl)))

# check completion time in prolific data

prolific_dat %>%
  filter(as.numeric(`Time taken`) <= median_completion / 3) %>%
  pull(`Participant id`)

# no exclusions

### (2) Wanting and liking descriptives ----------------------------------------

dat_clean$task_1_dat %<>%
  mutate(
    calorie_category = case_when(
      str_detect(image, "high_cal") ~ "high",
      str_detect(image, "low_cal") ~ "low",
      TRUE ~ NA_character_
    ),
    taste_category = case_when(
      str_detect(image, "_sa") ~ "savoury",
      str_detect(image, "_sw") ~ "sweet",
      TRUE ~ NA_character_
    ),
    image_id = str_extract(image, "\\(\\d+\\)") %>% str_remove_all("[()]")
  ) 

# Wanting and liking per image category 

dat_clean$task_1_dat %>%
  group_by(category) %>%
  summarise(
    mean_liking = mean(liking_rating, na.rm = TRUE),
    median_liking = median(liking_rating, na.rm = TRUE),
    sd_liking = sd(liking_rating, na.rm = TRUE),
    mean_wanting = mean(wanting_rating, na.rm = TRUE),
    median_wanting = median(wanting_rating, na.rm = TRUE),
    sd_wanting = sd(wanting_rating, na.rm = TRUE)
  )

# linear mixed-effects model 

# Liking
liking_model <- lmer(
  liking_rating ~ calorie_category * taste_category +
    (1 | prolific_id),
  data = dat_clean$task_1_dat
)

# overall category effect
anova(liking_model)

# post hoc comparisons
emmeans(liking_model, pairwise ~ taste_category | calorie_category)
emmeans(liking_model, pairwise ~ calorie_category | taste_category)

# Wanting
wanting_model <- lmer(
  wanting_rating ~ calorie_category * taste_category +
    (1 | prolific_id),
  data = dat_clean$task_1_dat
)

# overall category effect
anova(wanting_model)

# post hoc comparisons
emmeans(wanting_model, pairwise ~ taste_category | calorie_category)
emmeans(wanting_model, pairwise ~ calorie_category | taste_category)

# Plot
want_like_summary <- dat_clean$task_1_dat %>%
  group_by(prolific_id, category) %>%
  summarise(
    mean_liking = mean(liking_rating, na.rm = TRUE),
    mean_wanting = mean(wanting_rating, na.rm = TRUE),
    .groups = "drop"
  )

# Liking
liking_plot <- want_like_summary %>% 
  ggplot(aes(x = category, y = mean_liking, fill = category)) +
  PupillometryR::geom_flat_violin(trim = FALSE, alpha = 0.6, color = NA) +
  geom_point(aes(color = category), position = position_jitter(width = 0.15), 
             size = 1.5, alpha = 0.4) + 
  geom_boxplot(width = 0.2, outlier.shape = NA, fill = "white", alpha = 0.6) +
  labs(
    x = " ",
    y = "Liking Rating (0-100)",
    fill = "Image Category"
  ) +
  scale_x_discrete(labels = c(
    "high_cal_sa" = "High calorie,\nsavoury", 
    "low_cal_sa"  = "Low calorie,\nsavoury",
    "high_cal_sw" = "High calorie,\nsweet", 
    "low_cal_sw"  = "Low calorie,\nsweet"
  )) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "none" 
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  scale_fill_viridis_d() +
  scale_color_viridis_d()

wanting_plot <- want_like_summary %>% 
  ggplot(aes(x = category, y = mean_wanting, fill = category)) +
  PupillometryR::geom_flat_violin(trim = FALSE, alpha = 0.6, color = NA) +
  geom_point(aes(color = category), position = position_jitter(width = 0.15), 
             size = 1.5, alpha = 0.4) + 
  geom_boxplot(width = 0.2, outlier.shape = NA, fill = "white", alpha = 0.6) +
  labs(
    x = " ",
    y = "Wanting Rating (0-100)",
    fill = "Image Category"
  ) +
  scale_x_discrete(labels = c(
    "high_cal_sa" = "High calorie,\nsavoury", 
    "low_cal_sa"  = "Low calorie,\nsavoury",
    "high_cal_sw" = "High calorie,\nsweet", 
    "low_cal_sw"  = "Low calorie,\nsweet"
  )) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "none" 
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  scale_fill_viridis_d() +
  scale_color_viridis_d()

### (3) Familiarity ----------------------------------------

image_familiarity <- dat_clean$task_1_dat %>%
  group_by(image, category) %>%
  summarise(
    n_ratings = n(),
    pct_familiar = mean(familiarity_response == "Yes", na.rm = TRUE) * 100,
    pct_unsure = mean(familiarity_response == "I'm not sure", na.rm = TRUE) * 100,
    pct_unfamiliar = mean(familiarity_response == "Not at all", na.rm = TRUE) * 100,
    .groups = "drop"
  )

image_familiarity %>%
  group_by(category) %>%
  summarise(
    mean_pct_familiar = mean(pct_familiar),
    sd_pct_familiar = sd(pct_familiar),
    min_pct_familiar = min(pct_familiar),
    max_pct_familiar = max(pct_familiar),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

familiarity_aov <- aov(pct_familiar ~ category, data = image_familiarity)
summary(familiarity_aov)

familiarity_posthoc <- emmeans(familiarity_aov, pairwise ~ category, adjust = "tukey")
familiarity_posthoc$contrasts

# Plot
# Prepare data with percentages
plot_data <- dat_clean$task_1_dat %>%
  count(category, familiarity_response) %>%
  group_by(category) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(familiarity_response = factor(familiarity_response, 
                                       levels = c("Yes", "I'm not sure", "Not at all")))

familiarity_plot <- ggplot(plot_data) +
  aes(x = category, y = percent, fill = familiarity_response) +
  geom_col() +
  theme_minimal() +
  labs(
    x = " ",
    y = "Percentage of ratings",
    fill = "Familiarity rating"
  ) +
  scale_x_discrete(labels = c(
    "high_cal_sa" = "High calorie,\nsavoury", 
    "low_cal_sa"  = "Low calorie,\nsavoury",
    "high_cal_sw" = "High calorie,\nsweet", 
    "low_cal_sw"  = "Low calorie,\nsweet"
  )) +
  scale_fill_viridis_d(
    option = "plasma",
    begin = 0.1,
    end = 0.8,
    alpha = 0.75
  )

### (4) Nutritional ratings ----------------------------------------

dat_clean$task_2_dat %<>%
  mutate(
    category = case_when(
      str_detect(image, "high_cal_sa") ~ "high_cal_sa",
      str_detect(image, "high_cal_sw") ~ "high_cal_sw",
      str_detect(image, "low_cal_sa") ~ "low_cal_sa",
      str_detect(image, "low_cal_sw") ~ "low_cal_sw",
      TRUE ~ NA_character_
    ),
    calorie_category = case_when(
      str_detect(image, "high_cal") ~ "high",
      str_detect(image, "low_cal") ~ "low",
      TRUE ~ NA_character_
    ),
    taste_category = case_when(
      str_detect(image, "_sa") ~ "savoury",
      str_detect(image, "_sw") ~ "sweet",
      TRUE ~ NA_character_
    ),
    image_id = str_extract(image, "\\(\\d+\\)") %>% str_remove_all("[()]")
  ) 

# Calorie rating
calorie_model <- lmer(
  calorie_rating ~ calorie_category + 
    (1 | prolific_id),
  data = dat_clean$task_2_dat
)

# difference in calorie ratings? 
summary(calorie_model)

# Plot 
task_2_dat_summary <- dat_clean$task_2_dat %>% 
  group_by(prolific_id, category) %>% 
  summarise(mean_calorie = mean(calorie_rating), 
            mean_carbs = mean(carbs_rating),
            mean_fats = mean(fats_rating), 
            mean_protein = mean(protein_rating)) 

high_low_cal_plot <- task_2_dat_summary %>% 
  ggplot(aes(x = category, y = mean_calorie, fill = category)) +
  PupillometryR::geom_flat_violin(trim = FALSE, alpha = 0.6, color = NA) +
  geom_point(aes(color = category), position = position_jitter(width = 0.15), 
             size = 1.5, alpha = 0.4) + 
  geom_boxplot(width = 0.2, outlier.shape = NA, fill = "white", alpha = 0.6) +
  labs(
    x = " ",
    y = "Estimated energy density",
    fill = "Image Category"
  ) +
  scale_x_discrete(labels = c(
    "high_cal_sa" = "High calorie,\nsavoury", 
    "high_cal_sw" = "High calorie,\nsweet", 
    "low_cal_sa"  = "Low calorie,\nsavoury", 
    "low_cal_sw"  = "Low calorie,\nsweet" 
  )) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "none" 
  ) +
  scale_y_continuous(
    limits = c(0, 6), 
    breaks = 0:6,
    labels = 1:7
  ) +
  scale_fill_viridis_d() +
  scale_color_viridis_d()


# Correlate participant nutrition ratings with objective values
nutrient_data <- read_excel("data/2_stimuli_validation/nutrient_data.xlsx")

nutrient_data %<>%
  mutate(
    category = case_when(
      str_detect(image, "high_cal_sa") ~ "high_cal_sa",
      str_detect(image, "high_cal_sw") ~ "high_cal_sw",
      str_detect(image, "low_cal_sa") ~ "low_cal_sa",
      str_detect(image, "low_cal_sw") ~ "low_cal_sw",
      TRUE ~ NA_character_
    ),
    image_id = str_extract(image, "\\(\\d+\\)") %>% str_remove_all("[()]")
  ) 

image_level_ratings <- dat_clean$task_2_dat %>%
  group_by(image) %>%
  summarise(
    mean_calorie_rating = mean(calorie_rating, na.rm = TRUE),
    mean_carbs_rating = mean(carbs_rating, na.rm = TRUE),
    mean_fats_rating = mean(fats_rating, na.rm = TRUE),
    mean_protein_rating = mean(protein_rating, na.rm = TRUE),
    n_ratings = n(),
    .groups = "drop"
  )

image_level_merged <- image_level_ratings %>%
  left_join(nutrient_data, by = "image")

cor.test(image_level_merged$mean_calorie_rating, 
                         image_level_merged$AI_calories)

cor.test(image_level_merged$mean_carbs_rating, 
                      image_level_merged$AI_carbohydrates)

cor.test(image_level_merged$mean_fats_rating, 
                     image_level_merged$AI_fat)

cor.test(image_level_merged$mean_protein_rating, 
                        image_level_merged$AI_protein)


# Plots
plot_colour <- viridis::plasma(4, begin = 0.1, end = 0.6)

# Calories
calorie_plot <- image_level_merged %>%
  ggplot(aes(x = mean_calorie_rating, 
             y = AI_calories)) +
  geom_point(
    alpha = 0.4, 
    size = 1.5, 
    color = plot_colour[1]
  ) +
  geom_smooth(
    method = "lm", 
    alpha = 0.2, 
    size = 1.2, 
    color = plot_colour[1], 
    fill = plot_colour[1]
  ) +
  labs(
    x = "Calorie ratings (1-7)",
    y = "Objective calories"
  ) +
  scale_x_continuous(
    breaks = 0:6,
    labels = 1:7
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

# Carbs
carbs_plot <- image_level_merged %>%
  ggplot(aes(x = mean_carbs_rating, 
             y = AI_carbohydrates)) +
  geom_point(
    alpha = 0.4, 
    size = 1.5, 
    color = plot_colour[2]
  ) +
  geom_smooth(
    method = "lm", 
    alpha = 0.2, 
    size = 1.2, 
    color = plot_colour[2], 
    fill = plot_colour[2]
  ) +
  labs(
    x = "Carbohydrate ratings (1-7)",
    y = "Objective carbohydrates"
  ) +
  scale_x_continuous(
    breaks = 0:6,
    labels = 1:7
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )


# Fat
fats_plot <- image_level_merged %>%
  ggplot(aes(x = mean_fats_rating, 
             y = AI_fat)) +
  geom_point(
    alpha = 0.4, 
    size = 1.5, 
    color = plot_colour[3]
  ) +
  geom_smooth(
    method = "lm", 
    alpha = 0.2, 
    size = 1.2, 
    color = plot_colour[3], 
    fill = plot_colour[3]
  ) +
  labs(
    x = "Fat ratings (1-7)",
    y = "Objective fats"
  ) +
  scale_x_continuous(
    breaks = 0:6,
    labels = 1:7
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

# Protein
protein_plot <- image_level_merged %>%
  ggplot(aes(x = mean_protein_rating, 
             y = AI_protein)) +
  geom_point(
    alpha = 0.4, 
    size = 1.5, 
    color = plot_colour[4]
  ) +
  geom_smooth(
    method = "lm", 
    alpha = 0.2, 
    size = 1.2, 
    color = plot_colour[4], 
    fill = plot_colour[4]
  ) +
  labs(
    x = "Protein ratings (1-7)",
    y = "Objective protein"
  ) +
  scale_x_continuous(
    breaks = 0:6,
    labels = 1:7
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

calorie_plot
carbs_plot
fats_plot
protein_plot

### (5) Metabolic state ----------------------------------------

task_1_hunger <- dat_clean$task_1_dat %>%
  left_join(dat_clean$demographic_dat %>% select(prolific_id, hunger, fullness, thirst), 
            by = "prolific_id")

hunger_liking_model <- lmer(
  liking_rating ~ hunger * calorie_category + 
    (1 | prolific_id),
  data = task_1_hunger
)

summary(hunger_liking_model)

hunger_wanting_model <- lmer(
  wanting_rating ~ hunger * calorie_category + 
    (1 | prolific_id),
  data = task_1_hunger
)

summary(hunger_wanting_model)

want_like_summary_hunger <- dat_clean$task_1_dat %>%
  group_by(prolific_id, calorie_category) %>%
  summarise(
    mean_liking = mean(liking_rating, na.rm = TRUE),
    mean_wanting = mean(wanting_rating, na.rm = TRUE),
    .groups = "drop"
  )  %>%
  left_join(dat_clean$demographic_dat %>% select(prolific_id, hunger), 
            by = "prolific_id")

# Liking
plot_colours <- c(
  "high" = viridis::viridis(7)[2],
  "low" = viridis::viridis(7)[6]
)

hunger_liking_plot <- want_like_summary_hunger %>%
  ggplot(aes(
    x = hunger, 
    y = mean_liking, 
    color = calorie_category, 
    fill = calorie_category
  )) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_smooth(method = "lm", alpha = 0.2, size = 1.2) +
  scale_color_manual(
    name = "Image Category",
    values = plot_colours,
    labels = c("High calorie", "Low calorie")
  ) +
  scale_fill_manual(
    name = " ",
    values = plot_colours,
    labels = c("High calorie", "Low calorie")
  ) +
  guides(fill = "none") +
  labs(
    x = "Hunger ratings",
    y = "Liking Rating (0-100)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "right"
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25))

hunger_wanting_plot <- want_like_summary_hunger %>%
  ggplot(aes(
    x = hunger, 
    y = mean_wanting, 
    color = calorie_category, 
    fill = calorie_category
  )) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_smooth(method = "lm", alpha = 0.2, size = 1.2) +
  scale_color_manual(
    name = "Image Category",
    values = plot_colours,
    labels = c("High calorie", "Low calorie")
  ) +
  scale_fill_manual(
    name = " ",
    values = plot_colours,
    labels = c("High calorie", "Low calorie")
  ) +
  guides(fill = "none") +
  labs(
    x = "Hunger ratings",
    y = "Wanting Rating (0-100)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "right"
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25))


### (6) Make figure -----------------------------------------------

# Arrange all plots

wanting_liking_plots <- (liking_plot | wanting_plot)

familiarity_plot <- familiarity_plot +
  theme(legend.position = "none")

nutrient_plots <- (calorie_plot | carbs_plot | fats_plot | protein_plot)

hunger_plots <- (hunger_liking_plot | hunger_wanting_plot) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

stimuli_validation_final_plot <- 
  wanting_liking_plots /
  (familiarity_plot | high_low_cal_plot) /
  nutrient_plots /
  hunger_plots

stimuli_validation_final_plot

# output
ggsave(
  "plots/stimuli_validation_plots.png",
  plot = stimuli_validation_final_plot,
  width = 12,
  height = 14,
  units = "in"
)


