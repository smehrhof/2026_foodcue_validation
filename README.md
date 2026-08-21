# AI Nutritional Estimation and Stimulus Validation

This repository contains two R scripts used to evaluate the accuracy of AI-generated nutritional estimates and to validate a food-image stimulus set using participant ratings.

## Scripts

### 1. `AI_nutritional_estimation.R`

This script evaluates the accuracy of AI-generated estimates of the nutritional content of food images using the Food$Life image repository (Charbonnier et al., 2016; https://osf.io/cx7tp/files/osfstorage).

#### Required data

The scripts expect the following input files and directory structure:

```text
data/
└── 1_ai_estimation/
    └── AI_macroestimate_test.xlsx
```

#### Required R packages

```r
librarian::shelf(
  tidyverse,
  readxl,
  patchwork
)
```

#### Output

The scripts generate the following figure:

```text
plots/
└── ai_estimation_plots.png
```

### 2. `stimuli_validation.R`

This script evaluates the behavioural validity of the food-image stimulus set using participant ratings.

#### Required data

The scripts expect the following input files and directory structure:

```text
data/
└── 2_stimuli_validation/
    ├── dat.RDS
    ├── prolific_dat.RDS
    └── nutrient_data.xlsx
```

#### Required R packages

```r
librarian::shelf(
  tidyverse,
  readxl,
  viridis,
  lme4,
  lmerTest,
  emmeans,
  patchwork,
  PupillometryR
)
```

#### Output

The scripts generate the following figure:

```text
plots/
└── stimuli_validation_plots.png
```
