# Load survey and dplyr packages
#+ message = FALSE, warning=FALSE
library(tidyverse)
library(survey)
library(sjPlot)
#'
options(survey.lonely.psu='adjust')

# Display Version Information
cat("R package versions:\n")
for (p in c("base", "survey","dplyr")) {
  cat(p, ": ", as.character(packageVersion(p)), "\n")
}

# Lendo a base --------------------------------------------------------------------------------
df <- read_rds(file = "df.rds")
colnames(df)

# DataPrep ------------------------------------------------------------------------------------
One <-
  df |>
  # remove duplicates
  dplyr::distinct(SEQN, .keep_all = TRUE) |>
  # adjusting physical activity - where is NA to change to zero
  dplyr::mutate(
    PAQ610 = tidyr::replace_na(PAQ605, 0),
    PAQ610 = tidyr::replace_na(PAQ610, 0),
    PAD615 = tidyr::replace_na(PAD615, 0),
    PAQ620 = tidyr::replace_na(PAQ620, 0),
    PAQ625 = tidyr::replace_na(PAQ625, 0),
    PAD630 = tidyr::replace_na(PAD630, 0),
    PAD630 = tidyr::replace_na(PAQ635, 0),
    PAQ640 = tidyr::replace_na(PAQ640, 0),
    PAD645 = tidyr::replace_na(PAD645, 0),
    PAD645 = tidyr::replace_na(PAQ650, 0),
    PAQ655 = tidyr::replace_na(PAQ655, 0),
    PAD660 = tidyr::replace_na(PAD660, 0),
    PAD660 = tidyr::replace_na(PAQ665, 0),
    PAQ670 = tidyr::replace_na(PAQ670, 0),
    PAD675 = tidyr::replace_na(PAD675, 0),
    PAD675 = tidyr::replace_na(PAD680, 0)
  ) |>
  # created physical activity outcomes
  dplyr::mutate(
    # PA work
    PAW = PAQ610 * PAD615 + PAQ625 * PAD630,
    # PA transport
    PAT = PAQ640 * PAD645,
    # PA leisure
    PAL = PAQ655 * PAD660 + PAQ670 * PAD675,
    PATOTAL = PAW + PAT + PAL,
    PA_CLASS = case_when(PATOTAL >= 150 ~ "ATIVO",
                         PATOTAL < 150 ~ "INATIVO")
  ) |>
  # to create BMI and obesity class
  dplyr::mutate(
    BMXHT = BMXHT / 100,
    BMI = BMXWT / (BMXHT^2),
    OBESITY = case_when(BMI >= 30 ~ "OBESO",
                        BMI < 30 ~ "NORMAL"),
    # create AGE CLASS
    AGE_CLASS = case_when(RIDAGEYR < 80 ~ "A_<80",
                          RIDAGEYR >= 80 ~ "B_>=80")
  ) |>
  # To create the variable INCAPAZ - PRIMARY OUTCOME
  dplyr::mutate(
    inAnalysis = (
      RIDAGEYR >= 65 &
        !is.na(PA_CLASS)
      # ENERGY_STATUS == 'LIKELY' & # veriricar se iremos incluir consumo alimentar no projeto
      # !is.na(ENERGY_PT_MODEL) &
      # DIQ010 < 3 & # Diabetes (1 = yes; 2 = no)
      # MCQ160F < 3 & # AVC (1 = yes; 2 = no)
      # MCQ160B < 3 & # ICC (1 = yes; 2 = no)
      # MCQ160E < 3 & # IAM (1 = yes; 2 = no)
      # MCQ220 < 3 & # cancer (1 = yes; 2 = no)
      # KIQ022 < 3 & # renal (1 = yes; 2 = no)
      # MCQ160O < 3 & # DPOC (1 = yes; 2 = no)
      # MCQ160L < 3  # hepatico (1 = yes; 2 = no)
    )
  )

#' ## Define survey design
# Define survey design for overall dataset
NHANES_all <- svydesign(data=One, id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTMEC2YR, nest=TRUE)

# Create a survey design object for the subset of interest: adults aged 20 and over with a valid depression score
# Subsetting the original survey design object ensures we keep the design information about the number of clusters and strata
NHANES <- subset(NHANES_all, inAnalysis)

# to verify number of protein < 0.80
nrow(NHANES$variables)

# Exploratory analysis ------------------------------------------------------------------------
# General Descriptive and distribution analysis
glimpse(NHANES$variables)
skimr::skim_without_charts(NHANES$variables)
DataExplorer::plot_missing(NHANES$variables)


NHANES$variables |>
  select(PATOTAL,
         CFDCST1,
         CFDCST2,
         CFDAST,
         CFDDS) |>
  cor(use = "pairwise.complete.obs") |>
  corrplot::corrplot(method = "number")

# Analysis - PA continua ------------------------------------------------------------------------------------
## crude logistic regression
crude_svy <-
  survey::svyglm(
    formula = as.factor(internação_ano) ~ as.factor(PA_CLASS),
    design = NHANES,
    family = binomial(link = "logit")
  )

# Summary
summary(crude_svy)
cbind(odds = exp(crude_svy$coefficients), exp(confint(crude_svy)))
sjPlot::tab_model(crude_svy)

