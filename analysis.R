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

# tirar notação cientifica
options(scipen=999)

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
                          RIDAGEYR >= 80 ~ "B_>=80"),
    # best CFDCST1
    CERAD_BEST = apply(df[,5:7], MARGIN = 1,FUN = max),
    # CFDAST_SCORE
    CFDAST_SCORE = case_when(
      CFDAST <= 5 ~ "0 pontos",
      CFDAST > 5 & CFDAST <= 8 ~ "1 pontos",
      CFDAST > 8 & CFDAST <= 11 ~ "2 pontos",
      CFDAST > 11 & CFDAST <= 14 ~ "3 pontos",
      CFDAST > 14 ~ "4 pontos"
    )
  ) |>
  # final survey
  dplyr::mutate(
    inAnalysis = (
      RIDAGEYR >= 65 &
        !is.na(PATOTAL) &
        !is.na(CFDAST) &
        !is.na(CFDDS) &
        !is.na(CERAD_BEST)&
        !is.na(CFDCSR)&
        !is.na(OBESITY)&
        PAQ610 < 8 &
        PAD615 < 841 &
        PAQ625 < 8 &
        PAD630 < 841 &
        PAQ640 < 8 &
        PAD645 < 661 &
        PAQ655 < 7 &
        PAD660 < 481 &
        PAQ670 < 8 &
        PAD675 < 540 &
        DMDEDUC2 < 7 & # escolaridade
        SMQ020 < 3 & # tabacco (1 = yes; 2 = no)
        DIQ010 < 3 & # Diabetes (1 = yes; 2 = no)
        MCQ160F < 3 & # AVC (1 = yes; 2 = no)
        BPQ040A < 3  # HAS (1 = yes; 2 = no)
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
         CFDDS,
         CFDCSR,
         CFDAST,
         CERAD_BEST) |>
  cor(use = "pairwise.complete.obs") |>
  corrplot::corrplot(method = "number",sig.level = 0.05)

# analyses ------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------
bgrouped <-
  NHANES$variable |>
  dplyr::group_by(PA_CLASS) |>
  dplyr::summarise(media = mean(CFDDS))

g1 <-
  NHANES$variables |>
  ggplot(aes(x=PA_CLASS,y=CFDDS,color = PA_CLASS))+
  geom_boxplot(outlier.size = 1,
               notch = FALSE,
               show.legend = FALSE)+
  geom_point(data = bgrouped,
             mapping = aes(x = PA_CLASS,
                           y = media),
             color = 'black',
             show.legend = FALSE,
             shape = 3,
             size = 2,
             stroke = 1.5)+
  theme_classic() +
  xlab(label = "")


t.test(NHANES$variable$CFDDS ~ NHANES$variable$PA_CLASS,
       NHANES$variable)
# ---------------------------------------------------------------------------------------------
bgrouped <-
  NHANES$variable |>
  dplyr::group_by(PA_CLASS) |>
  dplyr::summarise(media = mean(CFDCSR))

g2 <-
  NHANES$variables |>
  ggplot(aes(x=PA_CLASS,y=CFDCSR,color=PA_CLASS))+
  geom_boxplot(outlier.size = 1,
               show.legend = FALSE)+
  geom_point(data = bgrouped,
             mapping = aes(x = PA_CLASS,
                           y = media),
             color = 'black',
             show.legend = FALSE,
             shape = 3,
             size = 2,
             stroke = 1.5)+
  theme_classic() +
  xlab(label = '')

t.test(NHANES$variable$CFDCSR ~ NHANES$variable$PA_CLASS,
       NHANES$variable)
# ---------------------------------------------------------------------------------------------
bgrouped <-
  NHANES$variable |>
  dplyr::group_by(PA_CLASS) |>
  dplyr::summarise(media = mean(CFDAST))

g3 <-
  NHANES$variables |>
  ggplot(aes(x=PA_CLASS, y=CFDAST,color=PA_CLASS))+
  geom_boxplot(outlier.size = 1,
               show.legend = FALSE)+
  geom_point(data = bgrouped,
             mapping = aes(x = PA_CLASS,
                           y = media),
             color = 'black',
             show.legend = FALSE,
             shape = 3,
             size = 2,
             stroke = 1.5)+
  theme_classic()+
  xlab(label = '')


t.test(NHANES$variable$CFDAST ~ NHANES$variable$PA_CLASS,
       NHANES$variable)
# ---------------------------------------------------------------------------------------------
bgrouped <-
  NHANES$variable |>
  dplyr::group_by(PA_CLASS) |>
  dplyr::summarise(media = mean(CERAD_BEST))

g4 <-
  NHANES$variables |>
  ggplot(aes(x=PA_CLASS,y=CERAD_BEST,color=PA_CLASS))+
  geom_boxplot(outlier.size = 1,
               show.legend = FALSE)+
  geom_point(data = bgrouped,
             mapping = aes(x = PA_CLASS,
                           y = media),
             color = 'black',
             show.legend = FALSE,
             shape = 3,
             size = 2,
             stroke = 1.5)+
  theme_classic() +
  xlab(label = '')

t.test(NHANES$variable$CERAD_BEST ~ NHANES$variable$PA_CLASS,
       NHANES$variable)

# layout --------------------------------------------------------------------------------------
library(patchwork)
(g4+g2)/(g3+g1)

# CFDDS ---------------------------------------------------------------------------------------------
## crude linear regression
crude_svy <-
  survey::svyglm(
    formula = CFDDS ~ PA_CLASS,
    design = NHANES)

# Summary
summary(crude_svy)
sjPlot::tab_model(crude_svy)

## adjusted linear regression - age and sex
m1_crude_svy <-
  survey::svyglm(
    formula = CFDDS ~ PA_CLASS + AGE_CLASS + RIAGENDR,
    design = NHANES)

# Summary
summary(m1_crude_svy)
sjPlot::tab_model(m1_crude_svy)

## adjusted linear regression - CFDDS - model 1 + DMDEDUC2, SMQ020, BPQ040A, MCQ160F, OBESITY, DIQ010
m2_crude_svy <-
  survey::svyglm(
    formula = CFDDS ~ PA_CLASS + AGE_CLASS + RIAGENDR + DMDEDUC2 + SMQ020 + BPQ040A + MCQ160F + OBESITY + DIQ010,
    design = NHANES)

# Summary
summary(m2_crude_svy)
sjPlot::tab_model(m2_crude_svy)

# CFDAST ---------------------------------------------------------------------------------------------
## crude linear regression
crude_svy <-
  survey::svyglm(
    formula = CFDAST ~ PA_CLASS,
    design = NHANES)

# Summary
summary(crude_svy)
sjPlot::tab_model(crude_svy)

## adjusted linear regression - age and sex
m1_crude_svy <-
  survey::svyglm(
    formula = CFDAST ~ PA_CLASS + AGE_CLASS + RIAGENDR,
    design = NHANES)

# Summary
summary(m1_crude_svy)
sjPlot::tab_model(m1_crude_svy)

## adjusted linear regression - CFDDS - model 1 + DMDEDUC2, SMQ020, BPQ040A, MCQ160F, OBESITY, DIQ010
m2_crude_svy <-
  survey::svyglm(
    formula = CFDAST ~ PA_CLASS + AGE_CLASS + RIAGENDR + DMDEDUC2 + SMQ020 + BPQ040A + MCQ160F + OBESITY + DIQ010,
    design = NHANES)

# Summary
summary(m2_crude_svy)
sjPlot::tab_model(m2_crude_svy)

# CERAD_BEST ---------------------------------------------------------------------------------------------
## crude linear regression
crude_svy <-
  survey::svyglm(
    formula = CERAD_BEST ~ PA_CLASS,
    design = NHANES)

# Summary
summary(crude_svy)
sjPlot::tab_model(crude_svy)

## adjusted linear regression - age and sex
m1_crude_svy <-
  survey::svyglm(
    formula = CERAD_BEST ~ PA_CLASS + AGE_CLASS + RIAGENDR,
    design = NHANES)

# Summary
summary(m1_crude_svy)
sjPlot::tab_model(m1_crude_svy)

## adjusted linear regression - CFDDS - model 1 + DMDEDUC2, SMQ020, BPQ040A, MCQ160F, OBESITY, DIQ010
m2_crude_svy <-
  survey::svyglm(
    formula = CERAD_BEST ~ PA_CLASS + AGE_CLASS + RIAGENDR + DMDEDUC2 + SMQ020 + BPQ040A + MCQ160F + OBESITY + DIQ010,
    design = NHANES)

# Summary
summary(m2_crude_svy)
sjPlot::tab_model(m2_crude_svy)

# CFDCSR ---------------------------------------------------------------------------------------------
## crude linear regression
crude_svy <-
  survey::svyglm(
    formula = CFDCSR ~ PA_CLASS,
    design = NHANES)

# Summary
summary(crude_svy)
sjPlot::tab_model(crude_svy)

## adjusted linear regression - age and sex
m1_crude_svy <-
  survey::svyglm(
    formula = CFDCSR ~ PA_CLASS + AGE_CLASS + RIAGENDR,
    design = NHANES)

# Summary
summary(m1_crude_svy)
sjPlot::tab_model(m1_crude_svy)

## adjusted linear regression - CFDDS - model 1 + DMDEDUC2, SMQ020, BPQ040A, MCQ160F, OBESITY, DIQ010
m2_crude_svy <-
  survey::svyglm(
    formula = CFDCSR ~ PA_CLASS + AGE_CLASS + RIAGENDR + DMDEDUC2 + SMQ020 + BPQ040A + MCQ160F + OBESITY + DIQ010,
    design = NHANES)

# Summary
summary(m2_crude_svy)
sjPlot::tab_model(m2_crude_svy)

