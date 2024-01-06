#' Example R code to replicate NCHS Data Brief No.303, Figures 1
#' Prevalence of Depression Among Adults Aged 20 and Over: United States, 2013-2016

#' Brody DJ, Pratt LA, Hughes JP. Prevalence of Depression Among Adults Aged 20 and Over: United
#' States, 2013-2016. NCHS Data Brief. No 303. Hyattsville, MD: National Center for Health Statistics. 2018.

#' Available at: https://www.cdc.gov/nchs/products/databriefs/db303.htm

# Project - Association between PA and cognitive function in older adults ----------------------

#' ---------------------------------------------------------------------------------------------

# Load survey and dplyr packages
#+ message = FALSE, warning=FALSE
library(tidyverse)
library(survey)
library(GGally)
library(sjPlot)
#'
options(survey.lonely.psu='adjust')

# Display Version Information
cat("R package versions:\n")
for (p in c("base", "survey","dplyr")) {
  cat(p, ": ", as.character(packageVersion(p)), "\n")
}

#' # Data preparation
# Download & Read Transport Files

# Demographic ---------------------------------------------------------------------------------
# 11-12
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DEMO_G.XPT", tf <- tempfile(), mode="wb")
DEMO_11 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "RIDSTATR",
                                       "RIAGENDR",
                                       "RIDAGEYR",
                                       "RIDRETH1",
                                       "RIDRETH3",
                                       "DMDBORN4",
                                       "DMDEDUC2",
                                       "INDHHIN2",
                                       "SDMVSTRA",
                                       "SDMVPSU",
                                       "WTINT2YR",
                                       "WTMEC2YR"
                                       )]

# 13-14
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DEMO_H.XPT", tf <- tempfile(), mode="wb")
DEMO_13 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "RIDSTATR",
                                       "RIAGENDR",
                                       "RIDAGEYR",
                                       "RIDRETH1",
                                       "RIDRETH3",
                                       "DMDBORN4",
                                       "DMDEDUC2",
                                       "INDHHIN2",
                                       "SDMVSTRA",
                                       "SDMVPSU",
                                       "WTINT2YR",
                                       "WTMEC2YR"
)]

# Cognitive functioning------------------------------------------------------------------------
# 11-12
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/CFQ_G.XPT", tf <- tempfile(), mode="wb")
COGN_11 <- foreign::read.xport(tf)[, c("SEQN",
                                       "CFASTAT",
                                       "CFDCCS",
                                       "CFDCRNC",
                                       "CFDCST1",
                                       "CFDCST2",
                                       "CFDCST3",
                                       "CFDCSR",
                                       "CFDCIT1",
                                       "CFDCIT2",
                                       "CFDCIT3",
                                       "CFDCIR",
                                       "CFDARNC",
                                       "CFDAST",
                                       "CFDDRNC",
                                       "CFDDS")]

# 13-14
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/CFQ_H.XPT", tf <- tempfile(), mode="wb")
COGN_13 <- foreign::read.xport(tf)[, c("SEQN",
                                       "CFASTAT",
                                       "CFDCCS",
                                       "CFDCRNC",
                                       "CFDCST1",
                                       "CFDCST2",
                                       "CFDCST3",
                                       "CFDCSR",
                                       "CFDCIT1",
                                       "CFDCIT2",
                                       "CFDCIT3",
                                       "CFDCIR",
                                       "CFDARNC",
                                       "CFDAST",
                                       "CFDDRNC",
                                       "CFDDS")]

# Physical activity --------------------------------------------------------------------------
# 11-12
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/PAQ_G.XPT", tf <- tempfile(), mode="wb")
PA_11 <- foreign::read.xport(tf)[, c("SEQN",
                                     "PAQ706",
                                     "PAQ605",
                                     "PAQ610",
                                     "PAD615",
                                     "PAQ620",
                                     "PAQ625",
                                     "PAD630",
                                     "PAQ635",
                                     "PAQ640",
                                     "PAD645",
                                     "PAQ650",
                                     "PAQ655",
                                     "PAD660",
                                     "PAQ665",
                                     "PAQ670",
                                     "PAD675",
                                     "PAD680",
                                     "PAQ710",
                                     "PAQ715",
                                     "PAAQUEX")]

# 13-14
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/PAQ_H.XPT", tf <- tempfile(), mode="wb")
PA_13 <- foreign::read.xport(tf)[, c("SEQN",
                                     "PAQ706",
                                     "PAQ605",
                                     "PAQ610",
                                     "PAD615",
                                     "PAQ620",
                                     "PAQ625",
                                     "PAD630",
                                     "PAQ635",
                                     "PAQ640",
                                     "PAD645",
                                     "PAQ650",
                                     "PAQ655",
                                     "PAD660",
                                     "PAQ665",
                                     "PAQ670",
                                     "PAD675",
                                     "PAD680",
                                     "PAQ710",
                                     "PAQ715",
                                     "PAAQUEX")]
# Append Files ---------------------------------------------------------------------------------
# Demographics
DEMO <- dplyr::bind_rows(DEMO_11,
                         DEMO_13)

DEMO |> dplyr::distinct(SEQN, .keep_all = TRUE) |>  # testing duplicade rows - "none"
  nrow()

# Cognitive functiong
COGN <- dplyr::bind_rows(COGN_11,
                         COGN_13)

COGN |> dplyr::distinct(SEQN, .keep_all = TRUE)|>  # testing duplicade rows - "none"
  nrow() # testing duplicade rows - "none"

# Physical activity
PA <- dplyr::bind_rows(PA_11,
                       PA_13)

PA |> dplyr::distinct(SEQN, .keep_all = TRUE)|>  # testing duplicade rows - "none"
  nrow() # testing duplicade rows - "none"


# Merge COGN and DEMO files
COGN_DEMO <-
  dplyr::left_join(COGN, DEMO, by="SEQN")

# Merge COGN_DEMO and PA
COGN_DEMO_PA <-
  dplyr::left_join(COGN_DEMO, PA, by="SEQN")

# Rename to new object - df
df <- COGN_DEMO_PA

# testing duplicates
df |>
  dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "None"

# Created new variables -----------------------------------------------------------------------

###### salvando data.frame para explorar

# readr::write_rds(x = df, file = "df.rds") # esta comentado para não correr o risco de sobrescrever arquivo

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

# Exploratory analysis ------------------------------------------------------------------------
# General Descriptive and distribution analysis
glimpse(NHANES$variables)
skimr::skim_without_charts(NHANES$variables)
DataExplorer::plot_missing(NHANES$variables)
