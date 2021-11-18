# -----------------------------------------------------------------------------------
# Hands-on Exercise: Advanced 
# (Best for those with 3+ years of R and ADaM experience)
#  
# See "ExerciseSheet_advanced.docx" for all exercise questions, and if you get stuck ask for help from one of our facilitators or consult "SolutionSheet_advanced.docx"
# 
# This program is to be used as a starter template as you work through the exercises
# -----------------------------------------------------------------------------------

# Load all required packages

library(admiral)
library(dplyr)
library(lubridate)
library(stringr)
library(haven)

# Read in the input data from Github using haven

advs_temp <- read_xpt(url("https://github.com/Roche-GSK/admiral.phuse.workshop/raw/main/Hands_on_Exercises/advs_temp.xpt"))

# Use admiral functions to create the required baseline variables
advs <- advs_temp %>%
  
  # Calculate ABLFL
  derive_extreme_flag(  # Add a variable flagging the first or last observation within each by group
    by_vars = vars(STUDYID, USUBJID, PARAMCD),
    order = vars(ADT),
    new_var = ABLFL,
    mode = "last",
    filter = (!is.na(AVAL) & ADT <= TRTSDT)
  ) %>%
  
  # Calculate BASE
  derive_var_base(
    by_vars = vars(STUDYID, USUBJID, PARAMCD)
  ) %>%
  
  # Calculate CHG
  derive_var_chg() %>%
  
  # Sort the data frame
  arrange(USUBJID, PARAMCD, ADT)




# different derivation ####################################################

advs2 <- advs_temp %>%
  
  # Calculate ABLFL
  derive_extreme_flag(  # Add a variable flagging the first or last observation within each by group
    by_vars = vars(STUDYID, USUBJID, PARAMCD),
    order = vars(#ADT,
                 VSSEQ), # add VSSEQ
    new_var = ABLFL,
    mode = "last",
    filter = (!is.na(AVAL) & ADT <= TRTSDT)
  ) %>%
  
  # Calculate BASE
  derive_var_base(
    by_vars = vars(STUDYID, USUBJID, PARAMCD)
  ) %>%
  
  # Calculate CHG
  derive_var_chg() %>%
  
  # Sort the data frame
  arrange(USUBJID, PARAMCD, ADT)


library(diffdf) 

# Compare 2 data frames advs & advs2 with the key variables USUBJID, PARAMCD, ADT #############################
diffdf(advs, advs2, keys = c("USUBJID", "PARAMCD", "ADT"))




# Derive new parameter records
advs_temp2 <- advs_temp %>%
  derive_summary_records(
    by_vars = vars(STUDYID, USUBJID, TRTSDT, VISITNUM, VISIT, VSDTC, ADT),
    filter = PARAMCD=="TEMP",
    analysis_var = AVAL,
    summary_fun = function(x) ((x*9/5) +32),
    set_values_to = vars(PARAMCD="TEMPF", PARAM="Temperature (F)", PARAMN=7)
  )


advs3 <- advs_temp2 %>%
  
  # Calculate ABLFL
  derive_extreme_flag(  # Add a variable flagging the first or last observation within each by group
    by_vars = vars(STUDYID, USUBJID, PARAMCD),
    order = vars(ADT),
    new_var = ABLFL,
    mode = "last",
    filter = (!is.na(AVAL) & ADT <= TRTSDT)
  ) %>%
  
  # Calculate BASE
  derive_var_base(
    by_vars = vars(STUDYID, USUBJID, PARAMCD)
  ) %>%
  
  # Calculate CHG
  derive_var_chg() %>%
  
  # Sort the data frame
  arrange(USUBJID, PARAMCD, ADT)



diffdf(base = advs, compare = advs3
       , keys = c("USUBJID", "PARAMCD", "ADT"), 
       )

