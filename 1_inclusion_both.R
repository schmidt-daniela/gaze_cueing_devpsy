# Clear Workspace ---------------------------------------------------------
rm(list = ls())

# Adjust Parameter --------------------------------------------------------
party <- "third" # adjust: "first" or "third"
sheet_name <- "4 mo" # adjust: "4 mo", "6 mo", "9 mo", "18 mo", or "adults"
ifelse(sheet_name %in% c("4 mo", "6 mo", "adults"), # don't adjust anything here
       protocol_name <- paste0("studyprotocol_cajo46_", party, ".xlsx"),
       protocol_name <- paste0("studyprotocol_cajo918_", party, ".xlsx"))
age_group <- 4 # adjust: 4, 6, 9, 18 or "adult"

# Packages ----------------------------------------------------------------
library(here)
library(openxlsx)
library(readxl)
library(tidyverse)
library(writexl)

# Functions ---------------------------------------------------------------
sample_count <- function(data, variable){
  summarized_data <- data |> 
    select({{variable}}) |> 
    group_by({{variable}}) |> 
    count() |> 
    ungroup()
  
  return(summarized_data)
}

sample_mean <- function(data, variable){
  variable <- sample_overview |> 
    mutate({{variable}} := as.numeric({{variable}})) |> 
    summarize({{variable}} := mean({{variable}}, na.rm = T))
  
  return(head(variable))
}

# Read Data ---------------------------------------------------------------

# Read and process protocol
protocol <- read_excel(here("docs", protocol_name), sheet = sheet_name)

protocol_sub <- protocol[, c("cajo_inclusion_counter", "running_no", "tobii_rec", "timeline")] |> 
  drop_na(c("tobii_rec","cajo_inclusion_counter")) |>
  slice(-1)

included_infants <- protocol_sub |> 
  filter(cajo_inclusion_counter == 1) |> 
  select(-cajo_inclusion_counter) |>
  mutate(tobii_rec = tobii_rec |> str_remove("ording")) |> 
  unite("no_rec_exp", c(running_no, tobii_rec, timeline), remove = F) |> 
  arrange("no_rec_exp")

# Read included files -----------------------------------------------------
# Read and edit all filenames
filenames <- list.files(path = here("data", party, "raw_all", age_group))
filenames_cleaned <- filenames |> str_remove_all("CAJO_|\\.tsv|_M_|_F_|C4|C6|A4|A6|A9|C9|C18|Exp")
filenames_cleaned <- filenames_cleaned |> str_remove_all("CAJOfollowup_")
filenames_tot <- filenames |> as.data.frame() |> mutate(cleaned = filenames_cleaned) |> arrange(cleaned)
filesnames_to_read <- filenames_tot |> filter(cleaned %in% included_infants$no_rec_exp) |> pull(filenames)

# Check
if (!(filenames_tot |>
      filter(cleaned %in% included_infants$no_rec_exp) |>
      pull(cleaned) |>
      length() == included_infants |> nrow())) {
  
  stop("The number of included files does not match the number of included infants.\nCheck for typos in protocol and filenames.")
}
# if the script stops, please excute the following two codes:
# filenames_tot |> filter(cleaned %in% included_infants$no_rec_exp) |> pull(cleaned) |> sort()
# included_infants |> pull(no_rec_exp) |> sort()
# manually check the discrepancy between the two vectors to find the (probably typo) error

# Save included files -----------------------------------------------------
for (filename in filesnames_to_read) {
  filepath <- file.path(here("data", party, "raw_all", age_group), filename)
  vp_data <- read.table(filepath, header = TRUE, sep = "\t")
  write.table(vp_data, here("data", party, "raw_included", age_group, filename), row.names = F, quote = F, sep = "\t", dec = ".")
  print(paste(filename, "done.", sep = " "))
}

# Exclusion Reasons -------------------------------------------------------
exclusion_overview <- protocol |> 
  filter(cajo_inclusion_counter == 0) |>
  select(if_excluded_reason, id, running_no) |> 
  drop_na(id) |> 
  rename(reason = if_excluded_reason)

write.table(exclusion_overview, here("data", "both", "exclusion_reasons", paste0("exclusionreasons_", age_group, "_", party, ".txt")), row.names = F, quote = F, sep = ";", dec = ".")
write_xlsx(exclusion_overview, here("data", "both", "exclusion_reasons", paste0("exclusionreasons_", age_group, "_", party, ".xlsx")))

# Check Distribution of Experiments ---------------------------------------
distribution_experiments <- included_infants |> group_by(timeline) |> count() |> rename(exp = timeline) # check how often which experiment (at best uniformly distributed)
write.table(distribution_experiments, here("data", "both", "distribution_experiments", paste0("experimentdistribution_", age_group, "_", party, ".txt")), row.names = F, quote = F, sep = ";", dec = ".")

# Check Sample Descriptives -----------------------------------------------
## Infants & Toddlers ----
if(age_group != "adult"){
  sample_overview <- protocol |> 
    filter(cajo_inclusion_counter == 1) |> 
    select(id, tobii_rec, sex, age_md, age_ddd, ssw, number_person_household, number_siblings, bilingual,
           language, kindergarten_yn, kindergarten_dpw, tagesmutter_yn, tagesmutter_dpw, 
           participated_in_other_study, where)
  
  age <- sample_overview |> 
    mutate(age_ddd = as.numeric(age_ddd)) |> 
    summarize(mean_age_d = mean(age_ddd), sd_age_d = sd(age_ddd), 
              min_age_d = min(age_ddd), max_age_d = max(age_ddd))
  sex <- sample_count(sample_overview, sex)
  household <- sample_count(sample_overview, number_person_household)
  siblings <- sample_count(sample_overview, number_siblings)
  siblings <- sample_count(sample_overview, number_siblings)
  kindergarten_yn <- sample_count(sample_overview, kindergarten_yn)
  kindergarten_dpw <- sample_mean(sample_overview, kindergarten_dpw)
  tagesmutter_yn <- sample_count(sample_overview, tagesmutter_yn)
  tagesmutter_dpw <- sample_mean(sample_overview, tagesmutter_dpw)
  participated_in_other_study <- sample_count(sample_overview, participated_in_other_study)
  bilingual <- sample_count(sample_overview, bilingual)
  
  wb <- createWorkbook()
  
  addWorksheet(wb, "sample_overview")
  writeData(wb, "sample_overview", sample_overview)
  
  addWorksheet(wb, "sex")
  writeData(wb, "sex", sex)
  
  addWorksheet(wb, "age")
  writeData(wb, "age", age)
  
  addWorksheet(wb, "household")
  writeData(wb, "household", household)
  
  addWorksheet(wb, "siblings")
  writeData(wb, "siblings", siblings)
  
  addWorksheet(wb, "bilingual")
  writeData(wb, "bilingual", bilingual)
  
  addWorksheet(wb, "kindergarten_yn")
  writeData(wb, "kindergarten_yn", kindergarten_yn)
  
  addWorksheet(wb, "kindergarten_dpw")
  writeData(wb, "kindergarten_dpw", kindergarten_dpw)
  
  addWorksheet(wb, "tagesmutter_yn")
  writeData(wb, "tagesmutter_yn", tagesmutter_yn)
  
  addWorksheet(wb, "tagesmutter_dpw")
  writeData(wb, "tagesmutter_dpw", tagesmutter_dpw)
  
  addWorksheet(wb, "participated_in_other_study")
  writeData(wb, "participated_in_other_study", participated_in_other_study)
  
  saveWorkbook(wb, here("data", "both", "final_sample_information", paste0("final_sample_information_", age_group, "_", party, ".xlsx")), overwrite = TRUE)
}

## Adults ----
if(age_group == "adult"){
  
  sample_overview <- protocol |> 
    filter(cajo_inclusion_counter == 1) |> 
    select(id, tobii_rec, sex, testing_date, birthday, age_y, klob, position_child)

  age_y <- sample_overview |> 
    mutate(age_y = as.numeric(age_y)) |> 
    summarize(mean_age = mean(age_y), sd_age = sd(age_y), 
              min_age = min(age_y), max_age = max(age_y))
  sex <- sample_count(sample_overview, sex)
  klob <- sample_count(sample_overview, klob)
  
  wb <- createWorkbook()
  
  addWorksheet(wb, "sample_overview")
  writeData(wb, "sample_overview", sample_overview)
  
  addWorksheet(wb, "sex")
  writeData(wb, "sex", sex)
  
  addWorksheet(wb, "age_y")
  writeData(wb, "age_y", age_y)
  
  addWorksheet(wb, "klob")
  writeData(wb, "klob", klob)
  
  saveWorkbook(wb, here("data", "both", "final_sample_information", paste0("final_sample_information_", age_group, "_", party, ".xlsx")), overwrite = TRUE)
}

