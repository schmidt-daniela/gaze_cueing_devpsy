# Clear Workspace ---------------------------------------------------------
rm(list = ls())

# Adjust Parameters -------------------------------------------------------
mean_or_median <- "median" # Write "mean" or "median" to get a subset with only included trials based on mean- or median-2sd-criterion (saved in df_included).
dropna_or_not_included <- "not"  # Write "dropna" or "not". If "dropna", then all NAs in the main dependent measure o_SRT_reactive_sample will be dropped in df_included.
                                    # df_included is the dataframe that was filtered wrt exclusion criterion. It only contains valid trials.
                                    # This means, that you cannot analyse, e.g., predicte looks, because valide predicted looks might get dropped.
dropna_or_not_total <- "not"  # Write "dropna" or "not". If "dropna", then all NAs in the main dependent measure o_SRT_reactive_sample will be dropped in df_tot.
                              # df_tot is the dataframe that was NOT filtered wrt exclusion criterion. It also contains invalid trials.
                              # This means, that you cannot analyse, e.g., predicte looks, because valide predicted looks might get dropped.

# Packages ----------------------------------------------------------------
library(here)
library(tidyverse)

# Read Data ---------------------------------------------------------------
df_tot <- data.frame()
for(i in c("4","6","9", "18", "adult")){
  
  sample_size <- length(list.files(here("data", "first", "preproc", i), pattern = "\\.tsv$"))
  
  for(j in c(1:sample_size)){
    
    age_group <- i
    filenames <- list.files(path = here("data", "first", "preproc", age_group))
    n <- j
    filename <- filenames[n]
    print(filename)
    
    if(!is.na(filename)){ # remove this if, when 6m sample is full
      df_temp <- read.table(here("data", "first", "preproc", age_group, filename), header = T, sep = "\t")
      df_tot <- df_tot |> 
        bind_rows(df_temp)
      
      rm(df_temp)
      rm(filename)
    }
  }
}

# Read Data (Marlene Calculation) -----------------------------------------
df_tot_marlene <- data.frame()
for(i in c("4","6","9", "18", "adult")){
  
  sample_size <- length(list.files(here("data", "first", "preproc", i), pattern = "\\.tsv$"))

  for(j in c(1:sample_size)){
    
    age_group <- i
    filenames <- list.files(path = here("data", "first", "marlene_preproc", age_group))
    n <- j
    filename <- filenames[n]
    print(filename)
    
    df_temp_marlene <- read.table(here("data", "first", "marlene_preproc", age_group, filename), header = T, sep = "\t")
    df_tot_marlene <- df_tot_marlene |>
        bind_rows(df_temp_marlene)

    rm(df_temp_marlene)
    rm(filename)
  }
}

# Plot (Marlene Calculation) ----------------------------------------------
df_plot <- df_tot_marlene |> 
  mutate(AgeGroup_Clean = case_when(
    AgeGroup %in% c("A4", "A6") ~ "Adults",
    AgeGroup == "C4"  ~ "4-Month-Olds",
    AgeGroup == "C6"  ~ "6-Month-Olds",
    AgeGroup == "C9"  ~ "9-Month-Olds",
    AgeGroup == "C18" ~ "18-Month-Olds"
  )) |> 
  mutate(AgeGroup_Clean = factor(AgeGroup_Clean, 
                                 levels = c("4-Month-Olds", "6-Month-Olds", 
                                            "9-Month-Olds", "18-Month-Olds", "Adults")))

ggplot(df_plot, aes(x = AgeGroup_Clean, y = marlene_coefficient, color = AgeGroup_Clean)) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 2) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", 
               width = 0.2, color = "black", linewidth = 1) +
  stat_summary(fun = "mean", geom = "point", 
               shape = 18, size = 5, color = "black") +
  theme_classic() +
  labs(
    x = "Age Group",
    y = "Marlene Coefficient",
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.title = element_text(face = "bold")
  ) +
  scale_y_continuous(breaks = seq(-100, 200, by = 10))

# Read Data ---------------------------------------------------------------
df_tot <- data.frame()
for(i in c("4","6","9", "18", "adult")){
  
  sample_size <- length(list.files(here("data", "first", "preproc", i), pattern = "\\.tsv$"))
  
  for(j in c(1:sample_size)){
    
    age_group <- i
    filenames <- list.files(path = here("data", "first", "preproc", age_group))
    n <- j
    filename <- filenames[n]
    print(filename)
    
    if(!is.na(filename)){ # remove this if, when 6m sample is full
      df_temp <- read.table(here("data", "first", "preproc", age_group, filename), header = T, sep = "\t")
      df_tot <- df_tot |> 
        bind_rows(df_temp)
      
      rm(df_temp)
      rm(filename)
    }
  }
}

# Prepare Data ------------------------------------------------------------
df_tot <- df_tot |> 
  mutate(AgeGroup = str_replace(AgeGroup, "A4", "A"),
         AgeGroup = str_replace(AgeGroup, "A6", "A"),
         AgeGroup = str_replace(AgeGroup, "A9", "A")) |> 
  mutate(AgeGroup = recode(AgeGroup, "A" = "Adults", "C18" = "18M", "C4" = "4M", "C6" = "6M", "C9" = "9M")) |>
  mutate(AgeGroup = factor(AgeGroup, levels = c("4M", "6M", "9M", "18M", "Adults"))) |> 
  mutate(Condition = paste(Con_shared, Con_outcon, sep = "_"))

if(mean_or_median == "mean"){
  df_included <- df_tot |> 
    filter(Excluded_100ms == "no" & Excluded_200ms == "no" & Excluded_2SD_Mean == "no")
}

if(mean_or_median == "median"){
  df_included <- df_tot |> 
    filter(Excluded_100ms == "no" & Excluded_200ms == "no" & Excluded_2SD_Median == "no")
}

if(dropna_or_not_included == "dropna"){
  df_included <- df_included |> 
    drop_na(o_SRT_reactive_sample)
}

if(dropna_or_not_total == "dropna"){
  df_tot <- df_tot |> 
    drop_na(o_SRT_reactive_sample)
}

# Valid Trials (Main DV) --------------------------------------------------
df_included |> 
  drop_na(o_SRT_reactive_sample) |> 
  select(AgeGroup, ID, RunningTrial, Condition) |> 
  distinct() |> 
  group_by(AgeGroup, ID, Condition) |> 
  summarize(TrialNumber = n()) |> 
  group_by(AgeGroup, Condition) |> 
  summarize(ValidTrialMax = max(TrialNumber, na.rm = T),
            ValidTrialMin = min(TrialNumber, na.rm = T),
            ValidTrialSD = sd(TrialNumber, na.rm = T),
            ValidTrialMean = mean(TrialNumber, na.rm = T))

# Descriptives ------------------------------------------------------------
df_included |> 
  group_by(AgeGroup, Condition, ID) |> 
  summarize(o_SRT_reactive_sample = mean(o_SRT_reactive_sample, na.rm = T)) |> 
  group_by(AgeGroup, Condition) |> 
  summarize(o_SRT_reactive_sample_sd = sd(o_SRT_reactive_sample, na.rm = T),
            o_SRT_reactive_sample = mean(o_SRT_reactive_sample, na.rm = T)) |> View()

# How Many Show A Congruence Effect? --------------------------------------
df_included |> 
  group_by(AgeGroup, ID, Con_outcon) |> 
  summarize(o_SRT_reactive_sample = mean(o_SRT_reactive_sample, na.rm = T)) |> 
  group_by(AgeGroup, ID) |> 
  pivot_wider(names_from = Con_outcon, values_from = o_SRT_reactive_sample) |> 
  mutate(congruency_effect = ifelse(con < incon, "yes", "no")) |> 
  group_by(AgeGroup) |> 
  count(congruency_effect) |> 
  ungroup() # in developmental sample, ca. 50% exhibit a congruency effect and 50% not

# Plot Data ---------------------------------------------------------------
## Plot 0: Latency ~ Congruence + Age ----
# Prepare plot 0 data
direction <-  c("up","down") # "up", "down", or c("up", "down)
trial <- 1:32 |> as.character()
exclude_predicitive_look <- "no" # "yes" or "no"

df_included_2 <- df_included

if(exclude_predicitive_look == "yes"){
  df_included_2 <- df_included_2 |>
    filter(!is.na(o_FL_AOI_predictive_fixation))
}

df_plot_0 <- df_included_2 |>
  filter(CueingDir %in% direction) |>
  filter(RunningTrial %in% trial) |>
  group_by(AgeGroup, RunningTrial, ID, Con_outcon) |>
  summarize(o_SRT_reactive_sample = mean(o_SRT_reactive_sample, na.rm = T)) |>
  group_by(AgeGroup, ID, Con_outcon) |>
  summarize(o_SRT_reactive_sample = mean(o_SRT_reactive_sample, na.rm = T)) |>
  group_by(AgeGroup, Con_outcon) |>
  summarize(sd_o_SRT_reactive_sample = sd(o_SRT_reactive_sample, na.rm = T),
            o_SRT_reactive_sample = mean(o_SRT_reactive_sample, na.rm = T)) |>
  mutate(participants = ifelse(AgeGroup == "4M", 
                               df_tot |> filter(AgeGroup == "4M") |> pull(ID) |> unique() |> length(), 36)) |>
  mutate(se = sd_o_SRT_reactive_sample / sqrt(participants),
         ci_lower = o_SRT_reactive_sample - qt(0.975, df = participants - 1) * se,
         ci_upper = o_SRT_reactive_sample + qt(0.975, df = participants - 1) * se) |>
  ungroup()

# Create plot 0
ggplot(df_plot_0, aes(x = AgeGroup, y = o_SRT_reactive_sample, fill = Con_outcon)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, aes(color = Con_outcon)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.1, position = position_dodge(width = 0.8), color = "black") +
  labs(x = "Age Group", y = "Latency (ms)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")

## Plot 1: Latency ~ Shared + Age ----
# Prepare plot 1 data
direction <-  c("up","down") # "up", "down", or c("up", "down)
trial <- 1:32 |> as.character()

df_plot_1 <- df_included |>
  filter(CueingDir %in% direction) |>
  filter(RunningTrial %in% trial) |>
  group_by(AgeGroup, RunningTrial, ID, Con_shared) |>
  summarize(o_SRT_reactive_sample = mean(o_SRT_reactive_sample, na.rm = T)) |>
  group_by(AgeGroup, ID, Con_shared) |>
  summarize(o_SRT_reactive_sample = mean(o_SRT_reactive_sample, na.rm = T)) |>
  group_by(AgeGroup, Con_shared) |>
  summarize(sd_o_SRT_reactive_sample = sd(o_SRT_reactive_sample, na.rm = T),
            o_SRT_reactive_sample = mean(o_SRT_reactive_sample, na.rm = T)) |>
  mutate(participants = ifelse(AgeGroup == "4M", 
                               df_tot |> filter(AgeGroup == "4M") |> pull(ID) |> unique() |> length(), 36)) |>
  mutate(se = sd_o_SRT_reactive_sample / sqrt(participants),
         ci_lower = o_SRT_reactive_sample - qt(0.975, df = participants - 1) * se,
         ci_upper = o_SRT_reactive_sample + qt(0.975, df = participants - 1) * se) |>
  ungroup()

# Create plot 1
ggplot(df_plot_1, aes(x = AgeGroup, y = o_SRT_reactive_sample, fill = Con_shared)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, aes(color = Con_shared)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.1, position = position_dodge(width = 0.8), color = "black") +
  labs(x = "Age Group", y = "Latency (ms)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")

## Plot 2: Latency ~ Cueing Direction + Age ----
# Prepare plot 2 data
direction <-  c("up","down") # "up", "down", or c("up", "down)
trial <- 1:32 |> as.character()

df_plot_2 <- df_included |>
  filter(CueingDir %in% direction) |>
  filter(RunningTrial %in% trial) |>
  group_by(AgeGroup, RunningTrial, ID, CueingDir) |>
  summarize(o_SRT_reactive_sample = mean(o_SRT_reactive_sample, na.rm = T)) |>
  group_by(AgeGroup, ID, CueingDir) |>
  summarize(o_SRT_reactive_sample = mean(o_SRT_reactive_sample, na.rm = T)) |>
  group_by(AgeGroup, CueingDir) |>
  summarize(sd_o_SRT_reactive_sample = sd(o_SRT_reactive_sample, na.rm = T),
            o_SRT_reactive_sample = mean(o_SRT_reactive_sample, na.rm = T)) |>
  mutate(participants = ifelse(AgeGroup == "4M", 
                               df_tot |> filter(AgeGroup == "4M") |> pull(ID) |> unique() |> length(), 36)) |>
  mutate(se = sd_o_SRT_reactive_sample / sqrt(participants),
         ci_lower = o_SRT_reactive_sample - qt(0.975, df = participants - 1) * se,
         ci_upper = o_SRT_reactive_sample + qt(0.975, df = participants - 1) * se) |>
  ungroup()

# Create plot 2
ggplot(df_plot_2, aes(x = AgeGroup, y = o_SRT_reactive_sample, fill = CueingDir)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, aes(color = CueingDir)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.1, position = position_dodge(width = 0.8), color = "black") +
  labs(x = "Age Group", y = "Latency (ms)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")

## Plot 3: Latency ~ Congruence + Social + Age ----
# Prepare plot 3 data
direction <- c("up", "down") # "up", "down", or c("up", "down)
trial <- 1:32 |> as.character()
exclude_predicitive_look <- "no" # "yes" or "no"

df_included_2 <- df_included

if(exclude_predicitive_look == "yes"){
  df_included_2 <- df_included_2 |>
    filter(!is.na(o_FL_AOI_predictive_fixation))
}

df_plot_3 <- df_included_2 |>
  filter(CueingDir %in% direction) |>
  filter(RunningTrial %in% trial) |>
  group_by(AgeGroup, RunningTrial, ID, Condition, ConNo) |>
  summarize(o_SRT_reactive_sample = mean(o_SRT_reactive_sample, na.rm = T)) |>
  group_by(AgeGroup, ID, Condition, ConNo) |>
  summarize(o_SRT_reactive_sample = mean(o_SRT_reactive_sample, na.rm = T)) |>
  group_by(AgeGroup, Condition, ConNo) |>
  summarize(sd_o_SRT_reactive_sample = sd(o_SRT_reactive_sample, na.rm = T),
            o_SRT_reactive_sample = mean(o_SRT_reactive_sample, na.rm = T)) |>
  mutate(participants = ifelse(AgeGroup == "4M",
                               df_tot |> filter(AgeGroup == "4M") |> pull(ID) |> unique() |> length(), 36)) |>
  mutate(se = sd_o_SRT_reactive_sample / sqrt(participants),
         ci_lower = o_SRT_reactive_sample - qt(0.975, df = participants - 1) * se,
         ci_upper = o_SRT_reactive_sample + qt(0.975, df = participants - 1) * se) |>
  ungroup()

# Create plot 3
png(here("img", "cajo_first_mainrq_median_cuedirbot_res420.png"), width = 2048, height = 1152, units = "px", res = 420)
ggplot(df_plot_3, aes(x = AgeGroup, y = o_SRT_reactive_sample, fill = Condition)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, aes(color = Condition)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.1, position = position_dodge(width = 0.8), color = "black") +
  labs(x = "Age Group", y = "Latency (ms)") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  ylim(100,1050)
dev.off()

## Plot 4: Latency across time ----
# Prepare plot 4 data
df_plot_4 <- df_included |>
  group_by(AgeGroup, RunningTrial, ID) |> 
  summarize(o_SRT_reactive_sample = mean(o_SRT_reactive_sample, na.rm = T)) |> 
  group_by(AgeGroup, RunningTrial) |>
  summarize(sd_o_SRT_reactive_sample = sd(o_SRT_reactive_sample, na.rm = T),
            o_SRT_reactive_sample = mean(o_SRT_reactive_sample, na.rm = T)) |> 
  mutate(participants = ifelse(AgeGroup == "4M", 
                               df_tot |> filter(AgeGroup == "4M") |> pull(ID) |> unique() |> length(), 36)) |>
  mutate(se = sd_o_SRT_reactive_sample / sqrt(participants),
         ci_lower = o_SRT_reactive_sample - qt(0.975, df = participants - 1) * se,
         ci_upper = o_SRT_reactive_sample + qt(0.975, df = participants - 1) * se) |> 
  ungroup()

# Create plot 4
ggplot(df_plot_4, aes(x = RunningTrial, y = o_SRT_reactive_sample, fill = AgeGroup)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, aes(color = AgeGroup)) +
  geom_line(position = position_dodge(width = 0.8), size = 1, aes(color = AgeGroup)) +
  labs(x = "Trial", y = "Latency (ms)") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") # no clear pattern (participants don't get generally faster or slower over the course of the experiment)

## Plot 5: Number of predictive fixations/ gaze samples ----
direction <- c("up","down") # c("up","down") # "up", "down", or c("up", "down) @ Maleen: Up versus down matters a lot. When they look from actor A to B, they stop in the middle and it hits the upper object AOI.
x <- "o_FL_AOI_predictive_fixation" # "o_FL_AOI_predictive_fixation" or "o_FL_AOI_predictive_sample"

df_plot_5 <- df_included |>
  filter(CueingDir %in% direction) |> 
  separate(!!x, c("x_cue", "x_obj"), remove = F) |> 
  drop_na(x_cue) |> 
  group_by(AgeGroup, ID) |> 
  count(x_cue)  |> 
  group_by(AgeGroup) |> 
  count(x_cue)  |> 
  ungroup()

ggplot(df_plot_5, aes(x = AgeGroup, y = n, fill = x_cue)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(x = "Age Group",
       y = "Frequency (n)",
       fill = "Fixation Category") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

## Plot 6: Number of reactive fixations/ gaze samples ----
direction <- c("up","down") # c("up","down") # "up", "down", or c("up", "down) @ Maleen: Up versus down matters a lot. When they look from actor A to B, they stop in the middle and it hits the upper object AOI.
x <- "o_FL_AOI_reactive_fixation" # "o_FL_AOI_predictive_fixation" or "o_FL_AOI_predictive_sample"

df_plot_6 <- df_included |>
  filter(CueingDir %in% direction) |> 
  drop_na(x) |> 
  group_by(AgeGroup, ID) |> 
  count(!!sym(x))  |> 
  group_by(AgeGroup, o_FL_AOI_reactive_fixation) |> 
  summarize(n = sum(n)) |> 
  #count(!!sym(x))  |> 
  ungroup()

ggplot(df_plot_6, aes(x = AgeGroup, y = n, fill = o_FL_AOI_reactive_fixation)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(x = "Age Group",
       y = "Frequency (n)",
       fill = "Fixation Category") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")


t_df <- df_included |> 
  filter(AgeGroup == "4M") |> 
  #filter(CueingDir == "up") |> 
  select(ID, RunningTrial, Condition, o_SRT_reactive_sample) |> 
  group_by(ID, Condition) |> 
  summarize(o_SRT_reactive_sample = mean(o_SRT_reactive_sample, na.rm = T)) 

t_df_wide <- t_df %>%
  pivot_wider(names_from = Condition, values_from = o_SRT_reactive_sample)

t.test(t_df_wide$joint_con, t_df_wide$joint_incon, paired = TRUE) # p = 0.06852 (mean) 0.31 (median) 0.12 (median, 23.05) 0.04 (mean, 23.05), 0.07 (mean) (17.06), 0.19 (median) (17.06)
#t.test(t_df_wide$parallel_con, t_df_wide$parallel_incon, paired = TRUE) # p = 0.988 (mean) 0.90 (median)
