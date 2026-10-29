# INCLUSION OF TRIALS
# A trial will be considered to be valid if the infant fixates the face for at least 200 cumulative milliseconds during both central parts of the 
# gaze-cueing phase prior to making a gaze shift to the target. As central parts, we consider (a) the (no) eye contact sequence and (b) the gaze-cueing 
# sequence excluding the motion sequence. Furthermore, individual reaction times of less than 100 ms or deviating more than two standard deviations of each 
# individual mean will be excluded from analysis.

# Clear Workspace ---------------------------------------------------------
rm(list = ls())

# Define AOIs -------------------------------------------------------------
# Remark: AOIs emtail stimulus size + 2°/80px per side

## Objects ----
# Contains + or - 40 pixel AOI buffer
aoi_topobject_x_topleft <- 830
aoi_topobject_y_topleft <- 40
aoi_topobject_x_botright <- 1090
aoi_topobject_y_botright <- 300

aoi_botobject_x_topleft <- 830
aoi_botobject_y_topleft <- 780
aoi_botobject_x_botright <- 1090
aoi_botobject_y_botright <- 1040

## Actors ----
# Contains + or - 40 pixel AOI buffer
aoi_leftface_x_left <- 100 # 140-40
aoi_leftface_x_right <- 620 # 580+40
aoi_leftface_y_top <- 305 # 345-40
aoi_leftface_y_bottom <- 840 # 800+40

aoi_rightface_x_left <- 1300 # 1340-40
aoi_rightface_x_right <- 1820 # 1780+40
aoi_rightface_y_top <- 305 # 345-40
aoi_rightface_y_bottom <- 840 # 800+40

## Screen ----
# Contains + or - 40 pixel AOI buffer
aoi_screen_x_left <- -40 # 0-40
aoi_screen_x_right <- 1960 # 1920+40
aoi_screen_y_top <- -40 # 0-40
aoi_screen_y_bottom <- 1120 # 1080+40

# Packages ----------------------------------------------------------------
library(here)
library(tidyverse)

# Adjust Parameter --------------------------------------------------------
age_group <- 4 # adjust: 4, 6, 9, 18 or "adult"
exclusion_criterion_200ms <- "fixation_all" # "fixation_all" or "samples_all" or "fixation_all_except_4mfollowup"

sample_size <- length(list.files(here("data", "first", "raw_included", age_group), pattern = "\\.tsv$"))
for(i in c(1:sample_size)){
  filenames <- list.files(path = here("data", "first", "raw_included", age_group))
  n <- i
  filename <- filenames[n]
  
# Read Data ---------------------------------------------------------------
raw <- read.table(here("data", "first", "raw_included", age_group, filename), header = T, sep = "\t")
df <- raw

# Data Preparation --------------------------------------------------------
## Separate information about trials, etc. ----
# In column "stimulus" "move" means moving/looming/shrinking and extending object and "still" means the object after it was looked at
# for 150ms (gaze-contingent criterion). "at" is attention getter. "cueing" is the complete actor-video.
df <- df |> 
  separate(Presented.Stimulus.name, into = c("trial", "task", "stimulus", "cond_both", "cond_jointness", "cue_direction",
                                             "cond_congruence", "actor", "actor_location", "object_identity", "object_location", "cond_party"), remove = F) |> 
  rename(fix_x = Fixation.point.X, fix_y = Fixation.point.Y, x = Gaze.point.X, y = Gaze.point.Y)

## Add gaze-sample duration ----
df$gaze_sample_duration <- c(diff(df$Recording.timestamp), NA) / 1000

## Add cumulative duration per stimulus ----
df <- df |> 
  group_by(trial, stimulus) |> 
  mutate(timeline = cumsum(gaze_sample_duration)) |> 
  ungroup()

# Add time markers for cueing phase
cueing_eyecontact <- which(df$stimulus == "cueing" & df$timeline >= 0 & df$timeline <= 2000) 
df[cueing_eyecontact, "stimulus"] <- "cueing_eyecontact"

cueing_motion <- which(df$stimulus == "cueing" & df$timeline > 2000 & df$timeline <= 3000) 
df[cueing_motion, "stimulus"] <- "cueing_motion"

cueing_objectgaze <- which(df$stimulus == "cueing" & df$timeline > 3000 & df$timeline <= 4000) 
df[cueing_objectgaze, "stimulus"] <- "cueing_objectgaze"

## Define AOI ----
df$aoi_screen_fix <- "off" # fix = based on fixations (refers to whole screen AOI)
df$aoi_fix <- "not_in_aoi" # fix = based on fixations (refers to object and actor AOI)
df$aoi_gs <- "not_in_aoi" # gs = based on gaze sample (refers to object and actor AOI)

# Screen (fixation based)
screen <- which(df$fix_x >= (aoi_screen_x_left) & df$fix_x <= (aoi_screen_x_right) & 
                      df$fix_y >= (aoi_screen_y_top) & df$fix_y <= (aoi_screen_y_bottom))
df[screen,"aoi_screen_fix"] <- "on"

# Objects (fixation based)
top_object <- which(df$fix_x >= (aoi_topobject_x_topleft) & df$fix_x <= (aoi_topobject_x_botright) & 
                      df$fix_y >= (aoi_topobject_y_topleft) & df$fix_y <= (aoi_topobject_y_botright))
df[top_object,"aoi_fix"] <- "top"

bot_object <- which(df$fix_x >= (aoi_botobject_x_topleft) & df$fix_x <= (aoi_botobject_x_botright) & 
                      df$fix_y >= (aoi_botobject_y_topleft) & df$fix_y <= (aoi_botobject_y_botright))
df[bot_object,"aoi_fix"] <- "bottom"

# Actors (fixation based)
left_actor <- which(df$fix_x >= (aoi_leftface_x_left) & df$fix_x <= (aoi_leftface_x_right) & 
                      df$fix_y >= (aoi_leftface_y_top) & df$fix_y <= (aoi_leftface_y_bottom))
df[left_actor,"aoi_fix"] <- "left_actor"

right_actor <- which(df$fix_x >= (aoi_rightface_x_left) & df$fix_x <= (aoi_rightface_x_right) &
                       df$fix_y >= (aoi_rightface_y_top) & df$fix_y <= (aoi_rightface_y_bottom))
df[right_actor,"aoi_fix"] <- "right_actor"

# Objects (gaze-sample based)
top_object <- which(df$x >= (aoi_topobject_x_topleft) & df$x <= (aoi_topobject_x_botright) & 
                      df$y >= (aoi_topobject_y_topleft) & df$y <= (aoi_topobject_y_botright))
df[top_object,"aoi_gs"] <- "top"

bot_object <- which(df$x >= (aoi_botobject_x_topleft) & df$x <= (aoi_botobject_x_botright) & 
                      df$y >= (aoi_botobject_y_topleft) & df$y <= (aoi_botobject_y_botright))
df[bot_object,"aoi_gs"] <- "bottom"

# Actors (gaze-sample based)
left_actor <- which(df$x >= (aoi_leftface_x_left) & df$x <= (aoi_leftface_x_right) & 
                      df$y >= (aoi_leftface_y_top) & df$y <= (aoi_leftface_y_bottom))
df[left_actor,"aoi_gs"] <- "left_actor"

right_actor <- which(df$x >= (aoi_rightface_x_left) & df$x <= (aoi_rightface_x_right) &
                       df$y >= (aoi_rightface_y_top) & df$y <= (aoi_rightface_y_bottom))
df[right_actor,"aoi_gs"] <- "right_actor"

# Only matters for first party, excludes cases in which infants looked at "void actor area"
# (i.e. area where no actor is presented).
df[which(df$actor_location == "left" & df$aoi_fix == "right_actor"), "aoi_fix"] <- NA
df[which(df$actor_location == "left" & df$aoi_gs == "right_actor"), "aoi_gs"] <- NA
df[which(df$actor_location == "right" & df$aoi_fix == "left_actor"), "aoi_fix"] <- NA
df[which(df$actor_location == "right" & df$aoi_gs == "left_actor"), "aoi_gs"] <- NA

# Latencies -----------------------------------------------------
## o_SRT_reactive_sample and o_SRT_reactive_fixation ----
# Outcome phase: Saccadic Reaction Time MAIN DEPENDENT MEASURE! Time interval between the appearance of the target object (video start) and the arrival of the infant’s gaze (not fixation) in the respective target AOI (i.e., the outcome position)
# Outcome phase: Saccadic Reaction Time SECONDARY DEPENDENT MEASURE (keine prio, nur zum exploren) Time interval between the appearance of the target object (video start) and the arrival of the infant’s gaze of first fixation in the respective target AOI (i.e., the outcome position)
# sample-based
latencies_top_gs <- df |> 
  filter(stimulus %in% "move") |> 
  filter(object_location == "top" & aoi_gs == "top") |>
  select(x, y, Presented.Stimulus.name, trial, timeline, aoi_gs) |> 
  group_by(trial) |> 
  slice(1) |> 
  ungroup() |> 
  rename(o_SRT_reactive_sample = timeline)

latencies_bottom_gs <- df |> 
  filter(stimulus %in% "move") |> 
  filter(object_location == "bottom" & aoi_gs == "bottom") |>
  select(x, y, Presented.Stimulus.name, trial, timeline, aoi_gs) |> 
  group_by(trial) |> 
  slice(1) |> 
  ungroup() |> 
  rename(o_SRT_reactive_sample = timeline)

# fixation-based 
latencies_top_fix <- df |> 
  filter(stimulus %in% "move") |> 
  filter(object_location == "top" & aoi_fix == "top") |>
  select(x, y, Presented.Stimulus.name, trial, timeline, aoi_fix) |> 
  group_by(trial) |> 
  slice(1) |> 
  ungroup() |> 
  rename(o_SRT_reactive_fixation = timeline)

latencies_bottom_fix <- df |> 
  filter(stimulus %in% "move") |> 
  filter(object_location == "bottom" & aoi_fix == "bottom") |>
  select(x, y, Presented.Stimulus.name, trial, timeline, aoi_fix) |> 
  group_by(trial) |> 
  slice(1) |> 
  ungroup() |> 
  rename(o_SRT_reactive_fixation = timeline)

# Add Information About Excluded Trials -----------------------------------
Excluded_100ms <- latencies_top_gs |> 
  bind_rows(latencies_bottom_gs) |> 
  filter(o_SRT_reactive_sample < 100) |>
  pull(trial)

if(age_group == "adult"){
  Excluded_100ms <- NULL
}

# Only include trials with at least 200ms cumulative fixation time on faces during (no) eye contact sequence AND
# at least 200ms cumulative fixation time on faces during the gaze-cueing sequence

# This code block identifies trials in which either during eyecontact or during objectgaze or during both,
# the infants exhibited less than 200ms fixation time on the actor. 
if (exclusion_criterion_200ms == "fixation_all_except_4mfollowup" &&
    any(age_group != 4)) {
  stop("`samples_4mfollowup` is only applicable when age_group = 4.")
}

Excluded_200ms_1 <- df |>
  mutate(
    aoi = case_when(
      exclusion_criterion_200ms == "fixation_all" ~ aoi_fix,
      exclusion_criterion_200ms == "samples_all" ~ aoi_gs,
      exclusion_criterion_200ms == "fixation_all_except_4mfollowup" & as.numeric(stringr::str_extract(filename, "(?<=_)[0-9]+(?=_[MF]_)")) < 81 ~ aoi_fix,
      exclusion_criterion_200ms == "fixation_all_except_4mfollowup" & as.numeric(stringr::str_extract(filename, "(?<=_)[0-9]+(?=_[MF]_)")) >= 81 ~ aoi_gs,
      TRUE ~ aoi_fix
    )
  ) |>
  filter(aoi %in% c("left_actor", "right_actor")) |>
  filter(stimulus %in% c("cueing_eyecontact", "cueing_objectgaze")) |>
  group_by(cond_both, cond_jointness, cond_congruence, trial, stimulus) |>
  summarize(sum_time = sum(gaze_sample_duration, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = stimulus, values_from = sum_time) |>
  filter(cueing_eyecontact < 200 | cueing_objectgaze < 200 | is.na(cueing_eyecontact) | is.na(cueing_objectgaze)) |>
  pull(trial)

# This code block identifies trials in which no fixation time during eyecontact AND no fixation time
# during objectgaze was shown. (There may be a smarter way of coding this.)
Excluded_200ms_2 <- df |>
  mutate(
    aoi = case_when(
      exclusion_criterion_200ms == "fixation_all" ~ aoi_fix,
      exclusion_criterion_200ms == "samples_all" ~ aoi_gs,
      exclusion_criterion_200ms == "fixation_all_except_4mfollowup" & as.numeric(stringr::str_extract(filename, "(?<=_)[0-9]+(?=_F_)")) < 81 ~ aoi_fix,
      exclusion_criterion_200ms == "fixation_all_except_4mfollowup" & as.numeric(stringr::str_extract(filename, "(?<=_)[0-9]+(?=_F_)")) >= 81 ~ aoi_gs,
      TRUE ~ aoi_fix
    )
  ) |>
  mutate(aoi = str_remove(aoi, "^(left_|right_)")) |>
  filter(stimulus %in% c("cueing_eyecontact", "cueing_objectgaze")) |>
  group_by(cond_both, cond_jointness, cond_congruence, trial, stimulus, aoi) |>
  dplyr::summarize(
    sum_time = sum(gaze_sample_duration, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(aoi %in% c("actor", "not_in_aoi")) |>
  pivot_wider(names_from = stimulus, values_from = sum_time) |>
  select(aoi, trial) |>
  distinct() |>
  group_by(trial) |>
  filter(n() == 1 & aoi == "not_in_aoi") |>
  ungroup() |>
  pull(trial)

Excluded_200ms <- c(Excluded_200ms_1, Excluded_200ms_2)

# Exclude latencies deviating more than two standard deviations of each individual mean
mean_latency <- c(latencies_top_gs$o_SRT_reactive_sample, latencies_bottom_gs$o_SRT_reactive_sample) |> mean(na.rm = T)
median_latency <- c(latencies_top_gs$o_SRT_reactive_sample, latencies_bottom_gs$o_SRT_reactive_sample) |> median(na.rm = T)
twosd_latency <- c(latencies_top_gs$o_SRT_reactive_sample, latencies_bottom_gs$o_SRT_reactive_sample) |> sd(na.rm = T) * 2

Excluded_2SD_Mean <- latencies_top_gs |> 
  bind_rows(latencies_bottom_gs) |>
  filter(o_SRT_reactive_sample > mean_latency + twosd_latency | o_SRT_reactive_sample < mean_latency - twosd_latency) |> 
  pull(trial)

Excluded_2SD_Median <- latencies_top_gs |> 
  bind_rows(latencies_bottom_gs) |>
  filter(o_SRT_reactive_sample > median_latency + twosd_latency | o_SRT_reactive_sample < median_latency - twosd_latency) |> 
  pull(trial)

# Add exclusion columns
if(Excluded_100ms |> length() > 0){df$Excluded_100ms <- ifelse(df$trial %in% Excluded_100ms, "yes", "no")}
if(Excluded_100ms |> length() == 0){df$Excluded_100ms <- "no"}
if(Excluded_200ms |> length() > 0){df$Excluded_200ms <- ifelse(df$trial %in% Excluded_200ms, "yes", "no")}
if(Excluded_200ms |> length() == 0){df$Excluded_200ms <- "no"}
if(Excluded_2SD_Mean |> length() > 0){df$Excluded_2SD_Mean <- ifelse(df$trial %in% Excluded_2SD_Mean, "yes", "no")}
if(Excluded_2SD_Mean |> length() == 0){df$Excluded_2SD_Mean <- "no"}
if(Excluded_2SD_Median |> length() > 0){df$Excluded_2SD_Median <- ifelse(df$trial %in% Excluded_2SD_Median, "yes", "no")}
if(Excluded_2SD_Median |> length() == 0){df$Excluded_2SD_Median <- "no"}

# Add valid-trials column
df$Valid_trial_mean <- ifelse(df$Excluded_100ms == "no" & df$Excluded_200ms == "no" & df$Excluded_2SD_Mean == "no", 1, 0)
df$Valid_trial_median <- ifelse(df$Excluded_100ms == "no" & df$Excluded_200ms == "no" & df$Excluded_2SD_Median == "no", 1, 0)

# Add More Columns --------------------------------------------------------

## TrialWithinCon ----
trial_within_condition <- df |> 
  ungroup() |> 
  select(trial, cond_both) |>
  distinct() |>
  filter(cond_both %in% 1:4) |> 
  group_by(cond_both) |> 
  mutate(TrialWithinCon = row_number()) |> 
  ungroup()

df <- df |> 
  left_join(trial_within_condition, by = c("trial", "cond_both"))

## c_LT_Screen ----
# cueing phase: Total looking time at the screen (i.e., duration of fixations) from beginning of cueing phase video until the end (0-4000ms)
c_lt_screen <- df |> 
  ungroup() |> 
  select(fix_x, fix_y, gaze_sample_duration, Eye.movement.type.index, trial, aoi_screen_fix, stimulus) |> 
  filter(stimulus %in% c("cueing_eyecontact", "cueing_motion", "cueing_objectgaze")) |>
  filter(trial %in% (c(1:32) |> as.character())) |> 
  filter(aoi_screen_fix == "on") |> 
  group_by(trial) |> 
  summarize(c_LT_Screen = sum(gaze_sample_duration, na.rm = T)) |> 
  ungroup()

## c_LT_FaceLeft_start_til_end ----
# cueing phase: Total looking time at the left face (i.e., duration of fixations) from beginning of cueing phase video until the end (0-4000ms)
c_lt_leftactor <- df |> 
  ungroup() |> 
  select(fix_x, fix_y, gaze_sample_duration, Eye.movement.type.index, trial, aoi_fix, stimulus) |> 
  filter(stimulus %in% c("cueing_eyecontact", "cueing_motion", "cueing_objectgaze")) |>
  filter(trial %in% (c(1:32) |> as.character())) |> 
  filter(aoi_fix == "left_actor") |> 
  group_by(trial) |> 
  summarize(c_LT_FaceLeft_start_til_end = sum(gaze_sample_duration, na.rm = T)) |> 
  ungroup()

## c_LT_FaceRight_start_til_end ----
# cueing phase: Total looking time at the right face (i.e., duration of fixations) from beginning of cueing phase video until the end (0-4000ms)
c_lt_rightactor <- df |> 
  ungroup() |> 
  select(fix_x, fix_y, gaze_sample_duration, Eye.movement.type.index, trial, aoi_fix, stimulus) |> 
  filter(stimulus %in% c("cueing_eyecontact", "cueing_motion", "cueing_objectgaze")) |>
  filter(trial %in% (c(1:32) |> as.character())) |> 
  filter(aoi_fix == "right_actor") |> 
  group_by(trial) |> 
  summarize(c_LT_FaceRight_start_til_end = sum(gaze_sample_duration, na.rm = T)) |> 
  ungroup()

## c_LT_FacesTotal_start_til_end ----
# cueing phase: Total looking time at both faces (i.e., duration of fixations) from beginning of cueing phase video until the end à Sum of the two previous columns
c_lt_actors <- c_lt_rightactor |> 
  full_join(c_lt_leftactor) |> 
  group_by(trial) |> 
  mutate(c_LT_FacesTotal_start_til_end = sum(c(c_LT_FaceRight_start_til_end, c_LT_FaceLeft_start_til_end), na.rm = T)) |> 
  ungroup()

## c_LT_FaceLeft_f2f_or_b2b ----
# cueing phase: Total looking time at the left face (i.e., duration of fixations) but only including the still initial sequence (face-to-face or back-to-back and analogous sequence in first-party). That is from video start til 2000ms
c_lt_leftactor_phase1 <- df |> 
  ungroup() |> 
  select(fix_x, fix_y, gaze_sample_duration, Eye.movement.type.index, trial, aoi_fix, stimulus) |> 
  filter(stimulus == "cueing_eyecontact") |>
  filter(trial %in% (c(1:32) |> as.character())) |> 
  filter(aoi_fix == "left_actor") |> 
  group_by(trial) |> 
  summarize(c_LT_FaceLeft_f2f_or_b2b = sum(gaze_sample_duration, na.rm = T)) |> 
  ungroup()

## c_LT_FaceRight_f2f_or_b2b ----
# cueing phase: Total looking time at the right face (i.e., duration of fixations) but only including the still initial sequence (face-to-face or back-to-back and analogous sequence in first-party). That is from video start til 2000ms
c_lt_rightactor_phase1 <- df |> 
  ungroup() |> 
  select(fix_x, fix_y, gaze_sample_duration, Eye.movement.type.index, trial, aoi_fix, stimulus) |> 
  filter(stimulus == "cueing_eyecontact") |>
  filter(trial %in% (c(1:32) |> as.character())) |> 
  filter(aoi_fix == "right_actor") |> 
  group_by(trial) |> 
  summarize(c_LT_FaceRight_f2f_or_b2b = sum(gaze_sample_duration, na.rm = T)) |> 
  ungroup()

## c_LT_FacesTotal_f2f_or_b2b ----
# cueing phase: Total looking time at both faces (i.e., duration of fixations) but only including the still initial sequence (face-to-face or back-to-back and analogous sequence in first-party). à Sum of the two previous coloumns
c_lt_actors_phase1 <- c_lt_rightactor_phase1 |> 
  full_join(c_lt_leftactor_phase1) |> 
  group_by(trial) |> 
  mutate(c_LT_FacesTotal_f2f_or_b2b = sum(c(c_LT_FaceLeft_f2f_or_b2b, c_LT_FaceRight_f2f_or_b2b), na.rm = T)) |> 
  ungroup()

## c_LT_FaceLeft_stillcueing ----
# cueing phase: Total looking time at the left face (i.e., duration of fixations) but only including the still cueing sequence (looking up or down). That is from 3001ms til 4000 
c_lt_leftactor_phase3 <- df |> 
  ungroup() |> 
  select(fix_x, fix_y, gaze_sample_duration, Eye.movement.type.index, trial, aoi_fix, stimulus) |> 
  filter(stimulus == "cueing_objectgaze") |>
  filter(trial %in% (c(1:32) |> as.character())) |> 
  filter(aoi_fix == "left_actor") |> 
  group_by(trial) |> 
  summarize(c_LT_FaceLeft_stillcueing = sum(gaze_sample_duration, na.rm = T)) |> 
  ungroup()

## c_LT_FaceRight_stillcueing ----
# cueing phase: Total looking time at the right face (i.e., duration of fixations) but only including the still cueing sequence (looking up or down). That is from 3001ms til 4000 
c_lt_rightactor_phase3 <- df |> 
  ungroup() |> 
  select(fix_x, fix_y, gaze_sample_duration, Eye.movement.type.index, trial, aoi_fix, stimulus) |> 
  filter(stimulus == "cueing_objectgaze") |>
  filter(trial %in% (c(1:32) |> as.character())) |> 
  filter(aoi_fix == "right_actor") |> 
  group_by(trial) |> 
  summarize(c_LT_FaceRight_stillcueing = sum(gaze_sample_duration, na.rm = T)) |> 
  ungroup()

## c_LT_FacesTotal_stillcueing ----
# cueing phase: Total looking time at the both faces (i.e., duration of fixations) but only including the still cueing sequence (looking up or down). à Sum of the two previous columns
c_lt_actors_phase3 <- c_lt_rightactor_phase3 |> 
  full_join(c_lt_leftactor_phase3) |> 
  group_by(trial) |> 
  mutate(c_LT_FacesTotal_stillcueing = sum(c(c_LT_FaceLeft_stillcueing, c_LT_FaceRight_stillcueing), na.rm = T)) |> 
  ungroup()

## o_duration_start_til_endofvideo ----
# Outcome phase: Duration of outcome phase from start til end (since the outcome phase has different length for different kids; idea: time passing by from video start til video end, irrespective of infants’ gaze.
duration_object_move <- df |> 
  filter(stimulus == "move") |> 
  group_by(trial) |> 
  summarize(o_duration_start_til_endofvideo = sum(gaze_sample_duration)) |>  # alternative to summarize: slice(n()) |> select(trial, timeline)
  ungroup()

## o_duration_start_til_endofstillimage ----
# Outcome phase: Duration of outcome phase from start til end (since the outcome phase has different length for different kids; idea: time passing by from video start til the end of the still image (meaning: inclusive of +1 second), irrespective of infants’ gaze.
duration_object_movestill <- df |> 
  filter(stimulus %in% c("move", "still")) |> 
  group_by(trial) |> 
  summarize(o_duration_start_til_endofstillimage = sum(gaze_sample_duration)) |> 
  ungroup()

## o_LTScreen ----
# Outcome phase: Total looking time at the screen (i.e., duration of fixations) from beginning of outcome phase video until the end
fixtime_onscreen_movestill <- df |> 
  filter(stimulus %in% c("move", "still")) |> 
  filter(aoi_screen_fix == "on") |> 
  group_by(trial) |> 
  summarize(o_LTScreen = sum(gaze_sample_duration, na.rm = T)) |> 
  ungroup()

## o_LT_con_AOI ----
# Outcome phase: Total looking time at the congruent outcome AOI (i.e., duration of fixations) from beginning of outcome phase until the end
fixtime_congruent_movestill <- df |> 
  filter(stimulus %in% c("move", "still")) |> 
  filter(aoi_fix %in% c("top", "bottom")) |> 
  group_by(trial, cue_direction, aoi_fix) |> 
  summarize(o_LT_AOI = sum(gaze_sample_duration, na.rm = T)) |> 
  ungroup() |> 
  mutate(congruence = ifelse((cue_direction == "up" & aoi_fix == "top") | (cue_direction == "down" & aoi_fix == "bottom"), "congruent", "incongruent")) |> 
  filter(congruence == "congruent") |> 
  group_by(trial) |> 
  summarize(o_LT_con_AOI = sum(o_LT_AOI))

## o_LT_incon_AOI ----
# Outcome phase: Total looking time at the incongruent outcome AOI (i.e., duration of fixations) from beginning of outcome phase until the end
fixtime_incongruent_movestill <- df |> 
  filter(stimulus %in% c("move", "still")) |> 
  filter(aoi_fix %in% c("top", "bottom")) |> 
  group_by(trial, cue_direction, aoi_fix) |> 
  summarize(o_LT_AOI = sum(gaze_sample_duration, na.rm = T)) |> 
  ungroup() |> 
  mutate(congruence = ifelse((cue_direction == "up" & aoi_fix == "top") | (cue_direction == "down" & aoi_fix == "bottom"), "congruent", "incongruent")) |> 
  filter(congruence == "incongruent") |> 
  group_by(trial) |> 
  summarize(o_LT_incon_AOI = sum(o_LT_AOI))

## o_FL_AOI_reactive_sample and o_FL_AOI_reactive_fixation ----
# Outcome phase: AOI that is being looked at first after object onset, based on first sample arriving in AOI (concue_conobj = first gaze sample was congruent to cueing direction and congruent to object location; inconcue_conobj, concue_inconobj, inconcue_inconobj)
# Outcome phase: AOI that is being looked at first after object onset, based on first fixation arriving in AOI  (concue_conobj = first gaze sample was congruent to cueing direction and congruent to object location; inconcue_conobj, concue_inconobj, inconcue_inconobj)
# sample-based
o_fl_reactive_sample <- df |> 
  ungroup() |> 
  select(aoi_gs, trial, stimulus, object_location, cue_direction) |> 
  filter(stimulus %in% c("move", "still")) |>
  filter(trial %in% (c(1:32) |> as.character())) |> 
  filter(aoi_gs %in% c("top", "bottom")) |> 
  group_by(trial) |> 
  slice(1) |> 
  ungroup() |> 
  select(-stimulus)

o_fl_reactive_sample$o_FL_AOI_reactive_sample <- NA
o_fl_reactive_sample[which(o_fl_reactive_sample$object_location == "top" & o_fl_reactive_sample$cue_direction == "up" &  o_fl_reactive_sample$aoi_gs == "top"),"o_FL_AOI_reactive_sample"] <- "concue_conobj"
o_fl_reactive_sample[which(o_fl_reactive_sample$object_location == "bottom" & o_fl_reactive_sample$cue_direction == "down" &  o_fl_reactive_sample$aoi_gs == "bottom"),"o_FL_AOI_reactive_sample"] <- "concue_conobj"
o_fl_reactive_sample[which(o_fl_reactive_sample$object_location == "top" & o_fl_reactive_sample$cue_direction == "down" &  o_fl_reactive_sample$aoi_gs == "top"),"o_FL_AOI_reactive_sample"] <- "inconcue_conobj"
o_fl_reactive_sample[which(o_fl_reactive_sample$object_location == "bottom" & o_fl_reactive_sample$cue_direction == "up" &  o_fl_reactive_sample$aoi_gs == "bottom"),"o_FL_AOI_reactive_sample"] <- "inconcue_conobj"
o_fl_reactive_sample[which(o_fl_reactive_sample$object_location == "top" & o_fl_reactive_sample$cue_direction == "down" &  o_fl_reactive_sample$aoi_gs == "bottom"),"o_FL_AOI_reactive_sample"] <- "concue_inconobj"
o_fl_reactive_sample[which(o_fl_reactive_sample$object_location == "bottom" & o_fl_reactive_sample$cue_direction == "up" &  o_fl_reactive_sample$aoi_gs == "top"),"o_FL_AOI_reactive_sample"] <- "concue_inconobj"
o_fl_reactive_sample[which(o_fl_reactive_sample$object_location == "top" & o_fl_reactive_sample$cue_direction == "up" &  o_fl_reactive_sample$aoi_gs == "bottom"),"o_FL_AOI_reactive_sample"] <- "inconcue_inconobj"
o_fl_reactive_sample[which(o_fl_reactive_sample$object_location == "bottom" & o_fl_reactive_sample$cue_direction == "down" &  o_fl_reactive_sample$aoi_gs == "top"),"o_FL_AOI_reactive_sample"] <- "inconcue_inconobj"

# fixation-based
o_fl_reactive_fixation <- df |> 
  ungroup() |> 
  select(aoi_fix, trial, stimulus, object_location, cue_direction) |> 
  filter(stimulus %in% c("move", "still")) |>
  filter(trial %in% (c(1:32) |> as.character())) |> 
  filter(aoi_fix %in% c("top", "bottom")) |> 
  group_by(trial) |> 
  slice(1) |> 
  ungroup() |> 
  select(-stimulus)

o_fl_reactive_fixation$o_FL_AOI_reactive_fixation <- NA
o_fl_reactive_fixation[which(o_fl_reactive_fixation$object_location == "top" & o_fl_reactive_fixation$cue_direction == "up" &  o_fl_reactive_fixation$aoi_fix == "top"),"o_FL_AOI_reactive_fixation"] <- "concue_conobj"
o_fl_reactive_fixation[which(o_fl_reactive_fixation$object_location == "bottom" & o_fl_reactive_fixation$cue_direction == "down" &  o_fl_reactive_fixation$aoi_fix == "bottom"),"o_FL_AOI_reactive_fixation"] <- "concue_conobj"
o_fl_reactive_fixation[which(o_fl_reactive_fixation$object_location == "top" & o_fl_reactive_fixation$cue_direction == "down" &  o_fl_reactive_fixation$aoi_fix == "top"),"o_FL_AOI_reactive_fixation"] <- "inconcue_conobj"
o_fl_reactive_fixation[which(o_fl_reactive_fixation$object_location == "bottom" & o_fl_reactive_fixation$cue_direction == "up" &  o_fl_reactive_fixation$aoi_fix == "bottom"),"o_FL_AOI_reactive_fixation"] <- "inconcue_conobj"
o_fl_reactive_fixation[which(o_fl_reactive_fixation$object_location == "top" & o_fl_reactive_fixation$cue_direction == "down" &  o_fl_reactive_fixation$aoi_fix == "bottom"),"o_FL_AOI_reactive_fixation"] <- "concue_inconobj"
o_fl_reactive_fixation[which(o_fl_reactive_fixation$object_location == "bottom" & o_fl_reactive_fixation$cue_direction == "up" &  o_fl_reactive_fixation$aoi_fix == "top"),"o_FL_AOI_reactive_fixation"] <- "concue_inconobj"
o_fl_reactive_fixation[which(o_fl_reactive_fixation$object_location == "top" & o_fl_reactive_fixation$cue_direction == "up" &  o_fl_reactive_fixation$aoi_fix == "bottom"),"o_FL_AOI_reactive_fixation"] <- "inconcue_inconobj"
o_fl_reactive_fixation[which(o_fl_reactive_fixation$object_location == "bottom" & o_fl_reactive_fixation$cue_direction == "down" &  o_fl_reactive_fixation$aoi_fix == "top"),"o_FL_AOI_reactive_fixation"] <- "inconcue_inconobj"

## o_FL_AOI_predictive_sample and o_FL_AOI_predictive_fixation ----
# Outcome phase: AOI that is being looked at first after cueing motion started (i.e., from 2001 onwards) first sample arrives in AOI, captured until object onset (i.e., video start of moving object) (concue_conobj = first gaze sample was congruent to cueing direction and congruent to object location; inconcue_conobj, concue_inconobj, inconcue_inconobj)
# Outcome phase: AOI that is being looked at first after cueing motion started (i.e., from 2001 onwards) first fixation arrives in AOI, captured until object onset (i.e., video start of moving object) (concue_conobj = first gaze sample was congruent to cueing direction and congruent to object location; inconcue_conobj, concue_inconobj, inconcue_inconobj)

# sample-based
o_fl_predictive_sample <- df |> 
  ungroup() |> 
  select(aoi_gs, trial, stimulus, object_location, cue_direction) |> 
  filter(stimulus %in% c("cueing_motion", "cueing_objectgaze")) |>
  filter(trial %in% (c(1:32) |> as.character())) |> 
  filter(aoi_gs %in% c("top", "bottom")) |> 
  group_by(trial) |> 
  slice(1) |> 
  ungroup() |> 
  select(-stimulus)

o_fl_predictive_sample$o_FL_AOI_predictive_sample <- NA
o_fl_predictive_sample[which(o_fl_predictive_sample$object_location == "top" & o_fl_predictive_sample$cue_direction == "up" &  o_fl_predictive_sample$aoi_gs == "top"),"o_FL_AOI_predictive_sample"] <- "concue_conobj"
o_fl_predictive_sample[which(o_fl_predictive_sample$object_location == "bottom" & o_fl_predictive_sample$cue_direction == "down" &  o_fl_predictive_sample$aoi_gs == "bottom"),"o_FL_AOI_predictive_sample"] <- "concue_conobj"
o_fl_predictive_sample[which(o_fl_predictive_sample$object_location == "top" & o_fl_predictive_sample$cue_direction == "down" &  o_fl_predictive_sample$aoi_gs == "top"),"o_FL_AOI_predictive_sample"] <- "inconcue_conobj"
o_fl_predictive_sample[which(o_fl_predictive_sample$object_location == "bottom" & o_fl_predictive_sample$cue_direction == "up" &  o_fl_predictive_sample$aoi_gs == "bottom"),"o_FL_AOI_predictive_sample"] <- "inconcue_conobj"
o_fl_predictive_sample[which(o_fl_predictive_sample$object_location == "top" & o_fl_predictive_sample$cue_direction == "down" &  o_fl_predictive_sample$aoi_gs == "bottom"),"o_FL_AOI_predictive_sample"] <- "concue_inconobj"
o_fl_predictive_sample[which(o_fl_predictive_sample$object_location == "bottom" & o_fl_predictive_sample$cue_direction == "up" &  o_fl_predictive_sample$aoi_gs == "top"),"o_FL_AOI_predictive_sample"] <- "concue_inconobj"
o_fl_predictive_sample[which(o_fl_predictive_sample$object_location == "top" & o_fl_predictive_sample$cue_direction == "up" &  o_fl_predictive_sample$aoi_gs == "bottom"),"o_FL_AOI_predictive_sample"] <- "inconcue_inconobj"
o_fl_predictive_sample[which(o_fl_predictive_sample$object_location == "bottom" & o_fl_predictive_sample$cue_direction == "down" &  o_fl_predictive_sample$aoi_gs == "top"),"o_FL_AOI_predictive_sample"] <- "inconcue_inconobj"

# fixation-based
o_fl_predictive_fixation <- df |> 
  ungroup() |> 
  select(aoi_fix, trial, stimulus, object_location, cue_direction) |> 
  filter(stimulus %in% c("cueing_motion", "cueing_objectgaze")) |>
  filter(trial %in% (c(1:32) |> as.character())) |> 
  filter(aoi_fix %in% c("top", "bottom")) |> 
  group_by(trial) |> 
  slice(1) |> 
  ungroup() |> 
  select(-stimulus)

o_fl_predictive_fixation$o_FL_AOI_predictive_fixation <- NA
o_fl_predictive_fixation[which(o_fl_predictive_fixation$object_location == "top" & o_fl_predictive_fixation$cue_direction == "up" &  o_fl_predictive_fixation$aoi_fix == "top"),"o_FL_AOI_predictive_fixation"] <- "concue_conobj"
o_fl_predictive_fixation[which(o_fl_predictive_fixation$object_location == "bottom" & o_fl_predictive_fixation$cue_direction == "down" &  o_fl_predictive_fixation$aoi_fix == "bottom"),"o_FL_AOI_predictive_fixation"] <- "concue_conobj"
o_fl_predictive_fixation[which(o_fl_predictive_fixation$object_location == "top" & o_fl_predictive_fixation$cue_direction == "down" &  o_fl_predictive_fixation$aoi_fix == "top"),"o_FL_AOI_predictive_fixation"] <- "inconcue_conobj"
o_fl_predictive_fixation[which(o_fl_predictive_fixation$object_location == "bottom" & o_fl_predictive_fixation$cue_direction == "up" &  o_fl_predictive_fixation$aoi_fix == "bottom"),"o_FL_AOI_predictive_fixation"] <- "inconcue_conobj"
o_fl_predictive_fixation[which(o_fl_predictive_fixation$object_location == "top" & o_fl_predictive_fixation$cue_direction == "down" &  o_fl_predictive_fixation$aoi_fix == "bottom"),"o_FL_AOI_predictive_fixation"] <- "concue_inconobj"
o_fl_predictive_fixation[which(o_fl_predictive_fixation$object_location == "bottom" & o_fl_predictive_fixation$cue_direction == "up" &  o_fl_predictive_fixation$aoi_fix == "top"),"o_FL_AOI_predictive_fixation"] <- "concue_inconobj"
o_fl_predictive_fixation[which(o_fl_predictive_fixation$object_location == "top" & o_fl_predictive_fixation$cue_direction == "up" &  o_fl_predictive_fixation$aoi_fix == "bottom"),"o_FL_AOI_predictive_fixation"] <- "inconcue_inconobj"
o_fl_predictive_fixation[which(o_fl_predictive_fixation$object_location == "bottom" & o_fl_predictive_fixation$cue_direction == "down" &  o_fl_predictive_fixation$aoi_fix == "top"),"o_FL_AOI_predictive_fixation"] <- "inconcue_inconobj"

## o_SRT_predictive_sample and o_SRT_predictive_fixation ----
# Outcome phase: Saccadic Reaction Time MAIN DEPENDENT MEASURE! Time interval between the start of cueing motion (i.e., from 2001 onwards) until object onset (i.e., video start of moving object), based on first sample appearance of the target object (video start) and the arrival of the infant’s gaze (not fixation) in the respective target AOI (i.e., the outcome position)
# Outcome phase: Saccadic Reaction Time SECONDARY DEPENDENT MEASURE (keine prio, nur zum exploren) Time interval between the start of cueing motion (i.e., from 2001 onwards) until object onset (i.e., video start of moving object), Time interval between the appearance of the target object (video start) and the arrival of the infant’s gaze of first fixation in the respective target AOI (i.e., the outcome position)

# sample-based
latencies_predicitive_gs <- df |> 
  filter(stimulus %in% c("cueing_motion", "cueing_objectgaze")) |> 
  group_by(trial, stimulus) |> 
  mutate(timeline = cumsum(gaze_sample_duration)) |> 
  ungroup() |> 
  select(x, y, Presented.Stimulus.name, trial, timeline, cue_direction, aoi_gs) |>
  filter(aoi_gs %in% c("top", "bottom")) |>
  group_by(trial) |> 
  slice(1) |> 
  ungroup() |> 
  rename(o_SRT_predictive_sample = timeline) |> 
  filter((cue_direction == "down" & aoi_gs == "bottom") | (cue_direction == "up" & aoi_gs == "top")) # only leave the cases in which the first predicitive look aoi and the cueing direction aligned with each other (both top or bottom)

# fixation-based
latencies_predicitive_fix <- df |> 
  filter(stimulus %in% c("cueing_motion", "cueing_objectgaze")) |> 
  group_by(trial, stimulus) |> 
  mutate(timeline = cumsum(gaze_sample_duration)) |> 
  ungroup() |> 
  select(x, y, Presented.Stimulus.name, trial, timeline, cue_direction, aoi_fix) |>
  filter(aoi_fix %in% c("top", "bottom")) |>
  group_by(trial) |> 
  slice(1) |> 
  ungroup() |> 
  rename(o_SRT_predictive_fixation = timeline) |> 
  filter((cue_direction == "down" & aoi_fix == "bottom") | (cue_direction == "up" & aoi_fix == "top")) # only leave the cases in which the first predicitive look aoi and the cueing direction aligned with each other (both top or bottom)

# o_SRT_valid_fixation and o_SRT_valid_sample
# If column “SRT_predictive_fixation” has data, then copy this data in this column here, otherwise use data from “SRT_reactive_fixation” column (if both reactive and predictive have data, predictive should be considered as the real initial response)
# If column “SRT_predictive_sample” has data, then copy this data in this column here, otherwise use data from “SRT_reactive_sample” column (if both reactive and predictive have data, predictive should be considered as the real initial response)

# Create DF Final ---------------------------------------------------------
df_final <- df |> select(trial, stimulus, Presented.Stimulus.name) |> rename(RunningTrial = trial) |> filter(!(stimulus %in% c(NA, "checkflake", "at", "Kalei1", "Kalei2", "Kalei3", "Kalei4", "Kalei5", "Kalei6"))) |> filter(str_detect(Presented.Stimulus.name, "move")) |> group_by(RunningTrial) |> slice(1) |> ungroup() |> select(RunningTrial, Presented.Stimulus.name) |> 
  left_join(latencies_top_gs |> rename(RunningTrial = trial) |> select(-c(x, y, aoi_gs)) |> bind_rows(latencies_bottom_gs |> rename(RunningTrial = trial) |> select(-c(x, y, aoi_gs)))) |> 
  left_join(latencies_top_fix |> bind_rows(latencies_bottom_fix) |> select(-c(x, y, aoi_fix)) |> rename(RunningTrial = trial)) |> 
  separate(Presented.Stimulus.name, into = c("x0", "x1", "x2", "ConNo", "Con_shared", "CueingDir",
                                             "Con_outcon", "Actors", "ObjectNo", "PosObjOut", "Study")) |> 
  select(-c(x0, x1, x2)) |> 
  left_join(latencies_predicitive_gs |> rename(RunningTrial = trial) |> select(RunningTrial, o_SRT_predictive_sample), by = "RunningTrial") |> 
  left_join(latencies_predicitive_fix |> rename(RunningTrial = trial) |> select(RunningTrial, o_SRT_predictive_fixation), by = "RunningTrial") |> 
  mutate(o_SRT_valid_sample = if_else(!is.na(o_SRT_predictive_sample), o_SRT_predictive_sample, o_SRT_reactive_sample)) |> 
  mutate(o_SRT_valid_fixation = if_else(!is.na(o_SRT_predictive_fixation), o_SRT_predictive_fixation, o_SRT_reactive_fixation)) |> 
  left_join(c_lt_screen |> rename(RunningTrial = trial), by = "RunningTrial") |> 
  left_join(o_fl_reactive_sample |> select(trial, o_FL_AOI_reactive_sample) |> rename(RunningTrial = trial), by = "RunningTrial") |> 
  left_join(o_fl_reactive_fixation |> select(trial, o_FL_AOI_reactive_fixation) |> rename(RunningTrial = trial), by = "RunningTrial") |> 
  left_join(o_fl_predictive_sample |> select(trial, o_FL_AOI_predictive_sample) |> rename(RunningTrial = trial), by = "RunningTrial") |> 
  left_join(o_fl_predictive_fixation |> select(trial, o_FL_AOI_predictive_fixation) |> rename(RunningTrial = trial), by = "RunningTrial") |> 
  left_join(c_lt_actors |> rename(RunningTrial = trial), by = "RunningTrial") |> 
  left_join(c_lt_actors_phase1 |> rename(RunningTrial = trial), by = "RunningTrial") |> 
  left_join(c_lt_actors_phase3 |> rename(RunningTrial = trial), by = "RunningTrial") |> 
  left_join(duration_object_move |> rename(RunningTrial = trial), by = "RunningTrial") |> 
  left_join(duration_object_movestill |> rename(RunningTrial = trial), by = "RunningTrial") |> 
  left_join(fixtime_onscreen_movestill |> rename(RunningTrial = trial), by = "RunningTrial") |> 
  left_join(fixtime_congruent_movestill |> rename(RunningTrial = trial), by = "RunningTrial") |> 
  left_join(fixtime_incongruent_movestill |> rename(RunningTrial = trial), by = "RunningTrial") |> 
  mutate(filename = filename) |>
  mutate(filename = str_remove(filename, ".tsv")) |> 
  separate(filename, into = c("x1", "ID", "Sex", "AgeGroup", "x2", "Experiment")) |> 
  select(-c(x1, x2)) |> 
  left_join(df |> ungroup() |> select(trial, Excluded_100ms, Excluded_200ms, Excluded_2SD_Mean, Excluded_2SD_Median, Valid_trial_mean, Valid_trial_median, TrialWithinCon) |> distinct() |> 
              rename(RunningTrial = trial) |> filter(!RunningTrial %in% c("", "Eyetracker", "pre", "post", "mid", "x")), by = "RunningTrial") |> 
  mutate(Sex = as.character(Sex)) |> 
  mutate(Sex = str_replace(Sex, "F", "W"))

df_final <- as.data.frame(df_final)
df_final <- df_final[,c("ID", "AgeGroup", "Sex", "Experiment", "RunningTrial", "TrialWithinCon", "ConNo", "Con_shared", "Con_outcon", "CueingDir",
                        "Actors", "ObjectNo", "PosObjOut", "Study", "Valid_trial_mean", "Valid_trial_median", "Excluded_100ms", "Excluded_200ms", "Excluded_2SD_Mean", "Excluded_2SD_Median",
                        "c_LT_Screen", "c_LT_FaceLeft_start_til_end", "c_LT_FaceRight_start_til_end", "c_LT_FacesTotal_start_til_end",
                        "c_LT_FaceLeft_f2f_or_b2b", "c_LT_FaceRight_f2f_or_b2b", "c_LT_FacesTotal_f2f_or_b2b",
                        "c_LT_FaceLeft_stillcueing", "c_LT_FaceRight_stillcueing", "c_LT_FacesTotal_stillcueing",
                        "o_duration_start_til_endofvideo", "o_duration_start_til_endofstillimage",
                        "o_LTScreen", "o_LT_con_AOI", "o_LT_incon_AOI",
                        "o_FL_AOI_reactive_sample", "o_FL_AOI_reactive_fixation", "o_FL_AOI_predictive_sample", "o_FL_AOI_predictive_fixation",
                        "o_SRT_reactive_sample", "o_SRT_reactive_fixation", "o_SRT_predictive_sample", "o_SRT_predictive_fixation", "o_SRT_valid_fixation", "o_SRT_valid_sample")]

## Calibration ----
df_calibration <- df |> 
  ungroup() |> 
  select(Recording.name, Timeline.name, matches("validation|calibration", ignore.case = TRUE)) |> 
  distinct() |> 
  rename(rec_name = Recording.name, 
         experiment = Timeline.name, 
         cal_acc_mm = Average.calibration.accuracy..mm., 
         cal_precsd_mm = Average.calibration.precision.SD..mm.,
         cal_precrms_mm = Average.calibration.precision.RMS..mm., 
         cal_acc_deg = Average.calibration.accuracy..degrees.,
         cal_precsd_deg = Average.calibration.precision.SD..degrees., 
         cal_precrms_deg = Average.calibration.precision.RMS..degrees.,
         cal_acc_px = Average.calibration.accuracy..pixels., 
         cal_precsd_px = Average.calibration.precision.SD..pixels.,
         cal_precrms_px = Average.calibration.precision.RMS..pixels., 
         val_acc_mm = Average.validation.accuracy..mm.,
         val_precsd_mm = Average.validation.precision.SD..mm., 
         val_precrms_mm = Average.validation.precision.RMS..mm.,
         val_acc_deg = Average.validation.accuracy..degrees., 
         val_precsd_deg = Average.validation.precision.SD..degrees.,
         val_precrms_deg = Average.validation.precision.RMS..degrees., 
         val_acc_px = Average.validation.accuracy..pixels.,
         val_precsd_px = Average.validation.precision.SD..pixels., 
         val_precrms_px = Average.validation.precision.RMS..pixels.)

# Implement Marlene Coefficent ----
# [(RTuncued trials – RTcued trials)/(( RTuncued trials + RTcued trials)/2)] * 100
df_final_marlene <- df_final |> 
  group_by(ID, AgeGroup) |>  
  summarise(
    rt_uncued = mean(o_SRT_reactive_sample[Con_outcon == "incon"], na.rm = TRUE),
    rt_cued = mean(o_SRT_reactive_sample[Con_outcon == "con"], na.rm = TRUE),
    .groups = "drop"
  ) |> 
  mutate(
    marlene_coefficient = ((rt_uncued - rt_cued) / (rt_uncued + rt_cued) / 2) * 100
  )
write.table(df_final_marlene, here("data", "first", "marlene_preproc", age_group, filename), row.names = F, quote = F, sep = "\t", dec = ".")

print(paste0(filename, ". Accuracy: ", df_calibration$cal_acc_deg, ". Precision (SD): ", df_calibration$cal_precsd_deg, ". Precision (RMS): ", df_calibration$cal_precrms_deg, "."))

# Write Data --------------------------------------------------------------
write.table(df_final, here("data", "first", "preproc", age_group, filename), row.names = F, quote = F, sep = "\t", dec = ".")
write.table(df_calibration, here("data", "both", "calibration_tobii", age_group, paste0("calibration_tobii_first_", age_group, "_", sub("\\.tsv$", "", filename), ".txt")), 
            row.names = F, quote = F, sep = "\t", dec = ".")
print(i)
}


