# 👀 Gaze-Cueing in Infancy: Data Processing and Analysis

## 📖 Description

Previous research has shown that preverbal infants show covert gaze-cueing effects from early on. When seeing an adult shift their (head and) gaze in the direction of location on a screen, infants are faster in looking at a subsequently appearing target object when it is displayed at the previously cued location compared to an incongruent location (“gaze-cueing effect”). However, in most previous studies, the adult directly gazed in the direction of the infant before shifting their gaze. Depending on the age of the infant, this simulated moment of initial eye contact may have potentially elicited a perception of interpersonal connectedness. This study systematically manipulates the presence of infant-directed gaze, to disentangle the influence of interpersonal connectedness on the gaze-cueing effect and, this way, advance our understanding of covert attentional processes underlying infants’ attention orienting during joint attention interactions. Three independent samples of 4-, 6- and 9-month-old infants will participate in a screen-based gaze-cueing task featuring videos displaying the face of a female actor. The actor will either face the infant and look in their direction (simulating interpersonal connectedness through eye contact; joint attention cueing condition) or avert their head and gaze, looking away from the infant (no interpersonal connectedness; parallel attention cueing condition). Then, the actor will shift their head and gaze in the direction of one out of two spatial locations on screen. After this cueing phase, the actor will disappear, and a target object will appear at one of the two target locations: the previously gaze-cued location (congruent outcome) or the opposite spatial location (incongruent outcome). To investigate infants’ covert attention shifts during joint and parallel attention cueing, we will compare their saccadic reaction times to the target object on congruent versus incongruent trials.

## 👩‍🔬 Researchers

- [Maleen Thiele](https://scholar.google.com/citations?user=akL3_ssAAAAJ&hl=de)
- [Moritz Daum](https://scholar.google.com/citations?user=vmv4eU8AAAAJ&hl=de)
- Daniela Schmidt

## 📁 Repository Structure

### 📜 R Project and Configuration

- `cajo_analysis.Rproj` — The RStudio project file. **Open this file before running any scripts** to ensure that project-relative paths and the project configuration are used correctly.
- `.Rprofile` — Loads the project-specific `renv` configuration.
- `renv.lock` — Records the R package versions and dependencies used in the project to facilitate reproducibility.
- `renv/` — Contains configuration files used by `renv`.

### 🧮 Analysis Scripts

| Script | Description |
|---|---|
| `1_inclusion_both.R` | Determines participant inclusion based on study protocols stored in the `docs/` folder. These protocols are not publicly available for privacy reasons. Consequently, this script cannot be run as provided in the public repository. |
| `2_preprocessing_first.R` | Preprocesses eye-tracking data from the first-party interaction condition. |
| `2_preprocessing_third.R` | Preprocesses eye-tracking data from the third-party interaction condition. |
| `3_descriptives_first` | Loads preprocessed data from the first-party interaction condition, calculates descriptive statistics, and generates plots. |
| `3_descriptives_third` | Loads preprocessed data from the third-party interaction condition, calculates descriptive statistics, and generates plots. |

### 🗂️ Data

The `data/` directory is organized by condition and data type.

#### `data/both/`

Contains information relevant to both conditions:

- `calibration_tobii/` — Tobii calibration results for individual participants.
- `distribution_experiments/` — Summaries of how often each of the four Tobii timelines was presented within each age group. Four timelines were created to counterbalance potentially influential factors.
- `exclusion_reasons/` — Files summarizing participant exclusion reasons by age group.
- `final_sample_information/` — Participant information, including age and sex.

#### `data/first/` and `data/third/`

- `preproc/` — Preprocessed eye-tracking data.
- `raw_all/` — Raw eye-tracking data.
- `raw_included/` — Raw eye-tracking data from included participants.

**Note:** Raw eye-tracking data are not included in the public repository due to storage constraints. The study protocols used for participant inclusion are also not publicly available for privacy reasons.

## 🛠️ Getting Started

### 1. Open the R Project

Clone or download this repository and open `cajo_analysis.Rproj` in RStudio.

Opening the project file is important because the scripts rely on the project environment and project-relative file paths.

### 2. Restore the R Package Environment

This project uses [`renv`](https://rstudio.github.io/renv/) to document and restore the R package environment.

The `renv.lock` file records the package versions used when the environment was saved. To install the corresponding packages:

1. Open the project in RStudio.
2. If `renv` is not installed, install it by running:

   ```r
   install.packages("renv")
   ```

3. Restore the project library by running:

   ```r
   renv::restore()
   ```

This will install the package versions recorded in `renv.lock`, where compatible with your R version and operating system.

To check the status of the project environment, run:

```r
renv::status()
```

**Important:** Package restoration may require additional system dependencies, depending on your operating system and R installation.

### 3. Run the Analysis Scripts

## 📩 Data Requests

The raw data is available upon request from the researchers involved in this project. Please contact **[Maleen Thiele](https://scholar.google.com/citations?user=akL3_ssAAAAJ&hl=de)** to enquire about access.

