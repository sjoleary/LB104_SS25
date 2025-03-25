
# Load necessary library
library(dplyr)
library(readr)

# Set seed for reproducibility
set.seed(42)

## Germination data ----

# Define experimental conditions
deicers <- c("rock_salt", "cheese_brine", "comm_blend", "control")

treatment_levels <- c("high", "medium", "low")

concentrations <- list(
  "rock_salt" = c(10, 1, 0.01),      # Concentration for rock_salt
  "cheese_brine" = c(10, 1, 0.1),    # Concentration for cheese_brine
  "comm_blend" = c(10, 1, 0.01), # Concentration for comm_blend
  "control" = c(0)                   # control (No Deicer)
)

units <- c("rock_salt" = "mg/L", "cheese_brine" = "mL/L", "comm_blend" = "mg/L", "control" = "-")

replicates <- c(1, 2, 3)

num_seeds <- 20

setup_date <- "2025-03-01"

observation_date <- "2025-03-02"

observation_time <- "08:00"

initials <- "JD"

# Generate data
experiment_data <- data.frame()

for (deicer in deicers) {
  for (i in 1:length(treatment_levels)) {
    for (replicate in replicates) {
      for (seed in 1:num_seeds) {
        
        # Germination logic based on concentration and deicer type
        concentration <- concentrations[[deicer]][i]
        
        # For rock_salt: Dose-dependent inhibition (0.01 g/L similar to control, higher concentrations more inhibitory)
        if (deicer == "rock_salt") {
          if (concentration == 10) {
            germination <- sample(c("germinated", "no_germination"), 1, prob = c(0.21, 0.79)) # Strong inhibition
          } else if (concentration == 1) {
            germination <- sample(c("germinated", "no_germination"), 1, prob = c(0.42, 0.58)) # Moderate inhibition
          } else {
            germination <- sample(c("germinated", "no_germination"), 1, prob = c(0.71, 0.29)) # low concentration, similar to control
          }
        }
        
        # For cheese_brine: Only inhibits germination at the highest concentration
        if (deicer == "cheese_brine") {
          if (concentration == 10) {
            germination <- sample(c("germinated", "no_germination"), 1, prob = c(0.68, 0.32)) # Strong inhibition
          } else if (concentration == 1) {
            germination <- sample(c("germinated", "no_germination"), 1, prob = c(0.89, 0.11)) # Moderate inhibition
          } else {
            germination <- sample(c("germinated", "no_germination"), 1, prob = c(0.89, 0.11)) # low concentration, similar to control
          }
        }
        
        # For comm_blend: Dose-dependent effect, but less strong than rock_salt
        if (deicer == "comm_blend") {
          if (concentration == 10) {
            germination <- sample(c("germinated", "no_germination"), 1, prob = c(0.43, 0.57)) # Moderate inhibition
          } else if (concentration == 1) {
            germination <- sample(c("germinated", "no_germination"), 1, prob = c(0.66, 0.34)) # Mild inhibition
          } else {
            germination <- sample(c("germinated", "no_germination"), 1, prob = c(0.85, 0.15)) # low concentration, similar to control
          }
        }
        
        # For control: No inhibition
        if (deicer == "control") {
          germination <- sample(c("germinated", "no_germination"), 1, prob = c(0.89, 0.11))
        }
        
        # Define notes (no issues for now)
        notes <- "No issues"
        
        # Add data to dataframe
        experiment_data <- rbind(experiment_data, data.frame(
          deicer = deicer,
          treatment_level = treatment_levels[i],
          concentration = concentration,
          units = units[[deicer]],
          replicate = replicate,
          seed_number = seed,
          germination = germination,
          setup_date = setup_date,
          observation_date = observation_date,
          observation_time = observation_time,
          initials = initials,
          notes = notes
        ))
      }
    }
  }
}

experiment_data <- experiment_data %>%
  filter(!(deicer == "control" & treatment_level %in% c("medium", "low")))

# Save as CSV
write_delim(experiment_data, "data/germination-1.csv", 
            delim = ",")


## make seedling length data ----

# Define mean and SD for seedling length (mm)
seedling_length_means <- list(
  "control" = 11.2,
  "rock_salt" = c(3.8, 6.2, 10.8),  # high, medium, low
  "cheese_brine" = c(8.5, 10.3, 11.1),  # high, medium, low
  "comm_blend" = c(4.2, 8.3, 10.5)  # high, medium, low
)

seedling_length_sds <- list(
  "control" = 2.4,
  "rock_salt" = c(1.2, 1.5, 2.4),
  "cheese_brine" = c(3.2, 2.9, 2.3),
  "comm_blend" = c(1.8, 2.4, 2.1)
)

# Generate data set
experiment_data <- data.frame()

for (deicer in deicers) {
  for (i in 1:length(treatment_levels)) {
    for (replicate in replicates) {
      for (seed in 1:num_seeds) {
        
        # Germination logic based on concentration and deicer type
        concentration <- concentrations[[deicer]][i]
        
        # Assign germination probabilities
        if (deicer == "rock_salt") {
          if (concentration == 10) {
            germination <- sample(c("germinated", "no_germination"), 1, prob = c(0.3, 0.7))
          } else if (concentration == 1) {
            germination <- sample(c("germinated", "no_germination"), 1, prob = c(0.5, 0.5))
          } else {
            germination <- "germinated"
          }
        } else if (deicer == "cheese_brine") {
          if (concentration == 10) {
            germination <- sample(c("germinated", "no_germination"), 1, prob = c(0.4, 0.6))
          } else {
            germination <- "germinated"
          }
        } else if (deicer == "comm_blend") {
          if (concentration == 10) {
            germination <- sample(c("germinated", "no_germination"), 1, prob = c(0.5, 0.5))
          } else if (concentration == 1) {
            germination <- sample(c("germinated", "no_germination"), 1, prob = c(0.7, 0.3))
          } else {
            germination <- "germinated"
          }
        } else {
          germination <- "germinated"
        }
        
        # Assign seedling length if germinated, otherwise NA
        if (germination == "germinated") {
          if (deicer == "control") {
            seedling_length <- rnorm(1, mean = seedling_length_means[["control"]], sd = seedling_length_sds[["control"]])
          } else {
            seedling_length <- rnorm(1, mean = seedling_length_means[[deicer]][i], sd = seedling_length_sds[[deicer]][i])
          }
        } else {
          seedling_length <- NA  # No length if not germinated
        }
        
        # Ensure no negative values for seedling length
        seedling_length <- max(seedling_length, 0)
        
        # Define notes (no issues for now)
        notes <- "No issues"
        
        # Append to dataframe
        experiment_data <- rbind(experiment_data, data.frame(
          deicer = deicer,
          treatment_Level = treatment_levels[i],
          concentration = concentration,
          unit = units[[deicer]],
          replicate = replicate,
          seed_number = seed,
          germination = germination,
          seedling_length_mm = round(seedling_length, 2),
          setup_date = setup_date,
          observation_date = observation_date,
          observation_time = observation_time,
          initials = initials,
          notes = notes
        ))
      }
    }
  }
}

# finalize data set
experiment_data <- experiment_data %>%
  filter(germination == "germinated") %>%
  select(-germination)

# Save as CSV
write_delim(experiment_data, "data/seedling_length-1.csv",
            delim = ",")




