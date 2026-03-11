### Preliminary ----------------------------------------------------------------
# Load libraries
library(arrow)
library(tidyverse)

# Load parquet files for the CHORUS data
positions <- read_parquet("Initial Data/Chorus/positions.parquet")
bills <- read_parquet("Initial Data/Chorus/bills.parquet")
clients <- read_parquet("Initial Data/Chorus/clients.parquet")

# Load slip df for Witness Slips data
load("Initial Data/Witness Slips/Witness Slips Dataframe (Cleaned).Rda")

### Compare Illinois and Arizona Data Coverage
position_az <- filter(positions, state == "AZ") %>%
  select(state, session, )
