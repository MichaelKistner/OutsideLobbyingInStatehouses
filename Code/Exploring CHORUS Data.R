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

### Compare Illinois and Arizona Data Coverage ---------------------------------
# Get Arizona data and format
positions_az <- positions %>% 
  filter(state == "AZ", year > 2006) %>%
  select(-c(lobbyist_firm_name, committee, docket_number, docket_prefix)) %>%
  rename(group_name = client_name,
         position_taker = lobbyist_rep_name,
         bill_name = description) %>%
  mutate(
    session = case_when(
      year <= 2008 ~ "2007-2008",
      year <= 2010 ~ "2009-2010",
      year <= 2012 ~ "2011-2012",
      year <= 2014 ~ "2013-2014",
      year <= 2016 ~ "2015-2016",
      year <= 2018 ~ "2017-2018",
      year <= 2020 ~ "2019-2020",
      year <= 2022 ~ "2021-2022",
    )
  )

# Get Illinois data and format
positions_il <- slips %>%
  mutate(state = "IL",
         session = case_when(
           session == "1314" ~ "2013-2014",
           session == "1516" ~ "2015-2016",
           session == "1718" ~ "2017-2018",
           session == "1920" ~ "2019-2020",
           session == "2122" ~ "2021-2022"
         )) %>%
  mutate(year = year(date)) %>%
  rename(position_numeric = position,
         position_taker = witness_name) 

# Save data samples for review
slice_sample(positions_az, n = 100) %>%
  write_csv("Processed Data/Sample of Arizona Position Data.csv")

slice_sample(positions_il, n = 100) %>%
  write_csv("Processed Data/Sample of Illinois Position Data.csv")

# Combine data for graphing
graphing_df <- bind_rows(positions_az, positions_il) %>%
  mutate(position = case_when(
    position_numeric == 1 ~ "For",
    position_numeric == -1 ~ "Against",
    TRUE ~ NA
  )) %>%
  group_by(state, session, position) %>%
  summarize(count = n()) %>%
  na.omit()

# Create plot
ggplot(graphing_df, aes(x = session, y = count, group = position)) +
  geom_point(aes(color = position, shape = position)) +
  geom_line(aes(color = position, lty = position)) +
  scale_color_manual(values = c("gray", "black")) +
  labs(x = NULL, y = "Count of Positions", 
       color = "Position", shape = "Position", lty = "Position") +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~state) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )

# Save plot
ggsave("Figures and Tables/Arizona and Illinois Data by Year.png",
       width = 10, height = 8)

### Compare Per Bill Mobilization ---------------------------------
# Create function to determine what share of bills reach each mobilization threshold
compute_threshold_shares <- function(positions, state_label,
                                     bill_var = "bill_name") {
  
  bill_counts <- positions |>
    count(!!sym(bill_var), name = "n_positions")
  
  total_bills <- nrow(bill_counts)
  
  thresholds <- c(10, 100, 1000, 10000)
  
  tibble(
    state     = state_label,
    threshold = thresholds,
    threshold_label = paste0("\u2265 ", format(thresholds, big.mark = ",")),
    pct       = map_dbl(thresholds, ~ mean(bill_counts$n_positions >= .x) * 100),
    n_bills   = map_int(thresholds, ~ sum(bill_counts$n_positions >= .x)),
    total     = total_bills
  )
}

# Calculate shares by state
shares_il <- compute_threshold_shares(positions_il, "Illinois")
shares_az <- compute_threshold_shares(positions_az, "Arizona")

# Combine into single dataframe
shares <- bind_rows(shares_il, shares_az) |>
  mutate(threshold_label = factor(threshold_label,
                                  levels = paste0("\u2265 ", format(
                                    c(10, 100, 1000, 10000), big.mark = ","))))

# Create plot
ggplot(shares, aes(x = threshold_label, y = pct, fill = state)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%\n(%s / %s)", pct,
                                format(n_bills, big.mark = ","),
                                format(total, big.mark = ","))),
            position = position_dodge(width = 0.7),
            vjust = -0.3, size = 3.2, lineheight = 0.85) +
  scale_fill_manual(values = c("Illinois" = "gray", "Arizona" = "black")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    x        = "Number of position records per bill",
    y        = "Percent of bills",
    fill     = NULL
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position  = "bottom",
    panel.grid.major.x = element_blank(),
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(color = "grey40")
  )

# Save plot
ggsave("Figures and Tables/Mobilization per Bill.png",
       width = 12, height = 6)

### Examine Participation by Group by Bill -------------------------------------
# Slips by the same group on the same bill
slips %>%
  filter(!(is.na(group_name))) %>%
  group_by(session, bill_name, group_name) %>%
  summarize(num_witnesses = n()) %>%
  arrange(desc(num_witnesses)) %>%
  view()
