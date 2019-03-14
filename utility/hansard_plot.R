# File: hansard_plot.R
# Description: Download hansard file and generate word cloud
# Author: Noel Dempsey
# Date: 2018-12-05

# Load data
hansard_cloud <- function(d, nam) {
# Load stop words
data(stop_words)
source(here("utility/hansard_words.R"))
words <- d %>% unnest_tokens(word, value) %>%
  filter(!word %in% stop_words$word) %>%
  filter(!word %in% mp_first_stop_words$word) %>%
  filter(!word %in% mp_last_stop_words$word) %>%
  filter(!word %in% parly_stop_words$word) %>%
  filter(!word %in% party_stop_words$word) %>%
  filter(!word %in% month_stop_words$word) %>%
  mutate(word=str_trim(str_replace_all(word, "[^[:alpha:]]", " "))) %>%
  filter(word != '') %>% unnest_tokens(word, value) %>%
  count(word, sort = TRUE)

# Top 100
data <- words[1:100,]

# Create word cloud
plot <- hansard_plot()

# Save file
ggsave(paste(nam, '.png', sep=''), width = 15.87, height = 8.86, units = "cm")

# Write sitting date
  # fileConn <- file("check.txt")
  # writeLines(hansard_date(), fileConn)
  # close(fileConn)
