library(tidyverse)

picks_1936_1969 <- read.csv("nfl_draft_1936-1969.csv")
picks_1970_2021 <- read.csv("nfl_draft_1970-2021.csv")
picks_2022_2024 <- read.csv("nfl_draft_2022-2024.csv")

picks_df <- picks_1970_2021 %>%
    select(year, college) %>%
    pivot_wider(names_from = year, values_from = college, values_fn = list)

old_picks <- picks_df %>%
    mutate(across(everything(), ~ map(.x, ~ {
        length(.x) <- max(sapply(picks_df[1, ], function(x) length(unlist(x))))
        .x}))) %>%
    unnest(cols = everything())

newish_picks <- cbind(old_picks, picks_2022_2024[seq_len(nrow(old_picks)), ,
                                                 drop = FALSE])
all_picks <- cbind(picks_1936_1969, newish_picks[seq_len(nrow(picks_1936_1969)),
                                                 , drop = FALSE])
colnames(all_picks) <- 1936:2024

write.csv(all_picks, "drafts.csv", row.names = FALSE)

