library(tidyverse)

file_list <- list.files(path = "Drafts", patter = "\\.csv$", 
                        full.names = TRUE)

combined_data <- do.call(rbind, lapply(str_sort(file_list, numeric = TRUE),
                                       read.csv))
combined_data[1, 28] <- "a"
colnames(combined_data) <- combined_data[1, ]

picks_df <- combined_data %>%
    select(Pick, "College/Univ") %>%
    mutate_all(~ ifelse(. == "", "Unknown", .))
colnames(picks_df) <- c("Picks", "College")

years <- 1936:1969

combined_df <- select(picks_df, Picks) %>%
    mutate(group = cumsum(Picks == "Pick")) %>%
    group_by(group) %>%
    mutate(Years = if_else(Picks == "Pick", NA_character_, 
                           as.character(years[group[1]])),
           Years = zoo::na.locf(Years, na.rm = FALSE)) %>%
    ungroup() %>%
    select(-group)

years_df <- cbind(combined_df, picks_df) %>%
    select(Years, College) %>%
    drop_na() %>%
    pivot_wider(names_from = Years, values_from = College, values_fn = list)

old_picks <- years_df %>%
    mutate(across(everything(), ~ map(.x, ~ {
        length(.x) <- max(sapply(years_df[1, ], function(x) length(unlist(x))))
        .x}))) %>%
    unnest(cols = everything())

write.csv(old_picks, "nfl_draft_1936-1969.csv", row.names = FALSE)
