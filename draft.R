library(tidyverse)

draft_df <- read.csv("2022-2024-draft.csv")
# draft <- read.csv("draft.csv")

rounds <- 1:7
years <- 2024
log_bias <- 1

# draft_df <- draft %>%
#     filter(Round %in% rounds) %>%
#     select(-Round)

draft_df <- draft_df %>%
    select(paste0("X", as.character(years)))

draft_picks <- draft_df %>%
    mutate(pick = rownames(draft_df))

combined_df <- draft_picks %>%
    pivot_longer(cols = colnames(select(draft_picks, -pick)), 
                 names_to = "year") %>%
    group_by(value, year) %>%
    summarize(pick = list(pick)) %>%
    pivot_wider(names_from = year, values_from = pick) %>%
    filter(value != "") %>%
    rowwise() %>%
    mutate(picks = list(c_across(everything()))) %>%
    ungroup() %>%
    select(value, picks)

pivot_df <- t(combined_df$picks)
colnames(pivot_df) <- combined_df$value

draft_scores <- as.data.frame(sort(apply(pivot_df, 2, function(x){
    sum(1 / as.numeric(unlist(x)))
}), decreasing = TRUE))

draft_scores_sqrt <- as.data.frame(sort(apply(pivot_df, 2, function(x){
    sum(1 / sqrt(as.numeric(unlist(x))))
}), decreasing = TRUE))

draft_scores_log <- as.data.frame(sort(apply(pivot_df, 2, function(x){
    sum(1 / log(as.numeric(unlist(x)) + log_bias))
}), decreasing = TRUE))

colnames(draft_scores)[1] <- "score"
colnames(draft_scores_sqrt)[1] <- "score"
colnames(draft_scores_log)[1] <- "score"

# draft_scores$score <- draft_scores$score / max(draft_scores$score)
# draft_scores_sqrt$score <- draft_scores_sqrt$score /
#     max(draft_scores_sqrt$score)
# draft_scores_log$score <- draft_scores_log$score / max(draft_scores_log$score)

p <- ggplot(data = draft_scores, 
            aes(x = reorder(rownames(draft_scores), -score), y = score)) +
    geom_col(fill = "purple", color = "gold") +
    geom_text(aes(label = round(score, 4)), 
              position = position_dodge(width = 0.9), 
              angle = 45, hjust = -0.1, vjust = -0.3) +
    labs(title = paste("College Draft Scores in Round", 
                       paste(rounds, collapse = ", "), "of the", 
                       paste(years, collapse = ", "), "NFL Draft"), 
         x = "School", y = "Score") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "none")

p_sqrt <- ggplot(data = draft_scores_sqrt, 
                 aes(x = reorder(rownames(draft_scores_sqrt), -score),  
                     y = score)) +
    geom_col(fill = "purple", color = "gold") +
    geom_text(aes(label = round(score, 4)), 
              position = position_dodge(width = 0.9), 
              angle = 45, hjust = -0.1, vjust = -0.3) +
    labs(title = paste("Square Root Transformed College Draft Scores in Round", 
                       paste(rounds, collapse = ", "), "of the", 
                       paste(years, collapse = ", "), "NFL Draft"),  
         x = "School", y = "Score") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "none")

p_log <- ggplot(data = draft_scores_log, 
                aes(x = reorder(rownames(draft_scores_log), -score),
                    y = score)) +
    geom_col(fill = "purple", color = "gold") +
    geom_text(aes(label = round(score, 4)), 
              position = position_dodge(width = 0.9), 
              angle = 45, hjust = -0.1, vjust = -0.3) +
    labs(title = paste("Log Transformed College Draft Scores in Round", 
                       paste(rounds, collapse = ", "), "of the", 
                       paste(years, collapse = ", "), "NFL Draft"), 
         x = "School", y = "Score") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "none")

p
p_sqrt
p_log
