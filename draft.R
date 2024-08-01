library(tidyverse)

draft_df <- read.csv("2022-2024-draft.csv")

log_bias <- exp(1) - 1

draft_scores <- function(years, num_teams, transformation) {
    draft_picks <- draft_df %>%
        select(paste0("X", as.character(years))) %>%
        mutate(pick = rownames(.)) %>%
        pivot_longer(cols = colnames(select(., -pick)), 
                     names_to = "year") %>%
        group_by(value, year) %>%
        summarize(pick = list(pick)) %>%
        pivot_wider(names_from = year, values_from = pick) %>%
        filter(value != "") %>%
        rowwise() %>%
        mutate(picks = list(c_across(everything()))) %>%
        ungroup() %>%
        select(value, picks)
    
    pivot_df <- t(draft_picks$picks)
    colnames(pivot_df) <- draft_picks$value
    
    if (transformation == "reciprocal") {
        tukey <- function(x) {
            sum(1 / as.numeric(unlist(x)))
        }
    } else if (transformation == "root") {
        tukey <- function(x) {
            sum(1 / sqrt(as.numeric(unlist(x))))
        }
    } else if (transformation == "log") {
        tukey <- function(x) {
            sum(1 / log(as.numeric(unlist(x)) + log_bias))
        }
    } else {
        return("Transformations must be either 'reciprocal', 'root', or 'log'")
    }
    
    scores_df <- as.data.frame(sort(apply(pivot_df, 2, tukey), 
                                    decreasing = TRUE))
    colnames(scores_df) <- "score"
    
    if (is.numeric(num_teams)) {
        scores_df <- head(scores_df, num_teams)
        top_teams <- paste("Top", num_teams)
    } else if (num_teams == "all") {
        top_teams <- ""
    } else {
        return(gsub("\\\n", "", "The number of teams must either be numeric or
 'all' for all teams"))
    }
    
    p <- ggplot(data = scores_df, 
           aes(x = reorder(rownames(scores_df), -score), y = score)) +
        geom_col(fill = "purple", color = "gold") +
        geom_text(aes(label = round(score, 4)), 
                  position = position_dodge(width = 0.9), 
                  angle = 45, hjust = -0.1, vjust = -0.3) +
        labs(title = paste(top_teams, str_to_title(transformation), 
                           "Transformed College Draft Scores in the",
                           paste(years, collapse = ", "), "NFL Draft"), 
             x = "School", y = "Score") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), 
              legend.position = "none")
    return(p)
}

scores <- draft_scores(2024, "all", "log")
scores
