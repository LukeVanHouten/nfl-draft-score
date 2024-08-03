library(tidyverse)
library(stringdist)

draft_df <- read.csv("drafts.csv")

log_bias <- exp(1) - 1

draft_scores <- function(years, top_teams, transformation) {
    if (all(years %in% 1936:2024)) {
        years <- years
    } else if (years == "all") {
        years <- 1936:2024
    } else {
        return(gsub("\\\n", "", "Years must be within the 
 years 1936-2024, or 'all' for all of these years"))
    }
    
    draft_picks <- draft_df %>%
        select(paste0("X", as.character(years))) %>%
        mutate(pick = rownames(.)) %>%
        pivot_longer(cols = colnames(select(., -pick)), names_to = "year") %>%
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
    
    score_df <- as.data.frame(sort(apply(pivot_df, 2, tukey), 
                                   decreasing = TRUE))
    colnames(score_df) <- "score"
    scores_df <- subset(score_df, !(rownames(score_df) %in% "Unknown"))
    
    if (is.numeric(top_teams)) {
        scores_df <- head(scores_df, top_teams)
        top_teams <- paste("Top", top_teams)
    } else if (top_teams == "all") {
        top_teams <- ""
    } else {
        return(gsub("\\\n", "", "The number of teams must either be numeric or
 'all' for all teams"))
    }
    
    if (length(years) == 1) {
        year_range <- years
        draft <- "NFL Draft"
    } else {
        year_range <- paste0(min(years), "-", max(years))
        draft <- "NFL Drafts"
    }
    
    p <- ggplot(data = scores_df, 
                aes(x = reorder(rownames(scores_df), -score), y = score)) +
        geom_col(fill = "purple", color = "gold") +
        geom_text(aes(label = round(score, 4)), color = "gold",
                  position = position_dodge(width = 0.9), angle = 90, 
                  hjust = 1.2, vjust = 0.5) +
        labs(title = paste0(top_teams, " ", str_to_title(transformation), 
                           "-Transformed College Draft Scores in the", " ",
                           year_range, " ", draft), x = "School", y = "Score") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), 
              legend.position = "none")
    return(p)
}

scores <- draft_scores(years = "all", top_teams = 50, 
                       transformation = "log")
scores

scores_df <- scores$plot_env$scores_df
