library(tidyverse)
library(plotly)
options(dplyr.summarise.inform = FALSE)

draft_df <- read.csv("drafts.csv")
top_50_df <- read.csv("top_50_seasons_jbp.csv")
fbs_ranking_df <- read.csv("fbs_ranking_jbp.csv") %>%
    mutate(jbp_score = (jbp_score / 1000) + 12.6)

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
    
    scores_df <- cbind(rownames(scores_df), scores_df) %>%
        `colnames<-`(c("school", "score")) %>%
        `rownames<-`(1:nrow(scores_df))
    
    scores_plot <- ggplot(data = scores_df, aes(x = reorder(school, -score), 
                                                y = score)) +
        geom_col(fill = "purple", color = "gold") +
        geom_text(aes(label = round(score, 4)), color = "gold",
                  position = position_dodge(width = 0.9), angle = 90, 
                  hjust = 1.2, vjust = 0.5) +
        labs(title = paste0(top_teams, " ", str_to_title(transformation), 
                           "-Transformed College Draft Scores in the ",
                           year_range, " ", draft), x = "School", y = "Score") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), 
              legend.position = "none")
    return(scores_plot)
}

scores <- draft_scores(years = "all", top_teams = 50, 
                       transformation = "log")
scores

draft_scores_df <- draft_scores(years = "all", top_teams = "all", 
                                transformation = "log")$plot_env$scores_df

get_schools <- function(school, years, transformation) {
    if (all(years %in% 1936:2024)) {
        years <- years
    } else if (years == "all") {
        years <- 1936:2024
    } else {
        return(gsub("\\\n", "", "Years must be within the 
 years 1936-2024, or 'all' for all of these years"))
    }
    
    if (!(school %in% draft_scores_df$school)) {
        return("School must be a valid team")
    }

    year_scores <- as.data.frame(t(sapply(years, function(x) {
        draft_scores(years = x, top_teams = "all", 
                     transformation = transformation)$plot_env$scores_df
    })))
    indices <- lapply(year_scores$school, function(y) {
        idx <- which(y == school)
        if (length(idx) == 0) NA else idx
    })
    school_scores <- unlist(Map(function(s, i) ifelse(is.na(i), 0, s[i]), 
                                year_scores$score, indices))
    schools_df <- as.data.frame(cbind(years, school_scores))

    if (length(years) == 1) {
        year_range <- years
        draft <- "NFL Draft"
    } else {
        year_range <- paste0(min(years), "-", max(years))
        draft <- "NFL Drafts"
    }
    
    school_plot <- ggplot(data = schools_df, aes(x = years,
                                                 y = school_scores)) +
        geom_col(fill = "purple", color = "gold") +
        geom_smooth(method = "lm", formula = y ~ x, se = FALSE, 
                    color = "black") +
        geom_text(aes(label = round(school_scores, 4)), color = "gold",
                  position = position_dodge(width = 0.9), angle = 90,
                  hjust = 1.2, vjust = 0.5) +
        scale_x_continuous(breaks = years) + 
        scale_y_continuous(limits = c(0, 4.5)) + 
        labs(title = paste0(school, "'s ", str_to_title(transformation), 
                            "-Transformed College Draft Scores in the ", 
                            year_range, " ", draft), x = "Years", y = "Score") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none")
    return(school_plot)
}

schools <- get_schools(school = "Clemson", years = 2012:2021,
                       transformation = "log")
schools

drafts_top_50 <- function(transformation) {
    if (!(transformation %in% c("reciprocal", "root", "log"))) {
        return("Transformation must be 'recriprocal', 'root', or 'log'!")
    } else {
        return(cbind(select(top_50_df, school, jbp_score, Conference), 
                            t(mapply(function(x, y) {as.vector(
                                get_schools(school = x, years = y, 
                                            transformation = 
                                            transformation)$plot_env$schools_df
                                )}, top_50_df$school, top_50_df$year + 1))) %>%
                   mutate(draft_score = round(as.numeric(school_scores), 3), 
                          team = paste(as.numeric(years) - 1, school), 
                          draft = paste(years, "Draft")) %>%
                   add_column(func = transformation, .before = 1) %>%
                   select(-school_scores) %>%
                   `rownames<-`(1:nrow(.)))
    }
}

top_drafts_df <- drafts_top_50("log")
top_50_model <- lm(draft_score ~ jbp_score, top_drafts_df)
summary(top_50_model)

top_50_plot <- ggplot(data = top_drafts_df, 
                      aes(x = jbp_score, y = draft_score)) +
    suppressWarnings(geom_point(aes(text = paste("Team:", team, "<br>Draft:", 
                                                 draft), color = Conference))) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
    labs(title = paste0("Top 50 College Football Seasons Vs. ", 
                        str_to_title(top_drafts_df[[1, 1]]), 
                        "-Transformed Draft Scores"), x = "Season Score", 
         y = "Draft Score")
ggplotly(top_50_plot)

drafts_fbs_ranking <- function(transformation) {
    return(left_join(fbs_ranking_df, draft_scores(
                        years = 1984:2023, top_teams = "all", 
                        transformation = transformation
                     )$plot_env$scores_df, by = "school") %>%
               mutate(draft_score = round(score, 4)) %>%
               add_column(func = transformation, .before = 1) %>%
               select(-score))
}

fbs_ranking_df <- drafts_fbs_ranking("log")
drafts_fbs_model <- lm(draft_score ~ jbp_score, fbs_ranking_df)
summary(drafts_fbs_model)

drafts_fbs_plot <- ggplot(data = fbs_ranking_df, 
                          aes(x = jbp_score, y = draft_score)) +
    geom_point(aes(group = school)) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
    geom_hline(yintercept = 0, color = "black") +
    labs(title = paste0("1983-2022 Team Scores Vs. ", 
                        str_to_title(fbs_ranking_df[[1, 1]]), 
                        "-Transformed Draft Scores"), x = "Team Score", 
         y = "Draft Score")
ggplotly(drafts_fbs_plot)
