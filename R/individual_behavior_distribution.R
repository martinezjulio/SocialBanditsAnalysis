library(here)
library(tidyverse)
library(janitor)
library(ggthemes)
library(stringr)
library(ggtext)

#' Create Individual Participant Behavior Distribution Plots
#'
#' Creates arm-pulling distribution plots for each participant showing how their
#' choices evolve across games and rounds.
#'
#' @param preprocessed_df Preprocessed data frame with columns: anonymous_id, 
#'   session_id, game_sequence, game_index, game, position, action
#' @param experiment_name Name of the experiment
#' @param version Version identifier
#' @param save Logical, whether to save the plots to disk
#' @return Invisible list of generated file paths
individual_behavior_distribution <- function(preprocessed_df, experiment_name, version, save = FALSE) {
  
  # Create the figure directory if saving
  if (save) {
    fig_dir <- here("figures", experiment_name, version)
    dist_dir <- here(fig_dir, "individual_distributions")
    if (!dir.exists(dist_dir)) {
      dir.create(dist_dir, recursive = TRUE)
    }
  } else {
    message("Note: save=FALSE, no files will be generated. Set save=TRUE to create individual plots.")
    return(invisible(NULL))
  }
  
  n_arms <- 12L
  
  # Helper function to parse zone_bandit string
  parse_zone_bandit <- function(s) {
    if (is.numeric(s) && length(s) > 1) return(as.numeric(s))
    s <- as.character(s[1])
    nums <- regmatches(
      s,
      gregexpr("-?\\d*\\.?\\d+(?:[eE][+-]?\\d+)?", s, perl = TRUE)
    )[[1]]
    as.numeric(nums)
  }
  
  # Map position (0-11) to payoff rank (1=worst, 12=best) based on zone_bandit
  # This ensures each game's positions are mapped to ranks based on that game's specific bandit configuration
  position_to_rank <- function(position, zone_bandit_str) {
    if (is.na(position) || is.na(zone_bandit_str) || zone_bandit_str == "") return(NA_real_)
    
    payoffs <- parse_zone_bandit(zone_bandit_str)
    if (length(payoffs) != 12) return(NA_real_)
    if (position < 0 || position > 11) return(NA_real_)
    
    # Rank arms: 1 = lowest payoff (worst), 12 = highest payoff (best)
    ranks <- rank(payoffs, ties.method = "average")
    ranks[position + 1]
  }
  
  # Helper function to compute rank frequency vector (instead of position frequencies)
  compute_rank_frequencies <- function(ranks) {
    freq <- table(factor(ranks, levels = 1:n_arms))
    as.numeric(freq)
  }
  
  # Map positions to ranks and compute rank frequencies per game for each participant-session
  game_rank_freqs <- preprocessed_df %>%
    filter(game != "choice", action == "shoot",
           !is.na(position), position >= 0, position < n_arms) %>%
    mutate(
      game_sequence_label = case_when(
        str_starts(game_sequence, "expert") ~ "Expert First Condition",
        str_starts(game_sequence, "novice") ~ "Novice First Condition",
        TRUE ~ game_sequence
      ),
      # Calculate game type based on position in round
      round_num = floor(game_index / 4) + 1,
      position_in_round = game_index %% 4,
      game_type_label = case_when(
        game_sequence_label == "Expert First Condition" & position_in_round == 0 ~ "Expert",
        game_sequence_label == "Expert First Condition" & position_in_round == 1 ~ "Participants",
        game_sequence_label == "Expert First Condition" & position_in_round == 2 ~ "Novice",
        game_sequence_label == "Expert First Condition" & position_in_round == 3 ~ "Participants",
        game_sequence_label == "Novice First Condition" & position_in_round == 0 ~ "Novice",
        game_sequence_label == "Novice First Condition" & position_in_round == 1 ~ "Participants",
        game_sequence_label == "Novice First Condition" & position_in_round == 2 ~ "Expert",
        game_sequence_label == "Novice First Condition" & position_in_round == 3 ~ "Participants",
        TRUE ~ game
      ),
      # Map each position to its rank within this game's bandit configuration
      arm_rank = purrr::map2_dbl(position, zone_bandit, position_to_rank)
    ) %>%
    filter(!is.na(arm_rank)) %>%
    group_by(anonymous_id, session_id, game_sequence_label, game_index, game, game_type_label) %>%
    summarise(
      rank_frequencies = list(compute_rank_frequencies(arm_rank)),
      .groups = "drop"
    )
  
  # Unnest to long format
  plot_data <- game_rank_freqs %>%
    mutate(rank = map(rank_frequencies, ~1:n_arms)) %>%
    unnest(c(rank, rank_frequencies)) %>%
    rename(count = rank_frequencies) %>%
    mutate(
      rank_label = case_when(
        rank == 1 ~ "0.0, **1**",
        rank == 2 ~ "0.10, **2**",
        rank == 3 ~ "0.20, **3**",
        rank == 4 ~ "0.30, **4**",
        rank == 5 ~ "0.35, **5**",
        rank == 6 ~ "0.40, **6**",
        rank == 7 ~ "0.45, **7**",
        rank == 8 ~ "0.50, **8**",
        rank == 9 ~ "0.55, **9**",
        rank == 10 ~ "0.60, **10**",
        rank == 11 ~ "0.75, **11**",
        rank == 12 ~ "<span style='color:#00CC00; font-weight:bold;'>0.80, 12</span>",
        TRUE ~ as.character(rank)
      ),
      game_sequence_label = factor(game_sequence_label, 
                                   levels = c("Expert First Condition", "Novice First Condition"))
    )
  
  # Create factor for proper ordering (1=worst to 12=best)
  plot_data <- plot_data %>%
    mutate(
      rank_factor = factor(rank, levels = 1:n_arms)
    )
  
  # Create named vector for rank labels (ensures correct order)
  rank_labels_vec <- c(
    "1" = "0.0, **1**",
    "2" = "0.10, **2**",
    "3" = "0.20, **3**",
    "4" = "0.30, **4**",
    "5" = "0.35, **5**",
    "6" = "0.40, **6**",
    "7" = "0.45, **7**",
    "8" = "0.50, **8**",
    "9" = "0.55, **9**",
    "10" = "0.60, **10**",
    "11" = "0.75, **11**",
    "12" = '<span style="color:#00CC00; font-weight:bold;">0.80, **12**</span>'
  )
  
  # Get unique participant-session combinations
  participants <- plot_data %>%
    distinct(anonymous_id, session_id, game_sequence_label) %>%
    arrange(anonymous_id, session_id)
  
  saved_files <- character(nrow(participants))
  
  # Helper function to clean session_id for filename
  session_label_from_id <- function(sid) {
    s <- as.character(sid)[1]
    s <- sub("^sid[_-]?", "", s)
    s <- sub(".*_sid[_-]?", "", s)
    s
  }
  
  # Create one plot per participant-session
  for (i in seq_len(nrow(participants))) {
    pid <- participants$anonymous_id[i]
    sid <- participants$session_id[i]
    condition <- participants$game_sequence_label[i]
    
    participant_data <- plot_data %>%
      filter(anonymous_id == pid, session_id == sid)
    
    p <- participant_data %>%
      ggplot(aes(x = rank_factor, y = count)) +
      geom_col(aes(fill = game_type_label), width = 0.8, alpha = 0.8) +
      scale_fill_manual(
        values = c(
          "Expert" = "#1f77b4",   # Blue for expert demonstrations
          "Novice" = "#d62728",   # Red for novice demonstrations
          "Play" = "#2ca02c"      # Green for participant play
        ),
        name = "Game Type"
      ) +
      facet_wrap(~ game_index, nrow = 2, ncol = 6,
                labeller = labeller(game_index = function(x) paste("Game", x))) +
      scale_x_discrete(labels = rank_labels_vec) +
      labs(
        title = paste0("Participant ", pid, " • Session ", session_label_from_id(sid), " - Arm Pull Distribution by Rank"),
        subtitle = paste0("Condition: ", condition, " | X-axis shows payoff/rank"),
        x = "Payoff / Rank (1=lowest, 12=highest)",
        y = "Pull Count"
      ) +
      theme_minimal(base_size = 10) +
      theme(
        strip.text = element_text(size = 8, face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 9, color = "gray40"),
        legend.position = "bottom",
        axis.text.x = element_markdown(size = 7, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 8),
        panel.background = element_rect(fill = "white", colour = "grey90"),
        plot.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(color = "grey95", linewidth = 0.3),
        panel.grid.minor = element_blank()
      )
    
    sess_lab <- session_label_from_id(sid)
    filename <- file.path(
      dist_dir,
      paste0("participant_", pid, "_session_", gsub("[^A-Za-z0-9_-]", "_", sess_lab), "_distribution.png")
    )
    
    ggsave(filename, plot = p, width = 16, height = 8, dpi = 200, bg = "white")
    saved_files[i] <- filename
    message("Saved: ", filename)
  }
  
  message(sprintf("\nGenerated %d individual distribution plots in: %s", 
                  length(saved_files), dist_dir))
  
  invisible(saved_files)
}

