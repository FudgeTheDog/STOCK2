
# Portfolio History Tracking -----------------------------------------------
# Appends a daily snapshot of total cost vs total worth to a CSV,
# then provides a plotting function to visualise the history over time.

# Resolve the directory whether sourced from a file or run from the console
.history_dir <- tryCatch({
  p <- rstudioapi::getSourceEditorContext()$path
  if (nchar(p) > 0) dirname(p) else getwd()
}, error = function(e) getwd())

HISTORY_FILE <- file.path(.history_dir, "portfolio_history.csv")

# record_portfolio_snapshot() ------------------------------------------------
# Call this once per day (after portfolio_summary has been computed).
# Safe to call multiple times — only one row per date is ever written.

record_portfolio_snapshot <- function() {
  if (!exists("portfolio_summary")) {
    stop("portfolio_summary not found. Run Connor_Sharesies_Source.R first.")
  }

  today     <- Sys.Date()
  new_row   <- tibble(
    date        = today,
    total_cost  = portfolio_summary$total_cost,
    total_worth = portfolio_summary$current_value
  )

  if (file.exists(HISTORY_FILE)) {
    history <- read_csv(HISTORY_FILE, show_col_types = FALSE) %>% 
      mutate(date = as.Date(date))

    # Only append if today isn't already recorded
    if (today %in% history$date) {
      message("Snapshot for ", today, " already exists — skipping.")
      return(invisible(history))
    }
    history <- bind_rows(history, new_row)
  } else {
    history <- new_row
  }

  write_csv(history, HISTORY_FILE)
  message("Snapshot recorded: ", today,
          " | Cost: $", round(new_row$total_cost, 2),
          " | Worth: $", round(new_row$total_worth, 2))
  invisible(history)
}

# update_portfolio_snapshot() ------------------------------------------------
# Like record_portfolio_snapshot(), but overwrites today's row if it exists.
# Use this when markets haven't closed yet and you want to refresh today's values.

update_portfolio_snapshot <- function() {
  if (!exists("portfolio_summary")) {
    stop("portfolio_summary not found. Run Connor_Sharesies_Source.R first.")
  }

  today   <- Sys.Date()
  new_row <- tibble(
    date        = today,
    total_cost  = portfolio_summary$total_cost,
    total_worth = portfolio_summary$current_value
  )

  if (file.exists(HISTORY_FILE)) {
    history <- read_csv(HISTORY_FILE, show_col_types = FALSE) %>%
      mutate(date = as.Date(date)) %>%
      filter(date != today)          # Remove today's row if it exists
    history <- bind_rows(history, new_row) %>% arrange(date)
  } else {
    history <- new_row
  }

  write_csv(history, HISTORY_FILE)
  message("Snapshot updated: ", today,
          " | Cost: $", round(new_row$total_cost, 2),
          " | Worth: $", round(new_row$total_worth, 2))
  invisible(history)
}

# plot_portfolio_history() ---------------------------------------------------
# Reads the history CSV and plots cost vs worth over time.

plot_portfolio_history <- function() {
  if (!file.exists(HISTORY_FILE)) {
    stop("No history file found. Run record_portfolio_snapshot() first.")
  }

  raw <- read_csv(HISTORY_FILE, show_col_types = FALSE) %>%
    mutate(date = as.Date(date)) %>%
    arrange(date)

  # Latest percentage difference between worth and cost
  latest_raw  <- slice_tail(raw, n = 1)
  pct_diff    <- (latest_raw$total_worth - latest_raw$total_cost) / latest_raw$total_cost * 100
  pct_label   <- sprintf("%+.2f%%", pct_diff)
  pct_colour  <- if (pct_diff >= 0) "#2ecc71" else "#e74c3c"

  history <- raw %>%
    pivot_longer(cols = c(total_cost, total_worth),
                 names_to  = "series",
                 values_to = "value") %>%
    mutate(series = dplyr::recode(series,
                                  "total_cost"  = "Cost",
                                  "total_worth" = "Worth"))

  latest <- history %>%
    filter(date == max(date))

  ggplot(history, aes(x = date, y = value, colour = series)) +
    geom_line(linewidth = 1.1) +
    geom_point(data = latest, size = 2.5) +
    annotate("label",
             x     = min(raw$date),
             y     = Inf,
             label = paste0("Return: ", pct_label),
             vjust = 1.5, hjust = -0.1,
             colour = pct_colour, fill = "white",
             fontface = "bold", size = 4.5) +
    scale_colour_manual(values = c("Cost" = "#8888aa", "Worth" = "#2ecc71")) +
    scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b '%y") +
    labs(
      title    = "Connor's Portfolio — Cost vs Worth Over Time",
      subtitle = paste0("As of ", max(raw$date)),
      x        = NULL,
      y        = "Value (USD)",
      colour   = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold"),
      legend.position  = "top",
      axis.text.x      = element_text(angle = 30, hjust = 1),
      panel.grid.minor = element_blank()
    )
}

