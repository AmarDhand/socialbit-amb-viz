# ---- Intro ----
# Parsing and visualizing SocialBit Ambassador data
# August 21, 2025
# Amar Dhand with ChatGPT drafting

# ---- Setup ----
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(purrr)
})

# Working directory  <-- OPTIONAL: EDIT FOR YOUR COMPUTER (or leave commented)
# setwd("~/path/to/your/project")

# Path to your CSV   <-- EDIT THIS to your file name/path
path <- "sample_data.csv"

# ---- 1) Read + convert data types ----
raw <- readr::read_csv(path, show_col_types = FALSE)

dat <- raw %>%
  rename(local_time_raw = localTime) %>%
  mutate(
    # Coerce base types
    batteryLevel      = as.integer(batteryLevel),
    prediction        = as.integer(prediction),
    predictionScore   = as.numeric(predictionScore),
    across(c(modelName, deviceID, userID, version), as.character),
    
    # Logical-ish columns
    onWrist = if (is.logical(onWrist)) onWrist else
      tolower(as.character(onWrist)) %in% c("true","t","1"),
    hasRecentHeartRate = dplyr::if_else(
      tolower(as.character(hasRecentHeartRate)) %in% c("true","t","1"),
      TRUE, FALSE, missing = NA
    ),
    
    # Time handling
    timestamp = as.numeric(timestamp),
    time_utc  = as_datetime(timestamp / 1000, tz = "UTC"),
    local_time = suppressWarnings(ymd_hms(local_time_raw)),  # has offset like -0400
    time_local = with_tz(time_utc, tzone = "America/New_York"),
    date_local = as_date(time_local),
    
    # Social flag
    social = prediction == 1
  ) %>%
  arrange(time_utc)

# ---- Robust per-row duration (handles irregular sampling) ----
# Duration = time until next sample, capped at 60s (typical 1-min cadence).
dat <- dat %>%
  mutate(
    next_time  = lead(time_utc),
    dur_sec_raw = as.numeric(difftime(next_time, time_utc, units = "secs")),
    dur_sec     = pmin(replace_na(dur_sec_raw, 60), 60),
    dur_min     = dur_sec / 60
  )

# ---- 2) Daily usage & social interaction minutes ----
daily <- dat %>%
  summarise(
    usage_min  = sum(if_else(onWrist, dur_min, 0), na.rm = TRUE),
    social_min = sum(if_else(social,  dur_min, 0), na.rm = TRUE),
    .by = date_local
  ) %>%
  arrange(date_local)

# For x-axis: show every date
all_dates <- sort(unique(daily$date_local))

# ---- Output folder for all PNGs (p1, p2, p3 daily) ----
out_dir <- "plots_png"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- p1: Daily on-wrist usage and social interaction (lines) ----
daily_long <- daily %>%
  pivot_longer(cols = c(usage_min, social_min),
               names_to = "metric", values_to = "minutes") %>%
  mutate(metric = recode(metric,
                         usage_min  = "On-wrist usage (min)",
                         social_min = "Social interaction (min)"))

# Means per metric (usage vs. social)
means_by_metric <- daily_long %>%
  group_by(metric) %>%
  summarise(mean_minutes = mean(minutes, na.rm = TRUE), .groups = "drop")

p1 <- ggplot(daily_long, aes(x = date_local, y = minutes, color = metric, group = metric)) +
  geom_hline(
    data = means_by_metric,
    aes(yintercept = mean_minutes, color = metric),
    linewidth = 1,
    alpha = 0.35
  ) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_x_date(breaks = all_dates, date_labels = "%Y-%m-%d") +
  labs(
    x = "Date",
    y = "Minutes per day",
    color = NULL,
    title = "Daily on-wrist usage and social interaction"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
  )

print(p1)
ggsave(filename = file.path(out_dir, "p1_daily_usage_social.png"),
       plot = p1, width = 10, height = 4, dpi = 200, bg = "white")

# ---- p2: Daily social interaction bar chart with threshold coloring ----
daily_social <- daily %>%
  mutate(threshold = if_else(social_min >= 380, "≥380 minutes", "<380 minutes"))

p2 <- ggplot(daily_social, aes(x = date_local, y = social_min, fill = threshold)) +
  geom_col() +
  scale_fill_manual(values = c("≥380 minutes" = "blue", "<380 minutes" = "pink")) +
  scale_x_date(breaks = all_dates, date_labels = "%Y-%m-%d") +
  labs(
    x = "Date",
    y = "Total social interaction (minutes)",
    fill = NULL,
    title = "Daily social interaction minutes with goal set at 380 minutes (6.3 hours)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
  )

print(p2)
ggsave(filename = file.path(out_dir, "p2_daily_social_threshold.png"),
       plot = p2, width = 10, height = 4, dpi = 200, bg = "white")

# ======================================================================
# ---- p3: Within-day timeline of social vs. no social (minute resolution)
#          Loop across ALL days and save a PNG for each day
#          *Caps each row to ≤1 minute*
#          *Sets tile width = 1 to avoid bridging gaps*
#          *Clips x-axis to observed minutes only (with small padding)*
#          *Adds extra plot margins and white background*
# ======================================================================

make_p3_for_date <- function(focus_date) {
  df_day <- dat %>%
    filter(date_local == focus_date) %>%
    mutate(
      next_time_local = with_tz(next_time, tzone = "America/New_York"),
      cap_end         = time_local + minutes(1),
      interval_end    = if_else(is.na(next_time_local), cap_end,
                                pmin(next_time_local, cap_end)),
      start_minute    = floor_date(time_local, unit = "minute"),
      end_minute      = floor_date(interval_end - seconds(1), unit = "minute"),
      state           = if_else(social, "Social", "No social"),
      # Robust sequence expansion (avoids seq() errors)
      minute_seq = map2(start_minute, end_minute, ~{
        if (is.na(.x) || is.na(.y) || .y < .x)
          as.POSIXct(character(), tz = "America/New_York")
        else
          seq(.x, .y, by = "1 min")
      })
    ) %>%
    select(minute_seq, state) %>%
    tidyr::unnest(minute_seq) %>%
    group_by(minute_seq) %>%
    summarise(state = if_else(any(state == "Social"), "Social", "No social"),
              .groups = "drop") %>%
    mutate(minute_of_day = hour(minute_seq) * 60 + minute(minute_seq)) %>%
    arrange(minute_seq)
  
  if (nrow(df_day) == 0) return(invisible(NULL))
  
  mins_range  <- range(df_day$minute_of_day, na.rm = TRUE)
  start_break <- floor(mins_range[1] / 60) * 60
  end_break   <- ceiling(mins_range[2] / 60) * 60
  hour_breaks <- seq(start_break, end_break, by = 60)
  if (length(hour_breaks) < 2) hour_breaks <- unique(c(start_break, end_break))
  
  p3 <- ggplot(df_day, aes(x = minute_of_day, y = 1, fill = state)) +
    geom_tile(height = 1, width = 1) +
    scale_x_continuous(
      breaks = hour_breaks,
      labels = function(x) sprintf("%02d:00", x / 60),
      expand = expansion(add = c(1, 1))  # pad by 1 minute on both sides
    ) +
    labs(
      x = "Time of day",
      y = NULL,
      fill = NULL,
      title = paste("Within-day timeline of social vs. no social (", as.character(focus_date), ")", sep = "")
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(5.5, 5.5, 5.5, 10.5),
    ) +
    coord_cartesian(clip = "off")
  
  ggsave(
    filename = file.path(out_dir, paste0("p3_", as.character(focus_date), ".png")),
    plot = p3, width = 10, height = 3, dpi = 200, bg = "white"
  )
  invisible(p3)
}

unique_dates <- sort(unique(dat$date_local))
invisible(lapply(unique_dates, make_p3_for_date))
# End
