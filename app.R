renv::snapshot(force = TRUE)

#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("readr")
#install.packages("scales")
#install.packages("forcats")
#install.packages("ggpattern")
#install.packages("remotes")
#remotes::install_github("hrbrmstr/waffle")
#install.packages("patchwork")
#install.packages("camcorder")
#install.packages("showtext")
#install.packages("ggtext")
#install.packages("sysfonts")
#install.packages("stringr")
#install.packages("packcircles")
#install.packages("shiny")
#install.packages("plotly")
#install.packages("bslib")
#install.packages("gganimate")
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(scales)
library(forcats)
library(stringr)
library(grid)
library(shiny)
library(plotly)
library(bslib)
library(gganimate)
library(rsconnect)


renv::install("hrbrmstr/waffle")

# Update your lockfile
renv::snapshot()

library(waffle)

rsconnect::writeManifest()

`%||%` <- function(x, y) if (is.null(x)) y else x

# Data
census_16_metro <- read.csv("census_16_metro_trim.csv")
census_16_other <- read.csv("census_16_other_trim.csv")
census_21_metro <- read.csv("census_21_metro_trim.csv")
census_21_other <- read.csv("census_21_other_trim.csv")

# Run this with trim function in next chunk and then change data to trimmed files and comment this and trim part
# census_16_metro <- read.csv("census_16_metro.csv")
# census_16_other <- read.csv("census_16_other.csv")
# census_21_metro <- read.csv("census_21_metro.csv")
# census_21_other <- read.csv("census_21_other.csv")

harmonize_census_names <- function(df) {
  nm <- names(df)
  swap <- function(old, new) {
    if (old %in% nm && !(new %in% nm)) nm[nm == old] <<- new
  }
  swap("TENUR", "Tenur")
  swap("BEDRM", "BedRm")
  swap("ROOM",  "ROOMS")
  swap("Value", "VALUE")
  swap("VISMIN","VisMin")
  swap("SUBSIDY","Subsidy")
  swap("PRESMORTG","PresMortG")
  swap("MARSTH","MarStH")
  swap("Gender","Sex")
  names(df) <- nm
  df
}

census_16_metro <- harmonize_census_names(census_16_metro)
census_16_other <- harmonize_census_names(census_16_other)
census_21_metro <- harmonize_census_names(census_21_metro)
census_21_other <- harmonize_census_names(census_21_other)

keep_vars <- c(
  "EFDecile","SHELCO","HHInc_AT","WEIGHT","Tenur","WRKACT","LFACT","CMA",
  "PR","TotInc_AT","HDGREE","AGEGRP","CFSTAT","COW","NAICS","DPGRSUM"
)

# write_trimmed <- function(df, out_file) {
#   df_small <- df %>% select(any_of(keep_vars))
#   write.csv(df_small, out_file, row.names = FALSE)
# }
# 
# write_trimmed(census_16_metro, "census_16_metro_trim.csv")
# write_trimmed(census_16_other, "census_16_other_trim.csv")
# write_trimmed(census_21_metro, "census_21_metro_trim.csv")
# write_trimmed(census_21_other, "census_21_other_trim.csv")

census_16         <- census_16_metro %>% select(all_of(keep_vars)) %>% mutate(Year = 2016)
census_21         <- census_21_metro %>% select(all_of(keep_vars)) %>% mutate(Year = 2021)
census_16_other_s <- census_16_other %>% select(all_of(keep_vars)) %>% mutate(Year = 2016)
census_21_other_s <- census_21_other %>% select(all_of(keep_vars)) %>% mutate(Year = 2021)

census <- bind_rows(census_16, census_21, census_16_other_s, census_21_other_s)

census <- census %>%
  mutate(
    TotInc_AT = as.numeric(TotInc_AT),
    SHELCO = as.numeric(SHELCO),
    WEIGHT = as.numeric(WEIGHT),
    CMA = as.numeric(CMA),
    EFDecile = as.numeric(EFDecile),
    Tenur = as.numeric(Tenur),
    HDGREE = as.numeric(HDGREE),
    HHInc_AT = as.numeric(HHInc_AT),
    NAICS = as.numeric(NAICS),
    DPGRSUM = as.numeric(DPGRSUM)
  )

census <- census %>%
  filter(
    !TotInc_AT %in% c(1, -1, 99999999, 88888888),
    TotInc_AT > 0,
    !is.na(TotInc_AT)
  ) %>%
  mutate(
    SHELCO = if_else(SHELCO <= 0, NA_real_, SHELCO),
    # STIR = (monthly shelter cost * 12) / annual after-tax income
    STIR = (SHELCO * 12) / TotInc_AT,
    CMAS = case_when(
      CMA == 535 ~ "Toronto",
      CMA == 462 ~ "Montréal",
      CMA == 933 ~ "Vancouver",
      CMA == 505 ~ "Ottawa–Gatineau",
      CMA == 825 ~ "Calgary",
      CMA == 835 ~ "Edmonton",
      CMA == 602 ~ "Winnipeg",
      CMA == 421 ~ "Québec",
      CMA == 537 ~ "Hamilton",
      CMA == 541 ~ "Waterloo",
      TRUE ~ NA_character_
    )
  )

census2 <- census %>%
  mutate(
    Year = as.integer(Year),
    STIR = as.numeric(STIR),
    
    TENURE = case_when(
      Tenur == 2 ~ "Renters",
      Tenur == 1 ~ "Owners",
      Tenur == 8 ~ NA_character_,
      TRUE ~ NA_character_
    ),
    TENURE = factor(TENURE, levels = c("Renters", "Owners")),
    
    EDU = case_when(
      HDGREE %in% c(1, 99)        ~ "No formal education",
      HDGREE == 2                 ~ "High School (secondary)",
      HDGREE %in% c(3,4,5,6,7)    ~ "CEGEP (2 Years)",
      HDGREE %in% c(8,9,10)       ~ "University (Undergrad)",
      HDGREE %in% c(11,12,13)     ~ "Graduate School (Masters)",
      HDGREE == 88                ~ NA_character_,
      TRUE ~ NA_character_
    ),
    EDU = factor(
      EDU,
      levels = c(
        "No formal education",
        "High School (secondary)",
        "CEGEP (2 Years)",
        "University (Undergrad)",
        "Graduate School (Masters)"
      )
    ),
    
    INCOME = case_when(
      HHInc_AT %in% 1:8   ~ "Under $20,000",
      HHInc_AT %in% 9:12  ~ "$20,000 to $39,999",
      HHInc_AT %in% 13:16 ~ "$40,000 to $59,999",
      HHInc_AT %in% 17:20 ~ "$60,000 to $79,999",
      HHInc_AT %in% 21:24 ~ "$80,000 to $99,999",
      HHInc_AT %in% 25:28 ~ "$100,000 to $149,999",
      HHInc_AT %in% 29:30 ~ "$150,000 to $199,999",
      HHInc_AT %in% 31:32 ~ "$200,000 and over",
      HHInc_AT == 88      ~ NA_character_,
      TRUE                ~ NA_character_
    ),
    INCOME = factor(
      INCOME,
      levels = c(
        "Under $20,000",
        "$20,000 to $39,999",
        "$40,000 to $59,999",
        "$60,000 to $79,999",
        "$80,000 to $99,999",
        "$100,000 to $149,999",
        "$150,000 to $199,999",
        "$200,000 and over"
      )
    ),
    
    INDUSTRY_GROUP = case_when(
      NAICS %in% c(1, 2, 11, 21) ~ "Agriculture & Resource Extraction",
      NAICS %in% c(3, 4, 22, 23) ~ "Utilities & Construction",
      NAICS %in% c(5, 31, 32, 33) ~ "Manufacturing",
      NAICS %in% c(6, 7, 41, 44, 45) ~ "Trade (Wholesale + Retail)",
      NAICS %in% c(8, 48, 49) ~ "Transportation & Warehousing",
      NAICS %in% c(9, 52, 53, 55) ~ "Finance/Business",
      NAICS %in% c(10, 11, 12, 51, 54, 56) ~ "Technical & Administrative Support",
      NAICS %in% c(13, 14, 61, 62) ~ "Education, Health & Social Assistance",
      NAICS %in% c(15, 16, 17, 18, 71, 72, 81) ~ "Arts, Recreation & Other Services",
      NAICS %in% c(19, 91) ~ "Public Administration",
      NAICS %in% c(88, 99, 888, 999) ~ NA_character_,
      TRUE ~ NA_character_
    ),
    INDUSTRY_GROUP = factor(
      INDUSTRY_GROUP,
      levels = c(
        "Agriculture & Resource Extraction",
        "Utilities & Construction",
        "Manufacturing",
        "Trade (Wholesale + Retail)",
        "Transportation & Warehousing",
        "Finance/Business",
        "Technical & Administrative Support",
        "Education, Health & Social Assistance",
        "Arts, Recreation & Other Services",
        "Public Administration"
      )
    )
  ) %>%
  filter(!is.na(Year), Year %in% c(2016, 2021),
         !is.na(STIR), STIR < 1)

empty_plot <- function(msg = "No data") {
  ggplot() +
    annotate("text", x = 0, y = 0, label = msg, size = 5) +
    xlim(-1, 1) + ylim(-1, 1) +
    theme_void()
}

wtd_mean <- function(x, w) {
  w <- ifelse(is.na(w), 0, w)
  x <- ifelse(is.na(x), NA_real_, x)
  s <- sum(w, na.rm = TRUE)
  if (!is.finite(s) || s <= 0) return(NA_real_)
  sum(x * w, na.rm = TRUE) / s
}

wtd_quantile <- function(x, w, probs = 0.5) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (length(x) == 0) return(rep(NA_real_, length(probs)))
  o <- order(x)
  x <- x[o]; w <- w[o]
  cw <- cumsum(w)
  total <- sum(w)
  sapply(probs, function(p) {
    target <- p * total
    idx <- which(cw >= target)[1]
    x[idx]
  })
}

fmt_money <- function(x) dollar(x, accuracy = 1, big.mark = ",")

# Accessible palette (colorblind-friendly-ish)
COL_2016 <- "#0072B2"  # blue
COL_2021 <- "#E69F00"  # orange
COL_TEAL <- "#0F766E"  # dashboard "affordable"
COL_ORNG <- "#C2410C"  # dashboard "unaffordable"

make_tenure_anim_plot <- function(d) {
  if (nrow(d) == 0) return(NULL)
  
  dd <- d %>%
    filter(!is.na(Year), Year %in% c(2016, 2021),
           !is.na(TENURE), !is.na(STIR), STIR < 1,
           !is.na(WEIGHT), WEIGHT > 0) %>%
    group_by(Year, TENURE) %>%
    summarise(
      w_sum = sum(WEIGHT, na.rm = TRUE),
      AVG_STIR = ifelse(w_sum > 0, sum(STIR * WEIGHT, na.rm = TRUE) / w_sum, NA_real_),
      .groups = "drop"
    ) %>%
    mutate(
      AVG_STIR_pct = 100 * AVG_STIR,
      TENURE = factor(as.character(TENURE), levels = c("Renters", "Owners")),
      Year = as.integer(Year),
      Year_lab = factor(Year, levels = c(2016, 2021), labels = c("2016", "2021"))
    )
  
  if (nrow(dd) == 0) return(NULL)
  
  y_max <- suppressWarnings(max(dd$AVG_STIR_pct, na.rm = TRUE))
  if (!is.finite(y_max)) y_max <- 1
  y_lim <- max(35, y_max * 1.15)
  
  ggplot(dd, aes(x = TENURE, y = AVG_STIR_pct, fill = TENURE)) +
    geom_col(width = 0.62, color = "white", linewidth = 0.5, na.rm = TRUE) +
    geom_text(
      aes(label = ifelse(is.na(AVG_STIR_pct), "", paste0(round(AVG_STIR_pct, 1), "%"))),
      vjust = -0.35, fontface = "bold", size = 4.6, color = "#111111", na.rm = TRUE
    ) +
    scale_y_continuous(
      labels = function(x) paste0(x, "%"),
      limits = c(0, y_lim),
      expand = expansion(mult = c(0, 0.06))
    ) +
    scale_fill_manual(values = c("Renters" = COL_2016, "Owners" = COL_2021), guide = "none") +
    labs(
      title = "Avg STIR by tenure — {closest_state}",
      subtitle = "Weighted average STIR (2016 → 2021).",
      x = NULL,
      y = "Average STIR (%)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10.5, color = "grey25"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "grey88", linewidth = 0.4),
      panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
      plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
      plot.margin = margin(6, 10, 6, 10)
    ) +
    transition_states(Year_lab, transition_length = 4, state_length = 2) +
    ease_aes("sine-in-out")
}

make_decile_burdened_plot <- function(d, year_val = 2016) {
  if (nrow(d) == 0) return(empty_plot("No decile data for this selection"))
  
  year_val <- as.integer(year_val)
  bar_col <- if (identical(year_val, 2016L)) COL_2016 else COL_2021
  
  d2 <- d %>%
    filter(Year == year_val) %>%
    mutate(
      burdened = as.integer(STIR > 0.30),
      w = WEIGHT
    ) %>%
    filter(!is.na(EFDecile), EFDecile >= 1, EFDecile <= 10,
           !is.na(w), w > 0,
           !is.na(TotInc_AT), TotInc_AT > 0)
  
  if (nrow(d2) == 0) return(empty_plot("No valid decile rows after filtering."))
  
  inc_rng <- d2 %>%
    group_by(EFDecile) %>%
    summarise(
      p10 = wtd_quantile(TotInc_AT, w, 0.10)[1],
      p90 = wtd_quantile(TotInc_AT, w, 0.90)[1],
      .groups = "drop"
    ) %>%
    mutate(
      EFDecile = as.integer(EFDecile),
      inc_range = paste0(fmt_money(p10), " – ", fmt_money(p90))
    )
  
  dd <- d2 %>%
    group_by(EFDecile) %>%
    summarise(
      pct_burdened = 100 * sum(w * burdened, na.rm = TRUE) / sum(w, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(inc_rng, by = c("EFDecile")) %>%
    mutate(
      EFDecile = factor(EFDecile, levels = 1:10),
      pct_lab = paste0(round(pct_burdened, 1), "%"),
      text = paste0(
        "<b>Decile ", as.character(EFDecile), "</b>",
        "<br>Year: ", year_val,
        "<br>% burdened (STIR > 30%): ", pct_lab,
        "<br>After-tax income (p10–p90): ", inc_range
      )
    )
  
  y_max <- suppressWarnings(max(dd$pct_burdened, na.rm = TRUE))
  if (!is.finite(y_max)) y_max <- 1
  y_lim <- max(35, y_max * 1.20)
  
  ggplot(dd, aes(x = EFDecile, y = pct_burdened, text = text)) +
    geom_col(width = 0.75, fill = bar_col, color = "white", linewidth = 0.5) +
    geom_hline(yintercept = 30, linetype = "dashed", linewidth = 0.5, color = "grey55") +
    scale_y_continuous(
      labels = function(x) paste0(x, "%"),
      limits = c(0, y_lim),
      expand = expansion(mult = c(0.01, 0.06))
    ) +
    labs(
      title = "Burdened households by income decile",
      subtitle = "Hover bars for % burdened and typical after-tax income range (p10–p90).",
      x = "Income decile (1 = lowest, 10 = highest)",
      y = "% burdened (STIR > 30%)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "grey88", linewidth = 0.4),
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9.5, color = "grey25"),
      panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
      plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
      plot.margin = margin(6, 10, 6, 10)
    )
}

make_industry_slope <- function(d) {
  if (!all(c("INDUSTRY_GROUP","WEIGHT","Year","STIR") %in% names(d))) {
    return(empty_plot("Missing data for this plot"))
  }
  
  industry <- d %>%
    filter(!is.na(Year), Year %in% c(2016, 2021),
           !is.na(STIR), STIR < 1,
           !is.na(INDUSTRY_GROUP),
           !is.na(WEIGHT), WEIGHT > 0) %>%
    group_by(Year, INDUSTRY_GROUP) %>%
    summarise(
      w_sum = sum(WEIGHT, na.rm = TRUE),
      AVG_STIR = ifelse(w_sum > 0, sum(STIR * WEIGHT, na.rm = TRUE) / w_sum, NA_real_),
      .groups = "drop"
    ) %>%
    mutate(AVG_STIR_pct = AVG_STIR * 100) %>%
    filter(!is.na(AVG_STIR_pct), is.finite(AVG_STIR_pct))
  
  if (nrow(industry) == 0) return(empty_plot("No valid industry rows after filtering"))
  
  order_2021 <- industry %>%
    filter(Year == 2021) %>%
    arrange(desc(AVG_STIR_pct)) %>%
    pull(INDUSTRY_GROUP) %>% as.character() %>% unique()
  order_2021 <- order_2021[!is.na(order_2021)]
  
  industry <- industry %>%
    mutate(
      INDUSTRY_GROUP = factor(as.character(INDUSTRY_GROUP), levels = order_2021),
      Year = as.integer(Year),
      text = paste0(
        "<b>", as.character(INDUSTRY_GROUP), "</b>",
        "<br>Year: ", Year,
        "<br>Avg STIR: ", round(AVG_STIR_pct, 1), "%"
      )
    )
  
  ggplot(industry, aes(x = INDUSTRY_GROUP, y = AVG_STIR_pct)) +
    geom_line(aes(group = INDUSTRY_GROUP), linewidth = 0.55, color = "grey75") +
    geom_point(
      aes(color = factor(Year), fill = factor(Year), text = text),
      shape = 21, size = 3.0, stroke = 1.4
    ) +
    scale_color_manual(values = c("2016" = COL_2016, "2021" = COL_2021), name = "Year") +
    scale_fill_manual(values = c("2016" = COL_2016, "2021" = "white"), guide = "none") +
    geom_hline(yintercept = 30, linetype = "dashed", linewidth = 0.5, color = "grey60") +
    labs(
      title = "Avg STIR by industry (2016 vs 2021)",
      subtitle = "Dashed line marks 30% burden threshold. Hover points for exact values.",
      x = "Industry sector",
      y = "Average STIR (%)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "grey87", linewidth = 0.4),
      legend.position = "top",
      axis.text.y = element_text(size = 9.2),
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9.5, color = "grey25"),
      panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
      plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
      plot.margin = margin(6, 10, 6, 10)
    ) +
    coord_flip()
}

build_waffle_for_year <- function(d, year_val, base_tiles = 100L) {
  d <- d %>% mutate(Year = as.integer(Year)) %>% filter(Year == as.integer(year_val))
  if (nrow(d) == 0) return(empty_plot(paste0("No rows for ", year_val)))
  
  burdened <- d %>% filter(STIR > 0.30, !is.na(EDU), !is.na(WEIGHT), WEIGHT > 0)
  if (nrow(burdened) == 0) {
    return(empty_plot("No burdened households (STIR > 30%) for this selection."))
  }
  
  shares2 <- burdened %>%
    group_by(EDU) %>%
    summarise(burdened_w = sum(WEIGHT, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      share = burdened_w / sum(burdened_w),
      share_lab = percent(share, accuracy = 1),
      EDU_label = paste0(str_wrap(as.character(EDU), width = 24), "\n(", share_lab, ")")
    ) %>%
    arrange(desc(share))
  
  alloc <- shares2 %>%
    mutate(raw = share * base_tiles, tiles = floor(raw), rem = raw - tiles)
  
  leftover <- base_tiles - sum(alloc$tiles, na.rm = TRUE)
  if (leftover > 0) {
    bump <- order(alloc$rem, decreasing = TRUE)[seq_len(leftover)]
    alloc$tiles[bump] <- alloc$tiles[bump] + 1L
  }
  alloc <- alloc %>% mutate(tiles = pmax(0L, as.integer(tiles)))
  
  waffle_df <- alloc %>%
    transmute(EDU_label, Share = tiles, Remainder = base_tiles - tiles) %>%
    pivot_longer(c(Share, Remainder), names_to = "type", values_to = "percent") %>%
    mutate(
      type = factor(type, levels = c("Share", "Remainder")),
      EDU_label = factor(EDU_label, levels = alloc$EDU_label),
      percent = as.integer(coalesce(percent, 0L))
    )
  
  # Softer, consistent colors (no x/y mention anywhere)
  main_col <- if (year_val == 2016) "#2A9D8F" else "#8E44AD"
  dark_col <- "#1F2937"
  bg <- "#222725"
  
  ggplot(waffle_df, aes(fill = type, values = percent)) +
    geom_waffle(na.rm = TRUE, n_rows = 4, flip = FALSE, size = 0.33, colour = "white") +
    facet_wrap(~EDU_label, ncol = 1, strip.position = "left") +
    coord_equal() +
    labs(
      title = "Burdened households by education",
      subtitle = paste0("Year: ", year_val, " — each square = 1% of burdened households (STIR > 30%)."),
      fill = NULL
    ) +
    scale_fill_manual(
      values = c("Share" = main_col, "Remainder" = dark_col),
      labels = c("Share" = "Education group's share", "Remainder" = "Remaining share")
    ) +
    guides(fill = guide_legend(title.position = "top")) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(color = "white", size = 9),
      legend.key = element_rect(fill = bg, color = NA),
      legend.background = element_rect(fill = bg, color = NA),
      plot.background = element_rect(fill = bg, color = NA),
      panel.background = element_rect(fill = bg, color = NA),
      
      strip.text = element_text(
        hjust = 1, vjust = 0.5, size = 9.2,
        margin = margin(r = 4, l = 8, unit = "pt"),
        lineheight = 0.95, color = "white"
      ),
      plot.title = element_text(size = 11.2, face = "bold", color = "white",
                                margin = margin(6, 6, 2, 6), lineheight = 1.05),
      plot.subtitle = element_text(size = 9.0, color = "white",
                                   margin = margin(0, 6, 6, 6), lineheight = 1.05),
      plot.margin = margin(6, 10, 6, 10),
      strip.placement = "outside"
    )
}

ui <- fluidPage(
  theme = bs_theme(version = 5),
  tags$style(HTML("
    body { background: #f5f1e6; }
    .frame {
      max-width: 1250px;
      width: calc(100% - 40px);
      margin: 28px auto;
      border: 3px solid #111;
      background: #f8f6ef;
      padding: 18px;
      box-shadow: 0 8px 22px rgba(0,0,0,0.10);
    }
    .inner { border: 3px solid #111; padding: 16px; background: #fbfaf4; }
    .leftBox, .rightColBox { border: 2px solid #111; background: #fffdf7; padding: 14px; height: 100%; }
    .appTitle { font-weight: 900; font-size: 26px; margin: 0; letter-spacing: 0.2px; }
    .appSub { margin: 6px 0 0 0; opacity: 0.85; line-height: 1.25; }
    .defs { margin-top: 10px; font-size: 13.5px; opacity: 0.9; line-height: 1.25; }
    .title { font-weight: 800; letter-spacing: 0.5px; margin-bottom: 10px; }
    .stirBox { border: 2px solid #111; background: #fffdf7; padding: 20px; margin-bottom: 10px; min-height: 320px; }
    .bigText { font-size: 28px; font-weight: 900; }
    .metricRow { display: flex; gap: 16px; flex-wrap: wrap; margin-top: 8px; }
    .metricPill { border: 2px solid #111; border-radius: 16px; padding: 14px 16px; background: #fff; min-width: 270px; }
    .pillTop { display:flex; align-items:baseline; justify-content:space-between; gap: 12px; }
    .pillYear { font-size: 16px; font-weight: 900; letter-spacing: 0.2px; }
    .metricValue { font-size: 28px; font-weight: 900; }
    .pillSub { font-size: 13.5px; opacity: 0.82; margin-top: 6px; }
    .pill-affordable { border-color: #0F766E; color: #0F766E; }
    .pill-unaffordable { border-color: #C2410C; color: #C2410C; }

    .btn-outline-dark { border-width: 2px; font-weight: 800; padding: 10px 16px; font-size: 15px; }
    .note { font-size: 13px; opacity: 0.85; margin-top: 10px; line-height: 1.25; }
    .exploreWrap { display:flex; justify-content:center; margin-top: 12px; }

    .vizHead { display:flex; align-items:flex-start; justify-content:space-between; gap: 14px; margin-bottom: 10px; }
    .vizTextWrap { flex: 1; min-width: 0; }
    .vizTitle { font-weight: 800; font-size: 17px; margin: 0; line-height: 1.12; white-space: normal; word-break: break-word; }
    .vizSub { opacity: 0.8; margin: 4px 0 0 0; font-size: 12.2px; line-height: 1.25; white-space: normal; word-break: break-word; }
    .vizCard { border: 2px solid #111; background: #fffdf7; border-radius: 16px; padding: 6px; margin: 6px; overflow: hidden; }
    .navWrap { display:flex; align-items:center; gap: 10px; flex: 0 0 auto; }
    .navBtn { width: 42px; height: 42px; border: 2px solid #111; border-radius: 999px;
              background: #fff; font-size: 18px; font-weight: 900;
              display:flex; align-items:center; justify-content:center; padding: 0; }
    .navBtn:active { transform: translateY(1px); }
    .navBtnDisabled { opacity: 0.35; cursor: not-allowed; }
    .stepPill { border: 2px solid #111; border-radius: 999px; padding: 8px 10px; background:#fff;
                font-weight: 800; font-size: 12px; }
    .waffleBar { margin: 8px 10px 0 10px; }

    /* Make GIF responsive inside vizCard */
    .vizCard img { max-width: 100% !important; height: auto !important; display: block; }
  ")),
  
  div(class="frame",
      div(class="inner",
          tags$div(
            tags$h1(class="appTitle", "Shelter-to-Income Ratio (STIR) Explorer"),
            tags$p(class="appSub",
                   "Use the filters to explore housing affordability patterns across 2016 vs 2021. Filters apply to both the dashboard tiles and the visualizations."),
            tags$div(class="defs",
                     tags$b("Definitions:"), tags$br(),
                     tags$b("STIR"), " = the share of your income that goes to housing costs (higher = less affordable).", tags$br(),
                     tags$b("CMA"), " = a big city and the surrounding towns/suburbs that are part of the same metro area."
            )
          ),
          tags$hr(style="border-top:2px solid #111; opacity: 0.15;"),
          
          fluidRow(
            column(
              4,
              div(class="leftBox",
                  div(class="title","Filters"),
                  selectInput("cma", "Census Metropolitan Area (CMA)", choices = NULL),
                  selectInput("edu", "Education", choices = NULL),
                  selectInput("tenure", "Tenure Type", choices = NULL),
                  selectInput("income2", "Household After Tax Income", choices = NULL),
                  selectInput("industry", "Industry Sector", choices = NULL),
                  actionButton("reset_filters", "Reset (All filters)",
                               class = "btn btn-outline-dark",
                               style = "width:100%; margin-top:10px;"),
                  tags$div(
                    style="opacity:0.75; margin-top:8px; line-height:1.25;",
                    "Tip: Filters apply everywhere. Use Reset to return to the full dataset."
                  )
              )
            ),
            column(
              8,
              tabsetPanel(
                id = "mainTabs",
                
                tabPanel(
                  "Dashboard",
                  div(class="rightColBox",
                      div(class="stirBox",
                          div(class="bigText","Weighted AVG STIR:"),
                          uiOutput("avg_stir_text"),
                          tags$hr(),
                          tags$div(
                            tags$b("Burden threshold:"), " STIR > 30%", tags$br(),
                            tags$b("Affordable threshold:"), " STIR ≤ 30%"
                          ),
                          tags$hr(),
                          tags$div(class="note",
                                   "STIR = (monthly shelter costs × 12) / annual after-tax income.")
                      ),
                      div(class="exploreWrap",
                          actionButton("explore", "Explore Visualizations", class="btn btn-outline-dark")
                      )
                  )
                ),
                
                tabPanel(
                  "Visualizations",
                  div(class="rightColBox",
                      div(class="vizHead",
                          uiOutput("viz_text"),
                          uiOutput("viz_nav")
                      ),
                      uiOutput("viz_controls"),
                      div(class="vizCard", uiOutput("active_plot_ui")),
                      actionButton("back", "Back to Dashboard", class="btn btn-outline-dark")
                  )
                )
              )
            )
          )
      )
  )
)

server <- function(input, output, session) {
  
  observe({
    updateSelectInput(session, "cma",
                      choices = c("All"="All", "N/A"="__NA__", sort(unique(na.omit(census2$CMAS)))),
                      selected = "All"
    )
    updateSelectInput(session, "edu",
                      choices = c("All"="All", levels(census2$EDU)),
                      selected = "All"
    )
    updateSelectInput(session, "tenure",
                      choices = c("All"="All", "N/A"="__NA__", levels(census2$TENURE)),
                      selected = "All"
    )
    updateSelectInput(session, "income2",
                      choices = c("All"="All", "N/A"="__NA__", levels(census2$INCOME)),
                      selected = "All"
    )
    updateSelectInput(session, "industry",
                      choices = c("All"="All", "N/A"="__NA__", levels(census2$INDUSTRY_GROUP)),
                      selected = "All"
    )
  })
  
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "cma", selected = "All")
    updateSelectInput(session, "edu", selected = "All")
    updateSelectInput(session, "tenure", selected = "All")
    updateSelectInput(session, "income2", selected = "All")
    updateSelectInput(session, "industry", selected = "All")
  }, ignoreInit = TRUE)
  
  filtered <- reactive({
    d <- census2
    
    if (!is.null(input$cma) && input$cma != "All") {
      if (input$cma == "__NA__") d <- d %>% filter(is.na(CMAS)) else d <- d %>% filter(CMAS == input$cma)
    }
    if (!is.null(input$edu) && input$edu != "All") d <- d %>% filter(EDU == input$edu)
    
    if (!is.null(input$tenure) && input$tenure != "All") {
      if (input$tenure == "__NA__") d <- d %>% filter(is.na(TENURE)) else d <- d %>% filter(TENURE == input$tenure)
    }
    if (!is.null(input$income2) && input$income2 != "All") {
      if (input$income2 == "__NA__") d <- d %>% filter(is.na(INCOME))
      else d <- d %>% filter(INCOME == input$income2)
    }
    if (!is.null(input$industry) && input$industry != "All") {
      if (input$industry == "__NA__") d <- d %>% filter(is.na(INDUSTRY_GROUP))
      else d <- d %>% filter(INDUSTRY_GROUP == input$industry)
    }
    
    d
  })
  
  # Weighted AVG STIR tiles
  avg_by_year <- reactive({
    filtered() %>%
      filter(!is.na(WEIGHT), WEIGHT > 0, !is.na(STIR), is.finite(STIR)) %>%
      group_by(Year) %>%
      summarise(
        avg_stir = wtd_mean(STIR, WEIGHT),
        n = dplyr::n(),
        .groups="drop"
      )
  })
  
  output$avg_stir_text <- renderUI({
    a <- avg_by_year()
    
    year_box <- function(y) {
      row <- a %>% filter(Year == y)
      if (nrow(row) == 0) {
        return(div(class = "metricPill",
                   tags$div(class="pillTop",
                            tags$span(class = "pillYear", paste0(y, ":")),
                            tags$span(class = "metricValue", "—")
                   ),
                   tags$div(class = "pillSub", "No observations")
        ))
      }
      
      val <- row$avg_stir[1]
      burdened <- is.finite(val) && val > 0.30
      cls <- if (burdened) "metricPill pill-unaffordable" else "metricPill pill-affordable"
      label <- ifelse(burdened, "Burdened", "Not burdened")
      
      div(class = cls,
          tags$div(class="pillTop",
                   tags$span(class = "pillYear", paste0(y, ":")),
                   tags$span(class = "metricValue", percent(val, accuracy = 0.1))
          ),
          tags$div(class="pillSub",
                   paste0(label, " | n=", format(row$n[1], big.mark = ",")))
      )
    }
    
    div(class = "metricRow", year_box(2016), year_box(2021))
  })
  
  observeEvent(input$explore, { updateTabsetPanel(session, "mainTabs", selected="Visualizations") })
  observeEvent(input$back,    { updateTabsetPanel(session, "mainTabs", selected="Dashboard") })
  
  step <- reactiveVal(1)
  step_max <- 4
  observeEvent(input$viz_prev, { if (step() > 1) step(step() - 1) })
  observeEvent(input$viz_next, { if (step() < step_max) step(step() + 1) })
  
  output$viz_nav <- renderUI({
    s <- step()
    prev_disabled <- (s <= 1)
    next_disabled <- (s >= step_max)
    
    prev_ui <- if (prev_disabled) {
      tags$button(type = "button", class = "navBtn navBtnDisabled", disabled = TRUE, HTML("&#9664;"))
    } else {
      actionButton("viz_prev", label = HTML("&#9664;"), class = "navBtn")
    }
    
    next_ui <- if (next_disabled) {
      tags$button(type = "button", class = "navBtn navBtnDisabled", disabled = TRUE, HTML("&#9654;"))
    } else {
      actionButton("viz_next", label = HTML("&#9654;"), class = "navBtn")
    }
    
    tags$div(class = "navWrap",
             prev_ui,
             tags$div(class = "stepPill", paste0("Viz ", s, "/", step_max)),
             next_ui)
  })
  
  output$viz_text <- renderUI({
    s <- step()
    title <- switch(as.character(s),
                    "1" = "Avg STIR by tenure (Renters vs Owners)",
                    "2" = "Burdened households by income decile",
                    "3" = "Avg STIR by industry (2016 vs 2021)",
                    "4" = "Burdened households by education"
    )
    sub <- switch(as.character(s),
                  "1" = "Animated weighted averages (2016 → 2021).",
                  "2" = "STIR > 30% is burdened. Hover for income ranges (p10–p90).",
                  "3" = "Dashed line marks the 30% burden threshold. Hover for exact values.",
                  "4" = "Each square = 1% of burdened households (STIR > 30%)."
    )
    
    tags$div(class="vizTextWrap",
             tags$div(class = "vizTitle", title),
             tags$div(class = "vizSub", sub)
    )
  })
  
  output$viz_controls <- renderUI({
    if (step() == 2) {
      div(class="waffleBar",
          radioButtons("decile_year", NULL,
                       choices = c("2016" = 2016, "2021" = 2021),
                       selected = 2016, inline = TRUE))
    } else if (step() == 4) {
      div(class="waffleBar",
          radioButtons("waffle_year", NULL,
                       choices = c("2016" = 2016, "2021" = 2021),
                       selected = 2016, inline = TRUE))
    } else {
      NULL
    }
  })
  
  output$active_plot_ui <- renderUI({
    if (step() == 1) {
      # smaller height so it fits comfortably in the card
      imageOutput("active_gif", height = "560px")
    } else if (step() %in% c(2, 3)) {
      plotlyOutput("active_plotly", height = "640px")
    } else {
      plotOutput("active_plot", height = "640px")
    }
  })
  
  output$active_gif <- renderImage({
    req(step() == 1)
    
    d <- filtered() %>%
      filter(!is.na(Year), Year %in% c(2016, 2021),
             !is.na(STIR), STIR < 1,
             !is.na(TENURE),
             !is.na(WEIGHT), WEIGHT > 0)
    
    if (nrow(d) == 0) {
      png_file <- tempfile(fileext = ".png")
      ggsave(png_file, plot = empty_plot("No valid tenure rows after filtering"), width = 8.4, height = 5.2, dpi = 120)
      return(list(src = png_file, contentType = "image/png"))
    }
    
    p <- make_tenure_anim_plot(d)
    if (is.null(p)) {
      png_file <- tempfile(fileext = ".png")
      ggsave(png_file, plot = empty_plot("Not enough data"), width = 8.4, height = 5.2, dpi = 120)
      return(list(src = png_file, contentType = "image/png"))
    }
    
    gif_file <- tempfile(fileext = ".gif")
    
    # smaller render = better fit + less overflow risk
    if (requireNamespace("gifski", quietly = TRUE)) {
      anim <- animate(p, nframes = 100, fps = 10, width = 860, height = 520, renderer = gifski_renderer())
      anim_save(gif_file, animation = anim)
      return(list(src = gif_file, contentType = "image/gif"))
    } else if (requireNamespace("magick", quietly = TRUE)) {
      anim <- animate(p, nframes = 100, fps = 10, width = 860, height = 520, renderer = magick_renderer())
      anim_save(gif_file, animation = anim)
      return(list(src = gif_file, contentType = "image/gif"))
    } else {
      png_file <- tempfile(fileext = ".png")
      ggsave(png_file, plot = empty_plot("Install gifski or magick to render GIF animations"), width = 8.4, height = 5.2, dpi = 120)
      return(list(src = png_file, contentType = "image/png"))
    }
  }, deleteFile = FALSE)
  
  output$active_plotly <- renderPlotly({
    s <- step()
    
    if (s == 2) {
      yr <- as.integer(input$decile_year %||% 2016)
      
      d <- filtered() %>%
        filter(!is.na(Year), Year == yr,
               !is.na(STIR), STIR < 1,
               !is.na(EFDecile), EFDecile >= 1, EFDecile <= 10,
               !is.na(WEIGHT), WEIGHT > 0,
               !is.na(TotInc_AT), TotInc_AT > 0)
      
      if (nrow(d) == 0) return(ggplotly(empty_plot("No valid decile rows after filtering")))
      p <- make_decile_burdened_plot(d, year_val = yr)
      return(ggplotly(p, tooltip = "text") %>% layout(hoverlabel = list(align = "left")))
    }
    
    if (s == 3) {
      d <- filtered() %>%
        filter(!is.na(Year), Year %in% c(2016, 2021),
               !is.na(STIR), STIR < 1,
               !is.na(INDUSTRY_GROUP),
               !is.na(WEIGHT), WEIGHT > 0)
      
      if (nrow(d) == 0) return(ggplotly(empty_plot("No valid industry rows after filtering.")))
      p <- make_industry_slope(d)
      return(ggplotly(p, tooltip = "text") %>% layout(hoverlabel = list(align = "left")))
    }
    
    ggplotly(empty_plot(""))
  })
  
  output$active_plot <- renderPlot({
    req(step() == 4)
    yr <- as.integer(input$waffle_year %||% 2016)
    
    d <- filtered() %>%
      filter(!is.na(Year), Year %in% c(2016, 2021),
             !is.na(STIR), STIR < 1,
             !is.na(EDU),
             !is.na(WEIGHT), WEIGHT > 0)
    
    if (nrow(d) == 0) return(empty_plot("No valid education rows after filtering."))
    build_waffle_for_year(d, yr)
  }, res = 120)
}

shinyApp(ui = ui, server = server)

