#!/usr/bin/env Rscript
# =============================================================================
# Programme Skills Dashboard
# University of Stirling — Skills Development Survey
#
# Combines three views:
#   1. UN Competency Skills Wheel (radial wheel, competency highlight)
#   2. Module Skills Detail      (hover a module to see its full skills)
#   3. Programme Skills Map       (year grid + skills trajectory)
#
# Data files (in data/):
#   programmes.csv      — flat programme structure (one row per module slot)
#   module_skills.csv   — module skills + metadata in tidy/long format
#   module_topics.csv   — module topic tags
#   collections.csv     — collection module pools
#
# Local:   shiny::runApp("app.R")
# Deploy:  rsconnect::deployApp("app.R")
# =============================================================================

library(shiny)
library(bslib)

# ---------------------------------------------------------------------------
# DATA LOADING
# ---------------------------------------------------------------------------
app_dir <- NULL
tryCatch({ app_dir <- dirname(sys.frame(1)$ofile) }, error = function(e) {})
if (is.null(app_dir) || app_dir == "") app_dir <- "."

data_dir <- file.path(app_dir, "data")

read_csv <- function(name) {
  f <- file.path(data_dir, name)
  if (!file.exists(f)) {
    alt <- file.path(app_dir, name)
    if (file.exists(alt)) f <- alt
  }
  if (!file.exists(f)) {
    alt <- file.path(dirname(app_dir), "data", name)
    if (file.exists(alt)) f <- alt
  }
  read.csv(f, stringsAsFactors = FALSE)
}

programmes_csv    <- read_csv("programmes.csv")
skills_csv        <- read_csv("module_skills.csv")
module_topics_csv <- read_csv("module_topics.csv")

# Also load radial_data.csv (pre-computed UN competency wheel data)
radial_csv <- NULL
tryCatch({
  rf <- file.path(app_dir, "radial_data.csv")
  if (file.exists(rf)) radial_csv <- read.csv(rf, stringsAsFactors = FALSE)
}, error = function(e) {})
if (is.null(radial_csv) && file.exists(file.path(data_dir, "..", "radial_data.csv"))) {
  radial_csv <- read.csv(file.path(data_dir, "..", "radial_data.csv"), stringsAsFactors = FALSE)
}

# Build module_code -> module metadata lookup (from module_skills.csv)
module_meta <- list()
unique_mods <- unique(skills_csv[, c("module_code", "module_name", "coordinator", "year", "semester")])
for (i in seq_len(nrow(unique_mods))) {
  r <- unique_mods[i, ]
  mc <- r$module_code
  mod_topics <- module_topics_csv$topic[module_topics_csv$module_code == mc]
  module_meta[[mc]] <- list(
    name        = r$module_name,
    coordinator = r$coordinator,
    year        = as.integer(r$year),
    semester    = as.integer(r$semester),
    topics      = mod_topics
  )
}

# Build skills_lookup: module_code -> list of skill category -> named vector
skill_list <- split(skills_csv, skills_csv$module_code)
skills_lookup <- list()
for (mc in names(skill_list)) {
  sub <- skill_list[[mc]]
  cat_skills <- list()
  for (cat in unique(sub$category)) {
    cat_sub <- sub[sub$category == cat, ]
    cat_skills[[cat]] <- setNames(as.integer(cat_sub$level), cat_sub$skill_name)
  }
  skills_lookup[[mc]] <- cat_skills
}

# ---------------------------------------------------------------------------
# PROGRAMME LABELS
# ---------------------------------------------------------------------------
programme_labels <- unique(programmes_csv$programme_label)
degree_labels   <- unique(programmes_csv$degree)

# ---------------------------------------------------------------------------
# COMPETENCY DEFINITIONS
# ---------------------------------------------------------------------------
COMPETENCIES <- c(
  "Adaptability",
  "Systems Thinking",
  "Norms and Conflict Resolution",
  "Innovation",
  "Teamwork & Collaboration",
  "Critical Thinking",
  "Self-Awareness",
  "Integrated Problem Solving"
)

COMP_DESCRIPTIONS <- list(
  "Adaptability" = "The ability to understand and evaluate multiple scenarios for the future \u2013 possible, probable and desirable; to apply the precautionary principle; to assess the consequences of actions; and to deal with risks and changes.",
  "Systems Thinking" = "This data is estimated from a composite of Critical Thinking and Integrated Problem-Solving scores. The competency recognises the ability to identify and analyse complex systems and relationships across different domains and scales.",
  "Norms and Conflict Resolution" = "The ability to understand and reflect on the norms and values that underlie one\u2019s actions; and to negotiate values, principles, goals, and targets, in a context of conflicts of interest and trade-offs.",
  "Innovation" = "The ability to develop and implement innovative actions; this may include those taken at a personal-, local-level or further afield (e.g., designing effective research processes).",
  "Teamwork & Collaboration" = "The ability to learn from others; to understand and respect the needs, perspectives and actions of others (empathy); to understand, relate to and be sensitive to others.",
  "Critical Thinking" = "The ability to question norms, practices and opinions; to reflect on one\u2019s own values, perceptions and actions; and to frame new knowledge within a wider context.",
  "Self-Awareness" = "The ability to reflect on one\u2019s own role in the local community and (global) society; to continually evaluate and further motivate one\u2019s actions.",
  "Integrated Problem Solving" = "The overarching ability to apply different problem-solving frameworks to complex problems and develop viable, inclusive and equitable solution options."
)

COMP_COLORS <- c(
  "#4A90D9", "#E74C3C", "#E97132", "#50B86C",
  "#9B59B6", "#F1C40F", "#1ABC9C", "#34495E"
)

LEVEL_COLORS <- c("1" = "#156082", "2" = "#E97132", "3" = "#A02B93")
LEVEL_SHAPES <- c("1" = "rect", "2" = "rounded", "3" = "hexagon")
LEVEL_LABELS <- c("1" = "Fundamentals", "2" = "Basic", "3" = "Advanced")

# ---------------------------------------------------------------------------
# PROGRAMME MAPPER CONSTANTS
# ---------------------------------------------------------------------------
COL_GREY   <- "#c0c0c0"
COL_LIGHT  <- "#a8d86e"
COL_MEDIUM <- "#56a832"
COL_DARK   <- "#006938"
COL_ACCENT <- "#77BF22"

SKILL_COLS <- c(COL_GREY, COL_LIGHT, COL_MEDIUM, COL_DARK)
SKILL_LABS <- c("Not developed", "Fundamentals", "Basic", "Advanced")

CATEGORY_LABELS <- c(
  computational = "Computational & Digital Skills",
  field         = "Field Skills",
  lab           = "Laboratory Skills",
  personal      = "Personal & Professional Skills"
)

STATUS_COLS <- list(
  compulsory = COL_DARK,
  pathway    = COL_ACCENT,
  optional   = COL_LIGHT
)

CATEGORY_SKILLS <- list(
  computational = sort(unique(skills_csv$skill_name[skills_csv$category == "computational"])),
  field         = sort(unique(skills_csv$skill_name[skills_csv$category == "field"])),
  lab           = sort(unique(skills_csv$skill_name[skills_csv$category == "lab"])),
  personal      = sort(unique(skills_csv$skill_name[skills_csv$category == "personal"]))
)

# CSS circle builder
skill_circle <- function(level) {
  if (is.na(level)) return(NULL)
  col <- SKILL_COLS[level + 1]
  if (level == 0) {
    span(style = paste0(
      "display:inline-block; width:28px; height:28px; border-radius:50%;",
      "border: 3px solid ", col, "; vertical-align:middle;"
    ))
  } else if (level == 1) {
    span(style = paste0(
      "display:inline-block; width:28px; height:28px; border-radius:50%;",
      "border: 3px solid ", col, "; vertical-align:middle;",
      "background: radial-gradient(circle, ", col, " 25%, transparent 25%);"
    ))
  } else if (level == 2) {
    span(style = paste0(
      "display:inline-block; width:28px; height:28px; border-radius:50%;",
      "border: 2px solid ", col, "; vertical-align:middle;",
      "background: linear-gradient(to right, ", col, " 50%, transparent 50%);"
    ))
  } else {
    span(style = paste0(
      "display:inline-block; width:28px; height:28px; border-radius:50%;",
      "background: ", col, "; vertical-align:middle;"
    ))
  }
}

semester_key <- function(year, period) {
  paste0("Y", year, ifelse(period == "Autumn", "A", "S"))
}

# ---------------------------------------------------------------------------
# RADIAL WHEEL
# ---------------------------------------------------------------------------
N_COMP <- length(COMPETENCIES)
WEDGE_SPAN <- 40
GAP_DEG <- 5

RADIUS <- c(42, 84, 126, 168, 210)
CX <- 240
CY <- 240
SIZE <- 480

build_wheel_html <- function(programme, selected_comp, df_override = NULL) {
  df <- if (!is.null(df_override)) df_override else radial_csv[radial_csv$programme == programme, ]
  if (nrow(df) == 0) return("<p style='color:#888;'>No data for this programme.</p>")

  parts <- c()

  # Rings
  max_year <- max(as.integer(df$year))
  for (yi in seq_len(min(max_year, 5))) {
    r <- RADIUS[yi]
    parts <- c(parts, sprintf(
      '<div class="ring" style="width:%dpx;height:%dpx;left:%dpx;top:%dpx;border-color:%s"></div>',
      r*2, r*2, CX - r, CY - r, c("#a0a0a0","#909090","#808080","#707070","#606060")[yi]
    ))
  }

  comp_angles <- list()
  start_deg <- -90 + GAP_DEG

  for (ci in seq_len(N_COMP)) {
    comp <- COMPETENCIES[ci]
    comp_start <- start_deg + (ci - 1) * (WEDGE_SPAN + GAP_DEG)
    comp_end <- comp_start + WEDGE_SPAN
    comp_mid <- (comp_start + comp_end) / 2
    comp_angles[[comp]] <- list(start = comp_start, end = comp_end, mid = comp_mid)
  }

  # Module placements — only the selected competency
  for (ci in seq_len(N_COMP)) {
    comp <- COMPETENCIES[ci]
    comp_df <- df[df$competency == comp, ]
    if (nrow(comp_df) == 0) next
    if (comp != selected_comp) next

    ca <- comp_angles[[comp]]

    for (ri in seq_len(nrow(comp_df))) {
      row <- comp_df[ri, ]
      yr <- as.integer(row$year)
      if (yr > 5) yr <- 5
      r <- RADIUS[yr]

      lvl <- as.character(row$level)
      if (lvl == "0") next

      col <- LEVEL_COLORS[lvl]
      shape <- LEVEL_SHAPES[lvl]

      n_comp_year <- sum(comp_df$year == row$year)
      if (n_comp_year > 1) {
        same_idx <- sum(comp_df$year == row$year & seq_len(nrow(comp_df)) <= ri)
        frac <- (same_idx - 1) / max(n_comp_year - 1, 1)
        angle_deg <- ca$start + frac * (ca$end - ca$start)
      } else {
        angle_deg <- ca$mid
      }

      angle_rad <- angle_deg * pi / 180
      x <- CX + r * sin(angle_rad)
      y <- CY - r * cos(angle_rad)

      label <- sub("^[A-Z]+", "", row$module_code)

      parts <- c(parts, sprintf(
        '<div class="module %s" style="left:%.1fpx;top:%.1fpx;background:%s;border-color:%s;z-index:3" title="%s | %s" data-code="%s" data-year="%s" data-level="%s">%s</div>',
        shape, x - 22, y - 11, col, col,
        row$module_name, LEVEL_LABELS[lvl],
        row$module_code, row$year, lvl,
        label
      ))
    }
  }

  # Active wedge overlay
  if (selected_comp != "") {
    ca <- comp_angles[[selected_comp]]
    OVERLAY_WIDTH <- 100
    wedge_mid <- (ca$start + ca$end) / 2
    wedge_mid <- wedge_mid - (WEDGE_SPAN + GAP_DEG) * 2
    ov_start <- wedge_mid - OVERLAY_WIDTH / 2
    ov_end <- wedge_mid + OVERLAY_WIDTH / 2

    ov_start_css <- (ov_start + 90) %% 360
    ov_end_css <- (ov_end + 90) %% 360

    if (ov_start_css < ov_end_css) {
      conic_str <- sprintf(
        'conic-gradient(rgba(255,255,255,0.85) %.1fdeg %.1fdeg, transparent %.1fdeg %.1fdeg, rgba(255,255,255,0.85) %.1fdeg 360deg)',
        0, ov_start_css, ov_start_css, ov_end_css, ov_end_css
      )
    } else {
      conic_str <- sprintf(
        'conic-gradient(transparent 0deg %.1fdeg, rgba(255,255,255,0.85) %.1fdeg %.1fdeg, transparent %.1fdeg 360deg)',
        ov_end_css, ov_end_css, ov_start_css, ov_start_css
      )
    }

    parts <- c(parts, sprintf(
      '<div class="overlay" style="background: %s;"></div>', conic_str
    ))

    # Wedge label
    ci <- which(COMPETENCIES == selected_comp)
    comp_color <- COMP_COLORS[ci]
    words <- strsplit(selected_comp, " ")[[1]]
    words <- lapply(words, function(w) {
      if (w == "&") "&amp;" else if (w == "and") "and" else w
    })
    display_name <- paste(unlist(words), collapse = "<br>")

    parts <- c(parts, sprintf(
      '<div style="position:absolute;left:%.1fpx;top:%.1fpx;
                   transform:translate(-50%%,-50%%);
                   font-size:11px;font-weight:700;color:%s;
                   pointer-events:none;z-index:6;
                   text-align:center;line-height:1.2;
                   text-shadow:0 0 6px white,0 0 3px white,0 0 1px white;">%s</div>',
      CX + (max(RADIUS) + 20) * sin(wedge_mid * pi / 180),
      CY - (max(RADIUS) + 20) * cos(wedge_mid * pi / 180),
      comp_color, display_name
    ))

    # Ring labels — opposite side
    opp_angle <- wedge_mid + 180
    opp_rad <- opp_angle * pi / 180
    for (yi in seq_len(min(max_year, 5))) {
      r <- RADIUS[yi]
      lx <- CX + r * sin(opp_rad)
      ly <- CY - r * cos(opp_rad)
      parts <- c(parts, sprintf(
        '<div class="ring-label" style="left:%.1fpx;top:%.1fpx">Y%d</div>',
        lx - 5, ly - 7, yi
      ))
    }
  }

  paste(parts, collapse = "\n")
}

# ---------------------------------------------------------------------------
# SKILLS-AREA RADIAL WHEEL
# ---------------------------------------------------------------------------
build_skills_wheel_html <- function(programme, category, selected_skill = "",
                                    degree_filter = NULL, prog_modules_override = NULL) {
  if (!is.null(prog_modules_override)) {
    prog_modules <- prog_modules_override
  } else {
    prog_modules <- programmes_csv[programmes_csv$programme_label == programme, ]
  }
  if (nrow(prog_modules) == 0) return("<p style='color:#888;'>No data for this programme.</p>")

  all_skills <- sort(unique(skills_csv$skill_name[skills_csv$category == category]))
  if (length(all_skills) == 0) return("<p style='color:#888;'>No skills in this category.</p>")

  n_skills <- length(all_skills)
  gap <- 3
  wedge_span <- (360 - n_skills * gap) / n_skills
  OPENING_WIDTH <- max(wedge_span * 2, 72)

  parts <- c()

  max_year <- max(as.integer(prog_modules$year))
  ring_cols <- c("#a0a0a0","#909090","#808080","#707070","#606060")
  for (yi in seq_len(min(max_year, 5))) {
    r <- RADIUS[yi]
    parts <- c(parts, sprintf(
      '<div class="ring" style="width:%dpx;height:%dpx;left:%dpx;top:%dpx;border-color:%s"></div>',
      r*2, r*2, CX - r, CY - r, ring_cols[yi]
    ))
  }

  start_deg <- -90 + gap
  skill_angles <- list()
  for (si in seq_len(n_skills)) {
    skill <- all_skills[si]
    s_start <- start_deg + (si - 1) * (wedge_span + gap)
    s_end   <- s_start + wedge_span
    s_mid   <- (s_start + s_end) / 2
    skill_angles[[skill]] <- list(start = s_start, end = s_end, mid = s_mid)
  }

  mod_codes <- unique(prog_modules$module_code[prog_modules$has_skills == TRUE])
  mod_codes <- mod_codes[!grepl("^ANY", mod_codes)]

  mod_year <- list()
  mod_name <- list()
  for (i in seq_len(nrow(prog_modules))) {
    mc <- prog_modules$module_code[i]
    if (is.null(mod_year[[mc]])) mod_year[[mc]] <- as.integer(prog_modules$year[i])
    if (is.null(mod_name[[mc]])) mod_name[[mc]] <- prog_modules$module_name[i]
  }

  for (si in seq_len(n_skills)) {
    skill <- all_skills[si]
    sa <- skill_angles[[skill]]

    if (nchar(selected_skill) == 0 || skill != selected_skill) next

    skill_df <- skills_csv[skills_csv$category == category &
                            skills_csv$skill_name == skill &
                            skills_csv$module_code %in% mod_codes, ]
    skill_df <- skill_df[skill_df$level > 0, ]
    skill_df <- skill_df[!duplicated(skill_df$module_code), ]
    n_in_skill <- nrow(skill_df)
    if (n_in_skill == 0) next

    skill_df$year <- sapply(skill_df$module_code, function(mc) {
      y <- mod_year[[mc]]; if (is.null(y)) NA_integer_ else as.integer(y)
    })
    skill_df <- skill_df[!is.na(skill_df$year), ]
    if (nrow(skill_df) == 0) next
    skill_df <- skill_df[order(skill_df$year, skill_df$module_code), ]
    n_in_skill <- nrow(skill_df)
    skill_df$jitter <- 0
    yr_counts <- table(skill_df$year)
    for (y in names(yr_counts)) {
      nc <- as.integer(yr_counts[y])
      if (nc > 1) {
        idx <- which(skill_df$year == y)
        skill_df$jitter[idx] <- seq(-12, 12, length.out = nc)
      }
    }

    for (ri in seq_len(n_in_skill)) {
      row   <- skill_df[ri, ]
      mc    <- row$module_code
      lvl   <- as.character(row$level)

      yr <- skill_df$year[ri]
      if (is.na(yr)) next
      if (yr > 5) yr <- 5
      r <- RADIUS[yr] + skill_df$jitter[ri]

      col   <- LEVEL_COLORS[lvl]
      shape <- LEVEL_SHAPES[lvl]

      frac <- (ri - 1) / max(n_in_skill - 1, 1)
      wedge_center <- (sa$start + sa$end) / 2
      half_mod_deg <- 22 / r * 180 / pi
      avail_deg <- OPENING_WIDTH - 2 * half_mod_deg
      if (avail_deg <= 0) avail_deg <- 2
      angle_deg <- wedge_center - avail_deg/2 + frac * avail_deg
      angle_rad <- angle_deg * pi / 180
      x <- CX + r * sin(angle_rad)
      y <- CY - r * cos(angle_rad)

      label  <- sub("^[A-Z]+", "", mc)
      mname  <- if (!is.null(mod_name[[mc]])) mod_name[[mc]] else mc

      parts <- c(parts, sprintf(
        '<div class="module %s" style="left:%.1fpx;top:%.1fpx;background:%s;border-color:%s;z-index:3" title="%s | %s" data-code="%s" data-year="%s" data-level="%s">%s</div>',
        shape, x - 22, y - 11, col, col,
        mname, LEVEL_LABELS[lvl],
        mc, yr, lvl,
        label
      ))
    }
  }

  # Active wedge overlay
  if (selected_skill != "" && !is.null(skill_angles[[selected_skill]])) {
    sa <- skill_angles[[selected_skill]]
    wedge_center <- (sa$start + sa$end) / 2
    wedge_mid <- wedge_center - 90
    ov_start <- wedge_mid - OPENING_WIDTH / 2
    ov_end   <- wedge_mid + OPENING_WIDTH / 2

    ov_start_css <- (ov_start + 90) %% 360
    ov_end_css <- (ov_end + 90) %% 360

    if (ov_start_css < ov_end_css) {
      conic_str <- sprintf(
        'conic-gradient(rgba(255,255,255,0.85) %.1fdeg %.1fdeg, transparent %.1fdeg %.1fdeg, rgba(255,255,255,0.85) %.1fdeg 360deg)',
        0, ov_start_css, ov_start_css, ov_end_css, ov_end_css
      )
    } else {
      conic_str <- sprintf(
        'conic-gradient(transparent 0deg %.1fdeg, rgba(255,255,255,0.85) %.1fdeg %.1fdeg, transparent %.1fdeg 360deg)',
        ov_end_css, ov_end_css, ov_start_css, ov_start_css
      )
    }

    parts <- c(parts, sprintf(
      '<div class="overlay" style="background: %s;"></div>', conic_str
    ))

    # Wedge label
    display_name <- gsub(" ", "<br>", selected_skill)

    parts <- c(parts, sprintf(
      '<div style="position:absolute;left:%.1fpx;top:%.1fpx;
                   transform:translate(-50%%,-50%%);
                   font-size:11px;font-weight:700;color:#156082;
                   pointer-events:none;z-index:6;
                   text-align:center;line-height:1.2;
                   text-shadow:0 0 6px white,0 0 3px white,0 0 1px white;">%s</div>',
      CX + (max(RADIUS) + 20) * sin(wedge_mid * pi / 180),
      CY - (max(RADIUS) + 20) * cos(wedge_mid * pi / 180),
      display_name
    ))

    # Ring labels — opposite side
    opp_angle <- wedge_mid + 180
    opp_rad <- opp_angle * pi / 180
    for (yi in seq_len(min(max_year, 5))) {
      r <- RADIUS[yi]
      lx <- CX + r * sin(opp_rad)
      ly <- CY - r * cos(opp_rad)
      parts <- c(parts, sprintf(
        '<div class="ring-label" style="left:%.1fpx;top:%.1fpx">Y%d</div>',
        lx - 5, ly - 7, yi
      ))
    }
  }

  paste(parts, collapse = "\n")
}

# ---------------------------------------------------------------------------
# VIRTUAL PROGRAMME BUILDER (degree-level aggregation when no pathway selected)
# ---------------------------------------------------------------------------
build_virtual_programme <- function(degree) {
  all_rows <- programmes_csv[programmes_csv$degree == degree, ]
  if (nrow(all_rows) == 0) return(all_rows)
  pathways_present <- unique(all_rows$pathway[nzchar(all_rows$pathway)])
  if (length(pathways_present) == 0) return(all_rows)

  result <- list()
  vc_list <- list()
  yr_sems <- unique(paste(all_rows$year, all_rows$semester, sep = "|"))

  for (ys in yr_sems) {
    parts <- strsplit(ys, "\\|")[[1]]
    yr <- parts[1]
    sem <- parts[2]

    slot_rows <- all_rows[all_rows$year == yr & all_rows$semester == sem, ]
    compulsory <- slot_rows[slot_rows$status == "compulsory", ]
    optional   <- slot_rows[slot_rows$status == "optional", ]
    pathway    <- slot_rows[slot_rows$status == "pathway", ]

    seen_comp <- character(0)
    for (i in seq_len(nrow(compulsory))) {
      r <- compulsory[i, ]
      key <- paste(r$year, r$semester, r$module_code, r$module_name, r$collection)
      if (!(key %in% seen_comp)) {
        seen_comp <- c(seen_comp, key)
        result[[length(result) + 1]] <- r
      }
    }

    opt_per_pl <- list()
    for (i in seq_len(nrow(optional))) {
      r <- optional[i, ]
      key <- paste(r$year, r$semester, r$module_code, r$module_name, r$collection)
      pl <- as.character(r$programme_label)
      if (is.null(opt_per_pl[[key]])) opt_per_pl[[key]] <- list()
      cnt <- opt_per_pl[[key]][[pl]]
      opt_per_pl[[key]][[pl]] <- (if (is.null(cnt)) 0 else cnt) + 1
    }
    seen_opt <- character(0)
    for (i in seq_len(nrow(optional))) {
      r <- optional[i, ]
      key <- paste(r$year, r$semester, r$module_code, r$module_name, r$collection)
      if (!(key %in% seen_opt)) {
        seen_opt <- c(seen_opt, key)
        n <- max(unlist(opt_per_pl[[key]]))
        for (j in seq_len(n)) {
          result[[length(result) + 1]] <- r
        }
      }
    }

    if (nrow(pathway) > 0) {
      vc_name <- paste0("_VD_", gsub("[^A-Za-z0-9]", "_", degree), "_Y", yr,
                        ifelse(sem == "Autumn", "A", "S"))
      vc_mods <- list()
      for (i in seq_len(nrow(pathway))) {
        r <- pathway[i, ]
        mc <- r$module_code
        hs <- tolower(as.character(r$has_skills)) == "true"
        vc_mods[[length(vc_mods) + 1]] <- list(code = mc, name = r$module_name, has_skills = hs)
      }
      r2 <- pathway[1, ]
      r2$status <- "optional"
      r2$collection <- vc_name
      r2$has_skills <- FALSE
      r2$module_name <- "Pathway option"
      r2$module_code <- vc_name
      result[[length(result) + 1]] <- r2
      vc_list[[vc_name]] <- vc_mods
    }
  }

  prog_df <- do.call(rbind, lapply(result, function(x) {
    as.data.frame(x, stringsAsFactors = FALSE)
  }))
  attr(prog_df, "virtual_collections") <- vc_list
  prog_df
}

# ---------------------------------------------------------------------------
# LOAD COLLECTIONS
# ---------------------------------------------------------------------------
collections_data <- read_csv("collections.csv")
collections <- list()
for (coll_name in unique(collections_data$collection)) {
  sub <- collections_data[collections_data$collection == coll_name, ]
  collections[[coll_name]] <- lapply(seq_len(nrow(sub)), function(i) {
    list(
      code       = sub$module_code[i],
      name       = sub$module_name[i],
      has_skills = tolower(as.character(sub$has_skills[i])) == "true"
    )
  })
}

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
ui <- page_fillable(
  title = "Programme Skills Dashboard",
  theme = bs_theme(version = 5, bg = "#ffffff", fg = "#333333",
                   base_font = "'Inter', sans-serif", "font-size-base" = "0.9rem"),
  fill = FALSE,

  # ---- SECTION 1: Competency Skills Wheel ----
  div(style = "background: #006838; color: white; padding: 12px 24px; margin-bottom: 16px; border-radius: 6px;",
      h3("Competency Skills Wheel", style = "margin: 0; font-weight: 600;")
  ),

  div(
    style = "display: flex; gap: 12px; flex-wrap: wrap; margin-bottom: 12px;",
    div(style = "flex: 1; min-width: 200px;",
      selectInput("degree", "Select Degree",
                  choices = degree_labels)
    ),
    div(style = "flex: 1; min-width: 200px;",
      selectInput("pathway", "Select Pathway",
                  choices = c("\u2014 None \u2014"))
    ),
    div(style = "flex: 1; min-width: 200px;",
      selectInput("skills_area", "Skills Area",
                  choices = c(
                    "UN Competencies" = "UN Competencies",
                    "Computational & Digital" = "computational",
                    "Field" = "field",
                    "Lab" = "lab",
                    "Personal" = "personal"
                  ))
    )
  ),

  uiOutput("comp_buttons_ui"),

  # Wheel + side panel
  div(style = "display: flex; gap: 20px; flex-wrap: wrap; align-items: flex-start;",
      div(
        id = "wheel-container",
        style = sprintf("position: relative; width: %dpx; height: %dpx; flex-shrink: 0;", SIZE, SIZE),
        uiOutput("wheel")
      ),
      div(
        id = "side-panel",
        style = "width: 300px; padding: 8px;",
        uiOutput("comp_info")
      )
  ),

  div(style = "display: flex; gap: 16px; margin-top: 12px; font-size: 0.85rem; color: #555; flex-wrap: wrap;",
      tags$strong("Skill Levels:"),
      div(span(class = "legend-icon legend-fundamentals"), "Fundamentals"),
      div(span(class = "legend-icon legend-basic"), "Basic"),
      div(span(class = "legend-icon legend-advanced"), "Advanced")
  ),

  # ---- SECTION 2: Module Skills Detail ----
  uiOutput("module_skills"),

  # ---- SECTION 3: Programme Skills Map ----
  h4("Programme Skills Map", style = "margin: 0 0 12px 0; font-weight: 700; color: #156082;"),
  uiOutput("module_grid"),
  div(
    style = "margin-top: 8px; margin-bottom: 8px;",
    radioButtons("view_mode", NULL,
                 choices = c("Cumulative" = "cumulative", "Per Year" = "per_year"),
                 selected = "cumulative", inline = TRUE)
  ),
  uiOutput("skills_trajectory"),

  # ---- CSS + JS ----
  tags$head(tags$style(HTML(sprintf('
    .ring {
      position: absolute; border-radius: 50%%; border: 2px solid #ddd;
      pointer-events: none; box-sizing: border-box;
    }
    .ring-label {
      position: absolute; font-size: 11px; font-weight: 700;
      color: #156082; pointer-events: none; z-index: 7;
    }
    .module {
      position: absolute; width: 44px; height: 22px;
      display: flex; align-items: center; justify-content: center;
      font-size: 9px; font-weight: 700; color: white;
      cursor: pointer; box-sizing: border-box;
      transition: transform 0.15s, opacity 0.3s;
    }
    .module:hover { transform: scale(1.35); z-index: 10 !important; }
    .module.rect { border-radius: 2px; border: 2px solid; }
    .module.rounded { border-radius: 11px; border: 2px solid; }
    .module.hexagon {
      clip-path: polygon(25%% 0%%, 75%% 0%%, 100%% 50%%, 75%% 100%%, 25%% 100%%, 0%% 50%%);
      border: none;
    }
    .overlay {
      position: absolute; top: 0; left: 0;
      width: 100%%; height: 100%%; border-radius: 50%%;
      pointer-events: none; z-index: 5;
    }
    .comp-btn {
      transition: all 0.2s;
    }
    .comp-btn:hover {
      transform: scale(1.08);
    }
    .legend-icon {
      display: inline-block; width: 14px; height: 14px;
      vertical-align: middle; margin-right: 4px;
    }
    .legend-fundamentals { background: %s; border-radius: 2px; }
    .legend-basic { background: %s; border-radius: 7px; }
    .legend-advanced {
      background: %s;
      clip-path: polygon(25%% 0%%, 75%% 0%%, 100%% 50%%, 75%% 100%%, 25%% 100%%, 0%% 50%%);
    }
  ',
    LEVEL_COLORS["1"], LEVEL_COLORS["2"], LEVEL_COLORS["3"]
  )))),
  tags$script(HTML('
    document.addEventListener("mouseenter", function(e) {
      var el = e.target.closest(".module");
      if (el) {
        Shiny.setInputValue("hovered_module", {
          code: el.getAttribute("data-code"),
          year: el.getAttribute("data-year"),
          level: el.getAttribute("data-level")
        }, {priority: "event"});
      }
    }, true);
    document.addEventListener("mouseleave", function(e) {
      var el = e.target.closest(".module");
      if (el) {
        Shiny.setInputValue("hovered_module", null, {priority: "event"});
      }
    }, true);
  '))
)

# ===========================================================================
# SERVER
# ===========================================================================
server <- function(input, output, session) {

  selected_comp <- reactiveVal(COMPETENCIES[1])
  hovered_code <- reactiveVal(NULL)
  hovered_year <- reactiveVal(NULL)

  programme_label <- reactive({
    if (is.null(input$pathway) || input$pathway == "" || input$pathway == "\u2014 None \u2014") {
      input$degree
    } else {
      paste0(input$degree, " \u2014 ", input$pathway)
    }
  })

  observeEvent(input$degree, {
    pathways <- unique(programmes_csv$pathway[programmes_csv$degree == input$degree])
    pathways <- pathways[nzchar(pathways)]
    choices <- c("\u2014 None \u2014", pathways)
    sel <- if (input$pathway %in% choices) input$pathway else "\u2014 None \u2014"
    updateSelectInput(session, "pathway", choices = choices, selected = sel)
  })

  selected_skill <- reactiveVal("")

  is_virtual <- reactive({
    deg <- input$degree
    pwy <- input$pathway
    pathways_in_deg <- unique(programmes_csv$pathway[
      programmes_csv$degree == deg & nzchar(programmes_csv$pathway)
    ])
    (is.null(pwy) || pwy == "" || pwy == "\u2014 None \u2014") && length(pathways_in_deg) > 0
  })

  programme_data <- reactive({
    if (is_virtual()) {
      build_virtual_programme(input$degree)
    } else {
      req(programme_label())
      programmes_csv[programmes_csv$programme_label == programme_label(), ]
    }
  })

  collections_all <- reactive({
    vc <- attr(programme_data(), "virtual_collections", exact = TRUE)
    if (is.null(vc) || length(vc) == 0) return(collections)
    c(collections, vc)
  })

  virtual_radial_df <- reactive({
    if (!is_virtual()) return(NULL)
    deg <- input$degree
    rad <- radial_csv[radial_csv$degree == deg, ]
    rad <- rad[!duplicated(paste(rad$module_code, rad$competency, rad$level, rad$year)), ]
    rad
  })

  virtual_mod_codes <- reactive({
    if (!is_virtual()) return(NULL)
    pd <- programme_data()
    codes <- unique(pd$module_code[pd$has_skills == TRUE])
    codes[!grepl("^ANY|^_VD_", codes)]
  })

  lapply(seq_len(N_COMP), function(ci) {
    observeEvent(input[[paste0("btn_", ci)]], {
      selected_comp(COMPETENCIES[ci])
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
  })

  SKILL_CATS <- c("computational", "field", "lab", "personal")
  for (cat in SKILL_CATS) {
    local({
      cat_local <- cat
      skills <- CATEGORY_SKILLS[[cat_local]]
      for (si in seq_along(skills)) {
        local({
          skill_local <- skills[si]
          btn_id <- paste0("skill_", cat_local, "_", si)
          observeEvent(input[[btn_id]], {
            if (selected_skill() == skill_local) {
              selected_skill("")
            } else {
              selected_skill(skill_local)
            }
          }, ignoreInit = TRUE, ignoreNULL = TRUE)
        })
      }
    })
  }

  observeEvent(input$hovered_module, {
    if (is.null(input$hovered_module)) {
      hovered_code(NULL)
      hovered_year(NULL)
    } else {
      hovered_code(input$hovered_module$code)
      hovered_year(input$hovered_module$year)
    }
  })

  # ---- SECTION 1: Competency / Skill Buttons ----
  output$comp_buttons_ui <- renderUI({
    if (input$skills_area == "UN Competencies") {
      div(
        id = "comp-buttons",
        style = "display: flex; gap: 5px; margin-bottom: 16px; flex-wrap: wrap;",
        lapply(seq_len(N_COMP), function(ci) {
          comp <- COMPETENCIES[ci]
          actionButton(
            inputId = paste0("btn_", ci),
            label = HTML(sprintf(
              '<span style="display:inline-block;width:8px;height:8px;border-radius:50%%;
                            background:%s;margin-right:4px;"></span>%s',
              COMP_COLORS[ci], comp
            )),
            class = "comp-btn",
            style = sprintf(
              "padding:4px 10px; border-radius:14px; border:2px solid %s;
               font-size:0.75rem; font-weight:600; background:white; color:%s;
               cursor:pointer;",
              COMP_COLORS[ci], COMP_COLORS[ci]
            )
          )
        })
      )
    } else {
      cat <- input$skills_area
      skills <- CATEGORY_SKILLS[[cat]]
      if (is.null(skills) || length(skills) == 0) return(NULL)
      selected <- selected_skill()

      btn_list <- lapply(seq_along(skills), function(si) {
        skill <- skills[si]
        btn_id <- paste0("skill_", cat, "_", si)
        is_active <- skill == selected
        bg <- if (is_active) "#156082" else "white"
        tc <- if (is_active) "white" else "#333"
        style_hover <- if (!is_active) "scale(1.08)" else "none"

        actionButton(
          inputId = btn_id,
          label = skill,
          class = "comp-btn",
          style = sprintf(
            "padding:4px 10px; border-radius:14px; border:2px solid #156082;
             font-size:0.7rem; font-weight:600; background:%s; color:%s;
             cursor:pointer;", bg, tc
          )
        )
      })

      div(
        id = "skill-buttons",
        style = "display: flex; gap: 4px; margin-bottom: 16px; flex-wrap: wrap; padding: 2px;",
        tagList(btn_list)
      )
    }
  })

  # ---- SECTION 1: Wheel ----
  output$wheel <- renderUI({
    if (input$skills_area == "UN Competencies") {
      if (is_virtual()) {
        HTML(build_wheel_html(programme_label(), selected_comp(),
                              df_override = virtual_radial_df()))
      } else {
        HTML(build_wheel_html(programme_label(), selected_comp()))
      }
    } else {
      if (is_virtual()) {
        HTML(build_skills_wheel_html(programme_label(), input$skills_area,
                                     selected_skill = selected_skill(),
                                     prog_modules_override = programme_data()))
      } else {
        HTML(build_skills_wheel_html(programme_label(), input$skills_area,
                                     selected_skill = selected_skill()))
      }
    }
  })

  output$comp_info <- renderUI({
    if (input$skills_area != "UN Competencies") {
      cat <- input$skills_area
      label <- CATEGORY_LABELS[[cat]]
      sel_skill <- selected_skill()

      all_skills <- CATEGORY_SKILLS[[cat]]
      if (is.null(all_skills) || length(all_skills) == 0) {
        return(div(h4(label), p("No skills data for this category.")))
      }

      prog_mod <- programme_data()
      mod_codes <- unique(prog_mod$module_code[prog_mod$has_skills == TRUE])
      mod_codes <- mod_codes[!grepl("^ANY|^_VD_", mod_codes)]

      skills_to_show <- if (nzchar(sel_skill)) sel_skill else all_skills

      skill_items <- lapply(skills_to_show, function(skill_name) {
        sub <- skills_csv[skills_csv$category == cat &
                          skills_csv$skill_name == skill_name &
                          skills_csv$module_code %in% mod_codes, ]
        sub <- sub[sub$level > 0, ]
        if (nrow(sub) == 0) return(NULL)
        sub <- sub[!duplicated(sub$module_code), ]

        sub$year <- prog_mod$year[match(sub$module_code, prog_mod$module_code)]
        sub$semester <- prog_mod$semester[match(sub$module_code, prog_mod$module_code)]
        sub <- sub[!is.na(sub$year), ]
        sem_order <- ifelse(is.na(sub$semester) | sub$semester == "Autumn", 1, 2)
        sub <- sub[order(sub$year, sem_order, sub$module_code), ]

        badge_text <- sprintf("%d module(s)", nrow(sub))
        div(style = "margin-bottom: 6px;",
          div(style = "font-weight: 600; font-size: 0.85rem;", skill_name),
          div(style = "font-size: 0.8rem; color: #666; margin-bottom: 2px;", badge_text),
          lapply(seq_len(nrow(sub)), function(ri) {
            r <- sub[ri, ]
            lvl <- as.character(r$level)
            col <- LEVEL_COLORS[lvl]
            lbl <- LEVEL_LABELS[lvl]
            div(style = paste0("display:flex; align-items:center; gap:4px; padding:2px 6px;",
                               "font-size:0.8rem; border-left: 3px solid ", col, "; margin-bottom:2px;"),
              span(r$module_code, style = "font-weight:600; color:#333;"),
              span(paste0("Y", r$year), style = "color:#888; font-size:0.75rem;"),
              span(lbl, style = paste0("margin-left:auto; color:", col, "; font-size:0.7rem;",
                                       "padding:1px 6px; background:", col, "15; border-radius:8px;"))
            )
          })
        )
      })
      skill_items <- Filter(Negate(is.null), skill_items)

      return(div(
        h4(label, style = "margin: 0 0 6px 0; font-weight: 700; color: #156082;"),
        p(if (length(skill_items) == 0) "No skills available"
          else paste0(length(skill_items), if (nzchar(sel_skill)) " skill shown" else " skills in this area"),
          style = "font-size: 0.85rem; color: #555; margin-bottom: 8px;"),
        hr(style = "margin: 8px 0;"),
        do.call(tagList, skill_items)
      ))
    }

    comp <- selected_comp()
    desc <- COMP_DESCRIPTIONS[[comp]]

    df <- if (is_virtual()) {
      virtual_radial_df()
    } else {
      radial_csv[radial_csv$programme == programme_label(), ]
    }
    df <- df[df$competency == comp, ]
    df <- df[df$level != "0", ]
    df <- df[order(df$year, df$module_code), ]

    module_items <- ""
    if (nrow(df) > 0) {
      items <- apply(df, 1, function(r) {
        lvl <- r["level"]
        col <- LEVEL_COLORS[lvl]
        lbl <- LEVEL_LABELS[lvl]
        is_hovered <- !is.null(hovered_code()) &&
          r["module_code"] == hovered_code() &&
          r["year"] == hovered_year()
        bg <- if (is_hovered) "#d0e8f5" else "#f8f9fa"
        sprintf(
          '<div style="display:flex; align-items:center; gap:6px; margin-bottom:4px;
                      padding:4px 8px; background:%s; border-radius:4px;
                      border-left: 4px solid %s; font-size:0.85rem;">
             <span style="font-weight:700; color:#333;">%s</span>
             <span style="color:#888; font-size:0.75rem;">Y%s</span>
             <span style="margin-left:auto; color:%s; font-size:0.75rem; padding:1px 6px;
                    background:%s15; border-radius:8px;">%s</span>
           </div>',
          bg, col, r["module_code"], r["year"], col, col, lbl
        )
      })
      module_items <- paste(items, collapse = "\n")
    } else {
      module_items <- '<div style="color:#999; font-style:italic; font-size:0.85rem;">No modules in this programme develop this competency.</div>'
    }

    div(
      h4(comp, style = "margin: 0 0 6px 0; font-weight: 700; color: #156082;"),
      p(desc, style = "font-size: 0.85rem; color: #555; line-height: 1.4; margin-bottom: 8px;"),
      hr(style = "margin: 8px 0;"),
      h5("Modules", style = "margin: 6px 0; font-weight: 600; font-size: 0.9rem;"),
      HTML(module_items)
    )
  })

  # ---- SECTION 2: Module Skills Detail ----
  output$module_skills <- renderUI({
    code <- hovered_code()
    if (is.null(code)) {
      return(div(
        style = "color: #999; font-style: italic; font-size: 0.85rem; padding: 8px;",
        "Hover a module above to see details."
      ))
    }

    mod <- module_meta[[code]]
    if (is.null(mod)) {
      return(div(
        style = "color: #999; font-size: 0.85rem; padding: 8px;",
        paste0("No data available for ", code, ".")
      ))
    }

    info_div <- div(
      style = "margin-bottom: 10px;",
      h4(paste0(mod$name, " (", code, ")"),
         style = "margin-bottom: 2px; color: #156082;"),
      div(style = "color: #555; font-size: 0.9rem;",
        paste0("Coordinator: ", mod$coordinator, " | Year ", mod$year,
               ", Semester ", mod$semester)
      )
    )

    topics_div <- NULL
    if (length(mod$topics) > 0) {
      badges <- lapply(mod$topics, function(t) {
        span(style = paste0("display:inline-block; padding:2px 10px; margin:2px;",
                            "background:", COL_ACCENT, "; color: white; border-radius: 12px;",
                            "font-size: 0.8rem;"), t)
      })
      topics_div <- div(style = "margin-bottom: 8px;",
        strong("Topics: "), tags$span(badges)
      )
    }

    div(
      style = "padding: 12px; background: #fafafa; border-radius: 6px; border: 1px solid #e0e0e0;",
      info_div,
      topics_div
    )
  })

  # ---- SECTION 3: Programme Skills Map ----
  current_programme <- reactive({
    programme_data()
  })

  # Module Grid
  output$module_grid <- renderUI({
    prog_df <- current_programme()
    if (nrow(prog_df) == 0) return(NULL)

    years_list <- list()
    for (i in seq_len(nrow(prog_df))) {
      row <- prog_df[i, ]
      yr <- as.character(row$year)
      per <- row$semester
      if (is.null(years_list[[yr]])) years_list[[yr]] <- list()
      if (is.null(years_list[[yr]][[per]])) years_list[[yr]][[per]] <- list()
      years_list[[yr]][[per]] <- append(years_list[[yr]][[per]], list(row))
    }

    year_divs <- lapply(sort(as.integer(names(years_list))), function(yr) {
      year_data <- years_list[[as.character(yr)]]

      make_semester_col <- function(mods, period) {
        if (is.null(mods)) return(NULL)

        status_priority <- c(compulsory = 0, pathway = 1, optional = 2)
        mods <- mods[order(sapply(mods, function(m) {
          p <- status_priority[[as.character(m$status)]]
          if (is.null(p)) 99 else p
        }))]

        coll_seen <- list()
        items <- lapply(mods, function(m) {
          is_any <- grepl("^ANY", m$module_code)
          is_coll <- nchar(trimws(m$collection)) > 0

          if (is_any) {
            div(
              style = paste0("padding: 4px 8px; margin-bottom: 3px; border-left: 4px solid #ccc;",
                             "background: #f0f0f0; border-radius: 0 4px 4px 0; font-size: 0.8rem;",
                             "color: #999; font-style: italic;"),
              "Optional module"
            )
          } else if (is_coll) {
            coll_code <- m$collection
            key <- semester_key(yr, period)
            cnt <- coll_seen[[coll_code]]
            if (is.null(cnt)) {
              coll_seen[[coll_code]] <- 1
              input_id <- paste0("opt_", key, "_", coll_code)
            } else {
              coll_seen[[coll_code]] <- cnt + 1
              input_id <- paste0("opt_", key, "_", coll_code, "_", cnt + 1)
            }

            coll_modules <- collections_all()[[coll_code]]

            if (is.null(coll_modules) || length(coll_modules) == 0) {
              div(
                style = "padding: 4px 8px; margin-bottom: 3px; border-left: 4px solid #ccc;",
                "No modules available"
              )
            } else {
              choice_names <- sapply(coll_modules, function(cm) {
                paste0(cm$name, " (", cm$code, ")")
              })
              choice_vals <- sapply(coll_modules, function(cm) cm$code)
              names(choice_vals) <- choice_names

              div(
                style = paste0("padding: 4px 8px; margin-bottom: 3px; border-left: 4px solid ",
                               STATUS_COLS[["optional"]], "; background: #f8f9fa; border-radius: 0 4px 4px 0;"),
                selectizeInput(input_id, NULL,
                               choices = choice_vals,
                               selected = character(0),
                               multiple = FALSE,
                               options = list(
                                 placeholder = "Select optional module...",
                                 maxItems = 1,
                                 onInitialize = I('function() { this.setValue(""); }')
                               ))
              )
            }
          } else {
            status_col <- STATUS_COLS[[m$status]]
            if (is.null(status_col)) status_col <- COL_LIGHT

            has_skills_val <- tolower(as.character(m$has_skills)) == "true"
            skills_icon <- if (has_skills_val) "\u2713" else "\u2717"
            skills_col <- if (has_skills_val) COL_DARK else "#cc0000"

            name_display <- m$module_name
            if (nchar(name_display) > 30) name_display <- paste0(substr(name_display, 1, 27), "...")

            div(
              style = paste0("padding: 4px 8px; margin-bottom: 3px; border-left: 4px solid ",
                             status_col, "; background: #f8f9fa; border-radius: 0 4px 4px 0; font-size: 0.8rem;"),
              span(style = paste0("color:", skills_col, "; font-weight: bold; margin-right: 4px;"), skills_icon),
              span(style = "color: #666; margin-right: 4px;", m$module_code),
              name_display
            )
          }
        })

        div(
          style = "margin-bottom: 4px;",
          div(style = "font-weight: 600; font-size: 0.85rem; color: #555; margin-bottom: 4px;", period),
          do.call(tagList, items)
        )
      }

      autumn_html <- make_semester_col(year_data[["Autumn"]], "Autumn")
      spring_html <- make_semester_col(year_data[["Spring"]], "Spring")

      div(
        style = "flex: 1; min-width: 200px; margin-bottom: 16px;",
        div(
          style = paste0("background:", COL_DARK, "; color: white; padding: 6px 12px;",
                         "border-radius: 4px 4px 0 0; font-weight: 600; text-align: center;"),
          paste0("Year ", yr)
        ),
        div(
          style = "border: 1px solid #dee2e6; border-radius: 0 0 4px 4px; padding: 8px;",
          autumn_html,
          hr(style = "margin: 6px 0;"),
          spring_html
        )
      )
    })

    legend <- div(
      style = "margin-bottom: 16px; font-size: 0.8rem; color: #666;",
      tags$strong("Legend: "),
      span(style = paste0("color:", COL_DARK, ";"), " \u25CF Compulsory"),
      span(style = paste0("color:", COL_ACCENT, "; margin-left: 8px;"), " \u25D0 Pathway"),
      span(style = paste0("color:", COL_LIGHT, "; margin-left: 8px;"), " \u25CB Optional"),
      span(style = "margin-left: 8px;", " \u2713 Has skills data"),
      span(style = "color: #cc0000; margin-left: 8px;", " \u2717 Data missing")
    )

    tagList(
      legend,
      div(style = "display: flex; gap: 12px; flex-wrap: wrap;", year_divs)
    )
  })

  # Skills Trajectory
  output$skills_trajectory <- renderUI({
    req(input$view_mode)
    prog_df <- current_programme()
    if (nrow(prog_df) == 0) return(NULL)

    # Collect selected optional modules
    all_inputs <- reactiveValuesToList(input)
    selected_codes <- list()
    for (nm in names(all_inputs)) {
      if (grepl("^opt_", nm)) {
        val <- all_inputs[[nm]]
        if (!is.null(val) && nchar(val) > 0) {
          selected_codes[[nm]] <- val
        }
      }
    }

    # Group modules by semester
    sem_list <- list()
    for (i in seq_len(nrow(prog_df))) {
      row <- prog_df[i, ]
      key <- semester_key(row$year, row$semester)
      if (is.null(sem_list[[key]])) sem_list[[key]] <- list()
      sem_list[[key]] <- append(sem_list[[key]], list(row))
    }

    sem_order <- names(sem_list)

    category_divs <- lapply(names(CATEGORY_LABELS), function(cat) {
      all_skills <- sort(unique(skills_csv$skill_name[skills_csv$category == cat]))
      if (length(all_skills) == 0) return(NULL)

      # For each semester, collect skill levels
      sem_skills <- list()
      for (key in sem_order) {
        mods <- sem_list[[key]]
        levels <- setNames(rep(NA_integer_, length(all_skills)), all_skills)

        for (m in mods) {
          mc <- m$module_code
          has_skills_val <- tolower(as.character(m$has_skills)) == "true"
          is_coll <- nchar(trimws(m$collection)) > 0

          if (has_skills_val && !grepl("^ANY", mc) && !is_coll) {
            sk <- skills_lookup[[mc]]
            if (!is.null(sk)) {
              for (skill_name in all_skills) {
                val <- sk[[cat]][skill_name]
                if (!is.null(val) && !is.na(val)) {
                  if (is.na(levels[skill_name])) levels[skill_name] <- val
                  else levels[skill_name] <- max(levels[skill_name], val)
                }
              }
            }
          }

          if (is_coll) {
            input_id <- paste0("opt_", key, "_", m$collection)
            sel <- selected_codes[[input_id]]
            if (!is.null(sel) && nchar(sel) > 0) {
              sk <- skills_lookup[[sel]]
              if (!is.null(sk)) {
                for (skill_name in all_skills) {
                  val <- sk[[cat]][skill_name]
                  if (!is.null(val) && !is.na(val)) {
                    if (is.na(levels[skill_name])) levels[skill_name] <- val
                    else levels[skill_name] <- max(levels[skill_name], val)
                  }
                }
              }
            }
          }
        }
        sem_skills[[key]] <- levels
      }

      # Badge: cumulative max
      badge_levels <- list()
      for (skill_name in all_skills) {
        max_val <- NA_integer_
        for (key in sem_order) {
          val <- sem_skills[[key]][[skill_name]]
          if (!is.na(val)) {
            if (is.na(max_val)) max_val <- val
            else max_val <- max(max_val, val)
          }
        }
        badge_levels[[skill_name]] <- max_val
      }

      # Cumulative mode
      if (input$view_mode == "cumulative" && length(sem_skills) >= 2) {
        for (i in 2:length(sem_skills)) {
          prev <- sem_skills[[i - 1]]
          curr <- sem_skills[[i]]
          for (skill_name in names(curr)) {
            if (!is.na(prev[skill_name]) && !is.na(curr[skill_name])) {
              curr[skill_name] <- max(prev[skill_name], curr[skill_name])
            } else if (!is.na(prev[skill_name])) {
              curr[skill_name] <- prev[skill_name]
            }
          }
          sem_skills[[i]] <- curr
        }
      }

      # Build skill rows
      skill_rows <- lapply(all_skills, function(skill_name) {
        current_level <- badge_levels[[skill_name]]

        if (is.na(current_level)) {
          badge_col <- "#cccccc"
          badge_text <- "No data"
        } else {
          badge_col <- SKILL_COLS[current_level + 1]
          badge_text <- SKILL_LABS[current_level + 1]
        }

        dots <- lapply(sem_order, function(key) {
          val <- sem_skills[[key]][[skill_name]]
          if (is.na(val)) {
            span(
              style = "display:inline-block; width: 36px; height: 36px; line-height: 36px; text-align: center; font-size: 18px; color: #ddd;",
              "\u2014"
            )
          } else {
            span(
              style = "display:inline-block; width: 36px; height: 36px; text-align: center; vertical-align: middle;",
              skill_circle(val)
            )
          }
        })

        div(
          style = "display: flex; align-items: center; margin-bottom: 4px;",
          div(
            style = "width: 200px; font-size: 0.8rem; color: #333; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;",
            title = skill_name,
            skill_name
          ),
          span(
            style = paste0("display:inline-block; width: 120px; text-align: center;",
                           "padding: 1px 4px; margin-right: 8px;",
                           "background:", badge_col, "; color: white; border-radius: 10px;",
                           "font-size: 0.7rem; white-space: nowrap;"),
            badge_text
          ),
          div(style = "display: flex; gap: 2px;", dots)
        )
      })

      header_dots <- lapply(sem_order, function(key) {
        div(style = "width: 36px; text-align: center; font-size: 0.65rem; color: #888;", key)
      })

      div(
        style = "margin-bottom: 24px;",
        h5(CATEGORY_LABELS[cat], style = "margin-bottom: 8px; font-weight: 600;"),
        div(
          style = "display: flex; align-items: center; margin-bottom: 6px;",
          div(style = "width: 200px;", ""),
          div(style = "width: 120px;", ""),
          div(style = "display: flex; gap: 2px;", header_dots)
        ),
        do.call(tagList, skill_rows)
      )
    })

    skill_legend <- div(
      style = "margin-bottom: 12px; font-size: 0.8rem; color: #666; display: flex; align-items: center; gap: 12px; flex-wrap: wrap;",
      tags$strong("Skill Levels: "),
      span(skill_circle(0), " Not developed"),
      span(skill_circle(1), " Fundamentals"),
      span(skill_circle(2), " Basic"),
      span(skill_circle(3), " Advanced"),
      span(style = "color: #ccc;", " \u2014 No data")
    )

    tagList(
      skill_legend,
      do.call(tagList, Filter(Negate(is.null), category_divs))
    )
  })
}

shinyApp(ui, server)
