#!/usr/bin/env Rscript
# =============================================================================
# Build tidy CSV datasets from raw JSON sources
#
# Reads programmes.json + skills_data.json from this directory,
# produces:
#   programmes.csv       - One row per module slot per programme
#   skills.csv           - One row per (module, category, skill, level)
#   competencies.csv     - UN competency levels per (programme, module)
#   topics.csv           - Module topics
#   meta/collection_*.csv - Collection module pools
#
# Run from this directory:  source("build_data.R")
# The CSVs are human-editable in Excel or a text editor.
# =============================================================================

library(jsonlite)

data_dir <- tryCatch(
  dirname(sys.frame(1)$ofile),
  error = function(e) "."
)

read_json_file <- function(filename) {
  f <- file.path(data_dir, filename)
  if (!file.exists(f)) stop(paste("Cannot find", f))
  fromJSON(paste(readLines(f, warn = FALSE), collapse = "\n"),
           simplifyVector = FALSE)
}

prog <- read_json_file("programmes.json")
skill <- read_json_file("skills_data.json")

# ---------------------------------------------------------------------------
# UN competency mapping: personal skill name -> UN competency
# ---------------------------------------------------------------------------
UN_COMP_MAP <- list(
  "Adaptability"                  = "Adaptability",
  "Conflict Resolution"           = "Norms and Conflict Resolution",
  "Critical Thinking"             = "Critical Thinking",
  "Innovation"                    = "Innovation",
  "Integrated Problem-solving"    = "Integrated Problem Solving",
  "Self-awareness"                = "Self-Awareness",
  "Systems Thinking"              = "Systems Thinking",
  "Teamwork & Collaboration"      = "Teamwork & Collaboration"
)

# ---------------------------------------------------------------------------
# Build module skills lookup (module_code -> personal skills)
# ---------------------------------------------------------------------------
skills_lookup <- list()
topics_lookup <- list()
for (m in skill$modules) {
  codes <- trimws(strsplit(m$code, " and ")[[1]])
  for (c in codes) {
    skills_lookup[[c]] <- m$personal
    topics_lookup[[c]] <- m$topics
  }
}

# ---------------------------------------------------------------------------
# 1. programmes.csv
# ---------------------------------------------------------------------------
prog_rows <- list()
for (p in prog$programmes) {
  label <- p$label
  degree <- p$degree
  pathway <- p$pathway
  for (sem in p$semesters) {
    for (mod in sem$modules) {
      prog_rows <- append(prog_rows, list(list(
        programme_label = label,
        degree          = degree,
        pathway         = pathway,
        year            = as.character(sem$year),
        semester        = sem$period,
        module_code     = mod$code,
        module_name     = mod$name,
        status          = mod$status,
        collection      = if (!is.null(mod$collection)) mod$collection else ""
      )))
    }
  }
}
prog_df <- do.call(rbind, lapply(prog_rows, as.data.frame, stringsAsFactors = FALSE))
# Add has_skills column
prog_df$has_skills <- sapply(prog_df$module_code, function(c) {
  if (grepl("^ANY", c)) return(FALSE)
  codes <- trimws(strsplit(c, " and ")[[1]])
  any(sapply(codes, function(x) !is.null(skills_lookup[[x]])))
})
write.csv(prog_df, file.path(data_dir, "programmes.csv"),
          row.names = FALSE, na = "")
cat("Wrote programmes.csv:", nrow(prog_df), "rows\n")

# ---------------------------------------------------------------------------
# 2. skills.csv (long format: one row per module x category x skill)
# ---------------------------------------------------------------------------
skill_rows <- list()
for (m in skill$modules) {
  codes <- trimws(strsplit(m$code, " and ")[[1]])
  for (c in codes) {
    for (cat_name in c("computational", "field", "lab", "personal")) {
      vals <- m[[cat_name]]
      if (is.null(vals)) next
      for (skill_name in names(vals)) {
        skill_rows <- append(skill_rows, list(list(
          module_code = c,
          category    = cat_name,
          skill_name  = skill_name,
          level       = as.character(vals[[skill_name]])
        )))
      }
    }
  }
}
skill_df <- do.call(rbind, lapply(skill_rows, as.data.frame, stringsAsFactors = FALSE))
write.csv(skill_df, file.path(data_dir, "skills.csv"),
          row.names = FALSE, na = "")
cat("Wrote skills.csv:", nrow(skill_df), "rows\n")

# ---------------------------------------------------------------------------
# 3. topics.csv
# ---------------------------------------------------------------------------
topic_rows <- list()
for (m in skill$modules) {
  codes <- trimws(strsplit(m$code, " and ")[[1]])
  for (c in codes) {
    for (t in m$topics) {
      topic_rows <- append(topic_rows, list(list(
        module_code = c,
        topic       = t
      )))
    }
  }
}
topic_df <- do.call(rbind, lapply(topic_rows, as.data.frame, stringsAsFactors = FALSE))
write.csv(topic_df, file.path(data_dir, "topics.csv"),
          row.names = FALSE, na = "")
cat("Wrote topics.csv:", nrow(topic_df), "rows\n")

# ---------------------------------------------------------------------------
# 4. competencies.csv (UN competencies per programme x module)
# ---------------------------------------------------------------------------
comp_rows <- list()
for (p in prog$programmes) {
  label <- p$label
  for (sem in p$semesters) {
    for (mod in sem$modules) {
      if (mod$status != "compulsory" && mod$status != "pathway") next
      codes <- trimws(strsplit(mod$code, " and ")[[1]])
      for (c in codes) {
        if (grepl("^ANY", c)) next
        personals <- skills_lookup[[c]]
        if (is.null(personals)) next
        for (un_name in names(UN_COMP_MAP)) {
          skill_name <- UN_COMP_MAP[[un_name]]
          level <- personals[[un_name]]
          if (is.null(level)) next
          if (level < 1) next
          comp_rows <- append(comp_rows, list(list(
            programme   = label,
            module_code = c,
            competency  = skill_name,
            level       = as.character(level)
          )))
        }
      }
    }

    # Systems Thinking composite
    for (mod in sem$modules) {
      if (mod$status != "compulsory" && mod$status != "pathway") next
      codes <- trimws(strsplit(mod$code, " and ")[[1]])
      for (c in codes) {
        if (grepl("^ANY", c)) next
        personals <- skills_lookup[[c]]
        st_level <- 0
        if (!is.null(personals)) {
          ct <- personals[["Critical Thinking"]]
          ip <- personals[["Integrated Problem-solving"]]
          vals <- c(if (!is.null(ct)) ct, if (!is.null(ip)) ip)
          if (length(vals) > 0) {
            st_level <- round(mean(vals))
            if (st_level < 1) st_level <- 1
          }
        }
        comp_rows <- append(comp_rows, list(list(
          programme   = label,
          module_code = c,
          competency  = "Systems Thinking",
          level       = as.character(st_level)
        )))
      }
    }
  }
}

# Deduplicate competencies
comp_df <- do.call(rbind, lapply(comp_rows, as.data.frame, stringsAsFactors = FALSE))
comp_df <- unique(comp_df)
write.csv(comp_df, file.path(data_dir, "competencies.csv"),
          row.names = FALSE, na = "")
cat("Wrote competencies.csv:", nrow(comp_df), "rows\n")

# ---------------------------------------------------------------------------
# 5. Collection CSVs (meta/collection_*.csv)
# ---------------------------------------------------------------------------
meta_dir <- file.path(data_dir, "meta")
dir.create(meta_dir, showWarnings = FALSE, recursive = TRUE)
for (coll_name in names(prog$collections)) {
  coll_mods <- prog$collections[[coll_name]]
  coll_rows <- lapply(coll_mods, function(m) {
    has_skills <- !is.null(skills_lookup[[m$code]])
    list(module_code = m$code, module_name = m$name, has_skills = tolower(as.character(has_skills)))
  })
  coll_df <- do.call(rbind, lapply(coll_rows, as.data.frame, stringsAsFactors = FALSE))
  coll_file <- file.path(meta_dir, paste0("collection_", coll_name, ".csv"))
  write.csv(coll_df, coll_file, row.names = FALSE, na = "")
  cat("Wrote", coll_file, "-", nrow(coll_df), "rows\n")
}

cat("All dataset files written.\n")
