# Main analysis for the thesis.
#
# This script writes generated thesis tables and figures to:
#   MT2/thesis_outputs/
#
# Main run:
#   Rscript --vanilla thesis_analysis.R --run
#
# Environment check:
#   Rscript --vanilla thesis_analysis.R --check-only
#
# Reproduction order for the current thesis outputs:
#   1. Rscript --vanilla thesis_analysis.R --check-only
#   2. Rscript --vanilla thesis_analysis.R --run
#   3. Rscript --vanilla thesis_analysis.R --migration-only
#   4. Rscript --vanilla make_conley_radius_robustness_main_yields.R
#   5. Rscript --vanilla make_ibge_selected_crops_area_value_scope_figure.R
#   6. Rscript --vanilla make_supervisor_response_tables.R
# Run the migration module after the full pipeline because it regenerates the
# last-move municipal migration tables used in the current manuscript.
# Raw data are not bundled with the public code. In particular, the migration
# module requires a licensed IPUMS International extract supplied by the user.
# The full entry point loads several large panels in one R session and can use
# more than 16 GB of RAM. The targeted entry points below are easier to audit.

options(stringsAsFactors = FALSE, scipen = 999)

# =============================================================================
# 1. Runtime configuration, file paths, and shared output helpers
# =============================================================================
# Run this file from the project root. All paths below are relative to the
# current working directory, and generated files are written under MT2/.
PROJECT_DIR <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

paths <- list(
  project_dir = PROJECT_DIR,
  data_dir = file.path(PROJECT_DIR, "Data"),
  pam_rds = file.path(PROJECT_DIR, "Data", "PAM.rds"),
  pam_zeros_rds = file.path(PROJECT_DIR, "Data", "PAM_with_zeros.rds"),
  sfd_rds = file.path(PROJECT_DIR, "Data", "PAM_SFD_rigorous_clean_corrected.rds"),
  pam_geo2_rds = file.path(PROJECT_DIR, "Data", "PAM_geo2.rds"),
  ipums_parquet = file.path(PROJECT_DIR, "Data", "IPUMS", "ipumsi_00004.parquet"),
  ipums_geo2_shp = file.path(PROJECT_DIR, "Data", "IPUMS", "shp", "geo2_br1980_2010.shp"),
  muni_ref = file.path(PROJECT_DIR, "Data", "muni_geographic_reference.csv"),
  total_agri = file.path(PROJECT_DIR, "Data", "dt_total_agri_muni.rds"),
  abandonment = file.path(PROJECT_DIR, "Data", "agri_abandonment_muni_year_1986_2018.rds"),
  salinity_dir = file.path(PROJECT_DIR, "Salinity data", "salinity_data"),
  out_dir = file.path(PROJECT_DIR, "MT2", "thesis_outputs"),
  sidra_dir = file.path(PROJECT_DIR, "MT2", "thesis_outputs", "sidra_raw")
)

required_packages <- c(
  "arrow", "conleyreg", "curl", "data.table", "DBI", "duckdb", "fixest", "ggplot2",
  "jsonlite", "sf", "sidrar", "stringi", "terra"
)

ensure_user_library <- function() {
  user_lib <- Sys.getenv("R_LIBS_USER")
  if (!nzchar(user_lib)) {
    user_lib <- file.path(
      Sys.getenv("LOCALAPPDATA"),
      "R",
      "win-library",
      paste0(R.version$major, ".", R.version$minor)
    )
    Sys.setenv(R_LIBS_USER = user_lib)
  }
  if (!dir.exists(user_lib)) dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
  .libPaths(unique(c(user_lib, .libPaths())))
  invisible(user_lib)
}

install_missing_packages <- function(pkgs) {
  # Missing dependencies are installed in the user library, never in the
  # project directory. Use --check-only first to inspect the environment.
  ensure_user_library()
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) == 0L) return(invisible(character()))

  message("Installing missing R packages: ", paste(missing, collapse = ", "))
  install.packages(
    missing,
    repos = "https://cloud.r-project.org",
    dependencies = c("Depends", "Imports", "LinkingTo")
  )

  still_missing <- missing[!vapply(missing, requireNamespace, logical(1), quietly = TRUE)]
  if (length(still_missing) > 0L) {
    stop("Packages still missing after installation: ", paste(still_missing, collapse = ", "))
  }

  invisible(missing)
}

load_required_packages <- function() {
  install_missing_packages(required_packages)
  suppressPackageStartupMessages({
    library(arrow)
    library(data.table)
    library(DBI)
    library(duckdb)
    library(fixest)
    library(ggplot2)
    library(sf)
    library(sidrar)
    library(stringi)
  })
}

try_set_french_locale <- function() {
  candidates <- c("French_France.utf8", "French_France.1252", "fr_FR.UTF-8")
  for (loc in candidates) {
    res <- suppressWarnings(try(Sys.setlocale("LC_ALL", loc), silent = TRUE))
    if (!inherits(res, "try-error") && !is.na(res)) return(res)
  }
  warning("Could not set a French locale. Current locale is: ", Sys.getlocale())
  Sys.getlocale()
}

ensure_output_dir <- function() {
  if (!dir.exists(paths$out_dir)) dir.create(paths$out_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(paths$sidra_dir)) dir.create(paths$sidra_dir, recursive = TRUE, showWarnings = FALSE)
}

write_status <- function(...) {
  ensure_output_dir()
  line <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = ""))
  cat(line, "\n", file = file.path(paths$out_dir, "full_revision_run.log"), append = TRUE)
}

as_number <- function(x) suppressWarnings(as.numeric(as.character(x)))

weighted_mean_safe <- function(x, w = NULL) {
  x <- as.numeric(x)
  if (is.null(w)) {
    if (all(is.na(x))) return(NA_real_)
    return(mean(x, na.rm = TRUE))
  }
  w <- as.numeric(w)
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) {
    if (all(is.na(x))) return(NA_real_)
    return(mean(x, na.rm = TRUE))
  }
  sum(x[ok] * w[ok]) / sum(w[ok])
}

scale_safe <- function(x) {
  x <- as.numeric(x)
  s <- stats::sd(x, na.rm = TRUE)
  m <- mean(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(rep(NA_real_, length(x)))
  (x - m) / s
}

clean_name <- function(x) {
  x <- stringi::stri_trans_general(as.character(x), "Latin-ASCII")
  x <- tolower(x)
  gsub("[^a-z0-9]", "", x)
}

latex_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([#$%&_{}])", "\\\\\\1", x, perl = TRUE)
  x <- gsub("~", "\\\\textasciitilde{}", x, fixed = TRUE)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  x
}

fmt <- function(x, digits = 3) {
  ifelse(
    is.na(x),
    "",
    formatC(as.numeric(x), format = "f", digits = digits, big.mark = ",")
  )
}

stars <- function(p) {
  fifelse(
    is.na(p), "",
    fifelse(p < 0.01, "***", fifelse(p < 0.05, "**", fifelse(p < 0.1, "*", "")))
  )
}

write_latex_df <- function(dt, file, caption, label, note = NULL, size = "\\small", landscape = FALSE) {
  ensure_output_dir()
  dt <- as.data.table(dt)
  dt_chr <- copy(dt)
  for (j in names(dt_chr)) dt_chr[[j]] <- as.character(dt_chr[[j]])
  align <- paste(rep("l", ncol(dt_chr)), collapse = "")
  use_resize <- ncol(dt_chr) > 8L
  lines <- c()
  if (landscape) lines <- c(lines, "\\begin{landscape}")
  lines <- c(
    lines,
    "\\begin{table}[!htbp]",
    "\\color{red}",
    "\\centering",
    paste0("\\caption{", latex_escape(caption), "}"),
    paste0("\\label{", label, "}"),
    size,
    "\\setlength{\\tabcolsep}{3pt}"
  )
  if (use_resize) lines <- c(lines, "\\resizebox{\\linewidth}{!}{%")
  lines <- c(
    lines,
    paste0("\\begin{tabular}{", align, "}"),
    "\\toprule",
    paste(latex_escape(names(dt_chr)), collapse = " & "),
    "\\\\",
    "\\midrule"
  )
  for (i in seq_len(nrow(dt_chr))) {
    lines <- c(lines, paste(latex_escape(unlist(dt_chr[i])), collapse = " & "), "\\\\")
  }
  lines <- c(lines, "\\bottomrule", "\\end{tabular}")
  if (use_resize) lines <- c(lines, "}")
  if (!is.null(note)) {
    lines <- c(
      lines,
      "\\par\\addvspace{0.5ex}",
      "\\parbox{0.96\\linewidth}{\\footnotesize\\textit{Notes:} ",
      latex_escape(note),
      "}"
    )
  }
  lines <- c(lines, "\\end{table}")
  if (landscape) lines <- c(lines, "\\end{landscape}")
  writeLines(lines, file, useBytes = TRUE)
}

write_crop_stats_panel_table <- function(dt, file, caption, label, note = NULL) {
  ensure_output_dir()
  dt <- as.data.table(dt)
  stat_order <- c("Mean", "SD", "Min", "P25", "Median", "P75", "Max", "N")
  crop_order <- c("Beans", "Cassava", "Corn", "Rice", "Soybeans", "Sugarcane")
  variable_order <- unique(dt$Variable)
  lines <- c(
    "\\begin{landscape}",
    "\\begin{table}[!htbp]",
    "\\color{red}",
    "\\centering",
    paste0("\\caption{", latex_escape(caption), "}"),
    paste0("\\label{", label, "}"),
    "\\scriptsize",
    "\\setlength{\\tabcolsep}{3pt}",
    "\\renewcommand{\\arraystretch}{0.70}",
    "\\resizebox{\\linewidth}{!}{%",
    "\\begin{tabular}{llrrrrrrrr}",
    "\\toprule",
    "Crop & Variable & Mean & SD & Min & P25 & Median & P75 & Max & N",
    "\\\\",
    "\\midrule"
  )
  for (cr in crop_order) {
    for (v_idx in seq_along(variable_order)) {
      v <- variable_order[[v_idx]]
      row <- dt[Variable == v & Crop == cr]
      if (nrow(row) == 0L) next
      vals <- vapply(stat_order, function(st) {
        if (st == "N") {
          formatC(as.integer(row[[st]][1]), format = "d", big.mark = ",")
        } else {
          fmt(row[[st]][1], 2)
        }
      }, character(1))
      crop_cell <- if (v_idx == 1L) {
        paste0("\\textbf{", toupper(latex_escape(cr)), "}")
      } else {
        ""
      }
      lines <- c(
        lines,
        paste(c(crop_cell, latex_escape(v), vals), collapse = " & "),
        "\\\\"
      )
    }
    if (cr != tail(crop_order, 1L)) lines <- c(lines, "\\midrule")
  }
  lines <- c(lines, "\\bottomrule", "\\end{tabular}", "}")
  if (!is.null(note)) {
    lines <- c(
      lines,
      "\\par\\addvspace{0.5ex}",
      "\\parbox{0.96\\linewidth}{\\scriptsize\\textit{Notes:} ",
      latex_escape(note),
      "}"
    )
  }
  lines <- c(lines, "\\end{table}", "\\end{landscape}")
  writeLines(lines, file, useBytes = TRUE)
}

write_metric_table <- function(dt, file, caption, label, note = NULL) {
  write_latex_df(dt, file, caption, label, note, size = "\\small")
}

style_fixest_tex <- function(
    tex_file,
    size = "\\scriptsize",
    tabcolsep = "3pt",
    arraystretch = NULL,
    resize = FALSE,
    landscape = FALSE
) {
  lines <- readLines(tex_file, warn = FALSE, encoding = "UTF-8")
  table_line <- grep("^\\s*\\\\begin\\{table\\}", lines)[1]
  if (!is.na(table_line) && !any(grepl("\\\\color\\{red\\}", lines))) {
    lines <- append(lines, "\\color{red}", after = table_line)
  }

  centering_line <- grep("^\\s*\\\\centering", lines)[1]
  if (!is.na(centering_line)) {
    style_lines <- c(size, paste0("\\setlength{\\tabcolsep}{", tabcolsep, "}"))
    if (!is.null(arraystretch)) {
      style_lines <- c(style_lines, paste0("\\renewcommand{\\arraystretch}{", arraystretch, "}"))
    }
    missing_style <- !vapply(style_lines, function(x) any(grepl(x, lines, fixed = TRUE)), logical(1))
    if (any(missing_style)) {
      lines <- append(lines, style_lines[missing_style], after = centering_line)
    }
  }

  if (resize && !any(grepl("\\\\resizebox\\{\\\\linewidth\\}\\{!\\}\\{%", lines))) {
    tabular_line <- grep("^\\s*\\\\begin\\{tabular\\}", lines)[1]
    if (!is.na(tabular_line)) {
      lines <- append(lines, "\\resizebox{\\linewidth}{!}{%", after = tabular_line - 1L)
      end_tabular_line <- grep("^\\s*\\\\end\\{tabular\\}", lines)[1]
      if (!is.na(end_tabular_line)) {
        lines <- append(lines, "}%", after = end_tabular_line)
      }
    }
  }

  if (landscape && !any(grepl("^\\s*\\\\begin\\{landscape\\}", lines))) {
    lines <- c("\\begin{landscape}", lines, "\\end{landscape}")
  }

  writeLines(lines, tex_file, useBytes = TRUE)
  invisible(tex_file)
}

spec_control_flags <- function(spec) {
  data.table(
    `Year FE` = "Yes",
    GDD = ifelse(spec >= 2L, "Yes", "No"),
    KDD = ifelse(spec >= 2L, "Yes", "No"),
    `Soil moisture` = ifelse(spec >= 2L, "Yes", "No"),
    Elevation = ifelse(spec >= 3L, "Yes", "No"),
    Slope = ifelse(spec >= 3L, "Yes", "No"),
    Clay = ifelse(spec >= 3L, "Yes", "No")
  )
}

complete_data <- function(dt, cols) {
  dt <- as.data.table(dt)
  cols <- intersect(cols, names(dt))
  ok <- dt[, Reduce(`&`, lapply(.SD, function(x) {
    if (is.numeric(x) || is.integer(x)) return(is.finite(x))
    !is.na(x) & nzchar(trimws(as.character(x)))
  })), .SDcols = cols]
  dt[ok]
}

coef_row <- function(model, term_name, label, digits = 4) {
  ct <- as.data.table(fixest::coeftable(model), keep.rownames = "term")
  estimate_col <- "Estimate"
  se_col <- intersect(c("Std. Error", "Std..Error", "Std...Error"), names(ct))[1]
  p_col <- intersect(c("Pr(>|t|)", "Pr...t..", "Pr(>|z|)", "Pr...z.."), names(ct))[1]
  row <- ct[term == term_name]
  if (nrow(row) == 0L) {
    return(data.table(Model = label, Coefficient = "", SE = "", P = "", Observations = as.character(stats::nobs(model))))
  }
  data.table(
    Model = label,
    Coefficient = paste0(fmt(row[[estimate_col]], digits), stars(row[[p_col]])),
    SE = paste0("(", fmt(row[[se_col]], digits), ")"),
    P = fmt(row[[p_col]], 3),
    Observations = formatC(stats::nobs(model), format = "d", big.mark = ",")
  )
}

coef_numeric_row <- function(model, term_name, label = NULL) {
  ct <- as.data.table(fixest::coeftable(model), keep.rownames = "term")
  se_col <- intersect(c("Std. Error", "Std..Error", "Std...Error"), names(ct))[1]
  p_col <- intersect(c("Pr(>|t|)", "Pr...t..", "Pr(>|z|)", "Pr...z.."), names(ct))[1]
  row <- ct[term == term_name]
  if (nrow(row) == 0L) {
    return(data.table(
      Model = label,
      estimate = NA_real_,
      se = NA_real_,
      p = NA_real_,
      observations = stats::nobs(model)
    ))
  }
  data.table(
    Model = label,
    estimate = row$Estimate,
    se = row[[se_col]],
    p = row[[p_col]],
    observations = stats::nobs(model)
  )
}

make_conley <- function(lat = "lat", lon = "lon", cutoff = 200) {
  # The main inference procedure allows spatial correlation up to the stated
  # spherical-distance cutoff and repairs non-positive-semidefinite matrices.
  fixest::vcov_conley(
    lat = lat,
    lon = lon,
    cutoff = cutoff,
    distance = "spherical",
    vcov_fix = TRUE
  )
}

# =============================================================================
# 2. Core data readers and reusable Spatial First-Difference construction
# =============================================================================
read_pam_zeros <- function() {
  pam <- readRDS(paths$pam_zeros_rds)
  setDT(pam)
  pam[, Code := trimws(as.character(Code))]
  if (!"mean_salinity" %in% names(pam)) {
    sal_source <- if ("salinity_mean_large" %in% names(pam)) "salinity_mean_large" else "mean_salinity"
    sal_median <- stats::median(pam[[sal_source]], na.rm = TRUE)
    sal_scale <- if (is.finite(sal_median) && sal_median > 20) 100 else 1
    pam[, mean_salinity := get(sal_source) / sal_scale]
  }
  if (!"output_quantity" %in% names(pam)) pam[, output_quantity := quantity]
  if (!"harvested_area" %in% names(pam) && "recolted_area" %in% names(pam)) {
    pam[, harvested_area := recolted_area]
  }
  pam
}

read_pair_map <- function() {
  sfd <- readRDS(paths$sfd_rds)
  setDT(sfd)
  sfd[, `:=`(
    Code = trimws(as.character(Code)),
    code_neighbor_west = trimws(as.character(code_neighbor_west))
  )]
  if (!"pair_id" %in% names(sfd)) {
    sfd[, pair_id := paste(Code, code_neighbor_west, sep = "__")]
  }
  pair_cols <- intersect(
    c("Code", "code_neighbor_west", "pair_id", "lon", "lat", "lon_east", "lat_east", "pair_distance_km", "state", "main_basin", "channel_id"),
    names(sfd)
  )
  unique(sfd[, ..pair_cols])
}

make_sfd_from_unit_panel <- function(unit_dt, pair_map, id_cols, vars) {
  unit_dt <- copy(unit_dt)
  pair_map <- copy(pair_map)
  unit_dt[, Code := trimws(as.character(Code))]
  east <- merge(pair_map, unit_dt, by = "Code", all.x = FALSE, allow.cartesian = TRUE)

  west <- copy(unit_dt[, c("Code", id_cols, vars), with = FALSE])
  setnames(west, "Code", "code_neighbor_west")
  setnames(west, vars, paste0(vars, "_west"))

  dt <- merge(
    east,
    west,
    by = c("code_neighbor_west", id_cols),
    all.x = FALSE,
    allow.cartesian = TRUE
  )
  for (v in vars) {
    west_v <- paste0(v, "_west")
    if (v %in% names(dt) && west_v %in% names(dt)) {
      dt[, paste0("d_", v) := get(v) - get(west_v)]
    }
  }
  dt
}

copy_salinity_exposure_diagnostics_table <- function() {
  src <- file.path(paths$project_dir, "MT2", "salinity_exposure_diagnostics_supervisor_response.tex")
  dst <- file.path(paths$out_dir, "FullRevision_Salinity_Exposure_Diagnostics.tex")
  if (!file.exists(src)) return(invisible(NULL))
  lines <- readLines(src, warn = FALSE, encoding = "UTF-8")
  note_idx <- grep("\\\\parbox\\{0\\.95\\\\textwidth\\}\\{\\\\textit\\{Notes:\\}", lines)
  if (length(note_idx) == 1L) {
    lines[note_idx] <- paste0(
      "\\parbox{0.95\\textwidth}{\\textit{Notes:} ",
      "The table summarizes the estimation samples used in the main crop-yield Spatial First-Difference specifications. ",
      "An observation is an east-minus-west municipality-pair--crop--year cell entering the main yield regressions after requiring strictly positive yields and the non-missing covariates needed for the SFD specification. ",
      "Municipalities count unique east- and west-side IBGE municipality codes observed at least once in each crop sample. ",
      "Mean ECe is reported in dS/m; the source salinity rasters are stored as centi-dS/m and the processing divides raw values by 100 before constructing the salinity measures. ",
      "``Above FAO'' is the share of crop-pair-year observations whose salinity exceeds the crop-specific FAO agronomic threshold.}"
    )
  }
  table_line <- grep("^\\s*\\\\begin\\{table\\}", lines)[1]
  if (!is.na(table_line) && !any(grepl("\\\\color\\{red\\}", lines))) {
    lines <- append(lines, "\\color{red}", after = table_line)
  }
  writeLines(lines, dst, useBytes = TRUE)
  invisible(dst)
}

copy_aquaculture_area_table <- function() {
  src <- file.path(paths$project_dir, "MT2", "aquaculture_mean_salinity_mapbiomas.tex")
  dst <- file.path(paths$out_dir, "FullRevision_Aquaculture_Area.tex")
  if (!file.exists(src)) return(invisible(NULL))
  lines <- readLines(src, warn = FALSE, encoding = "UTF-8")
  lines <- gsub(
    "Outcome: Aquaculture Transition",
    "Outcome: $\\log(\\text{Aquaculture area}+1)$",
    lines,
    fixed = TRUE
  )
  lines <- gsub(
    "tab:sfd_aquaculture_mean_salinity_conley",
    "tab:full_revision_aquaculture_area",
    lines,
    fixed = TRUE
  )
  writeLines(lines, dst, useBytes = TRUE)
  invisible(dst)
}

# =============================================================================
# 3. Descriptive statistics and zero-observation diagnostics
# =============================================================================
make_descriptive_tables <- function() {
  write_status("Building PAM descriptive and zero diagnostics.")
  pam <- read_pam_zeros()
  sfd <- readRDS(paths$sfd_rds)
  setDT(sfd)
  if (!"harvested_area" %in% names(sfd) && "recolted_area" %in% names(sfd)) {
    sfd[, harvested_area := recolted_area]
  }
  regression_needed <- c(
    "log_yield", "d_log_yield", "d_excess_above_fao_large",
    "d_gdd_large", "d_kdd_large", "d_sm_season_large",
    "d_elevation_large", "d_slope_large", "d_clay_mean_large",
    "Year", "pair_id", "lat", "lon"
  )
  reg_sample <- complete_data(sfd, regression_needed)
  reg_sample <- reg_sample[is.finite(yield) & yield > 0]

  vars <- c("yield", "quantity", "value", "harvested_area", "planted_area")
  labels <- c(
    yield = "Yield (kg/ha)",
    quantity = "Production quantity",
    value = "Production value (nominal)",
    harvested_area = "Harvested area (ha)",
    planted_area = "Planted area (ha)"
  )
  crop_labels <- c(
    beans = "Beans", cassava = "Cassava", corn = "Corn",
    rice = "Rice", soy = "Soybeans", sugarcane = "Sugarcane"
  )
  crop_order <- intersect(c("beans", "cassava", "corn", "rice", "soy", "sugarcane"), unique(reg_sample$crop))
  desc <- rbindlist(lapply(crop_order, function(cr) {
    rbindlist(lapply(vars, function(v) {
      x <- reg_sample[crop == cr][[v]]
      data.table(
        Crop = crop_labels[[cr]],
        Variable = labels[[v]],
        Mean = mean(x, na.rm = TRUE),
        SD = stats::sd(x, na.rm = TRUE),
        Min = min(x, na.rm = TRUE),
        P25 = unname(stats::quantile(x, 0.25, na.rm = TRUE)),
        Median = stats::median(x, na.rm = TRUE),
        P75 = unname(stats::quantile(x, 0.75, na.rm = TRUE)),
        Max = max(x, na.rm = TRUE),
        N = sum(!is.na(x))
      )
    }))
  }))
  fwrite(desc, file.path(paths$out_dir, "FullRevision_PAM_Descriptive_Stats.csv"))
  write_crop_stats_panel_table(
    desc,
    file.path(paths$out_dir, "FullRevision_PAM_Descriptive_Stats.tex"),
    "PAM Descriptive Statistics by Crop in the Main Yield Regression Sample",
    "tab:full_revision_pam_descriptive",
    note = paste(
      "An observation is an east-side municipality--crop--year cell attached to an east-minus-west pair that enters the main log-yield SFD regression sample over 1985--2018.",
      "The sample requires strictly positive yield and non-missing salinity, GDD, KDD, soil moisture, soil/topographic controls, pair coordinates and year fixed effects.",
      "Values shown are east-side PAM variables; the corresponding west-side municipality is also observed because the SFD difference is defined.",
      "Production value is nominal in the monetary unit reported by PAM/IBGE."
    )
  )

  balance <- pam[, .(
    observations = .N,
    municipalities = uniqueN(Code),
    years = uniqueN(Year),
    complete_municipality_years = uniqueN(paste(Code, Year)),
    missing_yield = sum(is.na(yield)),
    zero_quantity = sum(quantity == 0, na.rm = TRUE),
    zero_planted_area = sum(planted_area == 0, na.rm = TRUE)
  ), by = crop][order(crop)]
  expected_cells <- 5425L * 6L * 34L
  if (nrow(pam) != expected_cells ||
      uniqueN(pam$Code) != 5425L ||
      uniqueN(pam$crop) != 6L ||
      uniqueN(pam$Year) != 34L) {
    stop("The zero-completed PAM panel no longer has the expected 5,425 x 6 x 34 support.")
  }
  balance_out <- balance[, .(
    Crop = crop,
    Observations = formatC(observations, format = "d", big.mark = ","),
    Municipalities = formatC(municipalities, format = "d", big.mark = ","),
    Years = years,
    `Missing yld.` = formatC(missing_yield, format = "d", big.mark = ","),
    `Zero qty.` = formatC(zero_quantity, format = "d", big.mark = ","),
    `Zero planted` = formatC(zero_planted_area, format = "d", big.mark = ",")
  )]
  write_latex_df(
    balance_out,
    file.path(paths$out_dir, "FullRevision_PAM_Balance_Diagnostics.tex"),
    "PAM Zero-Completed Panel Balance",
    "tab:full_revision_pam_balance",
    note = paste(
      "The zero-completed PAM support is balanced by construction:",
      "5,425 municipalities, six crops and 34 years produce 1,106,700 municipality--crop--year cells.",
      "The log regressions are not balanced because yield and log production are undefined when a crop is not effectively produced."
    ),
    size = "\\scriptsize"
  )

  setorder(pam, Code, crop, Year)
  pam[, zero_quantity_flag := is.finite(quantity) & quantity == 0]
  pam[, zero_planted_flag := is.finite(planted_area) & planted_area == 0]
  pam[, run_quantity := rleid(zero_quantity_flag), by = .(Code, crop)]
  pam[, run_planted := rleid(zero_planted_flag), by = .(Code, crop)]
  pam[, run_quantity_len := .N, by = .(Code, crop, run_quantity)]
  pam[, run_planted_len := .N, by = .(Code, crop, run_planted)]
  pam[, persistent_zero_quantity_5yr := zero_quantity_flag & run_quantity_len >= 5]
  pam[, persistent_zero_planted_5yr := zero_planted_flag & run_planted_len >= 5]
  zero_diag <- pam[, .(
    cells = .N,
    zero_quantity_cells = sum(zero_quantity_flag, na.rm = TRUE),
    persistent_zero_quantity_cells = sum(persistent_zero_quantity_5yr, na.rm = TRUE),
    zero_planted_cells = sum(zero_planted_flag, na.rm = TRUE),
    persistent_zero_planted_cells = sum(persistent_zero_planted_5yr, na.rm = TRUE)
  ), by = crop][order(crop)]
  if (zero_diag[, any(
    persistent_zero_quantity_cells > zero_quantity_cells |
      persistent_zero_planted_cells > zero_planted_cells
  )]) {
    stop("Persistent-zero counts cannot exceed the corresponding zero counts.")
  }
  zero_out <- zero_diag[, .(
    Crop = crop,
    Cells = formatC(cells, format = "d", big.mark = ","),
    `Zero qty.` = formatC(zero_quantity_cells, format = "d", big.mark = ","),
    `Qty. zero 5yr` = formatC(persistent_zero_quantity_cells, format = "d", big.mark = ","),
    `Zero planted` = formatC(zero_planted_cells, format = "d", big.mark = ","),
    `Planted zero 5yr` = formatC(persistent_zero_planted_cells, format = "d", big.mark = ",")
  )]
  write_latex_df(
    zero_out,
    file.path(paths$out_dir, "FullRevision_Zero_Run_Diagnostics.tex"),
    "Five-Year Persistent-Zero Diagnostic",
    "tab:full_revision_zero_runs",
    note = paste(
      "A zero is treated as structural only when it belongs to a run of at least five consecutive zero years for the same municipality and crop.",
      "Short zero spells are retained in asinh specifications; they cannot directly enter log specifications."
    ),
    size = "\\scriptsize"
  )

  invisible(list(pam = pam, balance = balance, zero_diag = zero_diag))
}

# The five-year rule distinguishes short recorded zero spells from persistent
# non-production spells without changing any positive observations.
add_five_year_zero_criterion <- function(pam) {
  pam <- copy(pam)
  if (!"harvested_area" %in% names(pam) && "recolted_area" %in% names(pam)) {
    pam[, harvested_area := recolted_area]
  }
  setorder(pam, Code, crop, Year)
  zero_vars <- intersect(c("quantity", "planted_area", "harvested_area"), names(pam))
  for (v in zero_vars) {
    zero_flag <- paste0("zero_", v, "_flag")
    run_id <- paste0("run_", v)
    run_len <- paste0("run_", v, "_len")
    structural <- paste0("structural_zero_", v, "_5yr")
    keep <- paste0(v, "_keep_5yr")
    pam[, (zero_flag) := is.finite(get(v)) & get(v) == 0]
    pam[, (run_id) := rleid(get(zero_flag)), by = .(Code, crop)]
    pam[, (run_len) := .N, by = .(Code, crop, get(run_id))]
    pam[, (structural) := get(zero_flag) & get(run_len) >= 5L]
    pam[, (keep) := fifelse(get(structural), NA_real_, as.numeric(get(v)))]
  }
  if ("quantity_keep_5yr" %in% names(pam)) {
    pam[, asinh_quantity_keep_5yr := asinh(quantity_keep_5yr)]
  }
  if ("harvested_area_keep_5yr" %in% names(pam)) {
    pam[, asinh_harvested_keep_5yr := asinh(harvested_area_keep_5yr)]
  }
  if ("planted_area_keep_5yr" %in% names(pam)) {
    pam[, asinh_planted_keep_5yr := asinh(planted_area_keep_5yr)]
    pam[, total_planted_keep_5yr := sum(planted_area_keep_5yr, na.rm = TRUE), by = .(Code, Year)]
    pam[, share_planted_keep_5yr := fifelse(
      is.finite(total_planted_keep_5yr) & total_planted_keep_5yr > 0,
      100 * planted_area_keep_5yr / total_planted_keep_5yr,
      NA_real_
    )]
  }
  if (all(c("quantity_keep_5yr", "harvested_area_keep_5yr") %in% names(pam))) {
    pam[, yield_keep_5yr := fifelse(
      is.finite(quantity_keep_5yr) & quantity_keep_5yr > 0 &
        is.finite(harvested_area_keep_5yr) & harvested_area_keep_5yr > 0,
      quantity_keep_5yr / harvested_area_keep_5yr,
      NA_real_
    )]
    pam[, log_yield_keep_5yr := fifelse(is.finite(yield_keep_5yr) & yield_keep_5yr > 0, log(yield_keep_5yr), NA_real_)]
  }
  lag_source <- intersect(
    c("excess_above_fao_large", "mean_salinity", "gdd_large", "kdd_large", "precip_season_large", "sm_season_large"),
    names(pam)
  )
  for (v in lag_source) {
    lag_v <- paste0(v, "_lag1")
    if (!lag_v %in% names(pam)) {
      pam[, (lag_v) := shift(get(v), 1L), by = .(Code, crop)]
    }
  }
  pam
}

make_zero_criterion_asinh_robustness <- function(pam = NULL) {
  write_status("Building asinh robustness with the 5-year zero criterion.")
  if (is.null(pam)) pam <- read_pam_zeros()
  pair_map <- read_pair_map()
  pam <- add_five_year_zero_criterion(pam)

  vars <- intersect(
    c(
      "asinh_quantity_keep_5yr", "asinh_planted_keep_5yr",
      "excess_above_fao_large", "excess_above_fao_large_lag1",
      "gdd_large", "kdd_large", "sm_season_large",
      "elevation_large", "slope_large", "clay_mean_large"
    ),
    names(pam)
  )
  sfd <- make_sfd_from_unit_panel(
    pam[, c("Code", "crop", "Year", vars), with = FALSE],
    pair_map,
    id_cols = c("crop", "Year"),
    vars = vars
  )
  vc <- make_conley()
  weather <- intersect(
    c("d_gdd_large", "d_kdd_large", "d_sm_season_large"),
    names(sfd)
  )
  topo <- intersect(c("d_elevation_large", "d_slope_large", "d_clay_mean_large"), names(sfd))
  crop_labels <- c(
    beans = "Beans", cassava = "Cassava", corn = "Corn",
    rice = "Rice", soy = "Soybeans", sugarcane = "Sugarcane"
  )
  outcomes <- list(
    list(label = "Production quantity", dep = "d_asinh_quantity_keep_5yr", treatment = "d_excess_above_fao_large"),
    list(label = "Planted area", dep = "d_asinh_planted_keep_5yr", treatment = "d_excess_above_fao_large_lag1")
  )
  rows <- list()
  for (cr in names(crop_labels)) {
    for (outcome in outcomes) {
      needed <- c(outcome$dep, outcome$treatment, weather, topo, "Year", "lat", "lon", "pair_id")
      d <- complete_data(sfd[crop == cr], needed)
      if (nrow(d) < 100L) next
      formula_txt <- paste(
        outcome$dep,
        "~",
        paste(c(outcome$treatment, weather, topo), collapse = " + "),
        "| Year"
      )
      model <- fixest::feols(
        as.formula(formula_txt),
        data = d,
        vcov = vc,
        panel.id = ~pair_id + Year,
        notes = FALSE
      )
      row <- coef_row(model, outcome$treatment, outcome$label)
      setnames(row, "Model", "Outcome")
      rows[[length(rows) + 1L]] <- cbind(Crop = crop_labels[[cr]], row)
    }
  }
  tab <- rbindlist(rows, fill = TRUE)
  write_latex_df(
    tab,
    file.path(paths$out_dir, "FullRevision_ZeroCriterion_Asinh_Robustness.tex"),
    "Asinh Robustness with the Five-Year Zero Criterion",
    "tab:full_revision_zero_criterion_asinh",
    note = paste(
      "Zeros in production quantity or planted area are dropped only when they belong to a run of at least five consecutive zero years for the same municipality and crop.",
      "Short zero spells are retained through the asinh transformation.",
      "Production uses contemporaneous excess salinity; planted area uses lagged excess salinity.",
      "All specifications include GDD, KDD, soil moisture, elevation, slope, clay, year fixed effects and Conley spatial standard errors with a 200 km cutoff."
    ),
    size = "\\scriptsize"
  )
  invisible(tab)
}

make_zero_criterion_coefficient_figures <- function(pam = NULL) {
  write_status("Re-estimating Figures 3--6 with the five-year zero criterion.")
  if (is.null(pam)) pam <- read_pam_zeros()
  pam <- add_five_year_zero_criterion(pam)
  pair_map <- read_pair_map()
  vars <- intersect(
    c(
      "log_yield_keep_5yr", "asinh_quantity_keep_5yr",
      "asinh_harvested_keep_5yr", "share_planted_keep_5yr",
      "excess_above_fao_large", "excess_above_fao_large_lag1",
      "gdd_large", "kdd_large", "sm_season_large",
      "gdd_large_lag1", "kdd_large_lag1", "sm_season_large_lag1",
      "elevation_large", "slope_large", "clay_mean_large"
    ),
    names(pam)
  )
  sfd <- make_sfd_from_unit_panel(
    pam[, c("Code", "crop", "Year", vars), with = FALSE],
    pair_map,
    id_cols = c("crop", "Year"),
    vars = vars
  )
  crop_labels <- c(
    beans = "Beans", cassava = "Cassava", corn = "Corn",
    rice = "Rice", soy = "Soybeans", sugarcane = "Sugarcane"
  )
  outcome_defs <- list(
    list(
      outcome = "Yield",
      dep = "d_log_yield_keep_5yr",
      treatment = "d_excess_above_fao_large",
      weather = c("d_gdd_large", "d_kdd_large", "d_sm_season_large"),
      ylab = "Effect on log yield",
      file = "FullRevision_Figure1_Yield_ZeroCriterion"
    ),
    list(
      outcome = "Production quantity",
      dep = "d_asinh_quantity_keep_5yr",
      treatment = "d_excess_above_fao_large",
      weather = c("d_gdd_large", "d_kdd_large", "d_sm_season_large"),
      ylab = "Effect on asinh production quantity",
      file = "FullRevision_Figure4_Output_ZeroCriterion"
    ),
    list(
      outcome = "Harvested area",
      dep = "d_asinh_harvested_keep_5yr",
      treatment = "d_excess_above_fao_large",
      weather = c("d_gdd_large", "d_kdd_large", "d_sm_season_large"),
      ylab = "Effect on asinh harvested area",
      file = "FullRevision_Figure5_HarvestedArea_ZeroCriterion"
    ),
    list(
      outcome = "Planted-area share",
      dep = "d_share_planted_keep_5yr",
      treatment = "d_excess_above_fao_large_lag1",
      weather = c("d_gdd_large_lag1", "d_kdd_large_lag1", "d_sm_season_large_lag1"),
      ylab = "Effect on planted-area share (p.p.)",
      file = "FullRevision_Figure6_PlantedShare_ZeroCriterion"
    )
  )
  topo <- c("d_elevation_large", "d_slope_large", "d_clay_mean_large")
  vc <- make_conley()
  rows <- list()
  for (od in outcome_defs) {
    weather <- intersect(od$weather, names(sfd))
    topo_i <- intersect(topo, names(sfd))
    for (cr in names(crop_labels)) {
      for (spec in 1:3) {
        controls <- switch(as.character(spec), "1" = character(), "2" = weather, "3" = c(weather, topo_i))
        needed <- c(od$dep, od$treatment, controls, "Year", "lat", "lon", "pair_id")
        d <- complete_data(sfd[crop == cr], needed)
        if (nrow(d) < 100L) next
        rhs <- paste(c(od$treatment, controls), collapse = " + ")
        f <- as.formula(paste(od$dep, "~", rhs, "| Year"))
        m <- fixest::feols(f, data = d, vcov = vc, panel.id = ~pair_id + Year, notes = FALSE)
        ct <- as.data.table(fixest::coeftable(m), keep.rownames = "term")
        r <- ct[term == od$treatment]
        if (nrow(r) != 1L) next
        se_col <- intersect(c("Std. Error", "Std..Error", "Std...Error"), names(r))[1]
        p_col <- intersect(c("Pr(>|t|)", "Pr...t..", "Pr(>|z|)", "Pr...z.."), names(r))[1]
        rows[[length(rows) + 1L]] <- cbind(
          data.table(
            outcome = od$outcome,
            crop = cr,
            crop_label = crop_labels[[cr]],
            specification = paste0("Spec. ", spec),
            spec_id = spec,
            estimate = r$Estimate,
            se = r[[se_col]],
            p = r[[p_col]],
            observations = stats::nobs(m)
          ),
          spec_control_flags(spec)
        )
      }
    }
  }
  coef_dt <- rbindlist(rows, fill = TRUE)
  coef_dt[, `:=`(
    ci95_low = estimate - 1.96 * se,
    ci95_high = estimate + 1.96 * se,
    ci90_low = estimate - 1.645 * se,
    ci90_high = estimate + 1.645 * se
  )]
  fwrite(coef_dt, file.path(paths$out_dir, "FullRevision_ZeroCriterion_Figures_3_6_Coefficients.csv"))

  plot_one <- function(od) {
    d <- coef_dt[outcome == od$outcome]
    if (nrow(d) == 0L) return(invisible(NULL))
    is_yield_figure <- identical(od$outcome, "Yield")
    crop_order_top <- c("Corn", "Rice", "Cassava", "Beans", "Soybeans", "Sugarcane")
    crop_order_bottom <- rev(crop_order_top)
    offsets <- c("Spec. 1" = -0.22, "Spec. 2" = 0, "Spec. 3" = 0.22)
    d[, specification := factor(specification, levels = names(offsets))]
    d[, crop_position := match(crop_label, crop_order_bottom)]
    d[, plot_position := crop_position + offsets[as.character(specification)]]
    pal <- c("Spec. 1" = "#1B9E77", "Spec. 2" = "#D95F02", "Spec. 3" = "#7570B3")
    shapes <- c("Spec. 1" = 16, "Spec. 2" = 17, "Spec. 3" = 15)
    fig <- ggplot(d, aes(x = estimate, y = plot_position, color = specification, shape = specification)) +
      geom_segment(aes(x = ci95_low, xend = ci95_high, yend = plot_position), linewidth = 0.45, alpha = 0.55, lineend = "butt") +
      geom_segment(aes(x = ci90_low, xend = ci90_high, yend = plot_position), linewidth = 1.15, lineend = "butt") +
      geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.45, color = "grey20") +
      geom_point(size = 2.6, stroke = 0.65) +
      scale_y_continuous(
        breaks = seq_along(crop_order_bottom),
        labels = crop_order_bottom,
        expand = expansion(mult = c(0.08, 0.08))
      ) +
      scale_color_manual(values = pal, guide = "none") +
      scale_shape_manual(values = shapes, guide = "none") +
      labs(x = od$ylab, y = NULL) +
      theme_minimal(base_size = if (is_yield_figure) 15 else 12) +
      theme(
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.35, color = "grey88"),
        panel.grid.major.x = element_line(linewidth = 0.35, color = "grey88"),
        axis.text.y = element_text(
          size = if (is_yield_figure) 15.5 else 13,
          face = "bold", color = "grey30"
        ),
        axis.text.x = element_text(
          size = if (is_yield_figure) 12.5 else 10.5,
          color = "grey30"
        ),
        axis.title.x = element_text(
          size = if (is_yield_figure) 14 else 12,
          margin = margin(t = if (is_yield_figure) 9 else 7)
        ),
        plot.margin = if (is_yield_figure) margin(10, 12, 10, 10) else margin(8, 10, 8, 8)
      )
    figure_width <- if (is_yield_figure) 8.3 else 7.0
    figure_height <- if (is_yield_figure) 5.8 else 4.6
    ggsave(file.path(paths$out_dir, paste0(od$file, ".pdf")), fig, width = figure_width, height = figure_height)
    ggsave(file.path(paths$out_dir, paste0(od$file, ".png")), fig, width = figure_width, height = figure_height, dpi = 320)
    invisible(fig)
  }
  invisible(lapply(outcome_defs, plot_one))
  file.copy(
    file.path(paths$out_dir, "FullRevision_Figure1_Yield_ZeroCriterion.pdf"),
    file.path(paths$out_dir, "FullRevision_Figure3_Yield_ZeroCriterion.pdf"),
    overwrite = TRUE
  )
  file.copy(
    file.path(paths$out_dir, "FullRevision_Figure1_Yield_ZeroCriterion.png"),
    file.path(paths$out_dir, "FullRevision_Figure3_Yield_ZeroCriterion.png"),
    overwrite = TRUE
  )
  coef_dt
}

# =============================================================================
# 4. Crop output, planted-area, and crop-composition analyses
# =============================================================================
make_incumbent_crop_adjustment_analysis_legacy_pair_fe <- function(pam = NULL) {
  write_status("Building next-campaign planted-area and production responses for incumbent crop producers.")
  if (is.null(pam)) pam <- read_pam_zeros()
  pam <- copy(pam)
  pair_map <- read_pair_map()
  setorder(pam, Code, crop, Year)

  lead_sources <- c(
    planted_area_next = "planted_area",
    quantity_next = "quantity",
    gdd_large_next = "gdd_large",
    kdd_large_next = "kdd_large",
    sm_season_large_next = "sm_season_large"
  )
  pam[, next_year := shift(Year, type = "lead"), by = .(Code, crop)]
  for (new_name in names(lead_sources)) {
    source_name <- lead_sources[[new_name]]
    pam[, (new_name) := shift(get(source_name), type = "lead"), by = .(Code, crop)]
  }
  invalid_lead <- !is.finite(pam$next_year) | pam$next_year != pam$Year + 1L
  pam[
    invalid_lead,
    c(names(lead_sources)) := lapply(names(lead_sources), function(x) NA_real_)
  ]

  # Incumbency is defined only with information observed in the baseline
  # campaign. Subsequent zeros are outcomes and remain in the sample.
  pam[, incumbent_t :=
    is.finite(planted_area) & planted_area > 0 &
    is.finite(quantity) & quantity > 0]
  pam[, planted_area_growth_next := fifelse(
    incumbent_t & is.finite(planted_area_next) & planted_area_next >= 0,
    (planted_area_next - planted_area) / (planted_area_next + planted_area),
    NA_real_
  )]
  pam[, production_growth_next := fifelse(
    incumbent_t & is.finite(quantity_next) & quantity_next >= 0,
    (quantity_next - quantity) / (quantity_next + quantity),
    NA_real_
  )]

  vars <- c(
    "planted_area_growth_next", "production_growth_next",
    "excess_above_fao_large",
    "gdd_large", "kdd_large", "sm_season_large",
    "gdd_large_next", "kdd_large_next", "sm_season_large_next",
    "elevation_large", "slope_large", "clay_mean_large"
  )
  vars <- intersect(vars, names(pam))
  sfd <- make_sfd_from_unit_panel(
    pam[, c("Code", "crop", "Year", vars), with = FALSE],
    pair_map,
    id_cols = c("crop", "Year"),
    vars = vars
  )

  crop_labels <- c(
    corn = "Corn", rice = "Rice", cassava = "Cassava",
    beans = "Beans", soy = "Soybeans", sugarcane = "Sugarcane"
  )
  outcome_defs <- list(
    planted_area = list(
      label = "Planted area",
      dep = "d_planted_area_growth_next",
      controls = c("d_gdd_large", "d_kdd_large", "d_sm_season_large"),
      xlab = "Effect on next-campaign planted-area growth",
      figure = "FullRevision_Incumbent_PlantedArea_Growth",
      table = "FullRevision_Incumbent_PlantedArea_Growth.tex",
      table_title = "Excess Salinity and Next-Campaign Planted-Area Growth",
      table_label = "tab:full_revision_incumbent_planted_area"
    ),
    production = list(
      label = "Production quantity",
      dep = "d_production_growth_next",
      controls = c(
        "d_gdd_large", "d_kdd_large", "d_sm_season_large",
        "d_gdd_large_next", "d_kdd_large_next", "d_sm_season_large_next"
      ),
      xlab = "Effect on next-campaign production growth",
      figure = "FullRevision_Incumbent_Production_Growth",
      table = "FullRevision_Incumbent_Production_Growth.tex",
      table_title = "Excess Salinity and Next-Campaign Production Growth",
      table_label = "tab:full_revision_incumbent_production"
    )
  )
  topo <- c("d_elevation_large", "d_slope_large", "d_clay_mean_large")
  treatment <- "d_excess_above_fao_large"
  coef_rows <- list()
  inference_rows <- list()
  diagnostic_rows <- list()
  models_by_outcome <- list()

  for (outcome_name in names(outcome_defs)) {
    od <- outcome_defs[[outcome_name]]
    models <- list()
    for (cr in names(crop_labels)) {
      needed <- c(od$dep, treatment, od$controls, topo, "Year", "pair_id", "lat", "lon")
      d <- complete_data(sfd[crop == cr], needed)
      # Pair fixed effects require repeated observations. Applying this rule
      # before both regressions keeps their estimation samples identical.
      d <- d[, if (.N >= 2L) .SD, by = pair_id]
      if (nrow(d) < 100L) next

      rhs_no_pair <- paste(c(treatment, od$controls, topo), collapse = " + ")
      rhs_pair <- paste(c(treatment, od$controls), collapse = " + ")
      model_no_pair <- fixest::feols(
        as.formula(paste(od$dep, "~", rhs_no_pair, "| Year")),
        data = d,
        cluster = ~pair_id,
        panel.id = ~pair_id + Year,
        notes = FALSE
      )
      model_pair <- fixest::feols(
        as.formula(paste(od$dep, "~", rhs_pair, "| Year + pair_id")),
        data = d,
        cluster = ~pair_id,
        panel.id = ~pair_id + Year,
        notes = FALSE
      )
      if (stats::nobs(model_no_pair) != stats::nobs(model_pair)) {
        stop("Incumbent crop specifications do not use the same sample for ", cr, " / ", outcome_name, ".")
      }

      crop_models <- list(model_no_pair, model_pair)
      names(crop_models) <- c("Year FE", "Year + pair FE")
      models <- c(models, crop_models)
      for (spec_name in names(crop_models)) {
        row <- coef_numeric_row(crop_models[[spec_name]], treatment, spec_name)
        coef_rows[[length(coef_rows) + 1L]] <- cbind(
          data.table(
            outcome = outcome_name,
            outcome_label = od$label,
            crop = cr,
            crop_label = crop_labels[[cr]],
            specification = spec_name
          ),
          row[, .(estimate, se, p, observations)]
        )
        inference_rows[[length(inference_rows) + 1L]] <- data.table(
          outcome = outcome_name,
          crop = cr,
          crop_label = crop_labels[[cr]],
          specification = spec_name,
          inference = "Pair cluster",
          cutoff_km = NA_integer_,
          estimate = row$estimate,
          se = row$se,
          p = row$p,
          valid = is.finite(row$se)
        )
        for (cutoff in c(50L, 100L)) {
          conley_summary <- try(
            summary(
              crop_models[[spec_name]],
              vcov = fixest::vcov_conley(
                lat = "lat", lon = "lon", cutoff = cutoff,
                distance = "spherical", vcov_fix = FALSE
              )
            ),
            silent = TRUE
          )
          conley_row <- if (inherits(conley_summary, "try-error")) {
            data.table(estimate = row$estimate, se = NA_real_, p = NA_real_)
          } else {
            coef_numeric_row(conley_summary, treatment, spec_name)[, .(estimate, se, p)]
          }
          inference_rows[[length(inference_rows) + 1L]] <- data.table(
            outcome = outcome_name,
            crop = cr,
            crop_label = crop_labels[[cr]],
            specification = spec_name,
            inference = paste0("Strict Conley ", cutoff, " km"),
            cutoff_km = cutoff,
            estimate = conley_row$estimate,
            se = conley_row$se,
            p = conley_row$p,
            valid = is.finite(conley_row$se)
          )
        }
      }

      diagnostic_rows[[length(diagnostic_rows) + 1L]] <- data.table(
        outcome = outcome_name,
        crop = cr,
        crop_label = crop_labels[[cr]],
        observations = nrow(d),
        pairs = uniqueN(d$pair_id),
        years = uniqueN(d$Year),
        baseline_year_min = min(d$Year),
        baseline_year_max = max(d$Year),
        nonzero_treatment_differences = sum(abs(d[[treatment]]) > 1e-12),
        treatment_sd = stats::sd(d[[treatment]]),
        outcome_sd = stats::sd(d[[od$dep]])
      )
    }
    models_by_outcome[[outcome_name]] <- models

    fixest::setFixest_dict(c(
      d_excess_above_fao_large = "$\\Delta_s$ Excess salinity at $t$ (dS/m)",
      d_gdd_large = "$\\Delta_s$ GDD at $t$",
      d_kdd_large = "$\\Delta_s$ KDD at $t$",
      d_sm_season_large = "$\\Delta_s$ Soil moisture at $t$",
      d_gdd_large_next = "$\\Delta_s$ GDD at $t+1$",
      d_kdd_large_next = "$\\Delta_s$ KDD at $t+1$",
      d_sm_season_large_next = "$\\Delta_s$ Soil moisture at $t+1$",
      d_elevation_large = "$\\Delta_s$ Elevation",
      d_slope_large = "$\\Delta_s$ Slope",
      d_clay_mean_large = "$\\Delta_s$ Clay"
    ), reset = TRUE)
    tex_file <- file.path(paths$out_dir, od$table)
    fixest::etable(
      models,
      tex = TRUE,
      file = tex_file,
      replace = TRUE,
      headers = list(
        "Crop" = rep(unname(crop_labels), each = 2L),
        "Specification" = rep(c("Year FE", "Year + pair FE"), times = length(crop_labels))
      ),
      depvar = FALSE,
      title = od$table_title,
      label = od$table_label,
      fitstat = ~ r2 + n,
      signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
      notes = paste(
        "Notes: The sample contains municipality pairs in which both municipalities have strictly positive planted area and production quantity for the crop at t.",
        "The dependent variable is the east-minus-west difference in (Y[t+1]-Y[t])/(Y[t+1]+Y[t]); it is not multiplied by two and retains exits to zero at t+1.",
        "The treatment is the east-minus-west difference in crop-specific excess salinity at t.",
        if (outcome_name == "production") "Production models control for GDD, KDD and soil moisture at t and t+1." else "Planted-area models control for GDD, KDD and soil moisture at t.",
        "Year-FE columns also include elevation, slope and clay differences. These time-invariant controls are absorbed in the pair-FE columns.",
        "Both columns use the same sample and cluster standard errors by spatial pair. No state fixed effects are included."
      )
    )
    style_fixest_tex(tex_file, size = "\\tiny", tabcolsep = "1.5pt", arraystretch = "0.60", resize = TRUE, landscape = TRUE)
  }

  coef_dt <- rbindlist(coef_rows, fill = TRUE)
  coef_dt[, `:=`(
    ci95_low = estimate - 1.96 * se,
    ci95_high = estimate + 1.96 * se,
    ci90_low = estimate - 1.645 * se,
    ci90_high = estimate + 1.645 * se
  )]
  diagnostics <- rbindlist(diagnostic_rows, fill = TRUE)
  inference_dt <- rbindlist(inference_rows, fill = TRUE)
  fwrite(coef_dt, file.path(paths$out_dir, "FullRevision_Incumbent_Crop_Adjustment_Coefficients.csv"))
  fwrite(diagnostics, file.path(paths$out_dir, "FullRevision_Incumbent_Crop_Adjustment_Diagnostics.csv"))
  fwrite(inference_dt, file.path(paths$out_dir, "FullRevision_Incumbent_Crop_Adjustment_Inference.csv"))

  for (outcome_name in names(outcome_defs)) {
    od <- outcome_defs[[outcome_name]]
    d <- copy(coef_dt[outcome == outcome_name])
    diag_outcome <- diagnostics[outcome == outcome_name]
    treatment_sd_by_crop <- setNames(diag_outcome$treatment_sd, diag_outcome$crop)
    control_labels <- if (outcome_name == "production") {
      c(
        d_change_gdd_next = "$\\Delta_s[GDD_{t+1}-GDD_t]$",
        d_change_kdd_next = "$\\Delta_s[KDD_{t+1}-KDD_t]$",
        d_change_sm_next = "$\\Delta_s[Soil\ moisture_{t+1}-Soil\ moisture_t]$",
        d_elevation_large = "$\\Delta_s$ Elevation",
        d_slope_large = "$\\Delta_s$ Slope",
        d_clay_mean_large = "$\\Delta_s$ Clay"
      )
    } else {
      c(
        d_gdd_large = "$\\Delta_s$ GDD at $t$",
        d_kdd_large = "$\\Delta_s$ KDD at $t$",
        d_sm_season_large = "$\\Delta_s$ Soil moisture at $t$",
        d_elevation_large = "$\\Delta_s$ Elevation",
        d_slope_large = "$\\Delta_s$ Slope",
        d_clay_mean_large = "$\\Delta_s$ Clay"
      )
    }
    write_split_incumbent_model_table(
      models_by_crop = models_by_outcome[[outcome_name]],
      treatment_sd = treatment_sd_by_crop,
      file = file.path(paths$out_dir, od$table),
      caption = od$table_title,
      label = od$table_label,
      outcome_symbol = od$outcome_symbol,
      outcome_name = od$outcome_note,
      control_labels = control_labels,
      year_min = min(diag_outcome$baseline_year_min),
      year_max = max(diag_outcome$baseline_year_max)
    )
    crop_order_top <- unname(crop_labels)
    crop_order_bottom <- rev(crop_order_top)
    offsets <- c("Year FE" = -0.15, "Year + pair FE" = 0.15)
    d[, specification := factor(specification, levels = names(offsets))]
    d[, crop_position := match(crop_label, crop_order_bottom)]
    d[, plot_position := crop_position + offsets[as.character(specification)]]
    pal <- c("Year FE" = "#1B9E77", "Year + pair FE" = "#7570B3")
    shapes <- c("Year FE" = 16, "Year + pair FE" = 15)
    fig <- ggplot(d, aes(x = estimate, y = plot_position, color = specification, shape = specification)) +
      geom_segment(aes(x = ci95_low, xend = ci95_high, yend = plot_position), linewidth = 0.45, alpha = 0.55, lineend = "butt") +
      geom_segment(aes(x = ci90_low, xend = ci90_high, yend = plot_position), linewidth = 1.15, lineend = "butt") +
      geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.45, color = "grey20") +
      geom_point(size = 2.6, stroke = 0.65) +
      scale_y_continuous(
        breaks = seq_along(crop_order_bottom),
        labels = crop_order_bottom,
        expand = expansion(mult = c(0.08, 0.08))
      ) +
      scale_color_manual(values = pal, name = NULL) +
      scale_shape_manual(values = shapes, name = NULL) +
      labs(x = od$xlab, y = NULL) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.35, color = "grey88"),
        panel.grid.major.x = element_line(linewidth = 0.35, color = "grey88"),
        axis.text.y = element_text(size = 12.5, face = "bold", color = "grey30"),
        axis.text.x = element_text(size = 10.5, color = "grey30"),
        axis.title.x = element_text(size = 11.5, margin = margin(t = 7)),
        legend.text = element_text(size = 10.5),
        plot.margin = margin(8, 10, 8, 8)
      )
    ggsave(file.path(paths$out_dir, paste0(od$figure, ".pdf")), fig, width = 7.0, height = 5.0)
    ggsave(file.path(paths$out_dir, paste0(od$figure, ".png")), fig, width = 7.0, height = 5.0, dpi = 320)
  }

  invisible(list(
    coefficients = coef_dt,
    diagnostics = diagnostics,
    inference = inference_dt,
    models = models_by_outcome,
    sfd = sfd
  ))
}

extract_model_term <- function(model, term_name, scale = 1) {
  ct <- as.data.table(fixest::coeftable(model), keep.rownames = "term")
  se_col <- intersect(c("Std. Error", "Std..Error", "Std...Error"), names(ct))[1]
  p_col <- intersect(c("Pr(>|t|)", "Pr...t..", "Pr(>|z|)", "Pr...z.."), names(ct))[1]
  row <- ct[ct[["term"]] == term_name]
  if (nrow(row) == 0L) {
    return(data.table(estimate = NA_real_, se = NA_real_, p = NA_real_))
  }
  data.table(
    estimate = scale * row$Estimate[1],
    se = abs(scale) * row[[se_col]][1],
    p = row[[p_col]][1]
  )
}

extract_model_sum <- function(model, terms, scale = 1) {
  beta <- stats::coef(model)
  terms <- intersect(terms, names(beta))
  if (length(terms) == 0L) {
    return(data.table(estimate = NA_real_, se = NA_real_, p = NA_real_))
  }
  v <- stats::vcov(model)[terms, terms, drop = FALSE]
  estimate <- scale * sum(beta[terms])
  se <- abs(scale) * sqrt(sum(v))
  p <- if (is.finite(se) && se > 0) 2 * stats::pnorm(-abs(estimate / se)) else NA_real_
  data.table(estimate = estimate, se = se, p = p)
}

format_model_cell <- function(row, digits = 4) {
  if (nrow(row) == 0L || !is.finite(row$estimate)) return("")
  paste0(fmt(row$estimate, digits), stars(row$p))
}

format_model_se <- function(row, digits = 4) {
  if (nrow(row) == 0L || !is.finite(row$se)) return("")
  paste0("(", fmt(row$se, digits), ")")
}

write_split_incumbent_model_table <- function(
    models_by_crop,
    treatment_sd,
    file,
    caption,
    label,
    outcome_symbol,
    outcome_name,
    control_labels,
    year_min,
    year_max,
    treatment_terms = c("d_excess_above_fao_large"),
    treatment_labels = c("$\\Delta_s$ Excess salinity at $t$ (dS/m)"),
    cumulative_terms = NULL
) {
  crop_labels <- c(
    corn = "Corn", rice = "Rice", cassava = "Cassava",
    beans = "Beans", soy = "Soybeans", sugarcane = "Sugarcane"
  )
  panels <- list(
    "Panel A: Corn, Rice and Cassava" = c("corn", "rice", "cassava"),
    "Panel B: Beans, Soybeans and Sugarcane" = c("beans", "soy", "sugarcane")
  )
  specs <- paste0("Spec. ", 1:3)

  make_cells <- function(crops, extractor) {
    unlist(lapply(crops, function(cr) {
      lapply(specs, function(sp) extractor(models_by_crop[[cr]][[sp]], cr))
    }), recursive = FALSE)
  }
  add_estimate_rows <- function(lines, crops, row_label, extractor) {
    rows <- make_cells(crops, extractor)
    coef_cells <- vapply(rows, format_model_cell, character(1))
    se_cells <- vapply(rows, format_model_se, character(1))
    c(
      lines,
      paste(c(row_label, coef_cells), collapse = " & "), "\\\\",
      paste(c("", se_cells), collapse = " & "), "\\\\"
    )
  }

  note <- paste0(
    "An observation is one spatial municipality pair, crop and baseline campaign t. ",
    "Baseline campaigns run from ", year_min, " to ", year_max,
    ", and outcomes are measured from t to t+1. ",
    "Both municipalities have positive planted area at t and positive production at t-1. ",
    "The dependent variable is the east-minus-west difference in ",
    "$\\operatorname{asinh}(", outcome_symbol, "_{t+1})-\\operatorname{asinh}(", outcome_symbol, "_t)$. ",
    if (outcome_symbol == "Q") {
      "Production quantities equal to zero at t or t+1 are retained. "
    } else {
      "Planted areas equal to zero at t+1 are retained. "
    },
    "Raw treatment coefficients are per 1 dS/m. The standardized row multiplies the coefficient and standard error by the crop-specific sample treatment SD. ",
    "Spec. 1 includes year fixed effects; preferred Spec. 2 adds the displayed GDD, KDD and soil-moisture controls; Spec. 3 additionally adds elevation, slope and clay. ",
    "All specifications use a crop-specific common complete-case sample. Standard errors are clustered by spatial pair. No pair or state fixed effects are included."
  )
  lines <- character()

  for (panel_name in names(panels)) {
    crops <- panels[[panel_name]]
    panel_index <- match(panel_name, names(panels))
    lines <- c(
      lines,
      "\\begin{landscape}",
      "\\begin{table}[!htbp]",
      "\\color{red}",
      "\\centering",
      if (panel_index == 1L) paste0("\\caption{", caption, "}") else
        paste0("\\par\\textbf{Table~\\ref{", label, "} (continued)}\\par\\smallskip"),
      if (panel_index == 1L) paste0("\\label{", label, "}") else character(),
      "\\scriptsize",
      "\\setlength{\\tabcolsep}{2.6pt}",
      "\\renewcommand{\\arraystretch}{0.72}",
      paste0("\\par\\textbf{", panel_name, "}\\par\\smallskip"),
      "\\resizebox{\\linewidth}{!}{%",
      "\\begin{tabular}{lccccccccc}",
      "\\toprule",
      paste(c(
        "Crop",
        vapply(crops, function(cr) paste0("\\multicolumn{3}{c}{", crop_labels[[cr]], "}"), character(1))
      ), collapse = " & "),
      "\\\\",
      paste(c("Specification", rep(specs, times = length(crops))), collapse = " & "),
      "\\\\",
      "\\midrule"
    )

    for (i in seq_along(treatment_terms)) {
      term_i <- treatment_terms[[i]]
      lines <- add_estimate_rows(
        lines, crops, treatment_labels[[i]],
        function(model, cr) extract_model_term(model, term_i)
      )
    }

    if (length(treatment_terms) == 1L) {
      lines <- add_estimate_rows(
        lines, crops, "Effect of one treatment SD",
        function(model, cr) extract_model_term(model, treatment_terms[[1]], treatment_sd[[cr]])
      )
    }
    if (!is.null(cumulative_terms)) {
      lines <- add_estimate_rows(
        lines, crops, "Cumulative effect of one treatment SD",
        function(model, cr) extract_model_sum(model, cumulative_terms, treatment_sd[[cr]])
      )
    }

    sd_cells <- unlist(lapply(crops, function(cr) rep(fmt(treatment_sd[[cr]], 4), 3L)))
    lines <- c(
      lines,
      paste(c("Treatment SD (dS/m)", sd_cells), collapse = " & "), "\\\\",
      "\\midrule"
    )

    for (term_i in names(control_labels)) {
      lines <- add_estimate_rows(
        lines, crops, control_labels[[term_i]],
        function(model, cr) extract_model_term(model, term_i)
      )
    }

    year_cells <- rep("Yes", 3L * length(crops))
    r2_cells <- unlist(lapply(crops, function(cr) {
      vapply(specs, function(sp) fmt(fixest::fitstat(models_by_crop[[cr]][[sp]], "r2")$r2, 5), character(1))
    }))
    n_cells <- unlist(lapply(crops, function(cr) {
      vapply(specs, function(sp) formatC(stats::nobs(models_by_crop[[cr]][[sp]]), format = "d", big.mark = ","), character(1))
    }))
    lines <- c(
      lines,
      "\\midrule",
      paste(c("Year fixed effects", year_cells), collapse = " & "), "\\\\",
      paste(c("R$^2$", r2_cells), collapse = " & "), "\\\\",
      paste(c("Observations", n_cells), collapse = " & "), "\\\\",
      "\\bottomrule",
      "\\end{tabular}",
      "}%",
      "\\par\\medskip",
      "\\parbox{0.98\\linewidth}{\\footnotesize\\textit{Notes:} ",
      note,
      "}",
      "\\end{table}",
      "\\end{landscape}"
    )
  }
  writeLines(lines, file, useBytes = TRUE)
  invisible(file)
}

make_incumbent_crop_asinh_change_analysis <- function(pam = NULL) {
  write_status("Building asinh-change outcomes for municipality-crop cells with positive baseline area and production.")
  if (is.null(pam)) pam <- read_pam_zeros()
  pam <- copy(pam)
  pair_map <- read_pair_map()
  setorder(pam, Code, crop, Year)

  pam[, total_selected_planted_area := {
    x <- as.numeric(planted_area)
    if (all(is.finite(x) & x >= 0)) sum(x) else NA_real_
  }, by = .(Code, Year)]
  pam[, other_selected_planted_area := fifelse(
    is.finite(total_selected_planted_area) &
      is.finite(planted_area) & planted_area >= 0,
    pmax(total_selected_planted_area - planted_area, 0),
    NA_real_
  )]

  pam[, `:=`(
    previous_year = shift(Year),
    second_previous_year = shift(Year, 2L),
    planted_area_previous = shift(planted_area),
    quantity_previous = shift(quantity),
    excess_previous = shift(excess_above_fao_large),
    excess_previous2 = shift(excess_above_fao_large, 2L),
    next_year = shift(Year, type = "lead"),
    planted_area_next = shift(planted_area, type = "lead"),
    quantity_next = shift(quantity, type = "lead"),
    gdd_next = shift(gdd_large, type = "lead"),
    kdd_next = shift(kdd_large, type = "lead"),
    sm_next = shift(sm_season_large, type = "lead"),
    total_selected_planted_area_next = shift(total_selected_planted_area, type = "lead"),
    other_selected_planted_area_next = shift(other_selected_planted_area, type = "lead"),
    second_next_year = shift(Year, 2L, type = "lead"),
    planted_area_next2 = shift(planted_area, 2L, type = "lead")
  ), by = .(Code, crop)]
  pam[!is.finite(previous_year) | previous_year != Year - 1L, `:=`(
    planted_area_previous = NA_real_,
    quantity_previous = NA_real_,
    excess_previous = NA_real_
  )]
  pam[!is.finite(second_previous_year) | second_previous_year != Year - 2L,
      excess_previous2 := NA_real_]
  pam[!is.finite(next_year) | next_year != Year + 1L, `:=`(
    planted_area_next = NA_real_,
    quantity_next = NA_real_,
    gdd_next = NA_real_,
    kdd_next = NA_real_,
    sm_next = NA_real_,
    total_selected_planted_area_next = NA_real_,
    other_selected_planted_area_next = NA_real_
  )]
  pam[!is.finite(second_next_year) | second_next_year != Year + 2L,
      planted_area_next2 := NA_real_]
  pam[, incumbent_t :=
    is.finite(planted_area) & planted_area > 0 &
    is.finite(quantity_previous) & quantity_previous > 0]
  pam[, `:=`(
    asinh_planted_t = fifelse(incumbent_t, asinh(planted_area), NA_real_),
    asinh_quantity_t = fifelse(
      incumbent_t & is.finite(quantity) & quantity >= 0,
      asinh(quantity), NA_real_
    ),
    asinh_planted_next = fifelse(
      incumbent_t & is.finite(planted_area_next) & planted_area_next >= 0,
      asinh(planted_area_next), NA_real_
    ),
    asinh_quantity_next = fifelse(
      incumbent_t & is.finite(quantity_next) & quantity_next >= 0,
      asinh(quantity_next), NA_real_
    )
  )]
  pam[, `:=`(
    change_asinh_planted = asinh_planted_next - asinh_planted_t,
    change_asinh_quantity = asinh_quantity_next - asinh_quantity_t,
    change_gdd_next = gdd_next - gdd_large,
    change_kdd_next = kdd_next - kdd_large,
    change_sm_next = sm_next - sm_season_large,
    change_log_planted_stayer = fifelse(
      incumbent_t & is.finite(planted_area_next) & planted_area_next > 0,
      log(planted_area_next) - log(planted_area),
      NA_real_
    ),
    exit_planted_next_pp = fifelse(
      incumbent_t & is.finite(planted_area_next) & planted_area_next >= 0,
      100 * as.numeric(planted_area_next == 0),
      NA_real_
    ),
    persistent_exit_planted_next2_pp = fifelse(
      incumbent_t & is.finite(planted_area_next) & planted_area_next >= 0 &
        is.finite(planted_area_next2) & planted_area_next2 >= 0,
      100 * as.numeric(planted_area_next == 0 & planted_area_next2 == 0),
      NA_real_
    ),
    change_asinh_planted_established = fifelse(
      incumbent_t & is.finite(planted_area_previous) & planted_area_previous > 0 &
        is.finite(quantity_previous) & quantity_previous > 0,
      asinh_planted_next - asinh_planted_t,
      NA_real_
    ),
    change_asinh_other_planted = fifelse(
      incumbent_t & is.finite(other_selected_planted_area) &
        is.finite(other_selected_planted_area_next),
      asinh(other_selected_planted_area_next) - asinh(other_selected_planted_area),
      NA_real_
    ),
    change_asinh_total_selected_planted = fifelse(
      incumbent_t & is.finite(total_selected_planted_area) &
        is.finite(total_selected_planted_area_next),
      asinh(total_selected_planted_area_next) - asinh(total_selected_planted_area),
      NA_real_
    ),
    change_asinh_planted_previous = fifelse(
      incumbent_t & is.finite(planted_area_previous) & planted_area_previous >= 0,
      asinh(planted_area) - asinh(planted_area_previous),
      NA_real_
    )
  )]

  vars <- c(
    "asinh_planted_t", "asinh_planted_next", "change_asinh_planted",
    "asinh_quantity_t", "asinh_quantity_next", "change_asinh_quantity",
    "change_log_planted_stayer", "exit_planted_next_pp",
    "persistent_exit_planted_next2_pp", "change_asinh_planted_established",
    "change_asinh_other_planted", "change_asinh_total_selected_planted",
    "change_asinh_planted_previous",
    "excess_above_fao_large", "excess_previous", "excess_previous2",
    "mean_salinity", "gdd_large", "kdd_large", "sm_season_large",
    "change_gdd_next", "change_kdd_next", "change_sm_next",
    "elevation_large", "slope_large", "clay_mean_large"
  )
  sfd <- make_sfd_from_unit_panel(
    pam[, c("Code", "crop", "Year", vars), with = FALSE],
    pair_map,
    id_cols = c("crop", "Year"),
    vars = vars
  )

  crop_labels <- c(
    corn = "Corn", rice = "Rice", cassava = "Cassava",
    beans = "Beans", soy = "Soybeans", sugarcane = "Sugarcane"
  )
  outcome_defs <- list(
    planted_area = list(
      label = "Planted area",
      dep = "d_change_asinh_planted",
      baseline = "d_asinh_planted_t",
      next_outcome = "d_asinh_planted_next",
      figure = "FullRevision_Incumbent_PlantedArea_AsinhChange",
      table = "FullRevision_Incumbent_PlantedArea_AsinhChange.tex",
      table_title = "Excess Salinity and Change in Planted Area",
      table_label = "tab:full_revision_incumbent_planted_area_asinh_change",
      xlab = "Effect of a one-SD increase in excess salinity",
      weather = c("d_gdd_large", "d_kdd_large", "d_sm_season_large"),
      outcome_symbol = "A",
      outcome_note = "Planted areas"
    ),
    production = list(
      label = "Production quantity",
      dep = "d_change_asinh_quantity",
      baseline = "d_asinh_quantity_t",
      next_outcome = "d_asinh_quantity_next",
      figure = "FullRevision_Incumbent_Production_AsinhChange",
      table = "FullRevision_Incumbent_Production_AsinhChange.tex",
      table_title = "Excess Salinity and Change in Production",
      table_label = "tab:full_revision_incumbent_production_asinh_change",
      xlab = "Effect of a one-SD increase in excess salinity",
      weather = c("d_change_gdd_next", "d_change_kdd_next", "d_change_sm_next"),
      outcome_symbol = "Q",
      outcome_note = "Production quantities"
    )
  )
  treatment <- "d_excess_above_fao_large"
  topo <- c("d_elevation_large", "d_slope_large", "d_clay_mean_large")
  coef_rows <- list()
  inference_rows <- list()
  diagnostic_rows <- list()
  models_by_outcome <- list()

  for (outcome_name in names(outcome_defs)) {
    od <- outcome_defs[[outcome_name]]
    weather <- od$weather
    models <- list()
    models_nested <- list()
    for (cr in names(crop_labels)) {
      needed <- c(od$dep, od$baseline, od$next_outcome, treatment, weather, topo, "Year", "pair_id", "lat", "lon")
      d <- complete_data(sfd[crop == cr], needed)
      if (nrow(d) < 100L) next
      treatment_sd <- stats::sd(d[[treatment]])
      crop_models <- list()

      for (spec in 1:3) {
        controls <- switch(
          as.character(spec),
          "1" = character(),
          "2" = weather,
          "3" = c(weather, topo)
        )
        rhs <- paste(c(treatment, controls), collapse = " + ")
        model <- fixest::feols(
          as.formula(paste(od$dep, "~", rhs, "| Year")),
          data = d,
          cluster = ~pair_id,
          panel.id = ~pair_id + Year,
          notes = FALSE
        )
        spec_name <- paste0("Spec. ", spec)
        crop_models[[spec_name]] <- model
        main_row <- coef_numeric_row(model, treatment, spec_name)
        coef_rows[[length(coef_rows) + 1L]] <- cbind(
          data.table(
            outcome = outcome_name,
            outcome_label = od$label,
            crop = cr,
            crop_label = crop_labels[[cr]],
            specification = spec_name,
            spec_id = spec,
            treatment_sd = treatment_sd
          ),
          main_row[, .(estimate, se, p, observations)],
          spec_control_flags(spec)
        )

        if (spec == 2L) {
          inference_rows[[length(inference_rows) + 1L]] <- data.table(
            outcome = outcome_name,
            crop = cr,
            crop_label = crop_labels[[cr]],
            inference = "Pair cluster",
            cutoff_km = NA_integer_,
            estimate = main_row$estimate,
            se = main_row$se,
            p = main_row$p,
            valid = is.finite(main_row$se),
            minimum_vcov_eigenvalue = NA_real_,
            vcov_warning = NA_character_
          )
          for (cutoff in c(50L, 100L, 200L, 250L, 300L)) {
            conley_warnings <- character()
            conley_vcov <- try(
              withCallingHandlers(
                stats::vcov(
                  model,
                  vcov = fixest::vcov_conley(
                    lat = "lat", lon = "lon", cutoff = cutoff,
                    distance = "spherical", vcov_fix = FALSE
                  )
                ),
                warning = function(w) {
                  conley_warnings <<- c(conley_warnings, conditionMessage(w))
                  invokeRestart("muffleWarning")
                }
              ),
              silent = TRUE
            )
            minimum_vcov_eigenvalue <- if (
              inherits(conley_vcov, "try-error") || !is.matrix(conley_vcov) ||
                any(!is.finite(conley_vcov))
            ) {
              NA_real_
            } else {
              min(eigen(conley_vcov, symmetric = TRUE, only.values = TRUE)$values)
            }
            eigen_tolerance <- if (is.matrix(conley_vcov)) {
              1e-10 * max(1, max(abs(diag(conley_vcov)), na.rm = TRUE))
            } else {
              NA_real_
            }
            covariance_was_fixed <- any(grepl(
              "positive semi-definite|was 'fixed'",
              conley_warnings,
              ignore.case = TRUE
            ))
            conley_valid <- !covariance_was_fixed && is.finite(minimum_vcov_eigenvalue) &&
              minimum_vcov_eigenvalue >= -eigen_tolerance
            conley_summary <- if (conley_valid) {
              try(summary(model, vcov = conley_vcov), silent = TRUE)
            } else {
              structure("Non-positive-semidefinite Conley covariance", class = "try-error")
            }
            conley_row <- if (inherits(conley_summary, "try-error")) {
              data.table(estimate = main_row$estimate, se = NA_real_, p = NA_real_)
            } else {
              coef_numeric_row(conley_summary, treatment, spec_name)[, .(estimate, se, p)]
            }
            inference_rows[[length(inference_rows) + 1L]] <- data.table(
              outcome = outcome_name,
              crop = cr,
              crop_label = crop_labels[[cr]],
              inference = paste0("Strict Conley ", cutoff, " km"),
              cutoff_km = cutoff,
              estimate = conley_row$estimate,
              se = conley_row$se,
              p = conley_row$p,
              valid = conley_valid && is.finite(conley_row$se),
              minimum_vcov_eigenvalue = minimum_vcov_eigenvalue,
              vcov_warning = if (length(conley_warnings)) {
                paste(unique(conley_warnings), collapse = " | ")
              } else {
                NA_character_
              }
            )
          }
        }
      }
      models_nested[[cr]] <- crop_models
      models <- c(models, crop_models)
      next_var <- sub("^d_", "", od$next_outcome)
      next_west_var <- paste0(next_var, "_west")
      zero_next_sides <- sum(d[[next_var]] == 0) + sum(d[[next_west_var]] == 0)
      diagnostic_rows[[length(diagnostic_rows) + 1L]] <- data.table(
        outcome = outcome_name,
        crop = cr,
        crop_label = crop_labels[[cr]],
        observations = nrow(d),
        pairs = uniqueN(d$pair_id),
        years = uniqueN(d$Year),
        baseline_year_min = min(d$Year),
        baseline_year_max = max(d$Year),
        nonzero_treatment_differences = sum(abs(d[[treatment]]) > 1e-12),
        treatment_sd = treatment_sd,
        municipality_side_observations = 2L * nrow(d),
        zero_next_municipality_sides = zero_next_sides,
        zero_next_side_share = zero_next_sides / (2L * nrow(d)),
        pairs_with_any_zero_next = sum(d[[next_var]] == 0 | d[[next_west_var]] == 0),
        baseline_outcome_sd = stats::sd(d[[od$baseline]]),
        change_outcome_sd = stats::sd(d[[od$dep]]),
        next_outcome_sd = stats::sd(d[[od$next_outcome]])
      )
    }
    models_by_outcome[[outcome_name]] <- models_nested
  }

  coef_dt <- rbindlist(coef_rows, fill = TRUE)
  inference_dt <- rbindlist(inference_rows, fill = TRUE)
  diagnostics <- rbindlist(diagnostic_rows, fill = TRUE)
  coef_dt[, `:=`(
    estimate_plot = estimate * treatment_sd,
    se_plot = se * treatment_sd
  )]
  coef_dt[, `:=`(
    ci95_low = estimate_plot - 1.96 * se_plot,
    ci95_high = estimate_plot + 1.96 * se_plot,
    ci90_low = estimate_plot - 1.645 * se_plot,
    ci90_high = estimate_plot + 1.645 * se_plot
  )]
  fwrite(coef_dt, file.path(paths$out_dir, "FullRevision_Incumbent_AsinhChange_Coefficients.csv"))
  fwrite(inference_dt, file.path(paths$out_dir, "FullRevision_Incumbent_AsinhChange_Inference.csv"))
  fwrite(diagnostics, file.path(paths$out_dir, "FullRevision_Incumbent_AsinhChange_Diagnostics.csv"))

  for (outcome_name in names(outcome_defs)) {
    od <- outcome_defs[[outcome_name]]
    d <- copy(coef_dt[outcome == outcome_name])
    crop_order_top <- unname(crop_labels)
    crop_order_bottom <- rev(crop_order_top)
    offsets <- c("Spec. 1" = -0.22, "Spec. 2" = 0, "Spec. 3" = 0.22)
    d[, specification := factor(specification, levels = names(offsets))]
    d[, crop_position := match(crop_label, crop_order_bottom)]
    d[, plot_position := crop_position + offsets[as.character(specification)]]
    pal <- c("Spec. 1" = "#1B9E77", "Spec. 2" = "#D95F02", "Spec. 3" = "#7570B3")
    shapes <- c("Spec. 1" = 16, "Spec. 2" = 17, "Spec. 3" = 15)
    fig <- ggplot(d, aes(x = estimate_plot, y = plot_position, color = specification, shape = specification)) +
      geom_segment(aes(x = ci95_low, xend = ci95_high, yend = plot_position), linewidth = 0.45, alpha = 0.55, lineend = "butt") +
      geom_segment(aes(x = ci90_low, xend = ci90_high, yend = plot_position), linewidth = 1.15, lineend = "butt") +
      geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.45, color = "grey20") +
      geom_point(size = 2.6, stroke = 0.65) +
      scale_y_continuous(
        breaks = seq_along(crop_order_bottom),
        labels = crop_order_bottom,
        expand = expansion(mult = c(0.08, 0.08))
      ) +
      scale_color_manual(values = pal, guide = "none") +
      scale_shape_manual(values = shapes, guide = "none") +
      labs(x = od$xlab, y = NULL) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.35, color = "grey88"),
        panel.grid.major.x = element_line(linewidth = 0.35, color = "grey88"),
        axis.text.y = element_text(size = 12.5, face = "bold", color = "grey30"),
        axis.text.x = element_text(size = 10.5, color = "grey30"),
        axis.title.x = element_text(size = 11.5, margin = margin(t = 7)),
        plot.margin = margin(8, 10, 8, 8)
      )
    ggsave(file.path(paths$out_dir, paste0(od$figure, ".pdf")), fig, width = 7.0, height = 4.8)
    ggsave(file.path(paths$out_dir, paste0(od$figure, ".png")), fig, width = 7.0, height = 4.8, dpi = 320)

    treatment_sd_by_crop <- setNames(
      diagnostics[outcome == outcome_name, treatment_sd],
      diagnostics[outcome == outcome_name, crop]
    )
    if (outcome_name == "planted_area") {
      control_labels <- c(
        d_gdd_large = "$\\Delta_s$ GDD at $t$",
        d_kdd_large = "$\\Delta_s$ KDD at $t$",
        d_sm_season_large = "$\\Delta_s$ Soil moisture at $t$",
        d_elevation_large = "$\\Delta_s$ Elevation",
        d_slope_large = "$\\Delta_s$ Slope",
        d_clay_mean_large = "$\\Delta_s$ Clay"
      )
    } else {
      control_labels <- c(
        d_change_gdd_next = "$\\Delta_s[GDD_{t+1}-GDD_t]$",
        d_change_kdd_next = "$\\Delta_s[KDD_{t+1}-KDD_t]$",
        d_change_sm_next = "$\\Delta_s[Soil\ moisture_{t+1}-Soil\ moisture_t]$",
        d_elevation_large = "$\\Delta_s$ Elevation",
        d_slope_large = "$\\Delta_s$ Slope",
        d_clay_mean_large = "$\\Delta_s$ Clay"
      )
    }
    write_split_incumbent_model_table(
      models_by_crop = models_by_outcome[[outcome_name]],
      treatment_sd = treatment_sd_by_crop,
      file = file.path(paths$out_dir, od$table),
      caption = od$table_title,
      label = od$table_label,
      treatment_terms = treatment,
      treatment_labels = "$\\Delta_s$ Excess salinity at $t$ (dS/m)",
      control_labels = control_labels,
      outcome_symbol = od$outcome_symbol,
      outcome_name = od$outcome_note,
      year_min = min(diagnostics[outcome == outcome_name, baseline_year_min]),
      year_max = max(diagnostics[outcome == outcome_name, baseline_year_max])
    )
  }

  invisible(list(
    coefficients = coef_dt,
    inference = inference_dt,
    diagnostics = diagnostics,
    models = models_by_outcome,
    sfd = sfd
  ))
}

make_output_distributed_lag_analysis <- function(incumbent_result) {
  write_status("Estimating joint distributed-lag output specifications.")
  sfd <- copy(incumbent_result$sfd)
  crop_labels <- c(
    corn = "Corn", rice = "Rice", cassava = "Cassava",
    beans = "Beans", soy = "Soybeans", sugarcane = "Sugarcane"
  )
  exposure_terms <- c(
    "d_excess_above_fao_large",
    "d_excess_previous",
    "d_excess_previous2"
  )
  exposure_labels <- c(
    "$\\Delta_s$ Excess salinity at $t$ (dS/m)",
    "$\\Delta_s$ Excess salinity at $t-1$ (dS/m)",
    "$\\Delta_s$ Excess salinity at $t-2$ (dS/m)"
  )
  weather <- c("d_change_gdd_next", "d_change_kdd_next", "d_change_sm_next")
  topo <- c("d_elevation_large", "d_slope_large", "d_clay_mean_large")
  models_by_crop <- list()
  coef_rows <- list()
  diagnostic_rows <- list()
  treatment_sd <- numeric()
  year_min <- Inf
  year_max <- -Inf

  for (cr in names(crop_labels)) {
    needed <- c(
      "d_change_asinh_quantity", exposure_terms, weather, topo,
      "Year", "pair_id", "lat", "lon"
    )
    d <- complete_data(sfd[crop == cr], needed)
    if (nrow(d) < 100L) next
    treatment_sd[[cr]] <- stats::sd(d[[exposure_terms[[1]]]])
    year_min <- min(year_min, d$Year)
    year_max <- max(year_max, d$Year)
    models_by_crop[[cr]] <- list()

    corr <- stats::cor(d[, ..exposure_terms])
    vif_values <- vapply(seq_along(exposure_terms), function(i) {
      lhs <- exposure_terms[[i]]
      rhs <- exposure_terms[-i]
      fit <- stats::lm(stats::as.formula(paste(lhs, "~", paste(rhs, collapse = " + "))), data = d)
      r2 <- summary(fit)$r.squared
      if (is.finite(r2) && r2 < 1) 1 / (1 - r2) else Inf
    }, numeric(1))
    diagnostic_rows[[length(diagnostic_rows) + 1L]] <- data.table(
      crop = cr,
      crop_label = crop_labels[[cr]],
      observations = nrow(d),
      pairs = uniqueN(d$pair_id),
      baseline_year_min = min(d$Year),
      baseline_year_max = max(d$Year),
      corr_t_tminus1 = corr[1, 2],
      corr_t_tminus2 = corr[1, 3],
      corr_tminus1_tminus2 = corr[2, 3],
      vif_t = vif_values[[1]],
      vif_tminus1 = vif_values[[2]],
      vif_tminus2 = vif_values[[3]],
      treatment_sd = treatment_sd[[cr]]
    )

    for (spec in 1:3) {
      controls <- switch(
        as.character(spec),
        "1" = character(),
        "2" = weather,
        "3" = c(weather, topo)
      )
      rhs <- paste(c(exposure_terms, controls), collapse = " + ")
      model <- fixest::feols(
        stats::as.formula(paste("d_change_asinh_quantity ~", rhs, "| Year")),
        data = d,
        cluster = ~pair_id,
        panel.id = ~pair_id + Year,
        notes = FALSE
      )
      spec_name <- paste0("Spec. ", spec)
      models_by_crop[[cr]][[spec_name]] <- model
      for (term_i in exposure_terms) {
        row <- extract_model_term(model, term_i)
        coef_rows[[length(coef_rows) + 1L]] <- cbind(
          data.table(
            crop = cr,
            crop_label = crop_labels[[cr]],
            specification = spec_name,
            spec_id = spec,
            term = term_i,
            treatment_sd = treatment_sd[[cr]]
          ),
          row,
          data.table(observations = stats::nobs(model))
        )
      }
      sum_row <- extract_model_sum(model, exposure_terms)
      coef_rows[[length(coef_rows) + 1L]] <- cbind(
        data.table(
          crop = cr,
          crop_label = crop_labels[[cr]],
          specification = spec_name,
          spec_id = spec,
          term = "cumulative_t_to_tminus2",
          treatment_sd = treatment_sd[[cr]]
        ),
        sum_row,
        data.table(observations = stats::nobs(model))
      )
    }
  }

  coef_dt <- rbindlist(coef_rows, fill = TRUE)
  diagnostics <- rbindlist(diagnostic_rows, fill = TRUE)
  coef_dt[, `:=`(
    estimate_one_sd = estimate * treatment_sd,
    se_one_sd = se * treatment_sd
  )]
  coef_dt[, `:=`(
    ci95_low = estimate_one_sd - 1.96 * se_one_sd,
    ci95_high = estimate_one_sd + 1.96 * se_one_sd,
    ci90_low = estimate_one_sd - 1.645 * se_one_sd,
    ci90_high = estimate_one_sd + 1.645 * se_one_sd
  )]
  fwrite(coef_dt, file.path(paths$out_dir, "FullRevision_Output_DistributedLag_Coefficients.csv"))
  fwrite(diagnostics, file.path(paths$out_dir, "FullRevision_Output_DistributedLag_Diagnostics.csv"))

  control_labels <- c(
    d_change_gdd_next = "$\\Delta_s[GDD_{t+1}-GDD_t]$",
    d_change_kdd_next = "$\\Delta_s[KDD_{t+1}-KDD_t]$",
    d_change_sm_next = "$\\Delta_s[Soil\ moisture_{t+1}-Soil\ moisture_t]$",
    d_elevation_large = "$\\Delta_s$ Elevation",
    d_slope_large = "$\\Delta_s$ Slope",
    d_clay_mean_large = "$\\Delta_s$ Clay"
  )
  write_split_incumbent_model_table(
    models_by_crop = models_by_crop,
    treatment_sd = treatment_sd,
    file = file.path(paths$out_dir, "FullRevision_Output_DistributedLag.tex"),
    caption = "Joint Distributed-Lag Effects of Excess Salinity on Output Changes",
    label = "tab:full_revision_output_distributed_lag",
    outcome_symbol = "Q",
    outcome_name = "Production quantities",
    control_labels = control_labels,
    year_min = year_min,
    year_max = year_max,
    treatment_terms = exposure_terms,
    treatment_labels = exposure_labels,
    cumulative_terms = exposure_terms
  )

  plot_dt <- coef_dt[specification == "Spec. 2"]
  timing_labels <- c(
    d_excess_above_fao_large = "Exposure at t",
    d_excess_previous = "Exposure at t-1",
    d_excess_previous2 = "Exposure at t-2",
    cumulative_t_to_tminus2 = "Cumulative effect"
  )
  plot_dt[, timing := factor(timing_labels[term], levels = unname(timing_labels))]
  plot_dt[, crop_label := factor(crop_label, levels = rev(unname(crop_labels)))]
  fig <- ggplot(plot_dt, aes(x = estimate_one_sd, y = crop_label)) +
    geom_segment(aes(x = ci95_low, xend = ci95_high, yend = crop_label), color = "#D95F02", linewidth = 0.42, alpha = 0.55, lineend = "butt") +
    geom_segment(aes(x = ci90_low, xend = ci90_high, yend = crop_label), color = "#D95F02", linewidth = 1.05, lineend = "butt") +
    geom_point(color = "#D95F02", shape = 17, size = 2.4) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4, color = "grey25") +
    facet_wrap(~timing, nrow = 1, scales = "free_x") +
    labs(x = "Effect of a one-SD increase in excess salinity", y = NULL) +
    theme_minimal(base_size = 10.5) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.3, color = "grey88"),
      axis.text.y = element_text(size = 9.5, face = "bold", color = "grey30"),
      axis.text.x = element_text(size = 8.5, color = "grey30"),
      strip.text = element_text(size = 9.5, face = "bold"),
      axis.title.x = element_text(size = 10, margin = margin(t = 6)),
      plot.margin = margin(7, 7, 7, 7)
    )
  ggsave(file.path(paths$out_dir, "FullRevision_Output_DistributedLag.pdf"), fig, width = 10.2, height = 4.8)
  ggsave(file.path(paths$out_dir, "FullRevision_Output_DistributedLag.png"), fig, width = 10.2, height = 4.8, dpi = 320)

  invisible(list(
    coefficients = coef_dt,
    diagnostics = diagnostics,
    models = models_by_crop
  ))
}

write_output_conley_robustness <- function(incumbent_result) {
  inference <- copy(incumbent_result$inference[outcome == "production"])
  main <- copy(incumbent_result$coefficients[
    outcome == "production" & specification == "Spec. 2",
    .(crop, crop_label, treatment_sd, observations)
  ])
  inference <- merge(inference, main, by = c("crop", "crop_label"), all.x = TRUE)
  inference[, `:=`(
    estimate_one_sd = estimate * treatment_sd,
    se_one_sd = se * treatment_sd
  )]
  fwrite(inference, file.path(paths$out_dir, "FullRevision_Output_Conley_Robustness.csv"))

  crop_order <- c("Corn", "Rice", "Cassava", "Beans", "Soybeans", "Sugarcane")
  inference_order <- c(
    "Pair cluster", "Strict Conley 50 km", "Strict Conley 100 km",
    "Strict Conley 200 km", "Strict Conley 250 km", "Strict Conley 300 km"
  )
  headers <- c("Pair cluster", "50 km", "100 km", "200 km", "250 km", "300 km")
  lines <- c(
    "\\begin{landscape}",
    "\\begin{table}[!htbp]",
    "\\color{red}",
    "\\centering",
    "\\caption{Spatial-Inference Robustness for Next-Season Output}",
    "\\label{tab:full_revision_output_conley_robustness}",
    "\\scriptsize",
    "\\setlength{\\tabcolsep}{5pt}",
    "\\renewcommand{\\arraystretch}{1.05}",
    "\\resizebox{\\linewidth}{!}{%",
    "\\begin{tabular}{lcccccc}",
    "\\toprule",
    paste(c("Crop", headers), collapse = " & "),
    "\\\\",
    "\\midrule"
  )
  for (cr in crop_order) {
    cells <- vapply(inference_order, function(inf) {
      row <- inference[crop_label == cr & inference == inf]
      if (nrow(row) == 0L || !isTRUE(row$valid[1]) || !is.finite(row$se_one_sd[1])) return("NA")
      paste0(
        "\\shortstack{", fmt(row$estimate_one_sd[1], 4), stars(row$p[1]),
        "\\\\(", fmt(row$se_one_sd[1], 4), ")}"
      )
    }, character(1))
    lines <- c(lines, paste(c(cr, cells), collapse = " & "), "\\\\")
  }
  lines <- c(
    lines,
    "\\bottomrule",
    "\\end{tabular}",
    "}%",
    "\\par\\addvspace{0.7ex}",
    "\\parbox{0.98\\linewidth}{\\footnotesize\\textit{Notes:} Cells report the preferred-specification effect of a one-standard-deviation increase in crop-specific excess salinity, with standard errors in parentheses. All regressions include year fixed effects and changes from t to t+1 in GDD, KDD and soil moisture. Point estimates and samples are identical across columns; only the variance estimator changes. Strict Conley estimates use spherical distance and no positive-semidefinite covariance correction. NA indicates that a valid strict covariance estimate could not be obtained.}",
    "\\end{table}",
    "\\end{landscape}"
  )
  writeLines(lines, file.path(paths$out_dir, "FullRevision_Output_Conley_Robustness.tex"), useBytes = TRUE)
  invisible(inference)
}

make_soybean_excess_support_analysis <- function(incumbent_result = NULL) {
  write_status("Diagnosing support for soybean excess-salinity estimates.")
  if (is.null(incumbent_result)) {
    incumbent_result <- make_incumbent_crop_asinh_change_analysis()
  }
  sfd <- copy(incumbent_result$sfd)
  weather <- c("d_gdd_large", "d_kdd_large", "d_sm_season_large")
  needed <- c(
    "d_change_asinh_planted", "d_change_asinh_quantity",
    "d_excess_above_fao_large", "d_mean_salinity", weather,
    "Year", "pair_id", "lat", "lon",
    "excess_above_fao_large", "excess_above_fao_large_west"
  )
  d <- complete_data(sfd[crop == "soy"], needed)
  if (nrow(d) < 100L) stop("Insufficient soybean observations for the support diagnostic.")

  d[, `:=`(
    d_exceedance = as.numeric(excess_above_fao_large > 0) -
      as.numeric(excess_above_fao_large_west > 0),
    d_asinh_excess = asinh(excess_above_fao_large) -
      asinh(excess_above_fao_large_west)
  )]

  support <- d[, .(
    observations = .N,
    pairs = uniqueN(pair_id),
    nonzero_excess_differences = sum(abs(d_excess_above_fao_large) > 1e-12),
    pairs_with_nonzero_excess = uniqueN(pair_id[abs(d_excess_above_fao_large) > 1e-12]),
    treatment_sd_dsm = stats::sd(d_excess_above_fao_large),
    p99_absolute_nonzero_difference = stats::quantile(
      abs(d_excess_above_fao_large[abs(d_excess_above_fao_large) > 1e-12]), 0.99
    ),
    maximum_absolute_difference = max(abs(d_excess_above_fao_large))
  )]
  fwrite(
    support,
    file.path(paths$out_dir, "FullRevision_Soybean_Treatment_Support_Diagnostics.csv")
  )

  fit_diagnostic <- function(dep, treatment, data, exposure_label) {
    model <- fixest::feols(
      as.formula(paste(
        dep, "~", paste(c(treatment, weather), collapse = " + "), "| Year"
      )),
      data = data,
      cluster = ~pair_id,
      panel.id = ~pair_id + Year,
      notes = FALSE
    )
    row <- coef_numeric_row(model, treatment, exposure_label)
    data.table(
      exposure = exposure_label,
      observations = stats::nobs(model),
      pairs = uniqueN(data$pair_id),
      treatment_sd = stats::sd(data[[treatment]]),
      estimate = row$estimate,
      se = row$se,
      p = row$p
    )
  }

  outcomes <- c(
    d_change_asinh_planted = "Planted area",
    d_change_asinh_quantity = "Production quantity"
  )
  rows <- list()
  for (dep in names(outcomes)) {
    tests <- list(
      list("d_excess_above_fao_large", d, "Crop-specific excess: full sample"),
      list(
        "d_excess_above_fao_large",
        d[excess_above_fao_large > 0 | excess_above_fao_large_west > 0],
        "Crop-specific excess: exposed pairs"
      ),
      list("d_exceedance", d, "Difference in threshold exceedance"),
      list("d_asinh_excess", d, "Asinh crop-specific excess"),
      list("d_mean_salinity", d, "Continuous mean salinity (different estimand)")
    )
    for (test in tests) {
      row <- fit_diagnostic(dep, test[[1]], test[[2]], test[[3]])
      rows[[length(rows) + 1L]] <- cbind(outcome = outcomes[[dep]], row)
    }
  }
  estimates <- rbindlist(rows, fill = TRUE)
  fwrite(
    estimates,
    file.path(paths$out_dir, "FullRevision_Soybean_Treatment_Support_AlternativeModels.csv")
  )

  table_out <- estimates[, .(
    Outcome = outcome,
    Exposure = exposure,
    Coefficient = paste0(fmt(estimate, 4), stars(p)),
    `Std. error` = paste0("(", fmt(se, 4), ")"),
    `P-value` = fmt(p, 3),
    Observations = formatC(observations, format = "d", big.mark = ","),
    Pairs = formatC(pairs, format = "d", big.mark = ",")
  )]
  write_latex_df(
    table_out,
    file.path(paths$out_dir, "FullRevision_Soybean_Treatment_Support.tex"),
    "Soybean Excess-Salinity Support Diagnostics",
    "tab:full_revision_soybean_treatment_support",
    note = paste(
      "All regressions use the main soybean sample, year fixed effects, crop-season GDD, KDD and soil-moisture controls, and standard errors clustered by spatial pair.",
      "The exposed-pair regression retains only pair-years in which at least one municipality has positive excess above the 5 dS/m threshold.",
      "The threshold-exceedance coefficient compares a one-unit east-minus-west difference in exposure status.",
      "Continuous mean salinity uses dS/m but estimates a different response from excess above the soybean threshold."
    ),
    size = "\\scriptsize"
  )
  invisible(list(support = support, estimates = estimates))
}

make_incumbent_planted_area_mechanism_analysis <- function(incumbent_result = NULL) {
  write_status("Decomposing planted-area adjustment in municipality-crop cells with positive baseline values.")
  if (is.null(incumbent_result)) {
    incumbent_result <- make_incumbent_crop_asinh_change_analysis()
  }
  sfd <- copy(incumbent_result$sfd)

  crop_labels <- c(
    corn = "Corn", rice = "Rice", cassava = "Cassava",
    beans = "Beans", soy = "Soybeans", sugarcane = "Sugarcane"
  )
  outcome_defs <- list(
    own_area = list(
      dep = "d_change_asinh_planted",
      label = "Own-crop area change",
      plot = FALSE
    ),
    continuing_area = list(
      dep = "d_change_log_planted_stayer",
      label = "A. Positive municipal area at t+1: log area change",
      plot = TRUE
    ),
    established_area = list(
      dep = "d_change_asinh_planted_established",
      label = "Positive municipal area and production in t-1 and t",
      plot = FALSE
    ),
    crop_exit = list(
      dep = "d_exit_planted_next_pp",
      label = "One-year crop exit at t+1 (percentage points)",
      plot = FALSE
    ),
    persistent_crop_exit = list(
      dep = "d_persistent_exit_planted_next2_pp",
      label = "B. Zero municipal area at t+1 and t+2 (percentage points)",
      plot = TRUE
    ),
    prior_area_change = list(
      dep = "d_change_asinh_planted_previous",
      label = "Prior own-crop area change (placebo)",
      plot = FALSE
    )
  )
  treatment <- "d_excess_above_fao_large"
  weather <- c("d_gdd_large", "d_kdd_large", "d_sm_season_large")
  topo <- c("d_elevation_large", "d_slope_large", "d_clay_mean_large")
  coefficient_rows <- list()
  inference_rows <- list()
  pair_fe_rows <- list()

  for (outcome_name in names(outcome_defs)) {
    od <- outcome_defs[[outcome_name]]
    for (cr in names(crop_labels)) {
      needed <- c(od$dep, treatment, weather, topo, "Year", "pair_id", "lat", "lon")
      d <- complete_data(sfd[crop == cr], needed)
      if (nrow(d) < 100L || stats::sd(d[[treatment]]) == 0) next
      treatment_sd <- stats::sd(d[[treatment]])

      for (spec in 1:3) {
        controls <- switch(
          as.character(spec),
          "1" = character(),
          "2" = weather,
          "3" = c(weather, topo)
        )
        model <- fixest::feols(
          as.formula(paste(od$dep, "~", paste(c(treatment, controls), collapse = " + "), "| Year")),
          data = d,
          cluster = ~pair_id,
          panel.id = ~pair_id + Year,
          notes = FALSE
        )
        row <- coef_numeric_row(model, treatment, paste0("Spec. ", spec))
        coefficient_rows[[length(coefficient_rows) + 1L]] <- cbind(
          data.table(
            outcome = outcome_name,
            outcome_label = od$label,
            crop = cr,
            crop_label = crop_labels[[cr]],
            specification = paste0("Spec. ", spec),
            spec_id = spec,
            treatment_sd = treatment_sd,
            pairs = uniqueN(d$pair_id),
            years = uniqueN(d$Year)
          ),
          row[, .(estimate, se, p, observations)],
          spec_control_flags(spec)
        )

        if (spec == 2L) {
          inference_rows[[length(inference_rows) + 1L]] <- data.table(
            outcome = outcome_name,
            outcome_label = od$label,
            crop = cr,
            crop_label = crop_labels[[cr]],
            inference = "Pair cluster",
            cutoff_km = NA_integer_,
            estimate = row$estimate,
            se = row$se,
            p = row$p,
            observations = row$observations,
            treatment_sd = treatment_sd,
            valid = is.finite(row$se)
          )
          for (cutoff in c(50L, 100L)) {
            conley_model <- try(
              summary(
                model,
                vcov = fixest::vcov_conley(
                  lat = "lat", lon = "lon", cutoff = cutoff,
                  distance = "spherical", vcov_fix = FALSE
                )
              ),
              silent = TRUE
            )
            conley_row <- if (inherits(conley_model, "try-error")) {
              data.table(estimate = row$estimate, se = NA_real_, p = NA_real_)
            } else {
              coef_numeric_row(conley_model, treatment, paste0("Conley ", cutoff))[, .(estimate, se, p)]
            }
            inference_rows[[length(inference_rows) + 1L]] <- data.table(
              outcome = outcome_name,
              outcome_label = od$label,
              crop = cr,
              crop_label = crop_labels[[cr]],
              inference = paste0("Strict Conley ", cutoff, " km"),
              cutoff_km = cutoff,
              estimate = conley_row$estimate,
              se = conley_row$se,
              p = conley_row$p,
              observations = stats::nobs(model),
              treatment_sd = treatment_sd,
              valid = is.finite(conley_row$se)
            )
          }
        }
      }

      pair_fe_model <- fixest::feols(
        as.formula(paste(
          od$dep, "~", paste(c(treatment, weather), collapse = " + "),
          "| pair_id + Year"
        )),
        data = d,
        cluster = ~pair_id,
        panel.id = ~pair_id + Year,
        notes = FALSE
      )
      pair_fe_row <- coef_numeric_row(pair_fe_model, treatment, "Pair and year FE")
      pair_fe_rows[[length(pair_fe_rows) + 1L]] <- data.table(
        outcome = outcome_name,
        outcome_label = od$label,
        crop = cr,
        crop_label = crop_labels[[cr]],
        estimate = pair_fe_row$estimate,
        se = pair_fe_row$se,
        p = pair_fe_row$p,
        observations = pair_fe_row$observations,
        treatment_sd = treatment_sd
      )
    }
  }

  coefficients <- rbindlist(coefficient_rows, fill = TRUE)
  inference <- rbindlist(inference_rows, fill = TRUE)
  pair_fe <- rbindlist(pair_fe_rows, fill = TRUE)
  for (dt in list(coefficients, inference, pair_fe)) {
    dt[, `:=`(
      effect_one_sd = estimate * treatment_sd,
      se_one_sd = se * treatment_sd
    )]
  }
  fwrite(
    coefficients,
    file.path(paths$out_dir, "FullRevision_Incumbent_PlantedArea_Mechanisms_Coefficients.csv")
  )
  fwrite(
    inference,
    file.path(paths$out_dir, "FullRevision_Incumbent_PlantedArea_Mechanisms_Inference.csv")
  )
  fwrite(
    pair_fe,
    file.path(paths$out_dir, "FullRevision_Incumbent_PlantedArea_Mechanisms_PairFE.csv")
  )

  plot_data <- copy(coefficients[spec_id == 2L & outcome %chin%
    names(outcome_defs)[vapply(outcome_defs, `[[`, logical(1), "plot")]])
  plot_data[, `:=`(
    ci95_low = effect_one_sd - 1.96 * se_one_sd,
    ci95_high = effect_one_sd + 1.96 * se_one_sd,
    ci90_low = effect_one_sd - 1.645 * se_one_sd,
    ci90_high = effect_one_sd + 1.645 * se_one_sd,
    crop_label = factor(crop_label, levels = rev(unname(crop_labels))),
    outcome_label = factor(
      outcome_label,
      levels = vapply(outcome_defs[vapply(outcome_defs, `[[`, logical(1), "plot")], `[[`, character(1), "label")
    )
  )]
  fig <- ggplot(plot_data, aes(x = effect_one_sd, y = crop_label)) +
    geom_segment(
      aes(x = ci95_low, xend = ci95_high, yend = crop_label),
      linewidth = 0.45, alpha = 0.55, color = "#D95F02", lineend = "butt"
    ) +
    geom_segment(
      aes(x = ci90_low, xend = ci90_high, yend = crop_label),
      linewidth = 1.1, color = "#D95F02", lineend = "butt"
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.45, color = "grey25") +
    geom_point(size = 2.6, shape = 17, color = "#D95F02") +
    scale_y_discrete(limits = rev(unname(crop_labels)), drop = FALSE) +
    facet_wrap(~outcome_label, ncol = 1L, scales = "free_x") +
    labs(
      x = "Effect of a one-SD increase in excess salinity",
      y = NULL
    ) +
    theme_minimal(base_size = 11.5) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.3, color = "grey88"),
      panel.grid.major.x = element_line(linewidth = 0.3, color = "grey88"),
      strip.text = element_text(size = 11, face = "bold", color = "grey25"),
      axis.text.y = element_text(size = 10.5, face = "bold", color = "grey30"),
      axis.text.x = element_text(size = 9.5, color = "grey30"),
      axis.title.x = element_text(size = 10.5, margin = margin(t = 7)),
      plot.margin = margin(7, 10, 7, 7)
    )
  ggsave(
    file.path(paths$out_dir, "FullRevision_Incumbent_PlantedArea_Mechanisms.pdf"),
    fig, width = 7.1, height = 5.7
  )
  ggsave(
    file.path(paths$out_dir, "FullRevision_Incumbent_PlantedArea_Mechanisms.png"),
    fig, width = 7.1, height = 5.7, dpi = 320
  )

  invisible(list(
    coefficients = coefficients,
    inference = inference,
    pair_fe = pair_fe,
    figure = fig
  ))
}

make_excess_salinity_crop_mix_analysis <- function(pam = NULL) {
  write_status("Building crop-specific excess-salinity crop-mix analysis.")
  if (is.null(pam)) pam <- read_pam_zeros()
  pam <- copy(pam)
  pair_map <- read_pair_map()

  crop_labels <- c(
    beans = "Beans", cassava = "Cassava", corn = "Corn",
    rice = "Rice", soy = "Soybeans", sugarcane = "Sugarcane"
  )
  crops <- names(crop_labels)
  baseline_years <- 1988L:1992L
  analysis_start <- max(baseline_years) + 1L
  tolerance_thresholds <- c(
    beans = 1.0, cassava = 1.5, corn = 1.7,
    rice = 3.0, soy = 5.0, sugarcane = 1.7
  )
  tolerance_z <- as.numeric(scale(tolerance_thresholds))
  names(tolerance_z) <- names(tolerance_thresholds)

  pam <- pam[crop %chin% crops]
  pam[, `:=`(
    Code = trimws(as.character(Code)),
    crop = as.character(crop),
    Year = as.integer(Year),
    planted_area = as.numeric(planted_area),
    mean_salinity = as.numeric(mean_salinity),
    excess_above_fao_large = as.numeric(excess_above_fao_large),
    gdd_large = as.numeric(gdd_large),
    kdd_large = as.numeric(kdd_large),
    sm_season_large = as.numeric(sm_season_large)
  )]

  cell_check <- pam[, .(
    rows = .N,
    n_crops = uniqueN(crop),
    complete_area = all(is.finite(planted_area) & planted_area >= 0)
  ), by = .(Code, Year)]
  valid_cells <- cell_check[rows == length(crops) & n_crops == length(crops) & complete_area, .(Code, Year)]
  pam <- pam[valid_cells, on = .(Code, Year), nomatch = 0L]
  write_status(sprintf(
    "Crop-mix complete municipality-years: %s cells, %s rows.",
    format(nrow(valid_cells), big.mark = ","),
    format(nrow(pam), big.mark = ",")
  ))
  pam[, total_gross_planted_area := sum(planted_area), by = .(Code, Year)]
  pam <- pam[is.finite(total_gross_planted_area) & total_gross_planted_area > 0]
  pam[, `:=`(
    crop_share_pct = 100 * planted_area / total_gross_planted_area,
    asinh_planted_area = asinh(planted_area),
    asinh_total_gross_planted_area = asinh(total_gross_planted_area)
  )]

  share_check <- pam[, .(
    n_crops = uniqueN(crop),
    share_sum = sum(crop_share_pct)
  ), by = .(Code, Year)]
  if (share_check[n_crops != length(crops) | abs(share_sum - 100) > 1e-8, .N] > 0L) {
    stop("Crop-mix shares are not exhaustive within municipality-year cells.")
  }

  setorder(pam, Code, crop, Year)
  pam[, previous_year_crop_mix := shift(Year), by = .(Code, crop)]
  lag_sources <- c(
    "mean_salinity", "excess_above_fao_large",
    "gdd_large", "kdd_large", "sm_season_large"
  )
  for (v in lag_sources) {
    lag_name <- paste0(v, "_crop_mix_lag1")
    pam[, (lag_name) := shift(get(v)), by = .(Code, crop)]
    pam[!is.finite(previous_year_crop_mix) | previous_year_crop_mix != Year - 1L, (lag_name) := NA_real_]
  }
  pam[, common_excess_1_crop_mix_lag1 := pmax(mean_salinity_crop_mix_lag1 - 1, 0)]

  baseline <- pam[
    Year %in% baseline_years & is.finite(crop_share_pct),
    .(
      initial_share_pct = mean(crop_share_pct),
      initial_share_years = uniqueN(Year),
      initial_total_gross_area = mean(total_gross_planted_area),
      initial_asinh_planted_area = mean(asinh_planted_area)
    ),
    by = .(Code, crop)
  ][initial_share_years >= 3L]
  write_status(sprintf(
    "Crop-mix initial-specialization support (1988--1992): %s municipality-crop cells.",
    format(nrow(baseline), big.mark = ",")
  ))
  pam <- merge(pam, baseline, by = c("Code", "crop"), all = FALSE, sort = FALSE)
  pam <- pam[Year >= analysis_start]
  pam[, tolerance_z := unname(tolerance_z[crop])]
  write_status(sprintf(
    paste0(
      "Crop-mix municipality panel before SFD: %s rows, %s municipalities; ",
      "pair-map overlap east=%s, west=%s."
    ),
    format(nrow(pam), big.mark = ","),
    format(uniqueN(pam$Code), big.mark = ","),
    format(length(intersect(unique(pam$Code), unique(pair_map$Code))), big.mark = ","),
    format(length(intersect(unique(pam$Code), unique(pair_map$code_neighbor_west))), big.mark = ",")
  ))

  vars <- c(
    "crop_share_pct", "mean_salinity_crop_mix_lag1",
    "excess_above_fao_large_crop_mix_lag1",
    "common_excess_1_crop_mix_lag1", "asinh_planted_area",
    "asinh_total_gross_planted_area",
    "gdd_large_crop_mix_lag1", "kdd_large_crop_mix_lag1",
    "sm_season_large_crop_mix_lag1", "initial_share_pct",
    "initial_total_gross_area", "initial_asinh_planted_area", "tolerance_z"
  )
  sfd <- make_sfd_from_unit_panel(
    pam[, c("Code", "crop", "Year", vars), with = FALSE],
    pair_map,
    id_cols = c("crop", "Year"),
    vars = vars
  )

  setnames(
    sfd,
    c(
      "d_crop_share_pct", "d_mean_salinity_crop_mix_lag1",
      "d_excess_above_fao_large_crop_mix_lag1",
      "d_common_excess_1_crop_mix_lag1",
      "d_gdd_large_crop_mix_lag1", "d_kdd_large_crop_mix_lag1",
      "d_sm_season_large_crop_mix_lag1", "d_initial_share_pct"
    ),
    c(
      "d_crop_share_pct", "d_mean_salinity_lag1_common",
      "d_excess_above_fao_lag1_crop_mix",
      "d_common_excess_1_lag1",
      "d_gdd_lag1_crop_mix", "d_kdd_lag1_crop_mix",
      "d_sm_lag1_crop_mix", "d_initial_share_pct"
    ),
    skip_absent = TRUE
  )
  write_status(sprintf(
    "Crop-mix SFD before complete-case filtering: %s rows, %s pair-years.",
    format(nrow(sfd), big.mark = ","),
    format(uniqueN(sfd[, .(pair_id, Year)]), big.mark = ",")
  ))
  sfd[, `:=`(
    crop = factor(crop, levels = crops),
    pair_id = as.character(pair_id),
    Year = as.integer(Year),
    baseline_pair_mean_share_pct = (initial_share_pct + initial_share_pct_west) / 2,
    baseline_pair_mean_total_area = (initial_total_gross_area + initial_total_gross_area_west) / 2,
    year_centered = Year - analysis_start
  )]

  required <- c(
    "pair_id", "Year", "crop", "lat", "lon", "d_crop_share_pct",
    "d_excess_above_fao_lag1_crop_mix",
    "d_asinh_planted_area", "d_asinh_total_gross_planted_area",
    "d_gdd_lag1_crop_mix",
    "d_kdd_lag1_crop_mix", "d_sm_lag1_crop_mix", "d_initial_share_pct",
    "d_initial_asinh_planted_area", "baseline_pair_mean_share_pct",
    "baseline_pair_mean_total_area", "tolerance_z"
  )
  missing_diagnostics <- rbindlist(lapply(required, function(v) {
    x <- sfd[[v]]
    data.table(
      variable = v,
      observations = length(x),
      missing_or_nonfinite = if (is.numeric(x)) sum(!is.finite(x)) else sum(is.na(x)),
      missing_share = if (is.numeric(x)) mean(!is.finite(x)) else mean(is.na(x))
    )
  }))
  fwrite(
    missing_diagnostics,
    file.path(paths$out_dir, "FullRevision_CropMix_Missing_Diagnostics.csv")
  )
  sfd <- complete_data(sfd, required)
  write_status(sprintf(
    "Crop-mix SFD after complete-case filtering: %s rows, %s pair-years.",
    format(nrow(sfd), big.mark = ","),
    format(uniqueN(sfd[, .(pair_id, Year)]), big.mark = ",")
  ))

  balanced_keys <- sfd[, .(
    rows = .N,
    n_crops = uniqueN(crop)
  ), by = .(pair_id, Year)][
    rows == length(crops) & n_crops == length(crops),
    .(pair_id, Year)
  ]
  write_status(sprintf(
    "Balanced crop-mix support: %s pair-years.",
    format(nrow(balanced_keys), big.mark = ",")
  ))
  sfd <- sfd[balanced_keys, on = .(pair_id, Year), nomatch = 0L]
  setorder(sfd, pair_id, Year, crop)

  adding_up <- sfd[, .(
    share_difference_sum = sum(d_crop_share_pct),
    crop_count = uniqueN(crop)
  ), by = .(pair_id, Year)]
  if (adding_up[crop_count != length(crops) | abs(share_difference_sum) > 1e-7, .N] > 0L) {
    stop("The balanced SFD crop shares do not add up to zero within pair-year cells.")
  }
  if (nrow(sfd) == 0L) stop("No balanced crop-mix observations remain.")

  pair_support <- sfd[, .(observed_years = uniqueN(Year)), by = pair_id]
  sfd <- sfd[pair_id %chin% pair_support[observed_years >= 2L, pair_id]]
  setorder(sfd, pair_id, Year, crop)
  adding_up <- sfd[, .(
    share_difference_sum = sum(d_crop_share_pct),
    crop_count = uniqueN(crop)
  ), by = .(pair_id, Year)]
  if (adding_up[crop_count != length(crops) | abs(share_difference_sum) > 1e-7, .N] > 0L) {
    stop("The estimation-sample SFD crop shares do not add up to zero within pair-year cells.")
  }

  sfd[, initial_share_trend := d_initial_share_pct * year_centered]
  sfd[, initial_asinh_area_trend := d_initial_asinh_planted_area * year_centered]
  sfd[, centered_initial_share :=
    baseline_pair_mean_share_pct - mean(baseline_pair_mean_share_pct),
    by = crop
  ]
  sfd[, excess_x_initial_share :=
    d_excess_above_fao_lag1_crop_mix * centered_initial_share
  ]

  rhs_by_spec <- list(
    "Spec. 1" = "d_excess_above_fao_lag1_crop_mix",
    "Spec. 2" = paste(
      c(
        "d_excess_above_fao_lag1_crop_mix", "d_gdd_lag1_crop_mix",
        "d_kdd_lag1_crop_mix", "d_sm_lag1_crop_mix"
      ),
      collapse = " + "
    ),
    "Spec. 3" = paste(
      c(
        "d_excess_above_fao_lag1_crop_mix", "d_gdd_lag1_crop_mix",
        "d_kdd_lag1_crop_mix", "d_sm_lag1_crop_mix", "d_initial_share_pct",
        "initial_share_trend"
      ),
      collapse = " + "
    )
  )
  fe_by_spec <- c("Spec. 1" = "Year", "Spec. 2" = "Year", "Spec. 3" = "Year")

  models <- list()
  specialization_models <- list()
  coefficient_rows <- list()
  inference_rows <- list()
  specialization_rows <- list()
  salinity_term <- "d_excess_above_fao_lag1_crop_mix"
  for (cr in crops) {
    crop_data <- sfd[as.character(crop) == cr]
    crop_models <- list()
    for (spec_name in names(rhs_by_spec)) {
      fml <- as.formula(paste0(
        "d_crop_share_pct ~ ", rhs_by_spec[[spec_name]], " | ", fe_by_spec[[spec_name]]
      ))
      model <- feols(
        fml,
        data = crop_data,
        vcov = make_conley(lat = "lat", lon = "lon", cutoff = 200),
        notes = FALSE
      )
      crop_models[[spec_name]] <- model
      row <- coef_numeric_row(model, salinity_term, spec_name)
      cluster_row <- coef_numeric_row(summary(model, vcov = ~pair_id), salinity_term, spec_name)
      coefficient_rows[[length(coefficient_rows) + 1L]] <- data.table(
        crop = cr,
        crop_label = crop_labels[[cr]],
        specification = spec_name,
        estimate = row$estimate,
        se = row$se,
        p = row$p,
        observations = row$observations,
        cluster_se = cluster_row$se,
        cluster_p = cluster_row$p
      )
    }
    models[[cr]] <- crop_models

    specialization_model <- feols(
      d_crop_share_pct ~ d_excess_above_fao_lag1_crop_mix + excess_x_initial_share +
        d_gdd_lag1_crop_mix + d_kdd_lag1_crop_mix + d_sm_lag1_crop_mix +
        d_initial_share_pct + initial_share_trend | Year,
      data = crop_data,
      vcov = make_conley(lat = "lat", lon = "lon", cutoff = 200),
      notes = FALSE
    )
    specialization_models[[cr]] <- specialization_model
    base_row <- coef_numeric_row(specialization_model, salinity_term, "At mean initial share")
    int_row <- coef_numeric_row(specialization_model, "excess_x_initial_share", "Interaction")
    specialization_rows[[length(specialization_rows) + 1L]] <- data.table(
      crop = cr,
      crop_label = crop_labels[[cr]],
      salinity_effect_at_mean_initial_share = base_row$estimate,
      salinity_effect_se = base_row$se,
      salinity_effect_p = base_row$p,
      salinity_x_initial_share = int_row$estimate,
      interaction_se = int_row$se,
      interaction_p = int_row$p
    )
  }

  coefficient_dt <- rbindlist(coefficient_rows, fill = TRUE)
  specialization_dt <- rbindlist(specialization_rows, fill = TRUE)
  for (spec_name in names(rhs_by_spec)) {
    inference_rows[[length(inference_rows) + 1L]] <- data.table(
      specification = spec_name,
      max_absolute_adding_up_error = max(abs(adding_up$share_difference_sum)),
      observations_per_crop = unique(coefficient_dt[specification == spec_name, observations]),
      pairs = uniqueN(sfd$pair_id),
      years = uniqueN(sfd$Year)
    )
  }
  inference_dt <- rbindlist(inference_rows, fill = TRUE)
  treatment_scale <- sfd[, .(
    treatment_sd = sd(d_excess_above_fao_lag1_crop_mix)
  ), by = .(crop)]
  coefficient_dt <- merge(
    coefficient_dt,
    treatment_scale,
    by = "crop",
    all.x = TRUE,
    sort = FALSE
  )
  coefficient_dt[, `:=`(
    estimate_plot = estimate * treatment_sd,
    se_plot = se * treatment_sd,
    ci95_low = (estimate - 1.96 * se) * treatment_sd,
    ci95_high = (estimate + 1.96 * se) * treatment_sd,
    ci90_low = (estimate - 1.645 * se) * treatment_sd,
    ci90_high = (estimate + 1.645 * se) * treatment_sd
  )]

  diagnostics <- sfd[, .(
    observations = .N,
    pairs = uniqueN(pair_id),
    years = uniqueN(Year),
    mean_share_pct = mean((crop_share_pct + crop_share_pct_west) / 2),
    mean_initial_share_pct = mean(baseline_pair_mean_share_pct),
    zero_share_east = mean(crop_share_pct == 0),
    zero_share_west = mean(crop_share_pct_west == 0),
    mean_pair_total_gross_area = mean(baseline_pair_mean_total_area),
    tolerance_threshold_ds_m = tolerance_thresholds[as.character(first(crop))]
  ), by = .(crop, crop_label = crop_labels[as.character(crop)])]

  fwrite(coefficient_dt, file.path(paths$out_dir, "FullRevision_CropMix_ExcessSalinity_Coefficients.csv"))
  fwrite(inference_dt, file.path(paths$out_dir, "FullRevision_CropMix_ExcessSalinity_DesignDiagnostics.csv"))
  fwrite(diagnostics, file.path(paths$out_dir, "FullRevision_CropMix_ExcessSalinity_SampleDiagnostics.csv"))
  fwrite(specialization_dt, file.path(paths$out_dir, "FullRevision_CropMix_ExcessSalinity_InitialSpecialization.csv"))

  table_file <- file.path(paths$out_dir, "FullRevision_CropMix_ExcessSalinity.tex")
  table_terms <- c(
    "d_excess_above_fao_lag1_crop_mix" = "Crop-specific FAO excess salinity (t-1)",
    "d_gdd_lag1_crop_mix" = "GDD (t-1)",
    "d_kdd_lag1_crop_mix" = "KDD (t-1)",
    "d_sm_lag1_crop_mix" = "Soil moisture (t-1)",
    "d_initial_share_pct" = "Initial-share difference",
    "initial_share_trend" = "Initial-share difference x trend"
  )
  table_lines <- c(
    "\\begin{landscape}", "\\color{red}", "\\small",
    "\\setlength{\\tabcolsep}{8pt}", "\\renewcommand{\\arraystretch}{0.82}",
    "\\setlength{\\LTleft}{0pt}", "\\setlength{\\LTright}{0pt}",
    "\\begin{longtable}{@{\\extracolsep{\\fill}}lccc}",
    "\\caption{Crop-Specific Excess Salinity and Municipal Crop Mix}\\label{tab:full_revision_crop_mix_excess_salinity}\\\\",
    "\\toprule", " & Spec. 1 & Spec. 2 & Spec. 3 \\\\", "\\midrule", "\\endfirsthead",
    "\\toprule", " & Spec. 1 & Spec. 2 & Spec. 3 \\\\", "\\midrule", "\\endhead",
    "\\bottomrule", "\\endfoot"
  )
  for (cr in crops) {
    if (cr != crops[1]) table_lines <- c(table_lines, "\\pagebreak")
    table_lines <- c(
      table_lines,
      paste0("\\multicolumn{4}{l}{\\textbf{", crop_labels[[cr]], "}}\\\\")
    )
    for (term in names(table_terms)) {
      coef_cells <- se_cells <- character()
      for (spec_name in names(rhs_by_spec)) {
        model <- models[[cr]][[spec_name]]
        row <- coef_numeric_row(model, term, spec_name)
        if (is.finite(row$estimate)) {
          coef_cells <- c(coef_cells, paste0(fmt(row$estimate, 3), stars(row$p)))
          se_cells <- c(se_cells, paste0("(", fmt(row$se, 3), ")"))
        } else {
          coef_cells <- c(coef_cells, "--")
          se_cells <- c(se_cells, "")
        }
      }
      table_lines <- c(
        table_lines,
        paste(c(table_terms[[term]], coef_cells), collapse = " & "), "\\\\",
        paste(c("", se_cells), collapse = " & "), "\\\\"
      )
    }
    n_cells <- vapply(models[[cr]], function(m) formatC(nobs(m), format = "d", big.mark = ","), character(1))
    table_lines <- c(
      table_lines,
      paste(c("Pair fixed effects", "No", "No", "No"), collapse = " & "), "\\\\",
      paste(c("Year fixed effects", "Yes", "Yes", "Yes"), collapse = " & "), "\\\\",
      paste(c("Observations", n_cells), collapse = " & "), "\\\\", "\\addlinespace[0.55em]"
    )
  }
  table_lines <- c(
    table_lines, "\\end{longtable}",
    "\\par\\addvspace{0.5ex}",
    paste0(
      "\\parbox{0.96\\linewidth}{\\footnotesize\\textit{Notes:} ",
      "Each panel is estimated separately on the same balanced pair-year sample. ",
      "The dependent variable is the east-minus-west difference in the crop share of gross planted area, in percentage points. ",
      "For each crop, the treatment is the east-minus-west difference in lagged mean excess salinity above that crop's FAO threshold, in dS/m. ",
      "Spec. 1 includes year fixed effects. Preferred Spec. 2 adds lagged crop-specific GDD, KDD and soil-moisture controls. ",
      "Spec. 3 adds the 1988--1992 initial share difference and its crop-specific linear trend. ",
      "There are no pair-by-year or state fixed effects. Conley spatial standard errors with a 200 km cutoff are in parentheses. ",
      "When required, fixest applies its positive-semidefinite correction to the Conley covariance matrix. ",
      "All six crops, including zero shares, are retained, and their SFD shares sum to zero in every pair-year. Because thresholds differ across crops, coefficients do not form an adding-up system and are not responses to one identical treatment. ",
      "Gross planted area is not physical cropland: sequential soybean and second-crop maize may use the same hectare within a year.}"
    ),
    "\\end{landscape}"
  )
  writeLines(table_lines, table_file, useBytes = TRUE)

  d <- copy(coefficient_dt)
  crop_order_top <- unname(crop_labels[c("corn", "rice", "cassava", "beans", "soy", "sugarcane")])
  crop_order_bottom <- rev(crop_order_top)
  offsets <- c("Spec. 1" = -0.20, "Spec. 2" = 0, "Spec. 3" = 0.20)
  d[, specification := factor(specification, levels = names(offsets))]
  d[, crop_position := match(crop_label, crop_order_bottom)]
  d[, plot_position := crop_position + offsets[as.character(specification)]]
  pal <- c("Spec. 1" = "#1B9E77", "Spec. 2" = "#D95F02", "Spec. 3" = "#7570B3")
  shapes <- c("Spec. 1" = 16, "Spec. 2" = 17, "Spec. 3" = 15)
  fig <- ggplot(d, aes(x = estimate_plot, y = plot_position, color = specification, shape = specification)) +
    geom_segment(aes(x = ci95_low, xend = ci95_high, yend = plot_position), linewidth = 0.42, alpha = 0.55, lineend = "butt") +
    geom_segment(aes(x = ci90_low, xend = ci90_high, yend = plot_position), linewidth = 1.05, lineend = "butt") +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4, color = "grey20") +
    geom_point(size = 2.25, stroke = 0.55) +
    scale_y_continuous(
      breaks = seq_along(crop_order_bottom), labels = crop_order_bottom,
      expand = expansion(mult = c(0.08, 0.08))
    ) +
    scale_color_manual(values = pal, guide = "none") +
    scale_shape_manual(values = shapes, guide = "none") +
    labs(
      x = "Effect of a one-SD increase in SFD lagged crop-specific excess salinity\non gross planted-area share (percentage points)",
      y = NULL
    ) +
    theme_minimal(base_size = 10.5) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.32, color = "grey88"),
      panel.grid.major.x = element_line(linewidth = 0.32, color = "grey88"),
      axis.text.y = element_text(size = 10.5, face = "bold", color = "grey30"),
      axis.text.x = element_text(size = 9.2, color = "grey30"),
      axis.title.x = element_text(size = 10.2, margin = margin(t = 7)),
      plot.margin = margin(7, 9, 7, 7)
    )
  ggsave(file.path(paths$out_dir, "FullRevision_CropMix_ExcessSalinity.pdf"), fig, width = 7.0, height = 4.8)
  ggsave(file.path(paths$out_dir, "FullRevision_CropMix_ExcessSalinity.png"), fig, width = 7.0, height = 4.8, dpi = 320)

  write_status(
    "Crop-specific excess-salinity crop mix completed: ", nrow(sfd), " stacked observations, ",
    uniqueN(sfd$pair_id), " pairs and ", uniqueN(sfd$Year), " years."
  )
  invisible(list(
    coefficients = coefficient_dt,
    diagnostics = diagnostics,
    adding_up = inference_dt,
    specialization = specialization_dt,
    models = models,
    specialization_models = specialization_models,
    sfd = sfd
  ))
}

make_crop_mix_bias_diagnostics <- function(crop_mix) {
  write_status("Building excess-salinity crop-mix identification diagnostics.")
  sfd <- copy(crop_mix$sfd)
  crop_labels <- c(
    beans = "Beans", cassava = "Cassava", corn = "Corn",
    rice = "Rice", soy = "Soybeans", sugarcane = "Sugarcane"
  )
  crops <- names(crop_labels)
  treatment <- "d_excess_above_fao_lag1_crop_mix"
  weather <- c("d_gdd_lag1_crop_mix", "d_kdd_lag1_crop_mix", "d_sm_lag1_crop_mix")

  pair_crop_year <- unique(sfd[, .(
    crop = as.character(crop), pair_id, Year,
    excess_difference = d_excess_above_fao_lag1_crop_mix,
    excess_east = excess_above_fao_large_crop_mix_lag1,
    excess_west = excess_above_fao_large_crop_mix_lag1_west
  )])
  setorder(pair_crop_year, crop, pair_id, Year)
  pair_crop_year[, `:=`(
    lag_excess_difference = shift(excess_difference),
    lag_year = shift(Year)
  ), by = .(crop, pair_id)]
  pair_crop_year[, between_excess_difference :=
    mean(excess_difference), by = .(crop, pair_id)]
  pair_crop_year[, within_excess_difference :=
    excess_difference - between_excess_difference]
  pair_means <- pair_crop_year[, .(
    between_excess_difference = first(between_excess_difference)
  ), by = .(crop, pair_id)]
  tolerance_thresholds <- c(
    beans = 1.0, cassava = 1.5, corn = 1.7,
    rice = 3.0, soy = 5.0, sugarcane = 1.7
  )
  support <- pair_crop_year[, {
    consecutive <- is.finite(lag_year) & Year == lag_year + 1L
    side_levels <- c(excess_east, excess_west)
    total_variance <- var(excess_difference)
    list(
      pair_year_observations = .N,
      pairs = uniqueN(pair_id),
      threshold_ds_m = tolerance_thresholds[first(crop)],
      treatment_sd = sd(excess_difference),
      within_pair_sd = sd(within_excess_difference),
      within_variance_share = if (is.finite(total_variance) && total_variance > 0) {
        var(within_excess_difference) / total_variance
      } else {
        NA_real_
      },
      treatment_ar1 = if (sum(consecutive) >= 2L) {
        cor(excess_difference[consecutive], lag_excess_difference[consecutive])
      } else {
        NA_real_
      },
      mean_absolute_annual_change = if (any(consecutive)) {
        mean(abs(excess_difference[consecutive] - lag_excess_difference[consecutive]))
      } else {
        NA_real_
      },
      municipality_side_positive_share = mean(side_levels > 1e-12),
      mean_municipality_side_excess = mean(side_levels),
      p90_municipality_side_excess = as.numeric(quantile(side_levels, 0.9)),
      maximum_municipality_side_excess = max(side_levels)
    )
  }, by = crop]
  between_support <- pair_means[, .(
    between_pair_sd = sd(between_excess_difference)
  ), by = crop]
  support <- merge(support, between_support, by = "crop", sort = FALSE)
  fwrite(
    support,
    file.path(paths$out_dir, "FullRevision_CropMix_ExcessSalinity_Support.csv")
  )
  treatment_sd_by_crop <- setNames(support$treatment_sd, support$crop)

  sfd[, excess_between := mean(get(treatment)), by = .(pair_id, crop)]
  sfd[, excess_within := get(treatment) - excess_between]
  setorder(sfd, crop, pair_id, Year)
  sfd[, `:=`(
    dd_crop_share_pct = d_crop_share_pct - shift(d_crop_share_pct),
    dd_asinh_planted_area = d_asinh_planted_area - shift(d_asinh_planted_area),
    dd_excess_above_fao_lag1_crop_mix = get(treatment) - shift(get(treatment)),
    dd_gdd_lag1_crop_mix = d_gdd_lag1_crop_mix - shift(d_gdd_lag1_crop_mix),
    dd_kdd_lag1_crop_mix = d_kdd_lag1_crop_mix - shift(d_kdd_lag1_crop_mix),
    dd_sm_lag1_crop_mix = d_sm_lag1_crop_mix - shift(d_sm_lag1_crop_mix),
    lag_year_diagnostic = shift(Year)
  ), by = .(crop, pair_id)]
  nonconsecutive <- !is.finite(sfd$lag_year_diagnostic) |
    sfd$Year != sfd$lag_year_diagnostic + 1L
  fd_vars <- c(
    "dd_crop_share_pct", "dd_asinh_planted_area",
    "dd_excess_above_fao_lag1_crop_mix", "dd_gdd_lag1_crop_mix",
    "dd_kdd_lag1_crop_mix", "dd_sm_lag1_crop_mix"
  )
  sfd[nonconsecutive, (fd_vars) := NA_real_]

  extract_diagnostic <- function(model, term, crop, outcome, estimator) {
    cluster_row <- coef_numeric_row(model, term, estimator)
    conley_model <- try(
      summary(model, vcov = make_conley(lat = "lat", lon = "lon", cutoff = 200)),
      silent = TRUE
    )
    conley_row <- if (inherits(conley_model, "try-error")) {
      data.table(estimate = cluster_row$estimate, se = NA_real_, p = NA_real_)
    } else {
      coef_numeric_row(conley_model, term, estimator)[, .(estimate, se, p)]
    }
    data.table(
      crop = crop,
      crop_label = crop_labels[[crop]],
      outcome = outcome,
      estimator = estimator,
      estimate = cluster_row$estimate,
      pair_cluster_se = cluster_row$se,
      pair_cluster_p = cluster_row$p,
      conley_estimate = conley_row$estimate,
      conley_se = conley_row$se,
      conley_p = conley_row$p,
      observations = cluster_row$observations,
      treatment_sd = treatment_sd_by_crop[[crop]]
    )
  }

  rows <- list()
  mundlak_rows <- list()
  for (cr in crops) {
    d <- sfd[as.character(crop) == cr]
    d[, initial_area_trend_diagnostic :=
      d_initial_asinh_planted_area * year_centered]

    share_cross_section <- feols(
      d_crop_share_pct ~ d_excess_above_fao_lag1_crop_mix +
        d_gdd_lag1_crop_mix + d_kdd_lag1_crop_mix + d_sm_lag1_crop_mix +
        d_initial_share_pct + initial_share_trend | Year,
      data = d,
      cluster = ~pair_id,
      notes = FALSE
    )
    share_pair_fe <- feols(
      d_crop_share_pct ~ d_excess_above_fao_lag1_crop_mix +
        d_gdd_lag1_crop_mix + d_kdd_lag1_crop_mix + d_sm_lag1_crop_mix |
        pair_id + Year,
      data = d,
      cluster = ~pair_id,
      notes = FALSE
    )
    fd_data <- complete_data(d, c(fd_vars, "Year", "pair_id", "lat", "lon"))
    share_temporal_fd <- feols(
      dd_crop_share_pct ~ dd_excess_above_fao_lag1_crop_mix +
        dd_gdd_lag1_crop_mix + dd_kdd_lag1_crop_mix + dd_sm_lag1_crop_mix |
        Year,
      data = fd_data,
      cluster = ~pair_id,
      notes = FALSE
    )

    area_cross_section <- feols(
      d_asinh_planted_area ~ d_excess_above_fao_lag1_crop_mix +
        d_gdd_lag1_crop_mix + d_kdd_lag1_crop_mix + d_sm_lag1_crop_mix +
        d_initial_asinh_planted_area + initial_area_trend_diagnostic | Year,
      data = d,
      cluster = ~pair_id,
      notes = FALSE
    )
    area_pair_fe <- feols(
      d_asinh_planted_area ~ d_excess_above_fao_lag1_crop_mix +
        d_gdd_lag1_crop_mix + d_kdd_lag1_crop_mix + d_sm_lag1_crop_mix |
        pair_id + Year,
      data = d,
      cluster = ~pair_id,
      notes = FALSE
    )
    area_temporal_fd <- feols(
      dd_asinh_planted_area ~ dd_excess_above_fao_lag1_crop_mix +
        dd_gdd_lag1_crop_mix + dd_kdd_lag1_crop_mix + dd_sm_lag1_crop_mix |
        Year,
      data = fd_data,
      cluster = ~pair_id,
      notes = FALSE
    )

    rows[[length(rows) + 1L]] <- rbindlist(list(
      extract_diagnostic(share_cross_section, treatment, cr, "Crop share (pp)", "Cross-sectional Spec. 3"),
      extract_diagnostic(share_pair_fe, treatment, cr, "Crop share (pp)", "Pair and year FE"),
      extract_diagnostic(
        share_temporal_fd, "dd_excess_above_fao_lag1_crop_mix", cr,
        "Crop share (pp)", "Temporal first difference"
      ),
      extract_diagnostic(area_cross_section, treatment, cr, "asinh planted area", "Cross-sectional Spec. 3"),
      extract_diagnostic(area_pair_fe, treatment, cr, "asinh planted area", "Pair and year FE"),
      extract_diagnostic(
        area_temporal_fd, "dd_excess_above_fao_lag1_crop_mix", cr,
        "asinh planted area", "Temporal first difference"
      )
    ))

    mundlak_model <- feols(
      d_crop_share_pct ~ excess_within + excess_between +
        d_gdd_lag1_crop_mix + d_kdd_lag1_crop_mix + d_sm_lag1_crop_mix +
        d_initial_share_pct + initial_share_trend | Year,
      data = d,
      cluster = ~pair_id,
      notes = FALSE
    )
    for (term in c("excess_within", "excess_between")) {
      row <- coef_numeric_row(mundlak_model, term, term)
      mundlak_rows[[length(mundlak_rows) + 1L]] <- data.table(
        crop = cr,
        crop_label = crop_labels[[cr]],
        component = sub("excess_", "", term),
        estimate = row$estimate,
        se = row$se,
        p = row$p,
        observations = row$observations
      )
    }

  }

  diagnostics <- rbindlist(rows)
  diagnostics[, `:=`(
    estimate_scaled = estimate * treatment_sd,
    se_scaled = pair_cluster_se * treatment_sd,
    ci95_low = (estimate - 1.96 * pair_cluster_se) * treatment_sd,
    ci95_high = (estimate + 1.96 * pair_cluster_se) * treatment_sd,
    ci90_low = (estimate - 1.645 * pair_cluster_se) * treatment_sd,
    ci90_high = (estimate + 1.645 * pair_cluster_se) * treatment_sd
  )]
  mundlak <- rbindlist(mundlak_rows)
  fwrite(diagnostics, file.path(paths$out_dir, "FullRevision_CropMix_ExcessSalinity_IdentificationDiagnostics.csv"))
  fwrite(mundlak, file.path(paths$out_dir, "FullRevision_CropMix_ExcessSalinity_WithinBetween.csv"))

  estimator_levels <- c(
    "Cross-sectional Spec. 3", "Pair and year FE", "Temporal first difference"
  )
  plot_data <- copy(diagnostics)
  plot_data[, estimator := factor(estimator, levels = estimator_levels)]
  crop_order_top <- unname(crop_labels[c("corn", "rice", "cassava", "beans", "soy", "sugarcane")])
  crop_order_bottom <- rev(crop_order_top)
  offsets <- c(
    "Cross-sectional Spec. 3" = -0.20,
    "Pair and year FE" = 0,
    "Temporal first difference" = 0.20
  )
  plot_data[, crop_position := match(crop_label, crop_order_bottom)]
  plot_data[, plot_position := crop_position + offsets[as.character(estimator)]]
  pal <- c(
    "Cross-sectional Spec. 3" = "#1B9E77",
    "Pair and year FE" = "#D95F02",
    "Temporal first difference" = "#7570B3"
  )
  shapes <- c(
    "Cross-sectional Spec. 3" = 16,
    "Pair and year FE" = 17,
    "Temporal first difference" = 15
  )

  make_panel <- function(outcome_name, x_label, panel_title) {
    d <- plot_data[outcome == outcome_name]
    ggplot(d, aes(
      x = estimate_scaled, y = plot_position,
      color = estimator, shape = estimator
    )) +
      geom_segment(
        aes(x = ci95_low, xend = ci95_high, yend = plot_position),
        linewidth = 0.38, alpha = 0.55, lineend = "butt"
      ) +
      geom_segment(
        aes(x = ci90_low, xend = ci90_high, yend = plot_position),
        linewidth = 0.95, lineend = "butt"
      ) +
      geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4, color = "grey20") +
      geom_point(size = 2.1, stroke = 0.5) +
      scale_y_continuous(
        breaks = seq_along(crop_order_bottom), labels = crop_order_bottom,
        expand = expansion(mult = c(0.08, 0.08))
      ) +
      scale_color_manual(values = pal, guide = "none") +
      scale_shape_manual(values = shapes, guide = "none") +
      labs(title = panel_title, x = x_label, y = NULL) +
      theme_minimal(base_size = 9.5) +
      theme(
        legend.position = "none",
        plot.title = element_text(size = 10.2, face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.3, color = "grey88"),
        panel.grid.major.x = element_line(linewidth = 0.3, color = "grey88"),
        axis.text.y = element_text(size = 9.2, face = "bold", color = "grey30"),
        axis.text.x = element_text(size = 8.2, color = "grey30"),
        axis.title.x = element_text(size = 8.8, margin = margin(t = 6)),
        plot.margin = margin(6, 7, 5, 6)
      )
  }

  share_panel <- make_panel(
    "Crop share (pp)",
    "Effect of a one-SD crop-specific excess-salinity increase\non gross planted-area share (percentage points)",
    "A. Crop-share outcome"
  )
  area_panel <- make_panel(
    "asinh planted area",
    "Effect of a one-SD crop-specific excess-salinity increase\non asinh planted area",
    "B. Absolute planted-area outcome"
  )
  write_diagnostic_plot <- function(filename, device = c("pdf", "png")) {
    device <- match.arg(device)
    if (device == "pdf") {
      grDevices::pdf(filename, width = 10.2, height = 4.9)
    } else {
      grDevices::png(filename, width = 10.2, height = 4.9, units = "in", res = 320)
    }
    on.exit(grDevices::dev.off(), add = TRUE)
    grid::grid.newpage()
    layout <- grid::grid.layout(1, 2)
    grid::pushViewport(grid::viewport(layout = layout))
    print(share_panel, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
    print(area_panel, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
    grid::popViewport()
  }
  write_diagnostic_plot(
    file.path(paths$out_dir, "FullRevision_CropMix_ExcessSalinity_IdentificationDiagnostics.pdf"),
    "pdf"
  )
  write_diagnostic_plot(
    file.path(paths$out_dir, "FullRevision_CropMix_ExcessSalinity_IdentificationDiagnostics.png"),
    "png"
  )

  write_status(
    "Excess-salinity crop-mix diagnostics completed for ", nrow(support),
    " crop-specific treatments."
  )
  invisible(list(
    coefficients = diagnostics,
    within_between = mundlak,
    support = support,
    sfd = sfd
  ))
}

download_sidra_corn_seasons <- function(force = FALSE, chunk_size = 500L) {
  ensure_output_dir()
  out_file <- file.path(
    paths$sidra_dir,
    paste0("corn_seasons_839_2003_2018_n", chunk_size, ".csv")
  )
  if (!force && file.exists(out_file) && file.info(out_file)$size > 0) {
    return(fread(out_file, encoding = "UTF-8"))
  }

  old_timeout <- getOption("sidrar.timeout")
  old_retries <- getOption("sidrar.retries")
  on.exit({
    options(sidrar.timeout = old_timeout)
    options(sidrar.retries = old_retries)
  }, add = TRUE)
  options(sidrar.timeout = 120, sidrar.retries = 1)

  code_chunks <- sidra_pair_sample_codes(chunk_size = chunk_size)
  fetch_codes <- function(codes, chunk_id) {
    chunk_file <- file.path(
      paths$sidra_dir,
      paste0("corn_seasons_839_2003_2018_n", chunk_size, "_chunk_", chunk_id, ".csv")
    )
    if (!force && file.exists(chunk_file) && file.info(chunk_file)$size > 0) {
      return(fread(chunk_file, encoding = "UTF-8"))
    }
    if (length(codes) > 250L) {
      mid <- ceiling(length(codes) / 2)
      return(rbindlist(list(
        fetch_codes(codes[seq_len(mid)], paste0(chunk_id, "a")),
        fetch_codes(codes[(mid + 1L):length(codes)], paste0(chunk_id, "b"))
      ), fill = TRUE))
    }
    write_status(
      "Downloading SIDRA table 839 corn seasons for municipality chunk ",
      chunk_id, " (", length(codes), " municipalities)."
    )
    z <- try(
      sidrar::get_sidra(
        x = 839,
        variable = "109",
        period = as.character(2003L:2018L),
        geo = "City",
        geo.filter = list(City = codes),
        classific = "81",
        category = list(c("114253", "114254")),
        value_type = "both"
      ),
      silent = TRUE
    )
    if (inherits(z, "try-error")) {
      write_status("SIDRA table 839 failed for chunk ", chunk_id, ": ", as.character(z))
      if (length(codes) > 1L) {
        mid <- ceiling(length(codes) / 2)
        return(rbindlist(list(
          fetch_codes(codes[seq_len(mid)], paste0(chunk_id, "a")),
          fetch_codes(codes[(mid + 1L):length(codes)], paste0(chunk_id, "b"))
        ), fill = TRUE))
      }
      return(NULL)
    }
    z <- as.data.table(z)
    z[, chunk_id := as.character(chunk_id)]
    fwrite(z, chunk_file)
    Sys.sleep(0.15)
    z
  }

  rows <- lapply(names(code_chunks), function(chunk_id) {
    fetch_codes(code_chunks[[chunk_id]], chunk_id)
  })
  raw <- rbindlist(rows, fill = TRUE)
  if (nrow(raw) == 0L) return(NULL)
  fwrite(raw, out_file)
  raw
}

tidy_sidra_corn_seasons <- function(raw) {
  if (is.null(raw) || nrow(raw) == 0L) return(NULL)
  raw <- as.data.table(raw)
  score_column <- function(pattern) {
    vapply(raw, function(x) {
      x <- as.character(x)
      mean(grepl(pattern, x), na.rm = TRUE)
    }, numeric(1))
  }
  municipality_col <- names(which.max(score_column("^[0-9]{7}$")))
  year_col <- names(which.max(score_column("^(200[3-9]|201[0-8])$")))
  category_col <- names(which.max(score_column("^11425[34]$")))
  if (!all(c("Valor", "Valor_raw") %in% names(raw))) {
    stop("SIDRA table 839 did not return the expected value columns.")
  }
  dt <- raw[, .(
    Code = trimws(as.character(get(municipality_col))),
    Year = as.integer(as.character(get(year_col))),
    season_code = as.character(get(category_col)),
    numeric_value = suppressWarnings(as.numeric(Valor)),
    raw_value = trimws(as.character(Valor_raw))
  )]
  dt[, planted_area_ha := fifelse(
    is.finite(numeric_value),
    numeric_value,
    fifelse(raw_value %chin% c("-", "0"), 0, NA_real_)
  )]
  dt <- dt[
    grepl("^[0-9]{7}$", Code) & Year %between% c(2003L, 2018L) &
      season_code %chin% c("114253", "114254")
  ]
  dt[, season := fifelse(season_code == "114253", "first_crop", "second_crop")]
  dcast(
    dt,
    Code + Year ~ season,
    value.var = "planted_area_ha",
    fun.aggregate = function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
  )
}

make_corn_season_crop_mix_diagnostic <- function(crop_mix, force_download = FALSE) {
  write_status("Building first- versus second-crop corn diagnostics from SIDRA table 839.")
  raw <- download_sidra_corn_seasons(force = force_download)
  seasons <- tidy_sidra_corn_seasons(raw)
  if (is.null(seasons) || nrow(seasons) == 0L) {
    write_status("Skipping corn-season diagnostics: SIDRA table 839 is unavailable.")
    return(NULL)
  }

  sfd_corn <- copy(crop_mix$sfd[as.character(crop) == "corn"])
  east <- sfd_corn[, .(
    Code,
    Year,
    total_selected_area = sinh(asinh_total_gross_planted_area),
    total_corn_area = sinh(asinh_planted_area),
    excess_salinity_lag1 = excess_above_fao_large_crop_mix_lag1,
    gdd_lag1 = gdd_large_crop_mix_lag1,
    kdd_lag1 = kdd_large_crop_mix_lag1,
    sm_lag1 = sm_season_large_crop_mix_lag1
  )]
  west <- sfd_corn[, .(
    Code = code_neighbor_west,
    Year,
    total_selected_area = sinh(asinh_total_gross_planted_area_west),
    total_corn_area = sinh(asinh_planted_area_west),
    excess_salinity_lag1 = excess_above_fao_large_crop_mix_lag1_west,
    gdd_lag1 = gdd_large_crop_mix_lag1_west,
    kdd_lag1 = kdd_large_crop_mix_lag1_west,
    sm_lag1 = sm_season_large_crop_mix_lag1_west
  )]
  municipality <- unique(rbindlist(list(east, west)))
  municipality <- merge(
    municipality,
    seasons,
    by = c("Code", "Year"),
    all.x = TRUE,
    sort = FALSE
  )
  municipality <- municipality[Year >= 2003L]
  municipality[, `:=`(
    first_crop_share_pct = 100 * first_crop / total_selected_area,
    second_crop_share_pct = 100 * second_crop / total_selected_area,
    total_corn_share_pct = 100 * total_corn_area / total_selected_area,
    asinh_first_crop = asinh(first_crop),
    asinh_second_crop = asinh(second_crop),
    asinh_total_corn = asinh(total_corn_area),
    season_sum_difference_ha = first_crop + second_crop - total_corn_area
  )]

  season_validation <- municipality[
    complete.cases(first_crop, second_crop, total_corn_area),
    .(
      municipality_years = .N,
      mean_absolute_difference_ha = mean(abs(season_sum_difference_ha)),
      median_absolute_difference_ha = median(abs(season_sum_difference_ha)),
      exact_or_one_ha_match_share = mean(abs(season_sum_difference_ha) <= 1),
      missing_first_crop_share = mean(!is.finite(first_crop)),
      missing_second_crop_share = mean(!is.finite(second_crop))
    )
  ]
  fwrite(
    season_validation,
    file.path(paths$out_dir, "FullRevision_CropMix_ExcessSalinity_CornSeasonDataValidation.csv")
  )

  vars <- c(
    "first_crop_share_pct", "second_crop_share_pct", "total_corn_share_pct",
    "asinh_first_crop", "asinh_second_crop", "asinh_total_corn",
    "excess_salinity_lag1", "gdd_lag1", "kdd_lag1", "sm_lag1"
  )
  season_sfd <- make_sfd_from_unit_panel(
    municipality[, c("Code", "Year", vars), with = FALSE],
    read_pair_map(),
    id_cols = "Year",
    vars = vars
  )
  needed <- c(
    paste0("d_", vars), "pair_id", "Year", "lat", "lon"
  )
  season_sfd <- complete_data(season_sfd, needed)
  pair_support <- season_sfd[, .(observed_years = uniqueN(Year)), by = pair_id]
  season_sfd <- season_sfd[pair_id %chin% pair_support[observed_years >= 2L, pair_id]]
  setorder(season_sfd, pair_id, Year)
  difference_vars <- paste0("d_", vars)
  for (v in difference_vars) {
    season_sfd[, paste0("dd_", sub("^d_", "", v)) := get(v) - shift(get(v)), by = pair_id]
  }
  season_sfd[, previous_year := shift(Year), by = pair_id]
  dd_vars <- paste0("dd_", vars)
  season_sfd[!is.finite(previous_year) | Year != previous_year + 1L, (dd_vars) := NA_real_]

  outcome_defs <- list(
    first_crop_share_pct = "First-crop corn share (pp)",
    second_crop_share_pct = "Second-crop corn share (pp)",
    total_corn_share_pct = "Total corn share (pp)",
    asinh_first_crop = "Asinh first-crop corn area",
    asinh_second_crop = "Asinh second-crop corn area",
    asinh_total_corn = "Asinh total corn area"
  )
  model_rows <- list()
  for (outcome in names(outcome_defs)) {
    level_dep <- paste0("d_", outcome)
    fd_dep <- paste0("dd_", outcome)
    pair_fe <- feols(
      as.formula(paste0(
        level_dep,
        " ~ d_excess_salinity_lag1 + d_gdd_lag1 + d_kdd_lag1 + d_sm_lag1 | pair_id + Year"
      )),
      data = season_sfd,
      cluster = ~pair_id,
      notes = FALSE
    )
    fd_data <- complete_data(season_sfd, c(
      fd_dep, "dd_excess_salinity_lag1", "dd_gdd_lag1", "dd_kdd_lag1",
      "dd_sm_lag1", "pair_id", "Year", "lat", "lon"
    ))
    temporal_fd <- feols(
      as.formula(paste0(
        fd_dep,
        " ~ dd_excess_salinity_lag1 + dd_gdd_lag1 + dd_kdd_lag1 + dd_sm_lag1 | Year"
      )),
      data = fd_data,
      cluster = ~pair_id,
      notes = FALSE
    )
    for (definition in list(
      list(
        model = pair_fe, term = "d_excess_salinity_lag1",
        estimator = "Pair and year FE", data = season_sfd
      ),
      list(
        model = temporal_fd, term = "dd_excess_salinity_lag1",
        estimator = "Temporal first difference", data = fd_data
      )
    )) {
      row <- coef_numeric_row(definition$model, definition$term, definition$estimator)
      conley_model <- try(
        summary(definition$model, vcov = make_conley(cutoff = 200)),
        silent = TRUE
      )
      conley_row <- if (inherits(conley_model, "try-error")) {
        data.table(se = NA_real_, p = NA_real_)
      } else {
        coef_numeric_row(conley_model, definition$term, definition$estimator)[, .(se, p)]
      }
      model_rows[[length(model_rows) + 1L]] <- data.table(
        outcome = outcome,
        outcome_label = outcome_defs[[outcome]],
        estimator = definition$estimator,
        estimate = row$estimate,
        pair_cluster_se = row$se,
        pair_cluster_p = row$p,
        conley_se = conley_row$se,
        conley_p = conley_row$p,
        observations = row$observations,
        pairs = uniqueN(definition$data$pair_id)
      )
    }
  }
  results <- rbindlist(model_rows)
  fwrite(
    results,
    file.path(paths$out_dir, "FullRevision_CropMix_ExcessSalinity_CornSeasonDiagnostics.csv")
  )
  fwrite(
    municipality[, .(
      Code, Year, first_crop, second_crop, total_corn_area,
      total_selected_area, season_sum_difference_ha
    )],
    file.path(paths$out_dir, "FullRevision_CropMix_ExcessSalinity_CornSeasonMunicipal.csv")
  )
  write_status(
    "Corn-season diagnostics completed: ", nrow(season_sfd),
    " pair-years and ", uniqueN(season_sfd$pair_id), " pairs."
  )
  invisible(list(results = results, validation = season_validation, sfd = season_sfd))
}

make_figure1_readable <- function() {
  write_status("Building readable yield coefficient figure.")
  coef_file <- file.path(paths$data_dir, "crop_yield_sfd_coefficient_plot_data_200km.csv")
  if (!file.exists(coef_file)) return(invisible(NULL))
  d <- fread(coef_file)
  d <- d[exposure == "Crop-specific FAO excess salinity"]
  d[, crop_label := factor(crop_label, levels = c("Corn", "Rice", "Cassava", "Beans", "Soybeans", "Sugarcane"))]
  d[, specification := factor(specification, levels = c("Spec 1", "Spec 2", "Spec 3"))]
  pal <- c("Spec 1" = "#6B6B6B", "Spec 2" = "#2F6F8F", "Spec 3" = "#B55D2A")
  fig <- ggplot(d, aes(x = estimate, y = crop_label, color = specification)) +
    geom_vline(xintercept = 0, linewidth = 0.45, color = "grey45") +
    geom_segment(aes(x = ci95_low, xend = ci95_high, yend = crop_label), linewidth = 0.65, position = position_dodge(width = 0.55)) +
    geom_segment(aes(x = ci90_low, xend = ci90_high, yend = crop_label), linewidth = 1.25, position = position_dodge(width = 0.55)) +
    geom_point(size = 2.8, position = position_dodge(width = 0.55)) +
    scale_color_manual(values = pal) +
    labs(x = "Effect on log yield", y = NULL, color = NULL) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text = element_text(size = 13),
      axis.title.x = element_text(size = 14),
      legend.text = element_text(size = 13),
      plot.margin = margin(10, 12, 10, 12)
    )
  ggsave(file.path(paths$out_dir, "FullRevision_Figure1_Yield_Coefficients_Readable.pdf"), fig, width = 8.2, height = 4.8)
  ggsave(file.path(paths$out_dir, "FullRevision_Figure1_Yield_Coefficients_Readable.png"), fig, width = 8.2, height = 4.8, dpi = 320)
  invisible(d)
}

make_ibge_scope_figure_readable <- function() {
  write_status("Building readable IBGE selected-crop scope figure.")
  input_csv <- file.path(paths$data_dir, "ibge_selected_crops_area_value_scope.csv")
  if (!file.exists(input_csv)) return(invisible(NULL))
  yearly <- fread(input_csv)
  crop_order <- c("corn", "rice", "cassava", "beans", "soy", "sugarcane")
  crop_labels <- c(
    corn = "Corn", rice = "Rice", cassava = "Cassava",
    beans = "Beans", soy = "Soybeans", sugarcane = "Sugarcane"
  )
  area_long <- melt(
    yearly,
    id.vars = "year",
    measure.vars = paste0(crop_order, "_area_mha"),
    variable.name = "crop",
    value.name = "value"
  )
  area_long[, `:=`(
    crop = sub("_area_mha$", "", crop),
    panel = "B. Selected planted area by crop (million ha)"
  )]
  value_long <- melt(
    yearly,
    id.vars = "year",
    measure.vars = paste0(crop_order, "_value_share_pct"),
    variable.name = "crop",
    value.name = "value"
  )
  value_long[, `:=`(
    crop = sub("_value_share_pct$", "", crop),
    panel = "C. Selected crops in crop-production value (%)"
  )]

  area_long[, crop_label := factor(crop_labels[crop], levels = crop_labels[crop_order])]
  value_long[, crop_label := factor(crop_labels[crop], levels = crop_labels[crop_order])]

  panel_theme <- theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 11),
      legend.title = element_blank(),
      plot.title = element_text(size = 13, face = "bold", hjust = 0),
      axis.text = element_text(size = 11, color = "black"),
      axis.title = element_text(size = 11),
      panel.grid.minor = element_blank(),
      plot.margin = margin(5, 8, 5, 8)
    )

  p_a <- ggplot(yearly, aes(year, selected_share_pct)) +
    geom_ribbon(aes(ymin = 0, ymax = selected_share_pct), fill = "#A6CEE3", alpha = 0.45) +
    geom_hline(yintercept = mean(yearly$selected_share_pct, na.rm = TRUE), linetype = "dashed", linewidth = 0.35, color = "grey35") +
    geom_line(color = "#1F78B4", linewidth = 0.9) +
    geom_point(color = "#1F78B4", size = 1.4) +
    scale_x_continuous(breaks = c(1988, 1995, 2000, 2005, 2010, 2015, 2018)) +
    labs(title = "A. Selected crops in temporary-crop planted area (%)", x = NULL, y = NULL) +
    panel_theme +
    theme(legend.position = "none")

  p_b <- ggplot(area_long, aes(year, value, fill = crop_label)) +
    geom_area(alpha = 0.9, linewidth = 0) +
    scale_x_continuous(breaks = c(1988, 1995, 2000, 2005, 2010, 2015, 2018)) +
    labs(title = "B. Selected planted area by crop (million ha)", x = NULL, y = NULL) +
    panel_theme +
    theme(legend.position = "none")

  p_c <- ggplot(value_long, aes(year, value, fill = crop_label)) +
    geom_area(alpha = 0.9, linewidth = 0) +
    geom_line(data = yearly, aes(year, selected_value_share_pct, color = "Selected crops total"), inherit.aes = FALSE, linewidth = 0.75) +
    geom_hline(yintercept = mean(yearly$selected_value_share_pct, na.rm = TRUE), linetype = "dashed", linewidth = 0.35, color = "grey35") +
    scale_color_manual(values = c("Selected crops total" = "black"), name = NULL) +
    scale_x_continuous(breaks = c(1988, 1995, 2000, 2005, 2010, 2015, 2018)) +
    labs(title = "C. Selected crops in crop-production value (%)", x = "Year", y = NULL, fill = NULL) +
    panel_theme

  write_scope_plot <- function(filename, device = c("pdf", "png")) {
    device <- match.arg(device)
    if (device == "pdf") {
      grDevices::pdf(filename, width = 11.2, height = 7.7)
    } else {
      grDevices::png(filename, width = 11.2, height = 7.7, units = "in", res = 320)
    }
    on.exit(grDevices::dev.off(), add = TRUE)
    grid::grid.newpage()
    lay <- grid::grid.layout(2, 2, heights = grid::unit(c(1, 1.15), "null"))
    grid::pushViewport(grid::viewport(layout = lay))
    print(p_a, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
    print(p_b, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
    print(p_c, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1:2))
    grid::popViewport()
  }

  write_scope_plot(file.path(paths$out_dir, "FullRevision_IBGE_Selected_Crops_Scope_Readable.pdf"), "pdf")
  write_scope_plot(file.path(paths$out_dir, "FullRevision_IBGE_Selected_Crops_Scope_Readable.png"), "png")
  invisible(yearly)
}

# =============================================================================
# 5. Producer prices and nonlinear salinity-response diagnostics
# =============================================================================
read_ibge_ipca_annual <- function() {
  cache <- file.path(paths$out_dir, "IBGE_SIDRA_1737_IPCA_1995_2018.csv")
  if (!file.exists(cache)) {
    url <- paste0(
      "https://apisidra.ibge.gov.br/values/t/1737/n1/all/v/2266/",
      "p/199501-201812"
    )
    raw <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE)
    raw <- as.data.table(raw[-1, , drop = FALSE])
    fwrite(raw, cache)
  } else {
    raw <- fread(cache, encoding = "UTF-8")
  }
  ipca <- raw[, .(
    month = as.integer(D3C),
    ipca_index = as.numeric(V)
  )]
  ipca <- ipca[is.finite(month) & is.finite(ipca_index)]
  ipca[, Year := month %/% 100L]
  annual <- ipca[, .(ipca_annual_mean = mean(ipca_index)), by = Year]
  if (!all(1995:2018 %in% annual$Year)) stop("The cached IPCA series is incomplete for 1995--2018.")
  annual
}

make_price_analysis <- function() {
  write_status("Building IPCA-deflated PAM producer-price indices and panel SFD price regressions.")
  pam <- read_pam_zeros()
  pair_map <- read_pair_map()
  controls <- c(
    "excess_above_fao_large", "gdd_large", "kdd_large", "sm_season_large",
    "elevation_large", "slope_large", "clay_mean_large", "quantity", "value"
  )
  controls <- intersect(controls, names(pam))
  pam[, producer_price_nominal := fifelse(
    is.finite(value) & value > 0 & is.finite(quantity) & quantity > 0,
    value / quantity,
    NA_real_
  )]
  pam[, log_producer_price := fifelse(
    is.finite(producer_price_nominal) & producer_price_nominal > 0,
    log(producer_price_nominal),
    NA_real_
  )]
  pam[, c("price_p01", "price_p99") := {
    x <- log_producer_price[is.finite(log_producer_price)]
    if (length(x) < 20L) list(NA_real_, NA_real_) else {
      q <- stats::quantile(x, c(0.01, 0.99), na.rm = TRUE, names = FALSE, type = 8)
      list(q[1], q[2])
    }
  }, by = .(crop, Year)]
  pam[, log_producer_price_trimmed := fifelse(
    is.finite(log_producer_price) & log_producer_price >= price_p01 & log_producer_price <= price_p99,
    log_producer_price,
    NA_real_
  )]

  national_price <- pam[
    Year >= 1995 & Year <= 2018 & is.finite(value) & value > 0 & is.finite(quantity) & quantity > 0,
    .(producer_price_nominal = sum(value, na.rm = TRUE) / sum(quantity, na.rm = TRUE)),
    by = .(crop, Year)
  ]
  national_price <- merge(national_price, read_ibge_ipca_annual(), by = "Year", all.x = TRUE)
  national_price[, producer_price_real := producer_price_nominal / ipca_annual_mean]
  base <- national_price[Year == 1995, .(base_price = producer_price_real), by = crop]
  national_price <- merge(national_price, base, by = "crop", all.x = TRUE)
  national_price[, real_price_index_1995 := 100 * producer_price_real / base_price]
  fwrite(national_price, file.path(paths$out_dir, "FullRevision_PAM_Real_Producer_Price_Index_1995_2018.csv"))

  crop_labels <- c(beans = "Beans", cassava = "Cassava", corn = "Corn", rice = "Rice", soy = "Soybeans", sugarcane = "Sugarcane")
  national_price[, crop_label := crop_labels[crop]]
  pal <- c(
    Beans = "#CC79A7", Cassava = "#009E73", Corn = "#D55E00",
    Rice = "#0072B2", Soybeans = "#E69F00", Sugarcane = "#222222"
  )
  line_types <- c(
    Beans = "solid", Cassava = "longdash", Corn = "twodash",
    Rice = "dotdash", Soybeans = "dashed", Sugarcane = "dotted"
  )
  fig <- ggplot(national_price, aes(
    Year, real_price_index_1995, color = crop_label, linetype = crop_label
  )) +
    geom_hline(yintercept = 100, linewidth = 0.35, color = "grey55") +
    geom_line(linewidth = 0.95) +
    scale_color_manual(values = pal) +
    scale_linetype_manual(values = line_types) +
    labs(x = NULL, y = "Real price index\n(1995 = 100)", color = NULL, linetype = NULL) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 13),
      axis.title.y = element_text(size = 14),
      legend.text = element_text(size = 13)
    )
  ggsave(file.path(paths$out_dir, "FullRevision_PAM_National_Price_Index.pdf"), fig, width = 7.8, height = 4.3)
  ggsave(file.path(paths$out_dir, "FullRevision_PAM_National_Price_Index.png"), fig, width = 7.8, height = 4.3, dpi = 320)

  sfd_vars <- c(controls, "log_producer_price", "log_producer_price_trimmed")
  sfd <- make_sfd_from_unit_panel(
    pam[, c("Code", "Year", "crop", sfd_vars), with = FALSE],
    pair_map,
    id_cols = c("Year", "crop"),
    vars = sfd_vars
  )
  sfd <- sfd[is.finite(d_log_producer_price)]
  vc <- make_conley()
  weather <- c("d_gdd_large", "d_kdd_large", "d_sm_season_large")
  topo <- c("d_elevation_large", "d_slope_large", "d_clay_mean_large")
  rows <- list()
  coef_plot_rows <- list()
  robustness_rows <- list()
  price_sfd_mean_rows <- list()
  models <- list()
  robust_models <- list()
  table_crop_order <- intersect(c("corn", "rice", "cassava", "beans", "soy", "sugarcane"), unique(sfd$crop))
  table_headers_crop <- character()
  table_headers_spec <- character()
  for (cr in table_crop_order) {
    needed <- c(
      "d_log_producer_price", "d_excess_above_fao_large", weather, topo,
      "Year", "state", "lat", "lon", "pair_id"
    )
    d <- complete_data(sfd[crop == cr], needed)
    if (nrow(d) < 100L) next
    price_sfd_mean_rows[[length(price_sfd_mean_rows) + 1L]] <- data.table(
      crop = cr,
      Crop = crop_labels[[cr]],
      observations = nrow(d),
      mean_sfd_log_price = mean(d$d_log_producer_price),
      sd_sfd_log_price = stats::sd(d$d_log_producer_price),
      relative_random_pair_sd = stats::sd(d$d_log_producer_price) /
        (sqrt(2) * stats::sd(c(d$log_producer_price, d$log_producer_price_west))),
      year_min = min(d$Year),
      year_max = max(d$Year),
      equivalent_percent = 100 * (exp(mean(d$d_log_producer_price)) - 1)
    )
    rhs_full <- paste(c("d_excess_above_fao_large", weather, topo), collapse = " + ")
    rhs_panel <- paste(c("d_excess_above_fao_large", weather), collapse = " + ")
    f1 <- as.formula(paste("d_log_producer_price ~", rhs_full, "| Year"))
    f2 <- as.formula(paste("d_log_producer_price ~", rhs_panel, "| pair_id + Year"))
    f3 <- as.formula(paste("d_log_producer_price ~", rhs_panel, "| pair_id + state^Year"))
    m1 <- fixest::feols(f1, data = d, vcov = vc, panel.id = ~pair_id + Year, notes = FALSE)
    m2 <- fixest::feols(f2, data = d, vcov = vc, panel.id = ~pair_id + Year, notes = FALSE)
    m3 <- fixest::feols(f3, data = d, vcov = vc, panel.id = ~pair_id + Year, notes = FALSE)
    crop_models <- list(m1, m2, m3)
    for (s in seq_along(crop_models)) {
      label <- paste0("Spec. ", s)
      models[[paste(crop_labels[[cr]], label, sep = " - ")]] <- crop_models[[s]]
      rows[[length(rows) + 1L]] <- cbind(
        Crop = crop_labels[[cr]],
        coef_row(crop_models[[s]], "d_excess_above_fao_large", label)
      )
      coef_plot_rows[[length(coef_plot_rows) + 1L]] <- cbind(
        data.table(Crop = crop_labels[[cr]], crop = cr, specification = label, spec_id = s),
        coef_numeric_row(crop_models[[s]], "d_excess_above_fao_large", label)
      )
    }
    table_headers_crop <- c(table_headers_crop, rep(crop_labels[[cr]], 3L))
    table_headers_spec <- c(table_headers_spec, paste0("Spec. ", 1:3))

    d_trim <- complete_data(
      sfd[crop == cr],
      c("d_log_producer_price_trimmed", "d_excess_above_fao_large", weather, "Year", "state", "lat", "lon", "pair_id")
    )
    f_trim <- as.formula(paste("d_log_producer_price_trimmed ~", rhs_panel, "| pair_id + state^Year"))
    m_trim <- fixest::feols(f_trim, data = d_trim, vcov = vc, panel.id = ~pair_id + Year, notes = FALSE)
    robust_models[[crop_labels[[cr]]]] <- m_trim
    robustness_rows[[length(robustness_rows) + 1L]] <- cbind(
      Crop = crop_labels[[cr]],
      coef_numeric_row(m3, "d_excess_above_fao_large", "Full sample"),
      trimmed_estimate = unname(coef(m_trim)["d_excess_above_fao_large"]),
      trimmed_se = sqrt(vcov(m_trim)["d_excess_above_fao_large", "d_excess_above_fao_large"]),
      trimmed_p = coeftable(m_trim)["d_excess_above_fao_large", "Pr(>|t|)"],
      trimmed_observations = nobs(m_trim)
    )
  }
  price_table <- rbindlist(rows, fill = TRUE)
  setnames(price_table, "Model", "Specification")
  fwrite(price_table, file.path(paths$out_dir, "FullRevision_Price_Effects_ExcessSalinity_Coefficients.csv"))
  price_robustness <- rbindlist(robustness_rows, fill = TRUE)
  fwrite(price_robustness, file.path(paths$out_dir, "FullRevision_Price_Effects_PanelFE_Trimmed_Robustness.csv"))

  price_sfd_means <- rbindlist(price_sfd_mean_rows, fill = TRUE)
  price_sfd_means[, crop_order := match(crop, table_crop_order)]
  setorder(price_sfd_means, crop_order)
  fwrite(
    price_sfd_means[, .(
      Crop, observations, mean_sfd_log_price, sd_sfd_log_price,
      relative_random_pair_sd, year_min, year_max, equivalent_percent
    )],
    file.path(paths$out_dir, "FullRevision_Price_SFD_Means_ByCrop.csv")
  )
  price_sfd_means_display <- price_sfd_means[, .(
    Crop,
    `Mean SFD log price` = fmt(mean_sfd_log_price, 6),
    `Equivalent difference (%)` = fmt(equivalent_percent, 3),
    Observations = formatC(observations, format = "d", big.mark = ",")
  )]
  write_latex_df(
    price_sfd_means_display,
    file.path(paths$out_dir, "FullRevision_Price_SFD_Means_ByCrop.tex"),
    "Mean Spatial Difference in Municipal Producer Prices by Crop",
    "tab:full_revision_price_sfd_means_by_crop",
    paste(
      "The SFD outcome is the east-minus-west difference in the log PAM average producer price,",
      "where the municipal price equals production value divided by production quantity.",
      "The equivalent difference is 100 times exp(mean SFD) minus one.",
      "Statistics use the crop-specific complete-data sample common to the three price specifications.",
      "Because the IPCA deflator is common to both municipalities in a given year, it cancels from the within-year log spatial difference."
    ),
    size = "\\small"
  )

  price_plot <- rbindlist(coef_plot_rows, fill = TRUE)
  price_plot[, `:=`(
    ci95_low = estimate - 1.96 * se,
    ci95_high = estimate + 1.96 * se,
    ci90_low = estimate - 1.645 * se,
    ci90_high = estimate + 1.645 * se
  )]
  crop_order_top <- c("Corn", "Rice", "Cassava", "Beans", "Soybeans", "Sugarcane")
  crop_order_bottom <- rev(crop_order_top)
  offsets <- c("Spec. 1" = -0.22, "Spec. 2" = 0, "Spec. 3" = 0.22)
  price_plot[, specification := factor(specification, levels = names(offsets))]
  price_plot[, crop_position := match(Crop, crop_order_bottom)]
  price_plot[, plot_position := crop_position + offsets[as.character(specification)]]
  pal_coef <- c("Spec. 1" = "#1B9E77", "Spec. 2" = "#D95F02", "Spec. 3" = "#7570B3")
  shapes_coef <- c("Spec. 1" = 16, "Spec. 2" = 17, "Spec. 3" = 15)
  price_fig <- ggplot(price_plot, aes(x = estimate, y = plot_position, color = specification, shape = specification)) +
    geom_segment(aes(x = ci95_low, xend = ci95_high, yend = plot_position), linewidth = 0.75, alpha = 0.55, lineend = "butt") +
    geom_segment(aes(x = ci90_low, xend = ci90_high, yend = plot_position), linewidth = 2.1, lineend = "butt") +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.65, color = "grey20") +
    geom_point(size = 4.2, stroke = 0.9) +
    scale_y_continuous(breaks = seq_along(crop_order_bottom), labels = crop_order_bottom, expand = expansion(mult = c(0.08, 0.08))) +
    scale_color_manual(values = pal_coef, guide = "none") +
    scale_shape_manual(values = shapes_coef, guide = "none") +
    labs(x = "Effect on log producer price of 1 dS/m of excess salinity", y = NULL) +
    theme_minimal(base_size = 16) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.45, color = "grey88"),
      panel.grid.major.x = element_line(linewidth = 0.45, color = "grey88"),
      axis.text.y = element_text(size = 16, face = "bold", color = "grey30"),
      axis.text.x = element_text(size = 14, color = "grey30"),
      axis.title.x = element_text(size = 15, margin = margin(t = 10)),
      plot.margin = margin(12, 14, 12, 12)
    )
  ggsave(file.path(paths$out_dir, "FullRevision_Price_Effects_ExcessSalinity_Coefficients.pdf"), price_fig, width = 8.8, height = 6.3)
  ggsave(file.path(paths$out_dir, "FullRevision_Price_Effects_ExcessSalinity_Coefficients.png"), price_fig, width = 8.8, height = 6.3, dpi = 320)

  fixest::setFixest_dict(c(
    d_excess_above_fao_large = "$\\Delta_s$ Excess salinity (dS/m)",
    d_gdd_large = "$\\Delta_s$ GDD",
    d_kdd_large = "$\\Delta_s$ KDD",
    d_sm_season_large = "$\\Delta_s$ Soil moisture",
    d_elevation_large = "$\\Delta_s$ Elevation",
    d_slope_large = "$\\Delta_s$ Slope",
    d_clay_mean_large = "$\\Delta_s$ Clay"
  ), reset = TRUE)
  price_table_note <- paste(
    "Notes: The dependent variable is the east-minus-west difference in the log PAM average producer price, equal to production value divided by production quantity.",
    "Spec. 1 includes year fixed effects, weather, soil and topographic controls.",
    "Spec. 2 includes pair and year fixed effects plus weather controls; Spec. 3 replaces year fixed effects with state-by-year fixed effects and is preferred.",
    "Time-invariant soil and topographic differences are absorbed by pair fixed effects in Specs. 2--3.",
    "All specifications start from a crop-specific common complete-data sample; observation counts can differ because singleton fixed-effect groups are removed in Specs. 2--3. Standard errors are Conley spatial with a 200 km cutoff."
  )
  price_panel_groups <- list(
    A = seq_len(min(9L, length(models))),
    B = if (length(models) > 9L) 10L:length(models) else integer()
  )
  for (panel_name in names(price_panel_groups)) {
    model_ids <- price_panel_groups[[panel_name]]
    if (!length(model_ids)) next
    tex_file <- file.path(paths$out_dir, paste0("FullRevision_Price_Effects_ExcessSalinity_Panel", panel_name, ".tex"))
    fixest::etable(
      models[model_ids],
      tex = TRUE,
      file = tex_file,
      replace = TRUE,
      headers = list(
        "Crop" = table_headers_crop[model_ids],
        "Spec" = table_headers_spec[model_ids]
      ),
      depvar = FALSE,
      title = paste0("Soil Salinity and Municipal Producer Prices: Panel ", panel_name),
      label = paste0("tab:full_revision_price_effects_panel_", tolower(panel_name)),
      fitstat = ~ r2 + n,
      signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
      notes = price_table_note
    )
    style_fixest_tex(
      tex_file,
      size = "\\scriptsize",
      tabcolsep = "2.5pt",
      arraystretch = "0.78",
      resize = TRUE,
      landscape = TRUE
    )
  }

  robust_file <- file.path(paths$out_dir, "FullRevision_Price_Effects_Trimmed_Robustness.tex")
  fixest::etable(
    robust_models,
    tex = TRUE,
    file = robust_file,
    replace = TRUE,
    depvar = FALSE,
    title = "Producer-Price Robustness after Trimming Extreme Unit Values",
    label = "tab:full_revision_price_effects_trimmed",
    fitstat = ~ r2 + n,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    notes = paste(
      "Notes: Municipal log producer prices below the first or above the ninety-ninth percentile within crop-year are excluded before spatial differencing.",
      "All models use the preferred specification with pair and state-by-year fixed effects, weather controls, and Conley spatial standard errors at 200 km."
    )
  )
  style_fixest_tex(
    robust_file,
    size = "\\scriptsize",
    tabcolsep = "3pt",
    arraystretch = "0.8",
    resize = TRUE
  )

  invisible(list(
    price_index = national_price,
    sfd = sfd,
    price_sfd_means = price_sfd_means,
    models = models,
    robustness_models = robust_models,
    figure = price_plot
  ))
}

make_raw_mean_salinity_bin_figure <- function() {
  write_status("Rebuilding the nonlinear yield figure with an explicit raw-salinity axis.")
  coefficient_file <- file.path(
    paths$data_dir, "nonlinear_salinity_bins_crop_yield_coefficients_200km.csv"
  )
  support_file <- file.path(
    paths$data_dir, "nonlinear_salinity_bins_crop_yield_support_200km.csv"
  )
  if (!all(file.exists(c(coefficient_file, support_file)))) {
    stop("Nonlinear salinity-bin coefficient or support data are missing.")
  }

  coefficients <- fread(coefficient_file)
  support <- fread(support_file)
  bin_labels <- support[order(bin_position), as.character(bin_label)]
  crop_order <- c("Corn", "Rice", "Cassava", "Beans", "Soybeans", "Sugarcane")
  coefficients[, crop_label := factor(crop_label, levels = crop_order)]
  support[, `:=`(
    ymin = -0.425,
    ymax = -0.425 + 0.085 * mean_crop_share / max(mean_crop_share),
    xmin = bin_position - 0.40,
    xmax = bin_position + 0.40
  )]
  support_by_crop <- CJ(crop_label = crop_order, bin_position = support$bin_position)
  support_by_crop <- merge(
    support_by_crop, support[, .(bin_position, xmin, xmax, ymin, ymax)],
    by = "bin_position", all.x = TRUE
  )
  support_by_crop[, crop_label := factor(crop_label, levels = crop_order)]

  palette <- c(
    Corn = "#0072B2", Rice = "#E69F00", Cassava = "#009E73",
    Beans = "#CC79A7", Soybeans = "#D55E00", Sugarcane = "#6F42C1"
  )
  shapes <- c(Corn = 18, Rice = 17, Cassava = 15, Beans = 16, Soybeans = 3, Sugarcane = 8)
  fig <- ggplot(coefficients, aes(bin_position, estimate, color = crop_label)) +
    geom_rect(
      data = support_by_crop,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE, fill = "grey80", color = "grey65", linewidth = 0.25
    ) +
    geom_hline(yintercept = 0, linewidth = 0.35, color = "grey45") +
    geom_line(linewidth = 0.75, na.rm = TRUE) +
    geom_linerange(
      aes(ymin = ci95_low, ymax = ci95_high),
      linewidth = 0.55, linetype = "dashed", na.rm = TRUE
    ) +
    geom_point(aes(shape = crop_label), size = 2.4, stroke = 0.65, na.rm = TRUE) +
    facet_wrap(~crop_label, ncol = 3) +
    scale_color_manual(values = palette, guide = "none") +
    scale_shape_manual(values = shapes, guide = "none") +
    scale_x_continuous(breaks = support$bin_position, labels = bin_labels) +
    coord_cartesian(ylim = c(-0.43, 0.22)) +
    labs(
      x = "Raw mean soil salinity (dS/m)",
      y = "Change in log yield relative to <0.5 dS/m"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(size = 8.2),
      axis.text.y = element_text(size = 9.5),
      axis.title = element_text(size = 11.5),
      strip.text = element_text(size = 12, face = "bold"),
      plot.margin = margin(6, 8, 6, 6)
    )
  ggsave(
    file.path(paths$out_dir, "FullRevision_RawMeanSalinity_Yield_Bins.pdf"),
    fig, width = 9.4, height = 5.8
  )
  ggsave(
    file.path(paths$out_dir, "FullRevision_RawMeanSalinity_Yield_Bins.png"),
    fig, width = 9.4, height = 5.8, dpi = 320
  )
  invisible(fig)
}

make_raw_mean_salinity_bin_table <- function() {
  write_status("Building the appendix table for the nonlinear yield figure.")
  coefficient_file <- file.path(
    paths$data_dir, "nonlinear_salinity_bins_crop_yield_coefficients_200km.csv"
  )
  if (!file.exists(coefficient_file)) {
    stop("Nonlinear salinity-bin coefficient data are missing: ", coefficient_file)
  }

  coefficients <- fread(coefficient_file)
  required <- c(
    "crop_label", "bin_label", "bin_position", "estimate",
    "standard_error", "identifying_pairs", "observations"
  )
  missing <- setdiff(required, names(coefficients))
  if (length(missing) > 0L) {
    stop("Missing nonlinear-table columns: ", paste(missing, collapse = ", "))
  }

  crop_order <- c("Corn", "Rice", "Cassava", "Beans", "Soybeans", "Sugarcane")
  bin_order <- coefficients[order(bin_position), unique(as.character(bin_label))]
  coefficients[, p_value := 2 * stats::pnorm(-abs(estimate / standard_error))]
  coefficients[bin_position == 1L, p_value := NA_real_]

  coefficient_cell <- function(crop_name, bin_name) {
    row <- coefficients[crop_label == crop_name & bin_label == bin_name]
    if (nrow(row) != 1L) return("Not available")
    if (row$bin_position == 1L) return("Reference")
    if (!is.finite(row$estimate) || !is.finite(row$standard_error)) {
      return("Not estimated")
    }
    significance <- stars(row$p_value)
    estimate_text <- if (nzchar(significance)) {
      paste0("$", fmt(row$estimate, 4), "^{", significance, "}$")
    } else {
      paste0("$", fmt(row$estimate, 4), "$")
    }
    paste0(estimate_text, " (", fmt(row$standard_error, 4), ")")
  }
  support_cell <- function(crop_name, bin_name) {
    row <- coefficients[crop_label == crop_name & bin_label == bin_name]
    if (nrow(row) != 1L || !is.finite(row$identifying_pairs)) return("")
    formatC(as.integer(row$identifying_pairs), format = "d", big.mark = ",")
  }
  bin_cell <- function(bin_name) {
    if (bin_name == "<0.5") return("$<0.5$")
    if (bin_name == ">=5.0") return("$\\geq 5.0$")
    bin_name
  }

  panel_lines <- function(panel_crops, panel_title) {
    lines <- c(
      paste0("\\multicolumn{7}{l}{\\textit{", panel_title, "}}\\\\"),
      paste0(
        "Salinity bin & ",
        paste(
          vapply(
            panel_crops,
            function(crop_name) paste0("\\multicolumn{2}{c}{", crop_name, "}"),
            character(1)
          ),
          collapse = " & "
        ),
        " \\\\"
      ),
      "(dS/m) & Estimate (SE) & Support & Estimate (SE) & Support & Estimate (SE) & Support \\\\ ",
      "\\midrule"
    )
    for (bin_name in bin_order) {
      cells <- c(bin_cell(bin_name))
      for (crop_name in panel_crops) {
        cells <- c(
          cells,
          coefficient_cell(crop_name, bin_name),
          support_cell(crop_name, bin_name)
        )
      }
      lines <- c(lines, paste(cells, collapse = " & "), "\\\\")
    }
    regression_n <- vapply(panel_crops, function(crop_name) {
      value <- unique(coefficients[crop_label == crop_name, observations])
      if (length(value) != 1L || !is.finite(value)) return("")
      formatC(as.integer(value), format = "d", big.mark = ",")
    }, character(1))
    lines <- c(
      lines,
      "\\midrule",
      paste(
        c(
          "Regression observations",
          unlist(lapply(regression_n, function(value) c(value, "")), use.names = FALSE)
        ),
        collapse = " & "
      ),
      "\\\\"
    )
    lines
  }

  table_lines <- c(
    "\\begin{table}[!htbp]",
    "\\color{red}",
    "\\centering",
    "\\caption{Detailed Nonlinear Yield Responses Across Raw Mean Salinity Bins}",
    "\\label{tab:full_revision_raw_mean_salinity_yield_bins}",
    "\\footnotesize",
    "\\setlength{\\tabcolsep}{2.5pt}",
    "\\renewcommand{\\arraystretch}{0.94}",
    "\\resizebox{\\linewidth}{!}{%",
    "\\begin{tabular}{lrlrlrl}",
    "\\toprule",
    panel_lines(c("Corn", "Rice", "Cassava"), "Panel A: Corn, rice, and cassava"),
    "\\midrule",
    panel_lines(c("Beans", "Soybeans", "Sugarcane"), "Panel B: Beans, soybeans, and sugarcane"),
    "\\bottomrule",
    "\\end{tabular}",
    "}",
    "\\par\\addvspace{0.5ex}",
    paste0(
      "\\parbox{0.96\\linewidth}{\\footnotesize\\textit{Notes:} ",
      "The table reports the coefficients underlying Figure~\\ref{fig:sal_non_lin}. ",
      "The dependent variable is the east-minus-west difference in log yield. ",
      "Raw mean salinity below 0.5 dS/m is the omitted category. ",
      "Models include year fixed effects and differences in GDD, KDD, soil moisture, slope, elevation, and clay content. ",
      "Standard errors in parentheses use the Conley spatial estimator with a 200 km cutoff. ",
      "Support reproduces the identifying count stored with each bin estimate; cells marked `Not estimated' lack sufficient identifying variation in the exported model. ",
      "*, **, and *** denote significance at the 10, 5, and 1 percent levels.}"
    ),
    "\\end{table}"
  )
  output_file <- file.path(
    paths$out_dir, "FullRevision_RawMeanSalinity_Yield_Bins_Details.tex"
  )
  writeLines(table_lines, output_file, useBytes = TRUE)
  invisible(output_file)
}

# =============================================================================
# 6. Aggregate cropland, land-use dynamics, and abandonment outcomes
# =============================================================================
make_muni_year_environment <- function(pam) {
  vars <- c(
    "mean_salinity", "gdd_large", "kdd_large", "precip_season_large",
    "sm_season_large", "elevation_large", "slope_large", "clay_mean_large"
  )
  vars <- intersect(vars, names(pam))
  pam[, weight_area := fifelse(is.finite(planted_area) & planted_area > 0, planted_area, NA_real_)]
  muni <- pam[, {
    w <- weight_area
    ans <- lapply(.SD, weighted_mean_safe, w = w)
    names(ans) <- vars
    ans$total_planted_area = sum(planted_area, na.rm = TRUE)
    ans
  }, by = .(Code, Year), .SDcols = vars]
  setorder(muni, Code, Year)
  lag_vars <- setdiff(names(muni), c("Code", "Year"))
  muni[, paste0(lag_vars, "_lag1") := shift(.SD, 1L), by = Code, .SDcols = lag_vars]
  muni[, asinh_total_planted_area := asinh(total_planted_area)]
  muni
}

make_total_planted_area_analysis <- function(pam = NULL) {
  write_status("Building total planted-area regressions and dynamics.")
  if (is.null(pam)) pam <- read_pam_zeros()
  pair_map <- read_pair_map()
  muni <- make_muni_year_environment(pam)
  vars <- c(
    "asinh_total_planted_area", "mean_salinity_lag1",
    "gdd_large_lag1", "kdd_large_lag1",
    "sm_season_large_lag1", "elevation_large", "slope_large", "clay_mean_large"
  )
  sfd <- make_sfd_from_unit_panel(muni[, c("Code", "Year", vars), with = FALSE], pair_map, id_cols = "Year", vars = vars)
  vc <- make_conley()
  weather <- c("d_gdd_large_lag1", "d_kdd_large_lag1", "d_sm_season_large_lag1")
  topo <- c("d_elevation_large", "d_slope_large", "d_clay_mean_large")
  needed <- c("d_asinh_total_planted_area", "d_mean_salinity_lag1", weather, topo, "Year", "lat", "lon", "pair_id")
  d <- complete_data(sfd, needed)
  m1 <- fixest::feols(d_asinh_total_planted_area ~ d_mean_salinity_lag1 | Year, data = d, vcov = vc, panel.id = ~pair_id + Year, notes = FALSE)
  m2 <- fixest::feols(
    as.formula(paste("d_asinh_total_planted_area ~ d_mean_salinity_lag1 +", paste(weather, collapse = " + "), "| Year")),
    data = d, vcov = vc, panel.id = ~pair_id + Year, notes = FALSE
  )
  m3 <- fixest::feols(
    as.formula(paste("d_asinh_total_planted_area ~ d_mean_salinity_lag1 +", paste(c(weather, topo), collapse = " + "), "| Year")),
    data = d, vcov = vc, panel.id = ~pair_id + Year, notes = FALSE
  )
  tab <- rbindlist(list(
    cbind(coef_row(m1, "d_mean_salinity_lag1", "Spec. 1"), spec_control_flags(1)),
    cbind(coef_row(m2, "d_mean_salinity_lag1", "Spec. 2"), spec_control_flags(2)),
    cbind(coef_row(m3, "d_mean_salinity_lag1", "Spec. 3"), spec_control_flags(3))
  ))
  fwrite(tab, file.path(paths$out_dir, "FullRevision_Total_Planted_Area_LaggedSalinity_Coefficients.csv"))

  fixest::setFixest_dict(c(
    d_mean_salinity_lag1 = "$\\Delta_s$ Mean salinity, lagged (dS/m)",
    d_gdd_large_lag1 = "$\\Delta_s$ GDD, lagged",
    d_kdd_large_lag1 = "$\\Delta_s$ KDD, lagged",
    d_sm_season_large_lag1 = "$\\Delta_s$ Soil moisture, lagged",
    d_elevation_large = "$\\Delta_s$ Elevation",
    d_slope_large = "$\\Delta_s$ Slope",
    d_clay_mean_large = "$\\Delta_s$ Clay"
  ), reset = TRUE)
  tex_file <- file.path(paths$out_dir, "FullRevision_Total_Planted_Area_LaggedSalinity.tex")
  fixest::etable(
    m1, m2, m3,
    tex = TRUE,
    file = tex_file,
    replace = TRUE,
    depvar = FALSE,
    title = "Lagged Mean Salinity and Total Municipal Planted Area",
    label = "tab:full_revision_total_planted_area",
    fitstat = ~ r2 + n,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    notes = paste(
      "Notes: The dependent variable is the spatial difference in the asinh of total planted area for the six PAM crops in each municipality.",
      "The treatment is the one-year lagged difference in mean salinity.",
      "Mean salinity is used because the outcome is not crop-specific.",
      "Spec. 1 includes year fixed effects; Spec. 2 adds lagged GDD, KDD and soil moisture; Spec. 3 additionally adds elevation, slope and clay content.",
      "All columns use the same complete-data sample; standard errors are Conley spatial with a 200 km cutoff."
    )
  )
  style_fixest_tex(tex_file, size = "\\scriptsize", tabcolsep = "3pt", arraystretch = "0.8")

  horizons <- -3:5
  for (h in horizons) {
    nm <- paste0("asinh_total_h", ifelse(h < 0, "m", "p"), abs(h))
    if (h >= 0) {
      muni[, (nm) := shift(asinh_total_planted_area, n = h, type = "lead"), by = Code]
    } else {
      muni[, (nm) := shift(asinh_total_planted_area, n = abs(h), type = "lag"), by = Code]
    }
  }
  dyn_vars <- c(vars, paste0("asinh_total_h", ifelse(horizons < 0, "m", "p"), abs(horizons)))
  dyn <- make_sfd_from_unit_panel(muni[, c("Code", "Year", dyn_vars), with = FALSE], pair_map, id_cols = "Year", vars = dyn_vars)
  dyn_rows <- list()
  for (h in horizons) {
    y <- paste0("d_asinh_total_h", ifelse(h < 0, "m", "p"), abs(h))
    need_h <- c(y, "d_mean_salinity_lag1", weather, topo, "Year", "lat", "lon", "pair_id")
    dh <- complete_data(dyn, need_h)
    if (nrow(dh) < 100) next
    f <- as.formula(paste(y, "~ d_mean_salinity_lag1 +", paste(c(weather, topo), collapse = " + "), "| Year"))
    mh <- fixest::feols(f, data = dh, vcov = vc, panel.id = ~pair_id + Year, notes = FALSE)
    ct <- as.data.table(fixest::coeftable(mh), keep.rownames = "term")
    r <- ct[term == "d_mean_salinity_lag1"]
    se_col <- intersect(c("Std. Error", "Std..Error", "Std...Error"), names(r))[1]
    p_col <- intersect(c("Pr(>|t|)", "Pr...t..", "Pr(>|z|)", "Pr...z.."), names(r))[1]
    dyn_rows[[length(dyn_rows) + 1L]] <- data.table(
      horizon = h,
      estimate = r$Estimate,
      se = r[[se_col]],
      p = r[[p_col]],
      observations = stats::nobs(mh)
    )
  }
  dyn_out <- rbindlist(dyn_rows)
  dyn_out[, `:=`(
    ci95_low = estimate - 1.96 * se,
    ci95_high = estimate + 1.96 * se,
    ci90_low = estimate - 1.645 * se,
    ci90_high = estimate + 1.645 * se
  )]
  fwrite(dyn_out, file.path(paths$out_dir, "FullRevision_Total_Planted_Area_Dynamic.csv"))
  fig <- ggplot(dyn_out, aes(horizon, estimate)) +
    geom_hline(yintercept = 0, linewidth = 0.35, color = "grey45") +
    geom_vline(xintercept = -0.5, linewidth = 0.3, linetype = "dashed", color = "grey60") +
    geom_linerange(aes(ymin = ci95_low, ymax = ci95_high), color = "#2F6F8F", linewidth = 0.55) +
    geom_linerange(aes(ymin = ci90_low, ymax = ci90_high), color = "#2F6F8F", linewidth = 1.15) +
    geom_point(color = "#B55D2A", size = 2.2) +
    scale_x_continuous(breaks = horizons) +
    labs(x = "Horizon around lagged salinity", y = "Effect on asinh total planted area") +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 13),
      axis.title = element_text(size = 14)
    )
  ggsave(file.path(paths$out_dir, "FullRevision_Total_Planted_Area_Dynamic.pdf"), fig, width = 6.8, height = 4.0)
  ggsave(file.path(paths$out_dir, "FullRevision_Total_Planted_Area_Dynamic.png"), fig, width = 6.8, height = 4.0, dpi = 320)

  dyn_tex <- dyn_out[, .(
    Horizon = horizon,
    Coefficient = paste0(fmt(estimate, 4), stars(p)),
    SE = paste0("(", fmt(se, 4), ")"),
    P = fmt(p, 3),
    Observations = formatC(observations, format = "d", big.mark = ",")
  )]
  write_latex_df(
    dyn_tex,
    file.path(paths$out_dir, "FullRevision_Total_Planted_Area_Dynamic.tex"),
    "Dynamic Effects on Total Planted Area",
    "tab:full_revision_total_planted_area_dynamic",
    note = paste(
      "Each row estimates separately the relationship between one-year lagged mean salinity and the spatial difference in asinh total planted area at the indicated horizon.",
      "Negative horizons are pre-trend diagnostics.",
      "All models include lagged GDD, lagged KDD, lagged soil moisture, soil and topographic controls and year fixed effects; standard errors are Conley spatial with a 200 km cutoff."
    ),
    size = "\\small"
  )

  invisible(list(table = tab, dynamic = dyn_out))
}

finite_mean <- function(x) {
  x <- x[is.finite(x)]
  if (length(x)) mean(x) else NA_real_
}

finite_first <- function(x) {
  x <- x[is.finite(x)]
  if (length(x)) x[1L] else NA_real_
}

make_common_land_environment <- function(pam = NULL) {
  if (is.null(pam)) pam <- read_pam_zeros()
  pam <- copy(pam)
  needed <- c(
    "Code", "Year", "mean_salinity", "gdd_large", "kdd_large",
    "sm_season_large", "elevation_large", "slope_large", "clay_mean_large"
  )
  missing <- setdiff(needed, names(pam))
  if (length(missing)) stop("Missing land-use environment variables: ", paste(missing, collapse = ", "))

  # Climate controls are equally averaged across the six crop calendars. Using
  # planted-area weights here would make the controls depend on the land outcome.
  env <- pam[, .(
    mean_salinity = finite_first(mean_salinity),
    salinity_values = uniqueN(round(mean_salinity[is.finite(mean_salinity)], 10L)),
    gdd_common = finite_mean(gdd_large),
    kdd_common = finite_mean(kdd_large),
    soil_moisture_common = finite_mean(sm_season_large),
    elevation = finite_mean(elevation_large),
    slope = finite_mean(slope_large),
    clay = finite_mean(clay_mean_large)
  ), by = .(Code, Year)]
  if (any(env$salinity_values > 1L, na.rm = TRUE)) {
    stop("Mean salinity is not unique within municipality-year cells.")
  }
  env[, salinity_values := NULL]
  setorder(env, Code, Year)
  for (lag_n in 1:2) {
    lag_name <- paste0("mean_salinity_lag", lag_n)
    year_name <- paste0("year_lag", lag_n)
    env[, (lag_name) := shift(mean_salinity, lag_n), by = Code]
    env[, (year_name) := shift(Year, lag_n), by = Code]
    env[get(year_name) != Year - lag_n, (lag_name) := NA_real_]
    env[, (year_name) := NULL]
  }
  env
}

make_mapbiomas_cropland_panel <- function(pam = NULL) {
  write_status("Building the all-crop MapBiomas municipal panel.")
  if (!file.exists(paths$total_agri) || !file.exists(paths$muni_ref) || !file.exists(paths$abandonment)) {
    stop("MapBiomas cropland, municipality reference, or municipal-area data are missing.")
  }

  ref <- fread(paths$muni_ref, encoding = "UTF-8", colClasses = list(character = "Code"))
  ref[, join_key := paste(state, clean_name(muni_name), sep = "__")]
  ref <- ref[, .SD[1L], by = join_key][, .(join_key, Code)]

  land <- as.data.table(readRDS(paths$total_agri))
  source_rows <- nrow(land)
  source_municipalities <- uniqueN(paste(land$state_acronym, land$Municipality, sep = "__"))
  land[, join_key := paste(state_acronym, clean_name(Municipality), sep = "__")]
  unmatched <- unique(land[!ref, on = "join_key", .(state_acronym, Municipality, join_key)])
  fwrite(unmatched, file.path(paths$out_dir, "FullRevision_AggregateCropland_Unmatched_Municipalities.csv"))
  land <- merge(land, ref, by = "join_key", all.x = FALSE, all.y = FALSE, sort = FALSE)
  land[, Code := trimws(as.character(Code))]

  gross <- as.data.table(readRDS(paths$abandonment))
  gross[, Code := trimws(as.character(Code))]
  muni_area <- gross[is.finite(municipality_total_hectares) & municipality_total_hectares > 0,
    .(municipality_total_hectares = stats::median(municipality_total_hectares)), by = Code]
  land <- merge(land, muni_area, by = "Code", all.x = TRUE, sort = FALSE)
  land <- land[Year >= 1985L & Year <= 2018L]
  land[, cropland_share_pct := fifelse(
    is.finite(municipality_total_hectares) & municipality_total_hectares > 0,
    100 * total_agri_hectares / municipality_total_hectares,
    NA_real_
  )]
  if (any(land$cropland_share_pct < -1e-8 | land$cropland_share_pct > 100 + 1e-6, na.rm = TRUE)) {
    stop("MapBiomas cropland shares fall outside [0, 100].")
  }
  land[, asinh_cropland_hectares := asinh(total_agri_hectares)]

  env <- make_common_land_environment(pam)
  panel <- merge(land, env, by = c("Code", "Year"), all.x = TRUE, sort = FALSE)
  setorder(panel, Code, Year)
  panel[, next_year := shift(Year, type = "lead"), by = Code]
  panel[, `:=`(
    cropland_share_next = shift(cropland_share_pct, type = "lead"),
    cropland_asinh_next = shift(asinh_cropland_hectares, type = "lead")
  ), by = Code]
  panel[next_year != Year + 1L, c("cropland_share_next", "cropland_asinh_next") := .(NA_real_, NA_real_)]
  panel[, `:=`(
    annual_cropland_change_pp = cropland_share_next - cropland_share_pct,
    annual_cropland_change_asinh = cropland_asinh_next - asinh_cropland_hectares
  )]

  diagnostics <- data.table(
    metric = c(
      "MapBiomas source municipality-years", "MapBiomas source municipalities",
      "Matched municipalities", "Unmatched municipality names", "Analysis municipality-years",
      "Analysis municipalities", "Zero-cropland municipality-years", "Missing municipal-area rows",
      "Missing salinity rows", "Annual transitions before SFD", "Minimum baseline year",
      "Maximum baseline year"
    ),
    value = c(
      source_rows,
      source_municipalities,
      uniqueN(panel$Code), nrow(unmatched), nrow(panel), uniqueN(panel$Code),
      sum(panel$total_agri_hectares == 0, na.rm = TRUE),
      sum(!is.finite(panel$municipality_total_hectares)),
      sum(!is.finite(panel$mean_salinity)),
      sum(is.finite(panel$annual_cropland_change_pp)),
      min(panel$Year[is.finite(panel$annual_cropland_change_pp)]),
      max(panel$Year[is.finite(panel$annual_cropland_change_pp)])
    )
  )
  fwrite(diagnostics, file.path(paths$out_dir, "FullRevision_AggregateCropland_Data_Diagnostics.csv"))
  invisible(list(panel = panel, environment = env, diagnostics = diagnostics, unmatched = unmatched))
}

fit_land_change_models <- function(
  d,
  outcome,
  treatment = "d_mean_salinity",
  weather = c("d_gdd_common", "d_kdd_common", "d_soil_moisture_common"),
  topo = c("d_elevation", "d_slope", "d_clay"),
  include_pair_fe = TRUE,
  vcov_spec = ~pair_id
) {
  formulas <- list(
    as.formula(paste(outcome, "~", treatment, "| Year")),
    as.formula(paste(outcome, "~", paste(c(treatment, weather), collapse = " + "), "| Year")),
    as.formula(paste(outcome, "~", paste(c(treatment, weather, topo), collapse = " + "), "| Year"))
  )
  if (include_pair_fe) {
    formulas <- c(
      formulas,
      list(as.formula(paste(
        outcome, "~", paste(c(treatment, weather), collapse = " + "), "| pair_id + Year"
      )))
    )
  }
  lapply(formulas, function(f) {
    fixest::feols(f, data = d, vcov = vcov_spec, panel.id = ~pair_id + Year, notes = FALSE)
  })
}

land_conley_vcov <- function(cutoff = 200, vcov_fix = FALSE) {
  fixest::vcov_conley(
    lat = "lat", lon = "lon", cutoff = cutoff,
    distance = "spherical", vcov_fix = vcov_fix
  )
}

strict_conley_term <- function(model, term, cutoff) {
  conley_warnings <- character()
  conley_vcov <- try(
    withCallingHandlers(
      stats::vcov(
        model,
        vcov = fixest::vcov_conley(
          lat = "lat", lon = "lon", cutoff = cutoff,
          distance = "spherical", vcov_fix = FALSE
        )
      ),
      warning = function(w) {
        conley_warnings <<- c(conley_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    silent = TRUE
  )
  minimum_eigenvalue <- if (
    inherits(conley_vcov, "try-error") || !is.matrix(conley_vcov) || any(!is.finite(conley_vcov))
  ) {
    NA_real_
  } else {
    min(eigen((conley_vcov + t(conley_vcov)) / 2, symmetric = TRUE, only.values = TRUE)$values)
  }
  covariance_was_fixed <- any(grepl("positive semi-definite|positive-semidefinite|VCOV.*fixed", conley_warnings, ignore.case = TRUE))
  valid <- !covariance_was_fixed && is.finite(minimum_eigenvalue) && minimum_eigenvalue >= -1e-10
  sm <- if (valid) try(summary(model, vcov = conley_vcov), silent = TRUE) else structure("Invalid Conley covariance", class = "try-error")
  row <- if (inherits(sm, "try-error")) {
    data.table(estimate = unname(coef(model)[term]), se = NA_real_, p = NA_real_)
  } else {
    coef_numeric_row(sm, term, paste0("Conley ", cutoff))[, .(estimate, se, p)]
  }
  row[, `:=`(
    cutoff_km = cutoff,
    valid = valid && is.finite(se),
    minimum_vcov_eigenvalue = minimum_eigenvalue,
    vcov_warning = if (length(conley_warnings)) paste(unique(conley_warnings), collapse = " | ") else NA_character_
  )]
  row
}

write_land_change_figure <- function(rows, filename, x_label) {
  rows <- copy(rows)
  rows[, specification := factor(
    specification,
    levels = rev(c("Year FE", "Weather", "Weather + soil/topography", "Pair and year FE"))
  )]
  palette <- c(
    "Year FE" = "#3A7D44", "Weather" = "#D47A1F",
    "Weather + soil/topography" = "#6A51A3", "Pair and year FE" = "#2F6F8F"
  )
  fig <- ggplot(rows, aes(estimate, specification, color = specification)) +
    geom_vline(xintercept = 0, linewidth = 0.35, color = "grey45") +
    geom_linerange(aes(xmin = estimate - 1.96 * se, xmax = estimate + 1.96 * se), linewidth = 0.55) +
    geom_linerange(aes(xmin = estimate - 1.645 * se, xmax = estimate + 1.645 * se), linewidth = 1.2) +
    geom_point(size = 2.5) +
    scale_color_manual(values = palette, guide = "none") +
    labs(x = x_label, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 11), axis.title = element_text(size = 12))
  ggsave(file.path(paths$out_dir, paste0(filename, ".pdf")), fig, width = 7.2, height = 3.7)
  ggsave(file.path(paths$out_dir, paste0(filename, ".png")), fig, width = 7.2, height = 3.7, dpi = 320)
  fig
}

make_aggregate_cropland_analysis <- function(pam = NULL, prepared = NULL) {
  write_status("Estimating salinity effects on all-crop physical cropland.")
  if (is.null(prepared)) prepared <- make_mapbiomas_cropland_panel(pam)
  panel <- prepared$panel
  pair_map <- read_pair_map()
  vars <- c(
    "annual_cropland_change_pp",
    "cropland_share_pct", "total_agri_hectares",
    "mean_salinity", "mean_salinity_lag1", "mean_salinity_lag2",
    "gdd_common", "kdd_common", "soil_moisture_common", "elevation", "slope", "clay"
  )
  sfd <- make_sfd_from_unit_panel(
    panel[, c("Code", "Year", vars), with = FALSE], pair_map,
    id_cols = "Year", vars = vars
  )
  weather <- c("d_gdd_common", "d_kdd_common", "d_soil_moisture_common")
  topo <- c("d_elevation", "d_slope", "d_clay")
  main_needed <- c(
    "d_annual_cropland_change_pp", "d_mean_salinity",
    weather, topo, "Year", "lat", "lon", "pair_id"
  )
  d <- complete_data(sfd, main_needed)
  side_cropland_shares <- c(d$cropland_share_pct, d$cropland_share_pct_west)
  side_cropland_hectares <- c(d$total_agri_hectares, d$total_agri_hectares_west)
  sample_diagnostics <- data.table(
    metric = c(
      "Common-sample SFD observations", "Spatial pairs", "East-side municipalities",
      "West-side municipalities", "Minimum baseline year", "Maximum baseline year",
      "Mean baseline cropland share (percent)", "SD baseline cropland share (percent)",
      "Zero-cropland municipality sides", "SD east-minus-west mean salinity (dS/m)",
      "Mean absolute east-minus-west mean salinity (dS/m)"
    ),
    value = c(
      nrow(d), uniqueN(d$pair_id), uniqueN(d$Code), uniqueN(d$code_neighbor_west),
      min(d$Year), max(d$Year), mean(side_cropland_shares), sd(side_cropland_shares),
      sum(side_cropland_hectares == 0), sd(d$d_mean_salinity), mean(abs(d$d_mean_salinity))
    )
  )
  fwrite(sample_diagnostics, file.path(paths$out_dir, "FullRevision_AggregateCropland_Sample_Diagnostics.csv"))
  models <- fit_land_change_models(
    d, "d_annual_cropland_change_pp", include_pair_fe = FALSE,
    vcov_spec = land_conley_vcov(200)
  )
  spec_names <- c("Year FE", "Weather", "Weather + soil/topography")
  coef_rows <- rbindlist(lapply(seq_along(models), function(i) {
    cbind(coef_numeric_row(models[[i]], "d_mean_salinity", spec_names[i]), specification = spec_names[i])
  }), fill = TRUE)
  fwrite(coef_rows, file.path(paths$out_dir, "FullRevision_AggregateCropland_AnnualChange_Coefficients.csv"))
  fixest::setFixest_dict(c(
    d_mean_salinity = "$\\Delta_s$ Mean salinity at baseline (dS/m)",
    d_mean_salinity_lag1 = "$\\Delta_s$ Mean salinity at $t-1$ (dS/m)",
    d_mean_salinity_lag2 = "$\\Delta_s$ Mean salinity at $t-2$ (dS/m)",
    d_gdd_common = "$\\Delta_s$ GDD at baseline",
    d_kdd_common = "$\\Delta_s$ KDD at baseline",
    d_soil_moisture_common = "$\\Delta_s$ Soil moisture at baseline",
    d_elevation = "$\\Delta_s$ Elevation",
    d_slope = "$\\Delta_s$ Slope",
    d_clay = "$\\Delta_s$ Clay"
  ), reset = TRUE)
  tex_file <- file.path(paths$out_dir, "FullRevision_AggregateCropland_AnnualChange.tex")
  fixest::etable(
    models[[1]], models[[2]], models[[3]],
    tex = TRUE, file = tex_file, replace = TRUE, depvar = FALSE,
    headers = list("Specification" = c("Year FE", "Weather", "Weather + soil/topography")),
    title = "Mean Salinity and the Annual Change in Total Physical Cropland",
    label = "tab:full_revision_aggregate_cropland_change",
    fitstat = ~ r2 + n,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    notes = paste(
      "Notes: An observation is a strictly contiguous east-west municipality pair and baseline year.",
      "The dependent variable is the east-minus-west difference in the annual percentage-point change in the share of municipal territory classified by MapBiomas as Agriculture (all crops combined, excluding pasture).",
      "Baseline years run from 1985 to 2017 and changes are measured from t to t+1. Zero cropland areas are retained.",
      "Mean salinity uses the same static historically cultivated municipal pixels as the other common-salinity specifications.",
      "Spec. 1 includes year fixed effects; Spec. 2 adds equally weighted crop-calendar GDD, KDD and soil moisture; Spec. 3 adds elevation, slope and clay.",
      "Spec. 2 is preferred because the SFD already absorbs much of the slowly varying soil and topographic heterogeneity shared by contiguous municipalities.",
      "All columns use the same complete-data sample. Standard errors are Conley spatial with a 200 km cutoff."
    )
  )
  style_fixest_tex(tex_file, size = "\\scriptsize", tabcolsep = "3pt", arraystretch = "0.78", resize = TRUE)
  preferred <- models[[2]]
  conley_cutoffs <- c(50, 100, 200, 250, 300)
  conley <- rbindlist(
    lapply(conley_cutoffs, function(km) strict_conley_term(preferred, "d_mean_salinity", km)),
    fill = TRUE
  )
  conley[, inference := paste0(conley_cutoffs, " km")]
  fwrite(conley, file.path(paths$out_dir, "FullRevision_AggregateCropland_Conley_Robustness.csv"))
  conley_tex <- conley[, .(
    Inference = inference,
    Coefficient = ifelse(is.finite(se), paste0(fmt(estimate, 4), stars(p)), "NA"),
    `Std. error` = ifelse(is.finite(se), paste0("(", fmt(se, 4), ")"), "NA"),
    `P-value` = ifelse(is.finite(p), fmt(p, 3), "NA")
  )]
  write_latex_df(
    conley_tex,
    file.path(paths$out_dir, "FullRevision_AggregateCropland_Conley_Robustness.tex"),
    "Spatial-Inference Robustness for Annual Cropland Change",
    "tab:full_revision_aggregate_cropland_conley",
    note = paste(
      "All columns use the Spec. 2 point estimate and sample; only the variance estimator changes.",
      "The main cutoff is 200 km. Strict Conley estimates use spherical distance and no positive-semidefinite covariance correction. NA denotes an invalid strict covariance estimate."
    ),
    size = "\\small"
  )

  dynamic <- make_aggregate_cropland_joint_distributed_lag(panel, pair_map)
  invisible(list(
    prepared = prepared, data = d, models = models,
    coefficients = coef_rows,
    sample_diagnostics = sample_diagnostics,
    conley = conley,
    dynamic = dynamic, figure = NULL
  ))
}

make_aggregate_cropland_three_period_long_differences <- function(
  panel, pair_map = read_pair_map()
) {
  write_status("Estimating the three-period aggregate-cropland long-differences panel without pair fixed effects.")
  output_stub <- "FullRevision_AggregateCropland_ThreePeriodLongDifferences_NoPairFE"
  window_labels <- c("1985-1989", "2000-2004", "2014-2018")
  endpoint <- copy(panel[
    Year %between% c(1985L, 1989L) |
      Year %between% c(2000L, 2004L) |
      Year %between% c(2014L, 2018L)
  ])
  endpoint[, window := fcase(
    Year %between% c(1985L, 1989L), window_labels[1],
    Year %between% c(2000L, 2004L), window_labels[2],
    Year %between% c(2014L, 2018L), window_labels[3]
  )]
  endpoint[, window_order := match(window, window_labels)]

  complete_window_mean <- function(x, year) {
    if (uniqueN(year) != 5L || length(x) != 5L || any(!is.finite(x))) return(NA_real_)
    mean(x)
  }
  window_means <- endpoint[, .(
    cropland_share_pct = complete_window_mean(cropland_share_pct, Year),
    mean_salinity = complete_window_mean(mean_salinity, Year),
    gdd_common = complete_window_mean(gdd_common, Year),
    kdd_common = complete_window_mean(kdd_common, Year),
    soil_moisture_common = complete_window_mean(soil_moisture_common, Year),
    elevation = complete_window_mean(elevation, Year),
    slope = complete_window_mean(slope, Year),
    clay = complete_window_mean(clay, Year)
  ), by = .(Code, window, window_order)]

  vars <- c(
    "cropland_share_pct", "mean_salinity", "gdd_common", "kdd_common",
    "soil_moisture_common", "elevation", "slope", "clay"
  )
  sfd <- make_sfd_from_unit_panel(
    window_means[, c("Code", "window", "window_order", vars), with = FALSE],
    pair_map, id_cols = c("window", "window_order"), vars = vars
  )
  required <- c(paste0("d_", vars), "pair_id", "lat", "lon", "window", "window_order")
  d <- complete_data(sfd, required)
  balanced_pairs <- d[, .(rows = .N, windows = uniqueN(window)), by = pair_id][
    rows == 3L & windows == 3L, pair_id
  ]
  d <- d[pair_id %in% balanced_pairs]
  d[, window := factor(window, levels = window_labels)]
  setorder(d, pair_id, window_order)
  if (nrow(d) == 0L) stop("The balanced three-period long-differences sample is empty.")

  level_data <- copy(d)
  window_diagnostics <- level_data[, .(
    observations = .N,
    pairs = uniqueN(pair_id),
    municipalities = uniqueN(c(Code, code_neighbor_west)),
    mean_sfd_cropland_share_pp = mean(d_cropland_share_pct),
    sd_sfd_cropland_share_pp = sd(d_cropland_share_pct),
    mean_sfd_salinity_ds_m = mean(d_mean_salinity),
    sd_sfd_salinity_ds_m = sd(d_mean_salinity),
    zero_cropland_municipality_sides = sum(
      c(cropland_share_pct, cropland_share_pct_west) == 0
    )
  ), by = .(window, window_order)]
  transition_labels <- c(
    "1985-1989 to 2000-2004", "2000-2004 to 2014-2018"
  )
  d <- level_data[, .(
    transition = transition_labels,
    transition_order = 1:2,
    d_ld_cropland_share_pp = diff(d_cropland_share_pct),
    d_ld_mean_salinity = diff(d_mean_salinity),
    d_ld_gdd = diff(d_gdd_common),
    d_ld_kdd = diff(d_kdd_common),
    d_ld_soil_moisture = diff(d_soil_moisture_common),
    baseline_d_elevation = first(d_elevation),
    baseline_d_slope = first(d_slope),
    baseline_d_clay = first(d_clay),
    lat = first(lat),
    lon = first(lon)
  ), by = .(pair_id, Code, code_neighbor_west)]
  d[, transition := factor(transition, levels = transition_labels)]

  formulas <- list(
    "Transition FE" = d_ld_cropland_share_pp ~ d_ld_mean_salinity | transition,
    "Weather" = d_ld_cropland_share_pp ~ d_ld_mean_salinity + d_ld_gdd +
      d_ld_kdd + d_ld_soil_moisture | transition,
    "Weather + baseline physical controls" = d_ld_cropland_share_pp ~
      d_ld_mean_salinity + d_ld_gdd + d_ld_kdd + d_ld_soil_moisture +
      baseline_d_elevation + baseline_d_slope + baseline_d_clay | transition
  )
  models <- lapply(formulas, function(fml) {
    fixest::feols(
      fml, data = d, vcov = land_conley_vcov(200),
      panel.id = ~pair_id + transition_order, notes = FALSE
    )
  })

  d[, treatment_transition_mean := mean(d_ld_mean_salinity), by = transition]
  d[, treatment_identifying := d_ld_mean_salinity - treatment_transition_mean]
  treatment_sd <- sd(d$treatment_identifying)
  coefficient_rows <- rbindlist(lapply(seq_along(models), function(i) {
    row <- coef_numeric_row(models[[i]], "d_ld_mean_salinity", names(models)[i])
    row[, `:=`(
      specification = names(models)[i],
      treatment_sd_after_transition_fixed_effects = treatment_sd,
      standardized_effect_pp = estimate * treatment_sd
    )]
    row
  }), fill = TRUE)
  fwrite(
    coefficient_rows,
    file.path(
      paths$out_dir,
      paste0(output_stub, "_Coefficients.csv")
    )
  )

  strict_conley <- rbindlist(lapply(c(50, 100, 200, 250, 300), function(km) {
    row <- strict_conley_term(models[[2]], "d_ld_mean_salinity", km)
    row[, cutoff_km := km]
    row
  }), fill = TRUE)
  fwrite(
    strict_conley,
    file.path(
      paths$out_dir,
      paste0(output_stub, "_Conley.csv")
    )
  )

  pairwise_models <- lapply(transition_labels, function(transition_value) {
    pair_data <- d[transition == transition_value]
    fixest::feols(
      d_ld_cropland_share_pp ~ d_ld_mean_salinity + d_ld_gdd +
        d_ld_kdd + d_ld_soil_moisture,
      data = pair_data, vcov = land_conley_vcov(200), notes = FALSE
    )
  })
  names(pairwise_models) <- transition_labels
  pairwise_rows <- rbindlist(lapply(seq_along(pairwise_models), function(i) {
    row <- coef_numeric_row(
      pairwise_models[[i]], "d_ld_mean_salinity", names(pairwise_models)[i]
    )
    row[, comparison := names(pairwise_models)[i]]
    row
  }), fill = TRUE)
  fwrite(
    pairwise_rows,
    file.path(
      paths$out_dir,
      paste0(output_stub, "_Pairwise.csv")
    )
  )

  diagnostics <- rbind(
    data.table(
      metric = c(
        "Long-difference observations", "Strictly contiguous pairs",
        "Long-difference transitions", "Five-year windows",
        "Municipalities represented on either side",
        "SD of salinity long difference after transition FE (dS/m)"
      ),
      value = c(
        nrow(d), uniqueN(d$pair_id), uniqueN(d$transition),
        length(window_labels), uniqueN(c(d$Code, d$code_neighbor_west)), treatment_sd
      )
    ),
    data.table(
      metric = paste0("Observations in ", window_diagnostics$window),
      value = window_diagnostics$observations
    )
  )
  fwrite(
    diagnostics,
    file.path(
      paths$out_dir,
      paste0(output_stub, "_Diagnostics.csv")
    )
  )
  fwrite(
    window_diagnostics,
    file.path(
      paths$out_dir,
      paste0(output_stub, "_ByWindow.csv")
    )
  )

  fixest::setFixest_dict(c(
    transition = "Transition",
    d_ld_mean_salinity = "$\\Delta_L\\Delta_s$ Mean salinity (dS/m)",
    d_ld_gdd = "$\\Delta_L\\Delta_s$ GDD",
    d_ld_kdd = "$\\Delta_L\\Delta_s$ KDD",
    d_ld_soil_moisture = "$\\Delta_L\\Delta_s$ Soil moisture",
    baseline_d_elevation = "$\\Delta_s$ Baseline elevation",
    baseline_d_slope = "$\\Delta_s$ Baseline slope",
    baseline_d_clay = "$\\Delta_s$ Baseline clay"
  ), reset = TRUE)
  tex_file <- file.path(
    paths$out_dir, paste0(output_stub, ".tex")
  )
  fixest::etable(
    models[[1]], models[[2]], models[[3]],
    tex = TRUE, file = tex_file, replace = TRUE, depvar = FALSE,
    headers = list("Specification" = names(models)),
    title = "Repeated Long Differences across Three Five-Year Windows",
    label = "tab:full_revision_aggregate_cropland_three_period_long_differences_no_pair_fe",
    fitstat = ~ r2 + wr2 + n,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    notes = paste(
      "Notes: An observation is one strictly contiguous east-west municipality pair and one long-difference transition: 1985--1989 to 2000--2004 or 2000--2004 to 2014--2018.",
      "The dependent variable is the change between five-year windows in the east-minus-west difference in mean municipal cropland share, in percentage points; all crop classes are combined and pasture is excluded.",
      "The treatment is the analogous long change in the common municipal mean-salinity measure.",
      "All columns include transition fixed effects, omit pair fixed effects and use the same balanced sample of pairs observed in all three windows.",
      "Spec. 2 adds long changes in crop-calendar GDD, KDD and soil moisture and is preferred.",
      "Spec. 3 adds east-minus-west differences in baseline elevation, slope and clay as controls for differential long-run trends.",
      "Zero cropland areas are retained. Standard errors are Conley spatial with a 200 km cutoff. All estimated controls are displayed."
    )
  )
  style_fixest_tex(
    tex_file, size = "\\scriptsize", tabcolsep = "2.5pt",
    arraystretch = "0.78", resize = TRUE
  )

  invisible(list(
    data = d, models = models, coefficients = coefficient_rows,
    conley = strict_conley, pairwise = pairwise_rows,
    diagnostics = diagnostics, by_window = window_diagnostics
  ))
}

make_aggregate_cropland_true_long_difference <- function(
  panel, pair_map = read_pair_map()
) {
  write_status("Estimating the 1985-1989 to 2014-2018 aggregate-cropland long difference.")
  endpoint_years <- c(1985:1989, 2014:2018)
  endpoint <- copy(panel[Year %in% endpoint_years])
  endpoint[, window := fifelse(Year <= 1989L, "1985-1989", "2014-2018")]

  complete_window_mean <- function(x, year) {
    if (uniqueN(year) != 5L || length(x) != 5L || any(!is.finite(x))) return(NA_real_)
    mean(x)
  }
  window_means <- endpoint[, .(
    cropland_share_pct = complete_window_mean(cropland_share_pct, Year),
    mean_salinity = complete_window_mean(mean_salinity, Year),
    gdd_common = complete_window_mean(gdd_common, Year),
    kdd_common = complete_window_mean(kdd_common, Year),
    soil_moisture_common = complete_window_mean(soil_moisture_common, Year),
    elevation = complete_window_mean(elevation, Year),
    slope = complete_window_mean(slope, Year),
    clay = complete_window_mean(clay, Year)
  ), by = .(Code, window)]

  early <- copy(window_means[window == "1985-1989"])
  late <- copy(window_means[window == "2014-2018"])
  early[, window := NULL]
  late[, window := NULL]
  measure_vars <- setdiff(names(early), "Code")
  setnames(early, measure_vars, paste0(measure_vars, "_early"))
  setnames(late, measure_vars, paste0(measure_vars, "_late"))
  municipal_ld <- merge(early, late, by = "Code", all = FALSE)
  municipal_ld[, `:=`(
    ld_cropland_share_pp = cropland_share_pct_late - cropland_share_pct_early,
    ld_mean_salinity = mean_salinity_late - mean_salinity_early,
    ld_gdd = gdd_common_late - gdd_common_early,
    ld_kdd = kdd_common_late - kdd_common_early,
    ld_soil_moisture = soil_moisture_common_late - soil_moisture_common_early,
    elevation = elevation_early,
    slope = slope_early,
    clay = clay_early,
    period_id = "1985-1989 to 2014-2018"
  )]

  vars <- c(
    "ld_cropland_share_pp", "ld_mean_salinity", "ld_gdd", "ld_kdd",
    "ld_soil_moisture", "elevation", "slope", "clay",
    "cropland_share_pct_early", "cropland_share_pct_late"
  )
  sfd <- make_sfd_from_unit_panel(
    municipal_ld[, c("Code", "period_id", vars), with = FALSE],
    pair_map, id_cols = "period_id", vars = vars
  )
  required <- c(paste0("d_", vars), "pair_id", "lat", "lon")
  d <- complete_data(sfd, required)
  if (nrow(d) == 0L) stop("The true long-difference sample is empty.")

  formulas <- list(
    "No controls" = d_ld_cropland_share_pp ~ d_ld_mean_salinity,
    "Long-change weather" = d_ld_cropland_share_pp ~ d_ld_mean_salinity +
      d_ld_gdd + d_ld_kdd + d_ld_soil_moisture,
    "Weather + physical controls" = d_ld_cropland_share_pp ~ d_ld_mean_salinity +
      d_ld_gdd + d_ld_kdd + d_ld_soil_moisture + d_elevation + d_slope + d_clay
  )
  models <- lapply(formulas, function(fml) {
    fixest::feols(fml, data = d, vcov = land_conley_vcov(200), notes = FALSE)
  })

  treatment_sd <- sd(d$d_ld_mean_salinity)
  coefficient_rows <- rbindlist(lapply(seq_along(models), function(i) {
    row <- coef_numeric_row(models[[i]], "d_ld_mean_salinity", names(models)[i])
    row[, `:=`(
      specification = names(models)[i],
      treatment_sd = treatment_sd,
      standardized_effect_pp = estimate * treatment_sd
    )]
    row
  }), fill = TRUE)
  fwrite(
    coefficient_rows,
    file.path(paths$out_dir, "FullRevision_AggregateCropland_TrueLongDifference_Coefficients.csv")
  )

  strict_conley <- rbindlist(lapply(c(50, 100, 200, 250, 300), function(km) {
    row <- strict_conley_term(models[[2]], "d_ld_mean_salinity", km)
    row[, cutoff_km := km]
    row
  }), fill = TRUE)
  fwrite(
    strict_conley,
    file.path(paths$out_dir, "FullRevision_AggregateCropland_TrueLongDifference_Conley.csv")
  )

  diagnostics <- data.table(
    metric = c(
      "Early window", "Late window", "Complete SFD observations", "Strictly contiguous pairs",
      "Municipalities represented on either side", "Mean dependent long difference (percentage points)",
      "SD dependent long difference (percentage points)", "Mean SFD salinity long difference (dS/m)",
      "SD SFD salinity long difference (dS/m)", "Zero early-window cropland municipality sides",
      "Zero late-window cropland municipality sides"
    ),
    value = c(
      "1985-1989", "2014-2018", nrow(d), uniqueN(d$pair_id),
      uniqueN(c(d$Code, d$code_neighbor_west)), mean(d$d_ld_cropland_share_pp),
      sd(d$d_ld_cropland_share_pp), mean(d$d_ld_mean_salinity), treatment_sd,
      sum(c(d$cropland_share_pct_early, d$cropland_share_pct_early_west) == 0),
      sum(c(d$cropland_share_pct_late, d$cropland_share_pct_late_west) == 0)
    )
  )
  fwrite(
    diagnostics,
    file.path(paths$out_dir, "FullRevision_AggregateCropland_TrueLongDifference_Diagnostics.csv")
  )

  fixest::setFixest_dict(c(
    d_ld_mean_salinity = "$\\Delta_s\\Delta_L$ Mean salinity (dS/m)",
    d_ld_gdd = "$\\Delta_s\\Delta_L$ GDD",
    d_ld_kdd = "$\\Delta_s\\Delta_L$ KDD",
    d_ld_soil_moisture = "$\\Delta_s\\Delta_L$ Soil moisture",
    d_elevation = "$\\Delta_s$ Baseline elevation",
    d_slope = "$\\Delta_s$ Baseline slope",
    d_clay = "$\\Delta_s$ Baseline clay"
  ), reset = TRUE)
  tex_file <- file.path(
    paths$out_dir, "FullRevision_AggregateCropland_TrueLongDifference.tex"
  )
  fixest::etable(
    models[[1]], models[[2]], models[[3]],
    tex = TRUE, file = tex_file, replace = TRUE, depvar = FALSE,
    headers = list("Specification" = names(models)),
    title = "Long Differences in Mean Salinity and Total Physical Cropland, 1985--1989 to 2014--2018",
    label = "tab:full_revision_aggregate_cropland_true_long_difference",
    fitstat = ~ r2 + n,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    notes = paste(
      "Notes: An observation is one strictly contiguous east-west municipality pair.",
      "The dependent variable is the east-minus-west difference in the change in mean cropland share from 1985--1989 to 2014--2018, in percentage points.",
      "The treatment is the analogous spatial difference in the long change in the common municipal mean-salinity measure.",
      "Spec. 1 includes no controls; Spec. 2 adds long changes in equally weighted crop-calendar GDD, KDD and soil moisture; Spec. 3 adds east-minus-west differences in baseline elevation, slope and clay.",
      "All columns use the same complete-data sample and retain zero cropland areas.",
      "There are no fixed effects because the long difference leaves one cross-sectional observation per pair.",
      "Standard errors are Conley spatial with a 200 km cutoff. All controls are displayed."
    )
  )
  style_fixest_tex(tex_file, size = "\\scriptsize", tabcolsep = "3pt", arraystretch = "0.80", resize = TRUE)

  invisible(list(
    data = d, models = models, coefficients = coefficient_rows,
    conley = strict_conley, diagnostics = diagnostics
  ))
}

make_aggregate_cropland_endogeneity_robustness <- function(
  sfd, preferred_model, panel, pair_map = read_pair_map()
) {
  write_status("Estimating initial-share and lagged multi-year-change robustness checks.")
  weather_level <- c("d_gdd_common", "d_kdd_common", "d_soil_moisture_common")
  outcome <- "d_annual_cropland_change_pp"
  preferred_data <- complete_data(sfd, c(
    outcome, "d_mean_salinity", weather_level,
    "d_elevation", "d_slope", "d_clay", "Year", "lat", "lon", "pair_id"
  ))

  initial_needed <- c(
    outcome, "d_mean_salinity", "d_initial_cropland_share_pct",
    weather_level, "Year", "pair_id"
  )
  initial_data <- complete_data(sfd, initial_needed)
  initial_formula <- as.formula(paste(
    outcome, "~ d_mean_salinity + d_initial_cropland_share_pct +",
    paste(weather_level, collapse = " + "), "| Year"
  ))
  initial_model <- fixest::feols(
    initial_formula, data = initial_data, vcov = land_conley_vcov(200),
    panel.id = ~pair_id + Year, notes = FALSE
  )

  comparison_rows <- rbindlist(list(
    cbind(
      coef_numeric_row(preferred_model, "d_mean_salinity", "Preferred level specification"),
      exposure = "Mean salinity at t (dS/m)", pairs = uniqueN(preferred_data$pair_id)
    ),
    cbind(
      coef_numeric_row(initial_model, "d_mean_salinity", "Initial cropland share"),
      exposure = "Mean salinity at t (dS/m)", pairs = uniqueN(initial_data$pair_id)
    )
  ), fill = TRUE)
  fwrite(
    comparison_rows,
    file.path(paths$out_dir, "FullRevision_AggregateCropland_Endogeneity_Robustness.csv")
  )

  fixest::setFixest_dict(c(
    d_mean_salinity = "$\\Delta_s$ Mean salinity at $t$ (dS/m)",
    d_initial_cropland_share_pct = "$\\Delta_s$ Initial cropland share in 1985 (p.p.)",
    d_gdd_common = "$\\Delta_s$ GDD at $t$",
    d_kdd_common = "$\\Delta_s$ KDD at $t$",
    d_soil_moisture_common = "$\\Delta_s$ Soil moisture at $t$"
  ), reset = TRUE)
  robustness_tex <- file.path(
    paths$out_dir, "FullRevision_AggregateCropland_Endogeneity_Robustness.tex"
  )
  fixest::etable(
    preferred_model, initial_model,
    tex = TRUE, file = robustness_tex, replace = TRUE, depvar = FALSE,
    headers = list("Robustness specification" = c(
      "Preferred level", "Initial cropland share"
    )),
    title = "Initial-Condition Robustness for Annual Cropland Change",
    label = "tab:full_revision_aggregate_cropland_endogeneity",
    fitstat = ~ r2 + n,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    notes = paste(
      "Notes: The dependent variable is the SFD annual percentage-point change in total physical cropland from t to t+1.",
      "Column 1 reproduces the preferred level specification. Column 2 adds the east-minus-west difference in the 1985 cropland share.",
      "Both columns include year fixed effects, display all weather controls, and report Conley spatial standard errors with a 200 km cutoff."
    )
  )
  style_fixest_tex(robustness_tex, size = "\\scriptsize", tabcolsep = "3pt", arraystretch = "0.80", resize = TRUE)

  blocks <- copy(panel[Year >= 1985L & Year <= 2018L])
  blocks[, block_id := (Year - 1985L) %/% 5L]
  blocks[, expected_years := fifelse(block_id == 6L, 4L, 5L)]
  blocks <- blocks[
    ,
    .(
      years_observed = uniqueN(Year),
      expected_years = first(expected_years),
      cropland_share_mean = if (.N == first(expected_years) && all(is.finite(cropland_share_pct))) mean(cropland_share_pct) else NA_real_,
      salinity_mean = if (.N == first(expected_years) && all(is.finite(mean_salinity))) mean(mean_salinity) else NA_real_,
      gdd_mean = if (.N == first(expected_years) && all(is.finite(gdd_common))) mean(gdd_common) else NA_real_,
      kdd_mean = if (.N == first(expected_years) && all(is.finite(kdd_common))) mean(kdd_common) else NA_real_,
      soil_moisture_mean = if (.N == first(expected_years) && all(is.finite(soil_moisture_common))) mean(soil_moisture_common) else NA_real_
    ),
    by = .(Code, block_id)
  ][years_observed == expected_years]
  blocks[, `:=`(
    block_start = 1985L + 5L * block_id,
    block_end = pmin(1989L + 5L * block_id, 2018L)
  )]
  setorder(blocks, Code, block_id)
  blocks[, previous_block_id := shift(block_id), by = Code]
  blocks[, `:=`(
    ld_cropland_share_pp = cropland_share_mean - shift(cropland_share_mean),
    ld_mean_salinity = salinity_mean - shift(salinity_mean),
    ld_gdd = gdd_mean - shift(gdd_mean),
    ld_kdd = kdd_mean - shift(kdd_mean),
    ld_soil_moisture = soil_moisture_mean - shift(soil_moisture_mean)
  ), by = Code]
  ld_vars <- c(
    "ld_cropland_share_pp", "ld_mean_salinity", "ld_gdd", "ld_kdd",
    "ld_soil_moisture"
  )
  blocks[previous_block_id != block_id - 1L, (ld_vars) := NA_real_]
  blocks[, `:=`(
    prior_change_block_id = shift(block_id),
    ld_mean_salinity_prior = shift(ld_mean_salinity)
  ), by = Code]
  blocks[prior_change_block_id != block_id - 1L, ld_mean_salinity_prior := NA_real_]

  long_sfd_vars <- c(ld_vars, "ld_mean_salinity_prior")
  long_sfd <- make_sfd_from_unit_panel(
    blocks[, c("Code", "block_id", long_sfd_vars), with = FALSE],
    pair_map,
    id_cols = "block_id",
    vars = long_sfd_vars
  )
  long_weather <- c("d_ld_gdd", "d_ld_kdd", "d_ld_soil_moisture")
  long_common <- c(
    "d_ld_cropland_share_pp", long_weather, "block_id", "pair_id", "lat", "lon"
  )
  long_lagged_data <- complete_data(long_sfd, c(long_common, "d_ld_mean_salinity_prior"))
  long_lagged_formula <- as.formula(paste(
    "d_ld_cropland_share_pp ~ d_ld_mean_salinity_prior +",
    paste(long_weather, collapse = " + "), "| block_id"
  ))
  long_lagged_model <- fixest::feols(
    long_lagged_formula, data = long_lagged_data, vcov = "iid",
    panel.id = ~pair_id + block_id, notes = FALSE
  )
  transition_ids <- sort(unique(long_lagged_data$block_id))
  balanced_pair_ids <- long_lagged_data[
    , .(observed_transitions = uniqueN(block_id)), by = pair_id
  ][observed_transitions == length(transition_ids), pair_id]
  long_balanced_data <- long_lagged_data[pair_id %in% balanced_pair_ids]
  long_balanced_model <- fixest::feols(
    long_lagged_formula, data = long_balanced_data, vcov = land_conley_vcov(200),
    panel.id = ~pair_id + block_id, notes = FALSE
  )
  long_models <- list(
    "All available pairs" = long_lagged_model,
    "Balanced pairs" = long_balanced_model
  )

  long_rows <- rbindlist(lapply(seq_along(long_models), function(i) {
    model_data <- if (i == 1L) long_lagged_data else long_balanced_data
    coefficient_row <- if (i == 1L) {
      cbind(
        strict_conley_term(long_models[[i]], "d_ld_mean_salinity_prior", 200)[, .(estimate, se, p)],
        Model = names(long_models)[i], observations = stats::nobs(long_models[[i]])
      )
    } else {
      coef_numeric_row(long_models[[i]], "d_ld_mean_salinity_prior", names(long_models)[i])
    }
    cbind(
      coefficient_row,
      pairs = uniqueN(model_data$pair_id),
      periods = uniqueN(model_data$block_id),
      treatment_sd = sd(model_data$d_ld_mean_salinity_prior)
    )
  }), fill = TRUE)
  long_rows[, standardized_effect_pp := estimate * treatment_sd]
  fwrite(
    long_rows,
    file.path(paths$out_dir, "FullRevision_AggregateCropland_LongDifferences_Coefficients.csv")
  )
  conley_cutoffs <- c(50, 100, 200, 250, 300)
  long_conley <- rbindlist(lapply(seq_along(long_models), function(i) {
    rbindlist(lapply(conley_cutoffs, function(km) {
      cbind(
        strict_conley_term(long_models[[i]], "d_ld_mean_salinity_prior", km),
        specification = names(long_models)[i]
      )
    }), fill = TRUE)
  }), fill = TRUE)
  fwrite(
    long_conley,
    file.path(paths$out_dir, "FullRevision_AggregateCropland_LongDifferences_Conley_Robustness.csv")
  )

  block_diagnostics <- long_sfd[, .(
    observations = .N,
    pairs = uniqueN(pair_id),
    mean_outcome_change_pp = mean(d_ld_cropland_share_pp, na.rm = TRUE),
    sd_outcome_change_pp = sd(d_ld_cropland_share_pp, na.rm = TRUE),
    mean_salinity_change_ds_m = mean(d_ld_mean_salinity, na.rm = TRUE),
    sd_salinity_change_ds_m = sd(d_ld_mean_salinity, na.rm = TRUE),
    mean_prior_salinity_change_ds_m = mean(d_ld_mean_salinity_prior, na.rm = TRUE),
    sd_prior_salinity_change_ds_m = sd(d_ld_mean_salinity_prior, na.rm = TRUE)
  ), by = block_id]
  block_diagnostics[, `:=`(
    current_block = paste0(1985L + 5L * block_id, "--", pmin(1989L + 5L * block_id, 2018L)),
    previous_block = paste0(1980L + 5L * block_id, "--", 1984L + 5L * block_id)
  )]
  fwrite(
    block_diagnostics,
    file.path(paths$out_dir, "FullRevision_AggregateCropland_LongDifferences_BlockDiagnostics.csv")
  )

  lagged_salinity_correlation <- stats::cor(
    long_lagged_data$d_ld_mean_salinity,
    long_lagged_data$d_ld_mean_salinity_prior,
    use = "complete.obs"
  )
  leave_one_transition_out <- rbindlist(lapply(
    sort(unique(long_balanced_data$block_id)),
    function(block_to_drop) {
      d_loo <- long_balanced_data[block_id != block_to_drop]
      m_loo <- fixest::feols(
        long_lagged_formula, data = d_loo, vcov = "iid",
        panel.id = ~pair_id + block_id, notes = FALSE
      )
      loo_inference <- strict_conley_term(m_loo, "d_ld_mean_salinity_prior", 200)
      cbind(
        data.table(
          Model = paste0("Omit block ending ", pmin(1989L + 5L * block_to_drop, 2018L)),
          estimate = loo_inference$estimate,
          se = loo_inference$se,
          p = loo_inference$p,
          observations = stats::nobs(m_loo),
          valid_conley_200 = loo_inference$valid
        ),
        omitted_block_id = block_to_drop,
        pairs = uniqueN(d_loo$pair_id)
      )
    }
  ), fill = TRUE)
  fwrite(
    leave_one_transition_out,
    file.path(paths$out_dir, "FullRevision_AggregateCropland_LongDifferences_LeaveOneTransitionOut.csv")
  )
  transition_models <- lapply(transition_ids, function(block_value) {
      d_block <- long_balanced_data[block_id == block_value]
      block_formula <- as.formula(paste(
        "d_ld_cropland_share_pp ~ d_ld_mean_salinity_prior +",
        paste(long_weather, collapse = " + ")
      ))
      fixest::feols(
        block_formula, data = d_block, vcov = "iid",
        panel.id = ~pair_id + block_id, notes = FALSE
      )
  })
  by_transition <- rbindlist(lapply(seq_along(transition_models), function(i) {
      block_value <- transition_ids[i]
      d_block <- long_balanced_data[block_id == block_value]
      strict_transition <- strict_conley_term(
        transition_models[[i]], "d_ld_mean_salinity_prior", 200
      )
      cbind(
        data.table(
          Model = paste0("Block ending ", pmin(1989L + 5L * block_value, 2018L)),
          estimate = strict_transition$estimate,
          se = strict_transition$se,
          p = strict_transition$p,
          observations = stats::nobs(transition_models[[i]])
        ),
        block_id = block_value,
        pairs = uniqueN(d_block$pair_id),
        treatment_sd = sd(d_block$d_ld_mean_salinity_prior),
        strict_conley_200_valid = strict_transition$valid,
        strict_conley_200_se = strict_transition$se,
        strict_conley_200_p = strict_transition$p
      )
  }), fill = TRUE)
  by_transition[, standardized_effect_pp := estimate * treatment_sd]
  fwrite(
    by_transition,
    file.path(paths$out_dir, "FullRevision_AggregateCropland_LongDifferences_ByTransition.csv")
  )

  fixest::setFixest_dict(c(
    d_ld_mean_salinity = "$\\Delta_s\\Delta_B$ Mean salinity (dS/m)",
    d_ld_mean_salinity_prior = "$\\Delta_s\\Delta_B$ Mean salinity, previous transition (dS/m)",
    d_ld_gdd = "$\\Delta_s\\Delta_B$ GDD",
    d_ld_kdd = "$\\Delta_s\\Delta_B$ KDD",
    d_ld_soil_moisture = "$\\Delta_s\\Delta_B$ Soil moisture",
    block_id = "Block transition"
  ), reset = TRUE)
  long_tex <- file.path(
    paths$out_dir, "FullRevision_AggregateCropland_LongDifferences.tex"
  )
  fixest::etable(
    long_balanced_model,
    tex = TRUE, file = long_tex, replace = TRUE, depvar = FALSE,
    headers = list("Sample" = "Balanced pairs"),
    title = "Lagged Multi-Year Changes in Mean Salinity and Aggregate Cropland: Balanced Sample",
    label = "tab:full_revision_aggregate_cropland_long_differences",
    fitstat = ~ r2 + n,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    notes = paste(
      "Notes: Municipality variables are averaged within six non-overlapping five-year blocks from 1985--1989 through 2010--2014 and the final four-year block 2015--2018.",
      "The dependent variable is the east-minus-west difference in the change in mean total-cropland share between consecutive blocks.",
      "The treatment is the east-minus-west difference in the salinity change over the preceding block transition, so exposure predates the cropland outcome.",
      "The specification includes block-transition fixed effects and displays the contemporaneous long changes in GDD, KDD and soil moisture.",
      "The sample keeps only pairs observed in all five lagged outcome transitions, including the transition ending in 2018.",
      "Standard errors are Conley spatial with a 200 km cutoff. The lagged specification requires three consecutive complete blocks."
    )
  )
  style_fixest_tex(long_tex, size = "\\scriptsize", tabcolsep = "3pt", arraystretch = "0.80", resize = TRUE)
  if (length(transition_models) != 5L) {
    stop("Expected five lagged multi-year transitions for the stability table.")
  }
  transition_tex <- file.path(
    paths$out_dir, "FullRevision_AggregateCropland_LongDifferences_ByTransition.tex"
  )
  transition_terms <- c(
    "d_ld_mean_salinity_prior", "d_ld_gdd", "d_ld_kdd", "d_ld_soil_moisture"
  )
  transition_term_labels <- c(
    "Previous-transition mean salinity change (dS/m)",
    "GDD change", "KDD change", "Soil moisture change"
  )
  transition_table <- data.table(
    Variable = unlist(lapply(transition_term_labels, function(label) c(label, "")))
  )
  for (i in seq_along(transition_models)) {
    inference <- lapply(
      transition_terms,
      function(term) strict_conley_term(transition_models[[i]], term, 200)
    )
    values <- unlist(lapply(inference, function(row) {
      c(
        paste0(fmt(row$estimate, 4), if (isTRUE(row$valid)) stars(row$p) else ""),
        if (is.finite(row$se)) paste0("(", fmt(row$se, 4), ")") else "(NA)"
      )
    }))
    transition_table[, (as.character(pmin(1989L + 5L * transition_ids[i], 2018L))) := values]
  }
  transition_table <- rbind(
    transition_table,
    data.table(
      Variable = c("Observations", "Pairs", "Strict Conley covariance valid"),
      `1999` = c("1,992", "1,992", ifelse(by_transition[block_id == 2L, strict_conley_200_valid], "Yes", "No")),
      `2004` = c("1,992", "1,992", ifelse(by_transition[block_id == 3L, strict_conley_200_valid], "Yes", "No")),
      `2009` = c("1,992", "1,992", ifelse(by_transition[block_id == 4L, strict_conley_200_valid], "Yes", "No")),
      `2014` = c("1,992", "1,992", ifelse(by_transition[block_id == 5L, strict_conley_200_valid], "Yes", "No")),
      `2018` = c("1,992", "1,992", ifelse(by_transition[block_id == 6L, strict_conley_200_valid], "Yes", "No"))
    ),
    fill = TRUE
  )
  write_latex_df(
    transition_table, transition_tex,
    "Lagged Multi-Year Salinity Changes: Stability across Outcome Transitions",
    "tab:full_revision_aggregate_cropland_long_differences_by_transition",
    note = paste(
      "Each column estimates one outcome transition on the balanced sample. The final block covers 2015--2018; all earlier blocks contain five years.",
      "The treatment is the east-minus-west salinity change over the still earlier block transition.",
      "All controls are displayed. Standard errors use strict Conley spatial with a 200 km cutoff and no positive-semidefinite correction.",
      "NA means that the strict covariance matrix is not positive semidefinite, so no inference is reported for that transition. No transition fixed effect is needed within a single-transition column."
    ),
    size = "\\scriptsize", landscape = TRUE
  )

  diagnostics <- data.table(
    metric = c(
      "Initial-share observations", "Initial-share pairs",
      "Complete multi-year municipality blocks", "Municipalities in multi-year blocks",
      "Lagged-exposure long-difference observations", "Lagged-exposure long-difference pairs",
      "Balanced long-difference observations", "Balanced long-difference pairs",
      "Minimum complete block start", "Maximum complete block end",
      "Correlation of current and prior multi-year SFD salinity changes"
    ),
    value = c(
      stats::nobs(initial_model), uniqueN(initial_data$pair_id),
      nrow(blocks), uniqueN(blocks$Code),
      stats::nobs(long_lagged_model), uniqueN(long_lagged_data$pair_id),
      stats::nobs(long_balanced_model), uniqueN(long_balanced_data$pair_id),
      min(blocks$block_start), max(blocks$block_end), lagged_salinity_correlation
    )
  )
  fwrite(
    diagnostics,
    file.path(paths$out_dir, "FullRevision_AggregateCropland_Endogeneity_Diagnostics.csv")
  )

  invisible(list(
    initial_model = initial_model,
    long_lagged_model = long_lagged_model,
    long_balanced_model = long_balanced_model,
    comparison = comparison_rows, long_differences = long_rows,
    long_difference_conley = long_conley,
    block_diagnostics = block_diagnostics,
    leave_one_transition_out = leave_one_transition_out,
    by_transition = by_transition, transition_models = transition_models,
    diagnostics = diagnostics
  ))
}

make_aggregate_cropland_dynamics <- function(panel, pair_map = read_pair_map()) {
  write_status("Estimating cumulative cropland-change horizons.")
  horizons <- 1:5
  rows <- list()
  weather <- c("d_gdd_common", "d_kdd_common", "d_soil_moisture_common")
  for (h in horizons) {
    x <- copy(panel)
    x[, comparison_year := shift(Year, h, type = "lead"), by = Code]
    x[, comparison_share := shift(cropland_share_pct, h, type = "lead"), by = Code]
    x[comparison_year != Year + h, comparison_share := NA_real_]
    x[, cumulative_change_pp := comparison_share - cropland_share_pct]
    vars <- c("cumulative_change_pp", "mean_salinity", "gdd_common", "kdd_common", "soil_moisture_common")
    sfd <- make_sfd_from_unit_panel(x[, c("Code", "Year", vars), with = FALSE], pair_map, id_cols = "Year", vars = vars)
    needed <- c("d_cumulative_change_pp", "d_mean_salinity", weather, "Year", "lat", "lon", "pair_id")
    d <- complete_data(sfd, needed)
    f <- as.formula(paste(
      "d_cumulative_change_pp ~ d_mean_salinity +", paste(weather, collapse = " + "),
      "| Year"
    ))
    m <- fixest::feols(
      f, data = d, vcov = land_conley_vcov(200),
      panel.id = ~pair_id + Year, notes = FALSE
    )
    row <- coef_numeric_row(m, "d_mean_salinity", paste0("h=", h))
    row[, `:=`(horizon = h, observations = stats::nobs(m), pairs = uniqueN(d$pair_id))]
    rows[[length(rows) + 1L]] <- row
  }
  out <- rbindlist(rows)
  out[, `:=`(
    ci95_low = estimate - 1.96 * se, ci95_high = estimate + 1.96 * se,
    ci90_low = estimate - 1.645 * se, ci90_high = estimate + 1.645 * se
  )]
  fwrite(out, file.path(paths$out_dir, "FullRevision_AggregateCropland_CumulativeDynamics.csv"))
  dynamic_tex <- out[, .(
    Horizon = horizon,
    Coefficient = paste0(fmt(estimate, 4), stars(p)),
    `Std. error` = paste0("(", fmt(se, 4), ")"),
    `P-value` = fmt(p, 3),
    Observations = formatC(observations, format = "d", big.mark = ","),
    Pairs = formatC(pairs, format = "d", big.mark = ",")
  )]
  write_latex_df(
    dynamic_tex,
    file.path(paths$out_dir, "FullRevision_AggregateCropland_CumulativeDynamics.tex"),
    "Mean Salinity and Cumulative Changes in Total Physical Cropland",
    "tab:full_revision_aggregate_cropland_dynamics",
    note = paste(
      "Horizons measure the cumulative change from t to t+h.",
      "Each row is a separate SFD regression with baseline-year fixed effects and equally weighted crop-calendar GDD, KDD and soil moisture.",
      "Standard errors are Conley spatial with a 200 km cutoff. Stars denote significance at the 10, 5 and 1 percent levels."
    ),
    size = "\\small"
  )
  fig <- ggplot(out, aes(horizon, estimate)) +
    geom_hline(yintercept = 0, linewidth = 0.35, color = "grey45") +
    geom_vline(xintercept = 0, linewidth = 0.35, linetype = "dashed", color = "grey60") +
    geom_linerange(aes(ymin = ci95_low, ymax = ci95_high), color = "#2F6F8F", linewidth = 0.55) +
    geom_linerange(aes(ymin = ci90_low, ymax = ci90_high), color = "#2F6F8F", linewidth = 1.15) +
    geom_point(color = "#B55D2A", size = 2.3) +
    scale_x_continuous(breaks = horizons) +
    labs(x = "Horizon relative to baseline salinity", y = "Coefficient for cumulative cropland change (p.p.)") +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 12), axis.title = element_text(size = 13))
  ggsave(file.path(paths$out_dir, "FullRevision_AggregateCropland_CumulativeDynamics.pdf"), fig, width = 7.0, height = 4.1)
  ggsave(file.path(paths$out_dir, "FullRevision_AggregateCropland_CumulativeDynamics.png"), fig, width = 7.0, height = 4.1, dpi = 320)
  invisible(list(estimates = out, figure = fig))
}

make_aggregate_cropland_joint_distributed_lag <- function(
  panel, pair_map = read_pair_map(), max_lag = 5L
) {
  write_status("Estimating one joint distributed-lag model for aggregate cropland change.")
  max_lag <- as.integer(max_lag)
  if (!is.finite(max_lag) || max_lag < 0L) stop("max_lag must be a non-negative integer.")

  x <- copy(panel)
  setorder(x, Code, Year)
  exposure_source <- "mean_salinity"
  weather_sources <- c("gdd_common", "kdd_common", "soil_moisture_common")

  for (h in 0:max_lag) {
    lag_year_name <- paste0("source_year_lag", h)
    exposure_name <- paste0(exposure_source, "_lag", h)
    if (h == 0L) {
      x[, (lag_year_name) := Year]
      x[, (exposure_name) := get(exposure_source)]
    } else {
      x[, (lag_year_name) := shift(Year, h), by = Code]
      x[, (exposure_name) := shift(get(exposure_source), h), by = Code]
      x[get(lag_year_name) != Year - h, (exposure_name) := NA_real_]
    }
  }
  x[, (paste0("source_year_lag", 0:max_lag)) := NULL]
  for (v in weather_sources) x[, (paste0(v, "_lag0")) := get(v)]

  exposure_vars <- paste0(exposure_source, "_lag", 0:max_lag)
  weather_vars <- paste0(weather_sources, "_lag0")
  topo_vars <- c("elevation", "slope", "clay")
  vars <- c("annual_cropland_change_pp", exposure_vars, weather_vars, topo_vars)
  sfd <- make_sfd_from_unit_panel(
    x[, c("Code", "Year", vars), with = FALSE], pair_map,
    id_cols = "Year", vars = vars
  )

  exposure_terms <- paste0("d_", exposure_vars)
  weather_terms <- paste0("d_", weather_vars)
  topo_terms <- paste0("d_", topo_vars)
  needed <- c(
    "d_annual_cropland_change_pp", exposure_terms, weather_terms, topo_terms,
    "Year", "lat", "lon", "pair_id"
  )
  d <- complete_data(sfd, needed)
  if (nrow(d) == 0L) stop("The joint distributed-lag cropland sample is empty.")

  rhs <- list(
    "Year FE" = exposure_terms,
    "Weather" = c(exposure_terms, weather_terms),
    "Weather + soil/topography" = c(exposure_terms, weather_terms, topo_terms)
  )
  models <- lapply(rhs, function(terms) {
    fixest::feols(
      stats::as.formula(paste(
        "d_annual_cropland_change_pp ~", paste(terms, collapse = " + "), "| Year"
      )),
      data = d, vcov = land_conley_vcov(200),
      panel.id = ~pair_id + Year, notes = FALSE
    )
  })
  preferred <- models[["Weather"]]

  coefficient_rows <- rbindlist(lapply(seq_along(models), function(i) {
    rbindlist(lapply(seq_along(exposure_terms), function(j) {
      row <- coef_numeric_row(models[[i]], exposure_terms[[j]], names(models)[i])
      row[, `:=`(
        specification = names(models)[i], lag = j - 1L,
        term = exposure_terms[[j]], observations = stats::nobs(models[[i]])
      )]
      row
    }))
  }))

  preferred_lags <- coefficient_rows[specification == "Weather"]
  cumulative_rows <- rbindlist(lapply(seq_along(exposure_terms), function(j) {
    row <- extract_model_sum(preferred, exposure_terms[seq_len(j)])
    row[, `:=`(
      specification = "Weather", lag = j - 1L,
      term = paste0("sum_through_lag", j - 1L),
      observations = stats::nobs(preferred)
    )]
    row
  }))

  lag_correlation <- stats::cor(d[, ..exposure_terms])
  off_diagonal <- lag_correlation[upper.tri(lag_correlation)]
  vif_rows <- rbindlist(lapply(seq_along(exposure_terms), function(i) {
    lhs <- exposure_terms[[i]]
    rhs_terms <- exposure_terms[-i]
    fit <- stats::lm(
      stats::as.formula(paste(lhs, "~", paste(rhs_terms, collapse = " + "), "+ factor(Year)")),
      data = d
    )
    r2 <- summary(fit)$r.squared
    data.table(
      lag = i - 1L, term = lhs, r_squared_on_other_lags_and_year_fe = r2,
      vif = if (is.finite(r2) && r2 < 1) 1 / (1 - r2) else Inf
    )
  }))

  wald_test <- function(model, terms, test_name) {
    b <- stats::coef(model)[terms]
    v <- stats::vcov(model)[terms, terms, drop = FALSE]
    stat <- try(as.numeric(crossprod(b, solve(v, b))), silent = TRUE)
    if (inherits(stat, "try-error") || !is.finite(stat)) stat <- NA_real_
    data.table(
      test = test_name, restrictions = length(terms), statistic = stat,
      p_value = if (is.finite(stat)) stats::pchisq(stat, df = length(terms), lower.tail = FALSE) else NA_real_
    )
  }
  joint_tests <- rbind(
    wald_test(preferred, exposure_terms, "All salinity lags jointly zero"),
    wald_test(preferred, exposure_terms[-1L], "Delayed salinity lags jointly zero")
  )
  preferred_b <- stats::coef(preferred)[exposure_terms]
  preferred_v <- stats::vcov(preferred)[exposure_terms, exposure_terms, drop = FALSE]
  equality_r <- matrix(0, nrow = length(exposure_terms) - 1L, ncol = length(exposure_terms))
  for (i in seq_len(nrow(equality_r))) {
    equality_r[i, 1L] <- -1
    equality_r[i, i + 1L] <- 1
  }
  equality_diff <- as.numeric(equality_r %*% preferred_b)
  equality_v <- equality_r %*% preferred_v %*% t(equality_r)
  equality_stat <- try(as.numeric(crossprod(equality_diff, solve(equality_v, equality_diff))), silent = TRUE)
  if (inherits(equality_stat, "try-error") || !is.finite(equality_stat) || equality_stat < 0) {
    equality_stat <- NA_real_
  }
  joint_tests <- rbind(
    joint_tests,
    data.table(
      test = "All salinity-lag coefficients equal",
      restrictions = nrow(equality_r),
      statistic = equality_stat,
      p_value = if (is.finite(equality_stat)) {
        stats::pchisq(equality_stat, df = nrow(equality_r), lower.tail = FALSE)
      } else {
        NA_real_
      }
    ),
    fill = TRUE
  )
  sum_row <- extract_model_sum(preferred, exposure_terms)
  joint_tests <- rbind(
    joint_tests,
    data.table(
      test = "Sum of all salinity-lag coefficients", restrictions = 1L,
      statistic = if (is.finite(sum_row$se) && sum_row$se > 0) (sum_row$estimate / sum_row$se)^2 else NA_real_,
      p_value = sum_row$p,
      estimate = sum_row$estimate,
      standard_error = sum_row$se
    ),
    fill = TRUE
  )

  lag_comparisons <- rbindlist(lapply(seq_along(exposure_terms), function(i) {
    rbindlist(lapply(seq_along(exposure_terms), function(j) {
      if (j <= i) return(NULL)
      contrast <- rep(0, length(exposure_terms))
      contrast[i] <- 1
      contrast[j] <- -1
      difference <- sum(contrast * preferred_b)
      difference_se <- sqrt(as.numeric(t(contrast) %*% preferred_v %*% contrast))
      data.table(
        lag_1 = i - 1L, lag_2 = j - 1L,
        difference = difference, standard_error = difference_se,
        p_value = if (is.finite(difference_se) && difference_se > 0) {
          2 * stats::pnorm(-abs(difference / difference_se))
        } else {
          NA_real_
        }
      )
    }), fill = TRUE)
  }), fill = TRUE)

  conley_rows <- rbindlist(lapply(c(50, 100, 200, 250, 300), function(km) {
    rbindlist(lapply(exposure_terms, function(term) {
      row <- strict_conley_term(preferred, term, km)
      row[, `:=`(term = term, lag = match(term, exposure_terms) - 1L)]
      row
    }))
  }))

  diagnostics <- data.table(
    metric = c(
      "Joint-model observations", "Strictly contiguous pairs",
      "Minimum baseline year", "Maximum baseline year", "Salinity lags in one model",
      "Minimum pairwise correlation among salinity lags",
      "Maximum pairwise correlation among salinity lags",
      "Maximum VIF among salinity lags", "Mean annual SFD cropland change (p.p.)",
      "SD annual SFD cropland change (p.p.)"
    ),
    value = c(
      nrow(d), uniqueN(d$pair_id), min(d$Year), max(d$Year), length(exposure_terms),
      min(off_diagonal), max(off_diagonal), max(vif_rows$vif),
      mean(d$d_annual_cropland_change_pp), sd(d$d_annual_cropland_change_pp)
    )
  )

  output_stub <- "FullRevision_AggregateCropland_JointDistributedLag"
  fwrite(coefficient_rows, file.path(paths$out_dir, paste0(output_stub, "_Coefficients.csv")))
  fwrite(cumulative_rows, file.path(paths$out_dir, paste0(output_stub, "_CumulativeSums.csv")))
  fwrite(vif_rows, file.path(paths$out_dir, paste0(output_stub, "_VIF.csv")))
  fwrite(as.data.table(lag_correlation, keep.rownames = "term"), file.path(paths$out_dir, paste0(output_stub, "_Correlations.csv")))
  fwrite(joint_tests, file.path(paths$out_dir, paste0(output_stub, "_JointTests.csv")))
  fwrite(lag_comparisons, file.path(paths$out_dir, paste0(output_stub, "_LagComparisons.csv")))
  fwrite(conley_rows, file.path(paths$out_dir, paste0(output_stub, "_Conley.csv")))
  fwrite(diagnostics, file.path(paths$out_dir, paste0(output_stub, "_Diagnostics.csv")))

  dictionary <- c(
    d_elevation = "$\\Delta_s$ Elevation",
    d_slope = "$\\Delta_s$ Slope",
    d_clay = "$\\Delta_s$ Clay"
  )
  for (h in 0:max_lag) {
    dictionary[paste0("d_mean_salinity_lag", h)] <- paste0(
      "$\\Delta_s$ Mean salinity at $t", if (h == 0L) "" else paste0("-", h), "$ (dS/m)"
    )
    dictionary[paste0("d_gdd_common_lag", h)] <- paste0(
      "$\\Delta_s$ GDD at $t", if (h == 0L) "" else paste0("-", h), "$"
    )
    dictionary[paste0("d_kdd_common_lag", h)] <- paste0(
      "$\\Delta_s$ KDD at $t", if (h == 0L) "" else paste0("-", h), "$"
    )
    dictionary[paste0("d_soil_moisture_common_lag", h)] <- paste0(
      "$\\Delta_s$ Soil moisture at $t", if (h == 0L) "" else paste0("-", h), "$"
    )
  }
  fixest::setFixest_dict(dictionary, reset = TRUE)
  tex_file <- file.path(paths$out_dir, paste0(output_stub, ".tex"))
  fixest::etable(
    models[[1]], models[[2]], models[[3]],
    tex = TRUE, file = tex_file, replace = TRUE, depvar = FALSE,
    keep = "%d_mean_salinity_lag",
    headers = list("Specification" = c("Year FE", "Weather", "Weather + soil/topography")),
    title = "Joint Distributed-Lag Model for Annual Total-Cropland Change: Salinity Coefficients",
    label = "tab:full_revision_aggregate_cropland_joint_distributed_lag",
    fitstat = ~ r2 + n,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    notes = paste(
      "Notes: An observation is one strictly contiguous east-west municipality pair and one baseline year.",
      "The dependent variable is the east-minus-west difference in the annual percentage-point change in total physical cropland from t to t+1.",
      paste0("Mean salinity at t through t-", max_lag, " enters jointly in every column."),
      "Spec. 2 adds GDD, KDD and soil moisture in t and is preferred.",
      "Spec. 3 adds elevation, slope and clay. All columns use the same complete-data sample and include year fixed effects.",
      "No pair fixed effects are included. Standard errors are Conley spatial with a 200 km cutoff.",
      "All estimated controls are reported in the companion controls table."
    )
  )
  style_fixest_tex(tex_file, size = "\\scriptsize", tabcolsep = "2.2pt", arraystretch = "0.82", resize = FALSE)

  controls_tex_file <- file.path(paths$out_dir, paste0(output_stub, "_Controls.tex"))
  fixest::etable(
    models[[1]], models[[2]], models[[3]],
    tex = TRUE, file = controls_tex_file, replace = TRUE, depvar = FALSE,
    drop = "%d_mean_salinity_lag",
    headers = list("Specification" = c("Year FE", "Weather", "Weather + soil/topography")),
    title = "Joint Distributed-Lag Model for Annual Total-Cropland Change: Estimated Controls",
    label = "tab:full_revision_aggregate_cropland_joint_distributed_lag_controls",
    fitstat = ~ r2 + n,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    notes = paste(
      "Notes: This table reports every estimated control from the same three models and common sample as the salinity-coefficient table.",
      "Spec. 2 includes GDD, KDD and soil moisture in t and is preferred. Spec. 3 additionally includes elevation, slope and clay.",
      "All columns include year fixed effects and omit pair fixed effects. Standard errors are Conley spatial with a 200 km cutoff."
    )
  )
  style_fixest_tex(controls_tex_file, size = "\\scriptsize", tabcolsep = "2.2pt", arraystretch = "0.82", resize = FALSE)

  period_levels <- c(
    if (max_lag > 0L) paste0("t-", max_lag:1L) else character(),
    "t"
  )
  period_label <- function(lag_value) {
    factor(
      ifelse(lag_value == 0L, "t", paste0("t-", lag_value)),
      levels = period_levels
    )
  }
  plot_lags <- preferred_lags[, .(
    lag, period = period_label(lag), estimate, se, p,
    ci95_low = estimate - 1.96 * se, ci95_high = estimate + 1.96 * se,
    ci90_low = estimate - 1.645 * se, ci90_high = estimate + 1.645 * se,
    panel = "A. Conditional effect by year"
  )]
  plot_sums <- cumulative_rows[, .(
    lag, period = period_label(lag), estimate, se, p,
    ci95_low = estimate - 1.96 * se, ci95_high = estimate + 1.96 * se,
    ci90_low = estimate - 1.645 * se, ci90_high = estimate + 1.645 * se,
    panel = "B. Sustained exposure through t"
  )]
  plot_data <- rbind(plot_lags, plot_sums)
  plot_data[, panel := factor(
    panel, levels = c(
      "A. Conditional effect by year",
      "B. Sustained exposure through t"
    )
  )]
  setorder(plot_data, panel, period)
  figure_stub <- paste0(output_stub, "_Chronological_Stacked")
  fwrite(plot_data, file.path(paths$out_dir, paste0(figure_stub, "_PlotData.csv")))
  fig <- ggplot(plot_data, aes(period, estimate)) +
    geom_hline(yintercept = 0, linewidth = 0.35, color = "grey45") +
    geom_linerange(aes(ymin = ci95_low, ymax = ci95_high), color = "#2F6F8F", linewidth = 0.55) +
    geom_linerange(aes(ymin = ci90_low, ymax = ci90_high), color = "#2F6F8F", linewidth = 1.15) +
    geom_point(color = "#B55D2A", size = 2.3) +
    facet_wrap(
      ~panel, ncol = 1, scales = "free_y",
      axes = "all_x", axis.labels = "all_x"
    ) +
    scale_x_discrete(drop = FALSE) +
    labs(
      x = "Year of salinity exposure",
      y = "Effect of 1 dS/m on annual net cropland change (p.p.)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 10.5), axis.title = element_text(size = 11.5),
      strip.text = element_text(size = 11.5, face = "bold"),
      panel.spacing.y = grid::unit(0.8, "lines")
    )
  ggsave(file.path(paths$out_dir, paste0(figure_stub, ".pdf")), fig, width = 8.2, height = 7.2)
  ggsave(file.path(paths$out_dir, paste0(figure_stub, ".png")), fig, width = 8.2, height = 7.2, dpi = 320)

  invisible(list(
    data = d, models = models, coefficients = coefficient_rows,
    cumulative_sums = cumulative_rows, joint_tests = joint_tests,
    vif = vif_rows, correlations = lag_correlation, conley = conley_rows,
    diagnostics = diagnostics, figure = fig
  ))
}

make_cropland_exit_decomposition <- function(prepared = NULL) {
  write_status("Decomposing gross cropland exit, entry, and net loss.")
  if (is.null(prepared)) prepared <- make_mapbiomas_cropland_panel()
  panel <- copy(prepared$panel)
  setorder(panel, Code, Year)
  panel[, `:=`(
    lag_total_agri_hectares = shift(total_agri_hectares),
    lag_year = shift(Year)
  ), by = Code]
  panel[lag_year != Year - 1L, lag_total_agri_hectares := NA_real_]

  gross <- as.data.table(readRDS(paths$abandonment))
  gross[, `:=`(Code = trimws(as.character(Code)), Year = as.integer(Year))]
  gross <- gross[Year >= 1986L & Year <= 2018L]
  gross <- merge(
    gross,
    panel[, .(Code, Year, total_agri_hectares, lag_total_agri_hectares)],
    by = c("Code", "Year"), all.x = FALSE, all.y = FALSE
  )
  gross[, lag_stock_difference_hectares := lag_agriculture_hectares - lag_total_agri_hectares]
  gross[, gross_exit_pct := fifelse(
    is.finite(lag_agriculture_hectares) & lag_agriculture_hectares > 0,
    100 * abandoned_hectares / lag_agriculture_hectares,
    NA_real_
  )]
  gross[, net_loss_pct := fifelse(
    is.finite(lag_agriculture_hectares) & lag_agriculture_hectares > 0,
    100 * (lag_agriculture_hectares - total_agri_hectares) / lag_agriculture_hectares,
    NA_real_
  )]
  gross[, entry_pct := gross_exit_pct - net_loss_pct]
  gross[entry_pct < 0 & entry_pct > -1e-6, entry_pct := 0]
  identity_error <- gross[, max(abs(gross_exit_pct - entry_pct - net_loss_pct), na.rm = TRUE)]
  transition_diagnostics <- gross[, .(
    municipality_years = .N,
    positive_lag_stock_rows = sum(is.finite(lag_agriculture_hectares) & lag_agriculture_hectares > 0),
    mean_absolute_lag_stock_difference_hectares = mean(abs(lag_stock_difference_hectares), na.rm = TRUE),
    maximum_absolute_lag_stock_difference_hectares = max(abs(lag_stock_difference_hectares), na.rm = TRUE),
    negative_entry_rows = sum(entry_pct < -1e-6, na.rm = TRUE),
    minimum_entry_pct = min(entry_pct, na.rm = TRUE),
    identity_max_absolute_error = identity_error
  )]
  transition_diagnostics[, consistent_stock_share := mean(
    abs(gross$lag_stock_difference_hectares) <= pmax(1, 1e-4 * gross$lag_agriculture_hectares),
    na.rm = TRUE
  )]
  fwrite(transition_diagnostics, file.path(paths$out_dir, "FullRevision_Cropland_Exit_Transition_Diagnostics.csv"))
  if (
    transition_diagnostics$consistent_stock_share < 0.99 ||
    transition_diagnostics$negative_entry_rows > 0
  ) {
    invalid_outputs <- file.path(paths$out_dir, c(
      "FullRevision_Cropland_Exit_Decomposition_Coefficients.csv",
      "FullRevision_Cropland_Exit_Decomposition.pdf",
      "FullRevision_Cropland_Exit_Decomposition.png",
      "FullRevision_Cropland_Exit_Decomposition.tex",
      "FullRevision_Cropland_Exit_Identity_Check.csv"
    ))
    unlink(invalid_outputs[file.exists(invalid_outputs)])
    write_status(
      "Skipping gross-exit decomposition: the existing transition mask includes a broader land-use class than the all-crop stock."
    )
    return(invisible(list(valid = FALSE, diagnostics = transition_diagnostics)))
  }

  env <- copy(prepared$environment)
  env[, Year := Year + 1L]
  setnames(
    env,
    c("mean_salinity", "gdd_common", "kdd_common", "soil_moisture_common", "elevation", "slope", "clay"),
    c("mean_salinity_lag1", "gdd_common_lag1", "kdd_common_lag1", "soil_moisture_common_lag1", "elevation", "slope", "clay")
  )
  keep_env <- c("Code", "Year", "mean_salinity_lag1", "gdd_common_lag1", "kdd_common_lag1", "soil_moisture_common_lag1", "elevation", "slope", "clay")
  gross <- merge(gross, env[, ..keep_env], by = c("Code", "Year"), all.x = TRUE)

  pair_map <- read_pair_map()
  outcomes <- c("gross_exit_pct", "entry_pct", "net_loss_pct")
  weather <- c("d_gdd_common_lag1", "d_kdd_common_lag1", "d_soil_moisture_common_lag1")
  topo <- c("d_elevation", "d_slope", "d_clay")
  vars <- unique(c(
    outcomes, "mean_salinity_lag1",
    sub("^d_", "", weather),
    sub("^d_", "", topo)
  ))
  sfd <- make_sfd_from_unit_panel(gross[, c("Code", "Year", vars), with = FALSE], pair_map, id_cols = "Year", vars = vars)
  needed <- c(paste0("d_", outcomes), "d_mean_salinity_lag1", weather, topo, "Year", "lat", "lon", "pair_id")
  d <- complete_data(sfd, needed)

  outcome_labels <- c(
    gross_exit_pct = "Gross exit", entry_pct = "Entry into cropland", net_loss_pct = "Net cropland loss"
  )
  spec_names <- c("Year FE", "Weather", "Weather + soil/topography", "Pair and year FE")
  model_sets <- lapply(outcomes, function(outcome) {
    fit_land_change_models(
      d, paste0("d_", outcome), "d_mean_salinity_lag1",
      weather = weather, topo = topo
    )
  })
  names(model_sets) <- outcomes
  rows <- rbindlist(lapply(outcomes, function(outcome) {
    rbindlist(lapply(seq_along(model_sets[[outcome]]), function(i) {
      row <- coef_numeric_row(model_sets[[outcome]][[i]], "d_mean_salinity_lag1", spec_names[i])
      row[, `:=`(outcome = outcome, outcome_label = outcome_labels[outcome], specification = spec_names[i])]
      row
    }))
  }))
  fwrite(rows, file.path(paths$out_dir, "FullRevision_Cropland_Exit_Decomposition_Coefficients.csv"))
  fwrite(data.table(identity_max_absolute_error = identity_error), file.path(paths$out_dir, "FullRevision_Cropland_Exit_Identity_Check.csv"))

  plot_data <- rows[specification %in% c("Weather", "Weather + soil/topography", "Pair and year FE")]
  plot_data[, outcome_label := factor(outcome_label, levels = rev(unname(outcome_labels)))]
  plot_data[, specification := factor(specification, levels = c("Weather", "Weather + soil/topography", "Pair and year FE"))]
  palette <- c("Weather" = "#D47A1F", "Weather + soil/topography" = "#6A51A3", "Pair and year FE" = "#2F6F8F")
  fig <- ggplot(plot_data, aes(estimate, outcome_label, color = specification, shape = specification)) +
    geom_vline(xintercept = 0, linewidth = 0.35, color = "grey45") +
    geom_linerange(aes(xmin = estimate - 1.96 * se, xmax = estimate + 1.96 * se), position = position_dodge(width = 0.45), linewidth = 0.5) +
    geom_linerange(aes(xmin = estimate - 1.645 * se, xmax = estimate + 1.645 * se), position = position_dodge(width = 0.45), linewidth = 1.05) +
    geom_point(position = position_dodge(width = 0.45), size = 2.3) +
    scale_color_manual(values = palette) +
    scale_shape_manual(values = c("Weather" = 16, "Weather + soil/topography" = 17, "Pair and year FE" = 15)) +
    labs(x = "Effect of 1 dS/m lagged mean salinity (p.p.)", y = NULL, color = NULL, shape = NULL) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom", axis.text = element_text(size = 11))
  ggsave(file.path(paths$out_dir, "FullRevision_Cropland_Exit_Decomposition.pdf"), fig, width = 7.4, height = 4.2)
  ggsave(file.path(paths$out_dir, "FullRevision_Cropland_Exit_Decomposition.png"), fig, width = 7.4, height = 4.2, dpi = 320)

  all_models <- unlist(model_sets, recursive = FALSE)
  fixest::setFixest_dict(c(
    d_mean_salinity_lag1 = "$\\Delta_s$ Mean salinity in $t-1$ (dS/m)",
    d_gdd_common_lag1 = "$\\Delta_s$ GDD in $t-1$",
    d_kdd_common_lag1 = "$\\Delta_s$ KDD in $t-1$",
    d_soil_moisture_common_lag1 = "$\\Delta_s$ Soil moisture in $t-1$",
    d_elevation = "$\\Delta_s$ Elevation", d_slope = "$\\Delta_s$ Slope", d_clay = "$\\Delta_s$ Clay"
  ), reset = TRUE)
  tex_file <- file.path(paths$out_dir, "FullRevision_Cropland_Exit_Decomposition.tex")
  fixest::etable(
    all_models,
    tex = TRUE, file = tex_file, replace = TRUE, depvar = FALSE,
    headers = list(
      "Outcome" = rep(unname(outcome_labels), each = 4L),
      "Specification" = rep(spec_names, times = 3L)
    ),
    title = "Mean Salinity, Gross Cropland Exit, Entry and Net Loss",
    label = "tab:full_revision_cropland_exit_decomposition",
    fitstat = ~ r2 + n,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    notes = paste(
      "Notes: Gross exit is the share of cropland hectares in t-1 reclassified as non-cropland in t. Entry is obtained from the exact stock identity, and net loss equals gross exit minus entry.",
      "All outcomes use cropland in t-1 as the denominator. The treatment and weather controls are measured in t-1.",
      "All columns use one common complete-data sample. Standard errors are clustered by spatial pair."
    )
  )
  style_fixest_tex(tex_file, size = "\\tiny", tabcolsep = "1.6pt", arraystretch = "0.68", resize = TRUE, landscape = TRUE)
  invisible(list(
    data = d, models = model_sets, coefficients = rows,
    identity_error = identity_error, diagnostics = transition_diagnostics, figure = fig
  ))
}

make_land_abandonment_analysis <- function() {
  write_status("Building net abandonment and persistence proxies.")
  if (!file.exists(paths$total_agri) || !file.exists(paths$muni_ref)) {
    write_status("Skipping net abandonment: required MapBiomas total-agriculture files are missing.")
    return(NULL)
  }
  ref <- fread(paths$muni_ref, encoding = "UTF-8", colClasses = list(character = "Code"))
  ag <- readRDS(paths$total_agri)
  setDT(ag)
  ref[, join_key := paste(state, clean_name(muni_name), sep = "__")]
  ag[, join_key := paste(state_acronym, clean_name(Municipality), sep = "__")]
  ref_unique <- ref[, .SD[1], by = join_key][, .(join_key, Code)]
  ag <- merge(ag, ref_unique, by = "join_key", all.x = TRUE, sort = FALSE)
  match_diag <- ag[, .(
    rows = .N,
    missing_code_rows = sum(is.na(Code)),
    matched_codes = uniqueN(Code, na.rm = TRUE)
  )]
  fwrite(match_diag, file.path(paths$out_dir, "FullRevision_MapBiomas_TotalAgri_Join_Diagnostics.csv"))
  if (match_diag$matched_codes < 5000) {
    write_status("Skipping net abandonment because the name-code join matched fewer than 5,000 municipalities.")
    return(NULL)
  }
  ag <- ag[Year >= 1985 & Year <= 2018 & !is.na(Code)]
  ag[, Code := trimws(as.character(Code))]
  setorder(ag, Code, Year)
  ag[, lag_total_agri_hectares := shift(total_agri_hectares), by = Code]
  ag[, net_abandonment_pct := fifelse(
    is.finite(lag_total_agri_hectares) & lag_total_agri_hectares > 0,
    100 * (lag_total_agri_hectares - total_agri_hectares) / lag_total_agri_hectares,
    NA_real_
  )]
  ag[, net_loss_positive := is.finite(net_abandonment_pct) & net_abandonment_pct > 0]
  ag[, spell_id := rleid(net_loss_positive), by = Code]
  ag[, spell_age := seq_len(.N), by = .(Code, spell_id)]
  ag[net_loss_positive == FALSE, spell_age := 0L]

  env <- make_muni_year_environment(read_pam_zeros())
  keep_env <- c("Code", "Year", "mean_salinity_lag1", "gdd_large_lag1", "kdd_large_lag1", "sm_season_large_lag1", "elevation_large", "slope_large", "clay_mean_large")
  ag <- merge(ag, env[, ..keep_env], by = c("Code", "Year"), all.x = TRUE)
  pair_map <- read_pair_map()
  vars <- c("net_abandonment_pct", "spell_age", setdiff(keep_env, c("Code", "Year")))
  sfd <- make_sfd_from_unit_panel(ag[, c("Code", "Year", vars), with = FALSE], pair_map, id_cols = "Year", vars = vars)
  vc <- make_conley()
  weather <- c("d_gdd_large_lag1", "d_kdd_large_lag1", "d_sm_season_large_lag1")
  topo <- c("d_elevation_large", "d_slope_large", "d_clay_mean_large")
  needed <- c("d_net_abandonment_pct", "d_spell_age", "d_mean_salinity_lag1", weather, topo, "Year", "lat", "lon", "pair_id")
  d <- complete_data(sfd, needed)
  f_net1 <- d_net_abandonment_pct ~ d_mean_salinity_lag1 | Year
  f_net2 <- as.formula(paste("d_net_abandonment_pct ~ d_mean_salinity_lag1 +", paste(weather, collapse = " + "), "| Year"))
  f_net3 <- as.formula(paste("d_net_abandonment_pct ~ d_mean_salinity_lag1 +", paste(c(weather, topo), collapse = " + "), "| Year"))
  f_spell1 <- d_spell_age ~ d_mean_salinity_lag1 | Year
  f_spell2 <- as.formula(paste("d_spell_age ~ d_mean_salinity_lag1 +", paste(weather, collapse = " + "), "| Year"))
  f_spell3 <- as.formula(paste("d_spell_age ~ d_mean_salinity_lag1 +", paste(c(weather, topo), collapse = " + "), "| Year"))
  m_net1 <- fixest::feols(f_net1, data = d, vcov = vc, panel.id = ~pair_id + Year, notes = FALSE)
  m_net2 <- fixest::feols(f_net2, data = d, vcov = vc, panel.id = ~pair_id + Year, notes = FALSE)
  m_net3 <- fixest::feols(f_net3, data = d, vcov = vc, panel.id = ~pair_id + Year, notes = FALSE)
  m_spell1 <- fixest::feols(f_spell1, data = d, vcov = vc, panel.id = ~pair_id + Year, notes = FALSE)
  m_spell2 <- fixest::feols(f_spell2, data = d, vcov = vc, panel.id = ~pair_id + Year, notes = FALSE)
  m_spell3 <- fixest::feols(f_spell3, data = d, vcov = vc, panel.id = ~pair_id + Year, notes = FALSE)
  tab <- rbindlist(list(
    cbind(coef_row(m_net1, "d_mean_salinity_lag1", "Net abandonment, Spec. 1"), spec_control_flags(1)),
    cbind(coef_row(m_net2, "d_mean_salinity_lag1", "Net abandonment, Spec. 2"), spec_control_flags(2)),
    cbind(coef_row(m_net3, "d_mean_salinity_lag1", "Net abandonment, Spec. 3"), spec_control_flags(3)),
    cbind(coef_row(m_spell1, "d_mean_salinity_lag1", "Net-loss duration, Spec. 1"), spec_control_flags(1)),
    cbind(coef_row(m_spell2, "d_mean_salinity_lag1", "Net-loss duration, Spec. 2"), spec_control_flags(2)),
    cbind(coef_row(m_spell3, "d_mean_salinity_lag1", "Net-loss duration, Spec. 3"), spec_control_flags(3))
  ))
  fwrite(tab, file.path(paths$out_dir, "FullRevision_Net_Abandonment_Persistence_Coefficients.csv"))

  fixest::setFixest_dict(c(
    d_mean_salinity_lag1 = "$\\Delta_s$ Mean salinity, lagged (dS/m)",
    d_gdd_large_lag1 = "$\\Delta_s$ GDD, lagged",
    d_kdd_large_lag1 = "$\\Delta_s$ KDD, lagged",
    d_sm_season_large_lag1 = "$\\Delta_s$ Soil moisture, lagged",
    d_elevation_large = "$\\Delta_s$ Elevation",
    d_slope_large = "$\\Delta_s$ Slope",
    d_clay_mean_large = "$\\Delta_s$ Clay"
  ), reset = TRUE)
  tex_file <- file.path(paths$out_dir, "FullRevision_Net_Abandonment_Persistence.tex")
  fixest::etable(
    m_net1, m_net2, m_net3, m_spell1, m_spell2, m_spell3,
    tex = TRUE,
    file = tex_file,
    replace = TRUE,
    headers = list(
      "Outcome" = c(rep("Net abandonment", 3L), rep("Net-loss duration", 3L)),
      "Spec" = rep(paste0("Spec. ", 1:3), 2L)
    ),
    depvar = FALSE,
    title = "Soil Salinity, Net Agricultural Abandonment and Persistence",
    label = "tab:full_revision_net_abandonment_persistence",
    fitstat = ~ r2 + n,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    notes = paste(
      "Notes: Net abandonment is the annual decline in total agricultural area from MapBiomas divided by agricultural area in the previous year.",
      "This measure differs from gross exit, which counts agricultural hectares in t-1 that become non-agricultural in t.",
      "Net-loss duration is a municipal persistence measure: the number of consecutive years up to t with a net agricultural-area loss.",
      "It does not track individual pixels; a parcel-level duration measure would require pixel-level land-use histories.",
      "Spec. 1 includes year fixed effects; Spec. 2 adds lagged GDD, KDD and soil moisture; Spec. 3 additionally adds elevation, slope and clay content.",
      "All columns use the same complete-data sample; standard errors are Conley spatial with a 200 km cutoff."
    )
  )
  style_fixest_tex(tex_file, size = "\\scriptsize", tabcolsep = "2pt", arraystretch = "0.78", resize = TRUE)

  horizons <- -3:5
  for (h in horizons) {
    nm <- paste0("net_abandonment_h", ifelse(h < 0, "m", "p"), abs(h))
    if (h >= 0) {
      ag[, (nm) := shift(net_abandonment_pct, n = h, type = "lead"), by = Code]
    } else {
      ag[, (nm) := shift(net_abandonment_pct, n = abs(h), type = "lag"), by = Code]
    }
  }
  event_vars <- c(
    paste0("net_abandonment_h", ifelse(horizons < 0, "m", "p"), abs(horizons)),
    setdiff(keep_env, c("Code", "Year"))
  )
  event_sfd <- make_sfd_from_unit_panel(ag[, c("Code", "Year", event_vars), with = FALSE], pair_map, id_cols = "Year", vars = event_vars)
  event_rows <- list()
  for (h in horizons) {
    y <- paste0("d_net_abandonment_h", ifelse(h < 0, "m", "p"), abs(h))
    needed_h <- c(y, "d_mean_salinity_lag1", weather, topo, "Year", "lat", "lon", "pair_id")
    dh <- complete_data(event_sfd, needed_h)
    if (nrow(dh) < 100L) next
    f <- as.formula(paste(y, "~ d_mean_salinity_lag1 +", paste(c(weather, topo), collapse = " + "), "| Year"))
    mh <- fixest::feols(f, data = dh, vcov = vc, panel.id = ~pair_id + Year, notes = FALSE)
    ct <- as.data.table(fixest::coeftable(mh), keep.rownames = "term")
    r <- ct[term == "d_mean_salinity_lag1"]
    se_col <- intersect(c("Std. Error", "Std..Error", "Std...Error"), names(r))[1]
    p_col <- intersect(c("Pr(>|t|)", "Pr...t..", "Pr(>|z|)", "Pr...z.."), names(r))[1]
    event_rows[[length(event_rows) + 1L]] <- data.table(
      horizon = h,
      estimate = r$Estimate,
      se = r[[se_col]],
      p = r[[p_col]],
      observations = stats::nobs(mh)
    )
  }
  event_dt <- rbindlist(event_rows)
  event_dt[, `:=`(
    ci95_low = estimate - 1.96 * se,
    ci95_high = estimate + 1.96 * se,
    ci90_low = estimate - 1.645 * se,
    ci90_high = estimate + 1.645 * se
  )]
  fwrite(event_dt, file.path(paths$out_dir, "FullRevision_NetAbandonment_EventStudy.csv"))
  fig <- ggplot(event_dt, aes(horizon, estimate)) +
    geom_hline(yintercept = 0, linewidth = 0.35, color = "grey45") +
    geom_vline(xintercept = -0.5, linewidth = 0.3, linetype = "dashed", color = "grey60") +
    geom_linerange(aes(ymin = ci95_low, ymax = ci95_high), color = "#2F6F8F", linewidth = 0.55) +
    geom_linerange(aes(ymin = ci90_low, ymax = ci90_high), color = "#2F6F8F", linewidth = 1.15) +
    geom_point(color = "#B55D2A", size = 2.4) +
    scale_x_continuous(breaks = horizons) +
    labs(x = "Horizon around lagged salinity", y = "Effect on net abandonment rate (p.p.)") +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 13),
      axis.title = element_text(size = 14)
    )
  ggsave(file.path(paths$out_dir, "FullRevision_NetAbandonment_EventStudy.pdf"), fig, width = 6.9, height = 4.1)
  ggsave(file.path(paths$out_dir, "FullRevision_NetAbandonment_EventStudy.png"), fig, width = 6.9, height = 4.1, dpi = 320)
  event_tex <- cbind(
    event_dt[, .(
      Horizon = horizon,
      Coefficient = paste0(fmt(estimate, 4), stars(p)),
      SE = paste0("(", fmt(se, 4), ")"),
      P = fmt(p, 3),
      Observations = formatC(observations, format = "d", big.mark = ",")
    )],
    spec_control_flags(3)[rep(1L, nrow(event_dt))]
  )
  write_latex_df(
    event_tex,
    file.path(paths$out_dir, "FullRevision_NetAbandonment_EventStudy.tex"),
    "Event Study for Net Agricultural Abandonment",
    "tab:full_revision_net_abandonment_event_study",
    note = paste(
      "Each row reports a separate SFD regression of net agricultural abandonment at the indicated horizon on one-year lagged mean salinity.",
      "Negative horizons are timing/placebo estimates.",
      "All models include lagged GDD, lagged KDD, lagged soil moisture, elevation, slope, clay, year fixed effects and Conley spatial standard errors with a 200 km cutoff."
    ),
    size = "\\scriptsize"
  )
  invisible(list(
    data = d,
    models = list(net = list(m_net1, m_net2, m_net3), spell = list(m_spell1, m_spell2, m_spell3)),
    event = event_dt,
    diagnostics = match_diag
  ))
}

# =============================================================================
# 7. Agricultural input analyses from census and municipal data
# =============================================================================
make_irrigation_sfd_analysis <- function() {
  write_status("Estimating 2006 irrigation input associations with SFD.")
  input_file <- file.path(paths$project_dir, "MT2", "research_inputs", "input_irrigation_2006_municipal.csv")
  if (!file.exists(input_file)) {
    write_status("Skipping irrigation SFD: input_irrigation_2006_municipal.csv is missing.")
    return(NULL)
  }
  irr <- fread(input_file, encoding = "UTF-8", colClasses = list(character = "Code"))
  irr[, Code := trimws(as.character(Code))]
  num_vars <- intersect(
    c(
      "irrigated_area_ha_2006", "any_irrigation_2006",
      "irrigation_share_selected_crops_2006", "asinh_irrigated_area_2006",
      "mean_salinity_2006", "excess_salinity_2006", "gdd_2006", "kdd_2006",
      "soil_moisture_2006", "elevation", "slope", "clay"
    ),
    names(irr)
  )
  for (v in num_vars) irr[, (v) := as_number(get(v))]
  irr[, sample_year := "2006"]

  pair_map <- read_pair_map()
  vars <- intersect(
    c(
      "any_irrigation_2006", "asinh_irrigated_area_2006",
      "irrigation_share_selected_crops_2006", "mean_salinity_2006",
      "gdd_2006", "kdd_2006", "soil_moisture_2006",
      "elevation", "slope", "clay"
    ),
    names(irr)
  )
  sfd <- make_sfd_from_unit_panel(
    irr[, c("Code", "sample_year", vars), with = FALSE],
    pair_map,
    id_cols = "sample_year",
    vars = vars
  )
  weather <- c("d_gdd_2006", "d_kdd_2006", "d_soil_moisture_2006")
  topo <- c("d_elevation", "d_slope", "d_clay")
  outcomes <- c(
    d_any_irrigation_2006 = "Any irrigation",
    d_asinh_irrigated_area_2006 = "Asinh irrigated area",
    d_irrigation_share_selected_crops_2006 = "Irrigated-area share"
  )
  needed <- c(names(outcomes), "d_mean_salinity_2006", weather, topo, "state", "lat", "lon", "pair_id")
  d <- complete_data(sfd, needed)
  if (nrow(d) < 100L) {
    write_status("Skipping irrigation SFD: fewer than 100 complete SFD observations.")
    return(NULL)
  }

  vc <- make_conley()
  models <- list()
  table_headers_outcome <- character()
  table_headers_spec <- character()
  for (dep in names(outcomes)) {
    f1 <- as.formula(paste(dep, "~ d_mean_salinity_2006 | state"))
    f2 <- as.formula(paste(dep, "~ d_mean_salinity_2006 +", paste(weather, collapse = " + "), "| state"))
    f3 <- as.formula(paste(dep, "~ d_mean_salinity_2006 +", paste(c(weather, topo), collapse = " + "), "| state"))
    m1 <- fixest::feols(f1, data = d, vcov = vc, notes = FALSE)
    m2 <- fixest::feols(f2, data = d, vcov = vc, notes = FALSE)
    m3 <- fixest::feols(f3, data = d, vcov = vc, notes = FALSE)
    models[[paste(outcomes[[dep]], "Spec. 1", sep = " - ")]] <- m1
    models[[paste(outcomes[[dep]], "Spec. 2", sep = " - ")]] <- m2
    models[[paste(outcomes[[dep]], "Spec. 3", sep = " - ")]] <- m3
    table_headers_outcome <- c(table_headers_outcome, rep(outcomes[[dep]], 3L))
    table_headers_spec <- c(table_headers_spec, paste0("Spec. ", 1:3))
  }

  fixest::setFixest_dict(c(
    d_mean_salinity_2006 = "$\\Delta_s$ Mean salinity, 2006 (dS/m)",
    d_gdd_2006 = "$\\Delta_s$ GDD, 2006",
    d_kdd_2006 = "$\\Delta_s$ KDD, 2006",
    d_soil_moisture_2006 = "$\\Delta_s$ Soil moisture, 2006",
    d_elevation = "$\\Delta_s$ Elevation",
    d_slope = "$\\Delta_s$ Slope",
    d_clay = "$\\Delta_s$ Clay"
  ), reset = TRUE)
  tex_file <- file.path(paths$out_dir, "FullRevision_Irrigation_2006_SFD.tex")
  fixest::etable(
    models,
    tex = TRUE,
    file = tex_file,
    replace = TRUE,
    headers = list("Outcome" = table_headers_outcome, "Spec" = table_headers_spec),
    depvar = FALSE,
    title = "Mean Salinity and Irrigation Inputs in 2006",
    label = "tab:full_revision_irrigation_sfd",
    fitstat = ~ r2 + n,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    notes = paste(
      "Notes: Cross-sectional SFD regressions using the 2006 Agricultural Census irrigation measures.",
      "The dependent variables are east-minus-west differences in: an indicator for any irrigation, asinh irrigated area, and irrigated area as a share of selected-crop planted area.",
      "The treatment is the difference in mean salinity in 2006.",
      "Spec. 1 includes state fixed effects; Spec. 2 adds GDD, KDD and soil moisture; Spec. 3 additionally adds elevation, slope and clay.",
      "All columns use the same complete-data sample. Standard errors are Conley spatial with a 200 km cutoff.",
      "The table is exploratory because irrigation can be both an adaptation to salinity and a cause of salinization."
    )
  )
  style_fixest_tex(tex_file, size = "\\tiny", tabcolsep = "2pt", arraystretch = "0.62", resize = TRUE, landscape = TRUE)
  invisible(list(data = d, models = models))
}

sidra_state_codes <- function() {
  c(
    "11", "12", "13", "14", "15", "16", "17",
    "21", "22", "23", "24", "25", "26", "27", "28", "29",
    "31", "32", "33", "35",
    "41", "42", "43",
    "50", "51", "52", "53"
  )
}

sidra_input_specs_2017 <- function() {
  list(
    list(
      key = "adubacao_2017_6847",
      table = 6847,
      variable = "183",
      classific = c("829", "12522", "12564", "12771", "800", "837"),
      category = list("46302", c("46545", "46546", "46547", "46548"), "41145", "45951", "41147", "46544"),
      category_pattern = "Uso de adubacao",
      source_note = "IBGE SIDRA table 6847, 2017 Agricultural Census: establishment counts by fertilizer/adubation use."
    ),
    list(
      key = "calcario_2017_6849",
      table = 6849,
      variable = "183",
      classific = c("829", "12549", "12564", "12771", "218", "800"),
      category = list("46302", c("46553", "46554"), "41145", "45951", "46502", "41147"),
      category_pattern = "Uso de calcario",
      source_note = "IBGE SIDRA table 6849, 2017 Agricultural Census: establishment counts by lime and other soil-pH corrective use."
    ),
    list(
      key = "agrotoxicos_2017_6851",
      table = 6851,
      variable = "183",
      classific = c("829", "12521", "12564", "218", "800", "12598"),
      category = list("46302", c("46556", "111611"), "41145", "46502", "41147", "41141"),
      category_pattern = "Uso de agrotoxicos",
      source_note = "IBGE SIDRA table 6851, 2017 Agricultural Census: establishment counts by pesticide use."
    ),
    list(
      key = "irrigacao_2017_6859",
      table = 6859,
      variable = c("2372", "2373"),
      classific = c("829", "12604", "12603", "220"),
      category = list("46302", "118477", "45927", "110085"),
      category_pattern = "Metodo utilizado para irrigacao",
      source_note = "IBGE SIDRA table 6859, 2017 Agricultural Census: irrigated establishments and irrigated area."
    ),
    list(
      key = "tratores_2017_6869",
      table = 6869,
      variable = c("1918", "1862"),
      classific = c("829", "12605", "12564", "12771"),
      category = list("46302", "113521", "41145", "45951"),
      category_pattern = "Potencia dos tratores",
      source_note = "IBGE SIDRA table 6869, 2017 Agricultural Census: tractor access and tractor stock."
    ),
    list(
      key = "maquinas_2017_6872",
      table = 6872,
      variable = "9572",
      classific = c("829", "796", "12564", "12771"),
      category = list("46302", c("46567", "40600"), "41145", "45951"),
      category_pattern = "Tratores, implementos",
      source_note = "IBGE SIDRA table 6872, 2017 Agricultural Census: machinery and fertilizer-spreader stock."
    ),
    list(
      key = "despesas_2017_6900",
      table = 6900,
      variable = c("2", "1996"),
      classific = c("829", "210", "220"),
      category = list("46302", c("113946", "45953", "111962", "111966", "5497", "45955", "45956"), "110085"),
      category_pattern = "Tipo de despesa",
      source_note = "IBGE SIDRA table 6900, 2017 Agricultural Census: establishment counts and values of agricultural expenses by expense type."
    )
  )
}

find_sidra_column <- function(dt, pattern, code = TRUE) {
  nm <- names(dt)
  nm_ascii <- stringi::stri_trans_general(nm, "Latin-ASCII")
  ok <- grepl(pattern, nm_ascii, ignore.case = TRUE)
  if (code) {
    ok <- ok & grepl("\\(Codigo\\)", nm_ascii, ignore.case = TRUE)
  } else {
    ok <- ok & !grepl("\\(Codigo\\)", nm_ascii, ignore.case = TRUE)
  }
  hit <- which(ok)
  if (length(hit) == 0L) return(NA_character_)
  nm[hit[1]]
}

sidra_pair_sample_codes <- function(chunk_size = 100L) {
  pair_map <- read_pair_map()
  codes <- unique(c(as.character(pair_map$Code), as.character(pair_map$code_neighbor_west)))
  codes <- sort(codes[grepl("^[0-9]{7}$", codes)])
  split(codes, ceiling(seq_along(codes) / chunk_size))
}

download_sidra_input_spec <- function(spec, force = FALSE, timeout_sec = 60, chunk_size = 100L) {
  ensure_output_dir()
  out_file <- file.path(paths$sidra_dir, paste0(spec$key, "_n", chunk_size, ".csv"))
  if (!force && file.exists(out_file) && file.info(out_file)$size > 0) {
    return(fread(out_file, encoding = "UTF-8", colClasses = list(character = c("chunk_id", "state_code"))))
  }

  old_timeout <- getOption("sidrar.timeout")
  old_retries <- getOption("sidrar.retries")
  on.exit({
    options(sidrar.timeout = old_timeout)
    options(sidrar.retries = old_retries)
  }, add = TRUE)
  options(sidrar.timeout = timeout_sec, sidrar.retries = 1)

  code_chunks <- sidra_pair_sample_codes(chunk_size = chunk_size)
  fetch_codes <- function(codes, chunk_id) {
    chunk_file <- file.path(paths$sidra_dir, paste0(spec$key, "_n", chunk_size, "_chunk_", chunk_id, ".csv"))
    if (!force && file.exists(chunk_file) && file.info(chunk_file)$size > 0) {
      return(fread(chunk_file, encoding = "UTF-8", colClasses = list(character = c("chunk_id", "state_code"))))
    }
    write_status(
      "Downloading SIDRA input table ", spec$table,
      " for municipality chunk ", chunk_id,
      " (", length(codes), " municipalities)."
    )
    z <- try(
      sidrar::get_sidra(
        x = spec$table,
        variable = as.character(spec$variable),
        period = "2017",
        geo = "City",
        geo.filter = list(City = codes),
        classific = spec$classific,
        category = spec$category,
        value_type = "both"
      ),
      silent = TRUE
    )
    if (inherits(z, "try-error")) {
      write_status("SIDRA table ", spec$table, " failed for chunk ", chunk_id, ": ", as.character(z))
      if (length(codes) > 1L) {
        mid <- ceiling(length(codes) / 2)
        left <- fetch_codes(codes[seq_len(mid)], paste0(chunk_id, "a"))
        right <- fetch_codes(codes[(mid + 1L):length(codes)], paste0(chunk_id, "b"))
        return(rbindlist(list(left, right), fill = TRUE))
      }
      write_status("SIDRA table ", spec$table, " skipped municipality ", codes[1], " after singleton failure.")
      return(NULL)
    }
    z <- as.data.table(z)
    z[, `:=`(
      dataset_key = spec$key,
      sidra_table = spec$table,
      chunk_id = chunk_id,
      state_code = substr(codes[1], 1, 2)
    )]
    fwrite(z, chunk_file)
    Sys.sleep(0.15)
    z
  }

  rows <- vector("list", length(code_chunks))
  names(rows) <- names(code_chunks)
  for (chunk_id in names(code_chunks)) {
    rows[[chunk_id]] <- fetch_codes(code_chunks[[chunk_id]], chunk_id)
  }

  out <- rbindlist(rows, fill = TRUE)
  if (nrow(out) > 0L) {
    fwrite(out, out_file)
  }
  out
}

tidy_sidra_input <- function(raw, spec) {
  if (is.null(raw) || nrow(raw) == 0L) return(NULL)
  raw <- as.data.table(raw)
  code_col <- find_sidra_column(raw, "Municipio", code = TRUE)
  var_col <- find_sidra_column(raw, "Variavel", code = TRUE)
  cat_col <- find_sidra_column(raw, spec$category_pattern, code = TRUE)
  if (anyNA(c(code_col, var_col, cat_col)) || !"Valor" %in% names(raw)) {
    write_status("Could not parse SIDRA columns for ", spec$key, ".")
    return(NULL)
  }
  raw_col <- if ("Valor_raw" %in% names(raw)) "Valor_raw" else "Valor"
  out <- raw[, .(
    dataset_key = spec$key,
    sidra_table = as.integer(spec$table),
    Code = trimws(as.character(get(code_col))),
    state_code = trimws(as.character(state_code)),
    variable_code = trimws(as.character(get(var_col))),
    category_code = trimws(as.character(get(cat_col))),
    value = as_number(get("Valor")),
    value_raw = trimws(as.character(get(raw_col)))
  )]
  out[grepl("^[0-9]{6}$", Code), Code := paste0("0", Code)]
  out
}

make_municipal_environment_2017 <- function(pam = NULL) {
  if (is.null(pam)) pam <- read_pam_zeros()
  setDT(pam)
  pam[, Code := trimws(as.character(Code))]
  if (!"mean_salinity" %in% names(pam)) {
    sal_source <- if ("salinity_mean_large" %in% names(pam)) "salinity_mean_large" else "mean_salinity"
    sal_median <- stats::median(pam[[sal_source]], na.rm = TRUE)
    sal_scale <- if (is.finite(sal_median) && sal_median > 20) 100 else 1
    pam[, mean_salinity := get(sal_source) / sal_scale]
  }
  area_var <- if ("planted_area" %in% names(pam)) "planted_area" else "total_planted_area"
  pam[Year == 2017, .(
    mean_salinity_2017 = weighted_mean_safe(mean_salinity, get(area_var)),
    gdd_2017 = weighted_mean_safe(gdd_large, get(area_var)),
    kdd_2017 = weighted_mean_safe(kdd_large, get(area_var)),
    soil_moisture_2017 = weighted_mean_safe(sm_season_large, get(area_var)),
    elevation = weighted_mean_safe(elevation_large, get(area_var)),
    slope = weighted_mean_safe(slope_large, get(area_var)),
    clay = weighted_mean_safe(clay_mean_large, get(area_var)),
    selected_crop_planted_area_2017 = sum(get(area_var), na.rm = TRUE)
  ), by = Code]
}

make_2017_input_panel <- function(force_download = FALSE) {
  specs <- sidra_input_specs_2017()
  raw_list <- lapply(specs, function(spec) download_sidra_input_spec(spec, force = force_download))
  tidy_list <- Map(tidy_sidra_input, raw_list, specs)
  long <- rbindlist(tidy_list, fill = TRUE)
  if (nrow(long) == 0L) {
    write_status("Skipping 2017 input panel: no SIDRA input table could be parsed.")
    return(NULL)
  }
  fwrite(long, file.path(paths$out_dir, "FullRevision_Inputs_2017_SIDRA_Long.csv"))

  cast_one <- function(key) {
    d <- long[dataset_key == key]
    if (nrow(d) == 0L) return(NULL)
    dcast(
      d,
      Code ~ variable_code + category_code,
      value.var = "value",
      fun.aggregate = function(x) x[which(!is.na(x))[1]],
      fill = NA_real_
    )
  }

  adub <- cast_one("adubacao_2017_6847")
  calc <- cast_one("calcario_2017_6849")
  agro <- cast_one("agrotoxicos_2017_6851")
  org <- cast_one("organica_2017_6853")
  irr <- cast_one("irrigacao_2017_6859")
  tr <- cast_one("tratores_2017_6869")
  mach <- cast_one("maquinas_2017_6872")
  exp <- cast_one("despesas_2017_6900")

  rename_if_present <- function(dt, old, new) {
    if (!is.null(dt) && old %in% names(dt)) setnames(dt, old, new)
    dt
  }
  adub <- rename_if_present(adub, "183_46545", "establishments_adub_total")
  adub <- rename_if_present(adub, "183_46546", "establishments_adub_any")
  adub <- rename_if_present(adub, "183_46547", "establishments_adub_chemical")
  adub <- rename_if_present(adub, "183_46548", "establishments_adub_organic")
  calc <- rename_if_present(calc, "183_46553", "establishments_calc_total")
  calc <- rename_if_present(calc, "183_46554", "establishments_calc_applied")
  agro <- rename_if_present(agro, "183_46556", "establishments_agrotox_total")
  agro <- rename_if_present(agro, "183_111611", "establishments_agrotox_used")
  org <- rename_if_present(org, "183_46559", "establishments_organic_total")
  org <- rename_if_present(org, "183_47123", "establishments_organic_yes")
  irr <- rename_if_present(irr, "2372_118477", "irrigated_establishments")
  irr <- rename_if_present(irr, "2373_118477", "irrigated_area_ha")
  tr <- rename_if_present(tr, "1918_113521", "tractor_establishments")
  tr <- rename_if_present(tr, "1862_113521", "tractors_total")
  mach <- rename_if_present(mach, "9572_46567", "machines_total")
  mach <- rename_if_present(mach, "9572_40598", "seeders_planters_total")
  mach <- rename_if_present(mach, "9572_40599", "harvesters_total")
  mach <- rename_if_present(mach, "9572_40600", "fertilizer_spreaders_total")
  exp <- rename_if_present(exp, "2_113946", "expense_establishments_total")
  exp <- rename_if_present(exp, "1996_113946", "expenses_total_mil_reais")
  exp <- rename_if_present(exp, "2_45953", "fertilizer_corrective_expense_establishments")
  exp <- rename_if_present(exp, "1996_45953", "fertilizer_corrective_expenses_mil_reais")
  exp <- rename_if_present(exp, "2_111962", "seed_expense_establishments")
  exp <- rename_if_present(exp, "1996_111962", "seed_expenses_mil_reais")
  exp <- rename_if_present(exp, "2_111966", "pesticide_expense_establishments")
  exp <- rename_if_present(exp, "1996_111966", "pesticide_expenses_mil_reais")
  exp <- rename_if_present(exp, "2_5497", "energy_expense_establishments")
  exp <- rename_if_present(exp, "1996_5497", "energy_expenses_mil_reais")
  exp <- rename_if_present(exp, "2_45955", "machinery_vehicle_expense_establishments")
  exp <- rename_if_present(exp, "1996_45955", "machinery_vehicle_expenses_mil_reais")
  exp <- rename_if_present(exp, "2_45956", "fuel_lubricant_expense_establishments")
  exp <- rename_if_present(exp, "1996_45956", "fuel_lubricant_expenses_mil_reais")

  tables <- Filter(Negate(is.null), list(adub, calc, agro, org, irr, tr, mach, exp))
  panel <- Reduce(function(x, y) merge(x, y, by = "Code", all = TRUE), tables)
  total_candidates <- intersect(
    c(
      "establishments_adub_total", "establishments_calc_total",
      "establishments_agrotox_total", "establishments_organic_total"
    ),
    names(panel)
  )
  if (length(total_candidates) == 0L) {
    write_status("Skipping 2017 input panel: no total-establishment denominator was downloaded.")
    return(NULL)
  }
  panel[, total_establishments_2017 := do.call(fcoalesce, .SD), .SDcols = total_candidates]

  col_or_na <- function(name) {
    if (name %in% names(panel)) return(panel[[name]])
    rep(NA_real_, nrow(panel))
  }
  share_of_establishments <- function(x) {
    fifelse(
      is.finite(x) & is.finite(panel$total_establishments_2017) & panel$total_establishments_2017 > 0,
      100 * x / panel$total_establishments_2017,
      NA_real_
    )
  }
  per_100_establishments <- share_of_establishments
  share_of_expenses <- function(x) {
    total_exp <- col_or_na("expenses_total_mil_reais")
    fifelse(
      is.finite(x) & is.finite(total_exp) & total_exp > 0,
      100 * x / total_exp,
      NA_real_
    )
  }

  panel[, `:=`(
    fertilizer_any_share_2017 = share_of_establishments(col_or_na("establishments_adub_any")),
    chemical_fertilizer_share_2017 = share_of_establishments(col_or_na("establishments_adub_chemical")),
    organic_fertilizer_share_2017 = share_of_establishments(col_or_na("establishments_adub_organic")),
    soil_ph_corrective_share_2017 = share_of_establishments(col_or_na("establishments_calc_applied")),
    pesticide_use_share_2017 = share_of_establishments(col_or_na("establishments_agrotox_used")),
    irrigation_establishment_share_2017 = share_of_establishments(col_or_na("irrigated_establishments")),
    asinh_irrigated_area_2017 = fifelse(is.finite(col_or_na("irrigated_area_ha")), asinh(col_or_na("irrigated_area_ha")), NA_real_),
    tractor_establishment_share_2017 = share_of_establishments(col_or_na("tractor_establishments")),
    tractors_per_100_establishments_2017 = per_100_establishments(col_or_na("tractors_total")),
    fertilizer_spreaders_per_100_establishments_2017 = per_100_establishments(col_or_na("fertilizer_spreaders_total")),
    fertilizer_corrective_expense_share_2017 = share_of_expenses(col_or_na("fertilizer_corrective_expenses_mil_reais")),
    seed_expense_share_2017 = share_of_expenses(col_or_na("seed_expenses_mil_reais")),
    pesticide_expense_share_2017 = share_of_expenses(col_or_na("pesticide_expenses_mil_reais")),
    energy_expense_share_2017 = share_of_expenses(col_or_na("energy_expenses_mil_reais")),
    machinery_vehicle_expense_share_2017 = share_of_expenses(col_or_na("machinery_vehicle_expenses_mil_reais")),
    fuel_lubricant_expense_share_2017 = share_of_expenses(col_or_na("fuel_lubricant_expenses_mil_reais"))
  )]

  env2017 <- make_municipal_environment_2017()
  panel <- merge(panel, env2017, by = "Code", all.x = TRUE)
  fwrite(panel, file.path(paths$out_dir, "FullRevision_Inputs_2017_Municipal.csv"))
  panel
}

input_2017_outcome_labels <- function() {
  c(
    fertilizer_any_share_2017 = "Any fertilizer/adubation share",
    chemical_fertilizer_share_2017 = "Chemical fertilizer share",
    organic_fertilizer_share_2017 = "Organic fertilizer share",
    soil_ph_corrective_share_2017 = "Lime or soil-pH corrective share",
    pesticide_use_share_2017 = "Pesticide-use share",
    irrigation_establishment_share_2017 = "Irrigated-establishment share",
    asinh_irrigated_area_2017 = "Asinh irrigated area",
    tractor_establishment_share_2017 = "Tractor-establishment share",
    tractors_per_100_establishments_2017 = "Tractors per 100 establishments",
    fertilizer_spreaders_per_100_establishments_2017 = "Fertilizer spreaders per 100 establishments",
    fertilizer_corrective_expense_share_2017 = "Fertilizer/corrective expense share",
    seed_expense_share_2017 = "Seed and seedling expense share",
    pesticide_expense_share_2017 = "Pesticide expense share",
    energy_expense_share_2017 = "Electricity expense share",
    machinery_vehicle_expense_share_2017 = "Machinery and vehicle expense share",
    fuel_lubricant_expense_share_2017 = "Fuel and lubricant expense share"
  )
}

write_input_2017_descriptives <- function(panel, labels) {
  desc <- rbindlist(lapply(names(labels), function(v) {
    x <- panel[[v]]
    data.table(
      Input = labels[[v]],
      Mean = fmt(mean(x, na.rm = TRUE), 2),
      SD = fmt(stats::sd(x, na.rm = TRUE), 2),
      Min = fmt(min(x, na.rm = TRUE), 2),
      Max = fmt(max(x, na.rm = TRUE), 2),
      N = formatC(sum(is.finite(x)), format = "d", big.mark = ",")
    )
  }))
  write_latex_df(
    desc,
    file.path(paths$out_dir, "FullRevision_Inputs_2017_Descriptives.tex"),
    "2017 Agricultural Census Input Measures",
    "tab:full_revision_inputs_2017_descriptives",
    note = paste(
      "Municipality-level measures are constructed from IBGE/SIDRA Agricultural Census tables 6847, 6849, 6851, 6859, 6869, 6872 and 6900.",
      "Establishment shares are percentages of agricultural establishments. Machinery variables are expressed per 100 establishments.",
      "Expense shares use total agricultural expenses in 2017 as the denominator and are therefore cross-sectional input-mix measures.",
      "Suppressed SIDRA values are kept as missing, not as zeros."
    ),
    size = "\\small",
    landscape = FALSE
  )
  fwrite(desc, file.path(paths$out_dir, "FullRevision_Inputs_2017_Descriptives.csv"))
  desc
}

write_input_2017_fixest_table <- function(models, labels, file, title, label, note) {
  headers_outcome <- rep(unname(labels), each = 3L)
  headers_spec <- rep(paste0("Spec. ", 1:3), times = length(labels))
  fixest::etable(
    models,
    tex = TRUE,
    file = file,
    replace = TRUE,
    headers = list("Outcome" = headers_outcome, "Spec" = headers_spec),
    depvar = FALSE,
    title = title,
    label = label,
    fitstat = ~ r2 + n,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    notes = note
  )
  style_fixest_tex(file, size = "\\tiny", tabcolsep = "1.4pt", arraystretch = "0.58", resize = TRUE, landscape = TRUE)
}

make_2017_input_sfd_analysis <- function(force_download = FALSE) {
  write_status("Building 2017 SIDRA input measures and SFD mechanism regressions.")
  panel <- make_2017_input_panel(force_download = force_download)
  if (is.null(panel) || nrow(panel) == 0L) return(NULL)

  labels <- input_2017_outcome_labels()
  labels <- labels[names(labels) %in% names(panel)]
  write_input_2017_descriptives(panel, labels)

  pair_map <- read_pair_map()
  panel[, sample_year := "2017"]
  controls <- c(
    "mean_salinity_2017", "gdd_2017", "kdd_2017",
    "soil_moisture_2017", "elevation", "slope", "clay"
  )
  vars <- c(names(labels), controls)
  sfd <- make_sfd_from_unit_panel(
    panel[, c("Code", "sample_year", vars), with = FALSE],
    pair_map,
    id_cols = "sample_year",
    vars = vars
  )
  weather <- c("d_gdd_2017", "d_kdd_2017", "d_soil_moisture_2017")
  topo <- c("d_elevation", "d_slope", "d_clay")
  complete_sfd_for <- function(outcome_names) {
    needed <- c(paste0("d_", outcome_names), "d_mean_salinity_2017", weather, topo, "state", "lat", "lon", "pair_id")
    complete_data(sfd, needed)
  }

  vc <- make_conley()
  fixest::setFixest_dict(c(
    d_mean_salinity_2017 = "$\\Delta_s$ Mean salinity, 2017 (dS/m)",
    d_gdd_2017 = "$\\Delta_s$ GDD, 2017",
    d_kdd_2017 = "$\\Delta_s$ KDD, 2017",
    d_soil_moisture_2017 = "$\\Delta_s$ Soil moisture, 2017",
    d_elevation = "$\\Delta_s$ Elevation",
    d_slope = "$\\Delta_s$ Slope",
    d_clay = "$\\Delta_s$ Clay"
  ), reset = TRUE)

  make_models <- function(outcome_names, data) {
    models <- list()
    for (dep0 in outcome_names) {
      dep <- paste0("d_", dep0)
      f1 <- as.formula(paste(dep, "~ d_mean_salinity_2017 | state"))
      f2 <- as.formula(paste(dep, "~ d_mean_salinity_2017 +", paste(weather, collapse = " + "), "| state"))
      f3 <- as.formula(paste(dep, "~ d_mean_salinity_2017 +", paste(c(weather, topo), collapse = " + "), "| state"))
      models[[paste(labels[[dep0]], "Spec. 1", sep = " - ")]] <- fixest::feols(f1, data = data, vcov = vc, notes = FALSE)
      models[[paste(labels[[dep0]], "Spec. 2", sep = " - ")]] <- fixest::feols(f2, data = data, vcov = vc, notes = FALSE)
      models[[paste(labels[[dep0]], "Spec. 3", sep = " - ")]] <- fixest::feols(f3, data = data, vcov = vc, notes = FALSE)
    }
    models
  }

  agchem_outcomes <- intersect(
    c(
      "fertilizer_any_share_2017", "chemical_fertilizer_share_2017",
      "organic_fertilizer_share_2017", "soil_ph_corrective_share_2017",
      "pesticide_use_share_2017"
    ),
    names(labels)
  )
  expense_outcomes <- intersect(
    c(
      "fertilizer_corrective_expense_share_2017", "seed_expense_share_2017",
      "pesticide_expense_share_2017", "energy_expense_share_2017",
      "machinery_vehicle_expense_share_2017", "fuel_lubricant_expense_share_2017"
    ),
    names(labels)
  )
  water_capital_outcomes <- setdiff(names(labels), c(agchem_outcomes, expense_outcomes))
  group_outcomes <- list(
    agchem = agchem_outcomes,
    water_capital = water_capital_outcomes,
    expense = expense_outcomes
  )
  group_data <- lapply(group_outcomes, function(x) {
    if (length(x) == 0L) return(NULL)
    complete_sfd_for(x)
  })
  small_groups <- names(group_data)[vapply(group_data, function(x) is.null(x) || nrow(x) < 100L, logical(1))]
  if (length(small_groups) == length(group_data)) {
    write_status("Skipping 2017 input SFD regressions: fewer than 100 complete observations in all input groups.")
    return(list(panel = panel, sfd = sfd))
  }
  agchem_models <- if (!is.null(group_data$agchem) && nrow(group_data$agchem) >= 100L) make_models(agchem_outcomes, group_data$agchem) else list()
  water_capital_models <- if (!is.null(group_data$water_capital) && nrow(group_data$water_capital) >= 100L) make_models(water_capital_outcomes, group_data$water_capital) else list()
  expense_models <- if (!is.null(group_data$expense) && nrow(group_data$expense) >= 100L) make_models(expense_outcomes, group_data$expense) else list()

  if (length(agchem_models) > 0L) {
    write_input_2017_fixest_table(
      agchem_models,
      labels[agchem_outcomes],
      file.path(paths$out_dir, "FullRevision_Inputs_2017_SFD_Agrochemical_Practices.tex"),
      "Mean Salinity and Agrochemical Input Practices in 2017",
      "tab:full_revision_inputs_2017_agrochemical_sfd",
      paste(
        "Notes: Cross-sectional SFD regressions using municipality-level 2017 Agricultural Census input measures.",
        "Dependent variables are east-minus-west differences in input-use shares.",
        "Spec. 1 includes state fixed effects; Spec. 2 adds GDD, KDD and soil moisture; Spec. 3 additionally adds elevation, slope and clay.",
        "All columns use the same complete-data SFD sample. Standard errors are Conley spatial with a 200 km cutoff.",
        "The estimates are exploratory mechanism tests, because input use can be an adaptation to salinity and can also affect salinization."
      )
    )
  }
  if (length(water_capital_models) > 0L) {
    write_input_2017_fixest_table(
      water_capital_models,
      labels[water_capital_outcomes],
      file.path(paths$out_dir, "FullRevision_Inputs_2017_SFD_Water_Capital.tex"),
      "Mean Salinity and Water/Capital Inputs in 2017",
      "tab:full_revision_inputs_2017_water_capital_sfd",
      paste(
        "Notes: Cross-sectional SFD regressions using municipality-level 2017 Agricultural Census input measures.",
        "Dependent variables are east-minus-west differences in irrigation, tractor access and machinery intensity.",
        "Spec. 1 includes state fixed effects; Spec. 2 adds GDD, KDD and soil moisture; Spec. 3 additionally adds elevation, slope and clay.",
        "All columns use the same complete-data SFD sample. Standard errors are Conley spatial with a 200 km cutoff.",
        "The estimates are exploratory mechanism tests, not additional controls in the yield equation."
      )
    )
  }
  if (length(expense_models) > 0L) {
    write_input_2017_fixest_table(
      expense_models,
      labels[expense_outcomes],
      file.path(paths$out_dir, "FullRevision_Inputs_2017_SFD_Expense_Composition.tex"),
      "Mean Salinity and Agricultural Input-Expense Composition in 2017",
      "tab:full_revision_inputs_2017_expense_sfd",
      paste(
        "Notes: Cross-sectional SFD regressions using municipality-level 2017 Agricultural Census input-expense measures from SIDRA table 6900.",
        "Dependent variables are east-minus-west differences in each expense category's share of total agricultural expenses.",
        "Spec. 1 includes state fixed effects; Spec. 2 adds GDD, KDD and soil moisture; Spec. 3 additionally adds elevation, slope and clay.",
        "All columns use the same complete-data SFD sample. Standard errors are Conley spatial with a 200 km cutoff.",
        "Because these outcomes are expenditure shares in one census year, they are used as mechanism evidence, not as controls in the yield equation."
      )
    )
  }

  coef_dt <- rbindlist(lapply(names(group_outcomes), function(group_name) {
    outcome_names <- group_outcomes[[group_name]]
    data_i <- group_data[[group_name]]
    if (length(outcome_names) == 0L || is.null(data_i) || nrow(data_i) < 100L) return(NULL)
    rbindlist(lapply(outcome_names, function(dep0) {
      models_dep <- make_models(dep0, data_i)
      rbindlist(lapply(seq_along(models_dep), function(i) {
        row <- coef_numeric_row(models_dep[[i]], "d_mean_salinity_2017", label = paste0("Spec. ", i))
        row[, `:=`(
          outcome = dep0,
          outcome_label = labels[[dep0]],
          specification = paste0("Spec. ", i),
          sample_group = group_name
        )]
        row
      }), fill = TRUE)
    }), fill = TRUE)
  }), fill = TRUE)
  coef_dt[, `:=`(
    ci95_low = estimate - 1.96 * se,
    ci95_high = estimate + 1.96 * se,
    ci90_low = estimate - 1.645 * se,
    ci90_high = estimate + 1.645 * se
  )]
  fwrite(coef_dt, file.path(paths$out_dir, "FullRevision_Inputs_2017_SFD_Coefficients.csv"))

  outcome_order <- rev(unname(labels[c(agchem_outcomes, water_capital_outcomes, expense_outcomes)]))
  offsets <- c("Spec. 1" = -0.22, "Spec. 2" = 0, "Spec. 3" = 0.22)
  coef_dt[, specification := factor(specification, levels = names(offsets))]
  coef_dt[, outcome_label := factor(outcome_label, levels = outcome_order)]
  coef_dt[, outcome_position := as.numeric(outcome_label)]
  coef_dt[, plot_position := outcome_position + offsets[as.character(specification)]]
  pal <- c("Spec. 1" = "#1B9E77", "Spec. 2" = "#D95F02", "Spec. 3" = "#7570B3")
  shapes <- c("Spec. 1" = 16, "Spec. 2" = 17, "Spec. 3" = 15)
  fig <- ggplot(coef_dt, aes(x = estimate, y = plot_position, color = specification, shape = specification)) +
    geom_segment(aes(x = ci95_low, xend = ci95_high, yend = plot_position), linewidth = 1.0, alpha = 0.55, lineend = "butt") +
    geom_segment(aes(x = ci90_low, xend = ci90_high, yend = plot_position), linewidth = 2.8, lineend = "butt") +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.85, color = "grey20") +
    geom_point(size = 6.2, stroke = 1.1) +
    scale_y_continuous(
      breaks = seq_along(outcome_order),
      labels = outcome_order,
      expand = expansion(mult = c(0.04, 0.04))
    ) +
    scale_color_manual(values = pal, guide = "none") +
    scale_shape_manual(values = shapes, guide = "none") +
    labs(x = "Effect of a 1 dS/m increase in east-minus-west mean salinity", y = NULL) +
    theme_minimal(base_size = 27) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.65, color = "grey88"),
      panel.grid.major.x = element_line(linewidth = 0.65, color = "grey88"),
      axis.text.y = element_text(size = 29, face = "bold", color = "grey30"),
      axis.text.x = element_text(size = 24, color = "grey30"),
      axis.title.x = element_text(size = 26, margin = margin(t = 14)),
      plot.margin = margin(14, 18, 14, 14)
    )
  ggsave(file.path(paths$out_dir, "FullRevision_Inputs_2017_SFD_Coefficients.pdf"), fig, width = 9.1, height = 9.8)
  ggsave(file.path(paths$out_dir, "FullRevision_Inputs_2017_SFD_Coefficients.png"), fig, width = 9.1, height = 9.8, dpi = 320)

  invisible(list(
    panel = panel,
    sfd = sfd,
    group_samples = group_data,
    agchem_models = agchem_models,
    water_capital_models = water_capital_models,
    expense_models = expense_models,
    coefficients = coef_dt
  ))
}

input_panel_sidra_specs <- function() {
  list(
    list(
      key = "fertilizer_2006", year = 2006L, table = 864L, category_field = "D4C",
      suffix = "/v/183/p/2006/c12522/0,114541/c218/0/c12552/0/c12609/0/c12548/0/c12567/0"
    ),
    list(
      key = "pesticide_2006", year = 2006L, table = 910L, category_field = "D4C",
      suffix = "/v/183/p/2006/c12521/0,111611/c12598/0/c12603/0/c220/0"
    ),
    list(
      key = "irrigation_2006", year = 2006L, table = 855L, category_field = "D4C",
      suffix = "/v/2372,2373/p/2006/c12604/118477/c218/0/c12602/113606/c12548/0/c12603/0"
    ),
    list(
      key = "expenses_2006", year = 2006L, table = 833L, category_field = "D4C",
      suffix = "/v/1996/p/2006/c210/113946,111960,111961,111962,111966,5497,118125,111959,116229/c218/0/c12517/113601/c220/0"
    ),
    list(
      key = "labor_2006", year = 2006L, table = 956L, category_field = "D4C",
      suffix = "/v/2379/p/2006/c2/0/c218/0/c12517/113601/c220/0"
    ),
    list(
      key = "fertilizer_2017", year = 2017L, table = 6847L, category_field = "D5C",
      suffix = "/v/183/p/2017/c829/46302/c12522/46545,46546/c12564/41145/c12771/45951/c800/41147/c837/46544"
    ),
    list(
      key = "ph_corrective_2017", year = 2017L, table = 6849L, category_field = "D5C",
      suffix = "/v/183/p/2017/c829/46302/c12549/46553,46554/c12564/41145/c12771/45951/c218/46502/c800/41147"
    ),
    list(
      key = "pesticide_2017", year = 2017L, table = 6851L, category_field = "D5C",
      suffix = "/v/183/p/2017/c829/46302/c12521/46556,111611/c12564/41145/c218/46502/c800/41147/c12598/41141"
    ),
    list(
      key = "irrigation_2017", year = 2017L, table = 6859L, category_field = "D5C",
      suffix = "/v/2372,2373/p/2017/c829/46302/c12604/118477/c12603/45927/c220/110085"
    ),
    list(
      key = "expenses_2017", year = 2017L, table = 6900L, category_field = "D5C",
      suffix = "/v/1996/p/2017/c829/46302/c210/113946,45953,111962,111966,5497,45956,45959/c220/110085"
    ),
    list(
      key = "wage_expense_2017", year = 2017L, table = 6900L, category_field = "D5C",
      suffix = "/v/1996/p/2017/c829/46302/c210/45959/c220/110085"
    ),
    list(
      key = "labor_2017", year = 2017L, table = 6884L, category_field = "D5C",
      suffix = "/v/185/p/2017/c829/46302/c2/6794/c223/46571/c218/46502/c12517/113601"
    )
  )
}

sidra_value_status <- function(x) {
  x <- trimws(as.character(x))
  fcase(
    x == "-", "reported_zero",
    x == "X", "suppressed",
    x == "..", "not_applicable",
    x == "...", "unavailable",
    is.finite(as_number(x)), "reported_numeric",
    default = "unparsed"
  )
}

download_sidra_input_panel_spec <- function(spec, force = FALSE, chunk_size = 500L) {
  ensure_output_dir()
  cache <- file.path(
    paths$sidra_dir,
    paste0("FullRevision_Inputs_2006_2017_", spec$key, "_n", chunk_size, ".csv")
  )
  char_cols <- c("Code", "variable_code", "category_code", "value_raw", "value_status", "dataset_key")
  if (!force && file.exists(cache) && file.info(cache)$size > 0) {
    return(fread(cache, encoding = "UTF-8", colClasses = list(character = char_cols)))
  }

  chunks <- sidra_pair_sample_codes(chunk_size = chunk_size)
  fetch_codes <- function(codes, chunk_id) {
    chunk_cache <- file.path(
      paths$sidra_dir,
      paste0("FullRevision_Inputs_2006_2017_", spec$key, "_n", chunk_size, "_chunk_", chunk_id, ".csv")
    )
    if (!force && file.exists(chunk_cache) && file.info(chunk_cache)$size > 0) {
      return(fread(chunk_cache, encoding = "UTF-8", colClasses = list(character = char_cols)))
    }

    url <- paste0(
      "https://apisidra.ibge.gov.br/values/t/", spec$table,
      "/n6/", paste(codes, collapse = ","), spec$suffix
    )
    write_status("Downloading SIDRA table ", spec$table, " (", spec$key, "), chunk ", chunk_id, ".")
    response <- try(curl::curl_fetch_memory(url, handle = curl::new_handle(timeout = 120)), silent = TRUE)
    if (!inherits(response, "try-error") && response$status_code == 200L) {
      parsed <- try(
        jsonlite::fromJSON(rawToChar(response$content), simplifyDataFrame = TRUE),
        silent = TRUE
      )
      if (!inherits(parsed, "try-error") && is.data.frame(parsed) && nrow(parsed) >= 2L) {
        parsed <- as.data.table(parsed[-1L, , drop = FALSE])
        needed <- c("D1C", "D2C", "D3C", spec$category_field, "V")
        if (all(needed %in% names(parsed))) {
          out <- parsed[, .(
            dataset_key = spec$key,
            sidra_table = as.integer(spec$table),
            census_year = as.integer(spec$year),
            Code = trimws(as.character(D1C)),
            variable_code = trimws(as.character(D2C)),
            category_code = trimws(as.character(get(spec$category_field))),
            value_raw = trimws(as.character(V))
          )]
          out[grepl("^[0-9]{6}$", Code), Code := paste0("0", Code)]
          out[, value_status := sidra_value_status(value_raw)]
          out[, value := fcase(
            value_status == "reported_zero", 0,
            value_status == "reported_numeric", as_number(value_raw),
            default = NA_real_
          )]
          fwrite(out, chunk_cache)
          Sys.sleep(0.10)
          return(out)
        }
      }
    }

    if (length(codes) > 1L) {
      mid <- ceiling(length(codes) / 2)
      left <- fetch_codes(codes[seq_len(mid)], paste0(chunk_id, "a"))
      right <- fetch_codes(codes[(mid + 1L):length(codes)], paste0(chunk_id, "b"))
      return(rbindlist(list(left, right), fill = TRUE))
    }
    warning("SIDRA request failed for table ", spec$table, ", municipality ", codes[1], ".")
    NULL
  }

  rows <- lapply(names(chunks), function(id) fetch_codes(chunks[[id]], id))
  out <- rbindlist(rows, fill = TRUE)
  if (nrow(out) == 0L) stop("No SIDRA observations downloaded for ", spec$key, ".")
  fwrite(out, cache)
  out
}

input_panel_outcome_metadata <- function() {
  data.table(
    outcome = c(
      "fertilizer_use_share", "pesticide_use_share", "irrigated_establishment_share",
      "asinh_irrigated_ha_per_100_establishments", "workers_per_100_establishments",
      "fertilizer_corrective_expense_share", "seed_expense_share",
      "pesticide_expense_share", "electricity_expense_share",
      "fuel_expense_share", "wage_expense_share"
    ),
    label = c(
      "Establishments using fertilizer", "Establishments using pesticides",
      "Establishments using irrigation", "Asinh irrigated hectares per 100 establishments",
      "Agricultural workers per 100 establishments", "Fertilizer and soil-corrective expense share",
      "Seed expense share", "Pesticide expense share", "Electricity expense share",
      "Fuel expense share", "Wage expense share"
    ),
    unit = c(
      rep("percentage points", 3), "asinh units", "workers per 100 establishments",
      rep("percentage points", 6)
    ),
    group = c(
      rep("Use of fertilizer, pesticides and irrigation", 3),
      "Irrigation intensity", "Agricultural labour",
      rep("Input-expenditure composition", 6)
    )
  )
}

build_input_census_panel <- function(force_download = FALSE) {
  specs <- input_panel_sidra_specs()
  download_specs <- Filter(
    function(x) x$year == 2006L || x$key %in% c("labor_2017", "wage_expense_2017"),
    specs
  )
  downloaded <- rbindlist(
    lapply(download_specs, download_sidra_input_panel_spec, force = force_download),
    fill = TRUE
  )
  downloaded[dataset_key == "wage_expense_2017", dataset_key := "expenses_2017"]

  existing_2017_file <- file.path(paths$out_dir, "FullRevision_Inputs_2017_SIDRA_Long.csv")
  if (!file.exists(existing_2017_file)) {
    stop("The verified 2017 SIDRA input cache is missing: ", existing_2017_file)
  }
  existing_2017 <- fread(
    existing_2017_file,
    encoding = "UTF-8",
    colClasses = list(character = c("Code", "variable_code", "category_code", "value_raw", "dataset_key"))
  )
  existing_2017[, dataset_key := fcase(
    dataset_key == "adubacao_2017_6847", "fertilizer_2017",
    dataset_key == "calcario_2017_6849", "ph_corrective_2017",
    dataset_key == "agrotoxicos_2017_6851", "pesticide_2017",
    dataset_key == "irrigacao_2017_6859", "irrigation_2017",
    dataset_key == "despesas_2017_6900", "expenses_2017",
    default = NA_character_
  )]
  existing_2017 <- existing_2017[!is.na(dataset_key), .(
    dataset_key,
    sidra_table = as.integer(sidra_table),
    census_year = 2017L,
    Code = trimws(as.character(Code)),
    variable_code = trimws(as.character(variable_code)),
    category_code = trimws(as.character(category_code)),
    value_raw = trimws(as.character(value_raw))
  )]
  existing_2017[, value_status := sidra_value_status(value_raw)]
  existing_2017[, value := fcase(
    value_status == "reported_zero", 0,
    value_status == "reported_numeric", as_number(value_raw),
    default = NA_real_
  )]
  raw <- rbindlist(list(downloaded, existing_2017), fill = TRUE)
  fwrite(raw, file.path(paths$out_dir, "FullRevision_Inputs_2006_2017_SIDRA_Long.csv"))

  raw_diagnostics <- raw[, .(
    cells = .N,
    municipalities = uniqueN(Code),
    reported_numeric = sum(value_status == "reported_numeric"),
    reported_zero = sum(value_status == "reported_zero"),
    suppressed = sum(value_status == "suppressed"),
    not_applicable = sum(value_status == "not_applicable"),
    unavailable = sum(value_status == "unavailable"),
    unparsed = sum(value_status == "unparsed")
  ), by = .(dataset_key, sidra_table, census_year)]
  fwrite(raw_diagnostics, file.path(paths$out_dir, "FullRevision_Inputs_2006_2017_SIDRA_Cell_Diagnostics.csv"))

  cast_key <- function(key) {
    d <- raw[dataset_key == key]
    if (nrow(d) == 0L) return(NULL)
    dcast(
      d,
      Code ~ variable_code + category_code,
      value.var = "value",
      fun.aggregate = function(x) {
        x <- x[is.finite(x)]
        if (length(x) == 0L) NA_real_ else x[1L]
      },
      fill = NA_real_
    )
  }
  col_or_na <- function(dt, name) {
    if (!is.null(dt) && name %in% names(dt)) return(dt[[name]])
    rep(NA_real_, if (is.null(dt)) 0L else nrow(dt))
  }
  combine_complete <- function(x, y) {
    fifelse(is.finite(x) & is.finite(y), x + y, NA_real_)
  }

  build_year <- function(year) {
    fert <- cast_key(paste0("fertilizer_", year))
    pest <- cast_key(paste0("pesticide_", year))
    irr <- cast_key(paste0("irrigation_", year))
    exp <- cast_key(paste0("expenses_", year))
    lab <- cast_key(paste0("labor_", year))
    ph <- if (year == 2017L) cast_key("ph_corrective_2017") else NULL
    base <- Reduce(function(x, y) merge(x, y, by = "Code", all = TRUE), Filter(Negate(is.null), list(fert, pest, irr, exp, lab, ph)))

    getv <- function(dt, name) {
      ans <- rep(NA_real_, nrow(base))
      if (is.null(dt) || !name %in% names(dt)) return(ans)
      idx <- match(base$Code, dt$Code)
      ans[!is.na(idx)] <- dt[[name]][idx[!is.na(idx)]]
      ans
    }
    ratio100 <- function(num, den) {
      fifelse(is.finite(num) & is.finite(den) & den > 0, 100 * num / den, NA_real_)
    }

    if (year == 2006L) {
      total_est <- getv(fert, "183_0")
      fertilizer_use <- getv(fert, "183_114541")
      pesticide_use <- getv(pest, "183_111611")
      irrigated_est <- getv(irr, "2372_118477")
      irrigated_ha <- getv(irr, "2373_118477")
      workers <- getv(lab, "2379_0")
      expense_total <- getv(exp, "1996_113946")
      fertilizer_corrective_expense <- combine_complete(getv(exp, "1996_111960"), getv(exp, "1996_111961"))
      seed_expense <- getv(exp, "1996_111962")
      pesticide_expense <- getv(exp, "1996_111966")
      electricity_expense <- getv(exp, "1996_5497")
      fuel_expense <- getv(exp, "1996_118125")
      wage_expense <- combine_complete(getv(exp, "1996_111959"), getv(exp, "1996_116229"))
      ph_share <- rep(NA_real_, nrow(base))
    } else {
      total_est <- getv(fert, "183_46545")
      fertilizer_use <- getv(fert, "183_46546")
      pesticide_use <- getv(pest, "183_111611")
      irrigated_est <- getv(irr, "2372_118477")
      irrigated_ha <- getv(irr, "2373_118477")
      workers <- getv(lab, "185_6794")
      expense_total <- getv(exp, "1996_113946")
      fertilizer_corrective_expense <- getv(exp, "1996_45953")
      seed_expense <- getv(exp, "1996_111962")
      pesticide_expense <- getv(exp, "1996_111966")
      electricity_expense <- getv(exp, "1996_5497")
      fuel_expense <- getv(exp, "1996_45956")
      wage_expense <- getv(exp, "1996_45959")
      ph_share <- ratio100(getv(ph, "183_46554"), getv(ph, "183_46553"))
    }

    data.table(
      Code = base$Code,
      census_year = year,
      total_establishments = total_est,
      fertilizer_use_share = ratio100(fertilizer_use, total_est),
      pesticide_use_share = ratio100(pesticide_use, total_est),
      irrigated_establishment_share = ratio100(irrigated_est, total_est),
      asinh_irrigated_ha_per_100_establishments = asinh(ratio100(irrigated_ha, total_est)),
      workers_per_100_establishments = ratio100(workers, total_est),
      fertilizer_corrective_expense_share = ratio100(fertilizer_corrective_expense, expense_total),
      seed_expense_share = ratio100(seed_expense, expense_total),
      pesticide_expense_share = ratio100(pesticide_expense, expense_total),
      electricity_expense_share = ratio100(electricity_expense, expense_total),
      fuel_expense_share = ratio100(fuel_expense, expense_total),
      wage_expense_share = ratio100(wage_expense, expense_total),
      soil_ph_corrective_share = ph_share
    )
  }

  panel <- rbindlist(list(build_year(2006L), build_year(2017L)), fill = TRUE)
  fwrite(panel, file.path(paths$out_dir, "FullRevision_Inputs_2006_2017_Census_Long.csv"))
  list(panel = panel, raw = raw, raw_diagnostics = raw_diagnostics)
}

mean_finite <- function(x) {
  x <- as.numeric(x)
  if (!any(is.finite(x))) return(NA_real_)
  mean(x[is.finite(x)])
}

median_finite <- function(x) {
  x <- as.numeric(x)
  if (!any(is.finite(x))) return(NA_real_)
  stats::median(x[is.finite(x)])
}

make_input_environment_endpoints <- function() {
  pam <- read_pam_zeros()
  pam[, `:=`(Code = trimws(as.character(Code)), Year = as.integer(Year))]
  sal <- pam[
    Year %between% c(2004L, 2006L) | Year %between% c(2015L, 2017L),
    .(annual_mean_salinity = finite_first(mean_salinity)),
    by = .(Code, Year)
  ][,
    .(mean_salinity = mean_finite(annual_mean_salinity)),
    by = .(Code, census_year = fifelse(Year <= 2006L, 2006L, 2017L))
  ]

  weather_annual <- pam[
    Year %between% c(2004L, 2006L) | Year %between% c(2015L, 2017L),
    .(
      gdd = mean_finite(gdd_large),
      kdd = mean_finite(kdd_large),
      soil_moisture = mean_finite(sm_season_large)
    ),
    by = .(Code, Year)
  ]
  weather <- weather_annual[, .(
    gdd = mean_finite(gdd),
    kdd = mean_finite(kdd),
    soil_moisture = mean_finite(soil_moisture)
  ), by = .(Code, census_year = fifelse(Year <= 2006L, 2006L, 2017L))]
  static <- pam[, .(
    elevation = median_finite(elevation_muni),
    slope = median_finite(slope_muni),
    clay = median_finite(clay_mean_large)
  ), by = Code]

  env <- merge(sal, weather, by = c("Code", "census_year"), all = TRUE)
  env <- merge(env, static, by = "Code", all.x = TRUE)
  env
}

make_input_change_panel <- function(census_panel, env) {
  meta <- input_panel_outcome_metadata()
  vars <- c(meta$outcome, "soil_ph_corrective_share", "total_establishments")
  long <- merge(census_panel, env, by = c("Code", "census_year"), all.x = TRUE)
  wide <- dcast(
    long,
    Code ~ census_year,
    value.var = c(vars, "mean_salinity", "gdd", "kdd", "soil_moisture"),
    sep = "_"
  )
  static <- unique(env[, .(Code, elevation, slope, clay)], by = "Code")
  wide <- merge(wide, static, by = "Code", all.x = TRUE)
  for (v in c(meta$outcome, "mean_salinity", "gdd", "kdd", "soil_moisture")) {
    wide[, paste0(v, "_change") := get(paste0(v, "_2017")) - get(paste0(v, "_2006"))]
  }
  fwrite(wide, file.path(paths$out_dir, "FullRevision_Inputs_2006_2017_Municipal.csv"))
  wide
}

extract_fixest_term <- function(model, term) {
  ct <- as.data.table(fixest::coeftable(model), keep.rownames = "term")
  se_col <- intersect(c("Std. Error", "Std..Error", "Std...Error"), names(ct))[1]
  p_col <- intersect(c("Pr(>|t|)", "Pr...t..", "Pr(>|z|)", "Pr...z.."), names(ct))[1]
  term_name <- term
  row <- ct[term == term_name]
  if (nrow(row) == 0L) {
    return(data.table(term = term, estimate = NA_real_, se = NA_real_, p = NA_real_))
  }
  data.table(term = term, estimate = row$Estimate[1], se = row[[se_col]][1], p = row[[p_col]][1])
}

estimate_input_change_sfd <- function(municipal_panel) {
  meta <- input_panel_outcome_metadata()
  vars <- c(
    paste0(meta$outcome, "_2006"), paste0(meta$outcome, "_change"),
    "mean_salinity_2006", "mean_salinity_change",
    "gdd_change", "kdd_change", "soil_moisture_change",
    "elevation", "slope", "clay"
  )
  pair_map <- unique(read_pair_map(), by = "pair_id")
  sfd <- make_sfd_from_unit_panel(
    municipal_panel[, c("Code", vars), with = FALSE],
    pair_map,
    id_cols = character(),
    vars = vars
  )
  fwrite(sfd, file.path(paths$out_dir, "FullRevision_Inputs_2006_2017_SFD_PairSample.csv"))

  treatment <- "d_mean_salinity_change"
  weather <- c("d_gdd_change", "d_kdd_change", "d_soil_moisture_change")
  topo <- c("d_elevation", "d_slope", "d_clay")
  vc <- make_conley()
  models <- list()
  coef_rows <- list()
  samples <- list()
  model_index <- 0L

  for (i in seq_len(nrow(meta))) {
    outcome <- meta$outcome[i]
    dep <- paste0("d_", outcome, "_change")
    baseline <- paste0("d_", outcome, "_2006")
    needed <- c(dep, treatment, weather, baseline, topo, "lat", "lon", "pair_id")
    dat <- complete_data(sfd, needed)
    samples[[outcome]] <- dat
    if (nrow(dat) < 100L) {
      warning("Input outcome ", outcome, " has fewer than 100 complete SFD pairs and was not estimated.")
      next
    }

    formulas <- list(
      as.formula(paste(dep, "~", treatment)),
      as.formula(paste(dep, "~", paste(c(treatment, weather), collapse = " + "))),
      as.formula(paste(dep, "~", paste(c(treatment, weather, baseline, topo), collapse = " + ")))
    )
    terms <- unique(c(treatment, weather, baseline, topo))
    for (spec in seq_along(formulas)) {
      model_index <- model_index + 1L
      model <- fixest::feols(formulas[[spec]], data = dat, vcov = vc, notes = FALSE)
      models[[paste(outcome, spec, sep = "__")]] <- model
      rows <- rbindlist(lapply(terms, function(term_name) {
        extract_fixest_term(model, term_name)
      }), fill = TRUE)
      rows[, `:=`(
        outcome = outcome,
        outcome_label = meta$label[i],
        outcome_unit = meta$unit[i],
        outcome_group = meta$group[i],
        specification = spec,
        observations = stats::nobs(model),
        outcome_change_sd = stats::sd(dat[[dep]], na.rm = TRUE),
        salinity_change_sd = stats::sd(dat[[treatment]], na.rm = TRUE)
      )]
      coef_rows[[model_index]] <- rows
    }
  }

  coefficients <- rbindlist(coef_rows, fill = TRUE)
  coefficients[, `:=`(
    ci95_low = estimate - 1.96 * se,
    ci95_high = estimate + 1.96 * se,
    ci90_low = estimate - 1.645 * se,
    ci90_high = estimate + 1.645 * se
  )]
  fwrite(coefficients, file.path(paths$out_dir, "FullRevision_Inputs_2006_2017_SFD_Coefficients.csv"))
  list(sfd = sfd, samples = samples, models = models, coefficients = coefficients)
}

input_change_term_labels <- function(outcome) {
  outcome_name <- outcome
  baseline_label <- input_panel_outcome_metadata()[outcome == outcome_name, label][1]
  c(
    d_mean_salinity_change = "Change in mean salinity (dS/m)",
    d_gdd_change = "Change in GDD",
    d_kdd_change = "Change in KDD",
    d_soil_moisture_change = "Change in soil moisture",
    setNames(paste0("Baseline ", baseline_label), paste0("d_", outcome_name, "_2006")),
    d_elevation = "Elevation",
    d_slope = "Slope",
    d_clay = "Clay content"
  )
}

write_input_change_regression_table <- function(coefficients, outcomes, file, caption, label) {
  meta <- input_panel_outcome_metadata()[outcome %in% outcomes]
  lines <- c(
    "\\begin{landscape}",
    "\\begin{table}[!htbp]",
    "\\color{red}",
    "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    "\\scriptsize",
    "\\renewcommand{\\arraystretch}{0.78}",
    "\\begin{tabular*}{\\linewidth}{@{\\extracolsep{\\fill}}lccc}",
    "\\toprule",
    "Variable & Spec. 1 & Spec. 2 & Spec. 3 \\\\",
    "\\midrule"
  )
  for (i in seq_len(nrow(meta))) {
    outcome_name <- meta$outcome[i]
    term_labels <- input_change_term_labels(outcome_name)
    lines <- c(
      lines,
      paste0("\\multicolumn{4}{l}{\\textit{Outcome: ", latex_escape(meta$label[i]), " (", latex_escape(meta$unit[i]), ")}} \\\\"),
      "\\addlinespace[0.2em]"
    )
    for (term_name in names(term_labels)) {
      cells <- vapply(1:3, function(spec) {
        row <- coefficients[outcome == outcome_name & specification == spec & term == term_name]
        if (nrow(row) == 0L || !is.finite(row$estimate[1])) return("--")
        paste0(fmt(row$estimate[1], 3), stars(row$p[1]), " \\; (", fmt(row$se[1], 3), ")")
      }, character(1))
      lines <- c(lines, paste(c(latex_escape(term_labels[[term_name]]), cells), collapse = " & "), "\\\\")
    }
    n_cells <- vapply(1:3, function(spec) {
      row <- coefficients[outcome == outcome_name & specification == spec][1]
      if (nrow(row) == 0L) "--" else formatC(row$observations, format = "d", big.mark = ",")
    }, character(1))
    lines <- c(
      lines,
      paste(c("Observations", n_cells), collapse = " & "), "\\\\",
      "Pair fixed effects & No & No & No \\\\",
      "State fixed effects & No & No & No \\\\",
      "\\addlinespace[0.45em]"
    )
  }
  lines <- c(
    lines,
    "\\bottomrule",
    "\\end{tabular*}",
    "\\par\\addvspace{0.5ex}",
    paste0(
      "\\parbox{0.96\\linewidth}{\\scriptsize\\textit{Notes:} An observation is one strictly contiguous east-minus-west municipality pair. ",
      "Dependent variables are the east-minus-west difference in the 2006--2017 change of the input listed above. ",
      "Mean salinity and weather are three-year averages for 2004--2006 and 2015--2017. ",
      "Spec. 1 includes the salinity change only. Spec. 2 adds changes in GDD, KDD and soil moisture. ",
      "Spec. 3 additionally controls for the baseline east-minus-west input difference, elevation, slope and clay content. ",
      "The same complete-data sample is used across the three specifications for each outcome. ",
      "Standard errors in parentheses are Conley spatial with a 200 km cutoff. ",
      "No pair fixed effect can be included because there is one long change per pair. ",
      "Common state-level changes cancel when the temporal change is spatially differenced within same-state pairs. ",
      "Expense outcomes are shares of total expenses, so nominal currency levels do not enter the regressions. ",
      "Significance: *** p<0.01, ** p<0.05, * p<0.10.}"
    ),
    "\\end{table}",
    "\\end{landscape}"
  )
  writeLines(lines, file, useBytes = TRUE)
}

write_input_change_descriptives <- function(census_panel, model_result) {
  meta <- input_panel_outcome_metadata()
  rows <- rbindlist(lapply(seq_len(nrow(meta)), function(i) {
    outcome <- meta$outcome[i]
    d06 <- census_panel[census_year == 2006L, get(outcome)]
    d17 <- census_panel[census_year == 2017L, get(outcome)]
    sample <- model_result$samples[[outcome]]
    data.table(
      Input = meta$label[i],
      Unit = meta$unit[i],
      `2006 mean` = mean_finite(d06),
      `2006 SD` = stats::sd(d06, na.rm = TRUE),
      `2006 N` = sum(is.finite(d06)),
      `2017 mean` = mean_finite(d17),
      `2017 SD` = stats::sd(d17, na.rm = TRUE),
      `2017 N` = sum(is.finite(d17)),
      `SFD pairs` = if (is.null(sample)) 0L else nrow(sample)
    )
  }), fill = TRUE)
  fwrite(rows, file.path(paths$out_dir, "FullRevision_Inputs_2006_2017_Descriptives.csv"))
  display <- copy(rows)
  numeric_cols <- c("2006 mean", "2006 SD", "2017 mean", "2017 SD")
  display[, (numeric_cols) := lapply(.SD, fmt, digits = 2), .SDcols = numeric_cols]
  display[, `:=`(
    `2006 N` = formatC(`2006 N`, format = "d", big.mark = ","),
    `2017 N` = formatC(`2017 N`, format = "d", big.mark = ","),
    `SFD pairs` = formatC(`SFD pairs`, format = "d", big.mark = ",")
  )]
  write_latex_df(
    display,
    file.path(paths$out_dir, "FullRevision_Inputs_2006_2017_Descriptives.tex"),
    "Agricultural Input Measures in the 2006 and 2017 Censuses",
    "tab:full_revision_inputs_2006_2017_descriptives",
    paste(
      "Municipal statistics use all non-missing observations in the strictly contiguous-pair universe.",
      "SFD pairs report the complete sample used for all three specifications of each outcome.",
      "A published dash in SIDRA is coded as an exact zero; suppressed or unavailable cells are missing.",
      "Expense categories are divided by total agricultural expenses, avoiding any comparison of nominal currency levels."
    ),
    size = "\\scriptsize",
    landscape = FALSE
  )
  rows
}

make_input_change_figure <- function(coefficients) {
  plot_dt <- coefficients[term == "d_mean_salinity_change" & specification == 2L & is.finite(estimate)]
  group_order <- c(
    "Use of fertilizer, pesticides and irrigation",
    "Irrigation intensity",
    "Agricultural labour",
    "Input-expenditure composition"
  )
  plot_dt[, outcome_group := factor(outcome_group, levels = group_order)]
  plot_dt[, outcome_label := factor(outcome_label, levels = rev(unique(outcome_label)))]
  fig <- ggplot(plot_dt, aes(x = estimate, y = outcome_label)) +
    geom_segment(aes(x = ci95_low, xend = ci95_high, yend = outcome_label), linewidth = 0.65, color = "#1B7837") +
    geom_segment(aes(x = ci90_low, xend = ci90_high, yend = outcome_label), linewidth = 1.8, color = "#1B7837") +
    geom_point(size = 2.8, color = "#1B7837") +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.55, color = "grey30") +
    facet_wrap(~outcome_group, ncol = 1, scales = "free") +
    labs(x = "Effect of a 1 dS/m increase in the 2006--2017 salinity change", y = NULL) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      strip.text = element_text(face = "bold", size = 14),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 11),
      axis.title.x = element_text(size = 12.5, margin = margin(t = 8)),
      plot.margin = margin(8, 10, 8, 8)
    )
  ggsave(file.path(paths$out_dir, "FullRevision_Inputs_2006_2017_SFD_Coefficients.pdf"), fig, width = 8.5, height = 10.6)
  ggsave(file.path(paths$out_dir, "FullRevision_Inputs_2006_2017_SFD_Coefficients.png"), fig, width = 8.5, height = 10.6, dpi = 320)
  invisible(fig)
}

make_input_change_sensitivity <- function(model_result) {
  meta <- input_panel_outcome_metadata()
  rows <- list()
  index <- 0L
  for (i in seq_len(nrow(meta))) {
    outcome <- meta$outcome[i]
    dat <- model_result$samples[[outcome]]
    if (is.null(dat) || nrow(dat) < 100L) next
    dep <- paste0("d_", outcome, "_change")
    formula <- as.formula(paste(
      dep,
      "~ d_mean_salinity_change + d_gdd_change + d_kdd_change + d_soil_moisture_change"
    ))
    bounds <- stats::quantile(dat$d_mean_salinity_change, c(0.01, 0.99), na.rm = TRUE)
    samples <- list(
      full = dat,
      trim_1_99 = dat[
        d_mean_salinity_change >= bounds[1] & d_mean_salinity_change <= bounds[2]
      ]
    )
    for (sample_name in names(samples)) {
      radii <- if (sample_name == "full") c(100, 200, 300) else 200
      for (radius in radii) {
        model <- fixest::feols(
          formula,
          data = samples[[sample_name]],
          vcov = make_conley(cutoff = radius),
          notes = FALSE
        )
        row <- extract_fixest_term(model, "d_mean_salinity_change")
        index <- index + 1L
        rows[[index]] <- data.table(
          outcome = outcome,
          outcome_label = meta$label[i],
          sample = sample_name,
          conley_radius_km = radius,
          observations = stats::nobs(model),
          estimate = row$estimate,
          se = row$se,
          p = row$p
        )
      }
    }
  }
  sensitivity <- rbindlist(rows, fill = TRUE)
  fwrite(
    sensitivity,
    file.path(paths$out_dir, "FullRevision_Inputs_2006_2017_SFD_Sensitivity.csv")
  )

  preferred <- sensitivity[sample == "full" & conley_radius_km == 200]
  preferred[, p_bh := stats::p.adjust(p, method = "BH")]
  fwrite(
    preferred,
    file.path(paths$out_dir, "FullRevision_Inputs_2006_2017_SFD_MultipleTesting.csv")
  )

  treatment <- model_result$sfd[is.finite(d_mean_salinity_change), d_mean_salinity_change]
  support <- data.table(
    statistic = c("N", "Mean", "SD", "Minimum", "P1", "P5", "Median", "P95", "P99", "Maximum"),
    value = c(
      length(treatment), mean(treatment), stats::sd(treatment), min(treatment),
      stats::quantile(treatment, 0.01), stats::quantile(treatment, 0.05),
      stats::median(treatment), stats::quantile(treatment, 0.95),
      stats::quantile(treatment, 0.99), max(treatment)
    )
  )
  fwrite(
    support,
    file.path(paths$out_dir, "FullRevision_Inputs_2006_2017_SalinityChange_Support.csv")
  )
  list(sensitivity = sensitivity, multiple_testing = preferred, support = support)
}

estimate_ph_corrective_2017_exploratory <- function(census_panel, env) {
  ph <- census_panel[census_year == 2017L, .(Code, soil_ph_corrective_share)]
  env17 <- env[census_year == 2017L]
  panel <- merge(ph, env17, by = "Code", all.x = TRUE)
  pair_map <- unique(read_pair_map(), by = "pair_id")
  vars <- c("soil_ph_corrective_share", "mean_salinity", "gdd", "kdd", "soil_moisture", "elevation", "slope", "clay")
  sfd <- make_sfd_from_unit_panel(panel[, c("Code", vars), with = FALSE], pair_map, character(), vars)
  needed <- c(paste0("d_", vars), "lat", "lon", "pair_id")
  dat <- complete_data(sfd, needed)
  if (nrow(dat) < 100L) return(NULL)
  rhs <- list(
    "d_mean_salinity",
    paste(c("d_mean_salinity", "d_gdd", "d_kdd", "d_soil_moisture"), collapse = " + "),
    paste(c("d_mean_salinity", "d_gdd", "d_kdd", "d_soil_moisture", "d_elevation", "d_slope", "d_clay"), collapse = " + ")
  )
  models <- lapply(rhs, function(x) fixest::feols(
    as.formula(paste("d_soil_ph_corrective_share ~", x)),
    data = dat,
    vcov = make_conley(),
    notes = FALSE
  ))
  names(models) <- paste0("Spec. ", 1:3)
  file <- file.path(paths$out_dir, "FullRevision_Inputs_pH_Corrective_2017_Exploratory.tex")
  fixest::etable(
    models,
    tex = TRUE,
    file = file,
    replace = TRUE,
    dict = c(
      d_soil_ph_corrective_share = "Soil-pH corrective-use share",
      d_mean_salinity = "Mean salinity (dS/m)", d_gdd = "GDD", d_kdd = "KDD",
      d_soil_moisture = "Soil moisture", d_elevation = "Elevation",
      d_slope = "Slope", d_clay = "Clay content"
    ),
    headers = list("Lime or soil-pH corrective use" = 3),
    fitstat = ~n,
    title = "Mean Salinity and Lime or Soil-pH Corrective Use in 2017",
    label = "tab:full_revision_inputs_ph_corrective_2017",
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    notes = paste(
      "The outcome is the east-minus-west difference in the percentage of establishments reporting lime or another soil-pH corrective in the 2017 Agricultural Census.",
      "This variable is not a salinity-specific amendment and is unavailable on a harmonized basis for 2006.",
      "Spec. 2 adds GDD, KDD and soil moisture; Spec. 3 adds elevation, slope and clay.",
      "Standard errors are Conley spatial with a 200 km cutoff. The cross-sectional association is exploratory and is not interpreted causally."
    )
  )
  style_fixest_tex(file, size = "\\scriptsize", tabcolsep = "5pt", arraystretch = "0.85", resize = FALSE, landscape = FALSE)
  coef <- rbindlist(lapply(seq_along(models), function(i) {
    row <- coef_numeric_row(models[[i]], "d_mean_salinity", paste0("Spec. ", i))
    row[, specification := i]
    row
  }))
  fwrite(coef, file.path(paths$out_dir, "FullRevision_Inputs_pH_Corrective_2017_Exploratory.csv"))
  list(data = dat, models = models, coefficients = coef)
}

make_input_panel_analysis <- function(force_download = FALSE) {
  write_status("Building harmonized 2006--2017 agricultural-input panel and SFD long-change regressions.")
  census <- build_input_census_panel(force_download = force_download)
  env <- make_input_environment_endpoints()
  municipal <- make_input_change_panel(census$panel, env)
  result <- estimate_input_change_sfd(municipal)
  descriptives <- write_input_change_descriptives(census$panel, result)

  write_input_change_regression_table(
    result$coefficients,
    c("fertilizer_use_share", "pesticide_use_share", "irrigated_establishment_share"),
    file.path(paths$out_dir, "FullRevision_Inputs_2006_2017_SFD_Use_Practices.tex"),
    "Salinity Changes and Fertilizer, Pesticide and Irrigation Use, 2006--2017",
    "tab:full_revision_inputs_change_use_practices"
  )
  write_input_change_regression_table(
    result$coefficients,
    c("asinh_irrigated_ha_per_100_establishments", "workers_per_100_establishments"),
    file.path(paths$out_dir, "FullRevision_Inputs_2006_2017_SFD_Water_Labour.tex"),
    "Salinity Changes, Irrigation Intensity and Agricultural Labour, 2006--2017",
    "tab:full_revision_inputs_change_water_labour"
  )
  write_input_change_regression_table(
    result$coefficients,
    c("fertilizer_corrective_expense_share", "seed_expense_share", "pesticide_expense_share"),
    file.path(paths$out_dir, "FullRevision_Inputs_2006_2017_SFD_Expense_Agrochemical.tex"),
    "Salinity Changes and Agrochemical Input-Expense Shares, 2006--2017",
    "tab:full_revision_inputs_change_expense_agrochemical"
  )
  write_input_change_regression_table(
    result$coefficients,
    c("electricity_expense_share", "fuel_expense_share", "wage_expense_share"),
    file.path(paths$out_dir, "FullRevision_Inputs_2006_2017_SFD_Expense_Operations.tex"),
    "Salinity Changes and Operating Input-Expense Shares, 2006--2017",
    "tab:full_revision_inputs_change_expense_operations"
  )
  make_input_change_figure(result$coefficients)
  sensitivity <- make_input_change_sensitivity(result)
  ph <- estimate_ph_corrective_2017_exploratory(census$panel, env)

  preferred <- result$coefficients[term == "d_mean_salinity_change" & specification == 2L]
  print(preferred[, .(outcome_label, estimate, se, p, observations, salinity_change_sd)])
  write_status("Harmonized 2006--2017 input panel completed.")
  invisible(list(
    census = census,
    environment = env,
    municipal = municipal,
    sfd = result,
    descriptives = descriptives,
    sensitivity = sensitivity,
    ph_2017 = ph
  ))
}

# =============================================================================
# 8. Census geography harmonization and migration analyses
# =============================================================================
build_muni_geo2_crosswalk <- function(muni_static) {
  if (!file.exists(paths$ipums_geo2_shp)) stop("IPUMS GEO2 shapefile not found.")
  muni_sf <- sf::st_as_sf(as.data.frame(muni_static), sf_column_name = "geom")
  if (is.na(sf::st_crs(muni_sf))) sf::st_crs(muni_sf) <- 4326
  geo2 <- sf::st_read(paths$ipums_geo2_shp, quiet = TRUE)
  geo_col <- intersect(c("GEOLEVEL2", "GEO2_BR", "GEO2"), names(geo2))[1]
  if (is.na(geo_col)) stop("Could not find a GEO2 identifier in the IPUMS shapefile.")
  geo2 <- sf::st_transform(geo2, sf::st_crs(muni_sf))
  old_s2 <- sf::sf_use_s2()
  on.exit(sf::sf_use_s2(old_s2), add = TRUE)
  sf::sf_use_s2(FALSE)
  pts <- sf::st_point_on_surface(sf::st_make_valid(muni_sf))
  joined <- suppressWarnings(sf::st_join(pts, geo2[, geo_col], join = sf::st_intersects))
  miss <- is.na(joined[[geo_col]])
  if (any(miss)) {
    nearest <- sf::st_nearest_feature(joined[miss, ], geo2)
    joined[[geo_col]][miss] <- geo2[[geo_col]][nearest]
  }
  cw <- as.data.table(sf::st_drop_geometry(joined[, c("Code", geo_col)]))
  setnames(cw, geo_col, "GEO2_BR")
  cw[, GEO2_BR := sub("^0+", "", trimws(as.character(GEO2_BR)))]
  cw[, Code := trimws(as.character(Code))]
  unique(cw)
}

make_geo2_year_environment <- function() {
  write_status("Building GEO2-year environmental exposure file.")
  pam <- readRDS(paths$pam_rds)
  setDT(pam)
  pam[, Code := trimws(as.character(Code))]
  if (!"mean_salinity" %in% names(pam)) {
    sal_median <- stats::median(pam$salinity_mean_large, na.rm = TRUE)
    sal_scale <- if (is.finite(sal_median) && sal_median > 20) 100 else 1
    pam[, mean_salinity := salinity_mean_large / sal_scale]
  }
  pam[, weight_area := fifelse(is.finite(planted_area) & planted_area > 0, planted_area, NA_real_)]
  vars <- intersect(c("mean_salinity", "excess_above_fao_large", "gdd_large", "kdd_large", "precip_season_large", "sm_season_large"), names(pam))
  muni_year <- pam[, {
    w <- weight_area
    ans <- lapply(.SD, weighted_mean_safe, w = w)
    names(ans) <- vars
    ans$total_planted_area = sum(planted_area, na.rm = TRUE)
    ans
  }, by = .(Code, Year), .SDcols = vars]
  muni_static <- unique(pam[, .(Code, geom)], by = "Code")
  cw <- build_muni_geo2_crosswalk(muni_static)
  muni_sf <- sf::st_sf(
    data.frame(Code = muni_static$Code),
    geometry = muni_static$geom
  )
  muni_points <- sf::st_transform(
    suppressWarnings(sf::st_point_on_surface(sf::st_transform(muni_sf, 5880))),
    4326
  )
  xy <- sf::st_coordinates(muni_points)
  muni_location <- data.table(
    Code = as.character(muni_static$Code),
    state = substr(as.character(muni_static$Code), 1L, 2L),
    lon = xy[, 1],
    lat = xy[, 2]
  )
  cw <- merge(cw, muni_location, by = "Code", all.x = TRUE)
  dt <- merge(muni_year, cw, by = "Code", all.x = TRUE)
  dt <- dt[!is.na(GEO2_BR) & GEO2_BR != ""]
  geo2 <- dt[, {
    w <- total_planted_area
    ans <- lapply(.SD, weighted_mean_safe, w = w)
    names(ans) <- vars
    ans$total_planted_area_geo2 = sum(total_planted_area, na.rm = TRUE)
    ans$state = {
      z <- state[!is.na(state) & nzchar(state)]
      if (length(z) == 0L) NA_character_ else names(which.max(table(z)))
    }
    ans$lon = mean(lon, na.rm = TRUE)
    ans$lat = mean(lat, na.rm = TRUE)
    ans
  }, by = .(GEO2_BR, Year), .SDcols = vars]
  fwrite(geo2, file.path(paths$out_dir, "FullRevision_GEO2_Year_Environment.csv"))
  geo2
}

make_exposure_windows <- function(geo2_year, type = c("pre_clean", "during")) {
  type <- match.arg(type)
  geo2_year <- copy(geo2_year)
  geo2_year[, GEO2_BR := trimws(as.character(GEO2_BR))]
  windows <- switch(
    type,
    pre_clean = data.table(YEAR = c("2000", "2010"), window_start = c(1990L, 2000L), window_end = c(1994L, 2004L)),
    during = data.table(YEAR = c("1991", "2000", "2010"), window_start = c(1986L, 1995L, 2005L), window_end = c(1990L, 1999L, 2009L))
  )
  vars <- setdiff(names(geo2_year), c("GEO2_BR", "Year", "state", "lon", "lat"))
  location <- unique(geo2_year[, .(GEO2_BR, state, lon, lat)], by = "GEO2_BR")
  out <- rbindlist(lapply(seq_len(nrow(windows)), function(i) {
    w <- windows[i]
    geo2_year[Year >= w$window_start & Year <= w$window_end, lapply(.SD, weighted_mean_safe), by = GEO2_BR, .SDcols = vars][
      ,
      `:=`(YEAR = w$YEAR, exposure_window = paste0(w$window_start, "-", w$window_end))
    ]
  }), fill = TRUE)
  out <- merge(out, location, by = "GEO2_BR", all.x = TRUE)
  scale_vars <- setdiff(names(out), c("GEO2_BR", "YEAR", "exposure_window"))
  scale_vars <- setdiff(scale_vars, c("state", "lon", "lat"))
  out[, paste0("z_", scale_vars) := lapply(.SD, scale_safe), by = YEAR, .SDcols = scale_vars]
  out
}

make_migration_start_exposure <- function(geo2_year) {
  geo2_year <- copy(geo2_year)
  geo2_year[, `:=`(
    GEO2_BR = trimws(as.character(GEO2_BR)),
    Year = as.integer(Year)
  )]
  starts <- data.table(
    Year = c(1985L, 1995L, 2005L),
    YEAR = c("1991", "2000", "2010"),
    exposure_window = c("1985", "1995", "2005")
  )
  keep <- intersect(
    c(
      "GEO2_BR", "Year", "mean_salinity", "gdd_large", "kdd_large",
      "sm_season_large", "state", "lon", "lat"
    ),
    names(geo2_year)
  )
  out <- merge(
    geo2_year[, ..keep],
    starts,
    by = "Year",
    all = FALSE,
    allow.cartesian = FALSE
  )
  if (anyDuplicated(out[, .(GEO2_BR, YEAR)])) {
    stop("Start-of-window migration exposure is not unique by GEO2 and census.")
  }
  required <- c(
    "mean_salinity", "gdd_large", "kdd_large", "sm_season_large"
  )
  missing <- setdiff(required, names(out))
  if (length(missing) > 0L) {
    stop("Missing start-of-window migration exposures: ", paste(missing, collapse = ", "))
  }
  out[]
}

ipums_ind_crop_map <- function() {
  rbindlist(list(
    data.table(
      YEAR = c("1980", "1980", "1980", "1980", "1980"),
      ind_code = c(20L, 13L, 19L, 21L, 17L),
      crop = c("corn", "rice", "cassava", "soy", "sugarcane")
    ),
    data.table(
      YEAR = c("1991", "1991", "1991", "1991", "1991"),
      ind_code = c(20L, 13L, 19L, 21L, 17L),
      crop = c("corn", "rice", "cassava", "soy", "sugarcane")
    ),
    data.table(
      YEAR = c("2000", "2000", "2000", "2000", "2000",
               "2010", "2010", "2010", "2010", "2010"),
      ind_code = rep(c(1102L, 1101L, 1108L, 1107L, 1105L), 2L),
      crop = rep(c("corn", "rice", "cassava", "soy", "sugarcane"), 2L)
    )
  ))
}

crop_salinity_thresholds <- function(include_beans = TRUE) {
  thresholds <- c(
    beans = 1.0, cassava = 1.5, corn = 1.7,
    rice = 3.0, soy = 5.0, sugarcane = 1.7
  )
  if (isTRUE(include_beans)) thresholds else thresholds[names(thresholds) != "beans"]
}

extract_ipums_rural_aggregates <- function() {
  write_status("Aggregating IPUMS rural migration and labor-force data with DuckDB.")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  p <- normalizePath(paths$ipums_parquet, winslash = "/", mustWork = TRUE)
  invalid <- "('','0','99999999','999999999','76097997')"
  current_sql <- sprintf(
    "
    WITH base_all AS (
      SELECT
        CAST(YEAR AS VARCHAR) AS YEAR,
        TRIM(CAST(GEO2_BR AS VARCHAR)) AS region,
        CAST(PERWT AS DOUBLE) AS perwt,
        CAST(AGE AS DOUBLE) AS age,
        CAST(SEX AS DOUBLE) AS sex,
        CAST(URBAN AS DOUBLE) AS urban,
        TRY_CAST(INDGEN AS DOUBLE) AS indgen,
        TRY_CAST(IND AS BIGINT) AS ind,
        CASE WHEN CAST(INCTOT AS DOUBLE) < 9999990 THEN CAST(INCTOT AS DOUBLE) ELSE NULL END AS income
      FROM read_parquet('%s')
      WHERE CAST(YEAR AS VARCHAR) IN ('1980','1991','2000','2010')
        AND CAST(AGE AS DOUBLE) >= 15
        AND CAST(PERWT AS DOUBLE) > 0
        AND GEO2_BR IS NOT NULL
        AND TRIM(CAST(GEO2_BR AS VARCHAR)) NOT IN %s
    ),
    current_region AS (
      SELECT
        YEAR,
        region,
        SUM(perwt) AS current_population,
        SUM(CASE WHEN urban = 1 THEN perwt ELSE 0 END) / SUM(perwt) AS rural_share,
        SUM(CASE WHEN income IS NOT NULL THEN income * perwt ELSE 0 END) / NULLIF(SUM(CASE WHEN income IS NOT NULL THEN perwt ELSE 0 END), 0) AS mean_income,
        SUM(age * perwt) / SUM(perwt) AS mean_age,
        SUM(CASE WHEN sex = 1 THEN perwt ELSE 0 END) / SUM(perwt) AS male_share,
        SUM(CASE WHEN indgen = 10 THEN perwt ELSE 0 END) AS ag_workers,
        SUM(CASE WHEN (YEAR IN ('1980','1991') AND ind = 20) OR (YEAR IN ('2000','2010') AND ind = 1102) THEN perwt ELSE 0 END) AS corn_workers,
        SUM(CASE WHEN (YEAR IN ('1980','1991') AND ind = 13) OR (YEAR IN ('2000','2010') AND ind = 1101) THEN perwt ELSE 0 END) AS rice_workers,
        SUM(CASE WHEN (YEAR IN ('1980','1991') AND ind = 19) OR (YEAR IN ('2000','2010') AND ind = 1108) THEN perwt ELSE 0 END) AS cassava_workers,
        SUM(CASE WHEN (YEAR IN ('1980','1991') AND ind = 21) OR (YEAR IN ('2000','2010') AND ind = 1107) THEN perwt ELSE 0 END) AS soy_workers,
        SUM(CASE WHEN (YEAR IN ('1980','1991') AND ind = 17) OR (YEAR IN ('2000','2010') AND ind = 1105) THEN perwt ELSE 0 END) AS sugarcane_workers,
        SUM(CASE WHEN indgen IS NOT NULL AND indgen NOT IN (0, 998, 999) THEN perwt ELSE 0 END) AS workers_with_industry
      FROM base_all
      GROUP BY YEAR, region
    )
    SELECT * FROM current_region
    ",
    p, invalid
  )
  current_region <- as.data.table(DBI::dbGetQuery(con, current_sql))

  flow_sql <- sprintf(
    "
    WITH base_flow AS (
      SELECT
        CAST(YEAR AS VARCHAR) AS YEAR,
        TRIM(CAST(MIG2_5_BR AS VARCHAR)) AS orig,
        TRIM(CAST(GEO2_BR AS VARCHAR)) AS dest,
        CAST(PERWT AS DOUBLE) AS perwt,
        TRY_CAST(INDGEN AS DOUBLE) AS indgen,
        TRY_CAST(IND AS BIGINT) AS ind
      FROM read_parquet('%s')
      WHERE CAST(YEAR AS VARCHAR) IN ('1991','2000','2010')
        AND CAST(AGE AS DOUBLE) >= 15
        AND CAST(PERWT AS DOUBLE) > 0
        AND MIG2_5_BR IS NOT NULL
        AND GEO2_BR IS NOT NULL
    ),
    base_valid AS (
      SELECT *
      FROM base_flow
      WHERE orig NOT IN %s
        AND dest NOT IN %s
        AND orig <> dest
    ),
    flows AS (
      SELECT
        YEAR,
        orig,
        dest,
        SUM(perwt) AS migrant_flow,
        SUM(CASE WHEN indgen = 10 THEN perwt ELSE 0 END) AS current_ag_migrant_flow,
        SUM(CASE WHEN (YEAR = '1991' AND ind = 20) OR (YEAR IN ('2000','2010') AND ind = 1102) THEN perwt ELSE 0 END) AS current_corn_migrant_flow,
        SUM(CASE WHEN (YEAR = '1991' AND ind = 13) OR (YEAR IN ('2000','2010') AND ind = 1101) THEN perwt ELSE 0 END) AS current_rice_migrant_flow,
        SUM(CASE WHEN (YEAR = '1991' AND ind = 19) OR (YEAR IN ('2000','2010') AND ind = 1108) THEN perwt ELSE 0 END) AS current_cassava_migrant_flow,
        SUM(CASE WHEN (YEAR = '1991' AND ind = 21) OR (YEAR IN ('2000','2010') AND ind = 1107) THEN perwt ELSE 0 END) AS current_soy_migrant_flow,
        SUM(CASE WHEN (YEAR = '1991' AND ind = 17) OR (YEAR IN ('2000','2010') AND ind = 1105) THEN perwt ELSE 0 END) AS current_sugarcane_migrant_flow
      FROM base_valid
      GROUP BY YEAR, orig, dest
    )
    SELECT * FROM flows
    ",
    p, invalid, invalid
  )
  flows <- as.data.table(DBI::dbGetQuery(con, flow_sql))

  current_region[, `:=`(
    ag_labor_share = fifelse(workers_with_industry > 0, 100 * ag_workers / workers_with_industry, NA_real_),
    ag_employment_rate = fifelse(current_population > 0, 100 * ag_workers / current_population, NA_real_),
    nonag_employment_rate = fifelse(
      current_population > 0,
      100 * (workers_with_industry - ag_workers) / current_population,
      NA_real_
    ),
    rural_region = rural_share >= 0.5
  )]
  for (cr in c("corn", "rice", "cassava", "soy", "sugarcane")) {
    workers <- paste0(cr, "_workers")
    current_region[, paste0(cr, "_worker_share") := fifelse(
      workers_with_industry > 0,
      100 * get(workers) / workers_with_industry,
      NA_real_
    )]
    current_region[, paste0(cr, "_employment_rate") := fifelse(
      current_population > 0,
      100 * get(workers) / current_population,
      NA_real_
    )]
  }
  baseline_map <- data.table(
    YEAR = c("1991", "2000", "2010"),
    baseline_year = c("1980", "1991", "2000")
  )
  baseline <- merge(
    baseline_map,
    current_region,
    by.x = "baseline_year",
    by.y = "YEAR",
    all.x = TRUE,
    allow.cartesian = TRUE
  )
  origin_stats <- baseline[
    rural_share >= 0.5,
    .(
      YEAR,
      baseline_year,
      orig = region,
      origin_population_prior_census = current_population,
      origin_mean_income = mean_income,
      origin_rural_share_baseline = rural_share,
      origin_ag_labor_share_baseline = ag_labor_share
    )
  ]

  origin_population_sql <- sprintf(
    "
    WITH origin_history AS (
      SELECT
        CAST(YEAR AS VARCHAR) AS YEAR,
        TRIM(CAST(MIG2_5_BR AS VARCHAR)) AS orig,
        TRIM(CAST(GEO2_BR AS VARCHAR)) AS dest,
        CAST(PERWT AS DOUBLE) AS perwt
      FROM read_parquet('%s')
      WHERE CAST(YEAR AS VARCHAR) IN ('1991','2000','2010')
        AND CAST(AGE AS DOUBLE) >= 15
        AND CAST(PERWT AS DOUBLE) > 0
        AND MIG2_5_BR IS NOT NULL
        AND GEO2_BR IS NOT NULL
    )
    SELECT
      YEAR,
      orig,
      SUM(perwt) AS origin_population
    FROM origin_history
    WHERE orig NOT IN %s
      AND dest NOT IN %s
    GROUP BY YEAR, orig
    ",
    p, invalid, invalid
  )
  origin_population <- as.data.table(DBI::dbGetQuery(con, origin_population_sql))
  origin_stats <- merge(
    origin_stats,
    origin_population,
    by = c("YEAR", "orig"),
    all.x = TRUE,
    allow.cartesian = FALSE
  )
  origin_stats <- origin_stats[
    is.finite(origin_population) & origin_population > 0
  ]
  fwrite(current_region, file.path(paths$out_dir, "FullRevision_IPUMS_GEO2_CurrentRegion_Stats.csv"))
  fwrite(origin_stats, file.path(paths$out_dir, "FullRevision_IPUMS_RuralOrigin_Stats.csv"))
  fwrite(flows, file.path(paths$out_dir, "FullRevision_IPUMS_RuralOrigin_Flows.csv"))
  list(current_region = current_region, origin_stats = origin_stats, flows = flows)
}

build_rural_migration_panel <- function(aggregates, exposure) {
  flows <- copy(aggregates$flows)
  origin_stats <- copy(aggregates$origin_stats)
  dest_stats <- copy(aggregates$current_region)
  flows[, `:=`(
    YEAR = as.character(YEAR),
    orig = trimws(as.character(orig)),
    dest = trimws(as.character(dest))
  )]
  origin_stats[, `:=`(
    YEAR = as.character(YEAR),
    orig = trimws(as.character(orig))
  )]
  dest_stats[, `:=`(
    YEAR = as.character(YEAR),
    region = trimws(as.character(region))
  )]
  exposure[, `:=`(
    YEAR = as.character(YEAR),
    GEO2_BR = trimws(as.character(GEO2_BR))
  )]
  regions_expo <- unique(exposure$GEO2_BR)
  origins <- sort(intersect(unique(origin_stats$orig), regions_expo))
  dests <- sort(intersect(unique(dest_stats$region), regions_expo))
  years <- sort(intersect(unique(exposure$YEAR), unique(origin_stats$YEAR)))
  potential_rows <- length(years) * (length(origins) * length(dests) - length(intersect(origins, dests)))
  informative_dyads <- unique(
    flows[
      YEAR %in% years &
        orig %in% origins &
        dest %in% dests &
        orig != dest &
        is.finite(migrant_flow) &
        migrant_flow > 0,
      .(orig, dest)
    ]
  )
  panel <- informative_dyads[, .(YEAR = years), by = .(orig, dest)]
  setcolorder(panel, c("YEAR", "orig", "dest"))
  panel <- merge(panel, flows, by = c("YEAR", "orig", "dest"), all.x = TRUE)
  panel[is.na(migrant_flow), migrant_flow := 0]
  flow_columns <- grep("^current_.*_migrant_flow$", names(panel), value = TRUE)
  for (flow_column in flow_columns) {
    set(panel, which(is.na(panel[[flow_column]])), flow_column, 0)
  }
  panel <- merge(panel, origin_stats, by = c("YEAR", "orig"), all.x = TRUE)
  dest_keep <- dest_stats[, .(
    YEAR,
    dest = region,
    dest_population = current_population,
    dest_mean_income = mean_income,
    dest_rural_share = rural_share,
    dest_ag_labor_share = ag_labor_share
  )]
  panel <- merge(panel, dest_keep, by = c("YEAR", "dest"), all.x = TRUE)
  expo_cols <- setdiff(names(exposure), c("GEO2_BR", "YEAR", "exposure_window"))
  eo <- copy(exposure)
  ed <- copy(exposure)
  setnames(eo, "GEO2_BR", "orig")
  setnames(ed, "GEO2_BR", "dest")
  setnames(eo, expo_cols, paste0(expo_cols, "_orig"))
  setnames(ed, expo_cols, paste0(expo_cols, "_dest"))
  panel <- merge(panel, eo, by = c("YEAR", "orig"), all.x = TRUE)
  panel <- merge(panel, ed, by = c("YEAR", "dest"), all.x = TRUE)
  if (!"state_orig" %in% names(panel)) panel[, state_orig := NA_character_]
  if (!"state_dest" %in% names(panel)) panel[, state_dest := NA_character_]
  panel[, `:=`(
    dyad = paste(orig, dest, sep = "__"),
    log_origin_income = fifelse(is.finite(origin_mean_income) & origin_mean_income > 0, log(origin_mean_income), NA_real_),
    log_dest_income = fifelse(is.finite(dest_mean_income) & dest_mean_income > 0, log(dest_mean_income), NA_real_),
    has_flow = as.integer(migrant_flow > 0),
    migration_rate = migrant_flow / origin_population
  )]
  if ("current_ag_migrant_flow" %in% names(panel)) {
    panel[, current_ag_migration_rate := current_ag_migrant_flow / origin_population]
  } else {
    panel[, current_ag_migration_rate := NA_real_]
  }
  panel <- panel[is.finite(origin_population) & origin_population > 0]
  attr(panel, "potential_rows") <- potential_rows
  attr(panel, "informative_dyads_pre_filter") <- nrow(informative_dyads)
  panel
}

make_migration_geo2_pair_map <- function() {
  crosswalk_file <- file.path(
    paths$project_dir, "MT2", "research_inputs",
    "muni_to_ipums_geo2_crosswalk.csv"
  )
  if (file.exists(crosswalk_file)) {
    crosswalk <- fread(
      crosswalk_file,
      encoding = "UTF-8",
      colClasses = list(character = c("Code", "GEO2_BR"))
    )
  } else {
    pam <- readRDS(paths$pam_rds)
    setDT(pam)
    pam[, Code := trimws(as.character(Code))]
    municipal_static <- unique(pam[, .(Code, geom)], by = "Code")
    crosswalk <- build_muni_geo2_crosswalk(municipal_static)
    fwrite(
      crosswalk,
      file.path(paths$out_dir, "FullRevision_Municipality_GEO2_Crosswalk.csv")
    )
  }
  crosswalk[, `:=`(
    Code = trimws(as.character(Code)),
    GEO2_BR = trimws(as.character(GEO2_BR))
  )]
  crosswalk <- unique(crosswalk[!is.na(GEO2_BR) & nzchar(GEO2_BR), .(Code, GEO2_BR)])

  municipal_pairs <- read_pair_map()
  east <- merge(
    municipal_pairs,
    crosswalk[, .(Code, GEO2_east = GEO2_BR)],
    by = "Code",
    all = FALSE
  )
  both <- merge(
    east,
    crosswalk[, .(code_neighbor_west = Code, GEO2_west = GEO2_BR)],
    by = "code_neighbor_west",
    all = FALSE
  )
  both <- both[
    !is.na(GEO2_east) & !is.na(GEO2_west) &
      nzchar(GEO2_east) & nzchar(GEO2_west) &
      GEO2_east != GEO2_west
  ]
  if (!"state" %in% names(both)) both[, state := NA_character_]
  if (!"main_basin" %in% names(both)) both[, main_basin := NA_character_]
  both[, geo2_pair := paste(GEO2_east, GEO2_west, sep = "__")]
  pair_map <- both[, .(
    lat = mean(as.numeric(lat), na.rm = TRUE),
    lon = mean(as.numeric(lon), na.rm = TRUE),
    municipal_pairs = uniqueN(pair_id),
    state = {
      values <- as.character(state)
      values <- values[!is.na(values) & nzchar(values)]
      if (length(values) == 0L) NA_character_ else values[1L]
    },
    basin = {
      values <- as.character(main_basin)
      values <- values[!is.na(values) & nzchar(values)]
      if (length(values) == 0L) NA_character_ else values[1L]
    }
  ), by = .(geo2_pair, GEO2_east, GEO2_west)]
  pair_map <- pair_map[is.finite(lat) & is.finite(lon)]
  if (anyDuplicated(pair_map$geo2_pair)) {
    stop("The migration GEO2 pair map is not unique.")
  }
  fwrite(
    pair_map,
    file.path(paths$out_dir, "FullRevision_RuralMigration_TwoStep_GEO2_Pairs.csv")
  )
  pair_map[]
}

extract_origin_period_fixed_effects <- function(model) {
  fixed_effects <- suppressMessages(fixest::fixef(model))
  origin_name <- grep("^orig\\^YEAR$", names(fixed_effects), value = TRUE)
  if (length(origin_name) != 1L) {
    stop("Could not identify the origin-by-census fixed effects in the first step.")
  }
  values <- fixed_effects[[origin_name]]
  result <- data.table(
    fixed_effect_key = names(values),
    origin_period_effect = as.numeric(values)
  )
  result[, YEAR := sub(
    "^.*_(1991|2000|2010)$", "\\1", fixed_effect_key
  )]
  result[, GEO2_BR := sub(
    "_(1991|2000|2010)$", "", fixed_effect_key
  )]
  if (any(!result$YEAR %in% c("1991", "2000", "2010"))) {
    stop("Unexpected census suffix in the origin-period fixed effects.")
  }
  if (anyDuplicated(result[, .(GEO2_BR, YEAR)])) {
    stop("Origin-period fixed effects are not unique.")
  }
  result[]
}

build_two_step_migration_sfd <- function(origin_period_effects, exposure, pair_map) {
  exposure <- copy(exposure)
  exposure[, `:=`(
    GEO2_BR = trimws(as.character(GEO2_BR)),
    YEAR = as.character(YEAR)
  )]
  variables <- c(
    "origin_period_effect", "mean_salinity", "gdd_large", "kdd_large",
    "sm_season_large"
  )
  origin_period <- merge(
    origin_period_effects,
    exposure[, c("GEO2_BR", "YEAR", setdiff(variables, "origin_period_effect")), with = FALSE],
    by = c("GEO2_BR", "YEAR"),
    all = FALSE,
    allow.cartesian = FALSE
  )

  east <- copy(origin_period[, c("GEO2_BR", "YEAR", variables), with = FALSE])
  setnames(east, "GEO2_BR", "GEO2_east")
  setnames(east, variables, paste0(variables, "_east"))
  west <- copy(origin_period[, c("GEO2_BR", "YEAR", variables), with = FALSE])
  setnames(west, "GEO2_BR", "GEO2_west")
  setnames(west, variables, paste0(variables, "_west"))

  data <- merge(pair_map, east, by = "GEO2_east", all = FALSE)
  data <- merge(
    data,
    west,
    by = c("GEO2_west", "YEAR"),
    all = FALSE,
    allow.cartesian = FALSE
  )
  for (variable in variables) {
    data[, paste0("d_", variable) :=
      get(paste0(variable, "_east")) - get(paste0(variable, "_west"))]
  }
  required <- c(
    paste0("d_", variables), "YEAR", "geo2_pair", "GEO2_east",
    "GEO2_west", "lat", "lon"
  )
  data <- complete_data(data, required)
  informative_pairs <- data[, .N, by = geo2_pair][N >= 2L, geo2_pair]
  data <- data[geo2_pair %in% informative_pairs]
  setorder(data, geo2_pair, YEAR)
  data[]
}

extract_two_step_coefficient <- function(model, specification, inference) {
  row <- coef_numeric_row(model, "d_mean_salinity", specification)
  setnames(row, "Model", "specification")
  row[, `:=`(
    inference = inference,
    percent_effect = 100 * (exp(estimate) - 1)
  )]
  row
}

estimate_two_step_structural_migration <- function(
    panel,
    start_exposure,
    during_exposure
) {
  write_status("Estimating the two-step structural-gravity and migration-SFD model.")
  first_required <- c(
    "migrant_flow", "origin_population", "dyad", "orig", "dest", "YEAR"
  )
  first_data <- complete_data(panel, first_required)
  first_data <- first_data[
    is.finite(migrant_flow) & migrant_flow >= 0 &
      is.finite(origin_population) & origin_population > 0 &
      orig != dest
  ]
  positive_dyads <- first_data[
    , .(positive_once = any(migrant_flow > 0)), by = dyad
  ][positive_once == TRUE, dyad]
  first_data <- first_data[dyad %in% positive_dyads]
  first_stage <- fixest::fepois(
    migrant_flow ~ 1 | dyad + orig^YEAR + dest^YEAR,
    data = first_data,
    offset = ~log(origin_population),
    notes = FALSE
  )
  if (!isTRUE(first_stage$convStatus)) {
    stop("The first-step migration PPML did not converge.")
  }
  first_model_data <- first_data[fixest::obs(first_stage)]
  origin_period_effects <- extract_origin_period_fixed_effects(first_stage)
  pair_map <- make_migration_geo2_pair_map()
  start_sfd <- build_two_step_migration_sfd(
    origin_period_effects, start_exposure, pair_map
  )
  during_sfd <- build_two_step_migration_sfd(
    origin_period_effects, during_exposure, pair_map
  )
  second_required <- c(
    "d_origin_period_effect", "d_mean_salinity", "d_gdd_large",
    "d_kdd_large", "d_sm_season_large", "YEAR", "geo2_pair",
    "GEO2_east", "GEO2_west", "lat", "lon"
  )
  start_sfd <- complete_data(start_sfd, second_required)
  during_sfd <- complete_data(during_sfd, second_required)
  if (nrow(start_sfd) < 100L || uniqueN(start_sfd$geo2_pair) < 50L) {
    stop("Insufficient support for the second-step migration SFD.")
  }

  formula_1 <- d_origin_period_effect ~ d_mean_salinity |
    YEAR + geo2_pair
  formula_2 <- d_origin_period_effect ~
    d_mean_salinity + d_gdd_large + d_kdd_large + d_sm_season_large |
    YEAR + geo2_pair
  capture_vcov_warnings <- function(expression) {
    messages <- character()
    value <- withCallingHandlers(
      eval.parent(substitute(expression)),
      warning = function(warning_condition) {
        messages <<- c(messages, conditionMessage(warning_condition))
        invokeRestart("muffleWarning")
      }
    )
    list(
      value = value,
      messages = messages,
      psd_adjusted = any(grepl(
        "positive semi-definite", messages, fixed = TRUE
      ))
    )
  }
  fit_1 <- capture_vcov_warnings(fixest::feols(
      formula_1,
      data = start_sfd,
      vcov = make_conley(cutoff = 200),
      panel.id = ~geo2_pair + YEAR,
      notes = FALSE
    ))
  fit_2 <- capture_vcov_warnings(fixest::feols(
      formula_2,
      data = start_sfd,
      vcov = make_conley(cutoff = 200),
      panel.id = ~geo2_pair + YEAR,
      notes = FALSE
    ))
  model_1 <- fit_1$value
  model_2 <- fit_2$value
  if (stats::nobs(model_1) != stats::nobs(model_2)) {
    stop("The two main second-step specifications use different samples.")
  }

  fit_during <- capture_vcov_warnings(fixest::feols(
      formula_2,
      data = during_sfd,
      vcov = make_conley(cutoff = 200),
      panel.id = ~geo2_pair + YEAR,
      notes = FALSE
    ))
  fit_250 <- capture_vcov_warnings(summary(
    model_2, vcov = make_conley(cutoff = 250)
  ))
  fit_300 <- capture_vcov_warnings(summary(
    model_2, vcov = make_conley(cutoff = 300)
  ))
  fit_two_way <- capture_vcov_warnings(summary(
    model_2, vcov = ~GEO2_east + GEO2_west
  ))
  during_model <- fit_during$value
  model_250 <- fit_250$value
  model_300 <- fit_300$value
  model_two_way <- fit_two_way$value

  coefficients <- rbindlist(list(
    extract_two_step_coefficient(model_1, "Spec. 1", "Conley 200 km"),
    extract_two_step_coefficient(model_2, "Spec. 2", "Conley 200 km"),
    extract_two_step_coefficient(model_250, "Spec. 2", "Conley 250 km"),
    extract_two_step_coefficient(model_300, "Spec. 2", "Conley 300 km"),
    extract_two_step_coefficient(model_two_way, "Spec. 2", "Two-way origin zones"),
    extract_two_step_coefficient(
      during_model, "During-window robustness", "Conley 200 km"
    )
  ), fill = TRUE)
  coefficients[, exposure := c(
    rep("Start-of-window", 5L), "During-window mean"
  )]
  coefficients[, vcov_psd_adjusted := c(
    fit_1$psd_adjusted,
    fit_2$psd_adjusted,
    fit_250$psd_adjusted,
    fit_300$psd_adjusted,
    fit_two_way$psd_adjusted,
    fit_during$psd_adjusted
  )]
  fwrite(
    coefficients,
    file.path(paths$out_dir, "FullRevision_RuralMigration_TwoStep_Coefficients.csv")
  )
  fwrite(
    origin_period_effects,
    file.path(paths$out_dir, "FullRevision_RuralMigration_OriginPeriod_Effects.csv")
  )
  fwrite(
    start_sfd,
    file.path(paths$out_dir, "FullRevision_RuralMigration_TwoStep_SFD_Sample.csv")
  )

  first_diagnostics <- data.table(
    Metric = c(
      "Rows before PPML fixed-effect removals",
      "Rows used in first-step PPML",
      "Zero-flow rows used in first-step PPML",
      "Positive-flow rows used in first-step PPML",
      "Origin-destination corridors",
      "Origin-period fixed effects",
      "Destination-period fixed effects",
      "Second-step observations",
      "Second-step contiguous pairs",
      "Second-step census periods"
    ),
    Value = c(
      formatC(nrow(first_data), format = "d", big.mark = ","),
      formatC(stats::nobs(first_stage), format = "d", big.mark = ","),
      formatC(sum(first_model_data$migrant_flow == 0), format = "d", big.mark = ","),
      formatC(sum(first_model_data$migrant_flow > 0), format = "d", big.mark = ","),
      formatC(uniqueN(first_model_data$dyad), format = "d", big.mark = ","),
      formatC(nrow(origin_period_effects), format = "d", big.mark = ","),
      formatC(length(fixest::fixef(first_stage)[["dest^YEAR"]]), format = "d", big.mark = ","),
      formatC(stats::nobs(model_2), format = "d", big.mark = ","),
      formatC(uniqueN(start_sfd$geo2_pair), format = "d", big.mark = ","),
      paste(sort(unique(start_sfd$YEAR)), collapse = ", ")
    )
  )
  write_latex_df(
    first_diagnostics,
    file.path(
      paths$out_dir,
      "FullRevision_RuralMigration_TwoStep_FirstStage_Diagnostics.tex"
    ),
    "Two-Step Migration Sample and First-Stage Diagnostics",
    "tab:full_revision_rural_migration_two_step_diagnostics",
    note = paste(
      "La première étape est un PPML bilatéral avec effets fixes de corridor, d'origine par recensement et de destination par recensement.",
      "Les flux nuls sont conservés à l'intérieur des corridors ayant au moins un flux positif; les corridors toujours nuls ne sont pas informatifs avec des effets fixes de corridor.",
      "La seconde étape compare les effets origine--période entre zones contiguës appartenant aux mêmes restrictions d'État et de bassin hydrographique."
    ),
    size = "\\small"
  )
  fwrite(
    first_diagnostics,
    file.path(
      paths$out_dir,
      "FullRevision_RuralMigration_TwoStep_FirstStage_Diagnostics.csv"
    )
  )

  fixest::setFixest_dict(c(
    d_origin_period_effect = "SFD origin-period emigration propensity",
    d_mean_salinity = "Start-of-window mean salinity (dS/m)",
    d_gdd_large = "Start-of-window GDD",
    d_kdd_large = "Start-of-window KDD",
    d_sm_season_large = "Start-of-window soil moisture"
  ), reset = TRUE)
  main_table <- file.path(
    paths$out_dir, "FullRevision_RuralMigration_TwoStep_Main.tex"
  )
  fixest::etable(
    model_1, model_2,
    tex = TRUE,
    file = main_table,
    replace = TRUE,
    title = "Two-Step Gravity-SFD Estimate of Rural Outmigration Propensity",
    label = "tab:full_revision_rural_migration_two_step_main",
    fitstat = ~n + r2,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    notes = paste(
      "Notes: la première étape estime les flux bilatéraux pondérés par PPML avec la population initiale en offset et des effets fixes de corridor, d'origine--recensement et de destination--recensement.",
      "La variable dépendante de la seconde étape est la différence est--ouest entre les effets origine--recensement estimés.",
      "La salinité et les contrôles sont mesurés en 1985, 1995 et 2005, avant les fenêtres migratoires correspondantes.",
      "Les deux colonnes comprennent des effets fixes de période et de paire contiguë; la colonne 2, préférée, ajoute GDD, KDD et humidité du sol.",
      "Les erreurs standards entre parenthèses sont corrigées par Conley à 200 km."
    )
  )
  style_fixest_tex(
    main_table,
    size = "\\small",
    tabcolsep = "5pt",
    arraystretch = "0.92"
  )
  table_lines <- readLines(main_table, warn = FALSE, encoding = "UTF-8")
  table_lines <- gsub(
    "geo2\\_pair", "Contiguous-pair fixed effects", table_lines, fixed = TRUE
  )
  table_lines <- gsub("YEAR", "Census-period fixed effects", table_lines, fixed = TRUE)
  writeLines(table_lines, main_table, useBytes = TRUE)

  robustness <- coefficients[
    specification != "Spec. 1",
    .(
      Exposure = exposure,
      Inference = inference,
      Coefficient = paste0(
        fmt(estimate, 4),
        fifelse(vcov_psd_adjusted, "", stars(p))
      ),
      `Std. error` = paste0("(", fmt(se, 4), ")"),
      `p-value` = fmt(p, 3),
      Observations = formatC(observations, format = "d", big.mark = ","),
      `PSD fix` = fifelse(vcov_psd_adjusted, "Yes", "No")
    )
  ]
  write_latex_df(
    robustness,
    file.path(
      paths$out_dir,
      "FullRevision_RuralMigration_TwoStep_Inference_Robustness.tex"
    ),
    "Two-Step Rural Migration: Exposure and Inference Robustness",
    "tab:full_revision_rural_migration_two_step_robustness",
    note = paste(
      "Toutes les lignes reprennent la spécification préférée avec contrôles climatiques et effets fixes de période et de paire contiguë.",
      "Les trois premières lignes ne modifient que le rayon de Conley; le coefficient reste donc identique.",
      "La colonne PSD adjustment signale une matrice de covariance non positive semi-définie corrigée numériquement; l'inférence correspondante ne doit pas être interprétée comme une robustesse concluante.",
      "La dernière ligne remplace la salinité mesurée avant la fenêtre par sa moyenne pendant la fenêtre migratoire."
    ),
    size = "\\scriptsize"
  )
  robustness_file <- file.path(
    paths$out_dir,
    "FullRevision_RuralMigration_TwoStep_Inference_Robustness.tex"
  )
  robustness_lines <- readLines(
    robustness_file, warn = FALSE, encoding = "UTF-8"
  )
  robustness_lines <- gsub(
    "\\\\setlength\\{\\\\tabcolsep\\}\\{3pt\\}",
    "\\\\setlength{\\\\tabcolsep}{1.5pt}",
    robustness_lines
  )
  writeLines(robustness_lines, robustness_file, useBytes = TRUE)

  normalization_test_data <- copy(start_sfd)
  set.seed(271828L)
  origin_shift <- data.table(
    GEO2_BR = unique(c(
      normalization_test_data$GEO2_east,
      normalization_test_data$GEO2_west
    )),
    arbitrary_shift = stats::rnorm(length(unique(c(
      normalization_test_data$GEO2_east,
      normalization_test_data$GEO2_west
    ))))
  )
  normalization_test_data <- merge(
    normalization_test_data,
    origin_shift,
    by.x = "GEO2_east",
    by.y = "GEO2_BR",
    all.x = TRUE
  )
  setnames(normalization_test_data, "arbitrary_shift", "shift_east")
  normalization_test_data <- merge(
    normalization_test_data,
    origin_shift,
    by.x = "GEO2_west",
    by.y = "GEO2_BR",
    all.x = TRUE
  )
  setnames(normalization_test_data, "arbitrary_shift", "shift_west")
  normalization_test_data[, shifted_outcome :=
    d_origin_period_effect + shift_east - shift_west]
  shifted_model <- fixest::feols(
    shifted_outcome ~
      d_mean_salinity + d_gdd_large + d_kdd_large + d_sm_season_large |
      YEAR + geo2_pair,
    data = normalization_test_data,
    vcov = make_conley(cutoff = 200),
    notes = FALSE
  )
  normalization_difference <- abs(
    stats::coef(shifted_model)["d_mean_salinity"] -
      stats::coef(model_2)["d_mean_salinity"]
  )
  if (!is.finite(normalization_difference) || normalization_difference > 1e-8) {
    stop("Pair fixed effects did not absorb the origin-effect normalization.")
  }

  write_status(
    "Two-step migration model completed: preferred beta = ",
    fmt(stats::coef(model_2)["d_mean_salinity"], 4),
    "; N = ", stats::nobs(model_2), "."
  )
  invisible(list(
    first_stage = first_stage,
    first_data = first_model_data,
    origin_period_effects = origin_period_effects,
    pair_map = pair_map,
    start_sfd = start_sfd,
    during_sfd = during_sfd,
    models = list(specification_1 = model_1, preferred = model_2),
    robustness_models = list(
      conley_250 = model_250,
      conley_300 = model_300,
      two_way = model_two_way,
      during_window = during_model
    ),
    coefficients = coefficients,
    diagnostics = first_diagnostics
  ))
}

make_ind_crop_migration_analysis <- function(panel) {
  write_status("Estimating pooled and heterogeneous crop-industry migration outcomes (IPUMS IND).")
  crop_labels <- c(
    corn = "Corn", rice = "Rice", cassava = "Cassava",
    soy = "Soybeans", sugarcane = "Sugarcane"
  )
  crops <- names(crop_labels)
  flow_columns <- paste0("current_", crops, "_migrant_flow")
  missing <- setdiff(
    c(flow_columns, "mean_salinity_orig", "mean_salinity_dest"),
    names(panel)
  )
  if (length(missing) > 0L) {
    stop("Missing IND crop-migration variables: ", paste(missing, collapse = ", "))
  }

  id_columns <- setdiff(names(panel), flow_columns)
  d <- melt(
    copy(panel),
    id.vars = id_columns,
    measure.vars = flow_columns,
    variable.name = "flow_variable",
    value.name = "crop_migrant_flow"
  )
  d[, crop := sub("^current_(.*)_migrant_flow$", "\\1", as.character(flow_variable))]
  d[, flow_variable := NULL]
  thresholds <- crop_salinity_thresholds(include_beans = FALSE)
  d[, threshold_ds_m := unname(thresholds[crop])]
  d[, `:=`(
    origin_crop_excess = pmax(mean_salinity_orig - threshold_ds_m, 0),
    destination_crop_excess = pmax(mean_salinity_dest - threshold_ds_m, 0),
    crop = factor(crop, levels = crops)
  )]
  weather <- intersect(c(
    "z_gdd_large_orig", "z_kdd_large_orig", "z_sm_season_large_orig",
    "z_gdd_large_dest", "z_kdd_large_dest", "z_sm_season_large_dest"
  ), names(d))
  required <- c(
    "crop_migrant_flow", "origin_population", "dyad", "orig", "dest",
    "YEAR", "state_orig", "state_dest", "crop", "origin_crop_excess",
    "destination_crop_excess", "origin_mean_income", weather
  )
  d <- complete_data(d, required)
  d <- d[
    is.finite(crop_migrant_flow) & crop_migrant_flow >= 0 &
      is.finite(origin_population) & origin_population > 0
  ]
  if (nrow(d) < 100L) stop("Insufficient complete IND crop-migration observations.")

  common_terms <- c("origin_crop_excess", "destination_crop_excess", weather)
  pooled_formula_base <- paste0(
    "crop_migrant_flow ~ origin_crop_excess + destination_crop_excess",
    " | dyad + crop + state_orig^YEAR + state_dest^YEAR"
  )
  pooled_formula <- paste0(
    "crop_migrant_flow ~ ",
    paste(common_terms, collapse = " + "),
    " | dyad + crop + state_orig^YEAR + state_dest^YEAR"
  )
  pooled_model <- fixest::fepois(
    as.formula(pooled_formula),
    data = d,
    offset = ~log(origin_population),
    cluster = ~orig + dest,
    notes = FALSE
  )
  pooled_data <- d[fixest::obs(pooled_model)]
  pooled_model_base <- fixest::fepois(
    as.formula(pooled_formula_base),
    data = pooled_data,
    offset = ~log(origin_population),
    cluster = ~orig + dest,
    notes = FALSE
  )
  if (stats::nobs(pooled_model_base) != stats::nobs(pooled_model)) {
    stop("The two pooled crop-migration specifications do not use the same sample.")
  }

  extract_terms <- function(model, terms, extra = NULL) {
    beta <- stats::coef(model)
    standard_errors <- sqrt(diag(stats::vcov(model)))
    p_values <- 2 * stats::pnorm(-abs(beta / standard_errors))
    out <- rbindlist(lapply(terms, function(term_name) {
      data.table(
        term = term_name,
        estimate = unname(beta[term_name]),
        standard_error = unname(standard_errors[term_name]),
        p_value = unname(p_values[term_name])
      )
    }))
    if (!is.null(extra)) out <- cbind(extra, out)
    out
  }

  term_labels <- c(
    origin_crop_excess = "Origin crop-specific excess salinity (dS/m)",
    destination_crop_excess = "Destination crop-specific excess salinity (dS/m)",
    z_gdd_large_orig = "Origin GDD (std.)",
    z_kdd_large_orig = "Origin KDD (std.)",
    z_sm_season_large_orig = "Origin soil moisture (std.)",
    z_gdd_large_dest = "Destination GDD (std.)",
    z_kdd_large_dest = "Destination KDD (std.)",
    z_sm_season_large_dest = "Destination soil moisture (std.)"
  )
  pooled_coefficients <- rbindlist(list(
    cbind(data.table(specification = 1L), extract_terms(
      pooled_model_base,
      c("origin_crop_excess", "destination_crop_excess")
    )),
    cbind(data.table(specification = 2L), extract_terms(pooled_model, common_terms))
  ), fill = TRUE)
  pooled_support <- data.table(
    observations_before_ppml_fe = nrow(d),
    observations = stats::nobs(pooled_model),
    observations_removed_by_ppml_fe = nrow(d) - stats::nobs(pooled_model),
    positive_flows = sum(pooled_data$crop_migrant_flow > 0),
    zero_flow_share = mean(pooled_data$crop_migrant_flow == 0),
    origins = uniqueN(pooled_data$orig),
    destinations = uniqueN(pooled_data$dest),
    dyads = uniqueN(pooled_data$dyad),
    crops = uniqueN(pooled_data$crop),
    mean_origin_excess = mean(pooled_data$origin_crop_excess),
    sd_origin_excess = stats::sd(pooled_data$origin_crop_excess),
    mean_destination_excess = mean(pooled_data$destination_crop_excess),
    sd_destination_excess = stats::sd(pooled_data$destination_crop_excess)
  )
  fwrite(
    pooled_coefficients,
    file.path(paths$out_dir, "FullRevision_IND_Crop_Migration_Pooled_Coefficients.csv")
  )
  fwrite(
    pooled_support,
    file.path(paths$out_dir, "FullRevision_IND_Crop_Migration_Pooled_Support.csv")
  )

  pooled_cell <- function(specification, term_name, standard_error = FALSE) {
    target_specification <- specification
    target_term <- term_name
    row <- pooled_coefficients[
      get("specification") == target_specification & term == target_term
    ]
    if (nrow(row) == 0L || !is.finite(row$estimate)) return("")
    if (standard_error) return(paste0("(", fmt(row$standard_error, 4), ")"))
    paste0(fmt(row$estimate, 4), stars(row$p_value))
  }
  pooled_table_lines <- c(
    "\\begin{table}[!htbp]",
    "\\color{red}",
    "\\centering",
    "\\caption{Crop-Industry Migration and Crop-Specific Excess Salinity: Pooled PPML}",
    "\\label{tab:full_revision_ind_crop_migration_pooled}",
    "\\small",
    "\\setlength{\\tabcolsep}{6pt}",
    "\\renewcommand{\\arraystretch}{0.90}",
    "\\begin{tabular}{lrr}",
    "\\toprule",
    "Variable & Spec. 1 & Spec. 2\\\\",
    "\\midrule"
  )
  for (term_name in common_terms) {
    pooled_table_lines <- c(
      pooled_table_lines,
      paste(
        latex_escape(unname(term_labels[term_name])),
        pooled_cell(1L, term_name),
        pooled_cell(2L, term_name),
        sep = " & "
      ),
      "\\\\",
      paste(
        "",
        pooled_cell(1L, term_name, TRUE),
        pooled_cell(2L, term_name, TRUE),
        sep = " & "
      ),
      "\\\\"
    )
  }
  pooled_table_lines <- c(
    pooled_table_lines,
    "\\midrule",
    "Crop fixed effects & Yes & Yes\\\\",
    "Dyad fixed effects & Yes & Yes\\\\",
    "Origin-state-by-census fixed effects & Yes & Yes\\\\",
    "Destination-state-by-census fixed effects & Yes & Yes\\\\",
    paste0(
      "Observations & ",
      formatC(stats::nobs(pooled_model_base), format = "d", big.mark = ","),
      " & ",
      formatC(stats::nobs(pooled_model), format = "d", big.mark = ","),
      "\\\\"
    ),
    paste0(
      "Squared correlation & ",
      fmt(fixest::fitstat(pooled_model_base, "sq.cor")$sq.cor, 3),
      " & ",
      fmt(fixest::fitstat(pooled_model, "sq.cor")$sq.cor, 3),
      "\\\\"
    ),
    paste0(
      "Pseudo R2 & ",
      fmt(fixest::fitstat(pooled_model_base, "pr2")$pr2, 3),
      " & ",
      fmt(fixest::fitstat(pooled_model, "pr2")$pr2, 3),
      "\\\\"
    ),
    "\\bottomrule",
    "\\end{tabular}",
    "\\par\\addvspace{0.5ex}",
    "\\parbox{0.96\\linewidth}{\\footnotesize\\textit{Notes:} ",
    latex_escape(paste(
      "The observation is an origin-destination-census-crop cell for corn, rice, cassava, soybeans or sugarcane.",
      "The dependent variable is the IPUMS-weighted bilateral migrant flow whose current destination industry is the indicated crop; the logarithm of previous-census origin population is an offset.",
      "Origin and destination treatments are crop-specific excess salinity in dS/m during the five years preceding each census.",
      "A single origin slope and a single destination slope are imposed across crops.",
      "Both specifications include crop, dyad, origin-state-by-census and destination-state-by-census fixed effects.",
      "Spec. 2 is preferred and adds GDD, KDD and soil moisture at origin and destination.",
      "Both columns use the same sample and standard errors are two-way clustered by origin and destination.",
      "Soybean rows contribute to crop and climate parameters but not to salinity slopes because excess salinity is zero in their observed support."
    )),
    "}",
    "\\end{table}"
  )
  writeLines(
    pooled_table_lines,
    file.path(paths$out_dir, "FullRevision_IND_Crop_Migration_Pooled.tex"),
    useBytes = TRUE
  )

  for (cr in crops) {
    d[, paste0("origin_excess_", cr) := fifelse(crop == cr, origin_crop_excess, 0)]
    d[, paste0("destination_excess_", cr) := fifelse(crop == cr, destination_crop_excess, 0)]
  }
  origin_terms <- paste0("origin_excess_", crops)
  destination_terms <- paste0("destination_excess_", crops)
  heterogeneity_terms <- c(origin_terms, destination_terms, weather)
  heterogeneity_formula <- paste0(
    "crop_migrant_flow ~ ", paste(heterogeneity_terms, collapse = " + "),
    " | dyad + crop + state_orig^YEAR + state_dest^YEAR"
  )
  heterogeneity_model <- fixest::fepois(
    as.formula(heterogeneity_formula),
    data = d,
    offset = ~log(origin_population),
    cluster = ~orig + dest,
    notes = FALSE
  )
  heterogeneity_data <- d[fixest::obs(heterogeneity_model)]
  if (!identical(fixest::obs(pooled_model), fixest::obs(heterogeneity_model))) {
    stop("Pooled and crop-heterogeneity migration models do not use the same rows.")
  }

  treatment_coefficients <- rbindlist(lapply(crops, function(cr) {
    rbindlist(lapply(c(
      Origin = paste0("origin_excess_", cr),
      Destination = paste0("destination_excess_", cr)
    ), function(term_name) {
      extract_terms(
        heterogeneity_model,
        term_name,
        data.table(
          crop = cr,
          crop_label = crop_labels[[cr]],
          exposure_location = if (startsWith(term_name, "origin")) "Origin" else "Destination"
        )
      )
    }))
  }))
  control_coefficients <- extract_terms(heterogeneity_model, weather)
  crop_support <- heterogeneity_data[, .(
    observations = .N,
    positive_flows = sum(crop_migrant_flow > 0),
    zero_flow_share = mean(crop_migrant_flow == 0),
    origins = uniqueN(orig),
    destinations = uniqueN(dest),
    dyads = uniqueN(dyad),
    mean_origin_excess = mean(origin_crop_excess),
    sd_origin_excess = stats::sd(origin_crop_excess),
    mean_destination_excess = mean(destination_crop_excess),
    sd_destination_excess = stats::sd(destination_crop_excess)
  ), by = .(crop)]
  crop_support[, crop_label := crop_labels[as.character(crop)]]

  equality_test <- function(terms, label) {
    beta <- stats::coef(heterogeneity_model)
    vc <- stats::vcov(heterogeneity_model)
    present <- terms[terms %in% names(beta)]
    if (length(present) < 2L) return(data.table(test = label, statistic = NA_real_, df = NA_integer_, p_value = NA_real_))
    restriction <- matrix(0, nrow = length(present) - 1L, ncol = length(beta), dimnames = list(NULL, names(beta)))
    for (i in seq_len(nrow(restriction))) {
      restriction[i, present[i + 1L]] <- 1
      restriction[i, present[1L]] <- -1
    }
    contrast <- as.numeric(restriction %*% beta)
    contrast_vcov <- restriction %*% vc %*% t(restriction)
    statistic <- as.numeric(t(contrast) %*% qr.solve(contrast_vcov, contrast))
    data.table(
      test = label, statistic = statistic, df = nrow(restriction),
      p_value = stats::pchisq(statistic, nrow(restriction), lower.tail = FALSE)
    )
  }
  tests <- rbindlist(list(
    equality_test(origin_terms, "Equal origin-excess slopes across crops"),
    equality_test(destination_terms, "Equal destination-excess slopes across crops")
  ))

  fwrite(
    treatment_coefficients,
    file.path(paths$out_dir, "FullRevision_IND_Crop_Migration_CropHeterogeneity_Coefficients.csv")
  )
  fwrite(
    control_coefficients,
    file.path(paths$out_dir, "FullRevision_IND_Crop_Migration_CropHeterogeneity_Controls.csv")
  )
  fwrite(
    crop_support,
    file.path(paths$out_dir, "FullRevision_IND_Crop_Migration_CropHeterogeneity_Support.csv")
  )
  fwrite(
    tests,
    file.path(paths$out_dir, "FullRevision_IND_Crop_Migration_CropHeterogeneity_EqualityTests.csv")
  )

  detailed_rows <- rbindlist(list(
    treatment_coefficients[, .(
      Variable = paste(exposure_location, crop_label, "excess salinity (dS/m)"),
      Estimate = ifelse(is.finite(estimate), paste0(fmt(estimate, 4), stars(p_value)), "Not identified"),
      `Standard error` = ifelse(is.finite(standard_error), paste0("(", fmt(standard_error, 4), ")"), ""),
      `p-value` = fmt(p_value, 3)
    )],
    control_coefficients[, .(
      Variable = unname(term_labels[term]),
      Estimate = paste0(fmt(estimate, 4), stars(p_value)),
      `Standard error` = paste0("(", fmt(standard_error, 4), ")"),
      `p-value` = fmt(p_value, 3)
    )],
    data.table(
      Variable = c(
        "Crop fixed effects", "Dyad fixed effects",
        "Origin-state-by-census fixed effects",
        "Destination-state-by-census fixed effects", "Observations"
      ),
      Estimate = c(
        "Yes", "Yes", "Yes", "Yes",
        formatC(stats::nobs(heterogeneity_model), format = "d", big.mark = ",")
      ),
      `Standard error` = "",
      `p-value` = ""
    )
  ), fill = TRUE)
  write_latex_df(
    detailed_rows,
    file.path(paths$out_dir, "FullRevision_IND_Crop_Migration_CropHeterogeneity_Detailed.tex"),
    "Crop-Industry Migration: Crop-Specific Slopes and Full Controls",
    "tab:full_revision_ind_crop_migration_heterogeneity_detailed",
    note = paste(
      "This secondary model uses exactly the same stacked sample, outcome, offset, controls and fixed effects as the pooled model, but interacts origin and destination excess salinity with crop.",
      "All crop slopes and weather-control coefficients are estimated jointly in one PPML regression.",
      "Precipitation and income are excluded. Standard errors are two-way clustered by origin and destination.",
      "Soybean salinity slopes are not identified because crop-specific excess salinity is zero throughout its observed support.",
      "Le secteur d'emploi est observé au recensement de destination et ne permet pas d'identifier le secteur dans lequel la personne travaillait avant sa migration."
    ),
    size = "\\footnotesize"
  )

  plot_data <- merge(
    treatment_coefficients,
    crop_support[, .(crop, sd_origin_excess, sd_destination_excess)],
    by = "crop", all.x = TRUE
  )
  plot_data[, treatment_sd := fifelse(
    exposure_location == "Origin", sd_origin_excess, sd_destination_excess
  )]
  plot_data[, `:=`(
    effect_percent = 100 * (exp(estimate * treatment_sd) - 1),
    ci90_low = 100 * (exp((estimate - 1.645 * standard_error) * treatment_sd) - 1),
    ci90_high = 100 * (exp((estimate + 1.645 * standard_error) * treatment_sd) - 1),
    ci95_low = 100 * (exp((estimate - 1.96 * standard_error) * treatment_sd) - 1),
    ci95_high = 100 * (exp((estimate + 1.96 * standard_error) * treatment_sd) - 1),
    crop_label = factor(crop_label, levels = unname(crop_labels)),
    exposure_location = factor(exposure_location, levels = c("Origin", "Destination"))
  )]
  fwrite(
    plot_data,
    file.path(paths$out_dir, "FullRevision_IND_Crop_Migration_CropHeterogeneity_PlotData.csv")
  )
  plot_complete <- plot_data[is.finite(effect_percent) & is.finite(ci95_low) & is.finite(ci95_high)]
  plot_complete[, y_position := as.numeric(crop_label) + c(
    Origin = -0.14,
    Destination = 0.14
  )[as.character(exposure_location)]]
  figure <- ggplot(plot_complete, aes(effect_percent, y_position, color = exposure_location)) +
    geom_vline(xintercept = 0, linewidth = 0.45, color = "grey45", linetype = "dashed") +
    geom_segment(
      aes(x = ci95_low, xend = ci95_high, yend = y_position),
      linewidth = 0.55
    ) +
    geom_segment(
      aes(x = ci90_low, xend = ci90_high, yend = y_position),
      linewidth = 1.10
    ) +
    geom_point(size = 2.5) +
    scale_color_manual(values = c(Origin = "#2F6F8F", Destination = "#D47A1F"), name = NULL) +
    scale_y_continuous(
      breaks = seq_along(levels(plot_complete$crop_label)),
      labels = levels(plot_complete$crop_label)
    ) +
    labs(
      x = "Change in expected migration rate for a one-SD increase\nin crop-specific excess salinity (%)",
      y = NULL
    ) +
    theme_minimal(base_size = 12.5) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text = element_text(size = 11, color = "black"),
      axis.title.x = element_text(size = 11.5),
      legend.text = element_text(size = 11),
      plot.margin = margin(8, 10, 8, 10)
    )
  ggsave(
    file.path(paths$out_dir, "FullRevision_IND_Crop_Migration_CropHeterogeneity.pdf"),
    figure, width = 7.6, height = 4.8
  )
  ggsave(
    file.path(paths$out_dir, "FullRevision_IND_Crop_Migration_CropHeterogeneity.png"),
    figure, width = 7.6, height = 4.8, dpi = 320
  )

  income_check <- d[, .(income_values = uniqueN(origin_mean_income)), by = .(YEAR, orig)]
  if (any(income_check$income_values != 1L)) {
    stop("Baseline origin income is not unique within origin-year cells in the IND crop sample.")
  }
  income_map <- unique(d[, .(YEAR, orig, origin_mean_income)])
  income_map[, income_quartile := paste0(
    "Q", pmin(4L, ceiling(4 * frank(origin_mean_income, ties.method = "average") / .N))
  ), by = YEAR]
  d_income <- merge(
    d,
    income_map[, .(YEAR, orig, income_quartile)],
    by = c("YEAR", "orig"), all = FALSE, sort = FALSE
  )
  quartiles <- paste0("Q", seq_len(4L))
  income_models <- list()
  income_coefficient_rows <- list()
  income_support_rows <- list()
  for (q in quartiles) {
    d_q <- d_income[income_quartile == q]
    if (nrow(d_q) == 0L) stop("Empty IND crop-migration income quartile: ", q)
    model_q <- fixest::fepois(
      as.formula(pooled_formula),
      data = d_q,
      offset = ~log(origin_population),
      cluster = ~orig + dest,
      notes = FALSE
    )
    income_models[[q]] <- model_q
    model_data_q <- d_q[fixest::obs(model_q)]
    income_coefficient_rows[[q]] <- cbind(
      data.table(income_quartile = q),
      extract_terms(model_q, common_terms)
    )
    income_support_rows[[q]] <- data.table(
      income_quartile = q,
      observations_before_ppml_fe = nrow(d_q),
      observations = stats::nobs(model_q),
      observations_removed_by_ppml_fe = nrow(d_q) - stats::nobs(model_q),
      positive_flows = sum(model_data_q$crop_migrant_flow > 0),
      zero_flow_share = mean(model_data_q$crop_migrant_flow == 0),
      origins = uniqueN(model_data_q$orig),
      destinations = uniqueN(model_data_q$dest),
      dyads = uniqueN(model_data_q$dyad),
      sd_origin_excess = stats::sd(model_data_q$origin_crop_excess),
      sd_destination_excess = stats::sd(model_data_q$destination_crop_excess),
      mean_origin_income = mean(model_data_q$origin_mean_income)
    )
  }
  income_coefficients <- rbindlist(income_coefficient_rows)
  income_support <- rbindlist(income_support_rows)
  fwrite(
    income_coefficients,
    file.path(paths$out_dir, "FullRevision_IND_Crop_Migration_IncomeQuartile_Coefficients.csv")
  )
  fwrite(
    income_support,
    file.path(paths$out_dir, "FullRevision_IND_Crop_Migration_IncomeQuartile_Support.csv")
  )

  estimate_cell <- function(q, term_name) {
    row <- income_coefficients[income_quartile == q & term == term_name]
    if (nrow(row) == 0L || !is.finite(row$estimate)) return("")
    paste0(fmt(row$estimate, 4), stars(row$p_value))
  }
  se_cell <- function(q, term_name) {
    row <- income_coefficients[income_quartile == q & term == term_name]
    if (nrow(row) == 0L || !is.finite(row$standard_error)) return("")
    paste0("(", fmt(row$standard_error, 4), ")")
  }
  support_cell <- function(q, variable, digits = 0L, percent = FALSE) {
    value <- income_support[income_quartile == q][[variable]]
    if (length(value) != 1L || !is.finite(value)) return("")
    if (percent) return(fmt(100 * value, 1))
    if (digits == 0L) return(formatC(value, format = "d", big.mark = ","))
    fmt(value, digits)
  }
  table_lines <- c(
    "\\begin{table}[!htbp]",
    "\\color{red}",
    "\\centering",
    "\\caption{Crop-Industry Migration: Separate Pooled Regressions by Baseline-Income Quartile}",
    "\\label{tab:full_revision_ind_crop_migration_income_quartiles}",
    "\\footnotesize",
    "\\setlength{\\tabcolsep}{4pt}",
    "\\renewcommand{\\arraystretch}{0.88}",
    "\\begin{tabular}{lrrrr}",
    "\\toprule",
    "Variable & Q1 (poorest) & Q2 & Q3 & Q4 (richest)\\\\",
    "\\midrule"
  )
  for (term_name in common_terms) {
    table_lines <- c(
      table_lines,
      paste(
        c(
          latex_escape(unname(term_labels[term_name])),
          vapply(quartiles, estimate_cell, character(1), term_name = term_name)
        ),
        collapse = " & "
      ),
      "\\\\",
      paste(
        c("", vapply(quartiles, se_cell, character(1), term_name = term_name)),
        collapse = " & "
      ),
      "\\\\"
    )
  }
  table_lines <- c(
    table_lines,
    "\\midrule",
    "Crop fixed effects & Yes & Yes & Yes & Yes\\\\",
    "Dyad fixed effects & Yes & Yes & Yes & Yes\\\\",
    "Origin-state-by-window fixed effects & Yes & Yes & Yes & Yes\\\\",
    "Destination-state-by-window fixed effects & Yes & Yes & Yes & Yes\\\\",
    "\\midrule",
    paste(
      c("Observations", vapply(quartiles, support_cell, character(1), variable = "observations")),
      collapse = " & "
    ),
    "\\\\",
    paste(
      c("Positive flows", vapply(quartiles, support_cell, character(1), variable = "positive_flows")),
      collapse = " & "
    ),
    "\\\\",
    paste(
      c("Zero flows (percent)", vapply(
        quartiles, support_cell, character(1), variable = "zero_flow_share", percent = TRUE
      )),
      collapse = " & "
    ),
    "\\\\",
    paste(
      c("Origins", vapply(quartiles, support_cell, character(1), variable = "origins")),
      collapse = " & "
    ),
    "\\\\",
    paste(
      c("Dyads", vapply(quartiles, support_cell, character(1), variable = "dyads")),
      collapse = " & "
    ),
    "\\\\",
    "\\bottomrule",
    "\\end{tabular}",
    "\\par\\addvspace{0.5ex}",
    "\\parbox{0.96\\linewidth}{\\footnotesize\\textit{Notes:} ",
    latex_escape(paste(
      "Each column is a separate PPML regression using the same stacked crop-industry design as the pooled model but restricted to the indicated previous-census origin-income quartile.",
      "Quartiles are formed within census years; Q1 is the poorest and Q4 the richest.",
      "The origin and destination treatments are crop-specific excess salinity in dS/m, and all five crop industries enter each regression with crop fixed effects.",
      "All coefficients on GDD, KDD and soil moisture are displayed. Precipitation and income are excluded.",
      "Standard errors are two-way clustered by origin and destination.",
      "PPML may remove fixed-effect groups with only zero flows, so differences across columns are descriptive rather than formal equality tests."
    )),
    "}",
    "\\end{table}"
  )
  writeLines(
    table_lines,
    file.path(paths$out_dir, "FullRevision_IND_Crop_Migration_IncomeQuartile_Heterogeneity.tex"),
    useBytes = TRUE
  )

  invisible(list(
    pooled_model = pooled_model,
    pooled_coefficients = pooled_coefficients,
    pooled_support = pooled_support,
    heterogeneity_model = heterogeneity_model,
    heterogeneity_coefficients = treatment_coefficients,
    heterogeneity_support = crop_support,
    heterogeneity_tests = tests,
    income_models = income_models,
    income_coefficients = income_coefficients,
    income_support = income_support
  ))
}

make_ind_crop_labor_analysis <- function(aggregates, exposure_during) {
  write_status("Estimating crop-specific agricultural employment responses using IPUMS IND.")
  crop_labels <- c(
    corn = "Corn", rice = "Rice", cassava = "Cassava",
    soy = "Soybeans", sugarcane = "Sugarcane"
  )
  crops <- names(crop_labels)
  share_columns <- paste0(crops, "_worker_share")
  rate_columns <- paste0(crops, "_employment_rate")
  missing <- setdiff(c(share_columns, rate_columns), names(aggregates$current_region))
  if (length(missing) > 0L) stop("Missing IND crop-employment variables: ", paste(missing, collapse = ", "))

  labor <- rbindlist(lapply(crops, function(cr) {
    copy(aggregates$current_region)[, .(
      YEAR = as.character(YEAR),
      GEO2_BR = trimws(as.character(region)),
      crop = cr,
      crop_worker_share = get(paste0(cr, "_worker_share")),
      crop_employment_rate = get(paste0(cr, "_employment_rate"))
    )]
  }))
  baseline <- labor[YEAR == "1980", .(
    GEO2_BR, crop, baseline_crop_worker_share = crop_worker_share
  )]
  baseline[, baseline_bin := pmin(
    5L,
    ceiling(5 * frank(baseline_crop_worker_share, ties.method = "average") / .N)
  ), by = crop]
  baseline[, crop_baseline_bin := factor(paste(crop, baseline_bin, sep = "__"))]

  exposure <- copy(exposure_during)
  exposure[, `:=`(YEAR = as.character(YEAR), GEO2_BR = trimws(as.character(GEO2_BR)))]
  d <- merge(labor, exposure, by = c("GEO2_BR", "YEAR"), all = FALSE)
  d <- merge(d, baseline, by = c("GEO2_BR", "crop"), all.x = TRUE)
  thresholds <- crop_salinity_thresholds(include_beans = FALSE)
  d[, crop_excess := pmax(mean_salinity - unname(thresholds[crop]), 0)]
  setorder(d, GEO2_BR, crop, YEAR)
  fd_vars <- c(
    "crop_worker_share", "crop_employment_rate", "crop_excess",
    "gdd_large", "kdd_large", "sm_season_large"
  )
  d[, paste0(fd_vars, "_lag") := shift(.SD, 1L), by = .(GEO2_BR, crop), .SDcols = fd_vars]
  for (v in fd_vars) d[, paste0("delta_", v) := get(v) - get(paste0(v, "_lag"))]
  for (cr in crops) {
    d[, paste0("delta_excess_", cr) := fifelse(crop == cr, delta_crop_excess, 0)]
  }
  treatment_terms <- paste0("delta_excess_", crops)
  weather <- c("delta_gdd_large", "delta_kdd_large", "delta_sm_season_large")
  d <- complete_data(d, c(
    "delta_crop_worker_share", "delta_crop_employment_rate", treatment_terms,
    weather, "crop", "crop_baseline_bin", "YEAR", "state", "lat", "lon", "GEO2_BR"
  ))
  if (nrow(d) < 100L) stop("Insufficient complete IND crop-employment observations.")

  outcome_definitions <- c(
    delta_crop_worker_share = "Crop-industry share among workers",
    delta_crop_employment_rate = "Crop-industry workers / population 15+"
  )
  all_results <- list()
  all_models <- list()
  vc <- make_conley(cutoff = 200)
  for (outcome in names(outcome_definitions)) {
    models <- list()
    for (spec in 1:3) {
      controls <- if (spec == 1L) character() else weather
      fixed_effects <- if (spec == 3L) {
        "crop^YEAR + state^YEAR + crop_baseline_bin^YEAR"
      } else {
        "crop^YEAR + state^YEAR"
      }
      formula_text <- paste0(
        outcome, " ~ ", paste(c(treatment_terms, controls), collapse = " + "),
        " | ", fixed_effects
      )
      models[[spec]] <- fixest::feols(
        as.formula(formula_text), data = d, vcov = vc, notes = FALSE
      )
    }
    rows <- rbindlist(lapply(seq_along(models), function(spec) {
      model <- models[[spec]]
      ct <- as.data.table(fixest::coeftable(model), keep.rownames = "term")
      se_col <- intersect(c("Std. Error", "Std..Error", "Std...Error"), names(ct))[1]
      p_col <- intersect(c("Pr(>|t|)", "Pr...t..", "Pr(>|z|)", "Pr...z.."), names(ct))[1]
      rbindlist(lapply(crops, function(cr) {
        target_term <- paste0("delta_excess_", cr)
        z <- ct[get("term") == target_term]
        if (nrow(z) != 1L) {
          return(data.table(
            outcome = outcome,
            outcome_label = outcome_definitions[[outcome]],
            crop = cr,
            crop_label = crop_labels[[cr]],
            specification = spec,
            estimate = NA_real_, standard_error = NA_real_, p_value = NA_real_,
            treatment_sd = stats::sd(d[crop == cr]$delta_crop_excess),
            one_sd_effect = NA_real_,
            observations = nrow(d[crop == cr]),
            model_observations = stats::nobs(model),
            geo2_regions = uniqueN(d[crop == cr]$GEO2_BR)
          ))
        }
        treatment_sd <- stats::sd(d[crop == cr]$delta_crop_excess)
        data.table(
          outcome = outcome,
          outcome_label = outcome_definitions[[outcome]],
          crop = cr,
          crop_label = crop_labels[[cr]],
          specification = spec,
          estimate = z$Estimate,
          standard_error = z[[se_col]],
          p_value = z[[p_col]],
          treatment_sd = treatment_sd,
          one_sd_effect = z$Estimate * treatment_sd,
          observations = nrow(d[crop == cr]),
          model_observations = stats::nobs(model),
          geo2_regions = uniqueN(d[crop == cr]$GEO2_BR)
        )
      }))
    }))
    all_results[[outcome]] <- rows
    all_models[[outcome]] <- models

    table_rows <- rows[, .(
      Crop = crop_label,
      Specification = paste0("Spec. ", specification),
      `Estimate (SE)` = ifelse(
        is.finite(estimate),
        paste0(fmt(estimate, 4), stars(p_value), " (", fmt(standard_error, 4), ")"),
        "Not identified"
      ),
      `Treatment SD` = fmt(treatment_sd, 4),
      `One-SD effect` = fmt(one_sd_effect, 4),
      Observations = formatC(observations, format = "d", big.mark = ","),
      `Weather controls` = ifelse(specification >= 2L, "Yes", "No"),
      `Initial-specialization FE` = ifelse(specification == 3L, "Yes", "No")
    )]
    suffix <- if (outcome == "delta_crop_worker_share") "WorkerShare" else "PopulationRate"
    write_latex_df(
      table_rows,
      file.path(paths$out_dir, paste0("FullRevision_IND_Crop_Employment_", suffix, ".tex")),
      paste0("Crop-Specific Employment and Excess Salinity: ", outcome_definitions[[outcome]]),
      paste0("tab:full_revision_ind_crop_employment_", tolower(suffix)),
      note = paste(
        "L'unité d'observation est une zone géographique harmonisée, une culture et un intervalle intercensitaire (1991--2000 ou 2000--2010).",
        "La variable dépendante est la variation temporelle, en points de pourcentage, de la mesure d'emploi associée à la culture indiquée.",
        "The treatment is the temporal change in crop-specific excess salinity in dS/m; the five crop slopes are estimated jointly.",
        "Treatment SD and one-SD effects are crop-specific; observations report crop-region-interval cells, while each pooled model contains 18,520 observations.",
        "All specifications include crop-by-census-year and state-by-census-year fixed effects. Preferred Spec. 2 adds changes in GDD, KDD and soil moisture.",
        "Spec. 3 additionally allows trends to differ across quintiles of the crop's 1980 employment share. Conley spatial standard errors use a 200 km cutoff.",
        "Le secteur d'emploi est observé au moment de chaque recensement et les mêmes travailleurs ne sont pas suivis dans le temps. Les haricots ne peuvent pas être isolés dans une catégorie comparable."
      ),
      size = "\\footnotesize",
      landscape = TRUE
    )
  }
  results <- rbindlist(all_results)
  fwrite(results, file.path(paths$out_dir, "FullRevision_IND_Crop_Employment_Coefficients.csv"))

  worker_plot_data <- copy(results[
    outcome == "delta_crop_worker_share" &
      is.finite(one_sd_effect) & is.finite(standard_error) & is.finite(treatment_sd)
  ])
  worker_plot_data[, `:=`(
    one_sd_se = standard_error * treatment_sd,
    ci90_low = one_sd_effect - 1.645 * standard_error * treatment_sd,
    ci90_high = one_sd_effect + 1.645 * standard_error * treatment_sd,
    ci95_low = one_sd_effect - 1.96 * standard_error * treatment_sd,
    ci95_high = one_sd_effect + 1.96 * standard_error * treatment_sd,
    crop_label = factor(crop_label, levels = unname(crop_labels)),
    specification_label = factor(
      paste0("Spec. ", specification),
      levels = c("Spec. 1", "Spec. 2", "Spec. 3")
    )
  )]
  worker_plot_data[, y_position := as.numeric(crop_label) + c(
    "Spec. 1" = -0.16,
    "Spec. 2" = 0,
    "Spec. 3" = 0.16
  )[as.character(specification_label)]]
  worker_figure <- ggplot(
    worker_plot_data,
    aes(one_sd_effect, y_position, color = specification_label, shape = specification_label)
  ) +
    geom_vline(xintercept = 0, linewidth = 0.45, color = "grey45", linetype = "dashed") +
    geom_segment(aes(x = ci95_low, xend = ci95_high, yend = y_position), linewidth = 0.55) +
    geom_segment(aes(x = ci90_low, xend = ci90_high, yend = y_position), linewidth = 1.10) +
    geom_point(size = 2.5) +
    scale_color_manual(
      values = c("Spec. 1" = "#6A51A3", "Spec. 2" = "#D95F02", "Spec. 3" = "#1B9E77"),
      name = NULL
    ) +
    scale_shape_manual(values = c("Spec. 1" = 15, "Spec. 2" = 17, "Spec. 3" = 16), name = NULL) +
    scale_y_continuous(
      breaks = seq_along(levels(worker_plot_data$crop_label)),
      labels = levels(worker_plot_data$crop_label)
    ) +
    labs(
      x = "Effect of a one-SD increase in crop-specific excess salinity\non crop-industry employment share (percentage points)",
      y = NULL
    ) +
    theme_minimal(base_size = 12.5) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text = element_text(size = 11, color = "black"),
      axis.title.x = element_text(size = 11.5),
      legend.text = element_text(size = 11),
      plot.margin = margin(8, 10, 8, 10)
    )
  ggsave(
    file.path(paths$out_dir, "FullRevision_IND_Crop_Employment_WorkerShare_Coefficients.pdf"),
    worker_figure, width = 7.6, height = 4.8
  )
  ggsave(
    file.path(paths$out_dir, "FullRevision_IND_Crop_Employment_WorkerShare_Coefficients.png"),
    worker_figure, width = 7.6, height = 4.8, dpi = 320
  )

  worker_models <- all_models[["delta_crop_worker_share"]]
  labor_term_labels <- c(
    delta_gdd_large = "Change in GDD (std.)",
    delta_kdd_large = "Change in KDD (std.)",
    delta_sm_season_large = "Change in soil moisture (std.)"
  )
  labor_model_cell <- function(model, term_name, standard_error = FALSE) {
    beta <- stats::coef(model)
    if (!term_name %in% names(beta) || !is.finite(beta[term_name])) return("")
    se <- sqrt(diag(stats::vcov(model)))[term_name]
    if (standard_error) return(paste0("(", fmt(se, 4), ")"))
    p_value <- 2 * stats::pnorm(-abs(beta[term_name] / se))
    paste0(fmt(beta[term_name], 4), stars(p_value))
  }
  for (cr in crops) {
    treatment_term <- paste0("delta_excess_", cr)
    displayed_terms <- c(treatment_term, weather)
    displayed_labels <- c(
      setNames(paste0("Change in ", crop_labels[[cr]], " excess salinity (dS/m)"), treatment_term),
      labor_term_labels
    )
    table_lines <- c(
      "\\begin{table}[!htbp]",
      "\\color{red}",
      "\\centering",
      paste0("\\caption{Crop-Specific Employment and Excess Salinity: ", crop_labels[[cr]], "}"),
      paste0("\\label{tab:full_revision_ind_crop_employment_workershare_", cr, "}"),
      "\\small",
      "\\setlength{\\tabcolsep}{6pt}",
      "\\renewcommand{\\arraystretch}{0.90}",
      "\\begin{tabular}{lrrr}",
      "\\toprule",
      "Variable & Spec. 1 & Spec. 2 & Spec. 3\\\\",
      "\\midrule"
    )
    for (term_name in displayed_terms) {
      table_lines <- c(
        table_lines,
        paste(c(
          latex_escape(unname(displayed_labels[term_name])),
          vapply(worker_models, labor_model_cell, character(1), term_name = term_name)
        ), collapse = " & "),
        "\\\\",
        paste(c(
          "",
          vapply(worker_models, labor_model_cell, character(1), term_name = term_name, standard_error = TRUE)
        ), collapse = " & "),
        "\\\\"
      )
    }
    table_lines <- c(
      table_lines,
      "\\midrule",
      "Crop-by-census fixed effects & Yes & Yes & Yes\\\\",
      "State-by-census fixed effects & Yes & Yes & Yes\\\\",
      "Initial-specialization-by-census fixed effects & No & No & Yes\\\\",
      paste(
        c("Observations", formatC(vapply(worker_models, stats::nobs, integer(1)), format = "d", big.mark = ",")),
        collapse = " & "
      ),
      "\\\\",
      paste(
        c("R2", vapply(worker_models, function(model) fmt(fixest::fitstat(model, "r2")$r2, 3), character(1))),
        collapse = " & "
      ),
      "\\\\",
      "\\bottomrule",
      "\\end{tabular}",
      "\\par\\addvspace{0.5ex}",
      "\\parbox{0.96\\linewidth}{\\footnotesize\\textit{Notes:} ",
      latex_escape(paste(
        "The dependent variable is the temporal change, in percentage points, in the indicated crop industry's share among workers.",
        "The five crop-specific salinity slopes are estimated jointly in each pooled model; this table displays the indicated crop slope and every estimated climate-control coefficient.",
        "L'unité d'observation est une zone géographique harmonisée, une culture et un intervalle intercensitaire (1991--2000 ou 2000--2010).",
        "All specifications include crop-by-census and state-by-census fixed effects.",
        "Preferred Spec. 2 adds changes in GDD, KDD and soil moisture; Spec. 3 also includes initial-specialization-quintile-by-census fixed effects.",
        "Conley spatial standard errors use a 200 km cutoff. Le secteur d'emploi est observé au moment de chaque recensement et les mêmes travailleurs ne sont pas suivis dans le temps."
      )),
      "}",
      "\\end{table}"
    )
    writeLines(
      table_lines,
      file.path(paths$out_dir, paste0("FullRevision_IND_Crop_Employment_WorkerShare_", cr, ".tex")),
      useBytes = TRUE
    )
  }
  invisible(list(data = d, models = all_models, coefficients = results))
}

estimate_rural_migration_income_heterogeneity <- function(d, weather) {
  income_map <- unique(d[
    is.finite(origin_mean_income) & origin_mean_income > 0,
    .(YEAR, orig, origin_mean_income, z_mean_salinity_orig)
  ])
  income_map[, income_quartile := paste0(
    "Q",
    pmin(4L, ceiling(4 * frank(origin_mean_income, ties.method = "average") / .N))
  ), by = YEAR]

  dq <- merge(
    d,
    income_map[, .(YEAR, orig, income_quartile)],
    by = c("YEAR", "orig"), all = FALSE, sort = FALSE
  )
  rhs <- c(
    "z_mean_salinity_orig",
    "z_mean_salinity_dest",
    weather
  )
  required <- c(
    rhs, "origin_population", "migrant_flow", "dyad",
    "state_orig", "state_dest", "YEAR", "orig", "dest"
  )
  dq <- complete_data(dq, required)
  dq <- dq[
    is.finite(origin_population) & origin_population > 0 &
      is.finite(migrant_flow) & migrant_flow >= 0
  ]

  model_store <- list()
  coefficient_rows <- list()
  support_rows <- list()
  # Sparse high-dimensional PPML subsamples can be numerically sensitive.
  # Check convergence diagnostics before interpreting quartile estimates.
  for (q in paste0("Q", seq_len(4L))) {
    d_q <- dq[income_quartile == q]
    if (nrow(d_q) == 0L) stop("Empty migration-income quartile: ", q)
    model <- fixest::fepois(
      as.formula(paste0(
        "migrant_flow ~ ", paste(rhs, collapse = " + "),
        " | dyad + state_orig^YEAR + state_dest^YEAR"
      )),
      data = d_q,
      offset = ~log(origin_population),
      cluster = ~orig + dest,
      notes = FALSE
    )
    model_store[[q]] <- model
    model_data <- d_q[fixest::obs(model)]
    beta <- stats::coef(model)
    standard_errors <- sqrt(diag(stats::vcov(model)))
    p_values <- 2 * stats::pnorm(-abs(beta / standard_errors))
    coefficient_rows[[q]] <- data.table(
      income_quartile = q,
      term = names(beta),
      estimate = unname(beta),
      standard_error = unname(standard_errors),
      p_value = unname(p_values)
    )
    origin_year_data <- unique(model_data[, .(
      YEAR, orig, z_mean_salinity_orig
    )])
    support_rows[[q]] <- data.table(
      income_quartile = q,
      observations = stats::nobs(model),
      observations_before_ppml_fe = nrow(d_q),
      observations_removed_by_ppml_fe = nrow(d_q) - stats::nobs(model),
      origins = uniqueN(model_data$orig),
      destinations = uniqueN(model_data$dest),
      dyads = uniqueN(model_data$dyad),
      origin_years = nrow(origin_year_data),
      mean_origin_salinity = mean(origin_year_data$z_mean_salinity_orig),
      squared_correlation = as.numeric(fixest::fitstat(model, "sq.cor")$sq.cor),
      pseudo_r2 = as.numeric(fixest::fitstat(model, "pr2")$pr2)
    )
  }

  coefficients <- rbindlist(coefficient_rows)
  support <- rbindlist(support_rows)
  fwrite(
    coefficients,
    file.path(
      paths$out_dir,
      "FullRevision_RuralMigration_IncomeQuartile_Coefficients.csv"
    )
  )
  fwrite(
    support,
    file.path(
      paths$out_dir,
      "FullRevision_RuralMigration_IncomeQuartile_Support.csv"
    )
  )

  term_labels <- c(
    z_mean_salinity_orig = "Origin mean salinity (std.)",
    z_mean_salinity_dest = "Destination mean salinity (std.)",
    z_gdd_large_orig = "Origin GDD (std.)",
    z_kdd_large_orig = "Origin KDD (std.)",
    z_sm_season_large_orig = "Origin soil moisture (std.)",
    z_gdd_large_dest = "Destination GDD (std.)",
    z_kdd_large_dest = "Destination KDD (std.)",
    z_sm_season_large_dest = "Destination soil moisture (std.)"
  )
  term_order <- intersect(names(term_labels), rhs)
  quartiles <- paste0("Q", seq_len(4L))
  estimate_cell <- function(q, term) {
    target_term <- term
    row <- coefficients[
      income_quartile == q & get("term") == target_term
    ]
    if (nrow(row) == 0L) return("")
    paste0(fmt(row$estimate, 4), stars(row$p_value))
  }
  se_cell <- function(q, term) {
    target_term <- term
    row <- coefficients[
      income_quartile == q & get("term") == target_term
    ]
    if (nrow(row) == 0L) return("")
    paste0("(", fmt(row$standard_error, 4), ")")
  }

  table_lines <- c(
    "\\begin{table}[!htbp]",
    "\\color{red}",
    "\\centering",
    "\\caption{Migration from Rural Origins: Separate Regressions by Baseline-Income Quartile}",
    "\\label{tab:full_revision_rural_migration_income_quartiles}",
    "\\footnotesize",
    "\\setlength{\\tabcolsep}{3pt}",
    "\\renewcommand{\\arraystretch}{0.92}",
    "\\begin{tabular}{lrrrr}",
    "\\toprule",
    "Variable & Q1 (poorest) & Q2 & Q3 & Q4 (richest)\\\\",
    "\\midrule"
  )
  for (term in term_order) {
    table_lines <- c(
      table_lines,
      paste(
        c(latex_escape(unname(term_labels[term])),
          vapply(quartiles, estimate_cell, character(1), term = term)),
        collapse = " & "
      ),
      "\\\\",
      paste(
        c("", vapply(quartiles, se_cell, character(1), term = term)),
        collapse = " & "
      ),
      "\\\\"
    )
  }
  table_lines <- c(
    table_lines,
    "\\midrule",
    "Dyad fixed effects & Yes & Yes & Yes & Yes\\\\",
    "Origin-state-by-window fixed effects & Yes & Yes & Yes & Yes\\\\",
    "Destination-state-by-window fixed effects & Yes & Yes & Yes & Yes\\\\",
    "\\midrule",
    paste(
      c(
        "Observations",
        vapply(quartiles, function(q) {
          formatC(support[income_quartile == q]$observations,
                  format = "d", big.mark = ",")
        }, character(1))
      ),
      collapse = " & "
    ),
    "\\\\",
    paste(
      c(
        "Origins",
        vapply(quartiles, function(q) {
          formatC(support[income_quartile == q]$origins,
                  format = "d", big.mark = ",")
        }, character(1))
      ),
      collapse = " & "
    ),
    "\\\\",
    paste(
      c(
        "Dyads",
        vapply(quartiles, function(q) {
          formatC(support[income_quartile == q]$dyads,
                  format = "d", big.mark = ",")
        }, character(1))
      ),
      collapse = " & "
    ),
    "\\\\",
    paste(
      c(
        "Origin-windows",
        vapply(quartiles, function(q) {
          formatC(support[income_quartile == q]$origin_years,
                  format = "d", big.mark = ",")
        }, character(1))
      ),
      collapse = " & "
    ),
    "\\\\",
    paste(
      c(
        "Mean origin salinity (std.)",
        vapply(quartiles, function(q) {
          fmt(support[income_quartile == q]$mean_origin_salinity, 3)
        }, character(1))
      ),
      collapse = " & "
    ),
    "\\\\",
    paste(
      c(
        "Squared correlation",
        vapply(quartiles, function(q) {
          fmt(support[income_quartile == q]$squared_correlation, 3)
        }, character(1))
      ),
      collapse = " & "
    ),
    "\\\\",
    paste(
      c(
        "Pseudo R2",
        vapply(quartiles, function(q) {
          fmt(support[income_quartile == q]$pseudo_r2, 3)
        }, character(1))
      ),
      collapse = " & "
    ),
    "\\\\",
    "\\bottomrule",
    "\\end{tabular}",
    "\\par\\addvspace{0.5ex}",
    "\\parbox{0.96\\linewidth}{\\footnotesize\\textit{Notes:} ",
    latex_escape(paste(
      "Each column is a separate PPML regression estimated only on origin--destination--window observations in the indicated baseline-income quartile.",
      "Q1 is the poorest quartile and Q4 the richest; quartiles are formed within each migration window from mean origin income measured in its baseline census, avoiding comparisons of nominal income levels across currencies.",
      "The outcome, offset and specification are those of the preferred migration model.",
      "Every regression includes destination salinity, GDD, KDD and soil moisture at origin and destination, dyad fixed effects, origin-state-by-window fixed effects and destination-state-by-window fixed effects.",
      "Precipitation and income are excluded.",
      "Standard errors in parentheses are two-way clustered by origin and destination.",
      "PPML may remove fixed-effect groups with only zero flows; the observation counts therefore refer to the final estimation sample of each separate regression.",
      "Because the four models are estimated separately, differences between columns are descriptive and are not formal tests of coefficient equality."
    )),
    "}",
    "\\end{table}"
  )
  table_file <- file.path(
    paths$out_dir,
    "FullRevision_RuralMigration_IncomeQuartile_Heterogeneity.tex"
  )
  writeLines(table_lines, table_file, useBytes = TRUE)

  invisible(list(
    models = model_store,
    coefficients = coefficients,
    support = support,
    data = dq
  ))
}

estimate_destination_agricultural_migration_excess_rate_sensitivity <- function(
  panel,
  weather
) {
  write_status(
    paste(
      "Estimating explicit-rate and alternative-salinity",
      "sensitivities for destination-agriculture migration."
    )
  )
  outcome <- "current_ag_migrant_flow"
  salinity_measures <- list(
    "Mean salinity" = c(
      origin = "z_mean_salinity_orig",
      destination = "z_mean_salinity_dest"
    ),
    "Crop-mix-weighted excess salinity" = c(
      origin = "z_excess_above_fao_large_orig",
      destination = "z_excess_above_fao_large_dest"
    )
  )
  salinity_variables <- unique(unlist(salinity_measures, use.names = FALSE))
  required <- c(
    outcome, "migrant_flow", "origin_population", "dyad", "orig", "dest",
    "YEAR", "state_orig", "state_dest", salinity_variables, weather
  )
  missing <- setdiff(c(required, "origin_mean_income"), names(panel))
  if (length(missing) > 0L) {
    stop(
      "Missing excess-rate migration-sensitivity variables: ",
      paste(missing, collapse = ", ")
    )
  }

  d_all <- complete_data(panel, required)
  d_all <- d_all[
    is.finite(get(outcome)) & get(outcome) >= 0 &
      is.finite(migrant_flow) & migrant_flow >= 0 &
      is.finite(origin_population) & origin_population > 0
  ]

  income_check <- panel[
    is.finite(origin_mean_income) & origin_mean_income > 0,
    .(income_values = uniqueN(origin_mean_income)),
    by = .(YEAR, orig)
  ]
  if (any(income_check$income_values != 1L)) {
    stop("Baseline origin income is not unique within origin-year cells.")
  }
  income_map <- unique(panel[
    is.finite(origin_mean_income) & origin_mean_income > 0,
    .(YEAR, orig, origin_mean_income)
  ])
  income_map[, income_quartile := paste0(
    "Q",
    pmin(4L, ceiling(4 * frank(origin_mean_income, ties.method = "average") / .N))
  ), by = YEAR]
  d_quartiles <- merge(
    d_all,
    income_map[, .(YEAR, orig, income_quartile)],
    by = c("YEAR", "orig"), all = FALSE, sort = FALSE
  )

  groups <- c("All", paste0("Q", seq_len(4L)))
  coefficient_rows <- list()
  support_rows <- list()
  model_store <- list()

  for (measure_name in names(salinity_measures)) {
    sal_o <- unname(salinity_measures[[measure_name]]["origin"])
    sal_d <- unname(salinity_measures[[measure_name]]["destination"])
    rhs <- c(sal_o, sal_d, weather)
    formula_flow <- as.formula(paste0(
      outcome, " ~ ", paste(rhs, collapse = " + "),
      " | dyad + state_orig^YEAR + state_dest^YEAR"
    ))
    formula_rate <- as.formula(paste0(
      "selected_outmigration_rate_pct ~ ", paste(rhs, collapse = " + "),
      " | dyad + state_orig^YEAR + state_dest^YEAR"
    ))

    for (group_name in groups) {
      d_group <- if (group_name == "All") {
        copy(d_all)
      } else {
        copy(d_quartiles[income_quartile == group_name])
      }
      if (nrow(d_group) == 0L) {
        stop("Empty salinity-rate migration group: ", group_name)
      }

      ppml_preliminary <- fixest::fepois(
        formula_flow,
        data = d_group,
        offset = ~log(origin_population),
        cluster = ~orig + dest,
        notes = FALSE
      )
      d_final <- d_group[fixest::obs(ppml_preliminary)]
      ppml <- fixest::fepois(
        formula_flow,
        data = d_final,
        offset = ~log(origin_population),
        cluster = ~orig + dest,
        notes = FALSE
      )
      d_final[, selected_outmigration_rate_pct :=
        100 * get(outcome) / origin_population]
      rate_ols <- fixest::feols(
        formula_rate,
        data = d_final,
        cluster = ~orig + dest,
        notes = FALSE
      )
      model_key <- paste(measure_name, group_name, sep = "__")
      model_store[[model_key]] <- list(ppml = ppml, rate_ols = rate_ols)

      for (estimator_name in c(
        "PPML with population offset",
        "OLS explicit rate"
      )) {
        model <- if (estimator_name == "PPML with population offset") {
          ppml
        } else {
          rate_ols
        }
        beta <- stats::coef(model)
        standard_errors <- sqrt(diag(stats::vcov(model)))
        p_values <- 2 * stats::pnorm(-abs(beta / standard_errors))
        row_key <- paste(measure_name, group_name, estimator_name)
        coefficient_rows[[row_key]] <- data.table(
          salinity_measure = measure_name,
          sample_group = group_name,
          estimator = estimator_name,
          term = names(beta),
          estimate = unname(beta),
          standard_error = unname(standard_errors),
          p_value = unname(p_values),
          observations = stats::nobs(model)
        )
      }

      origin_year_data <- unique(d_final[, .(
        YEAR, orig, origin_treatment = get(sal_o)
      )])
      support_rows[[model_key]] <- data.table(
        salinity_measure = measure_name,
        sample_group = group_name,
        observations_before_ppml_fe = nrow(d_group),
        observations_final = nrow(d_final),
        observations_removed_by_ppml_fe = nrow(d_group) - nrow(d_final),
        zero_selected_flow_rows = sum(d_final[[outcome]] == 0),
        positive_selected_flow_rows = sum(d_final[[outcome]] > 0),
        origins = uniqueN(d_final$orig),
        destinations = uniqueN(d_final$dest),
        dyads = uniqueN(d_final$dyad),
        mean_selected_outmigration_rate_pct = mean(
          100 * d_final[[outcome]] / d_final$origin_population
        ),
        mean_origin_treatment_std = mean(origin_year_data$origin_treatment)
      )
    }
  }

  coefficients <- rbindlist(coefficient_rows)
  support <- rbindlist(support_rows)
  fwrite(
    coefficients,
    file.path(
      paths$out_dir,
      "FullRevision_RuralMigration_DestinationAgriculture_SalinityRateSensitivity_Coefficients.csv"
    )
  )
  fwrite(
    support,
    file.path(
      paths$out_dir,
      "FullRevision_RuralMigration_DestinationAgriculture_SalinityRateSensitivity_Support.csv"
    )
  )

  invisible(list(
    models = model_store,
    coefficients = coefficients,
    support = support
  ))
}

estimate_destination_agricultural_migration_income_heterogeneity <- function(panel, weather) {
  write_status(
    "Estimating destination-agriculture migration heterogeneity by baseline-income quartile."
  )
  outcome <- "current_ag_migrant_flow"
  required_panel <- c(
    outcome, "migrant_flow", "origin_population", "origin_mean_income",
    "dyad", "orig", "dest", "YEAR", "state_orig", "state_dest",
    "z_mean_salinity_orig", "z_mean_salinity_dest", weather
  )
  missing <- setdiff(required_panel, names(panel))
  if (length(missing) > 0L) {
    stop(
      "Missing destination-agriculture income-heterogeneity variables: ",
      paste(missing, collapse = ", ")
    )
  }

  income_check <- panel[
    is.finite(origin_mean_income) & origin_mean_income > 0,
    .(income_values = uniqueN(origin_mean_income)),
    by = .(YEAR, orig)
  ]
  if (any(income_check$income_values != 1L)) {
    stop("Baseline origin income is not unique within origin-year cells.")
  }
  income_map <- unique(panel[
    is.finite(origin_mean_income) & origin_mean_income > 0,
    .(YEAR, orig, origin_mean_income)
  ])
  income_map[, income_quartile := paste0(
    "Q",
    pmin(4L, ceiling(4 * frank(origin_mean_income, ties.method = "average") / .N))
  ), by = YEAR]

  d <- merge(
    panel,
    income_map[, .(YEAR, orig, income_quartile)],
    by = c("YEAR", "orig"), all = FALSE, sort = FALSE
  )
  rhs <- c("z_mean_salinity_orig", "z_mean_salinity_dest", weather)
  required <- c(
    rhs, outcome, "migrant_flow", "origin_population", "dyad",
    "state_orig", "state_dest", "YEAR", "orig", "dest"
  )
  d <- complete_data(d, required)
  d <- d[
    is.finite(get(outcome)) & get(outcome) >= 0 &
      is.finite(migrant_flow) & migrant_flow >= 0 &
      is.finite(origin_population) & origin_population > 0
  ]

  quartiles <- paste0("Q", seq_len(4L))
  model_store <- list()
  coefficient_rows <- list()
  support_rows <- list()
  for (q in quartiles) {
    d_q <- d[income_quartile == q]
    if (nrow(d_q) == 0L) {
      stop("Empty destination-agriculture migration-income quartile: ", q)
    }
    model <- fixest::fepois(
      as.formula(paste0(
        outcome, " ~ ", paste(rhs, collapse = " + "),
        " | dyad + state_orig^YEAR + state_dest^YEAR"
      )),
      data = d_q,
      offset = ~log(origin_population),
      cluster = ~orig + dest,
      notes = FALSE
    )
    model_store[[q]] <- model
    model_data <- d_q[fixest::obs(model)]
    beta <- stats::coef(model)
    standard_errors <- sqrt(diag(stats::vcov(model)))
    p_values <- 2 * stats::pnorm(-abs(beta / standard_errors))
    coefficient_rows[[q]] <- data.table(
      income_quartile = q,
      term = names(beta),
      estimate = unname(beta),
      standard_error = unname(standard_errors),
      p_value = unname(p_values)
    )
    origin_year_data <- unique(model_data[, .(
      YEAR, orig, z_mean_salinity_orig
    )])
    support_rows[[q]] <- data.table(
      income_quartile = q,
      observations = stats::nobs(model),
      observations_before_ppml_fe = nrow(d_q),
      observations_removed_by_ppml_fe = nrow(d_q) - stats::nobs(model),
      zero_flow_rows = sum(model_data[[outcome]] == 0),
      positive_flow_rows = sum(model_data[[outcome]] > 0),
      origins = uniqueN(model_data$orig),
      destinations = uniqueN(model_data$dest),
      dyads = uniqueN(model_data$dyad),
      origin_years = nrow(origin_year_data),
      weighted_agricultural_migrants = sum(model_data[[outcome]], na.rm = TRUE),
      weighted_all_migrants = sum(model_data$migrant_flow, na.rm = TRUE),
      agricultural_share_of_weighted_migrants = 100 *
        sum(model_data[[outcome]], na.rm = TRUE) /
        sum(model_data$migrant_flow, na.rm = TRUE),
      mean_origin_salinity = mean(origin_year_data$z_mean_salinity_orig),
      squared_correlation = as.numeric(fixest::fitstat(model, "sq.cor")$sq.cor),
      pseudo_r2 = as.numeric(fixest::fitstat(model, "pr2")$pr2)
    )
  }

  coefficients <- rbindlist(coefficient_rows)
  support <- rbindlist(support_rows)
  fwrite(
    coefficients,
    file.path(
      paths$out_dir,
      "FullRevision_RuralMigration_DestinationAgriculture_IncomeQuartile_Coefficients.csv"
    )
  )
  fwrite(
    support,
    file.path(
      paths$out_dir,
      "FullRevision_RuralMigration_DestinationAgriculture_IncomeQuartile_Support.csv"
    )
  )

  term_labels <- c(
    z_mean_salinity_orig = "Origin mean salinity (std.)",
    z_mean_salinity_dest = "Destination mean salinity (std.)",
    z_gdd_large_orig = "Origin GDD (std.)",
    z_kdd_large_orig = "Origin KDD (std.)",
    z_sm_season_large_orig = "Origin soil moisture (std.)",
    z_gdd_large_dest = "Destination GDD (std.)",
    z_kdd_large_dest = "Destination KDD (std.)",
    z_sm_season_large_dest = "Destination soil moisture (std.)"
  )
  term_order <- intersect(names(term_labels), rhs)
  estimate_cell <- function(q, term) {
    target_term <- term
    row <- coefficients[
      income_quartile == q & get("term") == target_term
    ]
    if (nrow(row) == 0L) return("")
    paste0(fmt(row$estimate, 4), stars(row$p_value))
  }
  se_cell <- function(q, term) {
    target_term <- term
    row <- coefficients[
      income_quartile == q & get("term") == target_term
    ]
    if (nrow(row) == 0L) return("")
    paste0("(", fmt(row$standard_error, 4), ")")
  }
  support_cell <- function(q, variable, digits = 0L) {
    value <- support[income_quartile == q][[variable]]
    if (digits == 0L) {
      formatC(as.integer(value), format = "d", big.mark = ",")
    } else {
      fmt(value, digits)
    }
  }

  table_lines <- c(
    "\\begin{table}[!htbp]",
    "\\color{red}",
    "\\centering",
    "\\caption{Agricultural Employment at Destination: Separate Regressions by Baseline-Income Quartile}",
    "\\label{tab:full_revision_rural_migration_destination_agriculture_income_quartiles}",
    "\\small",
    "\\setlength{\\tabcolsep}{5pt}",
    "\\renewcommand{\\arraystretch}{0.90}",
    "\\resizebox{\\linewidth}{!}{%",
    "\\begin{tabular}{lrrrr}",
    "\\toprule",
    "Variable & Q1 (poorest) & Q2 & Q3 & Q4 (richest)\\\\",
    "\\midrule"
  )
  for (term in term_order) {
    table_lines <- c(
      table_lines,
      paste(
        c(
          latex_escape(unname(term_labels[term])),
          vapply(quartiles, estimate_cell, character(1), term = term)
        ),
        collapse = " & "
      ),
      "\\\\",
      paste(
        c("", vapply(quartiles, se_cell, character(1), term = term)),
        collapse = " & "
      ),
      "\\\\"
    )
  }
  table_lines <- c(
    table_lines,
    "\\midrule",
    "Dyad fixed effects & Yes & Yes & Yes & Yes\\\\",
    "Origin-state-by-window fixed effects & Yes & Yes & Yes & Yes\\\\",
    "Destination-state-by-window fixed effects & Yes & Yes & Yes & Yes\\\\",
    "\\midrule",
    paste(
      c(
        "Observations",
        vapply(quartiles, support_cell, character(1), variable = "observations")
      ),
      collapse = " & "
    ),
    "\\\\",
    paste(
      c(
        "Positive-flow observations",
        vapply(quartiles, support_cell, character(1), variable = "positive_flow_rows")
      ),
      collapse = " & "
    ),
    "\\\\",
    paste(
      c(
        "Origins",
        vapply(quartiles, support_cell, character(1), variable = "origins")
      ),
      collapse = " & "
    ),
    "\\\\",
    paste(
      c(
        "Dyads",
        vapply(quartiles, support_cell, character(1), variable = "dyads")
      ),
      collapse = " & "
    ),
    "\\\\",
    paste(
      c(
        "Mean origin salinity (std.)",
        vapply(
          quartiles, support_cell, character(1),
          variable = "mean_origin_salinity", digits = 3L
        )
      ),
      collapse = " & "
    ),
    "\\\\",
    paste(
      c(
        "Squared correlation",
        vapply(
          quartiles, support_cell, character(1),
          variable = "squared_correlation", digits = 3L
        )
      ),
      collapse = " & "
    ),
    "\\\\",
    paste(
      c(
        "Pseudo R2",
        vapply(
          quartiles, support_cell, character(1),
          variable = "pseudo_r2", digits = 3L
        )
      ),
      collapse = " & "
    ),
    "\\\\",
    "\\bottomrule",
    "\\end{tabular}",
    "}",
    "\\par\\addvspace{0.5ex}",
    "\\parbox{0.96\\linewidth}{\\footnotesize\\textit{Notes:} ",
    latex_escape(paste(
      "Chaque colonne présente une régression PPML séparée pour les migrants âgés d'au moins 15 ans, originaires d'une zone rurale et travaillant dans l'agriculture, la pêche ou la sylviculture au recensement de destination.",
      "Q1 is the poorest quartile and Q4 the richest; quartiles are formed within each migration window from mean origin income measured in its baseline census.",
      "The dependent variable is the selected weighted flow. The offset is the weighted population aged 15 or older residing in the origin five years before the census, including non-movers.",
      "All columns implement the preferred migration specification, including origin and destination salinity, GDD, KDD and soil moisture, dyad fixed effects, origin-state-by-window fixed effects and destination-state-by-window fixed effects.",
      "Precipitation and income are excluded, and standard errors are two-way clustered by origin and destination.",
      "Le secteur d'emploi est observé à destination après le déplacement; l'échantillon combine donc migration et sélection du secteur après la migration.",
      "PPML may remove fixed-effect groups containing only zero selected flows; observation counts refer to each final estimation sample.",
      "Differences between columns are descriptive and are not formal tests of coefficient equality."
    )),
    "}",
    "\\end{table}"
  )
  writeLines(
    table_lines,
    file.path(
      paths$out_dir,
      "FullRevision_RuralMigration_DestinationAgriculture_IncomeQuartile_Heterogeneity.tex"
    ),
    useBytes = TRUE
  )

  invisible(list(
    models = model_store,
    coefficients = coefficients,
    support = support,
    data = d
  ))
}

estimate_destination_agricultural_migration <- function(panel) {
  write_status("Estimating destination-census agricultural-employment migration robustness.")
  outcome <- "current_ag_migrant_flow"
  if (!outcome %in% names(panel)) {
    stop("The rural migration panel lacks current_ag_migrant_flow.")
  }
  sal_o <- "z_mean_salinity_orig"
  sal_d <- "z_mean_salinity_dest"
  weather <- c(
    "z_gdd_large_orig", "z_kdd_large_orig", "z_sm_season_large_orig",
    "z_gdd_large_dest", "z_kdd_large_dest", "z_sm_season_large_dest"
  )
  rhs1 <- c(sal_o, sal_d)
  rhs2 <- c(rhs1, weather)
  required <- unique(c(
    outcome, "migrant_flow", "origin_population", "dyad", "orig", "dest",
    "YEAR", "state_orig", "state_dest", rhs2
  ))
  missing <- setdiff(required, names(panel))
  if (length(missing) > 0L) {
    stop("Missing destination-agriculture migration variables: ", paste(missing, collapse = ", "))
  }

  d <- complete_data(panel, required)
  d <- d[
    is.finite(get(outcome)) & get(outcome) >= 0 &
      is.finite(origin_population) & origin_population > 0
  ]
  rows_before_positive_dyad <- nrow(d)
  positive_dyads <- d[
    , .(any_positive = any(get(outcome) > 0)), by = dyad
  ][any_positive == TRUE, dyad]
  d <- d[dyad %in% positive_dyads]
  rows_after_positive_dyad <- nrow(d)

  f1 <- as.formula(paste0(
    outcome, " ~ ", paste(rhs1, collapse = " + "),
    " | dyad + state_orig^YEAR + state_dest^YEAR"
  ))
  f2 <- as.formula(paste0(
    outcome, " ~ ", paste(rhs2, collapse = " + "),
    " | dyad + state_orig^YEAR + state_dest^YEAR"
  ))
  preferred_preliminary <- fixest::fepois(
    f2, data = d, offset = ~log(origin_population),
    cluster = ~orig + dest, notes = FALSE
  )
  common_observations <- fixest::obs(preferred_preliminary)
  d <- d[common_observations]
  m1 <- fixest::fepois(
    f1, data = d, offset = ~log(origin_population),
    cluster = ~orig + dest, notes = FALSE
  )
  m2 <- fixest::fepois(
    f2, data = d, offset = ~log(origin_population),
    cluster = ~orig + dest, notes = FALSE
  )
  model_n <- vapply(list(m1, m2), stats::nobs, integer(1))
  if (length(unique(model_n)) != 1L) {
    stop(
      "Destination-agriculture migration models use different samples: ",
      paste(model_n, collapse = ", ")
    )
  }

  support_by_year <- d[, .(
    observations = .N,
    positive_agricultural_flow_rows = sum(get(outcome) > 0),
    weighted_agricultural_migrants = sum(get(outcome), na.rm = TRUE),
    weighted_all_migrants = sum(migrant_flow, na.rm = TRUE),
    agricultural_share_of_weighted_migrants = 100 *
      sum(get(outcome), na.rm = TRUE) / sum(migrant_flow, na.rm = TRUE),
    origins = uniqueN(orig),
    destinations = uniqueN(dest),
    dyads = uniqueN(dyad)
  ), by = YEAR]
  support_overall <- d[, .(
    YEAR = "All years",
    observations = .N,
    positive_agricultural_flow_rows = sum(get(outcome) > 0),
    weighted_agricultural_migrants = sum(get(outcome), na.rm = TRUE),
    weighted_all_migrants = sum(migrant_flow, na.rm = TRUE),
    agricultural_share_of_weighted_migrants = 100 *
      sum(get(outcome), na.rm = TRUE) / sum(migrant_flow, na.rm = TRUE),
    origins = uniqueN(orig),
    destinations = uniqueN(dest),
    dyads = uniqueN(dyad)
  )]
  support <- rbind(support_by_year, support_overall, fill = TRUE)
  support[, `:=`(
    rows_before_positive_dyad = rows_before_positive_dyad,
    rows_after_positive_dyad = rows_after_positive_dyad,
    rows_final_ppml_sample = stats::nobs(m2)
  )]
  fwrite(
    support,
    file.path(
      paths$out_dir,
      "FullRevision_RuralMigration_DestinationAgriculture_Support.csv"
    )
  )

  coefficient_rows <- rbindlist(lapply(seq_along(list(m1, m2)), function(i) {
    model <- list(m1, m2)[[i]]
    beta <- stats::coef(model)
    standard_errors <- sqrt(diag(stats::vcov(model)))
    data.table(
      specification = paste0("Spec. ", i),
      term = names(beta),
      estimate = unname(beta),
      standard_error = unname(standard_errors),
      p_value = 2 * stats::pnorm(-abs(beta / standard_errors)),
      observations = stats::nobs(model)
    )
  }))
  fwrite(
    coefficient_rows,
    file.path(
      paths$out_dir,
      "FullRevision_RuralMigration_DestinationAgriculture_Coefficients.csv"
    )
  )

  fixest::setFixest_dict(c(
    current_ag_migrant_flow = "Agricultural-sector outmigration rate",
    z_mean_salinity_orig = "Origin mean salinity (std.)",
    z_mean_salinity_dest = "Destination mean salinity (std.)",
    z_gdd_large_orig = "Origin GDD (std.)",
    z_kdd_large_orig = "Origin KDD (std.)",
    z_sm_season_large_orig = "Origin soil moisture (std.)",
    z_gdd_large_dest = "Destination GDD (std.)",
    z_kdd_large_dest = "Destination KDD (std.)",
    z_sm_season_large_dest = "Destination soil moisture (std.)"
  ), reset = TRUE)
  tex_file <- file.path(
    paths$out_dir,
    "FullRevision_RuralMigration_DestinationAgriculture.tex"
  )
  fixest::etable(
    m1, m2,
    tex = TRUE,
    file = tex_file,
    replace = TRUE,
    title = "Migration from Rural Origins: Agricultural Employment at Destination",
    label = "tab:full_revision_rural_migration_destination_agriculture",
    fitstat = ~ n + sq.cor + pr2,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    notes = paste(
      "Notes: la régression PPML utilise le flux pondéré de migrants âgés d'au moins 15 ans, originaires d'une zone rurale et travaillant dans l'agriculture, la pêche ou la sylviculture au recensement de destination.",
      "Le secteur d'emploi est observé au recensement de destination, et non cinq ans plus tôt dans la zone d'origine ni à la date exacte du déplacement; la mesure combine donc migration et sélection du secteur après la migration.",
      "The offset is the weighted population aged 15 or older residing in the origin five years before the census, including non-movers.",
      "Both columns include dyad, origin-state-by-window and destination-state-by-window fixed effects.",
      "Column 2 is preferred and adds GDD, KDD and soil moisture at origin and destination; precipitation and income are excluded.",
      "All columns use the same final PPML sample and standard errors are two-way clustered by origin and destination."
    )
  )
  lines <- readLines(tex_file, warn = FALSE, encoding = "UTF-8")
  lines <- gsub("state\\_orig-YEAR", "Origin-state-by-window fixed effects", lines, fixed = TRUE)
  lines <- gsub("state\\_dest-YEAR", "Destination-state-by-window fixed effects", lines, fixed = TRUE)
  dyad_rows <- grepl("^[[:space:]]*dyad[[:space:]]*&", lines)
  lines[dyad_rows] <- sub("dyad", "Dyad fixed effects", lines[dyad_rows], fixed = TRUE)
  table_line <- which(grepl("^\\s*\\\\begin\\{table\\}", lines))[1]
  if (!is.na(table_line) && !any(grepl("\\\\color\\{red\\}", lines))) {
    lines <- append(lines, "\\color{red}", after = table_line)
  }
  centering_line <- which(grepl("^\\s*\\\\centering", lines))[1]
  if (!is.na(centering_line)) {
    lines <- append(
      lines,
      c(
        "\\small",
        "\\setlength{\\tabcolsep}{5pt}",
        "\\renewcommand{\\arraystretch}{0.90}"
      ),
      after = centering_line
    )
  }
  writeLines(lines, tex_file, useBytes = TRUE)

  income_heterogeneity <-
    estimate_destination_agricultural_migration_income_heterogeneity(panel, weather)
  excess_rate_sensitivity <-
    estimate_destination_agricultural_migration_excess_rate_sensitivity(panel, weather)

  invisible(list(
    models = list(m1, m2),
    coefficients = coefficient_rows,
    support = support,
    data = d,
    income_heterogeneity = income_heterogeneity,
    excess_rate_sensitivity = excess_rate_sensitivity
  ))
}

estimate_rural_migration <- function(
  panel,
  out_stub = "FullRevision_RuralMigration_PPML_PreLaggedMeanSalinity",
  desc_stub = "FullRevision_RuralMigration_Descriptive_Stats",
  title = "Lagged Mean Salinity and Migration from Previous-Census Rural Origins",
  label = "tab:full_revision_rural_migration_ppml",
  exposure_note = "Exposure is standardized mean salinity before the observed migration interval: 1990--1994 for the 2000 census and 2000--2004 for the 2010 census."
) {
  write_status("Estimating rural-origin PPML migration model.")
  desc_label <- if (grepl("During", desc_stub, fixed = TRUE)) {
    "tab:full_revision_rural_migration_during_desc"
  } else if (grepl("PreWindow", desc_stub, fixed = TRUE)) {
    "tab:full_revision_rural_migration_prewindow_desc"
  } else {
    "tab:full_revision_rural_migration_desc"
  }
  sal_o <- "z_mean_salinity_orig"
  sal_d <- "z_mean_salinity_dest"
  weather <- c(
    "z_gdd_large_orig", "z_kdd_large_orig", "z_sm_season_large_orig",
    "z_gdd_large_dest", "z_kdd_large_dest", "z_sm_season_large_dest"
  )
  common_terms <- c(sal_o, sal_d, weather)
  common_terms <- intersect(common_terms, names(panel))
  potential_rows <- attr(panel, "potential_rows", exact = TRUE)
  if (is.null(potential_rows) || !is.finite(potential_rows)) potential_rows <- nrow(panel)
  informative_dyads_pre_filter <- attr(panel, "informative_dyads_pre_filter", exact = TRUE)
  if (is.null(informative_dyads_pre_filter) || !is.finite(informative_dyads_pre_filter)) {
    informative_dyads_pre_filter <- uniqueN(panel$dyad)
  }
  d <- complete_data(panel, common_terms)
  before <- nrow(d)
  positive_dyads <- d[, .(any_positive = any(migrant_flow > 0)), by = dyad][any_positive == TRUE, dyad]
  d <- d[dyad %in% positive_dyads]
  sample_diag <- data.table(
    Metric = c(
      "Potential rows in full rural-origin panel",
      "Rows in informative dyad-year panel",
      "Rows after common complete-data filter",
      "Rows after positive-dyad filter",
      "Zero-flow rows after filters",
      "Positive-flow rows after filters",
      "Current-agriculture positive-flow rows after filters",
      "Informative dyads before complete-data filter",
      "Informative dyads",
      "Zones harmonisées d'origine rurale",
      "Zones harmonisées de destination",
      "Census years in estimation sample",
      "Baseline years used to classify origins"
    ),
    Value = c(
      formatC(potential_rows, format = "d", big.mark = ","),
      formatC(nrow(panel), format = "d", big.mark = ","),
      formatC(before, format = "d", big.mark = ","),
      formatC(nrow(d), format = "d", big.mark = ","),
      formatC(sum(d$migrant_flow == 0), format = "d", big.mark = ","),
      formatC(sum(d$migrant_flow > 0), format = "d", big.mark = ","),
      formatC(if ("current_ag_migrant_flow" %in% names(d)) sum(d$current_ag_migrant_flow > 0) else NA_integer_, format = "d", big.mark = ","),
      formatC(informative_dyads_pre_filter, format = "d", big.mark = ","),
      formatC(length(positive_dyads), format = "d", big.mark = ","),
      formatC(uniqueN(d$orig), format = "d", big.mark = ","),
      formatC(uniqueN(d$dest), format = "d", big.mark = ","),
      paste(sort(unique(d$YEAR)), collapse = ", "),
      paste(sort(unique(d$baseline_year)), collapse = ", ")
    )
  )
  write_latex_df(
    sample_diag,
    file.path(paths$out_dir, paste0(desc_stub, ".tex")),
    "Migration Sample Diagnostics for Previous-Census Rural Origins",
    desc_label,
    note = paste(
      "Une observation de régression est une paire origine--destination de zones géographiques harmonisées pour une année de recensement.",
      "An origin is classified as rural when the weighted rural share in the previous census is at least 50 percent.",
      if (any(d$YEAR == "1991")) {
        "Le recensement de 1980 sert à classer les origines des flux de 1991, mais il ne renseigne pas le lieu de résidence cinq ans auparavant et ne permet donc pas de construire un flux quinquennal comparable pour 1980."
      } else {
        "This pre-exposure robustness starts in 2000 because the salinity maps do not provide a complete pre-1986 window for 1991 flows."
      },
      "The common complete-data filter requires salinity, GDD, KDD, and soil moisture at both the origin and destination so that the two specifications use the same observations.",
      "Zero flows are kept when the panel is constructed; dyads that are always zero are dropped because dyad fixed effects perfectly predict them in PPML."
    ),
    size = "\\small"
  )
  fwrite(sample_diag, file.path(paths$out_dir, paste0(desc_stub, ".csv")))

  rhs1 <- paste(sal_o, sal_d, sep = " + ")
  rhs2 <- paste(c(sal_o, sal_d, weather), collapse = " + ")
  f3 <- as.formula(paste0("migrant_flow ~ ", rhs1, " | dyad + state_orig^YEAR + state_dest^YEAR"))
  f4 <- as.formula(paste0("migrant_flow ~ ", rhs2, " | dyad + state_orig^YEAR + state_dest^YEAR"))

  rows_before_fe <- nrow(d)
  m4_preliminary <- fixest::fepois(
    f4, data = d, offset = ~log(origin_population),
    cluster = ~orig + dest, notes = FALSE
  )
  common_observations <- fixest::obs(m4_preliminary)
  d <- d[common_observations]
  m3 <- fixest::fepois(f3, data = d, offset = ~log(origin_population), cluster = ~orig + dest, notes = FALSE)
  m4 <- fixest::fepois(f4, data = d, offset = ~log(origin_population), cluster = ~orig + dest, notes = FALSE)
  model_n <- vapply(list(m3, m4), stats::nobs, integer(1))
  if (length(unique(model_n)) != 1L) {
    stop("Migration specifications do not use the same estimation sample: ", paste(model_n, collapse = ", "))
  }

  sample_diag <- rbind(
    sample_diag,
    data.table(
      Metric = c(
        "Rows used by PPML after fixed-effect checks",
        "Rows additionally removed by PPML",
        "Zero-flow rows in final estimation sample",
        "Positive-flow rows in final estimation sample",
        "Dyads in final estimation sample"
      ),
      Value = c(
        formatC(stats::nobs(m4), format = "d", big.mark = ","),
        formatC(rows_before_fe - stats::nobs(m4), format = "d", big.mark = ","),
        formatC(sum(d$migrant_flow == 0), format = "d", big.mark = ","),
        formatC(sum(d$migrant_flow > 0), format = "d", big.mark = ","),
        formatC(uniqueN(d$dyad), format = "d", big.mark = ",")
      )
    )
  )
  write_latex_df(
    sample_diag,
    file.path(paths$out_dir, paste0(desc_stub, ".tex")),
    "Migration Sample Diagnostics for Previous-Census Rural Origins",
    desc_label,
    note = paste(
      "Une observation de régression est une paire origine--destination de zones géographiques harmonisées pour une année de recensement.",
      "An origin is classified as rural when the weighted rural share in the previous census is at least 50 percent.",
      if (any(d$YEAR == "1991")) {
        "Le recensement de 1980 sert à classer les origines des flux de 1991, mais il ne renseigne pas le lieu de résidence cinq ans auparavant et ne permet donc pas de construire un flux quinquennal comparable pour 1980."
      } else {
        "This pre-exposure robustness starts in 2000 because the salinity maps do not provide a complete pre-1986 window for 1991 flows."
      },
      "The common complete-data filter requires salinity, GDD, KDD, and soil moisture at both the origin and destination so that the two specifications use the same observations.",
      "Zero flows are kept when the panel is constructed; dyads that are always zero and any remaining observations perfectly predicted by the fixed effects are dropped in PPML."
    ),
    size = "\\small"
  )
  fwrite(sample_diag, file.path(paths$out_dir, paste0(desc_stub, ".csv")))

  fixest::setFixest_dict(c(
    migrant_flow = "Migrant flow (origin-population offset)",
    z_mean_salinity_orig = "Origin mean salinity (std.)",
    z_mean_salinity_dest = "Destination mean salinity (std.)",
    z_gdd_large_orig = "Origin GDD (std.)",
    z_kdd_large_orig = "Origin KDD (std.)",
    z_sm_season_large_orig = "Origin soil moisture (std.)",
    z_gdd_large_dest = "Destination GDD (std.)",
    z_kdd_large_dest = "Destination KDD (std.)",
    z_sm_season_large_dest = "Destination soil moisture (std.)"
  ), reset = TRUE)

  tex_file <- file.path(paths$out_dir, paste0(out_stub, ".tex"))
  fixest::etable(
    m3, m4,
    tex = TRUE,
    file = tex_file,
    replace = TRUE,
    title = title,
    label = label,
    fitstat = ~ n + sq.cor + pr2,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    notes = paste(
      "Notes: PPML; la variable dépendante est le flux pondéré des personnes âgées d'au moins 15 ans entre deux zones géographiques harmonisées.",
      "Origins are classified as rural when at least 50 percent of their weighted population lived in rural areas in the previous census.",
      "Destinations are unrestricted so that rural-to-urban migration remains in the outcome.",
      exposure_note,
      "All columns use the same complete-data sample.",
      "Both columns include dyad, origin-state-by-census and destination-state-by-census fixed effects.",
      "Column 2 is preferred and adds GDD, KDD and soil moisture at origin and destination; precipitation and income are excluded.",
      "The offset is the weighted population aged 15 or older residing in the origin five years before the census, including non-movers; standard errors are two-way clustered by origin and destination."
    )
  )
  lines <- readLines(tex_file, warn = FALSE, encoding = "UTF-8")
  lines <- gsub("state\\_orig-YEAR", "Origin-state-by-census fixed effects", lines, fixed = TRUE)
  lines <- gsub("state\\_dest-YEAR", "Destination-state-by-census fixed effects", lines, fixed = TRUE)
  dyad_rows <- grepl("^[[:space:]]*dyad[[:space:]]*&", lines)
  lines[dyad_rows] <- sub("dyad", "Dyad fixed effects", lines[dyad_rows], fixed = TRUE)
  table_line <- which(grepl("^\\s*\\\\begin\\{table\\}", lines))[1]
  if (!is.na(table_line)) {
    if (!any(grepl("\\\\color\\{red\\}", lines))) {
      lines <- append(lines, "\\color{red}", after = table_line)
    }
    centering_line <- which(grepl("^\\s*\\\\centering", lines))[1]
    migration_style <- if (grepl("PreWindowMeanSalinity_2000_2010", out_stub, fixed = TRUE)) {
      c("\\footnotesize", "\\setlength{\\tabcolsep}{4pt}", "\\renewcommand{\\arraystretch}{0.86}")
    } else if (grepl("IntervalMeanSalinity_1991_2010", out_stub, fixed = TRUE) ||
               grepl("DuringMeanSalinity_1991_2010", out_stub, fixed = TRUE)) {
      c("\\footnotesize", "\\setlength{\\tabcolsep}{4pt}", "\\renewcommand{\\arraystretch}{0.84}")
    } else {
      character()
    }
    missing_style <- !vapply(
      migration_style,
      function(x) any(grepl(x, lines, fixed = TRUE)),
      logical(1)
    )
    if (!is.na(centering_line) && any(missing_style)) {
      lines <- append(
        lines,
        migration_style[missing_style],
        after = centering_line
      )
    }
    writeLines(lines, tex_file, useBytes = TRUE)
  }
  income_heterogeneity <- if (grepl(
    "IntervalMeanSalinity_1991_2010", out_stub, fixed = TRUE
  )) {
    estimate_rural_migration_income_heterogeneity(d, weather)
  } else {
    NULL
  }
  destination_agriculture <- if (grepl(
    "IntervalMeanSalinity_1991_2010", out_stub, fixed = TRUE
  )) {
    estimate_destination_agricultural_migration(panel)
  } else {
    NULL
  }
  invisible(list(
    models = list(m3, m4), sample = sample_diag, data = d,
    income_heterogeneity = income_heterogeneity,
    destination_agriculture = destination_agriculture
  ))
}

build_fixed_baseline_rural_outmigration_panel <- function(aggregates, exposure) {
  current <- copy(aggregates$current_region)
  flows <- copy(aggregates$flows)
  current[, `:=`(
    YEAR = as.character(YEAR),
    region = trimws(as.character(region))
  )]
  flows[, `:=`(
    YEAR = as.character(YEAR),
    orig = trimws(as.character(orig))
  )]
  exposure <- copy(exposure)
  exposure[, `:=`(
    YEAR = as.character(YEAR),
    GEO2_BR = trimws(as.character(GEO2_BR))
  )]

  baseline_1980 <- current[
    YEAR == "1980" & is.finite(rural_share) & rural_share >= 0.5,
    .(
      orig = region,
      rural_share_1980 = rural_share,
      ag_labor_share_1980 = ag_labor_share,
      population_1980 = current_population
    )
  ]
  baseline_1980[, z_ag_labor_share_1980 := scale_safe(ag_labor_share_1980)]
  baseline_1980[, ag_specialization_bin_1980 := cut(
    ag_labor_share_1980,
    breaks = unique(stats::quantile(ag_labor_share_1980, probs = seq(0, 1, 0.2), na.rm = TRUE)),
    include.lowest = TRUE,
    labels = FALSE
  )]

  baseline_map <- data.table(
    YEAR = c("1991", "2000", "2010"),
    baseline_year = c("1980", "1991", "2000")
  )
  population <- merge(
    baseline_map,
    current[, .(
      baseline_year = YEAR,
      orig = region,
      origin_population = current_population
    )],
    by = "baseline_year",
    allow.cartesian = TRUE
  )
  years <- sort(intersect(unique(exposure$YEAR), baseline_map$YEAR))
  origins <- sort(intersect(baseline_1980$orig, exposure$GEO2_BR))
  panel <- data.table::CJ(YEAR = years, orig = origins, unique = TRUE)
  panel <- merge(panel, baseline_1980, by = "orig", all.x = TRUE)
  panel <- merge(panel, population, by = c("YEAR", "orig"), all.x = TRUE)
  total_flows <- flows[
    YEAR %in% years & orig %in% origins,
    .(outmigrant_flow = sum(migrant_flow, na.rm = TRUE)),
    by = .(YEAR, orig)
  ]
  panel <- merge(panel, total_flows, by = c("YEAR", "orig"), all.x = TRUE)
  panel[is.na(outmigrant_flow), outmigrant_flow := 0]
  expo <- copy(exposure)
  setnames(expo, "GEO2_BR", "orig")
  panel <- merge(panel, expo, by = c("YEAR", "orig"), all.x = TRUE)
  panel[, `:=`(
    outmigration_rate_per_1000 = 1000 * outmigrant_flow / origin_population,
    log_outmigration_rate = fifelse(
      is.finite(outmigrant_flow) & outmigrant_flow > 0 &
        is.finite(origin_population) & origin_population > 0,
      log(outmigrant_flow / origin_population),
      NA_real_
    ),
    log_origin_population = log(origin_population),
    ag_specialization_bin_1980 = factor(ag_specialization_bin_1980)
  )]
  panel[is.finite(origin_population) & origin_population > 0]
}

estimate_fixed_baseline_rural_outmigration <- function(
  panel,
  out_stub,
  desc_stub,
  title,
  label,
  exposure_note
) {
  write_status("Estimating origin-level outmigration from fixed 1980 rural origins.")
  treatment <- "z_mean_salinity"
  weather <- c("z_gdd_large", "z_kdd_large", "z_sm_season_large")
  needed <- c(
    "outmigrant_flow", "origin_population", treatment, weather,
    "z_ag_labor_share_1980", "ag_specialization_bin_1980",
    "orig", "YEAR", "state", "lon", "lat"
  )
  d <- complete_data(panel, needed)
  d[, `:=`(
    state = factor(state),
    YEAR = factor(YEAR),
    orig = factor(orig)
  )]
  rhs_weather <- paste(c(treatment, weather), collapse = " + ")
  f1 <- as.formula(paste("outmigrant_flow ~", treatment, "| orig + state^YEAR"))
  f2 <- as.formula(paste("outmigrant_flow ~", rhs_weather, "| orig + state^YEAR"))
  f3 <- as.formula(paste(
    "outmigrant_flow ~", rhs_weather,
    "+ z_mean_salinity:z_ag_labor_share_1980 | orig + state^YEAR"
  ))
  vc <- make_conley()
  m1 <- fixest::fepois(f1, data = d, offset = ~log(origin_population), vcov = vc, notes = FALSE)
  m2 <- fixest::fepois(f2, data = d, offset = ~log(origin_population), vcov = vc, notes = FALSE)
  m3 <- fixest::fepois(f3, data = d, offset = ~log(origin_population), vcov = vc, notes = FALSE)

  d_log <- complete_data(d, c(
    "log_outmigration_rate", treatment, weather, "z_ag_labor_share_1980",
    "orig", "YEAR", "state", "lon", "lat"
  ))
  ols_f1 <- as.formula(paste("log_outmigration_rate ~", treatment, "| orig + state^YEAR"))
  ols_f2 <- as.formula(paste("log_outmigration_rate ~", rhs_weather, "| orig + state^YEAR"))
  ols_f3 <- as.formula(paste(
    "log_outmigration_rate ~", rhs_weather,
    "+ z_mean_salinity:z_ag_labor_share_1980 | orig + state^YEAR"
  ))
  ols_models <- list(
    fixest::feols(ols_f1, data = d_log, vcov = vc, notes = FALSE),
    fixest::feols(ols_f2, data = d_log, vcov = vc, notes = FALSE),
    fixest::feols(ols_f3, data = d_log, vcov = vc, notes = FALSE)
  )

  sample_diag <- data.table(
    Metric = c(
      "Fixed baseline-rural GEO2 origins",
      "Census years",
      "Origin-year observations before PPML fixed-effect checks",
      "Origin-year observations used by PPML",
      "Zero-outmigration origin-years",
      "Mean outmigration rate per 1,000 baseline residents",
      "Median 1980 rural share (percent)",
      "Median 1980 agricultural-labor share (percent)"
    ),
    Value = c(
      formatC(uniqueN(d$orig), format = "d", big.mark = ","),
      paste(sort(unique(as.character(d$YEAR))), collapse = ", "),
      formatC(nrow(d), format = "d", big.mark = ","),
      formatC(nobs(m2), format = "d", big.mark = ","),
      formatC(sum(d$outmigrant_flow == 0), format = "d", big.mark = ","),
      formatC(mean(d$outmigration_rate_per_1000), digits = 2, format = "f"),
      formatC(100 * stats::median(d$rural_share_1980), digits = 1, format = "f"),
      formatC(stats::median(d$ag_labor_share_1980), digits = 1, format = "f")
    )
  )
  write_latex_df(
    sample_diag,
    file.path(paths$out_dir, paste0(desc_stub, ".tex")),
    "Origin-Level Migration Sample",
    paste0("tab:", tolower(gsub("_", "-", desc_stub))),
    note = paste(
      "An observation is a GEO2 origin and census year.",
      "The sample is fixed using a rural share of at least 50 percent in 1980 and is not reclassified after migration.",
      "The outcome sums migration from the origin to all observed Brazilian destinations and is divided by the previous-census population through the PPML offset."
    ),
    size = "\\small"
  )
  fwrite(sample_diag, file.path(paths$out_dir, paste0(desc_stub, ".csv")))

  fixest::setFixest_dict(c(
    z_mean_salinity = "Origin mean salinity (std.)",
    z_gdd_large = "Origin GDD (std.)",
    z_kdd_large = "Origin KDD (std.)",
    z_sm_season_large = "Origin soil moisture (std.)",
    `z_mean_salinity:z_ag_labor_share_1980` = "Salinity $\\times$ baseline agricultural specialization"
  ), reset = TRUE)
  tex_file <- file.path(paths$out_dir, paste0(out_stub, ".tex"))
  fixest::etable(
    m1, m2, m3,
    tex = TRUE,
    file = tex_file,
    replace = TRUE,
    title = title,
    label = label,
    fitstat = ~ n + sq.cor + pr2,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    notes = paste(
      "Notes: PPML at the origin-year level; the outcome is total weighted outmigration to all Brazilian destinations.",
      "Origins are fixed as rural using their 1980 rural share. The previous-census population enters as an offset.",
      exposure_note,
      "All columns include origin and state-by-census-year fixed effects. Spec. 2 adds weather and is preferred; Spec. 3 tests whether the association varies with the standardized 1980 agricultural-labor share.",
      "Standard errors are Conley spatial with a 200 km cutoff."
    )
  )
  style_fixest_tex(tex_file, size = "\\scriptsize", tabcolsep = "3pt", arraystretch = "0.78")

  ols_file <- file.path(paths$out_dir, paste0(out_stub, "_LogRate_OLS.tex"))
  fixest::etable(
    ols_models,
    tex = TRUE,
    file = ols_file,
    replace = TRUE,
    title = paste(title, "-- Log-Rate Robustness"),
    label = paste0(label, "_log_rate"),
    fitstat = ~ n + r2,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    notes = paste(
      "Notes: OLS at the origin-year level; the outcome is the log of total weighted outmigration divided by the previous-census population.",
      "The sample and fixed effects match the PPML table.",
      exposure_note,
      "Standard errors are Conley spatial with a 200 km cutoff."
    )
  )
  style_fixest_tex(ols_file, size = "\\scriptsize", tabcolsep = "3pt", arraystretch = "0.78")
  invisible(list(
    data = d,
    models = list(m1, m2, m3),
    ols_models = ols_models,
    sample = sample_diag
  ))
}

make_ag_labor_share_analysis <- function(aggregates, exposure_during) {
  write_status("Estimating agricultural labor reallocation in temporal first differences.")
  labor <- copy(aggregates$current_region)
  labor[, `:=`(
    YEAR = as.character(YEAR),
    GEO2_BR = trimws(as.character(region))
  )]
  baseline <- labor[YEAR == "1980", .(
    GEO2_BR,
    ag_labor_share_1980 = ag_labor_share,
    ag_employment_rate_1980 = ag_employment_rate
  )]
  baseline[, baseline_ag_share_bin := cut(
    ag_labor_share_1980,
    breaks = unique(stats::quantile(ag_labor_share_1980, probs = seq(0, 1, 0.2), na.rm = TRUE)),
    include.lowest = TRUE,
    labels = FALSE
  )]
  baseline[, baseline_ag_share_bin := factor(baseline_ag_share_bin)]
  d <- merge(labor, exposure_during, by = c("GEO2_BR", "YEAR"), all.x = FALSE)
  d <- merge(d, baseline, by = "GEO2_BR", all.x = TRUE)
  setorder(d, GEO2_BR, YEAR)
  fd_vars <- c(
    "ag_labor_share", "ag_employment_rate", "nonag_employment_rate",
    "mean_salinity", "gdd_large", "kdd_large", "sm_season_large"
  )
  d[, paste0(fd_vars, "_lag") := shift(.SD, 1L), by = GEO2_BR, .SDcols = fd_vars]
  d[, `:=`(
    delta_ag_labor_share = ag_labor_share - ag_labor_share_lag,
    delta_ag_employment_rate = ag_employment_rate - ag_employment_rate_lag,
    delta_nonag_employment_rate = nonag_employment_rate - nonag_employment_rate_lag,
    delta_mean_salinity = mean_salinity - mean_salinity_lag,
    delta_gdd_large = gdd_large - gdd_large_lag,
    delta_kdd_large = kdd_large - kdd_large_lag,
    delta_sm_season_large = sm_season_large - sm_season_large_lag
  )]
  delta_vars <- c(
    "delta_mean_salinity", "delta_gdd_large", "delta_kdd_large",
    "delta_sm_season_large"
  )
  d[, paste0("z_", delta_vars) := lapply(.SD, scale_safe), by = YEAR, .SDcols = delta_vars]
  needed <- c(
    "delta_ag_labor_share", "delta_ag_employment_rate", "delta_nonag_employment_rate",
    paste0("z_", delta_vars), "baseline_ag_share_bin", "GEO2_BR", "YEAR",
    "state", "lat", "lon"
  )
  d <- complete_data(d, needed)
  outcomes <- c(
    delta_ag_labor_share = "Agricultural share among workers",
    delta_ag_employment_rate = "Agricultural employment / population 15+"
  )
  vc <- make_conley()
  models <- list()
  headers <- character()
  for (outcome in names(outcomes)) {
    f1 <- as.formula(paste(outcome, "~ z_delta_mean_salinity | state^YEAR"))
    f2 <- as.formula(paste(
      outcome,
      "~ z_delta_mean_salinity + z_delta_gdd_large + z_delta_kdd_large + z_delta_sm_season_large | state^YEAR"
    ))
    f3 <- as.formula(paste(
      outcome,
      "~ z_delta_mean_salinity + z_delta_gdd_large + z_delta_kdd_large + z_delta_sm_season_large | state^YEAR + baseline_ag_share_bin^YEAR"
    ))
    crop_models <- list(
      fixest::feols(f1, data = d, vcov = vc, notes = FALSE),
      fixest::feols(f2, data = d, vcov = vc, notes = FALSE),
      fixest::feols(f3, data = d, vcov = vc, notes = FALSE)
    )
    for (s in seq_along(crop_models)) {
      models[[paste(outcomes[[outcome]], paste0("Spec. ", s), sep = " - ")]] <- crop_models[[s]]
    }
    headers <- c(headers, rep(outcomes[[outcome]], 3L))
  }
  fixest::setFixest_dict(c(
    z_delta_mean_salinity = "Change in mean salinity (std.)",
    z_delta_gdd_large = "Change in GDD (std.)",
    z_delta_kdd_large = "Change in KDD (std.)",
    z_delta_sm_season_large = "Change in soil moisture (std.)"
  ), reset = TRUE)

  model_terms <- c(
    "z_delta_mean_salinity", "z_delta_gdd_large",
    "z_delta_kdd_large", "z_delta_sm_season_large"
  )
  term_labels <- c(
    z_delta_mean_salinity = "Change in mean salinity (std.)",
    z_delta_gdd_large = "Change in GDD (std.)",
    z_delta_kdd_large = "Change in KDD (std.)",
    z_delta_sm_season_large = "Change in soil moisture (std.)"
  )
  extract_labor_term <- function(model, term_name) {
    beta <- stats::coef(model)
    standard_errors <- sqrt(diag(stats::vcov(model)))
    p_values <- 2 * stats::pnorm(-abs(beta / standard_errors))
    data.table(
      term = term_name,
      estimate = unname(beta[term_name]),
      standard_error = unname(standard_errors[term_name]),
      p_value = unname(p_values[term_name])
    )
  }
  coefficient_rows <- rbindlist(lapply(seq_along(models), function(i) {
    rbindlist(lapply(model_terms, function(term_name) {
      cbind(
        data.table(
          outcome = headers[i],
          specification = paste0("Spec. ", ((i - 1L) %% 3L) + 1L),
          model_index = i
        ),
        extract_labor_term(models[[i]], term_name)
      )
    }))
  }))
  coefficient_rows[, `:=`(
    ci90_low = estimate - 1.645 * standard_error,
    ci90_high = estimate + 1.645 * standard_error,
    ci95_low = estimate - 1.96 * standard_error,
    ci95_high = estimate + 1.96 * standard_error
  )]
  fwrite(
    coefficient_rows,
    file.path(paths$out_dir, "FullRevision_Agricultural_Labor_Share_Coefficients.csv")
  )

  plot_data <- coefficient_rows[term == "z_delta_mean_salinity"]
  plot_data[, `:=`(
    outcome = factor(outcome, levels = rev(unname(outcomes))),
    specification = factor(specification, levels = c("Spec. 1", "Spec. 2", "Spec. 3"))
  )]
  plot_data[, y_position := as.numeric(outcome) + c(
    "Spec. 1" = -0.16,
    "Spec. 2" = 0,
    "Spec. 3" = 0.16
  )[as.character(specification)]]
  labor_figure <- ggplot(plot_data, aes(estimate, y_position, color = specification)) +
    geom_vline(xintercept = 0, linewidth = 0.45, color = "grey45", linetype = "dashed") +
    geom_segment(
      aes(x = ci95_low, xend = ci95_high, yend = y_position),
      linewidth = 0.55
    ) +
    geom_segment(
      aes(x = ci90_low, xend = ci90_high, yend = y_position),
      linewidth = 1.10
    ) +
    geom_point(size = 2.5) +
    scale_color_manual(
      values = c("Spec. 1" = "#6A51A3", "Spec. 2" = "#D95F02", "Spec. 3" = "#1B9E77"),
      name = NULL
    ) +
    scale_y_continuous(
      breaks = seq_along(levels(plot_data$outcome)),
      labels = c("Agricultural employment /\npopulation 15+", "Agricultural share\namong workers")
    ) +
    labs(
      x = "Effect of a one-SD increase in mean salinity\non employment change (percentage points)",
      y = NULL
    ) +
    theme_minimal(base_size = 12.5) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text = element_text(size = 11, color = "black"),
      axis.title.x = element_text(size = 11.5),
      legend.text = element_text(size = 11),
      plot.margin = margin(8, 10, 8, 10)
    )
  ggsave(
    file.path(paths$out_dir, "FullRevision_Agricultural_Labor_Share_Coefficients.pdf"),
    labor_figure, width = 7.5, height = 3.9
  )
  ggsave(
    file.path(paths$out_dir, "FullRevision_Agricultural_Labor_Share_Coefficients.png"),
    labor_figure, width = 7.5, height = 3.9, dpi = 320
  )

  tex_file <- file.path(paths$out_dir, "FullRevision_Agricultural_Labor_Share_Change.tex")
  estimate_cell <- function(model_index, term_name) {
    target_index <- model_index
    target_term <- term_name
    row <- coefficient_rows[get("model_index") == target_index & term == target_term]
    if (nrow(row) == 0L || !is.finite(row$estimate)) return("")
    paste0(fmt(row$estimate, 4), stars(row$p_value))
  }
  se_cell <- function(model_index, term_name) {
    target_index <- model_index
    target_term <- term_name
    row <- coefficient_rows[get("model_index") == target_index & term == target_term]
    if (nrow(row) == 0L || !is.finite(row$standard_error)) return("")
    paste0("(", fmt(row$standard_error, 4), ")")
  }
  table_lines <- c(
    "\\begin{table}[!htbp]",
    "\\color{red}",
    "\\centering",
    "\\caption{Changes in Salinity and Agricultural Employment: Full Results}",
    "\\label{tab:full_revision_ag_labor_share_change}",
    "\\footnotesize",
    "\\setlength{\\tabcolsep}{5pt}",
    "\\renewcommand{\\arraystretch}{0.90}",
    "\\begin{tabular}{lrrr}",
    "\\toprule"
  )
  for (panel_index in seq_along(outcomes)) {
    outcome_name <- unname(outcomes[panel_index])
    model_indices <- ((panel_index - 1L) * 3L + 1L):(panel_index * 3L)
    table_lines <- c(
      table_lines,
      paste0(
        "\\multicolumn{4}{l}{\\textit{Panel ",
        LETTERS[panel_index], ": ", latex_escape(outcome_name), "}}\\\\"
      ),
      "Variable & Spec. 1 & Spec. 2 & Spec. 3\\\\",
      "\\midrule"
    )
    for (term_name in model_terms) {
      table_lines <- c(
        table_lines,
        paste(
          c(
            latex_escape(unname(term_labels[term_name])),
            vapply(model_indices, estimate_cell, character(1), term_name = term_name)
          ),
          collapse = " & "
        ),
        "\\\\",
        paste(
          c("", vapply(model_indices, se_cell, character(1), term_name = term_name)),
          collapse = " & "
        ),
        "\\\\"
      )
    }
    table_lines <- c(
      table_lines,
      "State-by-census fixed effects & Yes & Yes & Yes\\\\",
      "Initial-specialization-by-census fixed effects & No & No & Yes\\\\",
      paste0(
        "Observations & ",
        paste(vapply(models[model_indices], function(model) {
          formatC(stats::nobs(model), format = "d", big.mark = ",")
        }, character(1)), collapse = " & "),
        "\\\\"
      ),
      paste0(
        "R2 & ",
        paste(vapply(models[model_indices], function(model) {
          fmt(as.numeric(fixest::fitstat(model, "r2")$r2), 3)
        }, character(1)), collapse = " & "),
        "\\\\"
      )
    )
    if (panel_index < length(outcomes)) table_lines <- c(table_lines, "\\midrule")
  }
  table_lines <- c(
    table_lines,
    "\\bottomrule",
    "\\end{tabular}",
    "\\par\\addvspace{0.5ex}",
    "\\parbox{0.96\\linewidth}{\\footnotesize\\textit{Notes:} ",
    latex_escape(paste(
      "OLS in temporal first differences at the harmonized GEO2 census-region level for 1991--2000 and 2000--2010.",
      "Panel A uses agriculture's percentage-point share among workers with an observed industry; Panel B uses agricultural workers as a percentage of the population aged 15 and over.",
      "All specifications include state-by-census fixed effects. Spec. 2 adds changes in GDD, KDD and soil moisture and is preferred.",
      "Spec. 3 also includes 1980 agricultural-specialization-quintile-by-census fixed effects.",
      "Income and contemporaneous demographic composition are excluded because they may respond to salinity.",
      "Conley spatial standard errors with a 200 km cutoff are in parentheses."
    )),
    "}",
    "\\end{table}"
  )
  writeLines(table_lines, tex_file, useBytes = TRUE)

  diagnostic_models <- list()
  for (outcome in c("delta_nonag_employment_rate")) {
    f <- as.formula(paste(
      outcome,
      "~ z_delta_mean_salinity + z_delta_gdd_large + z_delta_kdd_large + z_delta_sm_season_large | state^YEAR"
    ))
    diagnostic_models[["Non-agricultural employment / population 15+"]] <- fixest::feols(
      f,
      data = d,
      vcov = vc,
      notes = FALSE
    )
  }
  diagnostic_file <- file.path(paths$out_dir, "FullRevision_NonAgricultural_Employment_Change.tex")
  fixest::etable(
    diagnostic_models,
    tex = TRUE,
    file = diagnostic_file,
    replace = TRUE,
    depvar = FALSE,
    title = "Salinity and Non-Agricultural Employment",
    label = "tab:full_revision_nonag_employment_change",
    fitstat = ~ n + r2,
    notes = paste(
      "Notes: The outcome is the temporal change in non-agricultural workers as a percentage of the population aged 15 and over.",
      "The model matches preferred Specification 2 in the main agricultural-employment table and uses Conley spatial standard errors at 200 km."
    )
  )
  style_fixest_tex(diagnostic_file, size = "\\scriptsize", tabcolsep = "3pt", arraystretch = "0.85")
  invisible(list(data = d, models = models, diagnostic_models = diagnostic_models))
}

make_ag_labor_share_sfd_fd_analysis <- function(aggregates, exposure_during) {
  write_status("Estimating agricultural labor-share SFD of temporal first differences.")
  cw_file <- file.path(paths$project_dir, "MT2", "research_inputs", "muni_to_ipums_geo2_crosswalk.csv")
  if (!file.exists(cw_file)) {
    write_status("Skipping agricultural labor SFD-FD: municipality-GEO2 crosswalk is missing.")
    return(NULL)
  }

  labor <- copy(aggregates$current_region)
  labor[, `:=`(YEAR = as.character(YEAR), GEO2_BR = trimws(as.character(region)))]
  d <- merge(labor, exposure_during, by = c("GEO2_BR", "YEAR"), all.x = FALSE)
  setorder(d, GEO2_BR, YEAR)
  fd_vars <- c(
    "ag_labor_share", "mean_salinity", "gdd_large", "kdd_large",
    "sm_season_large"
  )
  d[, paste0(fd_vars, "_lag") := shift(.SD, 1L), by = GEO2_BR, .SDcols = fd_vars]
  d[, `:=`(
    delta_ag_labor_share = ag_labor_share - ag_labor_share_lag,
    delta_mean_salinity = mean_salinity - mean_salinity_lag,
    delta_gdd_large = gdd_large - gdd_large_lag,
    delta_kdd_large = kdd_large - kdd_large_lag,
    delta_sm_season_large = sm_season_large - sm_season_large_lag
  )]
  delta_vars <- c(
    "delta_ag_labor_share", "delta_mean_salinity", "delta_gdd_large",
    "delta_kdd_large", "delta_sm_season_large"
  )
  d <- complete_data(d, c("GEO2_BR", "YEAR", delta_vars))

  cw <- fread(cw_file, encoding = "UTF-8", colClasses = list(character = c("Code", "GEO2_BR")))
  cw[, `:=`(Code = trimws(as.character(Code)), GEO2_BR = trimws(as.character(GEO2_BR)))]
  pair_map <- read_pair_map()
  pair_geo <- merge(
    pair_map,
    cw[, .(Code, GEO2_east = GEO2_BR)],
    by = "Code",
    all.x = FALSE
  )
  pair_geo <- merge(
    pair_geo,
    cw[, .(code_neighbor_west = Code, GEO2_west = GEO2_BR)],
    by = "code_neighbor_west",
    all.x = FALSE
  )
  pair_geo <- pair_geo[
    !is.na(GEO2_east) & !is.na(GEO2_west) &
      nzchar(GEO2_east) & nzchar(GEO2_west) &
      GEO2_east != GEO2_west
  ]
  pair_geo[, geo2_pair := paste(GEO2_east, GEO2_west, sep = "__")]
  pair_geo <- pair_geo[, .(
    lat = mean(lat, na.rm = TRUE),
    lon = mean(lon, na.rm = TRUE),
    state = {
      s <- as.character(state)
      if (all(is.na(s))) NA_character_ else s[which(!is.na(s))[1]]
    },
    municipal_pairs = uniqueN(pair_id)
  ), by = .(geo2_pair, GEO2_east, GEO2_west)]

  east <- merge(pair_geo, d, by.x = "GEO2_east", by.y = "GEO2_BR", allow.cartesian = TRUE)
  west <- copy(d[, c("GEO2_BR", "YEAR", delta_vars), with = FALSE])
  setnames(west, "GEO2_BR", "GEO2_west")
  setnames(west, delta_vars, paste0(delta_vars, "_west"))
  sfd <- merge(east, west, by = c("GEO2_west", "YEAR"), allow.cartesian = FALSE)
  if (!"state" %in% names(sfd)) {
    state_candidates <- intersect(c("state.x", "state.y"), names(sfd))
    if (length(state_candidates) == 0L) stop("State is missing from the agricultural-labor SFD data.")
    sfd[, state := as.character(get(state_candidates[1L]))]
    if (length(state_candidates) > 1L) {
      sfd[is.na(state) | !nzchar(state), state := as.character(get(state_candidates[2L]))]
    }
  }
  for (coord in c("lat", "lon")) {
    if (!coord %in% names(sfd)) {
      candidates <- intersect(paste0(coord, c(".x", ".y")), names(sfd))
      if (length(candidates) == 0L) stop(coord, " is missing from the agricultural-labor SFD data.")
      sfd[, (coord) := as.numeric(get(candidates[1L]))]
      if (length(candidates) > 1L) {
        sfd[!is.finite(get(coord)), (coord) := as.numeric(get(candidates[2L]))]
      }
    }
  }
  for (v in delta_vars) {
    sfd[, paste0("sfd_", v) := get(v) - get(paste0(v, "_west"))]
  }
  sfd_delta_vars <- paste0("sfd_", setdiff(delta_vars, "delta_ag_labor_share"))
  sfd[, paste0("z_", sfd_delta_vars) := lapply(.SD, scale_safe), by = YEAR, .SDcols = sfd_delta_vars]
  needed <- c(
    "sfd_delta_ag_labor_share",
    paste0("z_", sfd_delta_vars),
    "YEAR", "state", "geo2_pair"
  )
  sfd <- complete_data(sfd, needed)
  if (nrow(sfd) < 100L) {
    write_status("Skipping agricultural labor SFD-FD: fewer than 100 complete observations.")
    return(NULL)
  }

  f1 <- sfd_delta_ag_labor_share ~ z_sfd_delta_mean_salinity | YEAR + state
  f2 <- sfd_delta_ag_labor_share ~ z_sfd_delta_mean_salinity + z_sfd_delta_gdd_large + z_sfd_delta_kdd_large + z_sfd_delta_sm_season_large | YEAR + state
  vc <- make_conley()
  m1 <- fixest::feols(f1, data = sfd, vcov = vc, notes = FALSE)
  m2 <- fixest::feols(f2, data = sfd, vcov = vc, notes = FALSE)

  fixest::setFixest_dict(c(
    z_sfd_delta_mean_salinity = "SFD change in mean salinity (std.)",
    z_sfd_delta_gdd_large = "SFD change in GDD (std.)",
    z_sfd_delta_kdd_large = "SFD change in KDD (std.)",
    z_sfd_delta_sm_season_large = "SFD change in soil moisture (std.)"
  ), reset = TRUE)
  tex_file <- file.path(paths$out_dir, "FullRevision_Agricultural_Labor_Share_SFD_FD.tex")
  fixest::etable(
    m1, m2,
    tex = TRUE,
    file = tex_file,
    replace = TRUE,
    depvar = FALSE,
    title = "SFD of Changes in Mean Salinity and Agricultural-Labor Share",
    label = "tab:full_revision_ag_labor_share_sfd_fd",
    fitstat = ~ r2 + n,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    notes = c(
      "Notes: The dependent variable is the spatial difference in the change in the agricultural-labor share between consecutive census observations.",
      "The specification first differences the GEO2 outcome over time and then differences those changes across adjacent GEO2 pairs induced by municipal west-east pairs.",
      "Salinity and climate changes are standardized within census year after the spatial difference is formed.",
      "All specifications include census-year and state fixed effects. Income and demographic composition are excluded because they may respond to salinity.",
      "Standard errors are Conley spatial with a 200 km cutoff.",
      "This is a robustness exercise because the adjacency is built from municipality pairs and then aggregated to harmonized GEO2 regions."
    )
  )
  style_fixest_tex(tex_file, size = "\\scriptsize", tabcolsep = "3pt", arraystretch = "0.86")
  invisible(list(data = sfd, models = list(m1, m2)))
}

# =============================================================================
# 9. Farm-size heterogeneity and additional crop diagnostics
# =============================================================================
farm_size_crop_map <- function() {
  rbindlist(list(
    data.table(
      census_year = 1995L,
      crop = c("rice", "sugarcane", rep("beans", 3L), "cassava", "corn", "soy"),
      crop_code = c("4851", "4857", "4866", "4867", "4868", "4885", "4888", "4896")
    ),
    data.table(
      census_year = 2006L,
      crop = c("rice", "sugarcane", rep("beans", 3L), "cassava", "corn", "soy"),
      crop_code = c("4851", "4857", "111675", "111676", "111677", "4885", "4888", "4896")
    ),
    data.table(
      census_year = 2017L,
      crop = c("rice", "sugarcane", rep("beans", 3L), "cassava", "corn", "soy"),
      crop_code = c("4851", "4857", "111675", "111676", "111677", "4885", "4888", "4896")
    )
  ))
}

farm_size_class_map <- function() {
  rbindlist(list(
    data.table(
      census_year = 1995L,
      size_group = c("small", "medium", "large"),
      size_code = c("4795", "4797", "4800")
    ),
    data.table(
      census_year = 2006L,
      size_group = c("small", "medium", "large"),
      size_code = c("111551", "111553", "111556")
    ),
    data.table(
      census_year = 2017L,
      size_group = c("small", "medium", "large"),
      size_code = c("111551", "111553", "111556")
    )
  ))
}

farm_size_measure_map <- function() {
  data.table(
    census_year = c(1995L, 1995L, 2006L, 2006L, 2017L, 2017L),
    variable_code = c("214", "216", "214", "216", "10085", "10089"),
    measure = c(
      "quantity_tonnes", "harvested_area_ha",
      "quantity_tonnes", "harvested_area_ha",
      "quantity_tonnes", "harvested_area_ha"
    )
  )
}

farm_size_sidra_specs <- function() {
  crop_map <- farm_size_crop_map()
  list(
    list(
      key = "farm_size_crop_1995_quantity_497_selected3",
      table = 497,
      period = "1995",
      variable = "214",
      classific = c("226", "220"),
      category = list(
        unique(crop_map[census_year == 1995L, crop_code]),
        farm_size_class_map()[census_year == 1995L, size_code]
      )
    ),
    list(
      key = "farm_size_crop_1995_area_503_selected3",
      table = 503,
      period = "1995",
      variable = "216",
      classific = c("226", "220"),
      category = list(
        unique(crop_map[census_year == 1995L, crop_code]),
        farm_size_class_map()[census_year == 1995L, size_code]
      )
    ),
    list(
      key = "farm_size_crop_2006_822_selected3",
      table = 822,
      period = "2006",
      variable = c("214", "216"),
      classific = c("226", "218", "12517", "220", "12523"),
      category = list(
        unique(crop_map[census_year == 2006L, crop_code]),
        "0", "113601", farm_size_class_map()[census_year == 2006L, size_code], "0"
      )
    ),
    list(
      key = "farm_size_crop_2017_6959_selected3",
      table = 6959,
      period = "2017",
      variable = c("10085", "10089"),
      classific = c("829", "226", "220"),
      category = list(
        "46302",
        unique(crop_map[census_year == 2017L, crop_code]),
        farm_size_class_map()[census_year == 2017L, size_code]
      )
    )
  )
}

tidy_sidra_farm_size_raw <- function(raw, spec) {
  if (is.null(raw) || nrow(raw) == 0L) return(NULL)
  raw <- as.data.table(raw)
  code_col <- find_sidra_column(raw, "Municipio", code = TRUE)
  variable_col <- find_sidra_column(raw, "Variavel", code = TRUE)
  crop_col <- find_sidra_column(raw, "Produtos da lavoura temporaria", code = TRUE)
  size_col <- find_sidra_column(raw, "Grupos de area total", code = TRUE)
  needed <- c(code_col, variable_col, crop_col, size_col)
  if (anyNA(needed) || !"Valor" %in% names(raw)) {
    stop("Could not identify the SIDRA columns for ", spec$key, ".")
  }

  raw_col <- if ("Valor_raw" %in% names(raw)) "Valor_raw" else "Valor"
  value_raw <- trimws(as.character(raw[[raw_col]]))
  value <- as_number(raw[["Valor"]])
  value[value_raw %chin% c("-", "0")] <- 0
  suppressed <- toupper(value_raw) == "X"
  value[suppressed] <- NA_real_

  out <- data.table(
    dataset_key = spec$key,
    sidra_table = as.integer(spec$table),
    census_year = as.integer(spec$period),
    Code = trimws(as.character(raw[[code_col]])),
    variable_code = trimws(as.character(raw[[variable_col]])),
    crop_code = trimws(as.character(raw[[crop_col]])),
    size_code = trimws(as.character(raw[[size_col]])),
    value = value,
    value_raw = value_raw,
    suppressed = suppressed
  )
  out[grepl("^[0-9]{6}$", Code), Code := paste0("0", Code)]
  out
}

download_sidra_farm_size_spec <- function(
    spec,
    force = FALSE,
    timeout_sec = 120,
    chunk_size = 700L
) {
  ensure_output_dir()
  tidy_file <- file.path(paths$sidra_dir, paste0(spec$key, "_tidy_n", chunk_size, ".csv"))
  if (!force && file.exists(tidy_file) && file.info(tidy_file)$size > 0) {
    return(fread(tidy_file, encoding = "UTF-8", colClasses = list(character = c(
      "dataset_key", "Code", "variable_code", "crop_code", "size_code", "value_raw"
    ))))
  }

  old_timeout <- getOption("sidrar.timeout")
  old_retries <- getOption("sidrar.retries")
  on.exit({
    options(sidrar.timeout = old_timeout)
    options(sidrar.retries = old_retries)
  }, add = TRUE)
  options(sidrar.timeout = timeout_sec, sidrar.retries = 1)

  fetch_codes <- function(codes, chunk_id) {
    chunk_file <- file.path(
      paths$sidra_dir,
      paste0(spec$key, "_n", chunk_size, "_chunk_", chunk_id, ".csv")
    )
    if (!force && file.exists(chunk_file) && file.info(chunk_file)$size > 0) {
      raw <- fread(chunk_file, encoding = "UTF-8")
      return(tidy_sidra_farm_size_raw(raw, spec))
    }

    write_status(
      "Downloading SIDRA farm-size table ", spec$table,
      " for municipality chunk ", chunk_id,
      " (", length(codes), " municipalities)."
    )
    raw <- try(
      sidrar::get_sidra(
        x = spec$table,
        variable = as.character(spec$variable),
        period = spec$period,
        geo = "City",
        geo.filter = list(City = codes),
        classific = spec$classific,
        category = spec$category,
        format = 1,
        value_type = "both"
      ),
      silent = TRUE
    )
    if (inherits(raw, "try-error")) {
      write_status(
        "SIDRA farm-size table ", spec$table,
        " failed for chunk ", chunk_id, ": ", as.character(raw)
      )
      if (length(codes) > 1L) {
        mid <- ceiling(length(codes) / 2)
        return(rbindlist(list(
          fetch_codes(codes[seq_len(mid)], paste0(chunk_id, "a")),
          fetch_codes(codes[(mid + 1L):length(codes)], paste0(chunk_id, "b"))
        ), fill = TRUE))
      }
      return(NULL)
    }

    raw <- as.data.table(raw)
    raw[, chunk_id := chunk_id]
    fwrite(raw, chunk_file)
    out <- tidy_sidra_farm_size_raw(raw, spec)
    rm(raw)
    gc(FALSE)
    Sys.sleep(0.15)
    out
  }

  code_chunks <- sidra_pair_sample_codes(chunk_size = chunk_size)
  rows <- vector("list", length(code_chunks))
  names(rows) <- names(code_chunks)
  for (chunk_id in names(code_chunks)) {
    rows[[chunk_id]] <- fetch_codes(code_chunks[[chunk_id]], chunk_id)
  }
  out <- rbindlist(rows, fill = TRUE)
  if (nrow(out) > 0L) fwrite(out, tidy_file)
  out
}

make_farm_size_census_panel <- function(force_download = FALSE) {
  write_status("Building crop outcomes by farm-size class from the Agricultural Censuses.")
  panel_file <- file.path(paths$out_dir, "FullRevision_FarmSize_Census_Municipal_Panel.csv")
  if (!force_download && file.exists(panel_file) && file.info(panel_file)$size > 0) {
    return(fread(
      panel_file,
      encoding = "UTF-8",
      colClasses = list(character = c("Code", "crop", "size_group"))
    ))
  }
  specs <- farm_size_sidra_specs()
  long_list <- lapply(specs, function(spec) {
    download_sidra_farm_size_spec(spec, force = force_download)
  })
  long <- rbindlist(long_list, fill = TRUE)
  if (nrow(long) == 0L) stop("No farm-size SIDRA data were downloaded or read.")

  crop_map <- farm_size_crop_map()
  size_map <- farm_size_class_map()
  measure_map <- farm_size_measure_map()
  detail <- merge(
    long,
    crop_map,
    by = c("census_year", "crop_code"),
    all = FALSE,
    allow.cartesian = TRUE
  )
  detail <- merge(
    detail,
    size_map,
    by = c("census_year", "size_code"),
    all = FALSE,
    allow.cartesian = TRUE
  )
  detail <- merge(
    detail,
    measure_map,
    by = c("census_year", "variable_code"),
    all = FALSE
  )
  detail <- unique(
    detail,
    by = c("Code", "census_year", "crop", "crop_code", "size_group", "size_code", "measure")
  )

  expected <- merge(
    crop_map,
    size_map,
    by = "census_year",
    allow.cartesian = TRUE
  )[, .(expected_components = .N), by = .(census_year, crop, size_group)]
  detail <- merge(
    detail,
    expected,
    by = c("census_year", "crop", "size_group"),
    all.x = TRUE
  )
  aggregate_cells <- detail[, {
    complete <- .N == expected_components[1] && all(is.finite(value))
    list(
      aggregate_value = if (complete) sum(value) else NA_real_,
      complete = complete,
      observed_components = .N,
      missing_components = sum(!is.finite(value)),
      suppressed_components = sum(suppressed, na.rm = TRUE),
      expected_components = expected_components[1]
    )
  }, by = .(Code, census_year, crop, size_group, measure)]

  values <- dcast(
    aggregate_cells,
    Code + census_year + crop + size_group ~ measure,
    value.var = "aggregate_value"
  )
  completeness <- dcast(
    aggregate_cells,
    Code + census_year + crop + size_group ~ measure,
    value.var = "complete",
    fill = FALSE
  )
  measure_cols <- intersect(
    c("establishments", "quantity_tonnes", "harvested_area_ha"),
    names(completeness)
  )
  setnames(completeness, measure_cols, paste0("complete_", measure_cols))
  suppressed <- aggregate_cells[, .(
    any_suppressed = any(suppressed_components > 0L),
    missing_components = sum(missing_components),
    suppressed_components = sum(suppressed_components)
  ), by = .(Code, census_year, crop, size_group)]

  pair_map <- read_pair_map()
  pair_codes <- sort(unique(c(pair_map$Code, pair_map$code_neighbor_west)))
  panel <- CJ(
    Code = pair_codes,
    census_year = c(1995L, 2006L, 2017L),
    crop = c("beans", "cassava", "corn", "rice", "soy", "sugarcane"),
    size_group = c("small", "medium", "large"),
    unique = TRUE
  )
  panel <- merge(panel, values, by = c("Code", "census_year", "crop", "size_group"), all.x = TRUE)
  panel <- merge(panel, completeness, by = c("Code", "census_year", "crop", "size_group"), all.x = TRUE)
  panel <- merge(panel, suppressed, by = c("Code", "census_year", "crop", "size_group"), all.x = TRUE)
  for (v in c("establishments", "quantity_tonnes", "harvested_area_ha")) {
    if (!v %in% names(panel)) panel[, (v) := NA_real_]
    complete_v <- paste0("complete_", v)
    if (!complete_v %in% names(panel)) panel[, (complete_v) := FALSE]
    panel[is.na(get(complete_v)), (complete_v) := FALSE]
  }
  panel[is.na(any_suppressed), any_suppressed := FALSE]
  panel[, yield_kg_ha := fifelse(
    is.finite(quantity_tonnes) & quantity_tonnes > 0 &
      is.finite(harvested_area_ha) & harvested_area_ha > 0,
    1000 * quantity_tonnes / harvested_area_ha,
    NA_real_
  )]
  panel[, log_yield := fifelse(is.finite(yield_kg_ha) & yield_kg_ha > 0, log(yield_kg_ha), NA_real_)]

  support <- panel[, .(
    municipality_cells = .N,
    complete_quantity = sum(complete_quantity_tonnes),
    complete_area = sum(complete_harvested_area_ha),
    positive_yield = sum(is.finite(log_yield)),
    cells_with_suppression = sum(any_suppressed)
  ), by = .(census_year, crop, size_group)]
  fwrite(aggregate_cells, file.path(paths$out_dir, "FullRevision_FarmSize_Census_Aggregation_Audit.csv"))
  fwrite(panel, panel_file)
  fwrite(support, file.path(paths$out_dir, "FullRevision_FarmSize_Census_Support.csv"))
  panel
}

make_farm_size_census_sfd <- function(panel = NULL) {
  if (is.null(panel)) panel <- make_farm_size_census_panel()
  panel <- copy(as.data.table(panel))
  pam <- read_pam_zeros()
  env_vars <- c(
    "excess_above_fao_large", "gdd_large", "kdd_large", "sm_season_large",
    "elevation_large", "slope_large", "clay_mean_large"
  )
  missing_env <- setdiff(env_vars, names(pam))
  if (length(missing_env) > 0L) {
    stop("Missing PAM environmental variables: ", paste(missing_env, collapse = ", "))
  }
  env <- unique(
    pam[
      Year %in% c(1995L, 2006L, 2017L) &
        crop %in% c("beans", "cassava", "corn", "rice", "soy", "sugarcane"),
      c("Code", "crop", "Year", env_vars),
      with = FALSE
    ],
    by = c("Code", "crop", "Year")
  )
  setnames(env, "Year", "census_year")
  unit_panel <- merge(
    panel,
    env,
    by = c("Code", "crop", "census_year"),
    all.x = TRUE
  )
  pair_map <- read_pair_map()
  vars <- c("log_yield", env_vars)
  sfd <- make_sfd_from_unit_panel(
    unit_panel[, c("Code", "crop", "census_year", "size_group", vars), with = FALSE],
    pair_map,
    id_cols = c("crop", "census_year", "size_group"),
    vars = vars
  )

  needed <- c(
    "d_log_yield", "d_excess_above_fao_large",
    "d_gdd_large", "d_kdd_large", "d_sm_season_large",
    "d_elevation_large", "d_slope_large", "d_clay_mean_large",
    "pair_id", "lat", "lon", "census_year", "crop", "size_group"
  )
  complete <- complete_data(sfd, needed)
  complete[, available_size_groups := uniqueN(size_group), by = .(pair_id, crop, census_year)]
  common <- complete[available_size_groups == 3L]
  common[, size_group := factor(size_group, levels = c("small", "medium", "large"))]
  common[, year_size_fe := interaction(census_year, size_group, drop = TRUE, lex.order = TRUE)]

  support <- rbindlist(list(
    complete[, .(
      sample = "Available size-class cells",
      observations = .N,
      pair_years = uniqueN(paste(pair_id, census_year, sep = "__")),
      pairs = uniqueN(pair_id)
    ), by = .(crop, size_group)],
    common[, .(
      sample = "Common support across all size classes",
      observations = .N,
      pair_years = uniqueN(paste(pair_id, census_year, sep = "__")),
      pairs = uniqueN(pair_id)
    ), by = .(crop, size_group)]
  ), fill = TRUE)
  fwrite(sfd, file.path(paths$out_dir, "FullRevision_FarmSize_Census_SFD_AllAvailable.csv"))
  fwrite(common, file.path(paths$out_dir, "FullRevision_FarmSize_Census_SFD_CommonSupport.csv"))
  fwrite(support, file.path(paths$out_dir, "FullRevision_FarmSize_Census_SFD_Support.csv"))
  common
}

farm_size_regression_dictionary <- function() {
  c(
    d_excess_above_fao_large = "$\\Delta_s$ Excess salinity (dS/m)",
    d_gdd_large = "$\\Delta_s$ GDD",
    d_kdd_large = "$\\Delta_s$ KDD",
    d_sm_season_large = "$\\Delta_s$ Soil moisture",
    d_elevation_large = "$\\Delta_s$ Elevation",
    d_slope_large = "$\\Delta_s$ Slope",
    d_clay_mean_large = "$\\Delta_s$ Clay content"
  )
}

estimate_farm_size_bartlett_conley <- function(dt, outcome, regressors) {
  d <- copy(as.data.table(dt))
  d[, conley_unit_id := match(pair_id, unique(pair_id))]
  census_years <- sort(unique(d$census_year))
  year_dummies <- character()
  if (length(census_years) > 1L) {
    for (yr in census_years[-1L]) {
      dummy <- paste0("census_year_", yr)
      d[, (dummy) := as.integer(census_year == yr)]
      year_dummies <- c(year_dummies, dummy)
    }
  }

  point_formula <- stats::as.formula(paste(
    outcome, "~", paste(regressors, collapse = " + "), "| census_year"
  ))
  point_model <- fixest::feols(point_formula, data = d, notes = FALSE)
  conley_formula <- stats::as.formula(paste(
    outcome, "~", paste(c(regressors, year_dummies), collapse = " + ")
  ))
  invisible(utils::capture.output(
    conley_vcov <- suppressMessages(conleyreg::conleyreg(
      conley_formula,
      data = d,
      dist_cutoff = 200,
      model = "ols",
      unit = "conley_unit_id",
      time = "census_year",
      lat = "lat",
      lon = "lon",
      kernel = "bartlett",
      lag_cutoff = 0,
      verbose = FALSE,
      ncores = 2,
      vcov = TRUE
    ))
  ))
  slope_names <- names(stats::coef(point_model))
  if (!all(slope_names %in% rownames(conley_vcov))) {
    stop("The Bartlett-Conley covariance matrix does not contain every estimated slope.")
  }
  summary(
    point_model,
    vcov = conley_vcov[slope_names, slope_names, drop = FALSE]
  )
}

make_farm_size_pairwise_models <- function(dt, controls) {
  id_vars <- c(
    "pair_id", "census_year", "lon", "lat",
    "d_excess_above_fao_large", "d_gdd_large", "d_kdd_large",
    "d_sm_season_large", "d_elevation_large", "d_slope_large",
    "d_clay_mean_large"
  )
  wide <- data.table::dcast(
    dt,
    stats::as.formula(paste(paste(id_vars, collapse = " + "), "~ size_group")),
    value.var = "d_log_yield"
  )
  comparisons <- list(
    list(a = "small", b = "medium", label = "5--10 ha minus 20--50 ha"),
    list(a = "small", b = "large", label = "5--10 ha minus 200--500 ha"),
    list(a = "medium", b = "large", label = "20--50 ha minus 200--500 ha")
  )
  models <- list()
  rows <- list()
  for (comparison in comparisons) {
    d <- copy(wide)
    d[, d_log_yield_size_difference := get(comparison$a) - get(comparison$b)]
    model <- estimate_farm_size_bartlett_conley(
      d,
      outcome = "d_log_yield_size_difference",
      regressors = c("d_excess_above_fao_large", controls)
    )
    models[[comparison$label]] <- model
    row <- coef_numeric_row(model, "d_excess_above_fao_large", comparison$label)
    rows[[length(rows) + 1L]] <- data.table(
      Comparison = comparison$label,
      Difference = row$estimate,
      SE = row$se,
      P = row$p,
      observations = row$observations
    )
  }
  list(models = models, tests = rbindlist(rows))
}

write_farm_size_heterogeneity_tables <- function(models) {
  crop_labels <- c(
    beans = "Beans", cassava = "Cassava", corn = "Corn",
    rice = "Rice", soy = "Soybeans", sugarcane = "Sugarcane"
  )
  size_labels <- c(
    small = "5 to under 10 ha",
    medium = "20 to under 50 ha",
    large = "200 to under 500 ha"
  )
  out_files <- character()
  for (cr in names(crop_labels)) {
    if (is.null(models[[cr]])) next
    panel_models <- list()
    size_header <- character()
    spec_header <- character()
    for (group in names(size_labels)) {
      for (spec in 1:3) {
        panel_models[[paste(group, spec, sep = "_")]] <- models[[cr]][[group]][[spec]]
        size_header <- c(size_header, size_labels[[group]])
        spec_header <- c(spec_header, paste0("Spec. ", spec))
      }
    }
    tex_file <- file.path(
      paths$out_dir,
      paste0("FullRevision_FarmSize_Heterogeneity_", tools::toTitleCase(cr), ".tex")
    )
    fixest::etable(
      panel_models,
      tex = TRUE,
      file = tex_file,
      replace = TRUE,
      headers = list("Farm-size class" = size_header, "Specification" = spec_header),
      depvar = FALSE,
      dict = farm_size_regression_dictionary(),
      title = paste0("Farm-Size Heterogeneity in ", crop_labels[[cr]], " Yield Responses"),
      label = paste0("tab:full_revision_farm_size_heterogeneity_", cr),
      fitstat = ~ r2 + n,
      signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
      notes = paste(
        "Notes: The dependent variable is the east-minus-west difference in log crop yield constructed from the 1995, 2006 and 2017 Agricultural Censuses.",
        "Each column is estimated separately for the indicated selected total-farm-area class.",
        "The salinity coefficient is the effect of a 1 dS/m increase in crop-specific excess salinity.",
        "All columns use the crop-specific common-support sample in which all three size classes are observed for the same municipality pair and census year.",
        "Fixed effects are census-year fixed effects. Standard errors are Bartlett-kernel Conley spatial with a 200 km cutoff and no temporal lag."
      )
    )
    style_fixest_tex(
      tex_file,
      size = "\\tiny",
      tabcolsep = "2pt",
      arraystretch = "0.68",
      resize = TRUE,
      landscape = TRUE
    )
    out_files <- c(out_files, tex_file)
  }
  out_files
}

make_farm_size_heterogeneity_analysis <- function(force_download = FALSE) {
  write_status("Estimating crop-yield heterogeneity by farm-size class.")
  panel <- make_farm_size_census_panel(force_download = force_download)
  sfd <- make_farm_size_census_sfd(panel)
  crop_labels <- c(
    beans = "Beans", cassava = "Cassava", corn = "Corn",
    rice = "Rice", soy = "Soybeans", sugarcane = "Sugarcane"
  )
  size_labels <- c(
    small = "5 to under 10 ha",
    medium = "20 to under 50 ha",
    large = "200 to under 500 ha"
  )
  weather_terms <- c(
    "d_gdd_large", "d_kdd_large", "d_sm_season_large"
  )
  topo_terms <- c(
    "d_elevation_large", "d_slope_large", "d_clay_mean_large"
  )
  models <- list()
  coef_rows <- list()
  test_rows <- list()
  for (cr in names(crop_labels)) {
    d <- sfd[crop == cr]
    if (nrow(d) < 90L || uniqueN(d$pair_id) < 30L) {
      write_status("Skipping farm-size heterogeneity for ", cr, ": insufficient common support.")
      next
    }
    models[[cr]] <- list()
    treatment_support <- unique(
      d[, .(pair_id, census_year, d_excess_above_fao_large)]
    )
    treatment_sd <- stats::sd(treatment_support$d_excess_above_fao_large)
    treatment_nonzero <- sum(
      abs(treatment_support$d_excess_above_fao_large) > sqrt(.Machine$double.eps)
    )
    for (group in names(size_labels)) {
      d_group <- d[size_group == group]
      models[[cr]][[group]] <- vector("list", 3L)
      for (spec in 1:3) {
        controls <- switch(
          as.character(spec),
          "1" = character(),
          "2" = weather_terms,
          "3" = c(weather_terms, topo_terms)
        )
        model <- estimate_farm_size_bartlett_conley(
          d_group,
          outcome = "d_log_yield",
          regressors = c("d_excess_above_fao_large", controls)
        )
        models[[cr]][[group]][[spec]] <- model
        row <- coef_numeric_row(
          model, "d_excess_above_fao_large", label = size_labels[[group]]
        )
        coef_rows[[length(coef_rows) + 1L]] <- cbind(
          data.table(
            crop = cr,
            crop_label = crop_labels[[cr]],
            size_group = group,
            size_label = size_labels[[group]],
            specification = paste0("Spec. ", spec),
            spec_id = spec,
            pair_years = uniqueN(paste(d_group$pair_id, d_group$census_year, sep = "__")),
            pairs = uniqueN(d_group$pair_id),
            census_years = paste(sort(unique(d_group$census_year)), collapse = ", "),
            treatment_sd = treatment_sd,
            treatment_nonzero_pair_years = treatment_nonzero,
            treatment_nonzero_share = treatment_nonzero / nrow(treatment_support)
          ),
          row[, .(estimate, se, p, observations)],
          spec_control_flags(spec)
        )
      }
    }
    pairwise <- make_farm_size_pairwise_models(d, weather_terms)
    pairwise$tests[, `:=`(crop = cr, crop_label = crop_labels[[cr]], specification = "Spec. 2")]
    test_rows[[length(test_rows) + 1L]] <- pairwise$tests
  }
  coef_dt <- rbindlist(coef_rows, fill = TRUE)
  if (nrow(coef_dt) == 0L) stop("No crop has enough common support for the farm-size analysis.")
  coef_dt[, `:=`(
    ci95_low = estimate - 1.96 * se,
    ci95_high = estimate + 1.96 * se,
    ci90_low = estimate - 1.645 * se,
    ci90_high = estimate + 1.645 * se,
    standardized_estimate = estimate * treatment_sd,
    standardized_ci95_low = (estimate - 1.96 * se) * treatment_sd,
    standardized_ci95_high = (estimate + 1.96 * se) * treatment_sd,
    standardized_ci90_low = (estimate - 1.645 * se) * treatment_sd,
    standardized_ci90_high = (estimate + 1.645 * se) * treatment_sd,
    effect_percent = 100 * (exp(estimate * treatment_sd) - 1)
  )]
  tests <- rbindlist(test_rows, fill = TRUE)
  tests <- merge(
    tests,
    unique(coef_dt[, .(crop, treatment_sd)]),
    by = "crop",
    all.x = TRUE,
    sort = FALSE
  )
  tests[, `:=`(
    standardized_difference = Difference * treatment_sd,
    standardized_se = SE * treatment_sd
  )]
  fwrite(coef_dt, file.path(paths$out_dir, "FullRevision_FarmSize_Heterogeneity_Coefficients.csv"))
  fwrite(tests, file.path(paths$out_dir, "FullRevision_FarmSize_Heterogeneity_Pairwise_Tests.csv"))

  pairwise_table <- tests[, .(
    Crop = crop_label,
    Comparison,
    `Standardized difference` = fmt(standardized_difference, 3),
    `Standard error` = paste0("(", fmt(standardized_se, 3), ")"),
    `p-value` = fmt(P, 3),
    Observations = formatC(as.integer(observations), format = "d", big.mark = ",")
  )]
  write_latex_df(
    pairwise_table,
    file.path(paths$out_dir, "FullRevision_FarmSize_Heterogeneity_Pairwise_Tests.tex"),
    "Pairwise Tests of Farm-Size Heterogeneity",
    "tab:full_revision_farm_size_heterogeneity_pairwise",
    note = paste(
      "Each row comes from a direct regression of the difference between the two indicated farm-size-class log yields on crop-specific excess salinity and the preferred weather controls.",
      "Coefficients and standard errors are multiplied by the crop-specific standard deviation of excess salinity in the common-support sample.",
      "All models include census-year fixed effects. Standard errors are Bartlett-kernel Conley spatial with a 200 km cutoff and no temporal lag.",
      "A positive coefficient means that the response for the first class is more positive, or less negative, than for the second class."
    ),
    size = "\\scriptsize"
  )

  plot_dt <- copy(coef_dt)
  size_order_top <- unname(size_labels[c("small", "medium", "large")])
  size_order_bottom <- rev(size_order_top)
  offsets <- c("Spec. 1" = -0.20, "Spec. 2" = 0, "Spec. 3" = 0.20)
  plot_dt[, specification := factor(specification, levels = names(offsets))]
  plot_dt[, size_position := match(size_label, size_order_bottom)]
  plot_dt[, plot_position := size_position + offsets[as.character(specification)]]
  plot_dt[, crop_label := factor(
    crop_label,
    levels = c("Corn", "Rice", "Cassava", "Beans", "Soybeans", "Sugarcane")
  )]
  pal <- c("Spec. 1" = "#1B9E77", "Spec. 2" = "#D95F02", "Spec. 3" = "#7570B3")
  shapes <- c("Spec. 1" = 16, "Spec. 2" = 17, "Spec. 3" = 15)
  fig <- ggplot(
    plot_dt,
    aes(x = standardized_estimate, y = plot_position, color = specification, shape = specification)
  ) +
    geom_segment(
      aes(x = standardized_ci95_low, xend = standardized_ci95_high, yend = plot_position),
      linewidth = 0.40, alpha = 0.55, lineend = "butt"
    ) +
    geom_segment(
      aes(x = standardized_ci90_low, xend = standardized_ci90_high, yend = plot_position),
      linewidth = 1.05, lineend = "butt"
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.40, color = "grey20") +
    geom_point(size = 2.25, stroke = 0.55) +
    facet_wrap(~crop_label, ncol = 2, scales = "free_x") +
    scale_y_continuous(
      breaks = seq_along(size_order_bottom),
      labels = size_order_bottom,
      expand = expansion(mult = c(0.11, 0.11))
    ) +
    scale_color_manual(values = pal, name = NULL) +
    scale_shape_manual(values = shapes, name = NULL) +
    labs(x = "Effect of a one-SD increase in excess salinity on log yield", y = NULL) +
    theme_minimal(base_size = 10.5) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.30, color = "grey88"),
      panel.grid.major.x = element_line(linewidth = 0.30, color = "grey88"),
      axis.text.y = element_text(size = 9.2, face = "bold", color = "grey30"),
      axis.text.x = element_text(size = 8.4, color = "grey30"),
      axis.title.x = element_text(size = 10, margin = margin(t = 7)),
      strip.text = element_text(size = 10.5, face = "bold"),
      plot.margin = margin(7, 9, 7, 7)
    )
  ggsave(
    file.path(paths$out_dir, "FullRevision_FarmSize_Heterogeneity_Yield.pdf"),
    fig, width = 8.7, height = 10.2
  )
  ggsave(
    file.path(paths$out_dir, "FullRevision_FarmSize_Heterogeneity_Yield.png"),
    fig, width = 8.7, height = 10.2, dpi = 320
  )

  support_table <- coef_dt[spec_id == 2L, .(
    Crop = crop_label,
    `Farm-size class` = size_label,
    `Pair-year cells` = formatC(pair_years, format = "d", big.mark = ","),
    Pairs = formatC(pairs, format = "d", big.mark = ","),
    `Regression observations` = formatC(observations, format = "d", big.mark = ","),
    `Nonzero treatment cells` = formatC(
      treatment_nonzero_pair_years, format = "d", big.mark = ","
    ),
    `Nonzero treatment share` = paste0(fmt(100 * treatment_nonzero_share, 1), "%"),
    `Treatment SD` = fmt(treatment_sd, 3)
  )]
  write_latex_df(
    support_table,
    file.path(paths$out_dir, "FullRevision_FarmSize_Heterogeneity_Support.tex"),
    "Farm-Size Heterogeneity Estimation Support",
    "tab:full_revision_farm_size_heterogeneity_support",
    note = paste(
      "The table reports the common-support sample used for all three specifications.",
      "A regression observation is an east-minus-west municipality-pair, crop, census-year and farm-size-class cell.",
      "The analysis uses the 1995, 2006 and 2017 Agricultural Censuses, subject to municipal availability and SIDRA confidentiality suppression."
    ),
    size = "\\scriptsize",
    landscape = FALSE
  )
  table_files <- write_farm_size_heterogeneity_tables(models)
  write_status("Farm-size crop-yield heterogeneity analysis completed.")
  invisible(list(
    panel = panel,
    sfd = sfd,
    models = models,
    coefficients = coef_dt,
    pairwise_tests = tests,
    figure = fig,
    tables = table_files
  ))
}

# =============================================================================
# 10. Yield trends, contextual statistics, and adaptation diagnostics
# =============================================================================
make_pam_panel_yield_trends <- function(pam = NULL) {
  write_status("Building crop-yield trends from totals in the geocoded PAM panel.")
  if (is.null(pam)) pam <- read_pam_zeros()
  d <- copy(pam[
    crop %chin% c("beans", "cassava", "corn", "rice", "soy", "sugarcane") &
      Year >= 1985L & Year <= 2018L
  ])
  if (!"harvested_area" %in% names(d) && "recolted_area" %in% names(d)) {
    d[, harvested_area := recolted_area]
  }
  needed <- c("Code", "crop", "Year", "quantity", "harvested_area", "yield")
  if (!all(needed %in% names(d))) {
    stop("PAM-panel yield trends are missing: ", paste(setdiff(needed, names(d)), collapse = ", "))
  }

  d <- d[
    is.finite(quantity) & quantity >= 0 &
      is.finite(harvested_area) & harvested_area > 0
  ]
  trends <- d[, .(
    production_tonnes = sum(quantity),
    harvested_area_ha = sum(harvested_area),
    aggregate_yield_kg_ha = 1000 * sum(quantity) / sum(harvested_area),
    reported_area_weighted_yield_kg_ha = weighted.mean(yield, harvested_area, na.rm = TRUE),
    municipalities = uniqueN(Code),
    municipality_crop_cells = .N
  ), by = .(crop, Year)]
  trends[, yield_identity_gap_kg_ha :=
    aggregate_yield_kg_ha - reported_area_weighted_yield_kg_ha]
  if (any(!is.finite(trends$aggregate_yield_kg_ha))) {
    stop("At least one PAM-panel aggregate yield is not finite.")
  }
  if (max(abs(trends$yield_identity_gap_kg_ha), na.rm = TRUE) > 10) {
    stop("PAM quantity/area and reported-yield aggregates differ by more than 10 kg/ha.")
  }

  crop_labels <- c(
    beans = "Beans", cassava = "Cassava", corn = "Corn",
    rice = "Rice", soy = "Soybeans", sugarcane = "Sugarcane"
  )
  trends[, crop_label := factor(crop_labels[crop], levels = unname(crop_labels))]
  setorder(trends, crop_label, Year)
  fwrite(trends, file.path(paths$out_dir, "FullRevision_PAM_Panel_Yield_Trends.csv"))

  trend_summary <- trends[, {
    first_row <- .SD[which.min(Year)]
    last_row <- .SD[which.max(Year)]
    min_row <- .SD[which.min(aggregate_yield_kg_ha)]
    max_row <- .SD[which.max(aggregate_yield_kg_ha)]
    .(
      first_year = first_row$Year,
      first_yield_kg_ha = first_row$aggregate_yield_kg_ha,
      last_year = last_row$Year,
      last_yield_kg_ha = last_row$aggregate_yield_kg_ha,
      change_percent = 100 * (last_row$aggregate_yield_kg_ha / first_row$aggregate_yield_kg_ha - 1),
      minimum_year = min_row$Year,
      minimum_yield_kg_ha = min_row$aggregate_yield_kg_ha,
      maximum_year = max_row$Year,
      maximum_yield_kg_ha = max_row$aggregate_yield_kg_ha
    )
  }, by = .(crop, crop_label)]
  fwrite(trend_summary, file.path(paths$out_dir, "FullRevision_PAM_Panel_Yield_Trend_Summary.csv"))

  pal <- c(
    "Beans" = "#1B9E77", "Cassava" = "#D95F02", "Corn" = "#7570B3",
    "Rice" = "#E6AB02", "Soybeans" = "#1F78B4", "Sugarcane" = "#666666"
  )
  fig <- ggplot(trends, aes(Year, aggregate_yield_kg_ha, color = crop_label)) +
    geom_line(linewidth = 0.85) +
    facet_wrap(~crop_label, ncol = 2L, scales = "free_y") +
    scale_color_manual(values = pal, name = NULL) +
    scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2018)) +
    scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
    labs(x = "Year", y = "Aggregate yield (kg/ha)") +
    theme_minimal(base_size = 11.5) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.30, color = "grey88"),
      axis.text = element_text(size = 9.5, color = "black"),
      axis.title = element_text(size = 11),
      strip.text = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      plot.margin = margin(6, 8, 6, 8)
    )
  ggsave(
    file.path(paths$out_dir, "FullRevision_PAM_Panel_Yield_Trends.pdf"),
    fig, width = 8.5, height = 9.2
  )
  ggsave(
    file.path(paths$out_dir, "FullRevision_PAM_Panel_Yield_Trends.png"),
    fig, width = 8.5, height = 9.2, dpi = 320
  )
  invisible(list(trends = trends, summary = trend_summary, figure = fig))
}

make_crop_context_statistics <- function(pam = NULL) {
  write_status("Building 2018 crop context statistics from PAM.")
  if (is.null(pam)) pam <- read_pam_zeros()
  d <- copy(pam[
    crop %chin% c("beans", "cassava", "corn", "rice", "soy", "sugarcane") & Year == 2018L
  ])
  if (!"name_region" %in% names(d)) {
    region_map <- c(
      AC = "North", AP = "North", AM = "North", PA = "North", RO = "North", RR = "North", TO = "North",
      AL = "Northeast", BA = "Northeast", CE = "Northeast", MA = "Northeast", PB = "Northeast",
      PE = "Northeast", PI = "Northeast", RN = "Northeast", SE = "Northeast",
      ES = "Southeast", MG = "Southeast", RJ = "Southeast", SP = "Southeast",
      PR = "South", RS = "South", SC = "South", DF = "Center-West", GO = "Center-West",
      MS = "Center-West", MT = "Center-West"
    )
    state_var <- intersect(c("abbrev_state", "state"), names(d))[1]
    if (is.na(state_var)) stop("PAM has no state variable for crop context statistics.")
    d[, region_en := unname(region_map[as.character(get(state_var))])]
  } else {
    region_map <- c(
      "Norte" = "North", "Nordeste" = "Northeast", "Sudeste" = "Southeast",
      "Sul" = "South", "Centro Oeste" = "Center-West"
    )
    d[, region_en := unname(region_map[as.character(name_region)])]
  }
  d <- d[
    is.finite(planted_area) & planted_area >= 0 &
      is.finite(quantity) & quantity >= 0 & !is.na(region_en)
  ]
  regional <- d[, .(
    planted_area_ha = sum(planted_area),
    production_tonnes = sum(quantity)
  ), by = .(crop, region_en)]
  totals <- regional[, .(
    planted_area_ha = sum(planted_area_ha),
    production_tonnes = sum(production_tonnes),
    leading_region = region_en[which.max(planted_area_ha)],
    leading_region_area_share = 100 * max(planted_area_ha) / sum(planted_area_ha),
    northeast_area_share = 100 * sum(planted_area_ha[region_en == "Northeast"]) / sum(planted_area_ha)
  ), by = crop]
  totals[, geocoded_panel_selected_crop_area_share := 100 * planted_area_ha / sum(planted_area_ha)]
  crop_labels <- c(
    beans = "Beans", cassava = "Cassava", corn = "Corn",
    rice = "Rice", soy = "Soybeans", sugarcane = "Sugarcane"
  )
  totals[, crop_label := unname(crop_labels[crop])]
  scope_file <- file.path(paths$data_dir, "ibge_selected_crops_area_value_scope.csv")
  if (file.exists(scope_file)) {
    scope <- fread(scope_file)[year == 2018L]
    national <- rbindlist(lapply(unique(totals$crop), function(cr) {
      data.table(
        crop = cr,
        national_planted_area_ha = as.numeric(scope[[cr]][1]),
        national_crop_area_share = as.numeric(scope[[paste0(cr, "_area_share_pct")]][1]),
        national_production_value_share = as.numeric(scope[[paste0(cr, "_value_share_pct")]][1])
      )
    }))
    totals <- merge(totals, national, by = "crop", all.x = TRUE, sort = FALSE)
  }
  preferred_order <- c(
    "crop", "crop_label", "national_planted_area_ha", "national_crop_area_share",
    "national_production_value_share", "planted_area_ha", "production_tonnes",
    "geocoded_panel_selected_crop_area_share", "leading_region",
    "leading_region_area_share", "northeast_area_share"
  )
  setcolorder(totals, intersect(preferred_order, names(totals)))
  fwrite(totals, file.path(paths$out_dir, "FullRevision_Crop_Context_2018.csv"))
  fwrite(regional, file.path(paths$out_dir, "FullRevision_Crop_Context_2018_ByRegion.csv"))
  invisible(list(total = totals, regional = regional))
}

make_regional_yield_heterogeneity <- function() {
  write_status("Estimating regional heterogeneity in the main crop-yield specification.")
  sfd <- as.data.table(readRDS(paths$sfd_rds))
  required <- c(
    "crop", "name_region", "d_log_yield", "d_excess_above_fao_large",
    "d_gdd_large", "d_kdd_large", "d_sm_season_large",
    "d_elevation_large", "d_slope_large", "d_clay_mean_large",
    "Year", "pair_id", "lat", "lon"
  )
  if (!all(required %in% names(sfd))) {
    stop("Regional heterogeneity is missing: ", paste(setdiff(required, names(sfd)), collapse = ", "))
  }
  region_map <- c(
    "Norte" = "North", "Nordeste" = "Northeast", "Sudeste" = "Southeast",
    "Sul" = "South", "Centro Oeste" = "Center-West"
  )
  sfd[, region_en := unname(region_map[as.character(name_region)])]
  main_sample <- complete_data(sfd, required)
  main_sample <- main_sample[is.finite(yield) & yield > 0 & !is.na(region_en)]
  region_order <- c("North", "Northeast", "Southeast", "South", "Center-West")
  region_slug <- c(
    North = "north", Northeast = "northeast", Southeast = "southeast",
    South = "south", `Center-West` = "center_west"
  )
  crop_labels <- c(
    beans = "Beans", cassava = "Cassava", corn = "Corn",
    rice = "Rice", soy = "Soybeans", sugarcane = "Sugarcane"
  )
  coefficient_rows <- list()
  difference_rows <- list()
  audit_rows <- list()
  missing_rows <- list()
  model_store <- list()

  for (cr in names(crop_labels)) {
    raw_crop <- sfd[crop == cr]
    for (v in required) {
      missing_rows[[length(missing_rows) + 1L]] <- data.table(
        crop = cr,
        variable = v,
        observations = nrow(raw_crop),
        missing_or_nonfinite = if (is.numeric(raw_crop[[v]]) || is.integer(raw_crop[[v]])) {
          sum(!is.finite(raw_crop[[v]]))
        } else {
          sum(is.na(raw_crop[[v]]) | !nzchar(trimws(as.character(raw_crop[[v]]))))
        }
      )
    }
    d <- copy(main_sample[crop == cr])
    if (nrow(d) == 0L) next
    for (reg in region_order) {
      d[, paste0("treat_", region_slug[[reg]]) :=
        d_excess_above_fao_large * as.numeric(region_en == reg)]
    }
    regional_terms <- paste0("treat_", unname(region_slug[region_order]))
    weather <- c("d_gdd_large", "d_kdd_large", "d_sm_season_large")
    regional_formula <- as.formula(paste(
      "d_log_yield ~", paste(c(regional_terms, weather), collapse = " + "),
      "| Year + region_en"
    ))
    regional_model <- feols(
      regional_formula,
      data = d,
      vcov = make_conley(),
      panel.id = ~pair_id + Year,
      notes = FALSE
    )
    regional_vcov <- vcov(regional_model, vcov = make_conley())
    regional_coef <- coef(regional_model)

    for (reg in region_order) {
      term <- paste0("treat_", region_slug[[reg]])
      estimate <- unname(regional_coef[[term]])
      se <- sqrt(regional_vcov[term, term])
      z <- estimate / se
      p <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)
      support <- d[region_en == reg, .(
        observations = .N,
        pairs = uniqueN(pair_id),
        treatment_sd = stats::sd(d_excess_above_fao_large),
        nonzero_treatment_share = mean(abs(d_excess_above_fao_large) > 1e-12)
      )]
      coefficient_rows[[length(coefficient_rows) + 1L]] <- data.table(
        crop = cr,
        crop_label = crop_labels[[cr]],
        region = reg,
        estimate = estimate,
        se = se,
        p = p,
        ci90_low = estimate - stats::qnorm(0.95) * se,
        ci90_high = estimate + stats::qnorm(0.95) * se,
        ci95_low = estimate - stats::qnorm(0.975) * se,
        ci95_high = estimate + stats::qnorm(0.975) * se,
        observations = support$observations,
        pairs = support$pairs,
        treatment_sd = support$treatment_sd,
        nonzero_treatment_share = support$nonzero_treatment_share
      )
    }

    d[, `:=`(
      northeast = as.integer(region_en == "Northeast"),
      treat_northeast_difference = d_excess_above_fao_large * as.integer(region_en == "Northeast"),
      northeast_group = fifelse(region_en == "Northeast", "Northeast", "Rest of Brazil")
    )]
    binary_model <- feols(
      d_log_yield ~ d_excess_above_fao_large + treat_northeast_difference +
        d_gdd_large + d_kdd_large + d_sm_season_large | Year + northeast_group,
      data = d,
      vcov = make_conley(),
      panel.id = ~pair_id + Year,
      notes = FALSE
    )
    binary_vcov <- vcov(binary_model, vcov = make_conley())
    binary_coef <- coef(binary_model)
    base_term <- "d_excess_above_fao_large"
    diff_term <- "treat_northeast_difference"
    rest_est <- unname(binary_coef[[base_term]])
    diff_est <- unname(binary_coef[[diff_term]])
    rest_se <- sqrt(binary_vcov[base_term, base_term])
    diff_se <- sqrt(binary_vcov[diff_term, diff_term])
    northeast_est <- rest_est + diff_est
    northeast_var <- binary_vcov[base_term, base_term] + binary_vcov[diff_term, diff_term] +
      2 * binary_vcov[base_term, diff_term]
    northeast_se <- sqrt(max(northeast_var, 0))
    difference_rows[[length(difference_rows) + 1L]] <- data.table(
      crop = cr,
      crop_label = crop_labels[[cr]],
      rest_estimate = rest_est,
      rest_se = rest_se,
      rest_p = 2 * stats::pnorm(abs(rest_est / rest_se), lower.tail = FALSE),
      northeast_estimate = northeast_est,
      northeast_se = northeast_se,
      northeast_p = 2 * stats::pnorm(abs(northeast_est / northeast_se), lower.tail = FALSE),
      northeast_minus_rest = diff_est,
      difference_se = diff_se,
      difference_p = 2 * stats::pnorm(abs(diff_est / diff_se), lower.tail = FALSE),
      observations = nrow(d),
      northeast_observations = d[region_en == "Northeast", .N],
      rest_observations = d[region_en != "Northeast", .N]
    )

    main_model <- feols(
      d_log_yield ~ d_excess_above_fao_large + d_gdd_large + d_kdd_large + d_sm_season_large | Year,
      data = d,
      vcov = make_conley(),
      panel.id = ~pair_id + Year,
      notes = FALSE
    )
    audit_rows[[length(audit_rows) + 1L]] <- data.table(
      crop = cr,
      complete_case_sample = nrow(d),
      preferred_main_model = nobs(main_model),
      regional_model = nobs(regional_model),
      samples_identical = nrow(d) == nobs(main_model) && nrow(d) == nobs(regional_model)
    )
    model_store[[cr]] <- list(regional = regional_model, binary = binary_model, main = main_model)
  }

  coefficients <- rbindlist(coefficient_rows)
  differences <- rbindlist(difference_rows)
  audit <- rbindlist(audit_rows)
  missingness <- rbindlist(missing_rows)
  if (!all(audit$samples_identical)) stop("Main and regional yield samples are not identical.")
  fwrite(coefficients, file.path(paths$out_dir, "FullRevision_Regional_Yield_Heterogeneity.csv"))
  fwrite(differences, file.path(paths$out_dir, "FullRevision_Regional_Yield_Northeast_Tests.csv"))
  fwrite(audit, file.path(paths$out_dir, "FullRevision_Main_Yield_Sample_Audit.csv"))
  fwrite(missingness, file.path(paths$out_dir, "FullRevision_Main_Yield_Missingness_Audit.csv"))

  effect_cell <- function(x, se, p) paste0(fmt(x, 3), stars(p), " (", fmt(se, 3), ")")
  regional_cell <- function(dt, reg) {
    row <- dt[region == reg]
    if (nrow(row) != 1L) return("")
    effect_cell(row$estimate, row$se, row$p)
  }
  table_dt <- rbindlist(lapply(names(crop_labels), function(cr) {
    rows <- coefficients[crop == cr]
    data.table(
      Crop = crop_labels[[cr]],
      North = regional_cell(rows, "North"),
      Northeast = regional_cell(rows, "Northeast"),
      Southeast = regional_cell(rows, "Southeast"),
      South = regional_cell(rows, "South"),
      `Center-West` = regional_cell(rows, "Center-West")
    )
  }))
  table_dt <- merge(
    table_dt,
    differences[, .(
      Crop = crop_label,
      `Northeast - rest` = effect_cell(northeast_minus_rest, difference_se, difference_p),
      `Difference p-value` = fmt(difference_p, 3),
      Observations = formatC(observations, format = "d", big.mark = ","),
      `Northeast observations` = formatC(northeast_observations, format = "d", big.mark = ",")
    )],
    by = "Crop",
    sort = FALSE
  )
  write_latex_df(
    table_dt,
    file.path(paths$out_dir, "FullRevision_Regional_Yield_Heterogeneity.tex"),
    "Regional Heterogeneity in Crop-Yield Responses",
    "tab:full_revision_regional_yield_heterogeneity",
    note = paste(
      "Each regional cell reports the effect of a 1 dS/m increase in crop-specific excess salinity on the east-minus-west difference in log yield, with its Conley standard error in parentheses.",
      "All five regional slopes are estimated jointly within each crop on the exact complete-data sample used for the three main yield specifications.",
      "Models include year and macroregion fixed effects plus GDD, KDD and soil moisture. The Northeast-minus-rest column comes from a separate binary interaction on the same sample.",
      "Standard errors are Conley spatial with a 200 km cutoff. As in the main tables, fixest applies its positive-semidefinite correction when required. Stars denote significance at the 10, 5 and 1 percent levels."
    ),
    size = "\\footnotesize",
    landscape = TRUE
  )

  support_dt <- coefficients[, .(
    Crop = crop_label,
    Region = as.character(region),
    Observations = formatC(observations, format = "d", big.mark = ","),
    Pairs = formatC(pairs, format = "d", big.mark = ","),
    `Treatment SD (dS/m)` = fmt(treatment_sd, 3),
    `Nonzero treatment share` = paste0(fmt(100 * nonzero_treatment_share, 1), "%")
  )]
  write_latex_df(
    support_dt,
    file.path(paths$out_dir, "FullRevision_Regional_Yield_Heterogeneity_Support.tex"),
    "Regional Support for Crop-Yield Heterogeneity Estimates",
    "tab:full_revision_regional_yield_heterogeneity_support",
    note = paste(
      "The sample is the crop-specific complete-data sample used in the main yield regressions.",
      "An observation is an east-minus-west municipality-pair-year cell. The treatment is the spatial difference in crop-specific excess salinity.",
      "The table documents the regional sample composition and is not an additional regression."
    ),
    size = "\\footnotesize",
    landscape = TRUE
  )

  coefficients[, region := factor(region, levels = rev(region_order))]
  coefficients[, crop_label := factor(crop_label, levels = unname(crop_labels))]
  fig <- ggplot(coefficients, aes(estimate, region, color = region == "Northeast")) +
    geom_vline(xintercept = 0, linewidth = 0.40, color = "grey45") +
    geom_segment(aes(x = ci95_low, xend = ci95_high, yend = region), linewidth = 0.55) +
    geom_segment(aes(x = ci90_low, xend = ci90_high, yend = region), linewidth = 1.05) +
    geom_point(size = 2.1) +
    facet_wrap(~crop_label, ncol = 2L, scales = "free_x") +
    scale_color_manual(values = c(`FALSE` = "#4D6C7A", `TRUE` = "#B33A3A"), guide = "none") +
    labs(x = "Effect of 1 dS/m of excess salinity on log yield", y = NULL) +
    theme_minimal(base_size = 10.5) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.30, color = "grey88"),
      axis.text = element_text(size = 8.8, color = "black"),
      axis.title.x = element_text(size = 10),
      strip.text = element_text(size = 10.5, face = "bold"),
      plot.margin = margin(7, 9, 7, 7)
    )
  ggsave(
    file.path(paths$out_dir, "FullRevision_Regional_Yield_Heterogeneity.pdf"),
    fig, width = 8.7, height = 9.4
  )
  ggsave(
    file.path(paths$out_dir, "FullRevision_Regional_Yield_Heterogeneity.png"),
    fig, width = 8.7, height = 9.4, dpi = 320
  )
  invisible(list(
    coefficients = coefficients,
    northeast_tests = differences,
    audit = audit,
    missingness = missingness,
    models = model_store,
    figure = fig
  ))
}

make_historical_salinity_adaptation_analysis <- function(pam = NULL, reuse_stage_one = FALSE) {
  write_status("Estimating the Butler--Huybers two-stage historical-adaptation diagnostic.")
  crops <- c("corn", "rice", "cassava", "beans", "soy", "sugarcane")
  crop_labels <- c(
    corn = "Corn", rice = "Rice", cassava = "Cassava",
    beans = "Beans", soy = "Soybeans", sugarcane = "Sugarcane"
  )
  min_years <- 15L
  stage_one_bootstrap_reps <- 199L
  block_length <- 3L
  stage_two_bootstrap_reps <- 999L

  if (isTRUE(reuse_stage_one)) {
    sensitivity_file <- file.path(
      paths$out_dir, "FullRevision_Butler_Municipal_Sensitivities.csv"
    )
    if (!file.exists(sensitivity_file)) {
      stop("Cached municipal Butler--Huybers sensitivities are missing.")
    }
    sensitivities <- fread(sensitivity_file)
    if (!"stage_one_exposure" %in% names(sensitivities) ||
        any(sensitivities$stage_one_exposure != "excess_salinity_anomaly")) {
      stop("Cached Butler--Huybers sensitivities use an obsolete first-stage exposure; rerun --historical-adaptation-only.")
    }
  } else {
    if (is.null(pam)) pam <- read_pam_zeros()
    pam <- copy(pam)
    pam[, `:=`(
      Code = trimws(as.character(Code)),
      crop = trimws(tolower(as.character(crop)))
    )]
    pam[crop %in% c("soybean", "soybeans"), crop := "soy"]
    pam[crop == "maize", crop := "corn"]
    pam[crop %in% c(
      "sugar cane", "sugar-cane", "cana-de-acucar", "cana de acucar"
    ), crop := "sugarcane"]

    historical <- pam[
      Year >= 1985L & Year <= 1994L & is.finite(mean_salinity),
      .(annual_mean_salinity = finite_first(mean_salinity)),
      by = .(Code, Year)
    ][, .(
      historical_mean_salinity = mean(annual_mean_salinity),
      historical_years = .N
    ), by = Code]
    historical <- historical[
      historical_years >= 5L &
        is.finite(historical_mean_salinity) & historical_mean_salinity > 0
    ]

    model_vars <- c(
      "log_yield", "excess_above_fao_large", "gdd_large", "kdd_large", "sm_season_large"
    )
    stage_one_data <- complete_data(
      pam[
        crop %in% crops & Year >= 1985L & Year <= 2018L &
          is.finite(yield) & yield > 0
      ],
      c("Code", "crop", "Year", "abbrev_state", model_vars)
    )[, c("Code", "crop", "Year", "abbrev_state", model_vars), with = FALSE]
    stage_one_data <- merge(
      stage_one_data, historical, by = "Code", all = FALSE, sort = FALSE
    )

    deterministic_seed <- function(code, crop, offset = 0L) {
      key <- utf8ToInt(paste0(code, "_", crop, "_", offset))
      as.integer(sum(as.double(key) * seq_along(key)) %% 2147483000)
    }
    standardize_within <- function(x) {
      s <- stats::sd(x)
      if (!is.finite(s) || s <= 0) return(rep(NA_real_, length(x)))
      (x - mean(x)) / s
    }
    fit_municipality_sensitivity <- function(d, code, crop) {
      setorder(d, Year)
      if (anyDuplicated(d$Year)) d <- d[!duplicated(Year)]
      n <- nrow(d)
      if (
        uniqueN(d$Year) < min_years ||
          stats::sd(d$excess_above_fao_large) <= 1e-5 ||
          stats::sd(d$log_yield) <= 1e-8
      ) return(NULL)

      trend <- d$Year - mean(d$Year)
      salinity_anomaly <- d$excess_above_fao_large - mean(d$excess_above_fao_large)
      x <- cbind(
        constant = 1,
        trend = trend,
        salinity_anomaly = salinity_anomaly,
        gdd_anomaly = standardize_within(d$gdd_large),
        kdd_anomaly = standardize_within(d$kdd_large),
        moisture_anomaly = standardize_within(d$sm_season_large)
      )
      if (any(!is.finite(x))) return(NULL)

      unrestricted <- stats::lm.fit(x, d$log_yield)
      if (
        unrestricted$rank < ncol(x) ||
          unrestricted$df.residual < 5L
      ) return(NULL)
      rss_unrestricted <- sum(unrestricted$residuals^2)
      if (!is.finite(rss_unrestricted) || rss_unrestricted <= 1e-12) return(NULL)

      restricted <- stats::lm.fit(cbind(constant = 1, trend = trend), d$log_yield)
      rss_restricted <- sum(restricted$residuals^2)
      numerator_df <- ncol(x) - 2L
      f_statistic <- max(
        0,
        ((rss_restricted - rss_unrestricted) / numerator_df) /
          (rss_unrestricted / unrestricted$df.residual)
      )
      model_p <- stats::pf(
        f_statistic, numerator_df, unrestricted$df.residual, lower.tail = FALSE
      )

      xtx_inverse <- tryCatch(
        solve(crossprod(x)),
        error = function(e) NULL
      )
      if (is.null(xtx_inverse)) return(NULL)
      projection <- xtx_inverse %*% t(x)
      residuals <- unrestricted$residuals - mean(unrestricted$residuals)
      blocks_needed <- ceiling(n / block_length)
      set.seed(deterministic_seed(code, crop))
      starts <- matrix(
        sample.int(
          n,
          blocks_needed * stage_one_bootstrap_reps,
          replace = TRUE
        ),
        nrow = blocks_needed,
        ncol = stage_one_bootstrap_reps
      )
      bootstrap_indices <- vapply(
        seq_len(stage_one_bootstrap_reps),
        function(b) {
          unlist(
            lapply(
              starts[, b],
              function(start) {
                ((start - 1L + 0:(block_length - 1L)) %% n) + 1L
              }
            ),
            use.names = FALSE
          )[seq_len(n)]
        },
        integer(n)
      )
      bootstrap_residuals <- matrix(
        residuals[bootstrap_indices],
        nrow = n,
        ncol = stage_one_bootstrap_reps
      )
      bootstrap_slopes <- as.numeric(
        unrestricted$coefficients["salinity_anomaly"] +
          projection["salinity_anomaly", , drop = FALSE] %*% bootstrap_residuals
      )
      bootstrap_variance <- stats::var(bootstrap_slopes)
      if (!is.finite(bootstrap_variance) || bootstrap_variance <= 0) return(NULL)

      data.table(
        sensitivity = unrestricted$coefficients["salinity_anomaly"],
        sensitivity_bootstrap_se = sqrt(bootstrap_variance),
        sensitivity_bootstrap_variance = bootstrap_variance,
        observations = n,
        model_p_value = model_p,
        salinity_within_sd = stats::sd(d$excess_above_fao_large),
        yield_within_sd = stats::sd(d$log_yield),
        salinity_trend_correlation = suppressWarnings(
          stats::cor(d$excess_above_fao_large, d$Year)
        ),
        state = finite_first(d$abbrev_state)
      )
    }

    sensitivities <- stage_one_data[, {
      fit_municipality_sensitivity(.SD, .BY$Code, .BY$crop)
    }, by = .(crop, Code)]
    sensitivities <- merge(
      sensitivities,
      historical,
      by = "Code",
      all = FALSE,
      sort = FALSE
    )
    sensitivities <- sensitivities[
      is.finite(sensitivity) &
        is.finite(sensitivity_bootstrap_variance) &
        sensitivity_bootstrap_variance > 0 &
        is.finite(historical_mean_salinity) & historical_mean_salinity > 0
    ]
    sensitivities[, crop_label := crop_labels[crop]]
    sensitivities[, stage_one_exposure := "excess_salinity_anomaly"]
    fwrite(
      sensitivities,
      file.path(paths$out_dir, "FullRevision_Butler_Municipal_Sensitivities.csv")
    )
  }

  weighted_fit <- function(x, y, weights) {
    keep <- is.finite(x) & is.finite(y) & is.finite(weights) & weights > 0
    x <- x[keep]
    y <- y[keep]
    weights <- weights[keep]
    x_bar <- sum(weights * x) / sum(weights)
    y_bar <- sum(weights * y) / sum(weights)
    denominator <- sum(weights * (x - x_bar)^2)
    if (!is.finite(denominator) || denominator <= 0) {
      return(c(intercept = NA_real_, slope = NA_real_, r2 = NA_real_))
    }
    slope <- sum(weights * (x - x_bar) * (y - y_bar)) / denominator
    intercept <- y_bar - slope * x_bar
    residuals <- y - intercept - slope * x
    total <- sum(weights * (y - y_bar)^2)
    r2 <- if (is.finite(total) && total > 0) {
      1 - sum(weights * residuals^2) / total
    } else {
      NA_real_
    }
    c(intercept = intercept, slope = slope, r2 = r2)
  }
  transform_exposure <- function(x, form) {
    switch(
      form,
      Logarithmic = log(x),
      Linear = x,
      Inverse = 1 / x,
      stop("Unknown adaptation functional form: ", form)
    )
  }
  fit_adaptation_function <- function(d, form, crop_index) {
    x <- transform_exposure(d$historical_mean_salinity, form)
    y <- d$sensitivity
    weights <- 1 / d$sensitivity_bootstrap_variance
    fit <- weighted_fit(x, y, weights)
    n <- nrow(d)

    set.seed(91000L + crop_index * 100L + match(form, c("Logarithmic", "Linear", "Inverse")))
    bootstrap_slopes <- replicate(stage_two_bootstrap_reps, {
      sampled <- sample.int(n, n, replace = TRUE)
      weighted_fit(x[sampled], y[sampled], weights[sampled])["slope"]
    })
    bootstrap_slopes <- bootstrap_slopes[is.finite(bootstrap_slopes)]
    bootstrap_se <- stats::sd(bootstrap_slopes)
    ci <- stats::quantile(
      bootstrap_slopes, c(0.025, 0.975), na.rm = TRUE, names = FALSE
    )
    p_value <- if (length(bootstrap_slopes)) {
      min(
        1,
        2 * min(
          (sum(bootstrap_slopes <= 0) + 1) / (length(bootstrap_slopes) + 1),
          (sum(bootstrap_slopes >= 0) + 1) / (length(bootstrap_slopes) + 1)
        )
      )
    } else {
      NA_real_
    }

    weight_share <- weights / sum(weights)
    highest_weight <- which.max(weights)
    leave_one_out <- weighted_fit(
      x[-highest_weight], y[-highest_weight], weights[-highest_weight]
    )
    capped_weights <- pmin(weights, stats::quantile(weights, 0.99))
    capped_fit <- weighted_fit(x, y, capped_weights)
    top_one_percent <- order(weights, decreasing = TRUE)[
      seq_len(max(1L, ceiling(0.01 * n)))
    ]

    data.table(
      functional_form = form,
      intercept = unname(fit["intercept"]),
      adaptation_factor = unname(fit["slope"]),
      bootstrap_se = bootstrap_se,
      ci95_low = ci[1],
      ci95_high = ci[2],
      bootstrap_p_value = p_value,
      weighted_r2 = unname(fit["r2"]),
      municipalities = n,
      max_weight_share = max(weight_share),
      top_one_percent_weight_share = sum(weight_share[top_one_percent]),
      leave_most_precise_out_factor = unname(leave_one_out["slope"]),
      capped_weight_factor = unname(capped_fit["slope"])
    )
  }

  function_results <- rbindlist(lapply(seq_along(crops), function(i) {
    cr <- crops[[i]]
    d <- sensitivities[crop == cr]
    rbindlist(lapply(c("Logarithmic", "Linear", "Inverse"), function(form) {
      cbind(
        data.table(crop = cr, crop_label = crop_labels[[cr]]),
        fit_adaptation_function(d, form, i)
      )
    }))
  }))
  main_results <- function_results[functional_form == "Logarithmic"]
  support <- sensitivities[, .(
    municipalities = .N,
    median_years = as.numeric(stats::median(observations)),
    median_salinity_within_sd = stats::median(salinity_within_sd),
    median_sensitivity_bootstrap_se = stats::median(sensitivity_bootstrap_se),
    significant_first_stage_share = mean(model_p_value < 0.05),
    median_absolute_salinity_trend_correlation = stats::median(
      abs(salinity_trend_correlation), na.rm = TRUE
    )
  ), by = .(crop, crop_label)]
  fwrite(
    main_results,
    file.path(paths$out_dir, "FullRevision_HistoricalSalinity_Adaptation_Coefficients.csv")
  )
  fwrite(
    function_results,
    file.path(paths$out_dir, "FullRevision_Butler_Functional_Forms.csv")
  )
  fwrite(
    support,
    file.path(paths$out_dir, "FullRevision_HistoricalSalinity_Adaptation_Support.csv")
  )

  table_rows <- main_results[, .(
    Crop = crop_label,
    Municipalities = formatC(municipalities, format = "d", big.mark = ","),
    `Factor (bootstrap SE)` = paste0(
      fmt(adaptation_factor, 3), stars(bootstrap_p_value),
      " (", fmt(bootstrap_se, 3), ")"
    ),
    `95% CI` = paste0("[", fmt(ci95_low, 3), ", ", fmt(ci95_high, 3), "]"),
    `p-value` = fmt(bootstrap_p_value, 3),
    `Weighted R2` = fmt(weighted_r2, 3)
  )]
  write_latex_df(
    table_rows,
    file.path(paths$out_dir, "FullRevision_HistoricalSalinity_Adaptation.tex"),
    "Butler--Huybers Historical-Adaptation Diagnostic",
    "tab:full_revision_historical_salinity_adaptation",
    note = paste(
      "Stage 1 estimates a separate municipal crop-yield sensitivity to annual crop-specific excess-salinity anomalies over 1985--2018, controlling for a municipality-specific linear trend and within-municipality GDD, KDD and soil-moisture anomalies.",
      "Municipality--crop series require at least 15 positive-yield years and at least five salinity observations in 1985--1994.",
      "Stage-1 uncertainty uses 199 circular moving-block residual bootstrap replications with three-year blocks.",
      "Following Butler and Huybers (2013), Stage 2 regresses the estimated sensitivities on log historical mean salinity, weighting by the inverse stage-1 bootstrap variance.",
      "A positive adaptation factor means that sensitivity becomes less negative as historical salinity rises and is therefore compatible with adaptation; it is not a causal estimate of adaptation.",
      "Stage-2 confidence intervals and p-values use 999 municipality bootstrap replications."
    ),
    size = "\\small",
    landscape = FALSE
  )

  plot_bins <- copy(sensitivities)
  plot_bins[, exposure_bin := pmin(
    12L,
    ceiling(frank(historical_mean_salinity, ties.method = "average") / .N * 12L)
  ), by = crop]
  plot_bins <- plot_bins[, {
    precision_weights <- 1 / sensitivity_bootstrap_variance
    data.table(
      historical_mean_salinity = stats::weighted.mean(
        historical_mean_salinity, precision_weights
      ),
      sensitivity = stats::weighted.mean(sensitivity, precision_weights),
      sensitivity_se = sqrt(1 / sum(precision_weights)),
      municipalities = .N
    )
  }, by = .(crop, exposure_bin)]
  plot_bins[, crop_label := factor(
    crop_labels[crop], levels = unname(crop_labels)
  )]
  fitted_curves <- rbindlist(lapply(crops, function(cr) {
    d <- sensitivities[crop == cr]
    result <- main_results[crop == cr]
    x_values <- seq(
      stats::quantile(d$historical_mean_salinity, 0.01),
      stats::quantile(d$historical_mean_salinity, 0.99),
      length.out = 150L
    )
    data.table(
      crop = cr,
      crop_label = factor(crop_labels[[cr]], levels = unname(crop_labels)),
      historical_mean_salinity = x_values,
      fitted_sensitivity = result$intercept +
        result$adaptation_factor * log(x_values)
    )
  }))
  fig <- ggplot(
    plot_bins,
    aes(historical_mean_salinity, sensitivity)
  ) +
    geom_hline(yintercept = 0, linewidth = 0.35, color = "grey55") +
    geom_linerange(
      aes(
        ymin = sensitivity - 1.96 * sensitivity_se,
        ymax = sensitivity + 1.96 * sensitivity_se
      ),
      linewidth = 0.45,
      color = "grey35"
    ) +
    geom_point(size = 1.8, color = "grey20") +
    geom_line(
      data = fitted_curves,
      aes(historical_mean_salinity, fitted_sensitivity),
      linewidth = 1.05,
      color = "#B22222"
    ) +
    facet_wrap(~crop_label, scales = "free_y", ncol = 2) +
    labs(
      x = "Historical mean raw salinity, 1985-1994 (dS/m)",
      y = "Yield sensitivity to 1 dS/m of excess salinity"
    ) +
    theme_minimal(base_size = 11.5) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.25, color = "grey88"),
      strip.text = element_text(size = 11.5, face = "bold"),
      axis.text = element_text(size = 9.5, color = "black"),
      axis.title = element_text(size = 10.8),
      plot.margin = margin(8, 10, 8, 8)
    )
  ggsave(
    file.path(paths$out_dir, "FullRevision_HistoricalSalinity_Adaptation.pdf"),
    fig, width = 8.3, height = 9.1
  )
  ggsave(
    file.path(paths$out_dir, "FullRevision_HistoricalSalinity_Adaptation.png"),
    fig, width = 8.3, height = 9.1, dpi = 320
  )
  invisible(list(
    sensitivities = sensitivities,
    coefficients = main_results,
    functional_forms = function_results,
    support = support,
    figure = fig
  ))
}

make_sfd_historical_exposure_adaptation_analysis <- function(pam = NULL) {
  write_status("Estimating SFD yield adaptation by historical excess-salinity exposure.")
  if (is.null(pam)) pam <- read_pam_zeros()
  pam <- copy(pam)
  pam[, `:=`(
    Code = trimws(as.character(Code)),
    crop = trimws(tolower(as.character(crop)))
  )]
  pam[crop %in% c("soybean", "soybeans"), crop := "soy"]
  pam[crop == "maize", crop := "corn"]
  pam[crop %in% c("sugar cane", "sugar-cane", "cana-de-acucar", "cana de acucar"), crop := "sugarcane"]

  crops <- c("corn", "rice", "cassava", "beans", "soy", "sugarcane")
  crop_labels <- c(
    corn = "Corn", rice = "Rice", cassava = "Cassava",
    beans = "Beans", soy = "Soybeans", sugarcane = "Sugarcane"
  )
  historical <- pam[
    crop %in% crops & Year >= 1985L & Year <= 1994L &
      is.finite(excess_above_fao_large),
    .(
      historical_excess = mean(excess_above_fao_large),
      historical_years = uniqueN(Year)
    ),
    by = .(Code, crop)
  ][historical_years >= 5L]
  historical[, historical_excess_z := scale_safe(historical_excess), by = crop]
  historical <- historical[is.finite(historical_excess_z)]

  support <- historical[, .(
    municipalities = uniqueN(Code),
    mean_historical_excess = mean(historical_excess),
    sd_historical_excess = stats::sd(historical_excess),
    median_historical_excess = stats::median(historical_excess),
    zero_historical_exposure_share = mean(historical_excess == 0),
    mean_historical_years = mean(historical_years)
  ), by = crop]
  support[, crop_label := crop_labels[crop]]

  unit <- merge(
    pam[
      crop %in% crops & Year >= 1995L & Year <= 2018L &
        is.finite(yield) & yield > 0,
      .(
        Code, crop, Year, log_yield,
        excess_above_fao_large, gdd_large, kdd_large, sm_season_large,
        slope_large, elevation_large, clay_mean_large
      )
    ],
    historical[, .(Code, crop, historical_excess, historical_excess_z)],
    by = c("Code", "crop"), all = FALSE, sort = FALSE
  )
  unit[, excess_x_historical_z := excess_above_fao_large * historical_excess_z]

  pair_map <- read_pair_map()
  vars <- c(
    "log_yield", "excess_above_fao_large", "excess_x_historical_z",
    "gdd_large", "kdd_large", "sm_season_large",
    "slope_large", "elevation_large", "clay_mean_large"
  )
  sfd <- make_sfd_from_unit_panel(
    unit[, c("Code", "crop", "Year", vars), with = FALSE],
    pair_map,
    id_cols = c("crop", "Year"),
    vars = vars
  )

  weather <- c("d_gdd_large", "d_kdd_large", "d_sm_season_large")
  topography <- c("d_slope_large", "d_elevation_large", "d_clay_mean_large")
  treatment <- "d_excess_above_fao_large"
  interaction <- "d_excess_x_historical_z"
  rows <- list()
  models <- list()
  for (cr in crops) {
    required <- c(
      "d_log_yield", treatment, interaction, weather, topography,
      "Year", "lat", "lon", "pair_id"
    )
    d <- complete_data(sfd[crop == cr], required)
    if (nrow(d) < 100L) next
    for (spec in 1:3) {
      controls <- switch(
        as.character(spec),
        "1" = character(),
        "2" = weather,
        "3" = c(weather, topography)
      )
      rhs <- paste(c(treatment, interaction, controls), collapse = " + ")
      warning_messages <- character()
      model <- withCallingHandlers(
        fixest::feols(
          as.formula(paste("d_log_yield ~", rhs, "| Year")),
          data = d,
          vcov = make_conley(cutoff = 200),
          panel.id = ~pair_id + Year,
          notes = FALSE
        ),
        warning = function(w) {
          warning_messages <<- c(warning_messages, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      model_name <- paste(cr, spec, sep = "__")
      models[[model_name]] <- model
      ct <- as.data.table(fixest::coeftable(model), keep.rownames = "term")
      se_col <- intersect(c("Std. Error", "Std..Error", "Std...Error"), names(ct))[1]
      p_col <- intersect(c("Pr(>|t|)", "Pr...t..", "Pr(>|z|)", "Pr...z.."), names(ct))[1]
      beta <- ct[term == treatment]
      alpha <- ct[term == interaction]
      rows[[length(rows) + 1L]] <- cbind(
        data.table(
          crop = cr,
          crop_label = crop_labels[[cr]],
          specification = paste0("Spec. ", spec),
          spec_id = spec,
          excess_slope = beta$Estimate,
          excess_slope_se = beta[[se_col]],
          excess_slope_p = beta[[p_col]],
          adaptation_interaction = alpha$Estimate,
          adaptation_interaction_se = alpha[[se_col]],
          adaptation_interaction_p = alpha[[p_col]],
          observations = stats::nobs(model),
          pairs = uniqueN(d$pair_id),
          conley_psd_adjusted = any(grepl(
            "positive semi-definite", warning_messages, fixed = TRUE
          ))
        ),
        spec_control_flags(spec)
      )
    }
  }
  coefficients <- rbindlist(rows, fill = TRUE)
  coefficients[, `:=`(
    ci95_low = adaptation_interaction - 1.96 * adaptation_interaction_se,
    ci95_high = adaptation_interaction + 1.96 * adaptation_interaction_se,
    ci90_low = adaptation_interaction - 1.645 * adaptation_interaction_se,
    ci90_high = adaptation_interaction + 1.645 * adaptation_interaction_se
  )]
  fwrite(
    coefficients,
    file.path(paths$out_dir, "FullRevision_HistoricalExposure_SFD_Adaptation_Coefficients.csv")
  )
  fwrite(
    support,
    file.path(paths$out_dir, "FullRevision_HistoricalExposure_SFD_Adaptation_Support.csv")
  )

  table_rows <- coefficients[, .(
    Crop = crop_label,
    Specification = specification,
    `Excess-salinity slope` = paste0(fmt(excess_slope, 3), stars(excess_slope_p)),
    `Historical-exposure interaction` = paste0(
      fmt(adaptation_interaction, 3), stars(adaptation_interaction_p)
    ),
    `Interaction SE` = fmt(adaptation_interaction_se, 3),
    Observations = formatC(observations, format = "d", big.mark = ","),
    Pairs = formatC(pairs, format = "d", big.mark = ","),
    GDD = GDD,
    KDD = KDD,
    `Soil moisture` = `Soil moisture`,
    Elevation = Elevation,
    Slope = Slope,
    Clay = Clay,
    `PSD adjustment` = ifelse(conley_psd_adjusted, "Yes", "No")
  )]
  write_latex_df(
    table_rows,
    file.path(paths$out_dir, "FullRevision_HistoricalExposure_SFD_Adaptation.tex"),
    "Historical Excess-Salinity Exposure and Yield Sensitivity",
    "tab:full_revision_historical_exposure_sfd_adaptation",
    note = paste(
      "The dependent variable is the east-minus-west difference in log yield over 1995--2018.",
      "Historical exposure is the municipality-crop mean excess salinity over 1985--1994, standardized within crop.",
      "The reported interaction is the SFD of current excess salinity multiplied by historical exposure.",
      "A positive interaction means that the yield slope becomes less negative as historical exposure rises; this pattern is compatible with adaptation but can also reflect selection or persistent production differences.",
      "All specifications include year fixed effects. Spec. 2 is preferred and adds GDD, KDD and soil moisture; Spec. 3 also adds slope, elevation and clay.",
      "Each crop uses a common complete-data sample across specifications. Standard errors are Conley spatial with a 200 km cutoff."
    ),
    size = "\\scriptsize",
    landscape = TRUE
  )

  plot_dt <- copy(coefficients)
  crop_order_top <- c("Corn", "Rice", "Cassava", "Beans", "Soybeans", "Sugarcane")
  crop_order_bottom <- rev(crop_order_top)
  offsets <- c("Spec. 1" = -0.22, "Spec. 2" = 0, "Spec. 3" = 0.22)
  plot_dt[, specification := factor(specification, levels = names(offsets))]
  plot_dt[, crop_position := match(crop_label, crop_order_bottom)]
  plot_dt[, plot_position := crop_position + offsets[as.character(specification)]]
  pal <- c("Spec. 1" = "#7570B3", "Spec. 2" = "#D95F02", "Spec. 3" = "#1B9E77")
  shapes <- c("Spec. 1" = 15, "Spec. 2" = 17, "Spec. 3" = 16)
  fig <- ggplot(
    plot_dt,
    aes(
      x = adaptation_interaction, y = plot_position,
      color = specification, shape = specification
    )
  ) +
    geom_segment(
      aes(x = ci95_low, xend = ci95_high, yend = plot_position),
      linewidth = 0.45, alpha = 0.55, lineend = "butt"
    ) +
    geom_segment(
      aes(x = ci90_low, xend = ci90_high, yend = plot_position),
      linewidth = 1.15, lineend = "butt"
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.45, color = "grey20") +
    geom_point(size = 2.7, stroke = 0.65) +
    scale_y_continuous(
      breaks = seq_along(crop_order_bottom), labels = crop_order_bottom,
      expand = expansion(mult = c(0.08, 0.08))
    ) +
    scale_color_manual(values = pal, guide = "none") +
    scale_shape_manual(values = shapes, guide = "none") +
    labs(
      x = "Change in the excess-salinity slope for a one-SD increase in historical exposure",
      y = NULL
    ) +
    theme_minimal(base_size = 12.5) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.35, color = "grey88"),
      axis.text.y = element_text(size = 13, face = "bold", color = "grey30"),
      axis.text.x = element_text(size = 10.5, color = "grey30"),
      axis.title.x = element_text(size = 11.5, margin = margin(t = 8)),
      plot.margin = margin(8, 10, 8, 8)
    )
  ggsave(
    file.path(paths$out_dir, "FullRevision_HistoricalExposure_SFD_Adaptation.pdf"),
    fig, width = 8.3, height = 5.3
  )
  ggsave(
    file.path(paths$out_dir, "FullRevision_HistoricalExposure_SFD_Adaptation.png"),
    fig, width = 8.3, height = 5.3, dpi = 320
  )
  invisible(list(
    coefficients = coefficients,
    support = support,
    models = models,
    figure = fig
  ))
}

make_supervisor_output_standardized_analysis <- function() {
  write_status("Restoring and standardizing the supervisor-version output specification.")
  d_all <- as.data.table(readRDS(paths$sfd_rds))
  d_all[, crop := trimws(tolower(as.character(crop)))]
  d_all[crop %in% c("soybean", "soybeans"), crop := "soy"]
  d_all[crop == "maize", crop := "corn"]
  d_all[crop %in% c("sugar cane", "sugar-cane", "cana-de-acucar", "cana de acucar"), crop := "sugarcane"]

  crops <- c("corn", "rice", "cassava", "beans", "soy", "sugarcane")
  crop_labels <- c(
    corn = "Corn", rice = "Rice", cassava = "Cassava",
    beans = "Beans", soy = "Soybeans", sugarcane = "Sugarcane"
  )
  treatment <- "d_excess_above_fao_large"
  outcome <- "d_log_output_quantity"
  weather <- c("d_gdd_large", "d_kdd_large", "d_sm_season_large")
  topography <- c("d_slope_large", "d_elevation_large", "d_clay_mean_large")
  rows <- list()
  models <- list()

  for (cr in crops) {
    required <- c(
      outcome, treatment, weather, topography,
      "Year", "lat", "lon", "pair_id"
    )
    d <- complete_data(d_all[crop == cr], required)
    if (nrow(d) < 100L) next
    treatment_sd <- stats::sd(d[[treatment]])
    if (!is.finite(treatment_sd) || treatment_sd <= 0) next
    for (spec in 1:3) {
      controls <- switch(
        as.character(spec),
        "1" = character(),
        "2" = weather,
        "3" = c(weather, topography)
      )
      warning_messages <- character()
      model <- withCallingHandlers(
        fixest::feols(
          as.formula(paste(outcome, "~", paste(c(treatment, controls), collapse = " + "), "| Year")),
          data = d,
          vcov = make_conley(cutoff = 200),
          panel.id = ~pair_id + Year,
          notes = FALSE
        ),
        warning = function(w) {
          warning_messages <<- c(warning_messages, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      model_name <- paste(cr, spec, sep = "__")
      models[[model_name]] <- model
      ct <- as.data.table(fixest::coeftable(model), keep.rownames = "term")
      se_col <- intersect(c("Std. Error", "Std..Error", "Std...Error"), names(ct))[1]
      p_col <- intersect(c("Pr(>|t|)", "Pr...t..", "Pr(>|z|)", "Pr...z.."), names(ct))[1]
      estimate_row <- ct[term == treatment]
      raw_estimate <- estimate_row$Estimate
      raw_se <- estimate_row[[se_col]]
      pair_cluster_se <- NA_real_
      pair_cluster_p <- NA_real_
      if (spec == 2L) {
        pair_cluster_model <- fixest::feols(
          as.formula(paste(outcome, "~", paste(c(treatment, controls), collapse = " + "), "| Year")),
          data = d,
          cluster = ~pair_id,
          panel.id = ~pair_id + Year,
          notes = FALSE
        )
        pair_cluster_ct <- as.data.table(
          fixest::coeftable(pair_cluster_model),
          keep.rownames = "term"
        )
        pair_cluster_se_col <- intersect(
          c("Std. Error", "Std..Error", "Std...Error"), names(pair_cluster_ct)
        )[1]
        pair_cluster_p_col <- intersect(
          c("Pr(>|t|)", "Pr...t..", "Pr(>|z|)", "Pr...z.."), names(pair_cluster_ct)
        )[1]
        pair_cluster_row <- pair_cluster_ct[term == treatment]
        pair_cluster_se <- pair_cluster_row[[pair_cluster_se_col]]
        pair_cluster_p <- pair_cluster_row[[pair_cluster_p_col]]
      }
      rows[[length(rows) + 1L]] <- cbind(
        data.table(
          crop = cr,
          crop_label = crop_labels[[cr]],
          specification = paste0("Spec. ", spec),
          spec_id = spec,
          raw_estimate = raw_estimate,
          raw_se = raw_se,
          p_value = estimate_row[[p_col]],
          pair_cluster_se = pair_cluster_se,
          pair_cluster_p = pair_cluster_p,
          treatment_sd = treatment_sd,
          standardized_estimate = raw_estimate * treatment_sd,
          standardized_se = raw_se * treatment_sd,
          observations = stats::nobs(model),
          pairs = uniqueN(d$pair_id),
          conley_psd_adjusted = any(grepl(
            "positive semi-definite", warning_messages, fixed = TRUE
          ))
        ),
        spec_control_flags(spec)
      )
    }
  }
  coefficients <- rbindlist(rows, fill = TRUE)
  coefficients[, `:=`(
    ci95_low = standardized_estimate - 1.96 * standardized_se,
    ci95_high = standardized_estimate + 1.96 * standardized_se,
    ci90_low = standardized_estimate - 1.645 * standardized_se,
    ci90_high = standardized_estimate + 1.645 * standardized_se
  )]
  fwrite(
    coefficients,
    file.path(paths$out_dir, "FullRevision_Output_SupervisorSpec_Standardized_Coefficients.csv")
  )

  table_rows <- coefficients[, .(
    Crop = crop_label,
    Specification = specification,
    `Raw coefficient` = paste0(fmt(raw_estimate, 4), stars(p_value)),
    `Treatment SD` = fmt(treatment_sd, 4),
    `One-SD effect` = paste0(fmt(standardized_estimate, 4), stars(p_value)),
    `One-SD SE` = fmt(standardized_se, 4),
    Observations = formatC(observations, format = "d", big.mark = ","),
    Pairs = formatC(pairs, format = "d", big.mark = ","),
    GDD = GDD,
    KDD = KDD,
    `Soil moisture` = `Soil moisture`,
    Elevation = Elevation,
    Slope = Slope,
    Clay = Clay,
    `PSD adjustment` = ifelse(conley_psd_adjusted, "Yes", "No")
  )]
  write_latex_df(
    table_rows,
    file.path(paths$out_dir, "FullRevision_Output_SupervisorSpec_Standardized.tex"),
    "Crop Output Responses to Excess Soil Salinity",
    "tab:full_revision_output_supervisor_standardized",
    note = paste(
      "The dependent variable is the east-minus-west difference in log physical output quantity.",
      "The treatment is the east-minus-west difference in crop-specific excess salinity in dS/m.",
      "One-SD effects multiply the raw coefficient and its standard error by the crop-specific standard deviation of the treatment; this rescaling does not change p-values or statistical significance.",
      "All specifications include year fixed effects. Spec. 2 is preferred and adds GDD, KDD and soil moisture; Spec. 3 also adds slope, elevation and clay.",
      "Each crop uses one common complete-data sample across specifications. Standard errors are Conley spatial with a 200 km cutoff."
    ),
    size = "\\scriptsize",
    landscape = TRUE
  )

  inference_comparison <- coefficients[spec_id == 2L, .(
    Crop = crop_label,
    `Raw coefficient` = fmt(raw_estimate, 4),
    `Conley SE (200 km)` = fmt(raw_se, 4),
    `Conley p-value` = fmt(p_value, 3),
    `Pair-clustered SE` = fmt(pair_cluster_se, 4),
    `Pair-clustered p-value` = fmt(pair_cluster_p, 3),
    Observations = formatC(observations, format = "d", big.mark = ","),
    Pairs = formatC(pairs, format = "d", big.mark = ",")
  )]
  write_latex_df(
    inference_comparison,
    file.path(paths$out_dir, "FullRevision_Output_Conley_PairCluster_Comparison.tex"),
    "Crop Output: Conley and Pair-Clustered Inference",
    "tab:full_revision_output_conley_pair_cluster_comparison",
    note = paste(
      "The preferred crop-output specification, coefficient, and estimation sample are held fixed.",
      "Only the covariance estimator changes: Conley spatial with a 200 km cutoff versus clustering by spatial municipality pair.",
      "Pair clustering allows unrestricted dependence over time within an exact pair but does not replace the main Conley correction for dependence across nearby pairs."
    ),
    size = "\\small"
  )

  plot_dt <- copy(coefficients)
  crop_order_top <- c("Corn", "Rice", "Cassava", "Beans", "Soybeans", "Sugarcane")
  crop_order_bottom <- rev(crop_order_top)
  offsets <- c("Spec. 1" = -0.22, "Spec. 2" = 0, "Spec. 3" = 0.22)
  plot_dt[, specification := factor(specification, levels = names(offsets))]
  plot_dt[, crop_position := match(crop_label, crop_order_bottom)]
  plot_dt[, plot_position := crop_position + offsets[as.character(specification)]]
  pal <- c("Spec. 1" = "#7570B3", "Spec. 2" = "#D95F02", "Spec. 3" = "#1B9E77")
  shapes <- c("Spec. 1" = 15, "Spec. 2" = 17, "Spec. 3" = 16)
  fig <- ggplot(
    plot_dt,
    aes(
      x = standardized_estimate, y = plot_position,
      color = specification, shape = specification
    )
  ) +
    geom_segment(
      aes(x = ci95_low, xend = ci95_high, yend = plot_position),
      linewidth = 0.45, alpha = 0.55, lineend = "butt"
    ) +
    geom_segment(
      aes(x = ci90_low, xend = ci90_high, yend = plot_position),
      linewidth = 1.15, lineend = "butt"
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.45, color = "grey20") +
    geom_point(size = 2.7, stroke = 0.65) +
    scale_y_continuous(
      breaks = seq_along(crop_order_bottom), labels = crop_order_bottom,
      expand = expansion(mult = c(0.08, 0.08))
    ) +
    scale_color_manual(values = pal, guide = "none") +
    scale_shape_manual(values = shapes, guide = "none") +
    labs(
      x = "Effect of a one-SD increase in excess salinity on log output quantity",
      y = NULL
    ) +
    theme_minimal(base_size = 12.5) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.35, color = "grey88"),
      axis.text.y = element_text(size = 13, face = "bold", color = "grey30"),
      axis.text.x = element_text(size = 10.5, color = "grey30"),
      axis.title.x = element_text(size = 11.5, margin = margin(t = 8)),
      plot.margin = margin(8, 10, 8, 8)
    )
  ggsave(
    file.path(paths$out_dir, "FullRevision_Output_LogQuantity_SupervisorSpec_Standardized.pdf"),
    fig, width = 8.3, height = 5.2
  )
  ggsave(
    file.path(paths$out_dir, "FullRevision_Output_LogQuantity_SupervisorSpec_Standardized.png"),
    fig, width = 8.3, height = 5.2, dpi = 320
  )
  invisible(list(coefficients = coefficients, models = models, figure = fig))
}

make_output_asinh_comparison <- function(pam = NULL) {
  write_status("Comparing log and asinh crop-output specifications with and without short zero spells.")
  d_log <- as.data.table(readRDS(paths$sfd_rds))
  d_log[, crop := trimws(tolower(as.character(crop)))]
  d_log[crop %in% c("soybean", "soybeans"), crop := "soy"]
  d_log[crop == "maize", crop := "corn"]
  d_log[crop %in% c("sugar cane", "sugar-cane", "cana-de-acucar", "cana de acucar"), crop := "sugarcane"]
  d_log[, d_asinh_output_positive :=
    asinh(exp(log_output_quantity)) - asinh(exp(log_output_quantity_west))]

  if (is.null(pam)) pam <- read_pam_zeros()
  pam <- add_five_year_zero_criterion(pam)
  zero_vars <- intersect(c(
    "asinh_quantity_keep_5yr", "excess_above_fao_large",
    "gdd_large", "kdd_large", "sm_season_large",
    "slope_large", "elevation_large", "clay_mean_large"
  ), names(pam))
  d_zero <- make_sfd_from_unit_panel(
    pam[, c("Code", "crop", "Year", zero_vars), with = FALSE],
    read_pair_map(),
    id_cols = c("crop", "Year"),
    vars = zero_vars
  )
  d_zero[, crop := trimws(tolower(as.character(crop)))]
  d_zero[crop %in% c("soybean", "soybeans"), crop := "soy"]
  d_zero[crop == "maize", crop := "corn"]
  d_zero[crop %in% c("sugar cane", "sugar-cane", "cana-de-acucar", "cana de acucar"), crop := "sugarcane"]

  crops <- c("corn", "rice", "cassava", "beans", "soy", "sugarcane")
  crop_labels <- c(
    corn = "Corn", rice = "Rice", cassava = "Cassava",
    beans = "Beans", soy = "Soybeans", sugarcane = "Sugarcane"
  )
  treatment <- "d_excess_above_fao_large"
  weather <- c("d_gdd_large", "d_kdd_large", "d_sm_season_large")
  topography <- c("d_slope_large", "d_elevation_large", "d_clay_mean_large")
  variants <- list(
    "Log, positive-output sample" = list(data = d_log, outcome = "d_log_output_quantity", sample = "positive"),
    "Asinh, same positive-output sample" = list(data = d_log, outcome = "d_asinh_output_positive", sample = "positive"),
    "Asinh, short zero spells retained" = list(data = d_zero, outcome = "d_asinh_quantity_keep_5yr", sample = "zeros")
  )
  rows <- list()
  models <- list()
  vc <- make_conley(cutoff = 200)
  for (cr in crops) {
    prepared <- list()
    for (variant_name in names(variants)) {
      meta <- variants[[variant_name]]
      required <- c(
        meta$outcome, treatment, weather, topography,
        "Year", "lat", "lon", "pair_id"
      )
      prepared[[variant_name]] <- complete_data(meta$data[crop == cr], required)
    }
    for (variant_name in names(variants)) {
      meta <- variants[[variant_name]]
      d <- prepared[[variant_name]]
      if (nrow(d) < 100L) next
      treatment_sd <- stats::sd(d[[treatment]])
      for (spec in 1:3) {
        controls <- switch(
          as.character(spec),
          "1" = character(),
          "2" = weather,
          "3" = c(weather, topography)
        )
        model <- fixest::feols(
          as.formula(paste(
            meta$outcome, "~", paste(c(treatment, controls), collapse = " + "),
            "| Year"
          )),
          data = d,
          vcov = vc,
          panel.id = ~pair_id + Year,
          notes = FALSE
        )
        model_name <- paste(cr, variant_name, spec, sep = "__")
        models[[model_name]] <- model
        ct <- as.data.table(fixest::coeftable(model), keep.rownames = "term")
        se_col <- intersect(c("Std. Error", "Std..Error", "Std...Error"), names(ct))[1]
        p_col <- intersect(c("Pr(>|t|)", "Pr...t..", "Pr(>|z|)", "Pr...z.."), names(ct))[1]
        z <- ct[get("term") == treatment]
        rows[[length(rows) + 1L]] <- data.table(
          crop = cr,
          crop_label = crop_labels[[cr]],
          outcome_variant = variant_name,
          specification = spec,
          estimate = z$Estimate,
          standard_error = z[[se_col]],
          p_value = z[[p_col]],
          treatment_sd = treatment_sd,
          standardized_estimate = z$Estimate * treatment_sd,
          standardized_se = z[[se_col]] * treatment_sd,
          observations = stats::nobs(model),
          pairs = uniqueN(d$pair_id)
        )
      }
    }
  }
  results <- rbindlist(rows)
  fwrite(results, file.path(paths$out_dir, "FullRevision_Output_Log_Asinh_Comparison_Coefficients.csv"))

  make_asinh_output_figure <- function(variant_name, file_stub, x_label) {
    plot_data <- copy(results[
      outcome_variant == variant_name & is.finite(standardized_estimate) & is.finite(standardized_se)
    ])
    plot_data[, `:=`(
      ci90_low = standardized_estimate - 1.645 * standardized_se,
      ci90_high = standardized_estimate + 1.645 * standardized_se,
      ci95_low = standardized_estimate - 1.96 * standardized_se,
      ci95_high = standardized_estimate + 1.96 * standardized_se,
      crop_label = factor(crop_label, levels = rev(unname(crop_labels))),
      specification_label = factor(
        paste0("Spec. ", specification),
        levels = c("Spec. 1", "Spec. 2", "Spec. 3")
      )
    )]
    plot_data[, y_position := as.numeric(crop_label) + c(
      "Spec. 1" = -0.16,
      "Spec. 2" = 0,
      "Spec. 3" = 0.16
    )[as.character(specification_label)]]
    figure <- ggplot(
      plot_data,
      aes(standardized_estimate, y_position, color = specification_label, shape = specification_label)
    ) +
      geom_vline(xintercept = 0, linewidth = 0.45, color = "grey45", linetype = "dashed") +
      geom_segment(
        aes(x = ci95_low, xend = ci95_high, yend = y_position),
        linewidth = 0.55
      ) +
      geom_segment(
        aes(x = ci90_low, xend = ci90_high, yend = y_position),
        linewidth = 1.10
      ) +
      geom_point(size = 2.5) +
      scale_color_manual(
        values = c("Spec. 1" = "#6A51A3", "Spec. 2" = "#D95F02", "Spec. 3" = "#1B9E77"),
        name = NULL
      ) +
      scale_shape_manual(values = c("Spec. 1" = 15, "Spec. 2" = 17, "Spec. 3" = 16), name = NULL) +
      scale_y_continuous(
        breaks = seq_along(levels(plot_data$crop_label)),
        labels = levels(plot_data$crop_label)
      ) +
      labs(x = x_label, y = NULL) +
      theme_minimal(base_size = 12.5) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(size = 11, color = "black"),
        axis.title.x = element_text(size = 11.5),
        legend.text = element_text(size = 11),
        plot.margin = margin(8, 10, 8, 10)
      )
    ggsave(file.path(paths$out_dir, paste0(file_stub, ".pdf")), figure, width = 7.6, height = 4.8)
    ggsave(file.path(paths$out_dir, paste0(file_stub, ".png")), figure, width = 7.6, height = 4.8, dpi = 320)
  }
  make_asinh_output_figure(
    "Asinh, short zero spells retained",
    "FullRevision_Output_Asinh_ZeroInclusive",
    "One-SD effect on asinh physical output\n(short zero spells retained)"
  )
  make_asinh_output_figure(
    "Asinh, same positive-output sample",
    "FullRevision_Output_Asinh_SamePositiveSample",
    "One-SD effect on asinh physical output\n(same positive-output sample as log model)"
  )

  preferred <- results[specification == 2L, .(
    Crop = crop_label,
    Outcome = outcome_variant,
    `Raw coefficient` = paste0(fmt(estimate, 4), stars(p_value)),
    `Standard error` = paste0("(", fmt(standard_error, 4), ")"),
    `Treatment SD` = fmt(treatment_sd, 4),
    `One-SD effect` = fmt(standardized_estimate, 4),
    Observations = formatC(observations, format = "d", big.mark = ","),
    Pairs = formatC(pairs, format = "d", big.mark = ",")
  )]
  write_latex_df(
    preferred,
    file.path(paths$out_dir, "FullRevision_Output_Log_Asinh_Comparison.tex"),
    "Crop Output: Log and Asinh Specifications",
    "tab:full_revision_output_log_asinh_comparison",
    note = paste(
      "All rows report preferred Spec. 2 with year fixed effects, GDD, KDD and soil moisture, and Conley spatial standard errors with a 200 km cutoff.",
      "The log and same-sample asinh rows use exactly the same crop-specific complete-data sample with positive output on both sides of the spatial pair.",
      "Their difference isolates the functional form. The zero-inclusive asinh row additionally retains zero output when it belongs to a spell shorter than five consecutive years; longer zero runs are treated as structural missing values.",
      "Differences between the second and third rows therefore also reflect the enlarged sample and transitions into or out of zero output.",
      "Asinh coefficients approximate log changes for large positive quantities but do not have a percentage interpretation for transitions involving zero."
    ),
    size = "\\scriptsize",
    landscape = TRUE
  )
  invisible(list(coefficients = results, models = models))
}

make_north_south_yield_robustness <- function() {
  write_status("Regenerating readable north--south yield robustness tables.")
  input_file <- file.path(paths$data_dir, "PAM_SFD_north_south_200km_ready.rds")
  if (!file.exists(input_file)) stop("Missing north--south SFD file: ", input_file)
  d_all <- as.data.table(readRDS(input_file))

  crop_order <- c("corn", "rice", "cassava", "beans", "soy", "sugarcane")
  crop_labels <- c(
    corn = "Corn", rice = "Rice", cassava = "Cassava",
    beans = "Beans", soy = "Soybeans", sugarcane = "Sugarcane"
  )
  treatments <- list(
    Excess = list(
      variable = "d_excess_above_fao_large",
      title = "North--South Yield Robustness: Excess Salinity",
      label_stub = "excess",
      description = "crop-specific excess salinity above the FAO threshold"
    ),
    Mean = list(
      variable = "d_mean_salinity",
      title = "North--South Yield Robustness: Raw Mean Salinity",
      label_stub = "mean",
      description = "raw mean soil salinity"
    )
  )
  weather <- c("d_gdd_large", "d_kdd_large", "d_sm_season_large")
  topography <- c("d_slope_large", "d_elevation_large", "d_clay_mean_large")
  summary_rows <- list()
  output_files <- character()

  fixest::setFixest_dict(c(
    d_excess_above_fao_large = "$\\Delta_{N-S}$ Excess salinity (dS/m)",
    d_mean_salinity = "$\\Delta_{N-S}$ Raw mean salinity (dS/m)",
    d_gdd_large = "$\\Delta_{N-S}$ GDD",
    d_kdd_large = "$\\Delta_{N-S}$ KDD",
    d_sm_season_large = "$\\Delta_{N-S}$ Soil moisture",
    d_slope_large = "$\\Delta_{N-S}$ Slope",
    d_elevation_large = "$\\Delta_{N-S}$ Elevation",
    d_clay_mean_large = "$\\Delta_{N-S}$ Clay"
  ), reset = TRUE)

  for (treatment_name in names(treatments)) {
    meta <- treatments[[treatment_name]]
    models <- list()
    model_psd <- logical()
    headers_crop <- character()
    headers_spec <- character()

    for (cr in crop_order) {
      required <- c(
        "d_log_yield", meta$variable, weather, topography,
        "Year", "lat", "lon", "pair_id"
      )
      d <- complete_data(d_all[crop == cr], required)
      rhs <- list(
        meta$variable,
        paste(c(meta$variable, weather), collapse = " + "),
        paste(c(meta$variable, weather, topography), collapse = " + ")
      )
      for (spec_id in seq_along(rhs)) {
        warning_messages <- character()
        model <- withCallingHandlers(
          fixest::feols(
            as.formula(paste("d_log_yield ~", rhs[[spec_id]], "| Year")),
            data = d,
            vcov = make_conley(cutoff = 200),
            panel.id = ~pair_id + Year,
            notes = FALSE
          ),
          warning = function(w) {
            warning_messages <<- c(warning_messages, conditionMessage(w))
            invokeRestart("muffleWarning")
          }
        )
        model_name <- paste(crop_labels[[cr]], paste0("Spec. ", spec_id), sep = " - ")
        models[[model_name]] <- model
        psd_adjusted <- any(grepl("positive semi-definite", warning_messages, fixed = TRUE))
        model_psd[[model_name]] <- psd_adjusted
        ct <- fixest::coeftable(model)
        treatment_row <- ct[meta$variable, , drop = FALSE]
        summary_rows[[length(summary_rows) + 1L]] <- data.table(
          treatment = treatment_name,
          crop = crop_labels[[cr]],
          specification = paste0("Spec. ", spec_id),
          estimate = unname(treatment_row[1L, "Estimate"]),
          standard_error = unname(treatment_row[1L, "Std. Error"]),
          p_value = unname(treatment_row[1L, ncol(treatment_row)]),
          observations = stats::nobs(model),
          conley_psd_adjusted = psd_adjusted
        )
      }
      headers_crop <- c(headers_crop, rep(crop_labels[[cr]], 3L))
      headers_spec <- c(headers_spec, paste0("Spec. ", 1:3))
    }

    panel_groups <- list(A = seq_len(9L), B = 10L:18L)
    adjusted_models <- names(model_psd)[model_psd]
    adjustment_note <- if (length(adjusted_models)) {
      paste0(
        "The following covariance matrices required a positive semi-definite adjustment: ",
        paste(adjusted_models, collapse = ", "), "."
      )
    } else {
      "No covariance matrix required a positive semi-definite adjustment."
    }
    table_note <- paste(
      "Notes: The dependent variable is the north-minus-south difference in log crop yield.",
      "Pairs are strictly contiguous municipalities in the same State, hydrological basin and longitude channel; the nearest eligible southern neighbour is retained.",
      paste0("The treatment is ", meta$description, "."),
      "Spec. 1 includes year fixed effects; preferred Spec. 2 adds GDD, KDD and soil moisture; Spec. 3 adds slope, elevation and clay.",
      "Each crop uses one common complete-data sample across specifications. Conley spatial standard errors use a 200 km cutoff.",
      adjustment_note
    )
    for (panel_name in names(panel_groups)) {
      model_ids <- panel_groups[[panel_name]]
      tex_file <- file.path(
        paths$out_dir,
        paste0("FullRevision_NS_Yields_", meta$label_stub, "_Panel", panel_name, ".tex")
      )
      fixest::etable(
        models[model_ids],
        tex = TRUE,
        file = tex_file,
        replace = TRUE,
        headers = list(
          "Crop" = headers_crop[model_ids],
          "Spec" = headers_spec[model_ids]
        ),
        depvar = FALSE,
        title = paste0(meta$title, ": Panel ", panel_name),
        label = paste0("tab:full_revision_ns_yields_", meta$label_stub, "_panel_", tolower(panel_name)),
        fitstat = ~ r2 + n,
        signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
        notes = table_note
      )
      style_fixest_tex(
        tex_file,
        size = "\\scriptsize",
        tabcolsep = "2.5pt",
        arraystretch = "0.82",
        resize = TRUE,
        landscape = TRUE
      )
      output_files <- c(output_files, tex_file)
    }
  }

  summary_dt <- rbindlist(summary_rows)
  fwrite(summary_dt, file.path(paths$out_dir, "FullRevision_NS_Yields_Summary.csv"))
  invisible(list(summary = summary_dt, files = output_files))
}

make_pair_restriction_diagnostics <- function() {
  write_status("Building pair-restriction diagnostics.")
  pam <- read_pam_zeros()
  pair_map <- read_pair_map()
  all_munis <- unique(pam$Code)
  pair_munis <- unique(c(pair_map$Code, pair_map$code_neighbor_west))
  diag <- data.table(
    Metric = c(
      "Municipalities in PAM zero panel",
      "West-east pairs retained",
      "Municipalities with admissible western neighbor",
      "Municipalities appearing on either side of a pair",
      "Municipalities never appearing in a final pair",
      "Municipalities without admissible western-neighbor role"
    ),
    Value = c(
      formatC(length(all_munis), format = "d", big.mark = ","),
      formatC(uniqueN(pair_map$pair_id), format = "d", big.mark = ","),
      formatC(uniqueN(pair_map$Code), format = "d", big.mark = ","),
      formatC(length(pair_munis), format = "d", big.mark = ","),
      formatC(length(setdiff(all_munis, pair_munis)), format = "d", big.mark = ","),
      formatC(length(setdiff(all_munis, unique(pair_map$Code))), format = "d", big.mark = ",")
    )
  )
  write_metric_table(
    diag,
    file.path(paths$out_dir, "FullRevision_Pair_Restriction_Diagnostics.tex"),
    "Spatial-Pair Restriction Diagnostics",
    "tab:full_revision_pair_restrictions",
    note = paste(
      "Final pairs compare each municipality with its strictly contiguous western neighbor within the same state, main hydrological basin, and latitude channel.",
      "The number of excluded municipalities depends on the definition: some municipalities have no admissible western neighbor but appear as the western neighbor of another municipality."
    )
  )
  fwrite(diag, file.path(paths$out_dir, "FullRevision_Pair_Restriction_Diagnostics.csv"))
  invisible(diag)
}

make_sfd_covariate_variation_diagnostics <- function() {
  write_status("Building within-pair salinity and observed-characteristic diagnostics.")
  sfd <- readRDS(paths$sfd_rds)
  setDT(sfd)

  difference_row <- function(east, west, group, variable, unit, digits = 3L) {
    ok <- is.finite(east) & is.finite(west)
    east <- east[ok]
    west <- west[ok]
    difference <- east - west
    difference_sd <- stats::sd(difference)
    level_sd <- stats::sd(c(east, west))
    data.table(
      group = group,
      variable = variable,
      unit = unit,
      mean_sfd = mean(difference),
      sd_sfd = difference_sd,
      relative_random_pair_sd = fifelse(
        is.finite(level_sd) & level_sd > 0,
        difference_sd / (sqrt(2) * level_sd),
        NA_real_
      ),
      observations = length(difference),
      digits = digits
    )
  }

  main_variables <- data.table(
    variable = c(
      "Raw mean ECe", "Crop-specific excess ECe",
      "Elevation", "Slope", "Clay content"
    ),
    unit = c("dS/m", "dS/m", "m", "%", "%"),
    east = c(
      "mean_salinity", "excess_above_fao_large",
      "elevation_large", "slope_large", "clay_mean_large"
    ),
    west = c(
      "mean_salinity_west", "excess_above_fao_large_west",
      "elevation_large_west", "slope_large_west", "clay_mean_large_west"
    )
  )
  regression_needed <- c(
    "log_yield", "d_log_yield", "d_excess_above_fao_large",
    "d_gdd_large", "d_kdd_large", "d_sm_season_large",
    main_variables$east, main_variables$west,
    "yield", "Year", "pair_id", "lat", "lon"
  )
  sample <- complete_data(sfd, unique(regression_needed))
  sample <- sample[is.finite(yield) & yield > 0]
  if (nrow(sample) == 0L) stop("The common main-yield SFD sample is empty.")

  main_group <- "Panel A: Salinity and land characteristics - main yield sample, 1985--2018"
  main_rows <- rbindlist(lapply(seq_len(nrow(main_variables)), function(i) {
    difference_row(
      sample[[main_variables$east[i]]],
      sample[[main_variables$west[i]]],
      main_group,
      main_variables$variable[i],
      main_variables$unit[i]
    )
  }))

  price_file <- file.path(paths$out_dir, "FullRevision_Price_SFD_Means_ByCrop.csv")
  price_required <- c(
    "Crop", "observations", "mean_sfd_log_price", "sd_sfd_log_price",
    "relative_random_pair_sd", "year_min", "year_max"
  )
  if (!file.exists(price_file)) stop("The crop-price SFD diagnostics are missing. Run --price-only first.")
  price <- fread(price_file)
  if (!all(price_required %in% names(price))) {
    stop("The crop-price SFD diagnostics are stale. Run --price-only before rebuilding this table.")
  }
  price_rows <- price[, .(
    group = paste0(
      "Panel B: Municipal producer prices - crop-specific PAM samples, ",
      min(year_min), "--", max(year_max)
    ),
    variable = paste0("Log producer price: ", Crop),
    unit = "log points",
    mean_sfd = mean_sfd_log_price,
    sd_sfd = sd_sfd_log_price,
    relative_random_pair_sd = relative_random_pair_sd,
    observations = observations,
    digits = 4L
  )]

  detailed_file <- file.path(paths$out_dir, "FullRevision_Inputs_2017_Municipal.csv")
  harmonized_file <- file.path(paths$out_dir, "FullRevision_Inputs_2006_2017_Municipal.csv")
  if (!all(file.exists(c(detailed_file, harmonized_file)))) {
    stop("The cached Agricultural Census municipal files are missing. Run --inputs-2017-only and --inputs-panel-only first.")
  }
  census <- fread(detailed_file, colClasses = list(character = "Code"))
  harmonized <- fread(harmonized_file, colClasses = list(character = "Code"))
  census[, Code := trimws(Code)]
  harmonized[, Code := trimws(Code)]
  add_2017 <- c(
    "workers_per_100_establishments_2017",
    "asinh_irrigated_ha_per_100_establishments_2017",
    "wage_expense_share_2017"
  )
  census <- merge(
    census,
    harmonized[, c("Code", add_2017), with = FALSE],
    by = "Code", all.x = TRUE
  )
  ratio100 <- function(numerator, denominator) {
    fifelse(
      is.finite(numerator) & is.finite(denominator) & denominator > 0,
      100 * numerator / denominator,
      NA_real_
    )
  }
  census[, `:=`(
    log_total_establishments_2017 = fifelse(
      is.finite(total_establishments_2017) & total_establishments_2017 >= 0,
      log1p(total_establishments_2017), NA_real_
    ),
    machines_per_100_establishments_2017 = ratio100(
      machines_total, total_establishments_2017
    ),
    fertilizer_corrective_expense_establishment_share_2017 = ratio100(
      fertilizer_corrective_expense_establishments, total_establishments_2017
    ),
    seed_expense_establishment_share_2017 = ratio100(
      seed_expense_establishments, total_establishments_2017
    ),
    pesticide_expense_establishment_share_2017 = ratio100(
      pesticide_expense_establishments, total_establishments_2017
    ),
    energy_expense_establishment_share_2017 = ratio100(
      energy_expense_establishments, total_establishments_2017
    ),
    machinery_vehicle_expense_establishment_share_2017 = ratio100(
      machinery_vehicle_expense_establishments, total_establishments_2017
    ),
    fuel_expense_establishment_share_2017 = ratio100(
      fuel_lubricant_expense_establishments, total_establishments_2017
    )
  )]
  census_metadata <- data.table(
    outcome = c(
      "log_total_establishments_2017",
      "fertilizer_any_share_2017", "chemical_fertilizer_share_2017",
      "organic_fertilizer_share_2017", "soil_ph_corrective_share_2017",
      "pesticide_use_share_2017", "irrigation_establishment_share_2017",
      "asinh_irrigated_ha_per_100_establishments_2017",
      "tractor_establishment_share_2017", "tractors_per_100_establishments_2017",
      "machines_per_100_establishments_2017",
      "fertilizer_spreaders_per_100_establishments_2017",
      "workers_per_100_establishments_2017",
      "fertilizer_corrective_expense_establishment_share_2017",
      "seed_expense_establishment_share_2017",
      "pesticide_expense_establishment_share_2017",
      "energy_expense_establishment_share_2017",
      "machinery_vehicle_expense_establishment_share_2017",
      "fuel_expense_establishment_share_2017",
      "fertilizer_corrective_expense_share_2017", "seed_expense_share_2017",
      "pesticide_expense_share_2017", "energy_expense_share_2017",
      "machinery_vehicle_expense_share_2017", "fuel_lubricant_expense_share_2017",
      "wage_expense_share_2017"
    ),
    label = c(
      "Log agricultural establishments",
      "Establishments using fertilizer", "Establishments using chemical fertilizer",
      "Establishments using organic fertilizer", "Establishments using lime or a soil-pH corrective",
      "Establishments using pesticides", "Establishments using irrigation",
      "Asinh irrigated hectares per 100 establishments",
      "Establishments with tractors", "Tractors per 100 establishments",
      "Machines per 100 establishments", "Fertilizer spreaders per 100 establishments",
      "Agricultural workers per 100 establishments",
      "Establishments reporting fertilizer/corrective expenses",
      "Establishments reporting seed expenses", "Establishments reporting pesticide expenses",
      "Establishments reporting electricity expenses",
      "Establishments reporting machinery/vehicle expenses",
      "Establishments reporting fuel expenses",
      "Fertilizer/corrective share of total expenses", "Seed share of total expenses",
      "Pesticide share of total expenses", "Electricity share of total expenses",
      "Machinery/vehicle share of total expenses", "Fuel share of total expenses",
      "Wage share of total expenses"
    ),
    unit = c(
      "log points", rep("p.p.", 6), "asinh units",
      "p.p.", rep("per 100 est.", 4), rep("p.p.", 13)
    ),
    group = c(
      "Panel C: Agricultural Census, 2017 - adoption and irrigation",
      rep("Panel C: Agricultural Census, 2017 - adoption and irrigation", 7),
      rep("Panel D: Agricultural Census, 2017 - capital and labour", 5),
      rep("Panel E: Agricultural Census, 2017 - expense participation", 6),
      rep("Panel F: Agricultural Census, 2017 - expense composition", 7)
    )
  )
  stopifnot(
    nrow(census_metadata) == 26L,
    all(census_metadata$outcome %in% names(census))
  )
  pair_map <- unique(sfd[, .(pair_id, Code, code_neighbor_west)], by = "pair_id")
  pair_map[, `:=`(
    Code = trimws(as.character(Code)),
    code_neighbor_west = trimws(as.character(code_neighbor_west))
  )]
  census_east <- merge(
    pair_map,
    census[, c("Code", census_metadata$outcome), with = FALSE],
    by = "Code", all = FALSE
  )
  census_west <- census[, c("Code", census_metadata$outcome), with = FALSE]
  setnames(census_west, "Code", "code_neighbor_west")
  setnames(
    census_west,
    census_metadata$outcome,
    paste0(census_metadata$outcome, "_west")
  )
  census_pairs <- merge(census_east, census_west, by = "code_neighbor_west", all = FALSE)
  census_rows <- rbindlist(lapply(seq_len(nrow(census_metadata)), function(i) {
    outcome <- census_metadata$outcome[i]
    difference_row(
      census_pairs[[outcome]],
      census_pairs[[paste0(outcome, "_west")]],
      census_metadata$group[i],
      census_metadata$label[i],
      census_metadata$unit[i]
    )
  }))

  diagnostics <- rbindlist(
    list(main_rows, price_rows),
    use.names = TRUE,
    fill = TRUE
  )
  fwrite(
    diagnostics,
    file.path(paths$out_dir, "FullRevision_SFD_Covariate_Variation.csv")
  )

  note <- paste(
    paste0(
      "Panel A uses the common 1985--2018 sample of ",
      formatC(nrow(sample), format = "d", big.mark = ","),
      " municipality-pair--crop--year observations entering the main log-yield SFD regressions."
    ),
    "Panel B uses the crop-specific complete-data samples of the municipal producer-price regressions; log price is the log of PAM production value divided by production quantity, and the common annual deflator cancels from the spatial difference.",
    "Each SFD is east minus west. Relative SD is the SFD standard deviation divided by sqrt(2) times the standard deviation of the corresponding municipal levels; 100 percent is the benchmark for two independent draws from the same sample.",
    "The pairs are in the same state and main hydrological basin by construction. Salinity is measured in dS/m, elevation in metres, slope and clay in percent, and p.p. denotes percentage points.",
    "Producer-price differences describe local unit values but do not provide a direct measure of market access."
  )
  groups <- unique(diagnostics$group)
  lines <- c(
    "\\begingroup",
    "\\color{red}",
    "\\small",
    "\\setlength{\\tabcolsep}{3pt}",
    "\\renewcommand{\\arraystretch}{0.92}",
    "\\begin{longtable}{p{5.8cm}rrrr}",
    "\\caption{Within-Pair Variation in Salinity and Observed Characteristics}",
    "\\label{tab:full_revision_sfd_covariate_variation}\\\\",
    "\\toprule",
    "Variable & Mean SFD & SD of SFD & Rel. SD (\\%) & N \\\\",
    "\\midrule",
    "\\endfirsthead",
    "\\multicolumn{5}{c}{\\tablename\\ \\thetable{} -- continued}\\\\",
    "\\toprule",
    "Variable & Mean SFD & SD of SFD & Rel. SD (\\%) & N \\\\",
    "\\midrule",
    "\\endhead",
    "\\midrule",
    "\\multicolumn{5}{r}{Continued on next page}\\\\",
    "\\endfoot",
    "\\bottomrule",
    paste0(
      "\\multicolumn{5}{p{14.4cm}}{\\footnotesize\\textit{Notes:} ",
      latex_escape(note),
      "}\\\\"
    ),
    "\\endlastfoot"
  )
  for (group_name in groups) {
    lines <- c(
      lines,
      paste0(
        "\\multicolumn{5}{p{14.4cm}}{\\textit{",
        latex_escape(group_name),
        "}}\\\\"
      )
    )
    group_rows <- diagnostics[group == group_name]
    for (i in seq_len(nrow(group_rows))) {
      row <- group_rows[i]
      lines <- c(
        lines,
        paste(
          latex_escape(row$variable),
          fmt(row$mean_sfd, row$digits),
          fmt(row$sd_sfd, row$digits),
          fmt(100 * row$relative_random_pair_sd, 1),
          formatC(row$observations, format = "d", big.mark = ","),
          sep = " & "
        ),
        "\\\\"
      )
    }
  }
  lines <- c(lines, "\\end{longtable}", "\\endgroup")
  writeLines(
    lines,
    file.path(paths$out_dir, "FullRevision_SFD_Covariate_Variation.tex"),
    useBytes = TRUE
  )
  invisible(diagnostics)
}

# =============================================================================
# 11. Reproducible execution entry points
# =============================================================================
# Each entry point regenerates one family of outputs. The command-line switch
# at the end of this file selects exactly one entry point per R session.
run_full_revision <- function() {
  ensure_output_dir()
  try_set_french_locale()
  load_required_packages()
  data.table::setDTthreads(percent = 75)
  fixest::setFixest_notes(FALSE)

  copy_salinity_exposure_diagnostics_table()
  copy_aquaculture_area_table()
  desc <- make_descriptive_tables()
  national_yield_trends <- make_pam_panel_yield_trends(desc$pam)
  crop_context <- make_crop_context_statistics(desc$pam)
  raw_salinity_figure <- make_raw_mean_salinity_bin_figure()
  raw_salinity_table <- make_raw_mean_salinity_bin_table()
  historical_adaptation <- make_historical_salinity_adaptation_analysis(desc$pam)
  supervisor_output <- make_supervisor_output_standardized_analysis()
  output_asinh <- make_output_asinh_comparison(desc$pam)
  zero_criterion <- make_zero_criterion_asinh_robustness(desc$pam)
  zero_figures <- make_zero_criterion_coefficient_figures(desc$pam)
  farm_size_heterogeneity <- make_farm_size_heterogeneity_analysis()
  incumbent_crops <- make_incumbent_crop_asinh_change_analysis(desc$pam)
  output_distributed_lag <- make_output_distributed_lag_analysis(incumbent_crops)
  output_conley <- write_output_conley_robustness(incumbent_crops)
  soybean_support <- make_soybean_excess_support_analysis(incumbent_crops)
  incumbent_area_mechanisms <- make_incumbent_planted_area_mechanism_analysis(incumbent_crops)
  crop_mix <- make_excess_salinity_crop_mix_analysis(desc$pam)
  crop_mix_diagnostics <- make_crop_mix_bias_diagnostics(crop_mix)
  corn_season_diagnostics <- make_corn_season_crop_mix_diagnostic(crop_mix)
  make_ibge_scope_figure_readable()
  make_figure1_readable()
  make_pair_restriction_diagnostics()
  price <- make_price_analysis()
  land_prepared <- make_mapbiomas_cropland_panel(desc$pam)
  aggregate_cropland <- make_aggregate_cropland_analysis(prepared = land_prepared)
  cropland_exit <- make_cropland_exit_decomposition(prepared = land_prepared)
  irrigation_inputs <- make_irrigation_sfd_analysis()
  inputs_2017 <- make_2017_input_sfd_analysis()
  inputs_panel <- make_input_panel_analysis()
  make_sfd_covariate_variation_diagnostics()
  geo2_file <- file.path(paths$out_dir, "FullRevision_GEO2_Year_Environment.csv")
  geo2_env <- if (file.exists(geo2_file)) fread(geo2_file) else data.table()
  if (!all(c("state", "lon", "lat") %in% names(geo2_env))) {
    geo2_env <- make_geo2_year_environment()
  }
  exposure_pre <- make_exposure_windows(geo2_env, "pre_clean")
  exposure_during <- make_exposure_windows(geo2_env, "during")
  exposure_start <- make_migration_start_exposure(geo2_env)
  current_file <- file.path(paths$out_dir, "FullRevision_IPUMS_GEO2_CurrentRegion_Stats.csv")
  origins_file <- file.path(paths$out_dir, "FullRevision_IPUMS_RuralOrigin_Stats.csv")
  flows_file <- file.path(paths$out_dir, "FullRevision_IPUMS_RuralOrigin_Flows.csv")
  aggregates <- NULL
  if (all(file.exists(c(current_file, origins_file, flows_file)))) {
    cached_current <- fread(current_file)
    cached_origins <- fread(origins_file)
    if (all(c(
      "ag_employment_rate", "nonag_employment_rate",
      "corn_worker_share", "rice_worker_share", "cassava_worker_share",
      "soy_worker_share", "sugarcane_worker_share"
    ) %in% names(cached_current)) &&
        "origin_population_prior_census" %in% names(cached_origins)) {
      aggregates <- list(
        current_region = cached_current,
        origin_stats = cached_origins,
        flows = fread(flows_file)
      )
    }
  }
  if (is.null(aggregates)) aggregates <- extract_ipums_rural_aggregates()

  migration_panel <- build_rural_migration_panel(aggregates, exposure_during)
  two_step_migration <- estimate_two_step_structural_migration(
    migration_panel,
    exposure_start,
    exposure_during
  )

  rural_migration <- estimate_rural_migration(
    migration_panel,
    out_stub = "FullRevision_RuralMigration_PPML_IntervalMeanSalinity_1991_2010",
    desc_stub = "FullRevision_RuralMigration_Descriptive_Stats",
    title = "Mean Salinity and Migration Rates from Rural Origins",
    label = "tab:full_revision_rural_migration_ppml",
    exposure_note = "Exposure is mean salinity during the observed migration interval: 1986--1990 for 1991, 1995--1999 for 2000 and 2005--2009 for 2010, standardized within census year."
  )
  origin_outmigration <- estimate_fixed_baseline_rural_outmigration(
    build_fixed_baseline_rural_outmigration_panel(aggregates, exposure_during),
    out_stub = "FullRevision_RuralOrigin_TotalOutmigration_PPML_1991_2010",
    desc_stub = "FullRevision_RuralOrigin_TotalOutmigration_Descriptives",
    title = "Salinity and Total Outmigration from Baseline-Rural Origins",
    label = "tab:full_revision_rural_origin_total_outmigration",
    exposure_note = "Exposure is mean salinity during the observed migration interval: 1986--1990 for 1991, 1995--1999 for 2000 and 2005--2009 for 2010, standardized within census year."
  )
  origin_outmigration_pre <- estimate_fixed_baseline_rural_outmigration(
    build_fixed_baseline_rural_outmigration_panel(aggregates, exposure_pre),
    out_stub = "FullRevision_RuralOrigin_TotalOutmigration_PreWindow_PPML_2000_2010",
    desc_stub = "FullRevision_RuralOrigin_TotalOutmigration_PreWindow_Descriptives",
    title = "Pre-Interval Salinity and Total Outmigration from Baseline-Rural Origins",
    label = "tab:full_revision_rural_origin_total_outmigration_pre",
    exposure_note = "Exposure is mean salinity before the migration interval: 1990--1994 for 2000 and 2000--2004 for 2010, standardized within census year."
  )
  ag_labor <- make_ag_labor_share_analysis(aggregates, exposure_during)
  ag_labor_sfd_fd <- make_ag_labor_share_sfd_fd_analysis(aggregates, exposure_during)
  ind_crop_migration <- make_ind_crop_migration_analysis(migration_panel)
  ind_crop_labor <- make_ind_crop_labor_analysis(aggregates, exposure_during)
  run_appendix_format_revision()

  write_status("Full thesis pipeline completed.")
  invisible(list(
    descriptives = desc,
    national_yield_trends = national_yield_trends,
    crop_context = crop_context,
    raw_salinity_figure = raw_salinity_figure,
    raw_salinity_table = raw_salinity_table,
    historical_adaptation = historical_adaptation,
    supervisor_output = supervisor_output,
    output_asinh = output_asinh,
    zero_criterion = zero_criterion,
    zero_figures = zero_figures,
    farm_size_heterogeneity = farm_size_heterogeneity,
    incumbent_crops = incumbent_crops,
    output_distributed_lag = output_distributed_lag,
    output_conley = output_conley,
    soybean_support = soybean_support,
    incumbent_area_mechanisms = incumbent_area_mechanisms,
    crop_mix = crop_mix,
    crop_mix_diagnostics = crop_mix_diagnostics,
    corn_season_diagnostics = corn_season_diagnostics,
    price = price,
    aggregate_cropland = aggregate_cropland,
    cropland_exit = cropland_exit,
    irrigation_inputs = irrigation_inputs,
    inputs_2017 = inputs_2017,
    inputs_panel = inputs_panel,
    two_step_migration = two_step_migration,
    rural_migration = rural_migration,
    origin_outmigration = origin_outmigration,
    origin_outmigration_pre = origin_outmigration_pre,
    ag_labor = ag_labor,
    ag_labor_sfd_fd = ag_labor_sfd_fd,
    ind_crop_migration = ind_crop_migration,
    ind_crop_labor = ind_crop_labor
  ))
}

run_ind_crop_revision <- function() {
  ensure_output_dir()
  try_set_french_locale()
  load_required_packages()
  data.table::setDTthreads(percent = 75)
  fixest::setFixest_notes(FALSE)

  geo2_file <- file.path(paths$out_dir, "FullRevision_GEO2_Year_Environment.csv")
  if (!file.exists(geo2_file)) stop("Missing cached GEO2 environmental panel: ", geo2_file)
  exposure_during <- make_exposure_windows(fread(geo2_file), "during")
  current_file <- file.path(paths$out_dir, "FullRevision_IPUMS_GEO2_CurrentRegion_Stats.csv")
  origins_file <- file.path(paths$out_dir, "FullRevision_IPUMS_RuralOrigin_Stats.csv")
  flows_file <- file.path(paths$out_dir, "FullRevision_IPUMS_RuralOrigin_Flows.csv")
  required_current <- c(
    "corn_worker_share", "rice_worker_share", "cassava_worker_share",
    "soy_worker_share", "sugarcane_worker_share"
  )
  required_flows <- paste0(
    "current_", c("corn", "rice", "cassava", "soy", "sugarcane"),
    "_migrant_flow"
  )
  aggregates <- NULL
  if (all(file.exists(c(current_file, origins_file, flows_file)))) {
    current <- fread(current_file)
    flows <- fread(flows_file)
    if (all(required_current %in% names(current)) && all(required_flows %in% names(flows))) {
      aggregates <- list(
        current_region = current,
        origin_stats = fread(origins_file),
        flows = flows
      )
    }
  }
  if (is.null(aggregates)) aggregates <- extract_ipums_rural_aggregates()
  panel <- build_rural_migration_panel(aggregates, exposure_during)
  migration <- make_ind_crop_migration_analysis(panel)
  labor <- make_ind_crop_labor_analysis(aggregates, exposure_during)
  write_status("IND crop migration and employment pipeline completed.")
  invisible(list(migration = migration, labor = labor))
}

write_split_three_crop_panel_table <- function(
    input_file,
    output_file,
    title,
    label
) {
  lines <- readLines(input_file, warn = FALSE, encoding = "UTF-8")
  tab_start <- grep("^\\\\begin\\{tabular\\}", trimws(lines))
  tab_end <- grep("^\\\\end\\{tabular\\}", trimws(lines))
  note_start <- grep("\\textit{Notes:}", trimws(lines), fixed = TRUE)
  if (length(tab_start) != 1L || length(tab_end) != 1L || length(note_start) != 1L) {
    stop("Unexpected LaTeX table structure in ", input_file)
  }

  body <- lines[(tab_start + 1L):(tab_end - 1L)]
  note <- paste(trimws(lines[note_start:(length(lines) - 1L)]), collapse = " ")
  note <- trimws(sub("\\textit{Notes:}", "", note, fixed = TRUE))

  select_cells <- function(line, selected_columns) {
    cleaned <- trimws(line)
    if (endsWith(cleaned, "\\\\")) {
      cleaned <- substr(cleaned, 1L, nchar(cleaned) - 2L)
    }
    cells <- trimws(strsplit(cleaned, "&", fixed = TRUE)[[1L]])
    if (length(cells) != 19L) {
      stop("Expected 19 cells in generated crop-table row: ", line)
    }
    paste0("      ", paste(cells[c(1L, selected_columns)], collapse = " & "), " \\\\")
  }

  make_panel <- function(crop_names, selected_columns, panel_label) {
    panel <- character()
    for (line in body) {
      stripped <- trimws(line)
      if (startsWith(stripped, "Crop &")) {
        crop_header <- paste(
          vapply(crop_names, function(x) paste0("\\multicolumn{3}{c}{", x, "}"), character(1L)),
          collapse = " & "
        )
        panel <- c(panel, paste0("      Crop & ", crop_header, " \\\\"))
      } else if (grepl("&", stripped, fixed = TRUE)) {
        panel <- c(panel, select_cells(line, selected_columns))
      } else {
        panel <- c(panel, line)
      }
    }
    c(
      paste0("\\multicolumn{10}{l}{\\textit{", panel_label, "}} \\\\"),
      panel
    )
  }

  panel_a <- make_panel(c("Corn", "Rice", "Cassava"), 2L:10L, "Panel A: Corn, rice, and cassava")
  panel_b <- make_panel(c("Beans", "Soybeans", "Sugarcane"), 11L:19L, "Panel B: Beans, soybeans, and sugarcane")
  output <- c(
    "\\begin{table}[!htbp]",
    paste0("  \\caption{", title, "}"),
    paste0("  \\label{", label, "}"),
    "  \\centering",
    "  \\small",
    "  \\setlength{\\tabcolsep}{3pt}",
    "  \\renewcommand{\\arraystretch}{0.92}",
    "  \\resizebox{\\linewidth}{!}{%",
    "  \\begin{tabular}{lccccccccc}",
    panel_a,
    "  \\end{tabular}",
    "  }",
    "\\end{table}",
    "\\clearpage",
    "\\begin{table}[!htbp]",
    "  \\centering",
    "  \\small",
    paste0("  \\textit{Table~\\ref{", label, "} (continued)}\\\\[0.5em]"),
    "  \\setlength{\\tabcolsep}{3pt}",
    "  \\renewcommand{\\arraystretch}{0.92}",
    "  \\resizebox{\\linewidth}{!}{%",
    "  \\begin{tabular}{lccccccccc}",
    panel_b,
    "  \\end{tabular}",
    "  }",
    "  \\par\\raggedright\\footnotesize",
    paste0("  \\textit{Notes:} ", note),
    "\\end{table}"
  )
  writeLines(output, output_file, useBytes = TRUE)
  invisible(output_file)
}

strip_landscape_wrapper <- function(tex_file) {
  if (!file.exists(tex_file)) return(invisible(NULL))
  lines <- readLines(tex_file, warn = FALSE, encoding = "UTF-8")
  wrapper <- grepl(
    "^[[:space:]]*\\\\(begin|end)\\{landscape\\}[[:space:]]*$",
    lines
  )
  if (any(wrapper)) writeLines(lines[!wrapper], tex_file, useBytes = TRUE)
  invisible(tex_file)
}

fix_landscape_table_floats <- function(tex_file) {
  if (!file.exists(tex_file)) return(invisible(NULL))
  lines <- readLines(tex_file, warn = FALSE, encoding = "UTF-8")
  table_start <- grepl(
    "^[[:space:]]*\\\\begin\\{table\\}\\[[^]]*\\][[:space:]]*$",
    lines
  )
  if (any(table_start)) {
    lines[table_start] <- "\\begin{table}[H]"
    writeLines(lines, tex_file, useBytes = TRUE)
  }
  invisible(tex_file)
}

run_appendix_format_revision <- function() {
  ensure_output_dir()
  write_split_three_crop_panel_table(
    input_file = file.path(paths$project_dir, "MT2", "crop_yields_sfd.tex"),
    output_file = file.path(paths$out_dir, "FullRevision_Crop_Yields_Excess_Split.tex"),
    title = "Crop Yields and Excess Salinity (Spatial Conley Correction, 200 km)",
    label = "tab:sfd_crop_yield_excess_fao_conley"
  )
  write_split_three_crop_panel_table(
    input_file = file.path(paths$project_dir, "MT2", "crop_yields_mean_salinity_sfd.tex"),
    output_file = file.path(paths$out_dir, "FullRevision_Crop_Yields_Mean_Split.tex"),
    title = "Crop Yields and Mean Soil Salinity (Spatial Conley Correction, 200 km)",
    label = "tab:sfd_crop_yield_mean_salinity_conley"
  )
  write_split_three_crop_panel_table(
    input_file = file.path(paths$project_dir, "MT2", "area_planted.tex"),
    output_file = file.path(paths$out_dir, "FullRevision_Area_Planted_Split.tex"),
    title = "Impact of Lagged Soil Salinity on Crop Log Planted Area (Spatial Conley Correction, 200 km)",
    label = "tab:sfd_planted_area_conley"
  )
  write_split_three_crop_panel_table(
    input_file = file.path(paths$project_dir, "MT2", "share_planted_area.tex"),
    output_file = file.path(paths$out_dir, "FullRevision_Share_Planted_Area_Split.tex"),
    title = "Impact of Lagged Soil Salinity on Crop Share of Planted Area (Spatial Conley Correction, 200 km)",
    label = "tab:sfd_share_planted_area_conley"
  )
  landscape_outputs <- c(
    "FullRevision_Crop_Yields_Excess_Split.tex",
    "FullRevision_Crop_Yields_Mean_Split.tex",
    paste0(
      "FullRevision_NS_Yields_",
      rep(c("excess", "mean"), each = 2L),
      "_Panel",
      rep(c("A", "B"), times = 2L),
      ".tex"
    ),
    "FullRevision_Output_SupervisorSpec_Standardized.tex",
    "FullRevision_Output_Log_Asinh_Comparison.tex",
    "FullRevision_Output_Conley_PairCluster_Comparison.tex",
    "FullRevision_Soybean_Treatment_Support.tex",
    "FullRevision_Price_Effects_ExcessSalinity_PanelA.tex",
    "FullRevision_Price_Effects_ExcessSalinity_PanelB.tex",
    "FullRevision_Incumbent_PlantedArea_AsinhChange.tex"
  )
  invisible(lapply(
    file.path(paths$out_dir, landscape_outputs),
    strip_landscape_wrapper
  ))
  invisible(lapply(
    file.path(paths$out_dir, landscape_outputs),
    fix_landscape_table_floats
  ))
  fix_landscape_table_floats(file.path(
    paths$project_dir,
    "MT2",
    "conley_radius_robustness_main_yields.tex"
  ))
  write_status("Readable split-panel crop appendix tables completed.")
}

run_incumbent_crop_revision <- function() {
  ensure_output_dir()
  try_set_french_locale()
  load_required_packages()
  data.table::setDTthreads(percent = 75)
  fixest::setFixest_notes(FALSE)

  incumbent_crops <- make_incumbent_crop_asinh_change_analysis()
  output_distributed_lag <- make_output_distributed_lag_analysis(incumbent_crops)
  output_conley <- write_output_conley_robustness(incumbent_crops)
  soybean_support <- make_soybean_excess_support_analysis(incumbent_crops)
  incumbent_area_mechanisms <- make_incumbent_planted_area_mechanism_analysis(incumbent_crops)
  write_status("Incumbent-crop adjustment pipeline completed.")
  invisible(list(
    incumbent_crops = incumbent_crops,
    output_distributed_lag = output_distributed_lag,
    output_conley = output_conley,
    soybean_support = soybean_support,
    planted_area_mechanisms = incumbent_area_mechanisms
  ))
}

run_crop_mix_revision <- function() {
  ensure_output_dir()
  try_set_french_locale()
  load_required_packages()
  data.table::setDTthreads(percent = 75)
  fixest::setFixest_notes(FALSE)

  crop_mix <- make_excess_salinity_crop_mix_analysis()
  crop_mix_diagnostics <- make_crop_mix_bias_diagnostics(crop_mix)
  corn_season_diagnostics <- make_corn_season_crop_mix_diagnostic(
    crop_mix,
    force_download = "--force-download" %in% commandArgs(trailingOnly = TRUE)
  )
  run_warnings <- warnings()
  if (!is.null(run_warnings)) print(run_warnings)
  write_status("Crop-mix revision pipeline completed.")
  invisible(list(
    crop_mix = crop_mix,
    diagnostics = crop_mix_diagnostics,
    corn_seasons = corn_season_diagnostics
  ))
}

run_inputs_2017_revision <- function() {
  ensure_output_dir()
  try_set_french_locale()
  load_required_packages()
  data.table::setDTthreads(percent = 75)
  fixest::setFixest_notes(FALSE)

  inputs_2017 <- make_2017_input_sfd_analysis(force_download = "--force-download" %in% commandArgs(trailingOnly = TRUE))
  write_status("2017 inputs-only revision pipeline completed.")
  invisible(inputs_2017)
}

run_inputs_panel_revision <- function() {
  ensure_output_dir()
  try_set_french_locale()
  load_required_packages()
  data.table::setDTthreads(percent = 75)
  fixest::setFixest_notes(FALSE)

  inputs_panel <- make_input_panel_analysis(
    force_download = "--force-download" %in% commandArgs(trailingOnly = TRUE)
  )
  run_warnings <- warnings()
  if (!is.null(run_warnings)) print(run_warnings)
  write_status("2006--2017 inputs-panel revision pipeline completed.")
  invisible(inputs_panel)
}

run_land_use_revision <- function() {
  ensure_output_dir()
  try_set_french_locale()
  load_required_packages()
  data.table::setDTthreads(percent = 75)
  fixest::setFixest_notes(FALSE)

  pam <- read_pam_zeros()
  copy_aquaculture_area_table()
  prepared <- make_mapbiomas_cropland_panel(pam)
  aggregate_cropland <- make_aggregate_cropland_analysis(prepared = prepared)
  cropland_exit <- make_cropland_exit_decomposition(prepared = prepared)
  run_warnings <- warnings()
  if (!is.null(run_warnings)) print(run_warnings)
  write_status("All-crop land-use revision pipeline completed.")
  invisible(list(
    prepared = prepared,
    aggregate_cropland = aggregate_cropland,
    cropland_exit = cropland_exit
  ))
}

run_migration_revision <- function() {
  ensure_output_dir()
  try_set_french_locale()
  load_required_packages()
  data.table::setDTthreads(percent = 75)
  fixest::setFixest_notes(FALSE)

  geo2_file <- file.path(paths$out_dir, "FullRevision_GEO2_Year_Environment.csv")
  geo2_env <- if (file.exists(geo2_file)) fread(geo2_file) else data.table()
  if (!all(c("state", "lon", "lat") %in% names(geo2_env))) {
    geo2_env <- make_geo2_year_environment()
  }
  exposure_pre <- make_exposure_windows(geo2_env, "pre_clean")
  exposure_during <- make_exposure_windows(geo2_env, "during")
  exposure_start <- make_migration_start_exposure(geo2_env)
  current_file <- file.path(paths$out_dir, "FullRevision_IPUMS_GEO2_CurrentRegion_Stats.csv")
  origins_file <- file.path(paths$out_dir, "FullRevision_IPUMS_RuralOrigin_Stats.csv")
  flows_file <- file.path(paths$out_dir, "FullRevision_IPUMS_RuralOrigin_Flows.csv")
  required_current <- c(
    "ag_employment_rate", "nonag_employment_rate",
    "corn_worker_share", "rice_worker_share", "cassava_worker_share",
    "soy_worker_share", "sugarcane_worker_share"
  )
  required_flows <- paste0(
    "current_", c("corn", "rice", "cassava", "soy", "sugarcane"),
    "_migrant_flow"
  )
  aggregates <- NULL
  if (all(file.exists(c(current_file, origins_file, flows_file)))) {
    cached_current <- fread(current_file)
    cached_origins <- fread(origins_file)
    cached_flows <- fread(flows_file)
    if (all(required_current %in% names(cached_current)) &&
        all(required_flows %in% names(cached_flows)) &&
        "origin_population_prior_census" %in% names(cached_origins)) {
      aggregates <- list(
        current_region = cached_current,
        origin_stats = cached_origins,
        flows = cached_flows
      )
    }
  }
  if (is.null(aggregates)) aggregates <- extract_ipums_rural_aggregates()

  migration_panel <- build_rural_migration_panel(aggregates, exposure_during)
  two_step_migration <- estimate_two_step_structural_migration(
    migration_panel,
    exposure_start,
    exposure_during
  )

  rural_migration <- estimate_rural_migration(
    migration_panel,
    out_stub = "FullRevision_RuralMigration_PPML_IntervalMeanSalinity_1991_2010",
    desc_stub = "FullRevision_RuralMigration_Descriptive_Stats",
    title = "Mean Salinity and Migration Rates from Rural Origins",
    label = "tab:full_revision_rural_migration_ppml",
    exposure_note = "Exposure is mean salinity during the observed migration interval: 1986--1990 for 1991, 1995--1999 for 2000 and 2005--2009 for 2010, standardized within census year."
  )

  origin_outmigration <- estimate_fixed_baseline_rural_outmigration(
    build_fixed_baseline_rural_outmigration_panel(aggregates, exposure_during),
    out_stub = "FullRevision_RuralOrigin_TotalOutmigration_PPML_1991_2010",
    desc_stub = "FullRevision_RuralOrigin_TotalOutmigration_Descriptives",
    title = "Salinity and Total Outmigration from Baseline-Rural Origins",
    label = "tab:full_revision_rural_origin_total_outmigration",
    exposure_note = "Exposure is mean salinity during the observed migration interval: 1986--1990 for 1991, 1995--1999 for 2000 and 2005--2009 for 2010, standardized within census year."
  )
  origin_outmigration_pre <- estimate_fixed_baseline_rural_outmigration(
    build_fixed_baseline_rural_outmigration_panel(aggregates, exposure_pre),
    out_stub = "FullRevision_RuralOrigin_TotalOutmigration_PreWindow_PPML_2000_2010",
    desc_stub = "FullRevision_RuralOrigin_TotalOutmigration_PreWindow_Descriptives",
    title = "Pre-Interval Salinity and Total Outmigration from Baseline-Rural Origins",
    label = "tab:full_revision_rural_origin_total_outmigration_pre",
    exposure_note = "Exposure is mean salinity before the observed migration interval: 1990--1994 for 2000 and 2000--2004 for 2010, standardized within census year."
  )
  ag_labor <- make_ag_labor_share_analysis(aggregates, exposure_during)
  ag_labor_sfd_fd <- make_ag_labor_share_sfd_fd_analysis(aggregates, exposure_during)
  ind_crop_migration <- make_ind_crop_migration_analysis(migration_panel)
  ind_crop_labor <- make_ind_crop_labor_analysis(aggregates, exposure_during)
  write_status("Migration-only revision pipeline completed.")
  invisible(list(
    two_step_migration = two_step_migration,
    rural_migration = rural_migration,
    origin_outmigration = origin_outmigration,
    origin_outmigration_pre = origin_outmigration_pre,
    ag_labor = ag_labor,
    ag_labor_sfd_fd = ag_labor_sfd_fd,
    ind_crop_migration = ind_crop_migration,
    ind_crop_labor = ind_crop_labor
  ))
}

run_rural_migration_revision <- function() {
  ensure_output_dir()
  try_set_french_locale()
  load_required_packages()
  data.table::setDTthreads(percent = 75)
  fixest::setFixest_notes(FALSE)

  geo2_file <- file.path(paths$out_dir, "FullRevision_GEO2_Year_Environment.csv")
  if (!file.exists(geo2_file)) {
    stop("Missing cached GEO2 environmental panel: ", geo2_file)
  }
  geo2_env <- fread(geo2_file)
  if (!all(c("state", "lon", "lat") %in% names(geo2_env))) {
    stop("The cached GEO2 environmental panel lacks state or coordinates.")
  }
  exposure_during <- make_exposure_windows(geo2_env, "during")
  current_file <- file.path(paths$out_dir, "FullRevision_IPUMS_GEO2_CurrentRegion_Stats.csv")
  origins_file <- file.path(paths$out_dir, "FullRevision_IPUMS_RuralOrigin_Stats.csv")
  flows_file <- file.path(paths$out_dir, "FullRevision_IPUMS_RuralOrigin_Flows.csv")
  if (!all(file.exists(c(current_file, origins_file, flows_file)))) {
    stop("Missing cached IPUMS migration aggregates; run --migration-only first.")
  }
  aggregates <- list(
    current_region = fread(current_file),
    origin_stats = fread(origins_file),
    flows = fread(flows_file)
  )
  result <- estimate_rural_migration(
    build_rural_migration_panel(aggregates, exposure_during),
    out_stub = "FullRevision_RuralMigration_PPML_IntervalMeanSalinity_1991_2010",
    desc_stub = "FullRevision_RuralMigration_Descriptive_Stats",
    title = "Mean Salinity and Migration Rates from Rural Origins",
    label = "tab:full_revision_rural_migration_ppml",
    exposure_note = "Exposure is mean salinity during the observed migration interval: 1986--1990 for 1991, 1995--1999 for 2000 and 2005--2009 for 2010, standardized within census year."
  )
  write_status("Rural-migration-only revision pipeline completed.")
  invisible(result)
}

run_structural_migration_revision <- function() {
  ensure_output_dir()
  try_set_french_locale()
  load_required_packages()
  data.table::setDTthreads(percent = 75)
  fixest::setFixest_notes(FALSE)

  geo2_file <- file.path(paths$out_dir, "FullRevision_GEO2_Year_Environment.csv")
  if (!file.exists(geo2_file)) {
    stop("Missing cached GEO2 environmental panel: ", geo2_file)
  }
  geo2_env <- fread(geo2_file)
  if (!all(c("state", "lon", "lat") %in% names(geo2_env))) {
    stop("The cached GEO2 environmental panel lacks state or coordinates.")
  }
  exposure_start <- make_migration_start_exposure(geo2_env)
  exposure_during <- make_exposure_windows(geo2_env, "during")

  current_file <- file.path(paths$out_dir, "FullRevision_IPUMS_GEO2_CurrentRegion_Stats.csv")
  origins_file <- file.path(paths$out_dir, "FullRevision_IPUMS_RuralOrigin_Stats.csv")
  flows_file <- file.path(paths$out_dir, "FullRevision_IPUMS_RuralOrigin_Flows.csv")
  aggregates <- NULL
  if (all(file.exists(c(current_file, origins_file, flows_file)))) {
    cached_origins <- fread(origins_file)
    if ("origin_population_prior_census" %in% names(cached_origins)) {
      aggregates <- list(
        current_region = fread(current_file),
        origin_stats = cached_origins,
        flows = fread(flows_file)
      )
    }
  }
  if (is.null(aggregates)) aggregates <- extract_ipums_rural_aggregates()

  migration_panel <- build_rural_migration_panel(aggregates, exposure_during)
  result <- estimate_two_step_structural_migration(
    migration_panel,
    exposure_start,
    exposure_during
  )
  write_status("Structural two-step migration analysis completed.")
  invisible(result)
}

write_migration_compatibility_aliases <- function() {
  # Keep old generated filenames from earlier drafts from carrying obsolete controls.
  aliases <- list(
    c("FullRevision_RuralMigration_Descriptive_Stats.tex", "FullRevision_RuralMigration_DuringWindow_Descriptive_Stats.tex"),
    c("FullRevision_RuralMigration_PPML_IntervalMeanSalinity_1991_2010.tex", "FullRevision_RuralMigration_PPML_DuringMeanSalinity_1991_2010.tex"),
    c("FullRevision_RuralMigration_PPML_PreWindowMeanSalinity_2000_2010.tex", "FullRevision_RuralMigration_PPML_PreLaggedMeanSalinity.tex")
  )
  for (pair in aliases) {
    src <- file.path(paths$out_dir, pair[1])
    dst <- file.path(paths$out_dir, pair[2])
    if (file.exists(src)) file.copy(src, dst, overwrite = TRUE)
  }
  invisible(TRUE)
}

run_figures_revision <- function() {
  ensure_output_dir()
  try_set_french_locale()
  load_required_packages()
  data.table::setDTthreads(percent = 75)
  fixest::setFixest_notes(FALSE)

  pam <- read_pam_zeros()
  make_zero_criterion_coefficient_figures(pam)
  make_figure1_readable()
  make_ibge_scope_figure_readable()
  write_status("Figures-only revision pipeline completed.")
  invisible(TRUE)
}

run_farm_size_heterogeneity_revision <- function() {
  ensure_output_dir()
  try_set_french_locale()
  load_required_packages()
  data.table::setDTthreads(percent = 75)
  fixest::setFixest_notes(FALSE)

  result <- make_farm_size_heterogeneity_analysis(
    force_download = "--force-download" %in% commandArgs(trailingOnly = TRUE)
  )
  run_warnings <- warnings()
  if (!is.null(run_warnings)) print(run_warnings)
  invisible(result)
}

run_price_revision <- function() {
  ensure_output_dir()
  try_set_french_locale()
  load_required_packages()
  data.table::setDTthreads(percent = 75)
  fixest::setFixest_notes(FALSE)

  result <- make_price_analysis()
  run_warnings <- warnings()
  if (!is.null(run_warnings)) print(run_warnings)
  write_status("Price-only revision pipeline completed.")
  invisible(result)
}

run_historical_adaptation_revision <- function() {
  ensure_output_dir()
  try_set_french_locale()
  load_required_packages()
  data.table::setDTthreads(percent = 75)
  fixest::setFixest_notes(FALSE)

  pam <- read_pam_zeros()
  raw_salinity_figure <- make_raw_mean_salinity_bin_figure()
  historical_adaptation <- make_historical_salinity_adaptation_analysis(pam)
  run_warnings <- warnings()
  if (!is.null(run_warnings)) print(run_warnings)
  write_status("Historical-exposure adaptation analysis completed.")
  invisible(list(
    raw_salinity_figure = raw_salinity_figure,
    historical_adaptation = historical_adaptation
  ))
}

run_historical_adaptation_cached_revision <- function() {
  ensure_output_dir()
  try_set_french_locale()
  load_required_packages()
  data.table::setDTthreads(percent = 75)
  fixest::setFixest_notes(FALSE)

  historical_adaptation <- make_historical_salinity_adaptation_analysis(
    reuse_stage_one = TRUE
  )
  run_warnings <- warnings()
  if (!is.null(run_warnings)) print(run_warnings)
  write_status("Cached historical-exposure adaptation analysis completed.")
  invisible(historical_adaptation)
}

run_supervisor_output_revision <- function() {
  ensure_output_dir()
  try_set_french_locale()
  load_required_packages()
  data.table::setDTthreads(percent = 75)
  fixest::setFixest_notes(FALSE)

  result <- make_supervisor_output_standardized_analysis()
  asinh_comparison <- make_output_asinh_comparison()
  run_warnings <- warnings()
  if (!is.null(run_warnings)) print(run_warnings)
  write_status("Supervisor-version output specification completed.")
  invisible(list(log_output = result, asinh_comparison = asinh_comparison))
}

run_north_south_revision <- function() {
  ensure_output_dir()
  try_set_french_locale()
  load_required_packages()
  data.table::setDTthreads(percent = 75)
  fixest::setFixest_notes(FALSE)

  result <- make_north_south_yield_robustness()
  run_warnings <- warnings()
  if (!is.null(run_warnings)) print(run_warnings)
  write_status("North--south yield robustness tables completed.")
  invisible(result)
}

run_finalization_revision <- function() {
  ensure_output_dir()
  try_set_french_locale()
  load_required_packages()
  data.table::setDTthreads(percent = 75)
  fixest::setFixest_notes(FALSE)

  desc <- make_descriptive_tables()
  yield_trends <- make_pam_panel_yield_trends(desc$pam)
  crop_context <- make_crop_context_statistics(desc$pam)
  raw_salinity_figure <- make_raw_mean_salinity_bin_figure()
  raw_salinity_table <- make_raw_mean_salinity_bin_table()
  historical_adaptation <- make_historical_salinity_adaptation_analysis(desc$pam)
  run_warnings <- warnings()
  if (!is.null(run_warnings)) print(run_warnings)
  write_status("Finalization descriptive diagnostics completed.")
  invisible(list(
    descriptives = desc,
    yield_trends = yield_trends,
    crop_context = crop_context,
    raw_salinity_figure = raw_salinity_figure,
    raw_salinity_table = raw_salinity_table,
    historical_adaptation = historical_adaptation
  ))
}

# This check is read-only with respect to analytical data. It reports package
# versions and verifies that every expected input path is available.
check_environment <- function() {
  ensure_user_library()
  locale <- try_set_french_locale()
  pkg_status <- data.table::data.table(
    package = required_packages,
    available = vapply(required_packages, requireNamespace, logical(1), quietly = TRUE),
    version = vapply(required_packages, function(p) {
      if (requireNamespace(p, quietly = TRUE)) as.character(utils::packageVersion(p)) else NA_character_
    }, character(1))
  )
  input_status <- data.table::data.table(
    input = names(paths),
    path = unlist(paths, use.names = FALSE),
    exists = file.exists(unlist(paths, use.names = FALSE))
  )
  print(data.table::data.table(
    R_version = R.version.string,
    R_home = R.home(),
    Rscript = file.path(R.home("bin"), "Rscript.exe"),
    locale = locale
  ))
  print(pkg_status)
  print(input_status)
}

source(
  file.path(PROJECT_DIR, "MIGRATION_BRUNEL_LIU.R"),
  encoding = "UTF-8"
)

args <- commandArgs(trailingOnly = TRUE)
ensure_output_dir()

if ("--run" %in% args) {
  run_full_revision()
} else if ("--incumbent-crops-only" %in% args) {
  run_incumbent_crop_revision()
} else if ("--crop-mix-only" %in% args) {
  run_crop_mix_revision()
} else if ("--migration-only" %in% args) {
  run_brunel_liu_migration_revision()
} else if ("--rural-migration-only" %in% args) {
  run_rural_migration_revision()
} else if ("--structural-migration-only" %in% args) {
  run_structural_migration_revision()
} else if ("--ind-crop-only" %in% args) {
  run_ind_crop_revision()
} else if ("--appendix-format-only" %in% args) {
  run_appendix_format_revision()
} else if ("--inputs-2017-only" %in% args) {
  run_inputs_2017_revision()
} else if ("--inputs-panel-only" %in% args) {
  run_inputs_panel_revision()
} else if ("--land-use-only" %in% args) {
  run_land_use_revision()
} else if ("--figures-only" %in% args) {
  run_figures_revision()
} else if ("--farm-size-heterogeneity-only" %in% args) {
  run_farm_size_heterogeneity_revision()
} else if ("--price-only" %in% args) {
  run_price_revision()
} else if ("--historical-adaptation-only" %in% args) {
  run_historical_adaptation_revision()
} else if ("--historical-adaptation-cached-only" %in% args) {
  run_historical_adaptation_cached_revision()
} else if ("--supervisor-output-only" %in% args) {
  run_supervisor_output_revision()
} else if ("--north-south-only" %in% args) {
  run_north_south_revision()
} else if ("--pair-variation-only" %in% args) {
  try_set_french_locale()
  load_required_packages()
  make_sfd_covariate_variation_diagnostics()
} else if ("--finalization-only" %in% args) {
  run_finalization_revision()
} else if ("--check-only" %in% args || length(args) == 0L) {
  check_environment()
  cat("\nNo analysis was run. Use --run to generate the full thesis outputs.\n")
} else {
  stop("Unknown argument(s): ", paste(args, collapse = ", "))
}
