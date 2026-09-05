# Conley-radius robustness for the main crop-yield SFD regressions.
# This script creates new appendix outputs and does not overwrite main results.
#
# Input:  Data/PAM_SFD_rigorous_clean_corrected.rds
# Output: Data/conley_radius_robustness_main_yields.csv and a LaTeX table in MT2/.
# Run from the thesis project root with Rscript --vanilla.

# This optional legacy path is used only when it exists; otherwise R keeps the
# active library paths unchanged.
legacy_library <- "C:/Users/yessg/AppData/Local/R/win-library/4.5"
if (dir.exists(legacy_library)) {
  .libPaths(c(legacy_library, .libPaths()))
}

required_packages <- c("fixest")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Missing required packages: ",
    paste(missing_packages, collapse = ", "),
    ". No package was installed by this script."
  )
}

library(fixest)
fixest::setFixest_notes(FALSE)
if ("setFixest_nthreads" %in% getNamespaceExports("fixest")) {
  fixest::setFixest_nthreads(2L)
}

data_dir <- "Data"
out_dir <- "MT2"

sfd_file <- file.path(data_dir, "PAM_SFD_rigorous_clean_corrected.rds")
output_csv <- file.path(data_dir, "conley_radius_robustness_main_yields.csv")
output_tex <- file.path(out_dir, "conley_radius_robustness_main_yields.tex")

cutoffs_km <- c(50, 100, 200, 250, 300)
conley_distance <- "spherical"

crops <- c("corn", "rice", "cassava", "beans", "soy", "sugarcane")
crop_labels <- c(
  corn = "Corn",
  rice = "Rice",
  cassava = "Cassava",
  beans = "Beans",
  soy = "Soybeans",
  sugarcane = "Sugarcane"
)

exposures <- list(
  "Excess salinity above FAO threshold" = "d_excess_above_fao_large",
  "Mean salinity" = "d_mean_salinity"
)

control_specifications <- list(
  "Spec. 1" = character(),
  "Spec. 2" = c(
    "d_gdd_large",
    "d_kdd_large",
    "d_sm_season_large"
  ),
  "Spec. 3" = c(
    "d_gdd_large",
    "d_kdd_large",
    "d_sm_season_large",
    "d_slope_large",
    "d_elevation_large",
    "d_clay_mean_large"
  )
)

full_control_set <- control_specifications[["Spec. 3"]]

is_valid_model_value <- function(x) {
  if (is.numeric(x) || is.integer(x)) {
    return(is.finite(x))
  }
  x_character <- trimws(as.character(x))
  !is.na(x_character) & nzchar(x_character)
}

restrict_to_common_sample <- function(data, outcome, treatment, controls) {
  sample_variables <- unique(
    c(outcome, treatment, controls, "Year", "pair_id", "lat", "lon")
  )
  missing_variables <- setdiff(sample_variables, names(data))
  if (length(missing_variables) > 0L) {
    stop(
      "Variables missing from the estimation sample: ",
      paste(missing_variables, collapse = ", ")
    )
  }

  keep <- rep(TRUE, nrow(data))
  for (variable in sample_variables) {
    keep <- keep & is_valid_model_value(data[[variable]])
  }

  data[keep, , drop = FALSE]
}

extract_radius_result <- function(model, treatment, cutoff_km) {
  vcov_request <- fixest::vcov_conley(
    lat = "lat",
    lon = "lon",
    cutoff = cutoff_km,
    distance = conley_distance,
    vcov_fix = TRUE
  )

  warning_messages <- character()
  coefficient_table <- withCallingHandlers(
    fixest::coeftable(model, vcov = vcov_request),
    warning = function(warning_condition) {
      warning_messages <<- c(
        warning_messages,
        conditionMessage(warning_condition)
      )
      invokeRestart("muffleWarning")
    }
  )
  if (!treatment %in% rownames(coefficient_table)) {
    stop("Treatment was omitted from the model: ", treatment)
  }

  row <- coefficient_table[treatment, , drop = FALSE]
  p_column <- ncol(row)
  vcov_fixed <- any(grepl("not positive semi-definite", warning_messages))

  data.frame(
    cutoff_km = cutoff_km,
    estimate = as.numeric(row[1L, "Estimate"]),
    standard_error = as.numeric(row[1L, "Std. Error"]),
    p_value = as.numeric(row[1L, p_column]),
    vcov_fixed = vcov_fixed,
    vcov_warning = paste(unique(warning_messages), collapse = " | "),
    stringsAsFactors = FALSE
  )
}

stars <- function(p_value) {
  if (!is.finite(p_value)) {
    return("")
  }
  if (p_value < 0.01) {
    return("***")
  }
  if (p_value < 0.05) {
    return("**")
  }
  if (p_value < 0.10) {
    return("*")
  }
  ""
}

format_number <- function(x) {
  if (!is.finite(x)) {
    return("")
  }
  sprintf("%.3f", x)
}

format_standard_error <- function(x) {
  if (!is.finite(x)) {
    return("")
  }
  if (abs(x) > 0 && abs(x) < 0.001) {
    return("$<$0.001")
  }
  format_number(x)
}

format_cell <- function(estimate, standard_error, p_value, vcov_fixed) {
  warning_marker <- if (isTRUE(vcov_fixed)) {
    "$^{\\dagger}$"
  } else {
    ""
  }
  paste0(
    "\\shortstack{",
    format_number(estimate),
    stars(p_value),
    warning_marker,
    "\\\\(",
    format_standard_error(standard_error),
    ")}"
  )
}

format_cutoff_list <- function(cutoffs) {
  cutoff_labels <- paste0(cutoffs, " km")
  if (length(cutoff_labels) == 1L) {
    return(cutoff_labels)
  }
  if (length(cutoff_labels) == 2L) {
    return(paste(cutoff_labels, collapse = " and "))
  }
  paste(
    paste(head(cutoff_labels, -1L), collapse = ", "),
    tail(cutoff_labels, 1L),
    sep = ", and "
  )
}

escape_latex <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("&", "\\\\&", x)
  x <- gsub("%", "\\\\%", x)
  x <- gsub("_", "\\\\_", x)
  x
}

make_latex_table <- function(results, path) {
  table_metadata <- list(
    "Excess salinity above FAO threshold" = list(
      caption = "Conley Radius Robustness for Main Crop-Yield SFD Estimates: Excess Salinity",
      label = "tab:conley_radius_robustness_main_yields_excess"
    ),
    "Mean salinity" = list(
      caption = "Conley Radius Robustness for Main Crop-Yield SFD Estimates: Mean Salinity",
      label = "tab:conley_radius_robustness_main_yields_mean"
    )
  )

  rows <- character()
  cutoff_headers <- paste0(cutoffs_km, " km")
  numeric_columns <- paste(rep("c", length(cutoffs_km) + 1L), collapse = "")
  cutoff_note <- format_cutoff_list(cutoffs_km)

  crop_panels <- list(A = head(crops, 3L), B = tail(crops, 3L))
  for (current_exposure in names(exposures)) {
    metadata <- table_metadata[[current_exposure]]
    exposure_data <- results[results$exposure == current_exposure, ]
    for (panel_name in names(crop_panels)) {
      panel_crops <- crop_panels[[panel_name]]
      panel_label <- if (panel_name == "A") metadata$label else paste0(metadata$label, "_panel_b")
      rows <- c(
        rows,
        "\\begin{table}[H]",
        "\\centering",
        "\\scriptsize",
        "\\renewcommand{\\arraystretch}{0.90}",
        paste0("\\caption{", metadata$caption, ": Panel ", panel_name, "}"),
        paste0("\\label{", panel_label, "}"),
        "\\resizebox{\\linewidth}{!}{%",
        paste0("\\begin{tabular}{ll", numeric_columns, "}"),
        "\\toprule",
        paste(c("Crop", "Spec.", cutoff_headers, "Obs."), collapse = " & "),
        "\\midrule"
      )
      rows[length(rows) - 1L] <- paste0(rows[length(rows) - 1L], " \\\\")

      for (current_crop in panel_crops) {
        crop_data <- exposure_data[exposure_data$crop == current_crop, ]
        for (current_specification in names(control_specifications)) {
          row_data <- crop_data[crop_data$specification == current_specification, ]
          if (nrow(row_data) == 0L) next

          get_cutoff <- function(cutoff_km) {
            current <- row_data[row_data$cutoff_km == cutoff_km, ]
            if (nrow(current) != 1L) stop("Missing cutoff ", cutoff_km, " for table row.")
            format_cell(
              current$estimate,
              current$standard_error,
              current$p_value,
              current$vcov_fixed
            )
          }

          cutoff_cells <- vapply(cutoffs_km, get_cutoff, character(1))
          row_entries <- c(
            escape_latex(crop_labels[[current_crop]]),
            escape_latex(current_specification),
            cutoff_cells,
            format(row_data$observations[1L], big.mark = ",", scientific = FALSE)
          )
          rows <- c(rows, paste(row_entries, collapse = " & "))
          rows[length(rows)] <- paste0(rows[length(rows)], " \\\\")
        }
      }

      rows <- c(
        rows,
        "\\bottomrule",
        "\\end{tabular}%",
        "}",
        "\\par\\raggedright\\scriptsize",
        paste0(
          "\\textit{Notes:} Each cell reports the salinity coefficient and its Conley standard error. ",
          "Coefficients and samples are identical across cutoffs within a row; only inference changes. ",
          "Spec. 1 includes year fixed effects only, preferred Spec. 2 adds GDD, KDD, and soil moisture, and Spec. 3 adds slope, elevation, and clay content. ",
          "Municipality pairs remain contiguous; the radius only governs residual spatial correlation. ",
          "The tested radii are ",
          cutoff_note,
          ". The 300 km column is a wider-radius sensitivity check. ",
          "$^{\\dagger}$ indicates that the Conley variance-covariance matrix was not positive semi-definite and was adjusted by fixest. ",
          "Significance levels: * p$<$0.10, ** p$<$0.05, *** p$<$0.01."
        ),
        "\\end{table}",
        ""
      )
    }
  }

  writeLines(rows, path, useBytes = TRUE)
}

if (!file.exists(sfd_file)) {
  stop("SFD file not found: ", sfd_file)
}

sfd_data <- readRDS(sfd_file)
sfd_data <- as.data.frame(sfd_data)

sfd_data$crop <- trimws(tolower(as.character(sfd_data$crop)))
sfd_data$crop[sfd_data$crop %in% c("soybean", "soybeans")] <- "soy"
sfd_data$crop[sfd_data$crop == "maize"] <- "corn"
sfd_data$crop[
  sfd_data$crop %in% c(
    "sugar cane",
    "sugar-cane",
    "cana-de-açúcar",
    "cana de açúcar",
    "cana-de-acucar",
    "cana de acucar"
  )
] <- "sugarcane"

sfd_data <- sfd_data[sfd_data$crop %in% crops, , drop = FALSE]

climate_fallbacks <- c(
  d_gdd_large = "d_gdd_scaled",
  d_kdd_large = "d_kdd_scaled",
  d_sm_season_large = "d_sm_scaled"
)

for (target_variable in names(climate_fallbacks)) {
  source_variable <- climate_fallbacks[[target_variable]]
  if (
    !target_variable %in% names(sfd_data) &&
      source_variable %in% names(sfd_data)
  ) {
    sfd_data[[target_variable]] <- sfd_data[[source_variable]] * 1000
  }
}

required_variables <- unique(
  c(
    "crop",
    "Year",
    "pair_id",
    "lat",
    "lon",
    "d_log_yield",
    unname(exposures),
    full_control_set
  )
)

missing_variables <- setdiff(required_variables, names(sfd_data))
if (length(missing_variables) > 0L) {
  stop(
    "Variables missing from the SFD file: ",
    paste(missing_variables, collapse = ", ")
  )
}

all_results <- list()
result_index <- 1L

for (exposure_label in names(exposures)) {
  treatment <- exposures[[exposure_label]]

  for (current_crop in crops) {
    crop_data <- sfd_data[sfd_data$crop == current_crop, , drop = FALSE]
    crop_data <- restrict_to_common_sample(
      data = crop_data,
      outcome = "d_log_yield",
      treatment = treatment,
      controls = full_control_set
    )

    if (nrow(crop_data) == 0L) {
      warning("No usable observations for crop: ", current_crop)
      next
    }

    cat(
      exposure_label,
      " | ",
      crop_labels[[current_crop]],
      ": ",
      format(nrow(crop_data), big.mark = ","),
      " common-sample observations.\n",
      sep = ""
    )

    for (current_specification in names(control_specifications)) {
      controls <- control_specifications[[current_specification]]
      rhs <- c(treatment, controls)
      model_formula <- stats::as.formula(
        paste0("d_log_yield ~ ", paste(rhs, collapse = " + "), " | Year")
      )

      model <- fixest::feols(
        model_formula,
        data = crop_data,
        panel.id = ~ pair_id + Year,
        notes = FALSE
      )

      for (cutoff_km in cutoffs_km) {
        radius_result <- extract_radius_result(
          model = model,
          treatment = treatment,
          cutoff_km = cutoff_km
        )

        all_results[[result_index]] <- data.frame(
          exposure = exposure_label,
          crop = current_crop,
          crop_label = crop_labels[[current_crop]],
          specification = current_specification,
          cutoff_km = radius_result$cutoff_km,
          estimate = radius_result$estimate,
          standard_error = radius_result$standard_error,
          p_value = radius_result$p_value,
          vcov_fixed = radius_result$vcov_fixed,
          vcov_warning = radius_result$vcov_warning,
          observations = stats::nobs(model),
          stringsAsFactors = FALSE
        )
        result_index <- result_index + 1L
      }
    }
  }
}

results <- do.call(rbind, all_results)
write.csv(results, output_csv, row.names = FALSE, na = "")
make_latex_table(results, output_tex)

cat("Wrote: ", output_csv, "\n", sep = "")
cat("Wrote: ", output_tex, "\n", sep = "")
cat("Rows: ", nrow(results), "\n", sep = "")
cat("Cutoffs: ", paste(cutoffs_km, collapse = ", "), " km\n", sep = "")
cat("PSD-adjusted Conley cells: ", sum(results$vcov_fixed), "\n", sep = "")
