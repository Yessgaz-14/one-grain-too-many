# Small reproducibility helper for supervisor-response diagnostics.
# It reads already-generated diagnostics and writes new LaTeX outputs without
# replacing existing thesis tables.
# Run from the thesis project root after the SFD panel has been generated.

input_csv <- file.path("Data", "crop_salinity_exposure_diagnostics.csv")
input_sfd <- file.path("Data", "PAM_SFD_rigorous_clean_corrected.rds")
output_tex <- file.path("MT2", "salinity_exposure_diagnostics_supervisor_response.tex")

diagnostics <- read.csv(input_csv, stringsAsFactors = FALSE, check.names = FALSE)

if (!file.exists(input_sfd)) {
  stop("SFD data file not found: ", input_sfd)
}

sfd <- readRDS(input_sfd)
sfd$crop <- trimws(tolower(as.character(sfd$crop)))
sfd$crop[sfd$crop == "maize"] <- "corn"
sfd$crop[sfd$crop %in% c("soybean", "soybeans")] <- "soy"
sfd$crop[sfd$crop %in% c("pulse", "pulses")] <- "beans"
sfd$crop[
  sfd$crop %in% c(
    "sugar cane",
    "sugar-cane",
    "cana-de-acucar",
    "cana de acucar"
  )
] <- "sugarcane"

fmt_num <- function(x, digits = 3) {
  formatC(as.numeric(x), digits = digits, format = "f", big.mark = ",")
}

fmt_pct <- function(x, digits = 1) {
  paste0(formatC(as.numeric(x), digits = digits, format = "f", big.mark = ","), "\\%")
}

valid_value <- function(x) {
  if (is.numeric(x) || is.integer(x)) {
    return(is.finite(x))
  }

  x <- trimws(as.character(x))
  !is.na(x) & nzchar(x)
}

crops <- c("corn", "rice", "cassava", "beans", "soy", "sugarcane")
controls <- c(
  "d_gdd_large",
  "d_kdd_large",
  "d_sm_season_large",
  "d_slope_large",
  "d_elevation_large",
  "d_clay_mean_large"
)

sample_vars <- unique(c(
  "d_log_yield",
  "d_excess_above_fao_large",
  controls,
  "Year",
  "lat",
  "lon"
))

needed_sfd_vars <- unique(c(
  "crop",
  "Code",
  "code_neighbor_west",
  "pair_id",
  sample_vars
))

missing_sfd_vars <- setdiff(needed_sfd_vars, names(sfd))
if (length(missing_sfd_vars) > 0L) {
  stop("Variables missing from SFD data: ", paste(missing_sfd_vars, collapse = ", "))
}

municipality_counts <- lapply(crops, function(cr) {
  d <- sfd[sfd$crop == cr, , drop = FALSE]
  keep <- rep(TRUE, nrow(d))

  for (v in sample_vars) {
    keep <- keep & valid_value(d[[v]])
  }

  d <- d[keep, , drop = FALSE]
  municipalities <- unique(c(as.character(d$Code), as.character(d$code_neighbor_west)))
  municipalities <- municipalities[!is.na(municipalities) & nzchar(trimws(municipalities))]

  data.frame(
    crop = cr,
    unique_municipalities = length(unique(municipalities)),
    stringsAsFactors = FALSE
  )
})

municipality_counts <- do.call(rbind, municipality_counts)
diagnostics <- merge(diagnostics, municipality_counts, by = "crop", all.x = TRUE, sort = FALSE)
diagnostics <- diagnostics[order(match(diagnostics$crop, crops)), ]

rows <- apply(diagnostics, 1, function(r) {
  paste(
    r[["crop_label"]],
    formatC(as.integer(r[["model_observations"]]), format = "d", big.mark = ","),
    formatC(as.integer(r[["unique_municipalities"]]), format = "d", big.mark = ","),
    formatC(as.integer(r[["unique_pairs"]]), format = "d", big.mark = ","),
    fmt_num(r[["mean_raw_salinity"]], 3),
    fmt_pct(r[["share_above_fao_threshold_pct"]], 1),
    sep = " & "
  )
})

tex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Salinity Exposure Diagnostics in the Main SFD Yield Samples}",
  "\\label{tab:salinity_exposure_diagnostics_supervisor_response}",
  "\\small",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lrrrrr}",
  "\\toprule",
  "Crop & Observations & Municipalities & Pairs & Mean ECe (dS/m) & Above FAO \\\\",
  "\\midrule",
  paste0(rows, " \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "}",
  "\\par\\addvspace{0.5ex}",
  "\\parbox{0.95\\textwidth}{\\textit{Notes:} The table summarizes the estimation samples used in the main crop-yield Spatial First-Difference specifications. Observations are municipality-pair-crop-year observations. Municipalities count unique east- and west-side municipalities observed in each crop sample. Mean ECe is reported in dS/m; the source GeoTIFF rasters are stored as centi-dS/m and the processing divides raw values by 100 before constructing salinity measures. ``Above FAO'' is the share of crop-pair-year observations above the crop-specific FAO agronomic threshold.}",
  "\\end{table}"
)

writeLines(tex_lines, output_tex, useBytes = TRUE)
cat("Wrote: ", output_tex, "\n", sep = "")
