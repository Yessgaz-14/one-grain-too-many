# Municipal migration flows following the timing logic of Brunel and Liu (2025).
# This module is sourced by thesis_analysis.R and writes only to
# MT2/thesis_outputs.
#
# Required restricted input:
#   Data/IPUMS/ipumsi_00004.parquet
# The IPUMS microdata are intentionally not distributed with the public code.
# Run this module through:
#   Rscript --vanilla thesis_analysis.R --migration-only

# =============================================================================
# 1. Migration windows and predetermined environmental exposure
# =============================================================================
brunel_liu_window_map <- function() {
  data.table::data.table(
    YEAR = c("1981-1985", "1986-1990", "2001-2005", "2006-2010"),
    census_year = c("1991", "1991", "2010", "2010"),
    duration_min = c(5L, 0L, 5L, 0L),
    duration_max = c(9L, 4L, 9L, 4L),
    window_start = c(1981L, 1986L, 2001L, 2006L),
    window_end = c(1985L, 1990L, 2005L, 2010L),
    baseline_year = c("1980", "1980", "2000", "2000"),
    exposure_year = c(NA_integer_, 1985L, 2000L, 2005L),
    estimation_window = c(FALSE, TRUE, TRUE, TRUE)
  )
}

make_brunel_liu_migration_exposure <- function(geo2_year) {
  geo2_year <- data.table::copy(geo2_year)
  geo2_year[, `:=`(
    GEO2_BR = trimws(as.character(GEO2_BR)),
    Year = as.integer(Year)
  )]
  windows <- brunel_liu_window_map()[estimation_window == TRUE]
  keep <- intersect(
    c(
      "GEO2_BR", "Year", "mean_salinity", "excess_above_fao_large", "gdd_large", "kdd_large",
      "sm_season_large", "state", "lon", "lat"
    ),
    names(geo2_year)
  )
  out <- merge(
    windows,
    geo2_year[, ..keep],
    by.x = "exposure_year",
    by.y = "Year",
    all.x = TRUE,
    allow.cartesian = TRUE
  )
  required <- c(
    "mean_salinity", "excess_above_fao_large", "gdd_large", "kdd_large",
    "sm_season_large"
  )
  missing <- setdiff(required, names(out))
  if (length(missing) > 0L) {
    stop("Missing municipal migration exposure variables: ", paste(missing, collapse = ", "))
  }
  if (anyDuplicated(out[, .(GEO2_BR, YEAR)])) {
    stop("Municipal migration exposure is not unique by geography and window.")
  }
  incomplete <- out[, !stats::complete.cases(.SD), .SDcols = required]
  excluded_cells <- sum(incomplete)
  if (excluded_cells > 0L) out <- out[!incomplete]
  balanced_regions <- out[, .N, by = GEO2_BR][N == data.table::uniqueN(windows$YEAR), GEO2_BR]
  excluded_unbalanced <- data.table::uniqueN(out$GEO2_BR) - length(balanced_regions)
  out <- out[GEO2_BR %in% balanced_regions]
  if (excluded_cells > 0L || excluded_unbalanced > 0L) {
    warning(
      "Migration exposure excludes ", excluded_cells,
      " incomplete geography-window cells and ", excluded_unbalanced,
      " geographies without complete support in all estimated windows."
    )
  }
  scale_vars <- required
  out[, paste0("z_", scale_vars) := lapply(.SD, scale_safe), by = YEAR, .SDcols = scale_vars]
  data.table::setorder(out, window_start, GEO2_BR)
  data.table::fwrite(
    out,
    file.path(paths$out_dir, "FullRevision_BrunelLiu_MunicipalMigration_Exposure.csv")
  )
  out[]
}

# =============================================================================
# 2. Weighted individual records aggregated into bilateral municipal flows
# =============================================================================
extract_brunel_liu_municipal_flows <- function() {
  write_status("Constructing municipal last-move flows with the Brunel--Liu timing rule.")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  p <- normalizePath(paths$ipums_parquet, winslash = "/", mustWork = TRUE)
  invalid <- "('','0','99999999','999999999','76097997')"

  baseline_sql <- sprintf(
    "
    WITH base AS (
      SELECT
        CAST(YEAR AS VARCHAR) AS baseline_year,
        TRIM(CAST(GEO2_BR AS VARCHAR)) AS region,
        CAST(PERWT AS DOUBLE) AS weight,
        TRY_CAST(URBAN AS INTEGER) AS urban,
        CASE
          WHEN TRY_CAST(INCTOT AS DOUBLE) < 9999990
          THEN TRY_CAST(INCTOT AS DOUBLE)
          ELSE NULL
        END AS income
      FROM read_parquet('%s')
      WHERE CAST(YEAR AS VARCHAR) IN ('1980','2000')
        AND TRY_CAST(AGE AS INTEGER) >= 15
        AND TRY_CAST(PERWT AS DOUBLE) > 0
        AND GEO2_BR IS NOT NULL
    )
    SELECT
      baseline_year,
      region,
      SUM(weight) AS origin_population,
      SUM(CASE WHEN urban = 1 THEN weight ELSE 0 END) / NULLIF(SUM(weight), 0) AS rural_share,
      SUM(CASE WHEN income IS NOT NULL THEN income * weight ELSE 0 END) /
        NULLIF(SUM(CASE WHEN income IS NOT NULL THEN weight ELSE 0 END), 0) AS mean_income
    FROM base
    WHERE region NOT IN %s
    GROUP BY baseline_year, region
    ",
    p, invalid
  )
  baseline <- data.table::as.data.table(DBI::dbGetQuery(con, baseline_sql))

  flow_sql <- sprintf(
    "
    WITH persons AS (
      SELECT
        CAST(YEAR AS VARCHAR) AS census_year,
        CASE
          WHEN CAST(YEAR AS VARCHAR) = '1991' AND TRY_CAST(MIGYRS1 AS INTEGER) BETWEEN 5 AND 9 THEN '1981-1985'
          WHEN CAST(YEAR AS VARCHAR) = '1991' AND TRY_CAST(MIGYRS1 AS INTEGER) BETWEEN 0 AND 4 THEN '1986-1990'
          WHEN CAST(YEAR AS VARCHAR) = '2010' AND TRY_CAST(MIGYRS1 AS INTEGER) BETWEEN 5 AND 9 THEN '2001-2005'
          WHEN CAST(YEAR AS VARCHAR) = '2010' AND TRY_CAST(MIGYRS1 AS INTEGER) BETWEEN 0 AND 4 THEN '2006-2010'
          ELSE NULL
        END AS YEAR,
        TRIM(CAST(MIG2_P_BR AS VARCHAR)) AS orig,
        TRIM(CAST(GEO2_BR AS VARCHAR)) AS dest,
        TRY_CAST(MIGYRS1 AS INTEGER) AS years_in_current_municipality,
        CAST(PERWT AS DOUBLE) AS weight,
        TRY_CAST(INDGEN AS INTEGER) AS indgen,
        TRY_CAST(IND AS BIGINT) AS ind
      FROM read_parquet('%s')
      WHERE CAST(YEAR AS VARCHAR) IN ('1991','2010')
        AND TRY_CAST(AGE AS INTEGER) >= 15
        AND TRY_CAST(PERWT AS DOUBLE) > 0
        AND MIG2_P_BR IS NOT NULL
        AND GEO2_BR IS NOT NULL
    ),
    valid AS (
      SELECT *
      FROM persons
      WHERE YEAR IS NOT NULL
        AND orig NOT IN %s
        AND dest NOT IN %s
        AND orig <> dest
    )
    SELECT
      YEAR,
      census_year,
      orig,
      dest,
      COUNT(*) AS sampled_migrants,
      SUM(weight) AS migrant_flow,
      SUM(CASE WHEN indgen = 10 THEN weight ELSE 0 END) AS current_ag_migrant_flow,
      SUM(CASE WHEN (census_year = '1991' AND ind = 20) OR (census_year = '2010' AND ind = 1102) THEN weight ELSE 0 END) AS current_corn_migrant_flow,
      SUM(CASE WHEN (census_year = '1991' AND ind = 13) OR (census_year = '2010' AND ind = 1101) THEN weight ELSE 0 END) AS current_rice_migrant_flow,
      SUM(CASE WHEN (census_year = '1991' AND ind = 19) OR (census_year = '2010' AND ind = 1108) THEN weight ELSE 0 END) AS current_cassava_migrant_flow,
      SUM(CASE WHEN (census_year = '1991' AND ind = 21) OR (census_year = '2010' AND ind = 1107) THEN weight ELSE 0 END) AS current_soy_migrant_flow,
      SUM(CASE WHEN (census_year = '1991' AND ind = 17) OR (census_year = '2010' AND ind = 1105) THEN weight ELSE 0 END) AS current_sugarcane_migrant_flow
    FROM valid
    GROUP BY YEAR, census_year, orig, dest
    ",
    p, invalid, invalid
  )
  flows <- data.table::as.data.table(DBI::dbGetQuery(con, flow_sql))
  windows <- brunel_liu_window_map()
  origin_stats <- merge(
    windows,
    baseline,
    by = "baseline_year",
    all.x = TRUE,
    allow.cartesian = TRUE
  )
  data.table::setnames(origin_stats, "region", "orig")

  if (flows[, any(!is.finite(migrant_flow) | migrant_flow <= 0)]) {
    stop("Non-positive or missing weighted flow in the positive-flow table.")
  }
  if (!identical(sort(unique(flows$YEAR)), sort(windows$YEAR))) {
    stop("The four expected migration windows were not all constructed.")
  }
  if (baseline[, any(!is.finite(origin_population) | origin_population <= 0)]) {
    stop("Invalid baseline population in municipal migration data.")
  }

  data.table::fwrite(
    flows,
    file.path(paths$out_dir, "FullRevision_BrunelLiu_MunicipalMigration_PositiveFlows.csv")
  )
  data.table::fwrite(
    origin_stats,
    file.path(paths$out_dir, "FullRevision_BrunelLiu_MunicipalMigration_OriginStats.csv")
  )
  list(flows = flows, origin_stats = origin_stats, windows = windows)
}

# =============================================================================
# 3. Balanced dyad-window panel, rural-origin restriction, and zero flows
# =============================================================================
build_brunel_liu_municipal_panel <- function(
    aggregates,
    exposure,
    rural_threshold = 0.5,
    write_cache = TRUE) {
  if (!is.numeric(rural_threshold) || length(rural_threshold) != 1L ||
      !is.finite(rural_threshold) || rural_threshold < 0 || rural_threshold > 1) {
    stop("rural_threshold must be one number between zero and one.")
  }
  flows <- data.table::copy(aggregates$flows)
  origins <- data.table::copy(aggregates$origin_stats)
  exposure <- data.table::copy(exposure)
  for (d in list(flows, origins, exposure)) {
    d[, YEAR := as.character(YEAR)]
  }
  flows[, `:=`(orig = trimws(as.character(orig)), dest = trimws(as.character(dest)))]
  origins[, orig := trimws(as.character(orig))]
  exposure[, GEO2_BR := trimws(as.character(GEO2_BR))]

  origins <- origins[
    estimation_window == TRUE &
      is.finite(rural_share) & rural_share >= rural_threshold &
      is.finite(origin_population) & origin_population > 0
  ]
  estimation_periods <- sort(unique(exposure$YEAR))
  exposure_regions <- unique(exposure$GEO2_BR)
  origins <- origins[YEAR %in% estimation_periods & orig %in% exposure_regions]
  origin_codes <- sort(unique(origins$orig))
  destination_codes <- sort(exposure_regions)
  if (length(origin_codes) == 0L) {
    stop("No origin satisfies the requested rurality threshold.")
  }

  positive <- flows[
    YEAR %in% estimation_periods &
      orig %in% origin_codes &
      dest %in% destination_codes &
      orig != dest &
      is.finite(migrant_flow) & migrant_flow > 0
  ]
  informative_dyads <- unique(positive[, .(orig, dest)])
  if (nrow(informative_dyads) == 0L) {
    stop("No positive migration dyad remains at the requested rurality threshold.")
  }
  panel <- informative_dyads[, .(YEAR = estimation_periods), by = .(orig, dest)]
  panel <- merge(panel, flows, by = c("YEAR", "orig", "dest"), all.x = TRUE)
  flow_columns <- grep("migrant_flow$", names(panel), value = TRUE)
  for (column in flow_columns) {
    data.table::set(panel, which(is.na(panel[[column]])), column, 0)
  }
  panel[is.na(sampled_migrants), sampled_migrants := 0]
  panel <- merge(
    panel,
    origins[, .(
      YEAR, orig, census_year, baseline_year, window_start, window_end,
      origin_population, origin_rural_share_baseline = rural_share,
      origin_mean_income = mean_income
    )],
    by = c("YEAR", "orig"),
    all = FALSE
  )

  exposure_columns <- setdiff(
    names(exposure),
    c(
      "GEO2_BR", "YEAR", "census_year", "duration_min", "duration_max",
      "window_start", "window_end", "baseline_year", "exposure_year",
      "estimation_window"
    )
  )
  exposure_keep <- c("YEAR", "GEO2_BR", exposure_columns)
  origin_exposure <- data.table::copy(exposure[, ..exposure_keep])
  destination_exposure <- data.table::copy(exposure[, ..exposure_keep])
  data.table::setnames(origin_exposure, "GEO2_BR", "orig")
  data.table::setnames(destination_exposure, "GEO2_BR", "dest")
  data.table::setnames(origin_exposure, exposure_columns, paste0(exposure_columns, "_orig"))
  data.table::setnames(destination_exposure, exposure_columns, paste0(exposure_columns, "_dest"))
  panel <- merge(panel, origin_exposure, by = c("YEAR", "orig"), all.x = TRUE)
  panel <- merge(panel, destination_exposure, by = c("YEAR", "dest"), all.x = TRUE)

  panel[, `:=`(
    dyad = paste(orig, dest, sep = "__"),
    state_orig = as.character(state_orig),
    state_dest = as.character(state_dest),
    migration_rate = migrant_flow / origin_population
  )]
  required <- c(
    "migrant_flow", "origin_population", "z_mean_salinity_orig",
    "z_mean_salinity_dest", "state_orig", "state_dest"
  )
  missing_required <- vapply(
    panel[, ..required],
    function(x) sum(is.na(x) | (is.character(x) & !nzchar(x))),
    integer(1)
  )
  if (any(missing_required > 0L)) {
    stop(
      "Missing required values after constructing the municipal migration panel: ",
      paste(names(missing_required)[missing_required > 0L], missing_required[missing_required > 0L], collapse = ", ")
    )
  }
  attr(panel, "potential_rows") <-
    length(estimation_periods) *
    (length(origin_codes) * length(destination_codes) - length(intersect(origin_codes, destination_codes)))
  attr(panel, "informative_dyads_pre_filter") <- nrow(informative_dyads)
  attr(panel, "rural_threshold") <- rural_threshold
  data.table::setorder(panel, YEAR, orig, dest)
  if (isTRUE(write_cache)) {
    data.table::fwrite(
      panel,
      file.path(paths$out_dir, "FullRevision_BrunelLiu_MunicipalMigration_Panel.csv")
    )
  }
  panel[]
}

# =============================================================================
# 4. Descriptive outputs and migration timeline
# =============================================================================
write_brunel_liu_window_table <- function(aggregates, panel) {
  flows <- data.table::copy(aggregates$flows)
  origins <- aggregates$origin_stats[
    is.finite(rural_share) & rural_share >= 0.5,
    .(YEAR, orig)
  ]
  rural_flows <- merge(flows, origins, by = c("YEAR", "orig"), all = FALSE)
  windows <- brunel_liu_window_map()
  tab <- rural_flows[, .(
    `Sampled migrants` = sum(sampled_migrants),
    `Weighted migrant flow` = round(sum(migrant_flow)),
    `Positive corridors` = .N,
    Origins = data.table::uniqueN(orig),
    Destinations = data.table::uniqueN(dest)
  ), by = YEAR]
  tab <- merge(
    windows[, .(
      YEAR,
      Census = census_year,
      `Residence duration` = paste0(duration_min, "--", duration_max, " years"),
      `Main regression` = ifelse(estimation_window, "Yes", "No")
    )],
    tab,
    by = "YEAR",
    all.x = TRUE
  )
  data.table::setnames(tab, "YEAR", "Migration window")
  tab[, `Weighted migrant flow` := formatC(
    as.numeric(`Weighted migrant flow`), format = "f", digits = 0, big.mark = ","
  )]
  for (column in c("Sampled migrants", "Positive corridors", "Origins", "Destinations")) {
    tab[, (column) := formatC(as.numeric(get(column)), format = "d", big.mark = ",")]
  }
  write_latex_df(
    tab,
    file.path(paths$out_dir, "FullRevision_BrunelLiu_MunicipalMigration_Windows.tex"),
    "Construction of Municipal Migration Flows",
    "tab:full_revision_brunel_liu_migration_windows",
    note = paste(
      "Flows sum census person weights for people aged 15 or older whose last municipality differs from their municipality at the census.",
      "Origins are restricted to harmonized areas whose weighted rural population share was at least 50 percent in the baseline census.",
      "The 1981--1985 flow is constructed and reported but excluded from the regression because the salinity maps begin in 1985 and do not provide a predetermined exposure measure.",
      "Both duration bins approximate five-year windows because the census reports completed years in the current municipality rather than an exact move date. The 5--9-year windows may be more affected by return migration and attrition than the 0--4-year windows."
    ),
    size = "\\scriptsize"
  )
  data.table::fwrite(
    tab,
    file.path(paths$out_dir, "FullRevision_BrunelLiu_MunicipalMigration_Windows.csv")
  )
  invisible(tab)
}

make_brunel_liu_timeline <- function() {
  windows <- brunel_liu_window_map()
  windows[, row := ifelse(census_year == "1991", 2.4, 1.25)]
  windows[, label_y := row + ifelse(duration_min == 0L, 0.24, -0.24)]
  exposure <- windows[estimation_window == TRUE]
  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = windows,
      ggplot2::aes(x = window_start, xend = window_end, y = row, yend = row),
      linewidth = 1.1,
      colour = "#222222",
      arrow = grid::arrow(length = grid::unit(0.14, "cm"), type = "closed")
    ) +
    ggplot2::geom_point(
      data = windows,
      ggplot2::aes(x = window_start, y = row),
      size = 2.2,
      colour = "#222222"
    ) +
    ggplot2::geom_text(
      data = windows,
      ggplot2::aes(
        x = (window_start + window_end) / 2,
        y = label_y,
        label = paste0(YEAR, "  (", duration_min, "-", duration_max, " years)")
      ),
      size = 3.9,
      colour = "#222222"
    ) +
    ggplot2::geom_point(
      data = exposure,
      ggplot2::aes(x = exposure_year, y = 0.42),
      shape = 17,
      size = 3.4,
      colour = "#B2182B"
    ) +
    ggplot2::geom_text(
      data = exposure,
      ggplot2::aes(x = exposure_year, y = 0.16, label = exposure_year),
      size = 3.6,
      colour = "#B2182B"
    ) +
    ggplot2::annotate(
      "text", x = 1980.3, y = 0.76,
      label = "1981-1985 flow: descriptive only\n(no earlier salinity map)",
      hjust = 0, size = 3.5, colour = "#6B6B6B"
    ) +
    ggplot2::annotate(
      "text", x = 1993.2, y = 0.76,
      label = "Salinity measured before each estimated window",
      hjust = 0, size = 3.5, colour = "#B2182B"
    ) +
    ggplot2::annotate("point", x = 1991, y = 2.4, size = 3.2, colour = "#2166AC") +
    ggplot2::annotate("point", x = 2010, y = 1.25, size = 3.2, colour = "#2166AC") +
    ggplot2::annotate("text", x = 1991, y = 2.83, label = "1991 census", size = 4.0, colour = "#2166AC") +
    ggplot2::annotate("text", x = 2010, y = 1.68, label = "2010 census", size = 4.0, colour = "#2166AC") +
    ggplot2::scale_x_continuous(
      breaks = c(1980, 1985, 1990, 1995, 2000, 2005, 2010),
      limits = c(1980, 2011),
      expand = ggplot2::expansion(mult = c(0.01, 0.01))
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 3.05), breaks = NULL) +
    ggplot2::labs(
      title = "Construction of Five-Year Municipal Migration Flows",
      x = "Year", y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14),
      axis.title.x = ggplot2::element_text(size = 12),
      axis.text.x = ggplot2::element_text(size = 10, colour = "#222222"),
      plot.margin = ggplot2::margin(8, 14, 8, 14)
    )
  pdf_file <- file.path(paths$out_dir, "FullRevision_BrunelLiu_MunicipalMigration_Timeline.pdf")
  png_file <- file.path(paths$out_dir, "FullRevision_BrunelLiu_MunicipalMigration_Timeline.png")
  ggplot2::ggsave(pdf_file, p, width = 9.2, height = 4.2, device = grDevices::cairo_pdf)
  ggplot2::ggsave(png_file, p, width = 9.2, height = 4.2, dpi = 220)
  invisible(list(plot = p, pdf = pdf_file, png = png_file))
}

# =============================================================================
# 5. PPML gravity estimates and income heterogeneity
# =============================================================================
estimate_brunel_liu_municipal_migration <- function(panel) {
  write_status("Estimating the municipal Brunel--Liu-style PPML migration model.")
  salinity <- c("z_mean_salinity_orig", "z_mean_salinity_dest")
  weather <- c(
    "z_gdd_large_orig", "z_kdd_large_orig", "z_sm_season_large_orig",
    "z_gdd_large_dest", "z_kdd_large_dest", "z_sm_season_large_dest"
  )
  required <- c(
    "migrant_flow", "origin_population", "dyad", "orig", "dest", "YEAR",
    "state_orig", "state_dest", salinity, weather
  )
  d <- complete_data(panel, required)
  d <- d[
    is.finite(migrant_flow) & migrant_flow >= 0 &
      is.finite(origin_population) & origin_population > 0
  ]
  positive_dyads <- d[, .(positive_once = any(migrant_flow > 0)), by = dyad][
    positive_once == TRUE, dyad
  ]
  d <- d[dyad %in% positive_dyads]
  if (data.table::uniqueN(d$YEAR) != 3L) {
    stop("The preferred migration regression must contain three exposure windows.")
  }

  fixed_effects <- "dyad + state_orig^YEAR + state_dest^YEAR"
  preferred_formula <- stats::as.formula(paste0(
    "migrant_flow ~ ", paste(c(salinity, weather), collapse = " + "),
    " | ", fixed_effects
  ))
  preferred_preliminary <- fixest::fepois(
    preferred_formula,
    data = d,
    offset = ~log(origin_population),
    cluster = ~orig + dest,
    glm.iter = 100,
    notes = FALSE
  )
  d <- d[fixest::obs(preferred_preliminary)]
  simple_formula <- stats::as.formula(paste0(
    "migrant_flow ~ ", paste(salinity, collapse = " + "),
    " | ", fixed_effects
  ))
  simple <- fixest::fepois(
    simple_formula,
    data = d,
    offset = ~log(origin_population),
    cluster = ~orig + dest,
    glm.iter = 100,
    notes = FALSE
  )
  preferred <- fixest::fepois(
    preferred_formula,
    data = d,
    offset = ~log(origin_population),
    cluster = ~orig + dest,
    glm.iter = 100,
    notes = FALSE
  )
  if (stats::nobs(simple) != stats::nobs(preferred)) {
    stop("The two migration specifications do not use the same final sample.")
  }

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
  tex_file <- file.path(
    paths$out_dir,
    "FullRevision_BrunelLiu_MunicipalMigration_PPML.tex"
  )
  fixest::etable(
    simple, preferred,
    tex = TRUE,
    file = tex_file,
    replace = TRUE,
    title = "Soil Salinity and Municipal Migration from Rural Origins",
    label = "tab:full_revision_rural_migration_ppml",
    fitstat = ~ n + sq.cor + pr2,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    notes = paste(
      "Notes: PPML estimates of weighted bilateral migration flows for people aged 15 or older.",
      "The flow records the last intermunicipal move and is assigned to a five-year window from the reported duration in the current municipality.",
      "Origins are rural when at least 50 percent of their weighted population lived in rural areas in the baseline census; destinations are unrestricted.",
      "The estimated windows are 1986--1990, 2001--2005 and 2006--2010.",
      "Salinity, GDD, KDD and soil moisture are measured in 1985, 2000 and 2005, respectively, before the corresponding migration window, and standardized within window.",
      "Both columns include dyad, origin-state-by-window and destination-state-by-window fixed effects.",
      "Column 2 is preferred and adds GDD, KDD and soil moisture at origin and destination; precipitation and income are excluded.",
      "The offset is the weighted population aged 15 or older in the origin baseline census.",
      "All columns use the same final sample; standard errors are two-way clustered by origin and destination."
    )
  )
  lines <- readLines(tex_file, warn = FALSE, encoding = "UTF-8")
  table_line <- which(grepl("^\\s*\\\\begin\\{table\\}", lines))[1]
  if (!is.na(table_line) && !any(grepl("\\\\color\\{red\\}", lines))) {
    lines <- append(lines, "\\color{red}", after = table_line)
  }
  lines <- gsub(
    "state\\_orig-YEAR", "Origin-state-by-window fixed effects",
    lines, fixed = TRUE
  )
  lines <- gsub(
    "state\\_dest-YEAR", "Destination-state-by-window fixed effects",
    lines, fixed = TRUE
  )
  dyad_rows <- grepl("^[[:space:]]*dyad[[:space:]]*&", lines)
  lines[dyad_rows] <- sub("dyad", "Dyad fixed effects", lines[dyad_rows], fixed = TRUE)
  centering_line <- which(grepl("^\\s*\\\\centering", lines))[1]
  if (!is.na(centering_line)) {
    lines <- append(
      lines,
      c("\\footnotesize", "\\setlength{\\tabcolsep}{4pt}", "\\renewcommand{\\arraystretch}{0.86}"),
      after = centering_line
    )
  }
  writeLines(lines, tex_file, useBytes = TRUE)

  extract_model <- function(model, specification) {
    estimates <- stats::coef(model)
    standard_errors <- sqrt(diag(stats::vcov(model)))
    p_values <- 2 * stats::pnorm(-abs(estimates / standard_errors))
    data.table::data.table(
      specification = specification,
      term = names(estimates),
      estimate = unname(estimates),
      standard_error = unname(standard_errors),
      p_value = unname(p_values),
      expected_rate_change_percent = 100 * (exp(unname(estimates)) - 1)
    )
  }
  coefficients <- data.table::rbindlist(list(
    extract_model(simple, "Salinity and fixed effects"),
    extract_model(preferred, "Preferred: salinity and climate controls")
  ))
  model_data <- d[fixest::obs(preferred)]
  support <- data.table::data.table(
    metric = c(
      "Regression observations", "Zero-flow observations", "Positive-flow observations",
      "Origin-destination dyads", "Rural origins", "Destinations", "Migration windows",
      "Weighted migrants", "Mean bilateral migration rate"
    ),
    value = c(
      stats::nobs(preferred),
      sum(model_data$migrant_flow == 0),
      sum(model_data$migrant_flow > 0),
      data.table::uniqueN(model_data$dyad),
      data.table::uniqueN(model_data$orig),
      data.table::uniqueN(model_data$dest),
      data.table::uniqueN(model_data$YEAR),
      sum(model_data$migrant_flow),
      weighted_mean_safe(model_data$migration_rate, model_data$origin_population)
    )
  )
  data.table::fwrite(
    coefficients,
    file.path(paths$out_dir, "FullRevision_BrunelLiu_MunicipalMigration_Coefficients.csv")
  )
  data.table::fwrite(
    support,
    file.path(paths$out_dir, "FullRevision_BrunelLiu_MunicipalMigration_Support.csv")
  )
  invisible(list(
    models = list(simple = simple, preferred = preferred),
    coefficients = coefficients,
    support = support,
    data = model_data
  ))
}

# =============================================================================
# 6. Rural-definition and destination-exposure robustness checks
# =============================================================================
estimate_brunel_liu_rurality_threshold_robustness <- function(
    aggregates,
    exposure,
    thresholds = seq(0, 1, by = 0.1)) {
  write_status("Estimating migration robustness across baseline rurality thresholds.")
  weather <- c(
    "z_gdd_large_orig", "z_kdd_large_orig", "z_sm_season_large_orig",
    "z_gdd_large_dest", "z_kdd_large_dest", "z_sm_season_large_dest"
  )
  required <- c(
    "migrant_flow", "origin_population", "dyad", "orig", "dest", "YEAR",
    "state_orig", "state_dest", "z_mean_salinity_orig",
    "z_mean_salinity_dest", weather
  )
  fixed_effects <- "dyad + state_orig^YEAR + state_dest^YEAR"
  formula <- stats::as.formula(paste0(
    "migrant_flow ~ ",
    paste(c("z_mean_salinity_orig", "z_mean_salinity_dest", weather), collapse = " + "),
    " | ", fixed_effects
  ))

  estimate_one <- function(threshold) {
    support <- list(
      observations = NA_integer_, origins = NA_integer_, dyads = NA_integer_,
      zero_flows = NA_integer_
    )
    eligible_origins <- aggregates$origin_stats[
      estimation_window == TRUE & is.finite(rural_share) & rural_share >= threshold,
      data.table::uniqueN(orig)
    ]
    support$origins <- eligible_origins
    tryCatch({
      panel <- build_brunel_liu_municipal_panel(
        aggregates,
        exposure,
        rural_threshold = threshold,
        write_cache = FALSE
      )
      d <- complete_data(panel, required)
      d <- d[
        is.finite(migrant_flow) & migrant_flow >= 0 &
          is.finite(origin_population) & origin_population > 0
      ]
      positive_dyads <- d[, .(positive_once = any(migrant_flow > 0)), by = dyad][
        positive_once == TRUE, dyad
      ]
      d <- d[dyad %in% positive_dyads]
      if (data.table::uniqueN(d$YEAR) != 3L) {
        stop("Fewer than three migration windows remain.")
      }
      support <- list(
        observations = nrow(d),
        origins = data.table::uniqueN(d$orig),
        dyads = data.table::uniqueN(d$dyad),
        zero_flows = sum(d$migrant_flow == 0)
      )
      model <- fixest::fepois(
        formula,
        data = d,
        offset = ~log(origin_population),
        cluster = ~orig + dest,
        glm.iter = 100,
        notes = FALSE
      )
      model_data <- d[fixest::obs(model)]
      coefficients <- stats::coef(model)
      if (!"z_mean_salinity_orig" %in% names(coefficients)) {
        stop("The origin-salinity coefficient is not identified.")
      }
      estimate <- unname(coefficients[["z_mean_salinity_orig"]])
      standard_error <- sqrt(stats::vcov(model)[
        "z_mean_salinity_orig", "z_mean_salinity_orig"
      ])
      p_value <- 2 * stats::pnorm(-abs(estimate / standard_error))
      data.table::data.table(
        rural_threshold = threshold,
        estimate = estimate,
        standard_error = standard_error,
        p_value = p_value,
        expected_rate_change_percent = 100 * (exp(estimate) - 1),
        observations = stats::nobs(model),
        origins = data.table::uniqueN(model_data$orig),
        dyads = data.table::uniqueN(model_data$dyad),
        zero_flows = sum(model_data$migrant_flow == 0),
        status = "Estimated"
      )
    }, error = function(e) {
      data.table::data.table(
        rural_threshold = threshold,
        estimate = NA_real_,
        standard_error = NA_real_,
        p_value = NA_real_,
        expected_rate_change_percent = NA_real_,
        observations = support$observations,
        origins = support$origins,
        dyads = support$dyads,
        zero_flows = support$zero_flows,
        status = conditionMessage(e)
      )
    })
  }

  results <- data.table::rbindlist(lapply(thresholds, estimate_one), fill = TRUE)
  data.table::fwrite(
    results,
    file.path(
      paths$out_dir,
      "FullRevision_BrunelLiu_Migration_RuralityThreshold_Robustness.csv"
    )
  )

  significance <- function(p) {
    if (!is.finite(p)) return("")
    if (p < 0.01) return("***")
    if (p < 0.05) return("**")
    if (p < 0.10) return("*")
    ""
  }
  display <- results[, .(
    `Rural threshold` = paste0(round(100 * rural_threshold), "%"),
    Coefficient = ifelse(
      is.finite(estimate),
      paste0(sprintf("%.4f", estimate), vapply(p_value, significance, character(1))),
      "Not estimated"
    ),
    SE = ifelse(is.finite(standard_error), sprintf("%.4f", standard_error), ""),
    `Rate change` = ifelse(
      is.finite(expected_rate_change_percent),
      paste0(sprintf("%.2f", expected_rate_change_percent), "%"),
      ""
    ),
    Observations = ifelse(
      is.finite(observations), formatC(observations, format = "d", big.mark = ","), ""
    ),
    Origins = ifelse(is.finite(origins), formatC(origins, format = "d", big.mark = ","), ""),
    Dyads = ifelse(is.finite(dyads), formatC(dyads, format = "d", big.mark = ","), ""),
    Inference = ifelse(status == "Estimated", "Estimated", "Not estimable")
  )]
  write_latex_df(
    display,
    file.path(
      paths$out_dir,
      "FullRevision_BrunelLiu_Migration_RuralityThreshold_Robustness.tex"
    ),
    "Migration Robustness Across Baseline Rurality Thresholds",
    "tab:full_revision_migration_rurality_thresholds",
    note = paste(
      "Each row re-estimates the preferred PPML specification on origins whose weighted rural population share in the baseline census is at least the indicated threshold.",
      "The zero-percent row retains all origins, while the 100-percent row retains only fully rural origins.",
      "Every model includes origin and destination salinity, GDD, KDD and soil moisture, dyad fixed effects, origin-state-by-window and destination-state-by-window fixed effects, an origin-population offset, and two-way clustering by origin and destination.",
      "The samples are nested but their dyad composition and fixed-effect support change with the threshold; coefficient differences are therefore descriptive rather than formal equality tests.",
      "At 90 percent, the remaining support is too sparse for the required two-way clustered covariance matrix; no origin has a weighted rural share of exactly 100 percent in the relevant baseline records.",
      "Stars denote significance at the 10, 5 and 1 percent levels."
    ),
    size = "\\scriptsize"
  )
  invisible(results)
}

estimate_brunel_liu_no_destination_salinity_robustness <- function(main_data) {
  write_status("Estimating migration robustness without destination salinity.")
  d <- data.table::copy(main_data)
  weather <- c(
    "z_gdd_large_orig", "z_kdd_large_orig", "z_sm_season_large_orig",
    "z_gdd_large_dest", "z_kdd_large_dest", "z_sm_season_large_dest"
  )
  required <- c(
    "migrant_flow", "origin_population", "dyad", "orig", "dest", "YEAR",
    "state_orig", "state_dest", "z_mean_salinity_orig", weather
  )
  if (!all(required %in% names(d)) || !all(stats::complete.cases(d[, ..required]))) {
    stop("The main migration sample is incomplete for the no-destination-salinity robustness.")
  }
  fixed_effects <- "dyad + state_orig^YEAR + state_dest^YEAR"
  simple_formula <- stats::as.formula(paste0(
    "migrant_flow ~ z_mean_salinity_orig | ", fixed_effects
  ))
  preferred_formula <- stats::as.formula(paste0(
    "migrant_flow ~ z_mean_salinity_orig + ", paste(weather, collapse = " + "),
    " | ", fixed_effects
  ))
  simple <- fixest::fepois(
    simple_formula,
    data = d,
    offset = ~log(origin_population),
    cluster = ~orig + dest,
    glm.iter = 100,
    notes = FALSE
  )
  preferred <- fixest::fepois(
    preferred_formula,
    data = d,
    offset = ~log(origin_population),
    cluster = ~orig + dest,
    glm.iter = 100,
    notes = FALSE
  )
  model_n <- vapply(list(simple, preferred), stats::nobs, integer(1))
  if (any(model_n != nrow(d))) {
    stop(
      "Omitting destination salinity changed the intended common sample: ",
      paste(model_n, collapse = ", "), " versus ", nrow(d), "."
    )
  }

  fixest::setFixest_dict(c(
    migrant_flow = "Migrant flow (origin-population offset)",
    z_mean_salinity_orig = "Origin mean salinity (std.)",
    z_gdd_large_orig = "Origin GDD (std.)",
    z_kdd_large_orig = "Origin KDD (std.)",
    z_sm_season_large_orig = "Origin soil moisture (std.)",
    z_gdd_large_dest = "Destination GDD (std.)",
    z_kdd_large_dest = "Destination KDD (std.)",
    z_sm_season_large_dest = "Destination soil moisture (std.)"
  ), reset = TRUE)
  tex_file <- file.path(
    paths$out_dir,
    "FullRevision_BrunelLiu_Migration_NoDestinationSalinity.tex"
  )
  fixest::etable(
    simple, preferred,
    tex = TRUE,
    file = tex_file,
    replace = TRUE,
    title = "Migration Robustness Without Destination Salinity",
    label = "tab:full_revision_migration_no_destination_salinity",
    fitstat = ~ n + sq.cor + pr2,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    notes = paste(
      "Notes: These PPML models use exactly the final estimation sample of the main migration table.",
      "Destination salinity is omitted, while Column 2 retains GDD, KDD and soil moisture at both origin and destination.",
      "Both columns include dyad, origin-state-by-window and destination-state-by-window fixed effects and the origin-population offset.",
      "Standard errors are two-way clustered by origin and destination."
    )
  )
  lines <- readLines(tex_file, warn = FALSE, encoding = "UTF-8")
  table_line <- which(grepl("^\\s*\\\\begin\\{table\\}", lines))[1]
  if (!is.na(table_line) && !any(grepl("\\\\color\\{red\\}", lines))) {
    lines <- append(lines, "\\color{red}", after = table_line)
  }
  lines <- gsub(
    "state\\_orig-YEAR", "Origin-state-by-window fixed effects",
    lines, fixed = TRUE
  )
  lines <- gsub(
    "state\\_dest-YEAR", "Destination-state-by-window fixed effects",
    lines, fixed = TRUE
  )
  dyad_rows <- grepl("^[[:space:]]*dyad[[:space:]]*&", lines)
  lines[dyad_rows] <- sub("dyad", "Dyad fixed effects", lines[dyad_rows], fixed = TRUE)
  centering_line <- which(grepl("^\\s*\\\\centering", lines))[1]
  if (!is.na(centering_line)) {
    lines <- append(
      lines,
      c("\\footnotesize", "\\setlength{\\tabcolsep}{4pt}", "\\renewcommand{\\arraystretch}{0.86}"),
      after = centering_line
    )
  }
  writeLines(lines, tex_file, useBytes = TRUE)

  extract_model <- function(model, specification) {
    estimates <- stats::coef(model)
    standard_errors <- sqrt(diag(stats::vcov(model)))
    p_values <- 2 * stats::pnorm(-abs(estimates / standard_errors))
    data.table::data.table(
      specification = specification,
      term = names(estimates),
      estimate = unname(estimates),
      standard_error = unname(standard_errors),
      p_value = unname(p_values),
      expected_rate_change_percent = 100 * (exp(unname(estimates)) - 1),
      observations = stats::nobs(model)
    )
  }
  coefficients <- data.table::rbindlist(list(
    extract_model(simple, "Origin salinity and fixed effects"),
    extract_model(preferred, "Preferred controls without destination salinity")
  ))
  data.table::fwrite(
    coefficients,
    file.path(
      paths$out_dir,
      "FullRevision_BrunelLiu_Migration_NoDestinationSalinity.csv"
    )
  )
  invisible(list(models = list(simple = simple, preferred = preferred), coefficients = coefficients))
}

# =============================================================================
# 7. Module entry point
# =============================================================================
run_brunel_liu_migration_revision <- function(load_packages = TRUE) {
  ensure_output_dir()
  try_set_french_locale()
  if (isTRUE(load_packages)) load_required_packages()
  data.table::setDTthreads(percent = 75)
  fixest::setFixest_notes(FALSE)

  geo2_file <- file.path(paths$out_dir, "FullRevision_GEO2_Year_Environment.csv")
  if (!file.exists(geo2_file)) {
    stop("Missing cached GEO2 environmental panel: ", geo2_file)
  }
  geo2_year <- data.table::fread(geo2_file)
  exposure <- make_brunel_liu_migration_exposure(geo2_year)
  aggregates <- extract_brunel_liu_municipal_flows()
  panel <- build_brunel_liu_municipal_panel(aggregates, exposure)
  window_table <- write_brunel_liu_window_table(aggregates, panel)
  timeline <- make_brunel_liu_timeline()
  estimation <- estimate_brunel_liu_municipal_migration(panel)
  rurality_thresholds <- estimate_brunel_liu_rurality_threshold_robustness(
    aggregates,
    exposure
  )
  no_destination_salinity <- estimate_brunel_liu_no_destination_salinity_robustness(
    estimation$data
  )

  income_heterogeneity <- estimate_rural_migration_income_heterogeneity(
    estimation$data,
    c(
      "z_gdd_large_orig", "z_kdd_large_orig", "z_sm_season_large_orig",
      "z_gdd_large_dest", "z_kdd_large_dest", "z_sm_season_large_dest"
    )
  )
  destination_agriculture <- estimate_destination_agricultural_migration(panel)
  ind_crop_migration <- make_ind_crop_migration_analysis(panel)
  write_status("Municipal Brunel--Liu migration analysis completed.")
  invisible(list(
    exposure = exposure,
    aggregates = aggregates,
    panel = panel,
    window_table = window_table,
    timeline = timeline,
    estimation = estimation,
    rurality_thresholds = rurality_thresholds,
    no_destination_salinity = no_destination_salinity,
    income_heterogeneity = income_heterogeneity,
    destination_agriculture = destination_agriculture,
    ind_crop_migration = ind_crop_migration
  ))
}
