# Build a descriptive IBGE/PAM figure showing how large the six thesis crops are
# in Brazil's planted area and crop-production value. Uses base R only.
# The script downloads public aggregate SIDRA data and writes an audit CSV plus
# PDF and PNG figures. Run it from the thesis project root with Rscript --vanilla.

area_files <- file.path(
  "Data",
  "PAM",
  paste0("planted_area", 1:3, ".csv")
)

output_csv <- file.path(
  "Data",
  "ibge_selected_crops_area_value_scope.csv"
)

output_pdf <- file.path(
  "MT2",
  "figure_ibge_selected_crops_area_value_scope.pdf"
)

output_png <- file.path(
  "MT2",
  "figure_ibge_selected_crops_area_value_scope.png"
)

crop_order <- c("corn", "rice", "cassava", "beans", "soy", "sugarcane")

selected_crops <- c(
  corn = "milho_em_grao",
  rice = "arroz_em_casca",
  cassava = "mandioca",
  beans = "feijao_em_grao",
  soy = "soja_em_grao",
  sugarcane = "cana_de_acucar"
)

crop_labels <- c(
  corn = "Corn",
  rice = "Rice",
  cassava = "Cassava",
  beans = "Beans",
  soy = "Soybeans",
  sugarcane = "Sugarcane"
)

crop_colors <- c(
  corn = "#0072B2",
  rice = "#E69F00",
  cassava = "#009E73",
  beans = "#CC4C8A",
  soy = "#D55E00",
  sugarcane = "#6F3CC3"
)

# SIDRA table 5457, variable 215: "Valor da producao".
# Category 0 is total crop production value; selected categories are the six
# thesis crops. Values are current monetary units, but only annual ratios are
# used, so numerator and denominator are in the same currency in each year.
sidra_value_url <- paste0(
  "https://apisidra.ibge.gov.br/values/",
  "t/5457/n1/all/v/215/p/1988-2018/",
  "c782/0,40102,40106,40112,40119,40122,40124",
  "?formato=csv"
)

sidra_value_categories <- c(
  corn = "40122",
  rice = "40102",
  cassava = "40119",
  beans = "40112",
  soy = "40124",
  sugarcane = "40106"
)

clean_name <- function(x) {
  x_ascii <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x_ascii[is.na(x_ascii)] <- x[is.na(x_ascii)]
  x_ascii <- tolower(x_ascii)
  x_ascii <- gsub("[^a-z0-9]+", "_", x_ascii)
  x_ascii <- gsub("^_|_$", "", x_ascii)
  x_ascii
}

parse_sidra_number <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "...", "..", "X", "x")] <- NA_character_
  x[x == "-"] <- "0"
  suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))
}

sum_or_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  sum(x, na.rm = TRUE)
}

read_area_file <- function(path) {
  if (!file.exists(path)) {
    stop("Missing IBGE/PAM area file: ", path)
  }

  d <- read.csv(
    path,
    skip = 3,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("...", "..", "X", "x", ""),
    comment.char = ""
  )
  names(d) <- clean_name(names(d))

  required <- c("cod", "municipio", "ano", "total", unname(selected_crops))
  missing <- setdiff(required, names(d))
  if (length(missing) > 0L) {
    stop(
      "Missing expected columns in ",
      path,
      ": ",
      paste(missing, collapse = ", ")
    )
  }

  d$code <- as.character(d$cod)
  d$year <- suppressWarnings(as.integer(d$ano))

  keep <- grepl("^[0-9]+$", d$code) & is.finite(d$year)
  d <- d[keep, c("code", "year", "total", unname(selected_crops)), drop = FALSE]

  value_columns <- setdiff(names(d), c("code", "year"))
  for (v in value_columns) {
    d[[v]] <- parse_sidra_number(d[[v]])
  }

  d
}

read_sidra_value_csv <- function(url) {
  lines <- readLines(url, encoding = "UTF-8", warn = FALSE)
  if (length(lines) < 3L) {
    stop("SIDRA value query returned too few lines.")
  }

  d <- read.csv(
    text = paste(lines[-2L], collapse = "\n"),
    sep = ";",
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("...", "..", "X", "x", "")
  )

  required <- c("D3C", "D4C", "D4N", "MN", "V")
  missing <- setdiff(required, names(d))
  if (length(missing) > 0L) {
    stop(
      "Missing expected SIDRA API columns: ",
      paste(missing, collapse = ", ")
    )
  }

  d$year <- suppressWarnings(as.integer(d$D3C))
  d$category <- as.character(d$D4C)
  d$value_current_thousand <- parse_sidra_number(d$V)
  d
}

build_value_yearly <- function(years) {
  raw_value <- read_sidra_value_csv(sidra_value_url)
  raw_value <- raw_value[
    raw_value$year %in% years &
      raw_value$category %in% c("0", unname(sidra_value_categories)),
  ]

  total_value <- raw_value[raw_value$category == "0", ]
  if (nrow(total_value) != length(years)) {
    stop("Could not retrieve one total production-value row for each year.")
  }

  total_value <- total_value[match(years, total_value$year), ]

  value_yearly <- data.frame(
    year = years,
    total_crop_value_current_thousand = total_value$value_current_thousand,
    value_unit = total_value$MN,
    stringsAsFactors = FALSE
  )

  for (crop in crop_order) {
    d <- raw_value[raw_value$category == sidra_value_categories[[crop]], ]
    d <- d[match(years, d$year), ]
    if (nrow(d) != length(years) || any(d$year != years)) {
      stop("Could not retrieve one production-value row for each year: ", crop)
    }
    value_yearly[[paste0(crop, "_value_current_thousand")]] <-
      d$value_current_thousand
  }

  value_cols <- paste0(crop_order, "_value_current_thousand")
  value_yearly$selected_value_current_thousand <- rowSums(
    value_yearly[, value_cols, drop = FALSE],
    na.rm = TRUE
  )
  value_yearly$selected_value_share_pct <-
    100 * value_yearly$selected_value_current_thousand /
    value_yearly$total_crop_value_current_thousand

  for (crop in crop_order) {
    value_yearly[[paste0(crop, "_value_share_pct")]] <-
      100 * value_yearly[[paste0(crop, "_value_current_thousand")]] /
      value_yearly$total_crop_value_current_thousand
  }

  value_yearly
}

draw_y_grid <- function(y_values) {
  abline(h = y_values, col = "grey88", lty = "dotted", lwd = 0.8)
}

draw_stacked_area <- function(x, mat, colors) {
  cumulative <- apply(mat, 2, cumsum)
  lower <- rbind(0, cumulative[-nrow(cumulative), , drop = FALSE])

  for (i in seq_len(nrow(mat))) {
    polygon(
      c(x, rev(x)),
      c(cumulative[i, ], rev(lower[i, ])),
      col = colors[rownames(mat)[i]],
      border = NA
    )
  }

  invisible(cumulative)
}

plot_scope_figure <- function(yearly, file, device = c("pdf", "png")) {
  device <- match.arg(device)

  if (device == "pdf") {
    pdf(file, width = 9.2, height = 6.6, useDingbats = FALSE)
  } else {
    png(file, width = 2024, height = 1452, res = 220)
  }
  on.exit(dev.off(), add = TRUE)

  years <- yearly$year
  area_cols <- paste0(crop_order, "_area_mha")
  area_matrix <- t(as.matrix(yearly[, area_cols, drop = FALSE]))
  rownames(area_matrix) <- crop_order

  value_share_cols <- paste0(crop_order, "_value_share_pct")
  value_share_matrix <- t(as.matrix(yearly[, value_share_cols, drop = FALSE]))
  rownames(value_share_matrix) <- crop_order

  layout(
    matrix(c(1, 2, 3, 3, 4, 4), nrow = 3, byrow = TRUE),
    heights = c(1, 1, 0.18)
  )

  old_par <- par(
    family = "sans",
    bg = "white",
    cex.axis = 0.78,
    cex.lab = 0.86,
    cex.main = 0.90,
    las = 1,
    xaxs = "i",
    yaxs = "i"
  )
  on.exit(par(old_par), add = TRUE)

  x_ticks <- c(1988, 1995, 2000, 2005, 2010, 2015, 2018)

  par(mar = c(3.6, 4.3, 2.0, 0.9))
  plot(
    years,
    yearly$selected_share_pct,
    type = "n",
    ylim = c(0, 100),
    xaxt = "n",
    xlab = "",
    ylab = "Share of temporary-crop planted area (%)",
    main = "A. Coverage in planted area",
    bty = "l"
  )
  draw_y_grid(seq(0, 100, by = 20))
  polygon(
    c(years, rev(years)),
    c(yearly$selected_share_pct, rep(0, length(years))),
    col = grDevices::adjustcolor("#0072B2", alpha.f = 0.16),
    border = NA
  )
  lines(years, yearly$selected_share_pct, lwd = 1.4, col = "#0072B2")
  points(years, yearly$selected_share_pct, pch = 16, cex = 0.42, col = "#0072B2")
  abline(
    h = yearly$mean_selected_share_pct[1],
    lty = "dashed",
    lwd = 0.8,
    col = "grey35"
  )
  text(
    x = 1989,
    y = yearly$mean_selected_share_pct[1] + 5.0,
    labels = sprintf("Mean: %.1f%%", yearly$mean_selected_share_pct[1]),
    adj = c(0, 0.5),
    cex = 0.72,
    col = "grey25"
  )
  axis(1, at = x_ticks)

  par(mar = c(3.6, 4.3, 2.0, 0.9))
  area_max <- max(yearly$selected_area_mha, na.rm = TRUE)
  area_limit <- ceiling(area_max / 10) * 10
  area_ticks <- pretty(c(0, area_limit), n = 5)

  plot(
    range(years),
    c(0, area_limit),
    type = "n",
    xaxt = "n",
    xlab = "",
    ylab = "Planted area (million ha)",
    main = "B. Selected planted area by crop",
    bty = "l"
  )
  draw_y_grid(area_ticks)
  cumulative_area <- draw_stacked_area(
    years,
    area_matrix,
    crop_colors[rownames(area_matrix)]
  )
  lines(years, cumulative_area[nrow(cumulative_area), ], lwd = 1.2, col = "grey20")
  axis(1, at = x_ticks)

  par(mar = c(3.8, 4.3, 2.1, 0.9))
  value_max <- max(yearly$selected_value_share_pct, na.rm = TRUE)
  value_limit <- min(100, ceiling(value_max / 10) * 10)
  value_ticks <- seq(0, value_limit, by = 20)

  plot(
    range(years),
    c(0, value_limit),
    type = "n",
    xaxt = "n",
    xlab = "Year",
    ylab = "Share of total crop-production value (%)",
    main = "C. Production value share",
    bty = "l"
  )
  draw_y_grid(value_ticks)
  cumulative_value <- draw_stacked_area(
    years,
    value_share_matrix,
    crop_colors[rownames(value_share_matrix)]
  )
  lines(
    years,
    cumulative_value[nrow(cumulative_value), ],
    lwd = 1.2,
    col = "grey20"
  )
  abline(
    h = yearly$mean_selected_value_share_pct[1],
    lty = "dashed",
    lwd = 0.8,
    col = "grey35"
  )
  text(
    x = 1989,
    y = yearly$mean_selected_value_share_pct[1] + 5.0,
    labels = sprintf("Mean: %.1f%%", yearly$mean_selected_value_share_pct[1]),
    adj = c(0, 0.5),
    cex = 0.72,
    col = "grey25"
  )
  axis(1, at = x_ticks)

  par(mar = c(0.0, 0.0, 0.0, 0.0))
  plot.new()
  legend(
    "center",
    legend = c(crop_labels[crop_order], "Selected crops total"),
    fill = c(crop_colors[crop_order], NA),
    border = NA,
    lty = c(rep(NA, length(crop_order)), 1),
    lwd = c(rep(NA, length(crop_order)), 1.2),
    col = c(rep(NA, length(crop_order)), "grey20"),
    ncol = 4,
    bty = "n",
    cex = 0.78
  )
}

for (f in area_files) {
  if (!file.exists(f)) {
    stop("Missing IBGE/PAM input file: ", f)
  }
}

raw_area <- do.call(rbind, lapply(area_files, read_area_file))

if (anyDuplicated(raw_area[, c("code", "year")]) > 0L) {
  duplicated_rows <- raw_area[duplicated(raw_area[, c("code", "year")]), ]
  stop(
    "Duplicated municipality-year rows found in raw IBGE planted-area files. ",
    "First duplicate: ",
    duplicated_rows$code[1],
    "-",
    duplicated_rows$year[1]
  )
}

raw_area <- raw_area[raw_area$year >= 1988 & raw_area$year <= 2018, ]

area_vars <- c("total", unname(selected_crops))
area_yearly <- aggregate(
  raw_area[, area_vars],
  by = list(year = raw_area$year),
  FUN = sum_or_na
)
names(area_yearly)[match(unname(selected_crops), names(area_yearly))] <- crop_order

years <- sort(unique(area_yearly$year))
value_yearly <- build_value_yearly(years)

yearly <- merge(area_yearly, value_yearly, by = "year", all.x = TRUE, sort = TRUE)

yearly$selected_area_ha <- rowSums(yearly[, crop_order], na.rm = TRUE)
yearly$selected_share_pct <- 100 * yearly$selected_area_ha / yearly$total
yearly$total_area_mha <- yearly$total / 1e6
yearly$selected_area_mha <- yearly$selected_area_ha / 1e6

for (crop in crop_order) {
  yearly[[paste0(crop, "_area_share_pct")]] <- 100 * yearly[[crop]] / yearly$total
  yearly[[paste0(crop, "_area_mha")]] <- yearly[[crop]] / 1e6
}

yearly$mean_selected_share_pct <- mean(yearly$selected_share_pct, na.rm = TRUE)
yearly$mean_selected_value_share_pct <- mean(
  yearly$selected_value_share_pct,
  na.rm = TRUE
)

write.csv(yearly, output_csv, row.names = FALSE, na = "")
plot_scope_figure(yearly, output_pdf, device = "pdf")
plot_scope_figure(yearly, output_png, device = "png")

cat("Wrote: ", output_csv, "\n", sep = "")
cat("Wrote: ", output_pdf, "\n", sep = "")
cat("Wrote: ", output_png, "\n", sep = "")
cat("Years: ", min(yearly$year), "-", max(yearly$year), "\n", sep = "")
cat(
  "Selected planted-area share, mean 1988-2018: ",
  sprintf("%.1f%%", mean(yearly$selected_share_pct, na.rm = TRUE)),
  "\n",
  sep = ""
)
cat(
  "Selected planted-area share, 2018: ",
  sprintf("%.1f%%", yearly$selected_share_pct[yearly$year == 2018]),
  "\n",
  sep = ""
)
cat(
  "Selected production-value share, mean 1988-2018: ",
  sprintf("%.1f%%", mean(yearly$selected_value_share_pct, na.rm = TRUE)),
  "\n",
  sep = ""
)
cat(
  "Selected production-value share, 2018: ",
  sprintf("%.1f%%", yearly$selected_value_share_pct[yearly$year == 2018]),
  "\n",
  sep = ""
)
