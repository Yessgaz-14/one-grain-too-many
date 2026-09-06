# Create two plain-language figures from the preferred thesis estimates.
# Run from the repository root with: Rscript --vanilla make_public_results_visualizations.R

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required.")
}

library(ggplot2)

yield_file <- file.path("results", "tables", "Main_Yield_Coefficients.csv")
cropland_file <- file.path("results", "tables", "Total_Cropland_Change_Coefficients.csv")
output_dir <- file.path("results", "figures")

for (path in c(yield_file, cropland_file)) {
  if (!file.exists(path)) stop("Missing input file: ", path)
}
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

read_checked_csv <- function(path, required_columns) {
  data <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0L) {
    stop("Missing columns in ", path, ": ", paste(missing_columns, collapse = ", "))
  }
  data
}

percent_label <- function(x) paste0(formatC(x, format = "f", digits = 0), "%")

# Yield effects: use the preferred specification with year fixed effects and
# crop-calendar climate controls. Log coefficients and confidence limits are
# converted to exact percentage changes with 100 * (exp(beta) - 1).
yield <- read_checked_csv(
  yield_file,
  c(
    "exposure", "crop", "crop_label", "specification", "estimate",
    "p_value", "ci95_low", "ci95_high"
  )
)
yield <- yield[
  yield$exposure == "Crop-specific FAO excess salinity" &
    yield$specification == "Spec 2",
  , drop = FALSE
]

crop_order <- c("Corn", "Rice", "Cassava", "Beans", "Soybeans", "Sugarcane")
if (nrow(yield) != length(crop_order) || !setequal(yield$crop_label, crop_order)) {
  stop("The preferred yield sample does not contain exactly one estimate per crop.")
}

numeric_yield_columns <- c("estimate", "p_value", "ci95_low", "ci95_high")
yield[numeric_yield_columns] <- lapply(yield[numeric_yield_columns], as.numeric)
if (any(!is.finite(as.matrix(yield[numeric_yield_columns])))) {
  stop("The preferred yield estimates contain non-finite values.")
}

yield$estimate_pct <- 100 * (exp(yield$estimate) - 1)
yield$ci95_low_pct <- 100 * (exp(yield$ci95_low) - 1)
yield$ci95_high_pct <- 100 * (exp(yield$ci95_high) - 1)
yield$evidence <- ifelse(
  yield$estimate < 0 & yield$p_value < 0.05,
  "Robust evidence of a decrease",
  "Estimate remains uncertain"
)
yield$evidence <- factor(
  yield$evidence,
  levels = c("Robust evidence of a decrease", "Estimate remains uncertain")
)
yield$crop_label <- factor(yield$crop_label, levels = rev(crop_order))
yield$point_label <- sprintf("%+.1f%%", yield$estimate_pct)

evidence_colors <- c(
  "Robust evidence of a decrease" = "#B33A3A",
  "Estimate remains uncertain" = "#68727D"
)

yield_figure <- ggplot(yield, aes(x = estimate_pct, y = crop_label, color = evidence)) +
  geom_vline(xintercept = 0, color = "#1F2933", linewidth = 0.7, linetype = "dashed") +
  geom_segment(
    aes(x = ci95_low_pct, xend = ci95_high_pct, yend = crop_label),
    linewidth = 1.5,
    lineend = "round"
  ) +
  geom_point(size = 4.8) +
  geom_text(
    aes(label = point_label),
    position = position_nudge(y = 0.22),
    size = 4.3,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_color_manual(values = evidence_colors, name = NULL) +
  scale_x_continuous(
    breaks = c(-40, -20, 0, 20, 40),
    labels = percent_label,
    limits = c(-48, 42)
  ) +
  labs(
    title = "Higher excess salinity is linked to lower corn and bean yields",
    subtitle = paste(
      "Estimated yield difference associated with 1 dS/m more excess salinity.",
      "Points show the preferred estimate; lines show the 95% uncertainty range.",
      sep = "\n"
    ),
    x = "Estimated difference in crop yield",
    y = NULL,
    caption = paste(
      "Source: IBGE/PAM and Hassani et al. (2020). Brazil, 1985-2018.",
      "The preferred spatial first-difference model compares neighboring municipalities,",
      "includes year fixed effects and climate controls, and uses Conley standard errors (200 km).",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 14, face = "bold", color = "#263238"),
    axis.text.x = element_text(size = 12, color = "#374151"),
    axis.title.x = element_text(size = 13, face = "bold", margin = margin(t = 10)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 20, face = "bold", color = "#18212A"),
    plot.subtitle = element_text(size = 13, color = "#374151", lineheight = 1.05),
    plot.caption = element_text(size = 9.5, color = "#4B5563", hjust = 0, lineheight = 1.0),
    plot.margin = margin(18, 24, 16, 18)
  )

ggsave(
  file.path(output_dir, "Yield_Effects_for_General_Audience.png"),
  yield_figure,
  width = 10.5,
  height = 7.5,
  dpi = 320,
  bg = "white"
)

# Cropland effect: convert the preferred percentage-point coefficient into
# hectares for the 100,000-hectare illustration used in the thesis text.
cropland <- read_checked_csv(
  cropland_file,
  c("Model", "estimate", "se", "p", "observations", "specification")
)
cropland <- cropland[cropland$specification == "Weather", , drop = FALSE]
if (nrow(cropland) != 1L) stop("The preferred cropland estimate is not unique.")

cropland$estimate <- as.numeric(cropland$estimate)
cropland$se <- as.numeric(cropland$se)
if (!is.finite(cropland$estimate) || !is.finite(cropland$se)) {
  stop("The preferred cropland estimate is not finite.")
}

municipal_area_ha <- 100000
coefficient_low <- cropland$estimate - 1.96 * cropland$se
coefficient_high <- cropland$estimate + 1.96 * cropland$se
fewer_hectares <- -cropland$estimate / 100 * municipal_area_ha
fewer_hectares_low <- -coefficient_high / 100 * municipal_area_ha
fewer_hectares_high <- -coefficient_low / 100 * municipal_area_ha

cropland_plot_data <- data.frame(
  estimate = fewer_hectares,
  low = fewer_hectares_low,
  high = fewer_hectares_high,
  y = 1
)

cropland_figure <- ggplot(cropland_plot_data, aes(x = estimate, y = y)) +
  geom_vline(xintercept = 0, color = "#1F2933", linewidth = 1.0) +
  geom_segment(
    aes(x = 0, xend = estimate, yend = y),
    color = "#CDE8E2",
    linewidth = 15,
    lineend = "butt"
  ) +
  geom_segment(
    aes(x = low, xend = high, yend = y),
    color = "#00796B",
    linewidth = 1.7,
    lineend = "round"
  ) +
  geom_segment(
    aes(x = low, xend = low, y = 0.95, yend = 1.05),
    color = "#00796B",
    linewidth = 1.4
  ) +
  geom_segment(
    aes(x = high, xend = high, y = 0.95, yend = 1.05),
    color = "#00796B",
    linewidth = 1.4
  ) +
  geom_point(color = "#00796B", size = 8) +
  annotate(
    "text",
    x = fewer_hectares,
    y = 1.13,
    label = paste0(round(fewer_hectares), " hectares lower"),
    color = "#005F56",
    size = 6.2,
    fontface = "bold"
  ) +
  scale_x_continuous(
    limits = c(-5, 105),
    breaks = seq(0, 100, 25),
    labels = function(x) paste0(x, " ha")
  ) +
  coord_cartesian(ylim = c(0.72, 1.27), clip = "off") +
  labs(
    title = "Higher salinity is associated with a lower one-year change in cultivated area",
    subtitle = paste(
      "Illustration for two neighboring municipalities of 100,000 hectares",
      "that differ by 1 dS/m in mean salinity.",
      sep = "\n"
    ),
    x = "Lower annual change in cultivated area",
    y = NULL,
    caption = paste(
      "Source: MapBiomas and Hassani et al. (2020). All crops combined; pasture excluded; 1985-2017.",
      paste0(
        "The bar starts at no difference (0); the thin line is the 95% interval (",
        round(fewer_hectares_low), "-", round(fewer_hectares_high), " ha)."
      ),
      "The estimate can reflect less expansion or greater contraction; it does not mean every municipality loses 65 ha.",
      "Preferred model with year fixed effects, climate controls, and Conley standard errors (200 km).",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 12, color = "#374151"),
    axis.title.x = element_text(size = 13, face = "bold", margin = margin(t = 12)),
    plot.title = element_text(size = 20, face = "bold", color = "#18212A"),
    plot.subtitle = element_text(size = 13, color = "#374151", lineheight = 1.05),
    plot.caption = element_text(size = 9.5, color = "#4B5563", hjust = 0, lineheight = 1.0),
    plot.margin = margin(20, 28, 16, 20)
  )

ggsave(
  file.path(output_dir, "Cropland_Change_in_Hectares_for_General_Audience.png"),
  cropland_figure,
  width = 10.5,
  height = 6.3,
  dpi = 320,
  bg = "white"
)

message("Created two public-facing result figures in ", output_dir, ".")
