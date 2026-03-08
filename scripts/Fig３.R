pkgs <- c("dplyr", "readxl", "geepack", "ggplot2", "patchwork", "writexl", "tibble")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

out_dir <- "output：Fig３"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

in_file <- "UHC_HDI_dataset.xlsx"
if (!file.exists(in_file)) in_file <- "/mnt/data/UHC_HDI_dataset.xlsx"
if (!file.exists(in_file)) stop("Excel file not found: ", in_file, "\ngetwd() = ", getwd())

sheets <- readxl::excel_sheets(in_file)
sheet_use <- if ("dataset" %in% sheets) "dataset" else sheets[1]

df_raw <- readxl::read_excel(in_file, sheet = sheet_use) %>%
  dplyr::select(-starts_with("Unnamed"))

required_cols <- c(
  "country", "year",
  "UHC SCI", "RMNCH", "ID", "NCD", "SCA",
  "HDI", "GNI index", "Education index", "Life expectancy index",
  "Control of Corruption", "Population ages 65+ (%)", "Total fertility rate", "Urban population (%)"
)
miss <- setdiff(required_cols, names(df_raw))
if (length(miss) > 0) stop("Required columns are missing: ", paste(miss, collapse = ", "))

df <- df_raw %>%
  transmute(
    country = as.character(country),
    year = as.integer(year),
    uhc_sci = as.numeric(`UHC SCI`),
    rmnch = as.numeric(RMNCH),
    id = as.numeric(ID),
    ncd = as.numeric(NCD),
    sca = as.numeric(SCA),
    hdi_0_1 = as.numeric(HDI) * 10,
    gni_0_1 = as.numeric(`GNI index`) * 10,
    edu_0_1 = as.numeric(`Education index`) * 10,
    leidx_0_1 = as.numeric(`Life expectancy index`) * 10,
    coc = as.numeric(`Control of Corruption`),
    age65 = as.numeric(`Population ages 65+ (%)`),
    tfr = as.numeric(`Total fertility rate`),
    urban = as.numeric(`Urban population (%)`)
  ) %>%
  mutate(country_id = as.integer(factor(country))) %>%
  arrange(country_id, year)

x_limits_common <- c(-1.8, 19.5)
x_breaks_common <- seq(-2, 20, by = 2)
covars_adjusted <- c("coc", "age65", "tfr", "urban")

fit_one <- function(data, outcome, exposure, covars) {
  use_cols <- unique(c("country_id", "year", outcome, exposure, covars))
  d <- data %>%
    select(all_of(use_cols)) %>%
    filter(if_all(everything(), ~ !is.na(.x)))
  
  rhs <- paste(c(exposure, "factor(year)", covars), collapse = " + ")
  fml <- as.formula(paste0(outcome, " ~ ", rhs))
  
  mod <- geepack::geeglm(
    fml,
    id = country_id,
    data = d,
    family = gaussian(),
    corstr = "independence",
    std.err = "san.se"
  )
  
  co <- as.data.frame(summary(mod)$coefficients)
  co$term <- rownames(co)
  
  est_col <- if ("Estimate" %in% names(co)) "Estimate" else names(co)[1]
  se_col  <- if ("Std.err" %in% names(co)) "Std.err" else names(co)[2]
  p_col   <- names(co)[grepl("Pr\\(", names(co))]
  p_col   <- if (length(p_col) == 0) NA_character_ else p_col[1]
  
  row <- co[co$term == exposure, , drop = FALSE]
  
  tibble::tibble(
    estimate = as.numeric(row[[est_col]]),
    se = as.numeric(row[[se_col]]),
    ci_low = estimate - 1.96 * se,
    ci_high = estimate + 1.96 * se,
    p_value = if (!is.na(p_col)) as.numeric(row[[p_col]]) else NA_real_,
    n_obs = nrow(d),
    n_country = dplyr::n_distinct(d$country_id)
  )
}

run_exposure <- function(exposure, panel_title, show_x) {
  outcomes <- c("uhc_sci", "rmnch", "id", "ncd", "sca")
  outcome_labels <- c("UHC SCI", "RMNCH", "ID", "NCD", "SCA")
  
  res <- lapply(seq_along(outcomes), function(i) {
    out <- outcomes[i]
    lab <- outcome_labels[i]
    
    crude <- fit_one(df, out, exposure, character())
    adj   <- fit_one(df, out, exposure, covars_adjusted)
    
    dplyr::bind_rows(
      dplyr::mutate(crude, model = "Crude", outcome = out, outcome_label = lab),
      dplyr::mutate(adj,   model = "Adjusted", outcome = out, outcome_label = lab)
    )
  }) |> dplyr::bind_rows()
  
  res <- res %>%
    mutate(
      model = factor(model, levels = c("Crude", "Adjusted")),
      outcome_label = factor(outcome_label, levels = rev(c("UHC SCI", "RMNCH", "ID", "NCD", "SCA")))
    )
  
  p <- ggplot(res, aes(x = estimate, y = outcome_label, color = model, shape = model)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_errorbar(
      aes(xmin = ci_low, xmax = ci_high),
      height = 0.18,
      position = position_dodge(width = 0.6),
      orientation = "y"
    ) +
    geom_point(size = 2.4, position = position_dodge(width = 0.6)) +
    scale_x_continuous(limits = x_limits_common, breaks = x_breaks_common) +
    labs(title = panel_title, x = NULL, y = NULL, color = NULL, shape = NULL) +
    theme_minimal(base_size = 15) +
    theme(
      axis.text.y = element_text(hjust = 0.5),
      plot.title = element_text(face = "bold", hjust = 0, margin = margin(l = -12)),
      plot.title.position = "plot",
      legend.position = "bottom",
      legend.direction = "horizontal"
    )
  
  if (!show_x) p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  list(plot = p, data = res)
}

o1 <- run_exposure("hdi_0_1",  "HDI",                   FALSE)
o2 <- run_exposure("gni_0_1",  "GNI index",              FALSE)
o3 <- run_exposure("edu_0_1",  "Education index",        FALSE)
o4 <- run_exposure("leidx_0_1","Life expectancy index",   TRUE)

p_all <- (o1$plot / o2$plot / o3$plot / o4$plot) +
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(p_all)

out_plot <- file.path(out_dir, "Fig3_GEE_Slopes_CrudeAdj.tiff")
ggsave(out_plot, p_all, device = "tiff", dpi = 300, width = 9, height = 15, units = "in", compression = "lzw")

xlsx_out <- file.path(out_dir, "Fig3_GEE_Slopes_CrudeAdj.xlsx")
writexl::write_xlsx(
  list(
    "HDI" = o1$data,
    "GNI_index" = o2$data,
    "Education_index" = o3$data,
    "Life_expectancy_index" = o4$data
  ),
  path = xlsx_out
)

message("Saved: ", normalizePath(out_plot, winslash = "/"))
message("Saved: ", normalizePath(xlsx_out, winslash = "/"))