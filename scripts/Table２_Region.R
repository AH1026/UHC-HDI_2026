pkgs <- c("readxl", "dplyr", "geepack", "openxlsx")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

out_dir <- "output：Table２"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

in_file <- "UHC_HDI_dataset.xlsx"
if (!file.exists(in_file)) in_file <- "/mnt/data/UHC_HDI_dataset.xlsx"
if (!file.exists(in_file)) stop("Excel file not found: ", in_file, "\ngetwd() = ", getwd())

sheets <- readxl::excel_sheets(in_file)
sheet_use <- if ("dataset" %in% sheets) "dataset" else sheets[1]

df <- readxl::read_excel(in_file, sheet = sheet_use) %>%
  dplyr::select(-starts_with("Unnamed"))

need_cols <- c(
  "country", "year", "Income group", "Region",
  "UHC SCI", "RMNCH", "ID", "NCD", "SCA", "HDI",
  "Control of Corruption", "Population ages 65+ (%)",
  "Total fertility rate", "Urban population (%)"
)
miss <- setdiff(need_cols, names(df))
if (length(miss) > 0) stop("Required columns are missing: ", paste(miss, collapse = ", "))

econ_levels <- c("1", "2", "3", "4")
region_levels <- c("1", "2", "3", "4", "5", "6")

econ_labels <- c(
  "1" = "Low income",
  "2" = "Lower-middle income",
  "3" = "Upper-middle income",
  "4" = "High income"
)

region_labels <- c(
  "1" = "EMR",
  "2" = "EUR",
  "3" = "AFR",
  "4" = "AMR",
  "5" = "WPR",
  "6" = "SEAR"
)

normalize_code <- function(x) {
  out <- suppressWarnings(as.character(as.integer(as.numeric(as.character(x)))))
  out[is.na(out)] <- as.character(x[is.na(out)])
  out
}

df2 <- df %>%
  mutate(
    country = as.factor(country),
    year = as.integer(year),
    income_group_ord = ordered(normalize_code(`Income group`), levels = econ_levels),
    income_group = factor(as.character(income_group_ord), levels = econ_levels),
    income_group = relevel(income_group, ref = "4"),
    region_nom = factor(normalize_code(`Region`), levels = region_levels),
    region_nom = relevel(region_nom, ref = "2"),
    control_cor = as.numeric(`Control of Corruption`),
    age65 = as.numeric(`Population ages 65+ (%)`),
    tfr = as.numeric(`Total fertility rate`),
    urban = as.numeric(`Urban population (%)`)
  )

trunc_dec <- function(x, digits = 2) {
  ifelse(is.na(x) | is.infinite(x), NA_real_, floor(x * (10^digits)) / (10^digits))
}

fmt_ci <- function(est, low, high, digits = 2) {
  if (any(is.na(c(est, low, high)))) return(NA_character_)
  est2 <- trunc_dec(est, digits)
  low2 <- trunc_dec(low, digits)
  high2 <- trunc_dec(high, digits)
  sprintf(paste0("%.", digits, "f (%.", digits, "f, %.", digits, "f)"), est2, low2, high2)
}

fit_region_vec <- function(outcome_var, levels_chr, ref_level, adjusted = TRUE, digits = 2) {
  dat0 <- df2 %>%
    mutate(y = as.numeric(.data[[outcome_var]])) %>%
    filter(!is.na(y), !is.na(country), !is.na(year), !is.na(region_nom))
  
  if (adjusted) {
    dat0 <- dat0 %>%
      filter(!is.na(income_group), !is.na(control_cor), !is.na(age65), !is.na(tfr), !is.na(urban))
  }
  
  n_nonpositive <- sum(dat0$y <= 0, na.rm = TRUE)
  
  dat <- dat0 %>%
    filter(y > 0)
  
  fml <- if (adjusted) {
    y ~ region_nom + income_group + control_cor + age65 + tfr + urban
  } else {
    y ~ region_nom
  }
  
  fit <- tryCatch(
    geepack::geeglm(
      formula = fml,
      id = country,
      waves = year,
      family = gaussian(link = "log"),
      corstr = "independence",
      std.err = "san.se",
      data = dat
    ),
    error = function(e) NULL
  )
  
  vec <- setNames(rep(NA_character_, length(levels_chr)), levels_chr)
  vec[ref_level] <- "ref"
  
  info <- dat %>%
    summarise(
      outcome = outcome_var,
      model = ifelse(adjusted, "Adjusted (Income group + covariates)", "Crude"),
      family = "gaussian",
      link = "log",
      corstr = "independence",
      id_variable = "country",
      time_variable = "year",
      income_group_type = "ordinal (modeled as treatment-coded factor when adjusted)",
      region_type = "nominal",
      covariates_type = "continuous",
      n_obs = n(),
      n_country = dplyr::n_distinct(country),
      n_excluded_nonpos_y = n_nonpositive
    )
  
  if (nrow(dat) == 0 || is.null(fit)) return(list(vec = vec, info = info))
  
  co <- as.data.frame(summary(fit)$coefficients)
  co$term <- rownames(co)
  rownames(co) <- NULL
  
  est_col <- if ("Estimate" %in% names(co)) "Estimate" else names(co)[1]
  se_col <- if ("Std.err" %in% names(co)) "Std.err" else names(co)[2]
  
  for (lv in setdiff(levels_chr, ref_level)) {
    term <- paste0("region_nom", lv)
    row <- co[co$term == term, , drop = FALSE]
    
    if (nrow(row) == 1) {
      beta <- as.numeric(row[[est_col]])
      se <- as.numeric(row[[se_col]])
      ratio <- exp(beta)
      low <- exp(beta - 1.96 * se)
      high <- exp(beta + 1.96 * se)
      vec[lv] <- fmt_ci(ratio, low, high, digits = digits)
    }
  }
  
  list(vec = vec, info = info)
}

outcomes <- c("UHC SCI", "RMNCH", "ID", "NCD", "SCA", "HDI")

region_adj <- data.frame(`Region` = unname(region_labels[region_levels]), check.names = FALSE)
region_cru <- data.frame(`Region` = unname(region_labels[region_levels]), check.names = FALSE)
info_list <- list()

for (v in outcomes) {
  r_adj <- fit_region_vec(v, region_levels, "2", adjusted = TRUE, digits = 2)
  region_adj[[v]] <- unname(r_adj$vec[region_levels])
  info_list[[paste0("Adj_", v)]] <- r_adj$info
  
  r_cru <- fit_region_vec(v, region_levels, "2", adjusted = FALSE, digits = 2)
  region_cru[[v]] <- unname(r_cru$vec[region_levels])
  info_list[[paste0("Crude_", v)]] <- r_cru$info
}

model_info <- dplyr::bind_rows(info_list)

out_xlsx <- file.path(out_dir, "Table2_Region.xlsx")

if (file.exists(out_xlsx)) {
  ok_remove <- tryCatch(file.remove(out_xlsx), error = function(e) FALSE, warning = function(w) FALSE)
  if (!isTRUE(ok_remove) && file.exists(out_xlsx)) {
    stop("Existing output file could not be removed. Please close Excel if the file is open:\n", out_xlsx)
  }
}

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Adjusted_Region_refEUR")
openxlsx::addWorksheet(wb, "Crude_Region_refEUR")
openxlsx::addWorksheet(wb, "ModelInfo")

openxlsx::writeData(wb, "Adjusted_Region_refEUR", region_adj)
openxlsx::writeData(wb, "Crude_Region_refEUR", region_cru)
openxlsx::writeData(wb, "ModelInfo", model_info)

openxlsx::setColWidths(wb, "Adjusted_Region_refEUR", cols = 1:ncol(region_adj), widths = "auto")
openxlsx::setColWidths(wb, "Crude_Region_refEUR", cols = 1:ncol(region_cru), widths = "auto")
openxlsx::setColWidths(wb, "ModelInfo", cols = 1:ncol(model_info), widths = "auto")

openxlsx::freezePane(wb, "Adjusted_Region_refEUR", firstRow = TRUE, firstCol = TRUE)
openxlsx::freezePane(wb, "Crude_Region_refEUR", firstRow = TRUE, firstCol = TRUE)
openxlsx::freezePane(wb, "ModelInfo", firstRow = TRUE)

openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved: ", normalizePath(out_xlsx, winslash = "/", mustWork = FALSE))