pkgs <- c("readxl", "dplyr", "geepack", "openxlsx")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

out_dir <- "UHC×HDI論文_output：Table２"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

in_file <- "UHC・4サブ_HDI・3要素_共変量データセット.xlsx"
if (!file.exists(in_file)) in_file <- "/mnt/data/UHC・4サブ_HDI・3要素_共変量データセット.xlsx"
if (!file.exists(in_file)) stop("Excelが見つかりません: ", in_file, "\ngetwd() = ", getwd())

sheets <- readxl::excel_sheets(in_file)
sheet_use <- if ("ロング型" %in% sheets) "ロング型" else sheets[1]
df <- readxl::read_excel(in_file, sheet = sheet_use) %>%
  dplyr::select(-starts_with("Unnamed"))

need_cols <- c(
  "country","year","Income group","Region",
  "UHC SCI","RMNCH","ID","NCD","SCA","HDI",
  "Control of Corruption","Population ages 65+ (%)","Total fertility rate",
  "Urban population (%)"
)
miss <- setdiff(need_cols, names(df))
if (length(miss) > 0) stop("必要な列が見つかりません: ", paste(miss, collapse = ", "))

econ_levels   <- c("1","2","3","4")
region_levels <- c("1","2","3","4","5","6")

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

df2 <- df %>%
  mutate(
    country = as.factor(country),
    year    = as.integer(year),
    income_group = as.integer(as.character(`Income group`)),
    income_group = factor(income_group, levels = as.integer(econ_levels)),
    income_group = relevel(income_group, ref = "1"),
    region = as.integer(as.character(`Region`)),
    region = factor(region, levels = as.integer(region_levels)),
    region = relevel(region, ref = "3"),
    control_cor = as.numeric(`Control of Corruption`),
    age65       = as.numeric(`Population ages 65+ (%)`),
    tfr         = as.numeric(`Total fertility rate`),
    urban       = as.numeric(`Urban population (%)`)
  )

fmt_ci <- function(est, low, high, digits = 2) {
  if (any(is.na(c(est, low, high)))) return(NA_character_)
  sprintf(paste0("%.", digits, "f (%.", digits, "f, %.", digits, "f)"), est, low, high)
}

fit_ratio_vec <- function(outcome_var,
                          group_var,
                          levels_chr,
                          ref_level,
                          family_type = c("poisson", "gaussian"),
                          adjusted = TRUE,
                          require_y_positive = FALSE,
                          digits = 2) {
  
  family_type <- match.arg(family_type)
  
  dat <- df2 %>%
    mutate(y = as.numeric(.data[[outcome_var]])) %>%
    filter(!is.na(y), !is.na(country), !is.na(year), !is.na(.data[[group_var]]))
  
  if (adjusted) {
    dat <- dat %>% filter(!is.na(control_cor), !is.na(age65), !is.na(tfr), !is.na(urban))
  }
  if (require_y_positive) dat <- dat %>% filter(y > 0)
  
  fml <- if (adjusted) {
    as.formula(paste0("y ~ ", group_var, " + control_cor + age65 + tfr + urban"))
  } else {
    as.formula(paste0("y ~ ", group_var))
  }
  
  fam <- if (family_type == "poisson") poisson(link = "log") else gaussian(link = "log")
  
  fit <- tryCatch(
    geepack::geeglm(
      formula = fml,
      id      = country,
      waves   = year,
      family  = fam,
      corstr  = "independence",
      data    = dat
    ),
    error = function(e) NULL
  )
  
  vec <- setNames(rep(NA_character_, length(levels_chr)), levels_chr)
  vec[ref_level] <- "ref"
  
  info <- dat %>%
    summarise(
      outcome   = outcome_var,
      group     = group_var,
      adjusted  = adjusted,
      family    = family_type,
      link      = "log",
      corstr    = "independence",
      n_obs     = n(),
      n_country = dplyr::n_distinct(country)
    )
  
  if (is.null(fit)) return(list(vec = vec, info = info))
  
  co <- as.data.frame(summary(fit)$coefficients)
  co$term <- rownames(co)
  rownames(co) <- NULL
  names(co) <- gsub("Std\\.err", "StdErr", names(co))
  names(co) <- gsub("Pr\\(>\\|W\\|\\)", "p_value", names(co))
  
  for (lv in setdiff(levels_chr, ref_level)) {
    term <- paste0(group_var, lv)
    row  <- co[co$term == term, , drop = FALSE]
    if (nrow(row) == 1) {
      beta  <- row$Estimate
      se    <- row$StdErr
      ratio <- exp(beta)
      low   <- exp(beta - 1.96 * se)
      high  <- exp(beta + 1.96 * se)
      vec[lv] <- fmt_ci(ratio, low, high, digits = digits)
    }
  }
  
  list(vec = vec, info = info)
}

outcomes <- c("UHC SCI", "RMNCH", "ID", "NCD", "SCA", "HDI")
family_for <- function(v) if (v == "HDI") "gaussian" else "poisson"
need_pos   <- function(v) v == "HDI"

adj_econ    <- data.frame(`Income group` = unname(econ_labels[econ_levels]), check.names = FALSE)
crude_econ  <- data.frame(`Income group` = unname(econ_labels[econ_levels]), check.names = FALSE)
adj_region  <- data.frame(`Region` = unname(region_labels[region_levels]), check.names = FALSE)
crude_region<- data.frame(`Region` = unname(region_labels[region_levels]), check.names = FALSE)

info_list <- list()

for (v in outcomes) {
  r1 <- fit_ratio_vec(v, "income_group", econ_levels, "1", family_for(v), TRUE,  need_pos(v), digits = 2)
  adj_econ[[v]] <- unname(r1$vec[econ_levels])
  info_list[[paste0("AdjEcon_", v)]] <- r1$info
  
  r2 <- fit_ratio_vec(v, "income_group", econ_levels, "1", family_for(v), FALSE, need_pos(v), digits = 2)
  crude_econ[[v]] <- unname(r2$vec[econ_levels])
  info_list[[paste0("CrudeEcon_", v)]] <- r2$info
  
  r3 <- fit_ratio_vec(v, "region", region_levels, "3", family_for(v), TRUE,  need_pos(v), digits = 2)
  adj_region[[v]] <- unname(r3$vec[region_levels])
  info_list[[paste0("AdjRegion_", v)]] <- r3$info
  
  r4 <- fit_ratio_vec(v, "region", region_levels, "3", family_for(v), FALSE, need_pos(v), digits = 2)
  crude_region[[v]] <- unname(r4$vec[region_levels])
  info_list[[paste0("CrudeRegion_", v)]] <- r4$info
}

model_info <- dplyr::bind_rows(info_list)

out_xlsx <- file.path(out_dir, "GEE_EconRegion_Independence_AdjustedVsCrude.xlsx")

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Adjusted_Econ_ref1")
openxlsx::addWorksheet(wb, "Crude_Econ_ref1")
openxlsx::addWorksheet(wb, "Adjusted_Region_ref3")
openxlsx::addWorksheet(wb, "Crude_Region_ref3")
openxlsx::addWorksheet(wb, "ModelInfo")

openxlsx::writeData(wb, "Adjusted_Econ_ref1", adj_econ)
openxlsx::writeData(wb, "Crude_Econ_ref1", crude_econ)
openxlsx::writeData(wb, "Adjusted_Region_ref3", adj_region)
openxlsx::writeData(wb, "Crude_Region_ref3", crude_region)
openxlsx::writeData(wb, "ModelInfo", model_info)

openxlsx::setColWidths(wb, "Adjusted_Econ_ref1", cols = 1:ncol(adj_econ), widths = "auto")
openxlsx::setColWidths(wb, "Crude_Econ_ref1", cols = 1:ncol(crude_econ), widths = "auto")
openxlsx::setColWidths(wb, "Adjusted_Region_ref3", cols = 1:ncol(adj_region), widths = "auto")
openxlsx::setColWidths(wb, "Crude_Region_ref3", cols = 1:ncol(crude_region), widths = "auto")
openxlsx::setColWidths(wb, "ModelInfo", cols = 1:ncol(model_info), widths = "auto")

openxlsx::freezePane(wb, "Adjusted_Econ_ref1", firstRow = TRUE, firstCol = TRUE)
openxlsx::freezePane(wb, "Crude_Econ_ref1", firstRow = TRUE, firstCol = TRUE)
openxlsx::freezePane(wb, "Adjusted_Region_ref3", firstRow = TRUE, firstCol = TRUE)
openxlsx::freezePane(wb, "Crude_Region_ref3", firstRow = TRUE, firstCol = TRUE)
openxlsx::freezePane(wb, "ModelInfo", firstRow = TRUE)

openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved: ", normalizePath(out_xlsx, winslash = "/"))