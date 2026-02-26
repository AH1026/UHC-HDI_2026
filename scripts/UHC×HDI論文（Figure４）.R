pkgs <- c("readxl","dplyr","ggplot2","geepack","patchwork","openxlsx")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

out_dir <- "UHC×HDI論文_output：Figure４"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

in_file <- "UHC・4サブ_HDI・3要素_共変量データセット.xlsx"
if (!file.exists(in_file)) in_file <- "/mnt/data/UHC・4サブ_HDI・3要素_共変量データセット.xlsx"
if (!file.exists(in_file)) stop("Excelが見つかりません: ", in_file, "\ngetwd() = ", getwd())

years_keep <- c(2000, 2005, 2010, 2015, 2017, 2019, 2021)
corstr_value <- "independence"

sheets <- readxl::excel_sheets(in_file)
sheet_use <- if ("ロング型" %in% sheets) "ロング型" else sheets[1]
raw <- readxl::read_excel(in_file, sheet = sheet_use) %>%
  dplyr::select(-starts_with("Unnamed"))

if(!("year" %in% names(raw)) && ("Year" %in% names(raw))) raw <- dplyr::rename(raw, year = Year)
if(!("country" %in% names(raw)) && ("Country" %in% names(raw))) raw <- dplyr::rename(raw, country = Country)

need_cols <- c(
  "country","year",
  "UHC SCI",
  "HDI","GNI index","Education index","Life expectancy index",
  "Control of Corruption","Population ages 65+ (%)","Total fertility rate","Urban population (%)"
)
miss <- setdiff(need_cols, names(raw))
if(length(miss) > 0) stop("必要な列が見つかりません: ", paste(miss, collapse = ", "))

safe_vcov <- function(fit){
  V <- vcov(fit)
  V <- (V + t(V)) / 2
  ev <- suppressWarnings(eigen(V, symmetric = TRUE, only.values = TRUE)$values)
  min_ev <- suppressWarnings(min(ev))
  if(!is.finite(min_ev) || min_ev < 1e-10){
    eps <- ifelse(is.finite(min_ev), abs(min_ev) + 1e-6, 1e-6)
    V <- V + diag(eps, nrow(V))
  }
  V
}

estimate_yearly_slope <- function(outcome_col, outcome_label, expo_col, expo_label){
  dat <- raw %>%
    dplyr::select(
      country, year,
      dplyr::all_of(outcome_col),
      dplyr::all_of(expo_col),
      `Control of Corruption`,
      `Population ages 65+ (%)`,
      `Total fertility rate`,
      `Urban population (%)`
    ) %>%
    dplyr::rename(
      y = !!outcome_col,
      expo = !!expo_col,
      corruption = `Control of Corruption`,
      elderly = `Population ages 65+ (%)`,
      tfr = `Total fertility rate`,
      urban = `Urban population (%)`
    ) %>%
    dplyr::filter(year %in% years_keep) %>%
    dplyr::mutate(
      country = factor(country),
      year_f  = factor(year, levels = years_keep)
    ) %>%
    dplyr::filter(
      !is.na(y), !is.na(expo),
      !is.na(corruption), !is.na(elderly), !is.na(tfr), !is.na(urban)
    ) %>%
    dplyr::arrange(country, year)
  
  n_by_year <- dat %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(n_country = dplyr::n_distinct(country), .groups = "drop")
  
  fit <- geepack::geeglm(
    y ~ expo * year_f + corruption + elderly + tfr + urban,
    id     = country,
    data   = dat,
    family = gaussian(link = "identity"),
    corstr = corstr_value,
    waves  = year
  )
  
  b <- coef(fit)
  V <- safe_vcov(fit)
  cn <- names(b)
  
  get_int <- function(yr){
    cand1 <- paste0("expo:year_f", yr)
    cand2 <- paste0("year_f", yr, ":expo")
    if(cand1 %in% cn) return(cand1)
    if(cand2 %in% cn) return(cand2)
    NA_character_
  }
  
  calc_one <- function(yr){
    cvec <- rep(0, length(b)); names(cvec) <- cn
    cvec["expo"] <- 1
    if(yr != years_keep[1]){
      it <- get_int(yr)
      if(!is.na(it)) cvec[it] <- 1
    }
    est <- sum(cvec * b)
    var <- as.numeric(t(cvec) %*% V %*% cvec)
    if(!is.finite(var) || var < 0) var <- NA_real_
    se  <- ifelse(is.na(var), NA_real_, sqrt(var))
    
    est_01 <- est * 0.1
    se_01  <- se  * 0.1
    
    data.frame(
      year = yr,
      slope_per_0.1 = est_01,
      ci_low  = est_01 - 1.96 * se_01,
      ci_high = est_01 + 1.96 * se_01
    )
  }
  
  do.call(rbind, lapply(years_keep, calc_one)) %>%
    dplyr::left_join(n_by_year, by = "year") %>%
    dplyr::mutate(outcome = outcome_label, exposure = expo_label, corstr = corstr_value) %>%
    dplyr::filter(is.finite(slope_per_0.1), is.finite(ci_low), is.finite(ci_high))
}

make_panel <- function(df, title_text, y_lim, show_x_title){
  ggplot(df, aes(x = year, y = slope_per_0.1)) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "mediumpurple1", alpha = 0.25) +
    geom_line(color = "purple4", linewidth = 1.05) +
    geom_point(color = "purple4", size = 2.5) +
    scale_x_continuous(breaks = years_keep) +
    scale_y_continuous(breaks = seq(y_lim[1], y_lim[2], 2)) +
    coord_cartesian(ylim = y_lim) +
    labs(title = title_text, x = if(show_x_title) "Year" else NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(face = "bold", size = 13))
}

outcome_col <- "UHC SCI"
outcome_label <- "UHC SCI"
y_lim <- c(0, 10)

df_hdi  <- estimate_yearly_slope(outcome_col, outcome_label, "HDI", "HDI")
df_gni  <- estimate_yearly_slope(outcome_col, outcome_label, "GNI index", "GNI index")
df_edu  <- estimate_yearly_slope(outcome_col, outcome_label, "Education index", "Education index")
df_leix <- estimate_yearly_slope(outcome_col, outcome_label, "Life expectancy index", "Life expectancy index")

p_hdi  <- make_panel(df_hdi,  "HDI", y_lim, TRUE)
p_gni  <- make_panel(df_gni,  "GNI index", y_lim, FALSE)
p_edu  <- make_panel(df_edu,  "Education index", y_lim, FALSE)
p_leix <- make_panel(df_leix, "Life expectancy index", y_lim, FALSE)

p_2x2 <- (p_hdi + p_gni) / (p_edu + p_leix)
print(p_2x2)

tiff_out <- file.path(out_dir, "AdjOnly_INDEP_UHC_SCI_2x2.tiff")
ggsave(tiff_out, plot = p_2x2, width = 12, height = 8.2, dpi = 300, device = "tiff", compression = "lzw")

all_df <- dplyr::bind_rows(df_hdi, df_gni, df_edu, df_leix)

excel_out <- file.path(out_dir, "AdjOnly_INDEP_UHC_SCI_results.xlsx")
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "All_results")
openxlsx::writeData(wb, "All_results", all_df)
openxlsx::saveWorkbook(wb, excel_out, overwrite = TRUE)

cat("\nSaved:\n", tiff_out, "\n", excel_out, "\n", sep = "")