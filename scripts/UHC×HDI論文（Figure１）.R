pkgs <- c("readxl","dplyr","tidyr","ggplot2","patchwork","openxlsx","rlang")
to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

out_dir <- "UHC×HDI論文_output：Figure１・２"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

in_file <- "UHC・4サブ_HDI・3要素_共変量データセット.xlsx"
if (!file.exists(in_file)) in_file <- "/mnt/data/UHC・4サブ_HDI・3要素_共変量データセット.xlsx"
if (!file.exists(in_file)) stop("Excelが見つかりません: ", in_file, "\ngetwd() = ", getwd())

sheets <- readxl::excel_sheets(in_file)
sheet_use <- if ("ロング型" %in% sheets) "ロング型" else sheets[1]
df <- readxl::read_excel(in_file, sheet = sheet_use) %>%
  dplyr::select(-starts_with("Unnamed"))

rename_if_exists <- function(d, from, to) {
  if (from %in% names(d) && !(to %in% names(d))) dplyr::rename(d, !!to := !!rlang::sym(from)) else d
}
df <- df %>%
  rename_if_exists("UHC", "UHC SCI") %>%
  rename_if_exists("UHCSCI", "UHC SCI") %>%
  rename_if_exists("UHC_SCI", "UHC SCI") %>%
  rename_if_exists("CD", "ID")

year_col <- {
  cands <- c("year","Year","YEAR","年")
  hit <- cands[tolower(cands) %in% tolower(names(df))]
  if (length(hit) == 0) NA_character_ else names(df)[match(tolower(hit[1]), tolower(names(df)))]
}
if (is.na(year_col)) stop("year列が見つかりません。")

need_cols <- c("Income group","UHC SCI","RMNCH","ID","NCD","SCA","HDI")
miss <- setdiff(need_cols, names(df))
if (length(miss) > 0) stop("Missing columns: ", paste(miss, collapse = ", "))

normalize_income <- function(x) {
  if (is.numeric(x) || is.integer(x)) {
    return(dplyr::case_when(
      x == 1 ~ "Low",
      x == 2 ~ "Lower-middle",
      x == 3 ~ "Upper-middle",
      x == 4 ~ "High",
      TRUE ~ NA_character_
    ))
  }
  xx <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    grepl("^low$|low income", xx) ~ "Low",
    grepl("lower[- ]?middle|lower[- ]?middle income|lower middle", xx) ~ "Lower-middle",
    grepl("upper[- ]?middle|upper[- ]?middle income|upper middle", xx) ~ "Upper-middle",
    grepl("^high$|high income", xx) ~ "High",
    TRUE ~ NA_character_
  )
}

df2 <- df %>%
  transmute(
    year = as.numeric(.data[[year_col]]),
    income_raw = .data[["Income group"]],
    `UHC SCI` = .data[["UHC SCI"]],
    RMNCH = .data[["RMNCH"]],
    ID = .data[["ID"]],
    NCD = .data[["NCD"]],
    SCA = .data[["SCA"]],
    HDI = .data[["HDI"]]
  ) %>%
  mutate(
    income = normalize_income(income_raw),
    income = factor(income, levels = c("Low","Lower-middle","Upper-middle","High"))
  ) %>%
  filter(!is.na(income), !is.na(year))

ind_levels <- c("UHC SCI","RMNCH","ID","NCD","SCA","HDI")

dat_long <- df2 %>%
  select(year, income, all_of(ind_levels)) %>%
  tidyr::pivot_longer(cols = all_of(ind_levels), names_to = "Indicator", values_to = "Value") %>%
  mutate(Indicator = factor(Indicator, levels = ind_levels))

years_all <- sort(unique(dat_long$year))

income_sum <- dat_long %>%
  group_by(income, year, Indicator) %>%
  summarise(
    n = sum(!is.na(Value)),
    Median = if (all(is.na(Value))) NA_real_ else median(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::complete(
    income,
    year = years_all,
    Indicator = factor(ind_levels, levels = ind_levels),
    fill = list(n = 0, Median = NA_real_)
  ) %>%
  arrange(income, Indicator, year)

plot_panel <- function(sum_df_one, panel_label, show_legend, show_axis_titles) {
  BASE <- 14
  hdi_breaks <- seq(0, 1, by = 0.2)
  sum_df_one <- sum_df_one %>% mutate(y_plot = ifelse(Indicator == "HDI", Median * 100, Median))
  
  p <- ggplot() +
    geom_line(
      data = dplyr::filter(sum_df_one, Indicator != "HDI"),
      aes(x = year, y = y_plot, group = Indicator, color = Indicator),
      linewidth = 0.9, na.rm = TRUE
    ) +
    geom_point(
      data = dplyr::filter(sum_df_one, Indicator != "HDI"),
      aes(x = year, y = y_plot, color = Indicator),
      size = 2.1, na.rm = TRUE
    ) +
    geom_line(
      data = dplyr::filter(sum_df_one, Indicator == "HDI"),
      aes(x = year, y = y_plot, group = Indicator, color = Indicator),
      linewidth = 1.0, linetype = "dashed", na.rm = TRUE
    ) +
    geom_point(
      data = dplyr::filter(sum_df_one, Indicator == "HDI"),
      aes(x = year, y = y_plot, color = Indicator),
      size = 2.1, na.rm = TRUE
    ) +
    scale_x_continuous(breaks = years_all) +
    scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 20),
      name = "Score",
      sec.axis = sec_axis(~ . / 100, name = "HDI", breaks = hdi_breaks, labels = sprintf("%.1f", hdi_breaks))
    ) +
    labs(title = panel_label, x = "Year") +
    theme_classic(base_size = BASE) +
    theme(
      axis.ticks = element_blank(),
      legend.title = element_blank(),
      legend.direction = "vertical",
      axis.text.x = element_text(size = BASE - 1, angle = 45, hjust = 1, vjust = 1),
      axis.text.y = element_text(size = BASE - 1),
      axis.text.y.right = element_text(size = BASE - 1, angle = 90, vjust = 0.5),
      axis.title.y.right = element_text(angle = 90),
      legend.text = element_text(size = BASE - 1),
      plot.title = element_text(size = BASE, face = "bold", hjust = 0.5, margin = margin(b = 6))
    )
  
  if (!show_legend) p <- p + theme(legend.position = "none") else p <- p + theme(legend.position = "right")
  if (!show_axis_titles) p <- p + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.title.y.right = element_blank())
  p
}

labels_income <- c(
  "Low" = "Low income",
  "Lower-middle" = "Lower-middle income",
  "Upper-middle" = "Upper-middle income",
  "High" = "High income"
)

p_low <- plot_panel(income_sum %>% filter(income == "Low"), labels_income["Low"], TRUE, TRUE)
p_lm  <- plot_panel(income_sum %>% filter(income == "Lower-middle"), labels_income["Lower-middle"], FALSE, FALSE)
p_um  <- plot_panel(income_sum %>% filter(income == "Upper-middle"), labels_income["Upper-middle"], FALSE, FALSE)
p_hi  <- plot_panel(income_sum %>% filter(income == "High"), labels_income["High"], FALSE, FALSE)

fig_income4 <- (p_low + p_lm) / (p_um + p_hi) +
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "right", legend.direction = "vertical")

print(fig_income4)

ggsave(
  filename = file.path(out_dir, "Figure1_Income4_with_HDI.tiff"),
  plot = fig_income4,
  device = "tiff",
  dpi = 300,
  width = 12,
  height = 9,
  units = "in",
  compression = "lzw"
)

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Income_medians")
income_excel <- income_sum %>%
  mutate(
    Group = as.character(income),
    Group_label = dplyr::recode(Group, !!!as.list(labels_income)),
    Indicator = as.character(Indicator)
  ) %>%
  select(Group, Group_label, year, Indicator, n, Median)

openxlsx::writeData(wb, "Income_medians", income_excel)
openxlsx::saveWorkbook(wb, file = file.path(out_dir, "Figure1_Income_medians.xlsx"), overwrite = TRUE)

message("Saved to: ", normalizePath(out_dir, winslash = "/"))