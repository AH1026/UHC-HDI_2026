pkgs <- c("dplyr", "tidyr", "openxlsx", "readxl", "tibble")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

out_dir <- "output：Table１"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

in_file <- "UHC_HDI_dataset.xlsx"
if (!file.exists(in_file)) in_file <- "/mnt/data/UHC_HDI_dataset.xlsx"
if (!file.exists(in_file)) stop("Excel file not found: ", in_file, "\ngetwd() = ", getwd())

sheets <- readxl::excel_sheets(in_file)
sheet_use <- if ("dataset" %in% sheets) "dataset" else sheets[1]

df_long <- readxl::read_excel(in_file, sheet = sheet_use) %>%
  dplyr::select(-starts_with("Unnamed"))

if (!("year" %in% names(df_long))) {
  stop("Column 'year' was not found. Available columns: ", paste(names(df_long), collapse = ", "))
}

indicators <- c(
  "UHC SCI", "RMNCH", "ID", "NCD", "SCA",
  "HDI", "GNI index", "Education index", "Life expectancy index",
  "Control of Corruption", "Population ages 65+ (%)", "Total fertility rate",
  "Urban population (%)"
)

missing_cols <- setdiff(indicators, names(df_long))
if (length(missing_cols) > 0) stop("Required columns are missing: ", paste(missing_cols, collapse = ", "))

df_long <- df_long %>%
  mutate(across(all_of(indicators), ~ suppressWarnings(as.numeric(.))))

years_use <- sort(unique(df_long$year))

digits_map <- c(
  "UHC SCI" = 1, "RMNCH" = 1, "ID" = 1, "NCD" = 1, "SCA" = 1,
  "Population ages 65+ (%)" = 1, "Urban population (%)" = 1,
  "HDI" = 2, "GNI index" = 2, "Education index" = 2, "Life expectancy index" = 2,
  "Control of Corruption" = 2, "Total fertility rate" = 2
)

fmt_num <- function(x, digits) formatC(round(x, digits), format = "f", digits = digits)

fmt_median_iqr <- function(x, digits) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return("")
  q <- as.numeric(stats::quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE, type = 7))
  paste0(fmt_num(q[2], digits), " [", fmt_num(q[1], digits), ", ", fmt_num(q[3], digits), "]")
}

digits_df <- tibble::tibble(
  Indicator = names(digits_map),
  digits = as.integer(unname(digits_map))
)

long2 <- df_long %>%
  select(year, all_of(indicators)) %>%
  pivot_longer(cols = all_of(indicators), names_to = "Indicator", values_to = "value") %>%
  left_join(digits_df, by = "Indicator")

if (any(is.na(long2$digits))) {
  stop("Some indicators do not have digit settings in digits_map: ",
       paste(unique(long2$Indicator[is.na(long2$digits)]), collapse = ", "))
}

summ <- long2 %>%
  group_by(Indicator, year, digits) %>%
  summarise(
    n = sum(!is.na(value)),
    Median_IQR = fmt_median_iqr(value, digits[1]),
    .groups = "drop"
  )

wide_n <- summ %>% select(Indicator, year, n) %>% pivot_wider(names_from = year, values_from = n)
wide_m <- summ %>% select(Indicator, year, Median_IQR) %>% pivot_wider(names_from = year, values_from = Median_IQR)

out <- tibble::tibble(Indicator = indicators) %>%
  left_join(wide_n, by = "Indicator") %>%
  left_join(wide_m, by = "Indicator", suffix = c("_n", "_m"))

names(out) <- sub("^([0-9]{4})$", "\\1_n", names(out))
names(out) <- sub("^([0-9]{4})\\.1$", "\\1_m", names(out))

col_order <- c("Indicator")
for (yy in years_use) col_order <- c(col_order, paste0(yy, "_n"), paste0(yy, "_m"))

out <- out %>%
  mutate(Indicator = factor(Indicator, levels = indicators)) %>%
  arrange(Indicator) %>%
  select(all_of(col_order)) %>%
  mutate(Indicator = as.character(Indicator))

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Table1_Descriptive")

header1 <- c("", "Indicator")
for (yy in years_use) header1 <- c(header1, yy, "")
header2 <- c("", "")
for (yy in years_use) header2 <- c(header2, "n", "Median [Q1, Q3]")

openxlsx::writeData(wb, "Table1_Descriptive", x = as.data.frame(t(header1)), startRow = 2, colNames = FALSE)
openxlsx::writeData(wb, "Table1_Descriptive", x = as.data.frame(t(header2)), startRow = 3, colNames = FALSE)
openxlsx::writeData(wb, "Table1_Descriptive", x = out, startRow = 4, colNames = FALSE)

for (i in seq_along(years_use)) {
  c1 <- 3 + (i - 1) * 2
  c2 <- c1 + 1
  openxlsx::mergeCells(wb, "Table1_Descriptive", cols = c1:c2, rows = 2)
}

style_head  <- openxlsx::createStyle(textDecoration = "bold", halign = "center", valign = "center", border = "Bottom")
style_head2 <- openxlsx::createStyle(textDecoration = "bold", halign = "center", valign = "center")

openxlsx::addStyle(wb, "Table1_Descriptive", style = style_head,  rows = 2, cols = 1:(2 + 2 * length(years_use)), gridExpand = TRUE)
openxlsx::addStyle(wb, "Table1_Descriptive", style = style_head2, rows = 3, cols = 1:(2 + 2 * length(years_use)), gridExpand = TRUE)

openxlsx::setColWidths(wb, "Table1_Descriptive", cols = 1, widths = 2)
openxlsx::setColWidths(wb, "Table1_Descriptive", cols = 2, widths = 30)
openxlsx::setColWidths(wb, "Table1_Descriptive", cols = 3:(2 + 2 * length(years_use)), widths = 16)

openxlsx::freezePane(wb, "Table1_Descriptive", firstRow = TRUE)

out_file <- file.path(out_dir, "Table1_Descriptive.xlsx")
openxlsx::saveWorkbook(wb, out_file, overwrite = TRUE)

message("Input sheet: ", sheet_use)
message("Saved: ", normalizePath(out_file, winslash = "/"))