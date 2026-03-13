# ==============================
# ヒストグラム作成（UHCSCI, RMNCH, ID, NCD, SCA, HDI）
# 1変数ずつTIFFで保存
# ==============================

# ---- 必要パッケージ ----
pkgs <- c("readxl", "dplyr", "ggplot2")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---- 出力先フォルダ ----
out_dir <- "output：histogram"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 入力ファイル ----
in_file <- "UHC_HDI_dataset.xlsx"
if (!file.exists(in_file)) in_file <- "/mnt/data/UHC_HDI_dataset.xlsx"
if (!file.exists(in_file)) stop("Excel file not found: ", in_file, "\ngetwd() = ", getwd())

# ---- シート確認 ----
sheets <- readxl::excel_sheets(in_file)
sheet_use <- if ("dataset" %in% sheets) "dataset" else sheets[1]

# ---- データ読込 ----
df <- readxl::read_excel(in_file, sheet = sheet_use)

# ---- 列名確認 ----
required_cols_original <- c("UHC SCI", "RMNCH", "ID", "NCD", "SCA", "HDI")
missing_cols <- setdiff(required_cols_original, names(df))
if (length(missing_cols) > 0) {
  stop("以下の列が見つかりません: ", paste(missing_cols, collapse = ", "))
}

# ---- UHC SCI を UHCSCI に変更 ----
df <- df %>%
  dplyr::rename(UHCSCI = `UHC SCI`)

# ---- 使用変数 ----
vars_to_plot <- c("UHCSCI", "RMNCH", "ID", "NCD", "SCA", "HDI")

# ---- 数値化 ----
df <- df %>%
  mutate(across(all_of(vars_to_plot), ~ suppressWarnings(as.numeric(.))))

# ---- 保存先ファイル ----
plot_specs <- list(
  list(var = "UHCSCI", out_plot = file.path(out_dir, "UHCSCI.tiff")),
  list(var = "RMNCH",  out_plot = file.path(out_dir, "RMNCH.tiff")),
  list(var = "ID",     out_plot = file.path(out_dir, "ID.tiff")),
  list(var = "NCD",    out_plot = file.path(out_dir, "NCD.tiff")),
  list(var = "SCA",    out_plot = file.path(out_dir, "SCA.tiff")),
  list(var = "HDI",    out_plot = file.path(out_dir, "HDI.tiff"))
)

# ---- ヒストグラム作成関数 ----
make_histogram <- function(data, var_name, out_plot, bins = 30) {
  
  plot_df <- data %>%
    dplyr::select(all_of(var_name)) %>%
    dplyr::filter(!is.na(.data[[var_name]]))
  
  if (nrow(plot_df) == 0) {
    warning("変数 ", var_name, " には有効なデータがありません。スキップします。")
    return(NULL)
  }
  
  p <- ggplot(plot_df, aes(x = .data[[var_name]])) +
    geom_histogram(
      bins = bins,
      fill = "skyblue",
      color = "black",
      linewidth = 0.3
    ) +
    labs(
      title = paste0("Histogram of ", var_name),
      x = var_name,
      y = "Frequency"
    ) +
    theme_bw(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title = element_text(face = "bold")
    )
  
  ggsave(
    filename = out_plot,
    plot = p,
    width = 7,
    height = 5,
    units = "in",
    dpi = 300,
    compression = "lzw"
  )
  
  message("Saved: ", out_plot)
  return(p)
}

# ---- 6変数を順番に保存 ----
plot_list <- lapply(plot_specs, function(x) {
  make_histogram(
    data = df,
    var_name = x$var,
    out_plot = x$out_plot,
    bins = 30
  )
})

# ---- 完了メッセージ ----
message("すべてのヒストグラム作成が完了しました。")
message("出力先フォルダ: ", normalizePath(out_dir, winslash = "/", mustWork = FALSE))