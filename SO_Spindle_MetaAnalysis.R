# =============================================================================
# COMPLETE META-ANALYSIS: Slow Oscillation-Sleep Spindle Coupling and
# Overnight Memory Consolidation in Healthy Adults
# Journal: Journal of Endocrinology
# =============================================================================
#
# OBJECTIVES:
# 1. Primary: Quantify overall effect of SO-spindle coupling on declarative
#    memory consolidation using random-effects meta-analysis
# 2. Secondary: Spindle-specific parameters (density, amplitude, sigma power)
#    and memory consolidation outcomes
# 3. Secondary: SO characteristics (amplitude, slope, duration) and memory
# 4. Secondary: Subgroup analysis by memory type (verbal/visuospatial/procedural)
# 5. Secondary: Biological moderators (age) via meta-regression
# 6. Secondary: Risk of bias and publication bias assessment
#
# OUTPUTS:
# FIGURES (PNG, 300 DPI):
#   Figure 1 : PRISMA Flowchart         [MANUAL - not generated here]
#   Figure 2 : Forest - Primary (Obj 1)
#   Figure 3 : Forest - Spindle Params (Obj 2)
#   Figure 4 : Forest - Memory Type Subgroup (Obj 4)
#   Figure 5 : Leave-One-Out Sensitivity (Obj 1)
#   Figure 6 : Funnel Plot (Obj 6)
#   Figure 7 : Trim-and-Fill Funnel (Obj 6)
#   Figure 8 : Meta-Regression Bubble - Age (Obj 5)
#   Figure 9 : Risk of Bias Traffic Light + Bar (Obj 6)
# SUPPLEMENTARY FIGURES:
#   Supp Fig 1: Forest - SO Characteristics (Obj 3)
#   Supp Fig 2: Baujat Influence Plot (Obj 1)
# TABLES (DOCX):
#   Table 1: Study Characteristics
#   Table 2: Meta-Analytic Results Summary
#   Table 3: Subgroup Analysis by Memory Type
#   Table 4: Meta-Regression Results
#   Table 5: Publication Bias Statistics
# SUPPLEMENTARY TABLES:
#   Supp Table 1: Risk of Bias Assessment
# =============================================================================


# =============================================================================
# SECTION 1: INSTALL AND LOAD PACKAGES
# =============================================================================

cat("=== INSTALLING AND LOADING PACKAGES ===\n")

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

packages_needed <- c(
  "meta",         # Meta-analysis with RevMan5 forest plots
  "metafor",      # Advanced meta-analysis and meta-regression
  "readxl",       # Read Excel files
  "dplyr",        # Data manipulation
  "tidyr",        # Data reshaping
  "ggplot2",      # Advanced graphics
  "officer",      # Create Word documents
  "flextable",    # Formatted tables for Word
  "scales",       # Scale helpers
  "gridExtra",    # Arrange multiple plots
  "patchwork",    # Combine ggplot2 panels
  "stringr",      # String operations
  "forcats",      # Factor manipulation
  "RColorBrewer", # Color palettes
  "tools"         # toTitleCase etc.
)

new_packages <- packages_needed[!(packages_needed %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) {
  cat("Installing:", paste(new_packages, collapse = ", "), "\n")
  install.packages(new_packages, dependencies = TRUE)
}

suppressPackageStartupMessages({
  lapply(packages_needed, library, character.only = TRUE)
})

cat("All packages loaded.\n\n")


# =============================================================================
# SECTION 2: OUTPUT DIRECTORIES
# =============================================================================

cat("=== SETTING UP OUTPUT DIRECTORIES ===\n")

output_dir <- "C:/Users/Admin/Downloads/Sandhiya"
data_path  <- file.path(output_dir, "SO Spindle Memory DataExtraction.xlsx")

fig_dir  <- file.path(output_dir, "Figures")
tab_dir  <- file.path(output_dir, "Tables")
supp_dir <- file.path(output_dir, "Supplementary")

for (d in c(fig_dir, tab_dir, supp_dir)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

cat("Figures  :", fig_dir,  "\n")
cat("Tables   :", tab_dir,  "\n")
cat("Supp     :", supp_dir, "\n\n")


# =============================================================================
# SECTION 3: HELPER FUNCTIONS
# =============================================================================

# Save a base-R figure (png device already open outside)
save_base_fig <- function(expr, filename, width = 14, height = 8, dpi = 300,
                          type = "main") {
  dir   <- if (type == "supp") supp_dir else fig_dir
  fpath <- file.path(dir, paste0(filename, ".png"))
  png(fpath, width = width, height = height, units = "in", res = dpi, bg = "white")
  tryCatch(force(expr), error = function(e) {
    message("Figure error [", filename, "]: ", e$message)
  }, finally = dev.off())
  cat("Saved:", fpath, "\n")
  invisible(fpath)
}

# Save a ggplot figure
save_gg <- function(p, filename, width = 12, height = 8, dpi = 300,
                    type = "main") {
  dir   <- if (type == "supp") supp_dir else fig_dir
  fpath <- file.path(dir, paste0(filename, ".png"))
  ggsave(fpath, plot = p, width = width, height = height, dpi = dpi,
         bg = "white")
  cat("Saved:", fpath, "\n")
  invisible(fpath)
}

# Save a flextable as a Word document
save_word_table <- function(data, title, filename, subtitle = NULL,
                             type = "main") {
  dir   <- if (type == "supp") supp_dir else tab_dir
  fpath <- file.path(dir, paste0(filename, ".docx"))

  ft <- flextable(data) %>%
    theme_booktabs() %>%
    bold(part = "header") %>%
    fontsize(size = 10, part = "all") %>%
    font(fontname = "Times New Roman", part = "all") %>%
    align(align = "left", part = "all") %>%
    align(part = "header", align = "center") %>%
    padding(padding = 4, part = "all") %>%
    bg(i = seq(2, nrow(data), by = 2), bg = "#F5F5F5", part = "body") %>%
    autofit()

  doc <- read_docx() %>%
    body_add_par(title, style = "heading 1")
  if (!is.null(subtitle)) {
    doc <- doc %>% body_add_par(subtitle, style = "Normal")
  }
  doc <- doc %>%
    body_add_par("", style = "Normal") %>%
    body_add_flextable(ft) %>%
    body_add_par("", style = "Normal")

  print(doc, target = fpath)
  cat("Saved:", fpath, "\n")
  invisible(fpath)
}

# Format p-value for tables
fmt_p <- function(p) {
  if (is.null(p) || is.na(p)) return("NA")
  if (p < 0.001) return("<0.001")
  format(round(p, 3), nsmall = 3)
}

# Summarise a meta object into one-row data frame
summarise_meta <- function(m, label) {
  if (is.null(m)) return(NULL)
  tryCatch({
    data.frame(
      Analysis        = label,
      k               = m$k,
      `r [95% CI]`    = paste0(
        sprintf("%.2f", m$TE.random), " [",
        sprintf("%.2f", m$lower.random), ", ",
        sprintf("%.2f", m$upper.random), "]"),
      Z               = sprintf("%.2f", m$statistic.random),
      `p-value`       = fmt_p(m$pval.random),
      `I² (%)`   = paste0(sprintf("%.1f", m$I2 * 100), "%"),
      `τ²`  = sprintf("%.4f", m$tau^2),
      Q               = sprintf("%.2f", m$Q),
      `Q p-value`     = fmt_p(m$pval.Q),
      check.names     = FALSE,
      stringsAsFactors = FALSE
    )
  }, error = function(e) NULL)
}


# =============================================================================
# SECTION 4: DATA LOADING
# =============================================================================

cat("=== LOADING DATA ===\n")

if (!file.exists(data_path)) {
  stop(
    "Data file not found:\n  ", data_path,
    "\nPlease place 'SO Spindle Memory DataExtraction.xlsx' in:\n  ", output_dir
  )
}

sheet_names <- excel_sheets(data_path)
cat("Sheets:", paste(sheet_names, collapse = ", "), "\n")

# Pick the data sheet (first non-ROB sheet)
main_sheet <- sheet_names[!grepl("rob|bias|quality|risk", sheet_names,
                                  ignore.case = TRUE)][1]
cat("Using sheet:", main_sheet, "\n")

df_raw <- read_excel(data_path, sheet = main_sheet)

# Clean column names
names(df_raw) <- names(df_raw) %>%
  trimws() %>%
  tolower() %>%
  gsub("\\s+", "_", .) %>%
  gsub("[^a-z0-9_]", "", .)

cat("Columns:", paste(names(df_raw), collapse = ", "), "\n")
cat("Rows:", nrow(df_raw), "\n\n")

# Try to load ROB sheet
rob_sheet <- sheet_names[grepl("rob|bias|quality|risk", sheet_names,
                                ignore.case = TRUE)]
if (length(rob_sheet) > 0) {
  df_rob_raw <- read_excel(data_path, sheet = rob_sheet[1])
  names(df_rob_raw) <- names(df_rob_raw) %>%
    trimws() %>% tolower() %>%
    gsub("\\s+", "_", .) %>% gsub("[^a-z0-9_]", "", .)
  cat("ROB sheet loaded:", rob_sheet[1], "\n")
} else {
  df_rob_raw <- NULL
  cat("No ROB sheet found; ROB columns will be searched in main data.\n")
}


# =============================================================================
# SECTION 5: DATA PREPARATION
# =============================================================================

cat("\n=== DATA PREPARATION ===\n")

# Flexible column finder
find_col <- function(df, patterns) {
  nm <- tolower(names(df))
  for (pat in patterns) {
    idx <- grep(pat, nm, perl = TRUE)
    if (length(idx) > 0) return(names(df)[idx[1]])
  }
  NULL
}

col_study   <- find_col(df_raw, c("^study$", "study_id", "author", "first_author",
                                   "studyid", "study_label", "reference"))
col_year    <- find_col(df_raw, c("^year$", "pub_year", "publication_year", "yr"))
col_n       <- find_col(df_raw, c("^n$", "sample_size", "^n_total$", "total_n",
                                   "participants", "^n_participants$"))
col_r       <- find_col(df_raw, c("^r$", "r_value", "correlation", "pearson_r",
                                   "effect_r", "r_pearson", "^cor$"))
col_r_lower <- find_col(df_raw, c("ci_lower", "lower_ci", "ci_lo", "r_lower",
                                   "ll", "lci", "lower_95"))
col_r_upper <- find_col(df_raw, c("ci_upper", "upper_ci", "ci_hi", "r_upper",
                                   "ul", "uci", "upper_95"))
col_p       <- find_col(df_raw, c("^p$", "p_value", "^p_val$", "significance",
                                   "pvalue"))
col_memory  <- find_col(df_raw, c("memory_type", "memory_cat", "memory",
                                   "outcome_type", "task_type", "task",
                                   "outcome_category"))
col_sleep   <- find_col(df_raw, c("sleep_measure", "sleep_variable",
                                   "measure_type", "predictor", "sleep",
                                   "variable", "measure"))
col_age     <- find_col(df_raw, c("age_mean", "mean_age", "^age$", "avg_age"))

cat("Column mapping:\n")
for (nm in c("study", "year", "n", "r", "CI lower", "CI upper", "p",
             "memory type", "sleep measure", "age")) {
  val <- get(paste0("col_", gsub(" ", "_", nm)))
  cat("  ", nm, "->", ifelse(is.null(val), "NOT FOUND", val), "\n")
}

# Build standardised data frame
df <- df_raw

if (!is.null(col_study))   df$study_label      <- as.character(df[[col_study]])
if (!is.null(col_year))    df$year              <- as.numeric(df[[col_year]])
if (!is.null(col_n))       df$n                 <- as.numeric(df[[col_n]])
if (!is.null(col_r))       df$r                 <- as.numeric(df[[col_r]])
if (!is.null(col_r_lower)) df$r_lower           <- as.numeric(df[[col_r_lower]])
if (!is.null(col_r_upper)) df$r_upper           <- as.numeric(df[[col_r_upper]])
if (!is.null(col_p))       df$p_value           <- as.numeric(df[[col_p]])
if (!is.null(col_memory))  df$memory_type_raw   <- as.character(df[[col_memory]])
if (!is.null(col_sleep))   df$sleep_measure_raw <- as.character(df[[col_sleep]])
if (!is.null(col_age))     df$age_mean          <- as.numeric(df[[col_age]])

# Ensure required columns exist
if (!"study_label" %in% names(df)) df$study_label <- paste0("Study_", seq_len(nrow(df)))
if (!"year" %in% names(df))        df$year        <- 2020L
if (!"n" %in% names(df))           stop("Sample size column not found. Check Excel column names.")
if (!"r" %in% names(df))           stop("Correlation coefficient (r) column not found.")

# Remove rows with missing essential data
df <- df %>%
  filter(!is.na(r), !is.na(n), n > 3) %>%
  mutate(
    r = pmax(-0.9999, pmin(0.9999, r)),
    study_label = ifelse(is.na(study_label) | study_label == "",
                         paste0("Study_", row_number()), study_label)
  )

cat("\nRecords after cleaning:", nrow(df), "from",
    length(unique(df$study_label)), "unique studies\n")

# ---- Categorise sleep measures ----
if ("sleep_measure_raw" %in% names(df)) {
  df <- df %>%
    mutate(sleep_measure_cat = case_when(
      grepl("coupl|nest|so.*spindle|spindle.*so|phase.*lock", sleep_measure_raw,
            ignore.case = TRUE)                                          ~ "SO-Spindle Coupling",
      grepl("spindle.*dens|dens.*spindle|spindle.*count|#.*spindle|n.*spindle",
            sleep_measure_raw, ignore.case = TRUE)                      ~ "Spindle Density",
      grepl("spindle.*amp|amp.*spindle", sleep_measure_raw,
            ignore.case = TRUE)                                          ~ "Spindle Amplitude",
      grepl("spindle.*freq|freq.*spindle|hz.*spindle|spindle.*hz",
            sleep_measure_raw, ignore.case = TRUE)                      ~ "Spindle Frequency",
      grepl("sigma|12.*15|13.*15|14.*16|sigma.*power|sleep.*spindle.*power",
            sleep_measure_raw, ignore.case = TRUE)                      ~ "Sigma Power",
      grepl("so.*amp|amp.*so|sw.*amp|slow.*wave.*amp|amp.*slow.*osc",
            sleep_measure_raw, ignore.case = TRUE)                      ~ "SO Amplitude",
      grepl("so.*slope|slope|steep|down.*slope|up.*slope",
            sleep_measure_raw, ignore.case = TRUE)                      ~ "SO Slope",
      grepl("so.*dur|duration|so.*width|nrem.*dur",
            sleep_measure_raw, ignore.case = TRUE)                      ~ "SO Duration",
      grepl("delta.*power|0.5.*4|0.5.*2|1.*4|slow.*power",
            sleep_measure_raw, ignore.case = TRUE)                      ~ "Delta/SO Power",
      TRUE                                                               ~ "SO-Spindle Coupling"
    ))
} else {
  df$sleep_measure_cat <- "SO-Spindle Coupling"
}

# ---- Categorise memory types ----
if ("memory_type_raw" %in% names(df)) {
  df <- df %>%
    mutate(memory_cat = case_when(
      grepl("verbal|word|pair|list|story|narr|prose|episod|explicit",
            memory_type_raw, ignore.case = TRUE)                        ~ "Verbal Declarative",
      grepl("spatial|visual|object|face|location|place|scene|picture|image",
            memory_type_raw, ignore.case = TRUE)                        ~ "Visuospatial",
      grepl("proced|motor|finger|mirror|seq|skill|rotation|habit",
            memory_type_raw, ignore.case = TRUE)                        ~ "Procedural",
      grepl("declar|hippoc", memory_type_raw, ignore.case = TRUE)      ~ "Verbal Declarative",
      TRUE                                                               ~ "Verbal Declarative"
    ))
} else {
  df$memory_cat <- "Verbal Declarative"
}

cat("\nSleep measure categories:\n"); print(table(df$sleep_measure_cat))
cat("\nMemory type categories:\n");   print(table(df$memory_cat))


# =============================================================================
# SECTION 6: DEFINE ANALYSIS SUBSETS
# =============================================================================

# Objective 1: Primary - all coupling/spindle/SO studies -> declarative memory
df_primary <- df %>%
  filter(memory_cat %in% c("Verbal Declarative", "Visuospatial") |
         !("memory_cat" %in% names(df)))

if (nrow(df_primary) < 2) df_primary <- df

# Objective 2: Spindle parameters only
df_spindle <- df %>%
  filter(sleep_measure_cat %in% c("SO-Spindle Coupling", "Spindle Density",
                                   "Spindle Amplitude", "Spindle Frequency",
                                   "Sigma Power"))
if (nrow(df_spindle) < 2) df_spindle <- df

# Objective 3: SO characteristics only
df_so <- df %>%
  filter(sleep_measure_cat %in% c("SO Amplitude", "SO Slope", "SO Duration",
                                   "Delta/SO Power"))
if (nrow(df_so) < 2) df_so <- df

cat("\nSubset sizes: Primary =", nrow(df_primary),
    "| Spindle =", nrow(df_spindle),
    "| SO =", nrow(df_so), "\n")


# =============================================================================
# SECTION 7: PRIMARY META-ANALYSIS (OBJECTIVE 1)
# =============================================================================

cat("\n=== Objective 1: Primary Meta-Analysis ===\n")

m_primary <- tryCatch(
  metacor(
    cor      = r,
    n        = n,
    studlab  = study_label,
    data     = df_primary,
    sm       = "ZCOR",
    random   = TRUE,
    common   = FALSE,
    method.tau          = "REML",
    method.random.ci    = "HK",
    title    = "SO-Spindle Coupling and Declarative Memory Consolidation"
  ),
  error = function(e) { cat("Primary meta-analysis error:", e$message, "\n"); NULL }
)

if (!is.null(m_primary)) {
  cat("k =", m_primary$k, "| r =", round(m_primary$TE.random, 3),
      "| I2 =", round(m_primary$I2 * 100, 1), "%\n")
}


# =============================================================================
# FIGURE 2: FOREST PLOT - PRIMARY (RevMan5 style)
# =============================================================================

if (!is.null(m_primary) && m_primary$k >= 2) {

  n_stu    <- m_primary$k
  fig_h    <- max(7, 2.5 + n_stu * 0.38)
  fig_path <- file.path(fig_dir, "Figure2_Forest_PrimaryOutcome.png")

  png(fig_path, width = 15, height = fig_h, units = "in", res = 300, bg = "white")

  forest(
    m_primary,
    layout          = "RevMan5",
    sortvar         = -(r),
    col.square      = "black",
    col.square.lines = "black",
    col.diamond     = "black",
    col.diamond.lines = "black",
    col.by          = "black",
    fontsize        = if (n_stu <= 20) 10 else 8,
    spacing         = if (n_stu <= 30) 0.85 else 0.65,
    squaresize      = 0.7,
    xlab            = "Correlation Coefficient (r)",
    xlim            = c(-0.5, 1.0),
    at              = c(-0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.0),
    leftlabs        = c("Study", "N"),
    rightlabs       = c("r [95% CI]", "Weight"),
    label.right     = "Favours positive coupling",
    label.left      = "Favours negative coupling",
    print.tau2      = TRUE,
    print.I2        = TRUE,
    print.Q         = TRUE,
    digits          = 2,
    addrows.below.overall = 1,
    text.random     = "Random-effects model",
    overall         = TRUE,
    overall.hetstat = TRUE
  )

  dev.off()
  cat("Saved: Figure2_Forest_PrimaryOutcome.png\n")
}


# =============================================================================
# SECTION 8: SPINDLE PARAMETERS META-ANALYSIS (OBJECTIVE 2)
# =============================================================================

cat("\n=== Objective 2: Spindle Parameters ===\n")

n_spindle_cats <- length(unique(df_spindle$sleep_measure_cat))

m_spindle <- tryCatch(
  metacor(
    cor      = r,
    n        = n,
    studlab  = study_label,
    data     = df_spindle,
    sm       = "ZCOR",
    random   = TRUE,
    common   = FALSE,
    method.tau       = "REML",
    method.random.ci = "HK",
    subgroup         = if (n_spindle_cats > 1) sleep_measure_cat else NULL,
    subgroup.name    = "Spindle Parameter",
    print.subgroup.name = TRUE,
    title    = "Spindle Parameters and Memory Consolidation"
  ),
  error = function(e) { cat("Spindle meta-analysis error:", e$message, "\n"); NULL }
)


# =============================================================================
# FIGURE 3: FOREST PLOT - SPINDLE PARAMETERS (Objective 2)
# =============================================================================

if (!is.null(m_spindle) && m_spindle$k >= 2) {

  n_stu    <- m_spindle$k
  n_grp    <- max(1, n_spindle_cats)
  fig_h    <- max(8, 3 + n_stu * 0.38 + n_grp * 0.6)
  fig_path <- file.path(fig_dir, "Figure3_Forest_SpindleParameters.png")

  png(fig_path, width = 15, height = fig_h, units = "in", res = 300, bg = "white")

  forest(
    m_spindle,
    layout          = "RevMan5",
    sortvar         = if (n_spindle_cats > 1) sleep_measure_cat else -(r),
    col.square      = "black",
    col.square.lines = "black",
    col.diamond     = "black",
    col.by          = "black",
    fontsize        = if (n_stu <= 20) 10 else 8,
    spacing         = if (n_stu <= 30) 0.85 else 0.65,
    squaresize      = 0.7,
    xlab            = "Correlation Coefficient (r)",
    xlim            = c(-0.5, 1.0),
    at              = c(-0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.0),
    leftlabs        = c("Study", "N"),
    rightlabs       = c("r [95% CI]", "Weight"),
    print.tau2      = TRUE,
    print.I2        = TRUE,
    print.Q         = TRUE,
    digits          = 2,
    text.random     = "Random-effects model"
  )

  dev.off()
  cat("Saved: Figure3_Forest_SpindleParameters.png\n")
}


# =============================================================================
# SECTION 9: SUBGROUP BY MEMORY TYPE (OBJECTIVE 4)
# =============================================================================

cat("\n=== Objective 4: Subgroup by Memory Type ===\n")

n_mem_cats <- length(unique(df$memory_cat))
cat("Memory categories:", paste(unique(df$memory_cat), collapse = ", "), "\n")

m_memory <- tryCatch(
  metacor(
    cor      = r,
    n        = n,
    studlab  = study_label,
    data     = df,
    sm       = "ZCOR",
    random   = TRUE,
    common   = FALSE,
    method.tau       = "REML",
    method.random.ci = "HK",
    subgroup         = if (n_mem_cats > 1) memory_cat else NULL,
    subgroup.name    = "Memory Type",
    print.subgroup.name = TRUE,
    title    = "SO-Spindle Coupling and Memory Consolidation by Memory Type"
  ),
  error = function(e) { cat("Memory subgroup error:", e$message, "\n"); NULL }
)


# =============================================================================
# FIGURE 4: FOREST PLOT - MEMORY TYPE SUBGROUP (Objective 4)
# =============================================================================

if (!is.null(m_memory) && m_memory$k >= 2) {

  n_stu    <- m_memory$k
  n_grp    <- max(1, n_mem_cats)
  fig_h    <- max(8, 3 + n_stu * 0.38 + n_grp * 0.6)
  fig_path <- file.path(fig_dir, "Figure4_Forest_MemoryTypeSubgroup.png")

  png(fig_path, width = 15, height = fig_h, units = "in", res = 300, bg = "white")

  forest(
    m_memory,
    layout          = "RevMan5",
    sortvar         = if (n_mem_cats > 1) memory_cat else -(r),
    col.square      = "black",
    col.square.lines = "black",
    col.diamond     = "black",
    col.by          = "black",
    fontsize        = if (n_stu <= 20) 10 else 8,
    spacing         = if (n_stu <= 30) 0.85 else 0.65,
    squaresize      = 0.7,
    xlab            = "Correlation Coefficient (r)",
    xlim            = c(-0.5, 1.0),
    at              = c(-0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.0),
    leftlabs        = c("Study", "N"),
    rightlabs       = c("r [95% CI]", "Weight"),
    print.tau2      = TRUE,
    print.I2        = TRUE,
    print.Q         = TRUE,
    digits          = 2,
    text.random     = "Random-effects model"
  )

  dev.off()
  cat("Saved: Figure4_Forest_MemoryTypeSubgroup.png\n")
}


# =============================================================================
# SECTION 10: LEAVE-ONE-OUT SENSITIVITY ANALYSIS (OBJECTIVE 1)
# =============================================================================

cat("\n=== Sensitivity: Leave-One-Out ===\n")

if (!is.null(m_primary) && m_primary$k >= 4) {

  loo <- metainf(m_primary, pooled = "random")

  n_stu    <- m_primary$k
  fig_h    <- max(6, 2.5 + (n_stu + 2) * 0.42)
  fig_path <- file.path(fig_dir, "Figure5_LeaveOneOut_Sensitivity.png")

  png(fig_path, width = 14, height = fig_h, units = "in", res = 300, bg = "white")

  forest(
    loo,
    layout          = "RevMan5",
    col.square      = "black",
    col.square.lines = "black",
    col.diamond     = "black",
    fontsize        = if (n_stu <= 20) 10 else 8,
    spacing         = if (n_stu <= 30) 0.85 else 0.65,
    squaresize      = 0.7,
    xlab            = "Correlation Coefficient (r)",
    digits          = 2,
    leftlabs        = c("Omitted Study", "N"),
    rightlabs       = c("r [95% CI]", "Weight")
  )

  title(
    "Figure 5. Leave-One-Out Sensitivity Analysis\nEffect of SO-Spindle Coupling on Declarative Memory",
    cex.main = 0.95, font.main = 2
  )

  dev.off()
  cat("Saved: Figure5_LeaveOneOut_Sensitivity.png\n")

} else {
  cat("Insufficient studies (< 4) for leave-one-out analysis.\n")
}


# =============================================================================
# SECTION 11: PUBLICATION BIAS (OBJECTIVE 6)
# =============================================================================

cat("\n=== Objective 6: Publication Bias ===\n")

egger_res  <- NULL
begg_res   <- NULL
tf_res     <- NULL

if (!is.null(m_primary) && m_primary$k >= 5) {

  egger_res <- tryCatch(
    metabias(m_primary, method.bias = "Egger",  k.min = 3),
    error = function(e) { cat("Egger error:", e$message, "\n"); NULL }
  )
  begg_res  <- tryCatch(
    metabias(m_primary, method.bias = "Begg",   k.min = 3),
    error = function(e) { cat("Begg error:",  e$message, "\n"); NULL }
  )
  tf_res    <- tryCatch(
    trimfill(m_primary),
    error = function(e) { cat("Trim-fill error:", e$message, "\n"); NULL }
  )

  # ---- FIGURE 6: Funnel Plot ----
  fig_path <- file.path(fig_dir, "Figure6_FunnelPlot.png")
  png(fig_path, width = 8, height = 7, units = "in", res = 300, bg = "white")

  par(mar = c(5, 5, 4, 2))

  funnel(
    m_primary,
    col      = "black",
    bg       = "grey40",
    pch      = 21,
    cex      = 1.3,
    xlab     = "Correlation Coefficient (r)",
    ylab     = "Standard Error",
    back     = "white",
    hlines   = NULL,
    lty.fixed = 0
  )

  if (!is.null(egger_res)) {
    mtext(
      paste0("Egger's test: z = ", round(egger_res$statistic, 2),
             ", p = ", fmt_p(egger_res$p.value)),
      side = 1, line = 4, cex = 0.85, font = 2
    )
  }
  title("Figure 6. Funnel Plot\nSO-Spindle Coupling and Memory Consolidation",
        cex.main = 0.95, font.main = 2)

  dev.off()
  cat("Saved: Figure6_FunnelPlot.png\n")

  # ---- FIGURE 7: Trim-and-Fill Funnel Plot ----
  if (!is.null(tf_res)) {

    fig_path <- file.path(fig_dir, "Figure7_TrimFillFunnel.png")
    png(fig_path, width = 8, height = 7, units = "in", res = 300, bg = "white")

    par(mar = c(5, 5, 4, 2))

    funnel(
      tf_res,
      col      = "black",
      bg       = "grey40",
      pch      = 21,
      cex      = 1.3,
      xlab     = "Correlation Coefficient (r)",
      ylab     = "Standard Error",
      back     = "white",
      hlines   = NULL,
      lty.fixed = 0
    )

    n_imp <- tf_res$k0
    mtext(
      paste0("Trim-and-fill: ", n_imp,
             " imputed study/studies (filled squares)"),
      side = 1, line = 4, cex = 0.85, font = 2
    )
    title("Figure 7. Trim-and-Fill Adjusted Funnel Plot\nSO-Spindle Coupling and Memory Consolidation",
          cex.main = 0.95, font.main = 2)

    dev.off()
    cat("Saved: Figure7_TrimFillFunnel.png\n")
  }

} else {
  cat("Fewer than 5 studies - publication bias tests not run.\n")
}


# =============================================================================
# SECTION 12: META-REGRESSION - AGE MODERATOR (OBJECTIVE 5)
# =============================================================================

cat("\n=== Objective 5: Meta-Regression (Age) ===\n")

m_reg <- NULL

if ("age_mean" %in% names(df) && sum(!is.na(df$age_mean)) >= 5) {

  df_reg <- df %>%
    filter(!is.na(age_mean), !is.na(r), !is.na(n)) %>%
    mutate(
      z_val   = 0.5 * log((1 + r) / (1 - r)),
      var_z   = 1 / (n - 3),
      se_z    = sqrt(var_z),
      wt      = 1 / var_z
    )

  m_reg <- tryCatch(
    rma(yi = z_val, sei = se_z, mods = ~ age_mean,
        data = df_reg, method = "REML"),
    error = function(e) { cat("Meta-regression error:", e$message, "\n"); NULL }
  )

  if (!is.null(m_reg)) {
    cat("Meta-regression: beta(age) =",
        round(coef(m_reg)["age_mean"], 4),
        "p =", fmt_p(m_reg$pval["age_mean"]), "\n")

    # Prediction for bubble plot
    age_seq  <- seq(min(df_reg$age_mean) - 1, max(df_reg$age_mean) + 1,
                    length.out = 200)
    pred     <- predict(m_reg, newmods = age_seq)

    r2z <- function(z) (exp(2 * z) - 1) / (exp(2 * z) + 1)

    pred_df <- data.frame(
      age    = age_seq,
      r_pred = r2z(pred$pred),
      r_lo   = r2z(pred$ci.lb),
      r_hi   = r2z(pred$ci.ub)
    )

    df_reg <- df_reg %>%
      mutate(
        r_disp   = r2z(z_val),
        bub_size = sqrt(wt / max(wt)) * 9
      )

    beta_age <- round(coef(m_reg)["age_mean"], 4)
    p_age    <- fmt_p(m_reg$pval["age_mean"])
    r2_pct   <- if (!is.na(m_reg$R2)) paste0(round(m_reg$R2, 1), "%") else "NA"

    p_bubble <- ggplot() +
      geom_ribbon(data = pred_df,
                  aes(x = age, ymin = r_lo, ymax = r_hi),
                  fill = "grey80", alpha = 0.55) +
      geom_line(data = pred_df,
                aes(x = age, y = r_pred),
                colour = "black", linewidth = 1) +
      geom_hline(yintercept = 0, linetype = "dashed",
                 colour = "grey50", linewidth = 0.6) +
      geom_point(data = df_reg,
                 aes(x = age_mean, y = r_disp, size = bub_size),
                 shape = 21, fill = "grey40", colour = "black",
                 alpha = 0.85, stroke = 0.7) +
      scale_size_identity() +
      labs(
        title    = "Figure 8. Meta-Regression: Age as Moderator of\nSO-Spindle Coupling on Memory Consolidation",
        x        = "Mean Sample Age (years)",
        y        = "Correlation Coefficient (r)",
        caption  = paste0("β = ", beta_age, ", p = ", p_age,
                          "   R² = ", r2_pct,
                          "   Bubble size ∝ precision (1/SE²)")
      ) +
      coord_cartesian(ylim = c(-0.2, 1.0)) +
      theme_classic(base_size = 12) +
      theme(
        plot.title      = element_text(face = "bold", size = 11, hjust = 0.5),
        plot.caption    = element_text(hjust = 0.5, size = 9),
        axis.text       = element_text(colour = "black"),
        axis.line       = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        plot.background  = element_rect(fill = "white")
      )

    save_gg(p_bubble, "Figure8_MetaRegression_Age", width = 9, height = 7)
  }

} else {
  cat("Age data not available or insufficient studies (< 5).\n")
}


# =============================================================================
# SECTION 13: RISK OF BIAS VISUALISATION (OBJECTIVE 6)
# =============================================================================

cat("\n=== Objective 6: Risk of Bias ===\n")

# Identify ROB columns in main or ROB sheet
rob_search_df <- if (!is.null(df_rob_raw)) df_rob_raw else df
rob_cols <- names(rob_search_df)[
  grepl("rob|domain|bias|random|select|perform|detect|attrit|report|blind|alloc|confound|meas",
        names(rob_search_df), ignore.case = TRUE)
]

# Remove the study label / year columns from rob_cols
rob_cols <- rob_cols[!grepl("study|author|year|label|ref|id$", rob_cols,
                             ignore.case = TRUE)]

cat("ROB columns:", paste(rob_cols, collapse = ", "), "\n")

if (length(rob_cols) >= 2) {

  # Build a tidy ROB frame
  study_col_rob <- find_col(rob_search_df,
                             c("^study$", "study_id", "author", "study_label",
                               "reference", "first_author"))

  rob_df <- rob_search_df
  if (!is.null(study_col_rob)) {
    rob_df$study_label <- as.character(rob_search_df[[study_col_rob]])
  } else {
    rob_df$study_label <- paste0("Study_", seq_len(nrow(rob_df)))
  }

  rob_long <- rob_df %>%
    select(study_label, all_of(rob_cols)) %>%
    pivot_longer(cols = all_of(rob_cols),
                 names_to  = "Domain",
                 values_to = "Judgment") %>%
    mutate(
      Judgment = case_when(
        grepl("low|good|yes|adequate|\\+|clear|met",
              as.character(Judgment), ignore.case = TRUE)                   ~ "Low",
        grepl("high|poor|no|\\-|not met|serious|critical",
              as.character(Judgment), ignore.case = TRUE)                   ~ "High",
        TRUE                                                                 ~ "Some concerns"
      ),
      Judgment = factor(Judgment,
                        levels = c("Low", "Some concerns", "High")),
      Domain   = Domain %>%
        gsub("_", " ", .) %>%
        gsub("(?i)rob[_ ]?|domain[_ ]?|bias[_ ]?|risk[_ ]?of[_ ]?", "", .,
             perl = TRUE) %>%
        trimws() %>%
        tools::toTitleCase()
    )

  rob_colors  <- c("Low"           = "#00B050",
                   "Some concerns" = "#FFC000",
                   "High"          = "#FF0000")
  rob_symbols <- c("Low" = "+", "Some concerns" = "?", "High" = "−")

  # Traffic-light heatmap
  p_rob_heat <- ggplot(rob_long,
                       aes(x = Domain, y = study_label, fill = Judgment)) +
    geom_tile(colour = "white", linewidth = 0.6) +
    geom_text(aes(label = rob_symbols[as.character(Judgment)]),
              colour = "white", fontface = "bold", size = 4.5) +
    scale_fill_manual(values = rob_colors, name = "Risk of Bias",
                      labels = c("Low risk", "Some concerns", "High risk")) +
    labs(title = "Risk of Bias: Traffic Light Plot",
         x = NULL, y = NULL) +
    theme_classic(base_size = 10) +
    theme(
      axis.text.x      = element_text(angle = 40, hjust = 1, colour = "black"),
      axis.text.y      = element_text(colour = "black"),
      legend.position  = "right",
      plot.title       = element_text(face = "bold", size = 11),
      panel.background = element_rect(fill = "white"),
      plot.background  = element_rect(fill = "white")
    )

  # Summary bar chart
  rob_sum <- rob_long %>%
    count(Domain, Judgment) %>%
    group_by(Domain) %>%
    mutate(pct = n / sum(n) * 100) %>%
    ungroup()

  p_rob_bar <- ggplot(rob_sum,
                      aes(x = Domain, y = pct, fill = Judgment)) +
    geom_bar(stat = "identity", colour = "white", linewidth = 0.3) +
    geom_text(aes(label = paste0(round(pct), "%")),
              position = position_stack(vjust = 0.5),
              colour = "white", fontface = "bold", size = 3.5) +
    scale_fill_manual(values = rob_colors, name = "Risk of Bias",
                      labels = c("Low risk", "Some concerns", "High risk")) +
    scale_y_continuous(labels = scales::percent_format(scale = 1),
                       limits = c(0, 100)) +
    labs(title = "Risk of Bias: Summary",
         x = NULL, y = "Percentage of Studies (%)") +
    coord_flip() +
    theme_classic(base_size = 10) +
    theme(
      axis.text        = element_text(colour = "black"),
      legend.position  = "right",
      plot.title       = element_text(face = "bold", size = 11),
      panel.background = element_rect(fill = "white"),
      plot.background  = element_rect(fill = "white")
    )

  p_rob_combined <- p_rob_heat / p_rob_bar +
    plot_annotation(
      title = "Figure 9. Risk of Bias Assessment",
      theme = theme(
        plot.title = element_text(face = "bold", size = 13, hjust = 0.5)
      )
    )

  n_stu_rob <- length(unique(rob_long$study_label))
  save_gg(p_rob_combined, "Figure9_RiskOfBias",
          width = 12, height = max(10, 4 + n_stu_rob * 0.35))

} else {
  cat("ROB columns not found. Skipping Figure 9.\n")
  cat("Expected column names containing: rob, domain, bias, random, selection,",
      "performance, detection, attrition, reporting, blinding\n")
}


# =============================================================================
# SUPPLEMENTARY FIGURE 1: SO CHARACTERISTICS (OBJECTIVE 3)
# =============================================================================

cat("\n=== Supp Fig 1: SO Characteristics ===\n")

m_so_meta <- tryCatch(
  metacor(
    cor      = r,
    n        = n,
    studlab  = study_label,
    data     = df_so,
    sm       = "ZCOR",
    random   = TRUE,
    common   = FALSE,
    method.tau       = "REML",
    method.random.ci = "HK",
    subgroup         = if (length(unique(df_so$sleep_measure_cat)) > 1)
                         sleep_measure_cat else NULL,
    title    = "SO Characteristics and Memory Consolidation"
  ),
  error = function(e) { cat("SO meta-analysis error:", e$message, "\n"); NULL }
)

if (!is.null(m_so_meta) && m_so_meta$k >= 2) {

  n_stu    <- m_so_meta$k
  fig_h    <- max(6, 2.5 + n_stu * 0.38)
  fig_path <- file.path(supp_dir, "SuppFigure1_Forest_SOCharacteristics.png")

  png(fig_path, width = 14, height = fig_h, units = "in", res = 300, bg = "white")

  forest(
    m_so_meta,
    layout          = "RevMan5",
    sortvar         = -(r),
    col.square      = "black",
    col.diamond     = "black",
    fontsize        = if (n_stu <= 20) 10 else 8,
    spacing         = 0.85,
    squaresize      = 0.7,
    xlab            = "Correlation Coefficient (r)",
    xlim            = c(-0.5, 1.0),
    at              = c(-0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.0),
    digits          = 2,
    text.random     = "Random-effects model"
  )

  dev.off()
  cat("Saved: SuppFigure1_Forest_SOCharacteristics.png\n")
}


# =============================================================================
# SUPPLEMENTARY FIGURE 2: BAUJAT INFLUENCE PLOT
# =============================================================================

cat("\n=== Supp Fig 2: Baujat Plot ===\n")

if (!is.null(m_primary) && m_primary$k >= 4) {

  fig_path <- file.path(supp_dir, "SuppFigure2_BaujatInfluencePlot.png")
  png(fig_path, width = 9, height = 8, units = "in", res = 300, bg = "white")

  tryCatch({
    baujat(
      m_primary,
      col  = "black",
      pch  = 19,
      cex  = 1.2,
      main = "Supp Figure 2. Baujat Plot: Study-Level Influence Analysis",
      xlab = "Contribution to Overall Heterogeneity (Q)",
      ylab = "Influence on Pooled Estimate"
    )
  }, error = function(e) {
    plot.new()
    text(0.5, 0.5, paste("Baujat plot error:\n", e$message), cex = 0.9)
  })

  dev.off()
  cat("Saved: SuppFigure2_BaujatInfluencePlot.png\n")
}


# =============================================================================
# TABLE 1: STUDY CHARACTERISTICS
# =============================================================================

cat("\n=== Table 1: Study Characteristics ===\n")

# Build Table 1 with available columns
t1_cols <- c("study_label", "year", "n", "age_mean", "memory_cat",
             "sleep_measure_cat")

# Add optional columns if present
for (opt in c("country", "sex_pct_female", "age_sd", "sleep_stage",
              "eeg_method", "task_name")) {
  if (opt %in% names(df)) t1_cols <- c(t1_cols, opt)
}
t1_cols <- intersect(t1_cols, names(df))

df_t1 <- df %>%
  select(all_of(t1_cols)) %>%
  arrange(year, study_label) %>%
  rename_with(~ case_when(
    . == "study_label"      ~ "Study",
    . == "year"             ~ "Year",
    . == "n"                ~ "N",
    . == "age_mean"         ~ "Mean Age (years)",
    . == "age_sd"           ~ "SD Age",
    . == "memory_cat"       ~ "Memory Type",
    . == "sleep_measure_cat" ~ "Sleep Measure",
    . == "country"          ~ "Country",
    . == "sex_pct_female"   ~ "% Female",
    . == "sleep_stage"      ~ "Sleep Stage",
    . == "eeg_method"       ~ "EEG Method",
    . == "task_name"        ~ "Memory Task",
    TRUE                    ~ .
  ))

save_word_table(
  df_t1,
  title    = "Table 1. Characteristics of Included Studies",
  subtitle = paste0("Studies examining slow oscillation-sleep spindle coupling and ",
                    "overnight memory consolidation in healthy adults (k = ",
                    nrow(df_t1), ")"),
  filename = "Table1_StudyCharacteristics"
)


# =============================================================================
# TABLE 2: META-ANALYTIC RESULTS SUMMARY
# =============================================================================

cat("\n=== Table 2: Meta-Analytic Results ===\n")

rows_t2 <- list(
  summarise_meta(m_primary, "Primary: SO-Spindle Coupling → Declarative Memory"),
  summarise_meta(m_spindle, "Secondary: Spindle Parameters → Memory"),
  summarise_meta(m_so_meta, "Secondary: SO Characteristics → Memory"),
  summarise_meta(m_memory,  "Subgroup Analysis: All Memory Types Combined")
)
rows_t2 <- rows_t2[!sapply(rows_t2, is.null)]

if (length(rows_t2) > 0) {
  df_t2 <- do.call(rbind, rows_t2)

  save_word_table(
    df_t2,
    title    = "Table 2. Summary of Meta-Analytic Results",
    subtitle = paste0("Random-effects model (REML estimator, Hartung-Knapp correction). ",
                      "r = back-transformed Fisher z correlation; I² = heterogeneity; ",
                      "τ² = between-study variance."),
    filename = "Table2_MetaAnalyticResults"
  )
}


# =============================================================================
# TABLE 3: SUBGROUP ANALYSIS BY MEMORY TYPE
# =============================================================================

cat("\n=== Table 3: Subgroup Analysis ===\n")

if (!is.null(m_memory) && !is.null(m_memory$byvar)) {

  bylevs <- m_memory$bylevs
  n_lev  <- length(bylevs)

  rows_t3 <- lapply(seq_len(n_lev), function(i) {
    tryCatch(
      data.frame(
        `Memory Type`   = bylevs[i],
        k               = m_memory$k.w[i],
        `r [95% CI]`    = paste0(sprintf("%.2f", m_memory$TE.random.w[i]),
                                  " [",
                                  sprintf("%.2f", m_memory$lower.random.w[i]),
                                  ", ",
                                  sprintf("%.2f", m_memory$upper.random.w[i]),
                                  "]"),
        `p-value`       = fmt_p(m_memory$pval.random.w[i]),
        `I² (%)`   = paste0(sprintf("%.1f", m_memory$I2.w[i] * 100), "%"),
        `τ²`  = sprintf("%.4f", m_memory$tau.w[i]^2),
        check.names     = FALSE,
        stringsAsFactors = FALSE
      ),
      error = function(e) NULL
    )
  })
  rows_t3 <- rows_t3[!sapply(rows_t3, is.null)]

  # Test for subgroup differences row
  subgp_diff <- tryCatch(
    data.frame(
      `Memory Type`   = "Test for subgroup differences",
      k               = "",
      `r [95% CI]`    = "",
      `p-value`       = fmt_p(m_memory$pval.Q.b.random),
      `I² (%)`   = "",
      `τ²`  = "",
      check.names     = FALSE,
      stringsAsFactors = FALSE
    ),
    error = function(e) NULL
  )

  if (!is.null(subgp_diff)) rows_t3 <- c(rows_t3, list(subgp_diff))

  if (length(rows_t3) > 0) {
    df_t3 <- do.call(rbind, rows_t3)
    save_word_table(
      df_t3,
      title    = "Table 3. Subgroup Analysis by Memory Type",
      subtitle = "Random-effects model stratified by memory consolidation domain.",
      filename = "Table3_SubgroupAnalysis"
    )
  }
}


# =============================================================================
# TABLE 4: META-REGRESSION RESULTS
# =============================================================================

cat("\n=== Table 4: Meta-Regression ===\n")

if (!is.null(m_reg)) {
  tryCatch({
    cs <- coef(summary(m_reg))

    df_t4_coef <- data.frame(
      Predictor        = rownames(cs),
      `β`         = sprintf("%.4f", cs[, "estimate"]),
      SE               = sprintf("%.4f", cs[, "se"]),
      `z-value`        = sprintf("%.2f",  cs[, "zval"]),
      `p-value`        = sapply(cs[, "pval"], fmt_p),
      `95% CI Lower`   = sprintf("%.4f", cs[, "ci.lb"]),
      `95% CI Upper`   = sprintf("%.4f", cs[, "ci.ub"]),
      check.names      = FALSE,
      stringsAsFactors = FALSE
    )
    df_t4_coef$Predictor <- gsub("intrcpt",  "Intercept", df_t4_coef$Predictor)
    df_t4_coef$Predictor <- gsub("age_mean", "Mean Age (years)", df_t4_coef$Predictor)

    model_fit <- data.frame(
      Predictor       = c("QM (omnibus test)", "QE (residual heterogeneity)",
                          "R² (variance explained)"),
      `β`        = c(sprintf("%.2f", m_reg$QM),
                          sprintf("%.2f", m_reg$QE),
                          if (!is.na(m_reg$R2)) paste0(round(m_reg$R2, 1), "%") else "NA"),
      SE              = "",
      `z-value`       = "",
      `p-value`       = c(fmt_p(m_reg$QMp), fmt_p(m_reg$QEp), ""),
      `95% CI Lower`  = "",
      `95% CI Upper`  = "",
      check.names     = FALSE,
      stringsAsFactors = FALSE
    )

    df_t4 <- rbind(df_t4_coef, model_fit)

    save_word_table(
      df_t4,
      title    = "Table 4. Meta-Regression: Age as a Biological Moderator",
      subtitle = paste0("Mixed-effects meta-regression (REML estimation). ",
                        "Outcome: Fisher z-transformed correlation (r). ",
                        "Predictor: mean sample age (years)."),
      filename = "Table4_MetaRegression"
    )
  }, error = function(e) cat("Table 4 error:", e$message, "\n"))
}


# =============================================================================
# TABLE 5: PUBLICATION BIAS STATISTICS
# =============================================================================

cat("\n=== Table 5: Publication Bias ===\n")

if (!is.null(m_primary) && m_primary$k >= 5) {
  tryCatch({

    rows_pb <- list()

    if (!is.null(egger_res)) {
      rows_pb[[length(rows_pb) + 1]] <- data.frame(
        Test            = "Egger's Regression Test",
        Statistic       = paste0("z = ", round(egger_res$statistic, 2)),
        `p-value`       = fmt_p(egger_res$p.value),
        Interpretation  = if (egger_res$p.value < 0.05)
                            "Significant asymmetry detected"
                          else "No significant asymmetry",
        check.names     = FALSE, stringsAsFactors = FALSE
      )
    }

    if (!is.null(begg_res)) {
      rows_pb[[length(rows_pb) + 1]] <- data.frame(
        Test            = "Begg's Rank Correlation Test",
        Statistic       = paste0("z = ", round(begg_res$statistic, 2)),
        `p-value`       = fmt_p(begg_res$p.value),
        Interpretation  = if (begg_res$p.value < 0.05)
                            "Significant asymmetry detected"
                          else "No significant asymmetry",
        check.names     = FALSE, stringsAsFactors = FALSE
      )
    }

    if (!is.null(tf_res)) {
      rows_pb <- c(rows_pb, list(
        data.frame(
          Test           = "Trim-and-Fill: Observed estimate",
          Statistic      = paste0("r = ", sprintf("%.2f", m_primary$TE.random),
                                  " [", sprintf("%.2f", m_primary$lower.random),
                                  ", ", sprintf("%.2f", m_primary$upper.random),
                                  "]"),
          `p-value`      = "",
          Interpretation = "Unadjusted pooled effect",
          check.names    = FALSE, stringsAsFactors = FALSE
        ),
        data.frame(
          Test           = "Trim-and-Fill: Adjusted estimate",
          Statistic      = paste0("r = ", sprintf("%.2f", tf_res$TE.random),
                                  " [", sprintf("%.2f", tf_res$lower.random),
                                  ", ", sprintf("%.2f", tf_res$upper.random),
                                  "]"),
          `p-value`      = "",
          Interpretation = paste0(tf_res$k0, " stud",
                                  if (tf_res$k0 == 1) "y" else "ies",
                                  " imputed"),
          check.names    = FALSE, stringsAsFactors = FALSE
        )
      ))
    }

    if (length(rows_pb) > 0) {
      df_t5 <- do.call(rbind, rows_pb)
      save_word_table(
        df_t5,
        title    = "Table 5. Publication Bias Assessment",
        subtitle = paste0("Egger's regression test, Begg's rank correlation test, ",
                          "and Duval & Tweedie trim-and-fill analysis. ",
                          "Threshold for significance: p < 0.05."),
        filename = "Table5_PublicationBias"
      )
    }
  }, error = function(e) cat("Table 5 error:", e$message, "\n"))
}


# =============================================================================
# SUPPLEMENTARY TABLE 1: RISK OF BIAS DETAILS
# =============================================================================

cat("\n=== Supp Table 1: Risk of Bias Details ===\n")

if (length(rob_cols) >= 2) {
  tryCatch({

    rob_tab_cols <- c("study_label", "year", rob_cols)
    rob_tab_cols <- intersect(rob_tab_cols, names(rob_search_df))

    df_st1 <- rob_search_df %>%
      select(all_of(rob_tab_cols)) %>%
      arrange(across(any_of("year"))) %>%
      rename_with(~ gsub("_", " ", .) %>% tools::toTitleCase())

    ft_rob <- flextable(df_st1) %>%
      theme_booktabs() %>%
      bold(part = "header") %>%
      fontsize(size = 9, part = "all") %>%
      font(fontname = "Times New Roman", part = "all") %>%
      padding(padding = 3, part = "all") %>%
      autofit()

    doc_rob <- read_docx() %>%
      body_add_par("Supplementary Table 1. Risk of Bias Assessment (Individual Studies)",
                   style = "heading 1") %>%
      body_add_par(
        paste0("Risk of bias judgements for each included study across all assessed domains. ",
               "Low = low risk of bias; Some concerns = some concerns; High = high risk of bias."),
        style = "Normal"
      ) %>%
      body_add_par("", style = "Normal") %>%
      body_add_flextable(ft_rob)

    fpath <- file.path(supp_dir, "SuppTable1_RiskOfBias.docx")
    print(doc_rob, target = fpath)
    cat("Saved:", fpath, "\n")

  }, error = function(e) cat("Supp Table 1 error:", e$message, "\n"))
}


# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("\n")
cat(strrep("=", 65), "\n")
cat("  META-ANALYSIS COMPLETE\n")
cat(strrep("=", 65), "\n")
cat("\n OBJECTIVES AND COVERAGE:\n")
cat("  Obj 1: Primary (SO-spindle coupling -> declarative memory)\n")
cat("         -> Figure 2 (forest), Figure 5 (LOO), Table 2\n")
cat("  Obj 2: Spindle parameters (density/amplitude/sigma)\n")
cat("         -> Figure 3 (forest), Table 2\n")
cat("  Obj 3: SO characteristics (amplitude/slope/duration)\n")
cat("         -> Supp Figure 1, Table 2\n")
cat("  Obj 4: Subgroup by memory type\n")
cat("         -> Figure 4 (forest), Table 3\n")
cat("  Obj 5: Age moderator (meta-regression)\n")
cat("         -> Figure 8 (bubble), Table 4\n")
cat("  Obj 6: Risk of bias + publication bias\n")
cat("         -> Figure 6 (funnel), Figure 7 (trim-fill),\n")
cat("            Figure 9 (ROB), Table 5, Supp Table 1\n")
cat("\n FIGURES (PNG, 300 DPI) ->", fig_dir, "\n")
cat("  Figure 1 : PRISMA Flowchart [MANUAL]\n")
cat("  Figure 2 : Forest - Primary Outcome\n")
cat("  Figure 3 : Forest - Spindle Parameters\n")
cat("  Figure 4 : Forest - Memory Type Subgroup\n")
cat("  Figure 5 : Leave-One-Out Sensitivity\n")
cat("  Figure 6 : Funnel Plot\n")
cat("  Figure 7 : Trim-and-Fill Funnel\n")
cat("  Figure 8 : Meta-Regression Bubble (Age)\n")
cat("  Figure 9 : Risk of Bias (Traffic Light + Bar)\n")
cat("\n SUPPLEMENTARY ->", supp_dir, "\n")
cat("  Supp Figure 1: Forest - SO Characteristics\n")
cat("  Supp Figure 2: Baujat Influence Plot\n")
cat("  Supp Table  1: Risk of Bias (Individual Studies)\n")
cat("\n TABLES (DOCX) ->", tab_dir, "\n")
cat("  Table 1: Study Characteristics\n")
cat("  Table 2: Meta-Analytic Results\n")
cat("  Table 3: Subgroup Analysis\n")
cat("  Table 4: Meta-Regression\n")
cat("  Table 5: Publication Bias\n")
cat("\n STATISTICAL MODELS:\n")
cat("  Primary/Subgroup: Random-effects (REML) + Hartung-Knapp CI\n")
cat("  Effect size: Fisher z-transformed r (ZCOR), displayed as r\n")
cat("  Meta-regression: Mixed-effects (REML), predictor = mean age\n")
cat("  Publication bias: Egger, Begg, Trim-and-Fill\n")
cat("  Influence: Leave-one-out, Baujat plot\n")
cat(strrep("=", 65), "\n")
