# =============================================================================
# COMPLETE META-ANALYSIS: Slow Oscillation-Sleep Spindle Coupling and
# Overnight Memory Consolidation in Healthy Adults
# Journal: Journal of Endocrinology
# =============================================================================
# Data file: SO Spindle Memory DataExtraction.xlsx
#   Sheet used: 10_R_Ready_Dataset  (primary effect sizes)
#               8_Risk_of_Bias      (ROB assessment)
#               2_Participants_Demographics (age data for meta-regression)
# NOTE: Every sheet in this workbook has a merged TITLE row followed by
#       actual column headers. We use skip=1 to read all sheets correctly.
# =============================================================================


# =============================================================================
# SECTION 1: PACKAGES
# =============================================================================

options(repos = c(CRAN = "https://cloud.r-project.org"))
cat("=== INSTALLING AND LOADING PACKAGES ===\n")

pkgs <- c("meta", "metafor", "readxl", "dplyr", "tidyr", "ggplot2",
          "officer", "flextable", "scales", "patchwork", "stringr", "tools")

new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs) > 0) {
  cat("Installing:", paste(new_pkgs, collapse = ", "), "\n")
  install.packages(new_pkgs, dependencies = TRUE)
}
suppressPackageStartupMessages(lapply(pkgs, library, character.only = TRUE))
cat("All packages loaded.\n\n")


# =============================================================================
# SECTION 2: PATHS
# =============================================================================

output_dir <- "C:/Users/Admin/Downloads/Sandhiya"
data_path  <- file.path(output_dir, "SO Spindle Memory DataExtraction.xlsx")
fig_dir    <- file.path(output_dir, "Figures")
tab_dir    <- file.path(output_dir, "Tables")
supp_dir   <- file.path(output_dir, "Supplementary")

for (d in c(fig_dir, tab_dir, supp_dir))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(data_path))
  stop("Excel file not found at: ", data_path)

cat("Output directories ready.\n\n")


# =============================================================================
# SECTION 3: HELPER FUNCTIONS
# =============================================================================

fmt_p <- function(p) {
  if (is.null(p) || length(p) == 0 || is.na(p)) return("NA")
  if (p < 0.001) return("<0.001")
  format(round(p, 3), nsmall = 3)
}

save_gg <- function(p, filename, width = 12, height = 8, type = "main") {
  d     <- if (type == "supp") supp_dir else fig_dir
  fpath <- file.path(d, paste0(filename, ".png"))
  ggsave(fpath, plot = p, width = width, height = height, dpi = 300, bg = "white")
  cat("  Saved:", fpath, "\n")
}

save_word <- function(data, title, filename, subtitle = NULL, type = "main") {
  d     <- if (type == "supp") supp_dir else tab_dir
  fpath <- file.path(d, paste0(filename, ".docx"))
  ft    <- flextable(data) %>%
    theme_booktabs() %>%
    bold(part = "header") %>%
    fontsize(size = 10, part = "all") %>%
    font(fontname = "Times New Roman", part = "all") %>%
    align(align = "left", part = "all") %>%
    align(part = "header", align = "center") %>%
    padding(padding = 4, part = "all") %>%
    bg(i = seq(2, nrow(data), by = 2), bg = "#F5F5F5", part = "body") %>%
    autofit()
  doc <- read_docx() %>% body_add_par(title, style = "heading 1")
  if (!is.null(subtitle)) doc <- body_add_par(doc, subtitle, style = "Normal")
  doc <- doc %>%
    body_add_par("", style = "Normal") %>%
    body_add_flextable(ft) %>%
    body_add_par("", style = "Normal")
  print(doc, target = fpath)
  cat("  Saved:", fpath, "\n")
}

meta_row <- function(m, label) {
  if (is.null(m)) return(NULL)
  tryCatch(data.frame(
    Analysis   = label, k = m$k,
    `r [95% CI]` = paste0(sprintf("%.2f", m$TE.random), " [",
                           sprintf("%.2f", m$lower.random), ", ",
                           sprintf("%.2f", m$upper.random), "]"),
    Z          = sprintf("%.2f", m$statistic.random),
    `p-value`  = fmt_p(m$pval.random),
    `I2 (%)`   = paste0(sprintf("%.1f", m$I2 * 100), "%"),
    tau2       = sprintf("%.4f", m$tau^2),
    Q          = sprintf("%.2f", m$Q),
    `Q p-val`  = fmt_p(m$pval.Q),
    check.names = FALSE, stringsAsFactors = FALSE
  ), error = function(e) NULL)
}


# =============================================================================
# SECTION 4: LOAD DATA (skip=1 to skip merged title row in every sheet)
# =============================================================================

cat("=== LOADING DATA (skip=1 for merged title row) ===\n")

# ---- PRIMARY: Sheet 10_R_Ready_Dataset ----
# Columns: study_id, author, year, n, ri, fisher_z, sei, vi, pi,
#          memory_type, coupling_metric, spindle_type, sleep_type,
#          population, design, age_group, stress_condition, pharmacological,
#          significant, direction, rob_overall, notes
df_main <- read_excel(data_path, sheet = "10_R_Ready_Dataset", skip = 1)
df_main <- df_main %>% filter(!is.na(ri), !is.na(n))

cat("Sheet 10 loaded:", nrow(df_main), "rows,",
    ncol(df_main), "columns\n")
cat("Columns:", paste(names(df_main), collapse = ", "), "\n\n")

# ---- ROB: Sheet 8_Risk_of_Bias ----
# Columns: Study_ID, Author_Year, Study_Type, Selection_Bias, Performance_Bias,
#          Detection_Bias, Attrition_Bias, Reporting_Bias, Confounding_Bias,
#          Sample_Size_Adequacy, Randomization, Blinding,
#          Multiple_Testing_Control, Statistical_Method_Appropriateness,
#          Overall_RoB, RoB_Justification
df_rob <- read_excel(data_path, sheet = "8_Risk_of_Bias", skip = 1)
df_rob <- df_rob %>% filter(!is.na(Study_ID))
cat("Sheet 8 (ROB) loaded:", nrow(df_rob), "rows\n")

# ---- DEMOGRAPHICS: Sheet 2_Participants_Demographics ----
# Columns: Study_ID, Author_Year, N_Total, N_Experimental, N_Control,
#          N_Males, N_Females, Age_Mean_yrs, Age_SD, Age_Range, ...
df_demo <- tryCatch(
  read_excel(data_path, sheet = "2_Participants_Demographics", skip = 1),
  error = function(e) { cat("Demo sheet error:", e$message, "\n"); NULL }
)
if (!is.null(df_demo)) {
  df_demo <- df_demo %>% filter(!is.na(Study_ID))
  cat("Sheet 2 (Demographics) loaded:", nrow(df_demo), "rows\n")
}


# =============================================================================
# SECTION 5: DATA PREPARATION
# =============================================================================

cat("\n=== DATA PREPARATION ===\n")

df <- df_main %>%
  mutate(
    # Force numeric
    ri   = as.numeric(ri),
    n    = as.numeric(n),
    year = as.numeric(year),
    pi   = as.numeric(pi),

    # Bound r to valid range
    ri   = pmax(-0.9999, pmin(0.9999, ri)),

    # Base study ID (strip trailing letter: "S01a" -> "S01")
    base_id = sub("[a-z]+$", "", study_id),

    # Study label for forest plot
    study_label = paste0(
      tools::toTitleCase(as.character(author)),
      " et al. (", year, ")"
    )
  ) %>%
  filter(!is.na(ri), !is.na(n), n > 3)

# Disambiguate duplicate study labels (multiple outcomes per study)
df <- df %>%
  group_by(study_label) %>%
  mutate(
    cnt = n(),
    study_label = if_else(
      cnt > 1,
      paste0(study_label, " - ", tools::toTitleCase(as.character(memory_type))),
      study_label
    )
  ) %>%
  ungroup() %>%
  select(-cnt)

cat("Effect sizes:", nrow(df), "from", length(unique(df$base_id)), "unique studies\n\n")

# ---- Memory type categories ----
df <- df %>%
  mutate(memory_cat = case_when(
    grepl("declarative|verbal|explicit|episodic|hippocampal",
          memory_type, ignore.case = TRUE)    ~ "Verbal Declarative",
    grepl("spatial|visual|object|face|location|scene|picture",
          memory_type, ignore.case = TRUE)    ~ "Visuospatial",
    grepl("procedural|motor|implicit|skill|sequence|finger",
          memory_type, ignore.case = TRUE)    ~ "Procedural",
    TRUE                                      ~ "Verbal Declarative"
  ))

# ---- Sleep/coupling measure categories ----
df <- df %>%
  mutate(sleep_measure_cat = case_when(
    grepl("phase|degree|coupling|nest|lock|phase_degree",
          coupling_metric, ignore.case = TRUE) ~ "SO-Spindle Coupling (Phase)",
    grepl("MI|modulation.index|modulation_index",
          coupling_metric, ignore.case = TRUE) ~ "SO-Spindle Coupling (MI)",
    grepl("density|count|number",
          coupling_metric, ignore.case = TRUE) ~ "Spindle Density",
    grepl("amplitude|amp",
          coupling_metric, ignore.case = TRUE) ~ "Spindle Amplitude",
    grepl("sigma|power",
          coupling_metric, ignore.case = TRUE) ~ "Sigma Power",
    grepl("so.*amp|slow.*osc.*amp|delta",
          coupling_metric, ignore.case = TRUE) ~ "SO Amplitude",
    TRUE                                       ~ "SO-Spindle Coupling (Phase)"
  ))

# Broader category for grouping
df <- df %>%
  mutate(measure_broad = case_when(
    grepl("SO-Spindle", sleep_measure_cat) ~ "SO-Spindle Coupling",
    grepl("Spindle",    sleep_measure_cat) ~ "Spindle Parameter",
    grepl("SO Amp|SO Slope|SO Dur|Delta", sleep_measure_cat) ~ "SO Characteristic",
    TRUE ~ "SO-Spindle Coupling"
  ))

cat("Memory categories:\n"); print(table(df$memory_cat))
cat("\nSleep measure categories:\n"); print(table(df$sleep_measure_cat))
cat("\n")

# ---- Join continuous age ----
if (!is.null(df_demo) && "Age_Mean_yrs" %in% names(df_demo)) {
  df_age <- df_demo %>%
    select(Study_ID, age_mean = Age_Mean_yrs) %>%
    mutate(base_id = Study_ID,
           age_mean = as.numeric(age_mean))

  df <- df %>%
    left_join(df_age %>% select(base_id, age_mean), by = "base_id")

  cat("Age data joined:", sum(!is.na(df$age_mean)), "studies with age_mean\n")
} else {
  df$age_mean <- NA_real_
  cat("No continuous age data - meta-regression will be skipped.\n")
}
cat("\n")

# ---- Define analysis subsets ----
# Objective 1 (Primary): All declarative memory outcomes
df_primary <- df %>%
  filter(memory_cat %in% c("Verbal Declarative", "Visuospatial"))
if (nrow(df_primary) < 2) df_primary <- df

# Objective 2: Coupling/spindle measures
df_spindle <- df %>%
  filter(measure_broad %in% c("SO-Spindle Coupling", "Spindle Parameter"))
if (nrow(df_spindle) < 2) df_spindle <- df

# Objective 3: SO characteristics
df_so <- df %>% filter(measure_broad == "SO Characteristic")
if (nrow(df_so) < 2) df_so <- df

cat(sprintf("Subset sizes: Primary=%d | Spindle=%d | SO=%d | All=%d\n\n",
            nrow(df_primary), nrow(df_spindle), nrow(df_so), nrow(df)))


# =============================================================================
# SECTION 6: META-ANALYSIS RUNNER
# =============================================================================

run_meta <- function(data, subgroup_col = NULL, label = "") {
  has_sg <- !is.null(subgroup_col) &&
    subgroup_col %in% names(data) &&
    length(unique(data[[subgroup_col]])) > 1

  sg <- if (has_sg) data[[subgroup_col]] else NULL

  tryCatch(
    metacor(
      cor              = ri,
      n                = n,
      studlab          = study_label,
      data             = data,
      sm               = "ZCOR",
      random           = TRUE,
      common           = FALSE,
      method.tau       = "REML",
      method.random.ci = "HK",
      subgroup         = sg,
      subgroup.name    = if (has_sg) subgroup_col else NULL,
      print.subgroup.name = TRUE,
      title            = label
    ),
    error = function(e) {
      cat("  Meta-analysis ERROR [", label, "]:", e$message, "\n")
      NULL
    }
  )
}


# =============================================================================
# SECTION 7: RUN ALL META-ANALYSES
# =============================================================================

cat("=== RUNNING META-ANALYSES ===\n")

m_primary <- run_meta(df_primary,
                      label = "SO-Spindle Coupling - Declarative Memory Consolidation")

m_coupling <- run_meta(df_spindle, "sleep_measure_cat",
                       "Coupling/Spindle Parameters - Memory Consolidation")

m_memory   <- run_meta(df, "memory_cat",
                       "Subgroup by Memory Type")

m_so_meta  <- run_meta(df_so, "sleep_measure_cat",
                       "SO Characteristics - Memory Consolidation")

cat("\nResults summary:\n")
for (nm in c("m_primary","m_coupling","m_memory","m_so_meta")) {
  m <- get(nm)
  if (!is.null(m))
    cat(sprintf("  %-14s k=%2d  r=%.2f [%.2f, %.2f]  I2=%.0f%%\n",
                nm, m$k, m$TE.random, m$lower.random, m$upper.random, m$I2*100))
  else
    cat(sprintf("  %-14s FAILED\n", nm))
}
cat("\n")


# =============================================================================
# FOREST PLOT FUNCTION (RevMan5 style)
# =============================================================================

make_forest <- function(m, filename, title_txt = "", type = "main",
                        sort_desc = TRUE, width = 15) {
  if (is.null(m) || m$k < 2) {
    cat("  Skipping", filename, "(need >= 2 studies)\n")
    return(invisible(NULL))
  }
  d     <- if (type == "supp") supp_dir else fig_dir
  fpath <- file.path(d, paste0(filename, ".png"))
  k     <- m$k
  n_sg  <- if (!is.null(m$byvar)) length(unique(m$byvar)) else 0
  fig_h <- max(7, 2.5 + k * 0.40 + n_sg * 0.65)

  png(fpath, width = width, height = fig_h, units = "in", res = 300, bg = "white")

  forest(
    m,
    layout            = "RevMan5",
    sortvar           = if (sort_desc) -(m$TE) else NULL,
    col.square        = "black",
    col.square.lines  = "black",
    col.diamond       = "black",
    col.diamond.lines = "black",
    col.by            = "black",
    fontsize          = if (k <= 20) 10 else 8,
    spacing           = if (k <= 30) 0.85 else 0.65,
    squaresize        = 0.7,
    xlab              = "Correlation Coefficient (r)",
    xlim              = c(-0.5, 1.0),
    at                = c(-0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.0),
    leftlabs          = c("Study", "N"),
    rightlabs         = c("r [95% CI]", "Weight"),
    print.tau2        = TRUE,
    print.I2          = TRUE,
    print.Q           = TRUE,
    digits            = 2,
    addrows.below.overall = 1,
    text.random       = "Random-effects model",
    overall           = TRUE,
    overall.hetstat   = TRUE
  )

  if (nchar(trimws(title_txt)) > 0)
    title(title_txt, cex.main = 0.95, font.main = 2)

  dev.off()
  cat("  Saved:", fpath, "\n")
}


# =============================================================================
# SECTION 8: GENERATE ALL FIGURES
# =============================================================================

cat("=== GENERATING FIGURES ===\n")

# Figure 2: Primary forest plot
make_forest(
  m_primary, "Figure2_Forest_PrimaryOutcome",
  "Figure 2. SO-Spindle Coupling and Declarative Memory Consolidation\n(Random-effects model, REML estimator)"
)

# Figure 3: Coupling/spindle parameters subgroup
make_forest(
  m_coupling, "Figure3_Forest_CouplingParameters",
  "Figure 3. Coupling and Spindle Parameters as Predictors of Memory Consolidation\n(Subgroup by coupling metric)",
  sort_desc = FALSE
)

# Figure 4: Memory type subgroup
make_forest(
  m_memory, "Figure4_Forest_MemoryTypeSubgroup",
  "Figure 4. Subgroup Analysis by Memory Type\n(Verbal Declarative vs. Visuospatial vs. Procedural)",
  sort_desc = FALSE
)

# Supplementary Figure 1: SO characteristics
make_forest(
  m_so_meta, "SuppFigure1_Forest_SOCharacteristics",
  "Supplementary Figure 1. SO Characteristics and Memory Consolidation",
  type = "supp"
)


# =============================================================================
# FIGURE 5: LEAVE-ONE-OUT SENSITIVITY
# =============================================================================

if (!is.null(m_primary) && m_primary$k >= 4) {
  cat("  Generating Figure 5 (LOO)...\n")

  loo   <- metainf(m_primary, pooled = "random")
  k     <- m_primary$k
  fpath <- file.path(fig_dir, "Figure5_LeaveOneOut_Sensitivity.png")
  fig_h <- max(6, 2.5 + (k + 2) * 0.44)

  png(fpath, width = 14, height = fig_h, units = "in", res = 300, bg = "white")
  forest(
    loo,
    layout           = "RevMan5",
    col.square       = "black",
    col.square.lines = "black",
    col.diamond      = "black",
    fontsize         = if (k <= 20) 10 else 8,
    spacing          = 0.85,
    squaresize       = 0.7,
    xlab             = "Correlation Coefficient (r)",
    digits           = 2,
    leftlabs         = c("Omitted Study", "N"),
    rightlabs        = c("r [95% CI]", "Weight")
  )
  title("Figure 5. Leave-One-Out Sensitivity Analysis\nEffect of SO-Spindle Coupling on Declarative Memory",
        cex.main = 0.95, font.main = 2)
  dev.off()
  cat("  Saved:", fpath, "\n")
} else {
  cat("  Skipping Figure 5 LOO (need >= 4 studies, have",
      if(!is.null(m_primary)) m_primary$k else 0, ")\n")
}


# =============================================================================
# FIGURES 6 & 7: PUBLICATION BIAS
# =============================================================================

egger_res <- begg_res <- tf_res <- NULL

if (!is.null(m_primary) && m_primary$k >= 5) {

  egger_res <- tryCatch(
    metabias(m_primary, method.bias = "Egger", k.min = 3),
    error = function(e) { cat("  Egger error:", e$message, "\n"); NULL })
  begg_res  <- tryCatch(
    metabias(m_primary, method.bias = "Begg",  k.min = 3),
    error = function(e) { cat("  Begg error:",  e$message, "\n"); NULL })
  tf_res    <- tryCatch(
    trimfill(m_primary),
    error = function(e) { cat("  TrimFill error:", e$message, "\n"); NULL })

  # Figure 6: Funnel plot
  fpath <- file.path(fig_dir, "Figure6_FunnelPlot.png")
  png(fpath, width = 8, height = 7, units = "in", res = 300, bg = "white")
  par(mar = c(5.5, 5, 4, 2))
  funnel(m_primary, col = "black", bg = "grey40", pch = 21, cex = 1.3,
         xlab = "Correlation Coefficient (r)", ylab = "Standard Error",
         back = "white", hlines = NULL, lty.fixed = 0)
  if (!is.null(egger_res))
    mtext(paste0("Egger's test: z = ", round(egger_res$statistic, 2),
                 ", p = ", fmt_p(egger_res$p.value)),
          side = 1, line = 4.5, cex = 0.9, font = 2)
  title("Figure 6. Funnel Plot — Publication Bias Assessment\nSO-Spindle Coupling and Memory Consolidation",
        cex.main = 0.90, font.main = 2)
  dev.off()
  cat("  Saved: Figure6_FunnelPlot.png\n")

  # Figure 7: Trim-and-Fill
  if (!is.null(tf_res)) {
    fpath <- file.path(fig_dir, "Figure7_TrimFillFunnel.png")
    png(fpath, width = 8, height = 7, units = "in", res = 300, bg = "white")
    par(mar = c(5.5, 5, 4, 2))
    funnel(tf_res, col = "black", bg = "grey40", pch = 21, cex = 1.3,
           xlab = "Correlation Coefficient (r)", ylab = "Standard Error",
           back = "white", hlines = NULL, lty.fixed = 0)
    mtext(paste0("Trim-and-Fill: ", tf_res$k0, " stud",
                 if (tf_res$k0 == 1) "y" else "ies", " imputed"),
          side = 1, line = 4.5, cex = 0.9, font = 2)
    title("Figure 7. Trim-and-Fill Adjusted Funnel Plot\nSO-Spindle Coupling and Memory Consolidation",
          cex.main = 0.90, font.main = 2)
    dev.off()
    cat("  Saved: Figure7_TrimFillFunnel.png\n")
  }

} else {
  cat("  Figures 6 & 7 skipped (need >= 5 studies in primary set, have",
      if(!is.null(m_primary)) m_primary$k else 0, ")\n")
}


# =============================================================================
# FIGURE 8: META-REGRESSION BUBBLE PLOT (AGE)
# =============================================================================

m_reg <- NULL

if ("age_mean" %in% names(df) && sum(!is.na(df$age_mean)) >= 5) {

  df_reg <- df %>%
    filter(!is.na(age_mean), !is.na(ri), !is.na(n)) %>%
    mutate(
      z_val  = 0.5 * log((1 + ri) / (1 - ri)),
      var_z  = 1 / (n - 3),
      se_z   = sqrt(var_z),
      wt     = 1 / var_z
    )

  m_reg <- tryCatch(
    rma(yi = z_val, sei = se_z, mods = ~ age_mean, data = df_reg, method = "REML"),
    error = function(e) { cat("  Meta-regression error:", e$message, "\n"); NULL }
  )

  if (!is.null(m_reg)) {
    cat("  Meta-regression: beta(age)=", round(coef(m_reg)["age_mean"], 4),
        "  p=", fmt_p(m_reg$pval["age_mean"]), "\n")

    r2z  <- function(z) (exp(2*z) - 1) / (exp(2*z) + 1)
    ag   <- seq(min(df_reg$age_mean, na.rm=TRUE) - 1,
                max(df_reg$age_mean, na.rm=TRUE) + 1, length.out = 200)
    pr   <- predict(m_reg, newmods = ag)
    pdf  <- data.frame(age=ag, rp=r2z(pr$pred), rl=r2z(pr$ci.lb), rh=r2z(pr$ci.ub))
    df_reg <- df_reg %>%
      mutate(rdisp = r2z(z_val), bsz = sqrt(wt / max(wt)) * 9)

    b  <- round(coef(m_reg)["age_mean"], 4)
    pv <- fmt_p(m_reg$pval["age_mean"])
    r2 <- if (!is.na(m_reg$R2)) paste0(round(m_reg$R2, 1), "%") else "NA"

    p_bub <- ggplot() +
      geom_ribbon(data = pdf, aes(x=age, ymin=rl, ymax=rh),
                  fill = "grey80", alpha = 0.5) +
      geom_line(data = pdf, aes(x=age, y=rp),
                colour = "black", linewidth = 1) +
      geom_hline(yintercept = 0, linetype = "dashed",
                 colour = "grey50", linewidth = 0.6) +
      geom_point(data = df_reg,
                 aes(x=age_mean, y=rdisp, size=bsz),
                 shape=21, fill="grey40", colour="black", alpha=0.85, stroke=0.7) +
      scale_size_identity() +
      labs(
        title   = "Figure 8. Meta-Regression: Age as Moderator of\nSO-Spindle Coupling on Memory Consolidation",
        x       = "Mean Sample Age (years)",
        y       = "Correlation Coefficient (r)",
        caption = paste0("β = ", b, "   p = ", pv, "   R² = ", r2,
                         "   |   Bubble size ∝ precision (1/SE²)")
      ) +
      coord_cartesian(ylim = c(-0.3, 1.0)) +
      theme_classic(base_size = 12) +
      theme(
        plot.title       = element_text(face="bold", size=11, hjust=0.5),
        plot.caption     = element_text(hjust=0.5, size=9),
        axis.text        = element_text(colour="black"),
        panel.background = element_rect(fill="white"),
        plot.background  = element_rect(fill="white")
      )

    save_gg(p_bub, "Figure8_MetaRegression_Age", width=9, height=7)
  }

} else {
  cat("  Figure 8 skipped - age data available for only",
      sum(!is.na(df$age_mean)), "studies (need >= 5).\n")
}


# =============================================================================
# FIGURE 9: RISK OF BIAS (TRAFFIC LIGHT + BAR CHART)
# =============================================================================

cat("  Generating Figure 9 (ROB)...\n")

# ROB domain columns (known from sheet inspection)
rob_domain_cols <- intersect(
  c("Selection_Bias", "Performance_Bias", "Detection_Bias",
    "Attrition_Bias", "Reporting_Bias", "Confounding_Bias",
    "Sample_Size_Adequacy", "Randomization", "Blinding",
    "Multiple_Testing_Control", "Statistical_Method_Appropriateness"),
  names(df_rob)
)

cat("  ROB domains found:", length(rob_domain_cols), "\n")
cat("  Columns:", paste(rob_domain_cols, collapse=", "), "\n")

if (length(rob_domain_cols) >= 2) {

  # Study label from ROB sheet
  rob_study_col <- if ("Author_Year" %in% names(df_rob)) "Author_Year" else "Study_ID"

  rob_long <- df_rob %>%
    select(Study = all_of(rob_study_col), all_of(rob_domain_cols)) %>%
    pivot_longer(-Study, names_to = "Domain", values_to = "Judgment") %>%
    mutate(
      Judgment = case_when(
        grepl("^low$|^low\\s*-|^l$|^1$|adequate|yes|good|clear|minimal",
              as.character(Judgment), ignore.case = TRUE)                ~ "Low",
        grepl("high|poor|no|critical|serious|^3$|not met|major",
              as.character(Judgment), ignore.case = TRUE)                ~ "High",
        TRUE                                                              ~ "Some concerns"
      ),
      Judgment = factor(Judgment, levels = c("Low","Some concerns","High")),
      Domain   = Domain %>%
        gsub("_", " ", .) %>%
        gsub("Bias$", "", .) %>%
        trimws()
    )

  rob_pal <- c("Low"="#00B050","Some concerns"="#FFC000","High"="#FF0000")
  rob_sym <- c("Low"="+","Some concerns"="?","High"="−")

  # Traffic light heatmap
  p_heat <- ggplot(rob_long, aes(x=Domain, y=Study, fill=Judgment)) +
    geom_tile(colour="white", linewidth=0.7) +
    geom_text(aes(label=rob_sym[as.character(Judgment)]),
              colour="white", fontface="bold", size=4.5) +
    scale_fill_manual(values=rob_pal, name="Risk of Bias",
                      labels=c("Low risk","Some concerns","High risk")) +
    labs(title="A. Traffic Light Plot", x=NULL, y=NULL) +
    theme_classic(base_size=10) +
    theme(axis.text.x   = element_text(angle=40, hjust=1, colour="black"),
          axis.text.y   = element_text(colour="black"),
          legend.position="right",
          plot.title    = element_text(face="bold", size=11),
          panel.background = element_rect(fill="white"),
          plot.background  = element_rect(fill="white"))

  # Summary bar chart
  rob_sum <- rob_long %>%
    count(Domain, Judgment) %>%
    group_by(Domain) %>%
    mutate(pct = n / sum(n) * 100) %>%
    ungroup()

  p_bar <- ggplot(rob_sum, aes(x=Domain, y=pct, fill=Judgment)) +
    geom_bar(stat="identity", colour="white", linewidth=0.3) +
    geom_text(aes(label=paste0(round(pct),"%")),
              position=position_stack(vjust=0.5),
              colour="white", fontface="bold", size=3.5) +
    scale_fill_manual(values=rob_pal, name="Risk of Bias",
                      labels=c("Low risk","Some concerns","High risk")) +
    scale_y_continuous(labels=scales::percent_format(scale=1), limits=c(0,101)) +
    labs(title="B. Summary Bar Chart", x=NULL, y="Percentage of Studies (%)") +
    coord_flip() +
    theme_classic(base_size=10) +
    theme(axis.text        = element_text(colour="black"),
          legend.position  = "right",
          plot.title       = element_text(face="bold", size=11),
          panel.background = element_rect(fill="white"),
          plot.background  = element_rect(fill="white"))

  p_rob <- p_heat / p_bar +
    plot_annotation(
      title = "Figure 9. Risk of Bias Assessment",
      theme = theme(plot.title=element_text(face="bold", size=13, hjust=0.5))
    )

  nsr <- length(unique(rob_long$Study))
  save_gg(p_rob, "Figure9_RiskOfBias",
          width=12, height=max(10, 4 + nsr * 0.4))

} else {
  cat("  Figure 9 skipped: domain columns not detected in ROB sheet.\n")
}


# =============================================================================
# SUPPLEMENTARY FIGURE 2: BAUJAT INFLUENCE PLOT
# =============================================================================

if (!is.null(m_primary) && m_primary$k >= 4) {
  fpath <- file.path(supp_dir, "SuppFigure2_BaujatInfluencePlot.png")
  png(fpath, width=9, height=8, units="in", res=300, bg="white")
  tryCatch(
    baujat(m_primary, col="black", pch=19, cex=1.2,
           main="Supplementary Figure 2. Baujat Influence Analysis\nSO-Spindle Coupling and Declarative Memory",
           xlab="Contribution to Overall Heterogeneity (Q)",
           ylab="Influence on Pooled Estimate"),
    error=function(e){plot.new();text(0.5,0.5,paste("Error:",e$message))}
  )
  dev.off()
  cat("  Saved: SuppFigure2_BaujatInfluencePlot.png\n")
}


# =============================================================================
# SECTION 9: GENERATE WORD TABLES
# =============================================================================

cat("\n=== GENERATING TABLES ===\n")

# ---- TABLE 1: Study Characteristics ----
cat("  Building Table 1...\n")

t1_cols <- c("study_id","author","year","n","memory_cat",
             "sleep_measure_cat","spindle_type","sleep_type",
             "design","age_group","pharmacological","rob_overall")
t1_cols <- intersect(t1_cols, names(df))

df_t1 <- df %>%
  select(all_of(t1_cols)) %>%
  arrange(year, author) %>%
  rename_with(~ case_when(
    . == "study_id"         ~ "Study ID",
    . == "author"           ~ "First Author",
    . == "year"             ~ "Year",
    . == "n"                ~ "N",
    . == "memory_cat"       ~ "Memory Type",
    . == "sleep_measure_cat"~ "Coupling Measure",
    . == "spindle_type"     ~ "Spindle Type",
    . == "sleep_type"       ~ "Sleep Type",
    . == "design"           ~ "Study Design",
    . == "age_group"        ~ "Age Group",
    . == "pharmacological"  ~ "Pharmacological",
    . == "rob_overall"      ~ "Overall RoB",
    TRUE ~ tools::toTitleCase(gsub("_"," ",.))
  ))

save_word(
  df_t1,
  title    = "Table 1. Characteristics of Included Studies",
  subtitle = paste0("Characteristics of ", length(unique(df$base_id)),
                    " studies (", nrow(df), " effect sizes) examining slow oscillation-",
                    "sleep spindle coupling and overnight memory consolidation ",
                    "in healthy adults. RoB = Risk of Bias."),
  filename = "Table1_StudyCharacteristics"
)


# ---- TABLE 2: Meta-Analytic Results ----
cat("  Building Table 2...\n")

rows_t2 <- list(
  meta_row(m_primary, "Primary: SO-Spindle Coupling → Declarative Memory"),
  meta_row(m_coupling,"Secondary: Coupling/Spindle Parameters → Memory"),
  meta_row(m_so_meta, "Secondary: SO Characteristics → Memory"),
  meta_row(m_memory,  "Subgroup: All Memory Types (Combined)")
)
rows_t2 <- rows_t2[!sapply(rows_t2, is.null)]

if (length(rows_t2) > 0)
  save_word(
    do.call(rbind, rows_t2),
    title    = "Table 2. Summary of Meta-Analytic Results",
    subtitle = paste0("Random-effects model (REML estimator) with Hartung-Knapp ",
                      "confidence interval correction applied to all analyses. ",
                      "r = back-transformed Fisher z-correlation coefficient; ",
                      "I² = proportion of variance attributable to heterogeneity; ",
                      "τ² = estimated between-study variance; ",
                      "Q = Cochran's heterogeneity statistic."),
    filename = "Table2_MetaAnalyticResults"
  )


# ---- TABLE 3: Subgroup by Memory Type ----
cat("  Building Table 3...\n")

if (!is.null(m_memory) && !is.null(m_memory$byvar)) {
  levs    <- m_memory$bylevs
  rows_t3 <- lapply(seq_along(levs), function(i)
    tryCatch(data.frame(
      `Memory Type`  = levs[i],
      k              = m_memory$k.w[i],
      `r [95% CI]`   = paste0(sprintf("%.2f", m_memory$TE.random.w[i]),
                               " [", sprintf("%.2f", m_memory$lower.random.w[i]),
                               ", ", sprintf("%.2f", m_memory$upper.random.w[i]), "]"),
      `p-value`      = fmt_p(m_memory$pval.random.w[i]),
      `I2 (%)`       = paste0(sprintf("%.1f", m_memory$I2.w[i]*100), "%"),
      tau2           = sprintf("%.4f", m_memory$tau.w[i]^2),
      check.names=FALSE, stringsAsFactors=FALSE
    ), error=function(e) NULL)
  )
  rows_t3 <- rows_t3[!sapply(rows_t3, is.null)]

  diff_row <- tryCatch(data.frame(
    `Memory Type`="Test for subgroup differences", k="", `r [95% CI]`="",
    `p-value`=fmt_p(m_memory$pval.Q.b.random), `I2 (%)`="", tau2="",
    check.names=FALSE, stringsAsFactors=FALSE), error=function(e) NULL)
  if (!is.null(diff_row)) rows_t3 <- c(rows_t3, list(diff_row))

  if (length(rows_t3) > 0)
    save_word(
      do.call(rbind, rows_t3),
      title    = "Table 3. Subgroup Analysis by Memory Type",
      subtitle = "Random-effects model (REML) stratified by memory consolidation domain. Test for subgroup differences uses the Q-between statistic.",
      filename = "Table3_SubgroupAnalysis"
    )
}


# ---- TABLE 4: Meta-Regression ----
cat("  Building Table 4...\n")

if (!is.null(m_reg)) tryCatch({
  cs <- coef(summary(m_reg))
  df_t4 <- data.frame(
    Predictor      = gsub("intrcpt","Intercept",
                          gsub("age_mean","Mean Age (years)",rownames(cs))),
    Beta           = sprintf("%.4f", cs[,"estimate"]),
    SE             = sprintf("%.4f", cs[,"se"]),
    z              = sprintf("%.2f",  cs[,"zval"]),
    `p-value`      = sapply(cs[,"pval"], fmt_p),
    `95% CI Lower` = sprintf("%.4f", cs[,"ci.lb"]),
    `95% CI Upper` = sprintf("%.4f", cs[,"ci.ub"]),
    check.names=FALSE, stringsAsFactors=FALSE
  )
  fit_rows <- data.frame(
    Predictor=c("QM (omnibus)","QE (residual heterogeneity)","R² explained"),
    Beta=c(sprintf("%.2f",m_reg$QM), sprintf("%.2f",m_reg$QE),
           if(!is.na(m_reg$R2)) paste0(round(m_reg$R2,1),"%") else "NA"),
    SE="", z="",
    `p-value`=c(fmt_p(m_reg$QMp),fmt_p(m_reg$QEp),""),
    `95% CI Lower`="", `95% CI Upper`="",
    check.names=FALSE, stringsAsFactors=FALSE
  )
  save_word(rbind(df_t4,fit_rows),
            title="Table 4. Meta-Regression: Age as a Biological Moderator",
            subtitle="Mixed-effects meta-regression (REML). Outcome: Fisher z-transformed r. Predictor: mean sample age (years).",
            filename="Table4_MetaRegression")
}, error=function(e) cat("  Table 4 error:", e$message, "\n"))


# ---- TABLE 5: Publication Bias ----
cat("  Building Table 5...\n")

if (!is.null(m_primary) && m_primary$k >= 5) tryCatch({
  rows_pb <- list()

  if (!is.null(egger_res))
    rows_pb[[length(rows_pb)+1]] <- data.frame(
      Test="Egger's Regression Test",
      Statistic=paste0("z = ",round(egger_res$statistic,2)),
      `p-value`=fmt_p(egger_res$p.value),
      Interpretation=if(egger_res$p.value<0.05)"Asymmetry detected"else"No significant asymmetry",
      check.names=FALSE,stringsAsFactors=FALSE)

  if (!is.null(begg_res))
    rows_pb[[length(rows_pb)+1]] <- data.frame(
      Test="Begg's Rank Correlation Test",
      Statistic=paste0("z = ",round(begg_res$statistic,2)),
      `p-value`=fmt_p(begg_res$p.value),
      Interpretation=if(begg_res$p.value<0.05)"Asymmetry detected"else"No significant asymmetry",
      check.names=FALSE,stringsAsFactors=FALSE)

  if (!is.null(tf_res)) rows_pb <- c(rows_pb, list(
    data.frame(Test="Trim-and-Fill: Observed estimate",
               Statistic=paste0("r = ",sprintf("%.2f",m_primary$TE.random),
                                " [",sprintf("%.2f",m_primary$lower.random),
                                ", ",sprintf("%.2f",m_primary$upper.random),"]"),
               `p-value`="",Interpretation="Unadjusted pooled effect",
               check.names=FALSE,stringsAsFactors=FALSE),
    data.frame(Test="Trim-and-Fill: Adjusted estimate",
               Statistic=paste0("r = ",sprintf("%.2f",tf_res$TE.random),
                                " [",sprintf("%.2f",tf_res$lower.random),
                                ", ",sprintf("%.2f",tf_res$upper.random),"]"),
               `p-value`="",
               Interpretation=paste0(tf_res$k0," stud",
                                     if(tf_res$k0==1)"y"else"ies"," imputed"),
               check.names=FALSE,stringsAsFactors=FALSE)
  ))

  if (length(rows_pb) > 0)
    save_word(do.call(rbind,rows_pb),
              title="Table 5. Publication Bias Assessment",
              subtitle="Egger's linear regression test, Begg's rank correlation test, and Duval & Tweedie trim-and-fill analysis. Significance threshold: p < 0.05.",
              filename="Table5_PublicationBias")

}, error=function(e) cat("  Table 5 error:",e$message,"\n"))


# ---- SUPPLEMENTARY TABLE 1: ROB Details ----
cat("  Building Supp Table 1...\n")

if (length(rob_domain_cols) >= 2) tryCatch({
  rob_study_col <- if ("Author_Year" %in% names(df_rob)) "Author_Year" else "Study_ID"
  st1_cols <- intersect(c(rob_study_col,"Study_Type",rob_domain_cols,"Overall_RoB"),
                        names(df_rob))
  df_st1 <- df_rob %>%
    select(all_of(st1_cols)) %>%
    rename_with(~ gsub("_"," ",.) %>% tools::toTitleCase())

  ft_st1 <- flextable(df_st1) %>%
    theme_booktabs() %>% bold(part="header") %>%
    fontsize(size=9,part="all") %>%
    font(fontname="Times New Roman",part="all") %>%
    padding(padding=3,part="all") %>% autofit()

  doc_st1 <- read_docx() %>%
    body_add_par("Supplementary Table 1. Risk of Bias Assessment — Individual Studies",
                 style="heading 1") %>%
    body_add_par("Risk of bias judgements using the Newcastle-Ottawa Scale (NOS) for observational studies and RoB2 for randomised trials. Low = low risk; Some concerns = moderate risk; High = high risk.",
                 style="Normal") %>%
    body_add_par("",style="Normal") %>%
    body_add_flextable(ft_st1)

  fpath <- file.path(supp_dir,"SuppTable1_RiskOfBias.docx")
  print(doc_st1, target=fpath)
  cat("  Saved:", fpath, "\n")

}, error=function(e) cat("  Supp Table 1 error:", e$message,"\n"))


# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("\n", strrep("=",65), "\n", sep="")
cat("  META-ANALYSIS COMPLETE — FILES SAVED\n")
cat(strrep("=",65), "\n\n", sep="")

list_saved <- function(d, label) {
  ff <- list.files(d, pattern="\\.(png|docx)$")
  cat(label, "(", length(ff), "files):\n")
  for (f in ff) cat("   +", f, "\n")
  if (length(ff)==0) cat("   (none generated)\n")
}

list_saved(fig_dir,  paste0("FIGURES  -> ", fig_dir))
list_saved(tab_dir,  paste0("TABLES   -> ", tab_dir))
list_saved(supp_dir, paste0("SUPP     -> ", supp_dir))

cat("\nSTATISTICAL METHODS:\n")
cat("  Primary model : Random-effects (REML) + Hartung-Knapp CI\n")
cat("  Effect metric : Fisher z-transformed r, displayed as Pearson r\n")
cat("  Meta-regression: Mixed-effects REML (predictor: mean age)\n")
cat("  Publication bias: Egger's test, Begg's test, Trim-and-Fill\n")
cat("  Influence     : Leave-one-out sensitivity, Baujat plot\n")
cat(strrep("=",65),"\n",sep="")
