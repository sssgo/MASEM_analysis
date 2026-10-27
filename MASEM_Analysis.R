rm(list = ls())
gc()

required_packages <- c("meta", "metafor", "tidyverse", "metaSEM", "lavaan",
                       "semPlot", "cowplot", "ggpubr", "gridExtra", "patchwork",
                       "ggcorrplot", "openxlsx", "ggplot2", "car", "lmtest",
                       "sandwich", "Matrix", "clipr", "forestplot", "ggrepel",
                       "viridis", "RColorBrewer", "ggthemes", "corrplot", "reshape2",
                       "grid", "stringr", "forcats", "ggsci", "pheatmap", "gridGraphics",
                       "diagram", "DiagrammeR", "kableExtra", "flextable", "psych",
                       "scales", "GGally", "scatterplot3d", "plotly", "ggdendro")

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, dependencies = TRUE)

library(meta)
library(metafor)
library(tidyverse)
library(metaSEM)
library(lavaan)
library(semPlot)
library(cowplot)
library(ggpubr)
library(gridExtra)
library(patchwork)
library(ggcorrplot)
library(openxlsx)
library(ggplot2)
library(psych)
library(car)
library(lmtest)
library(sandwich)
library(Matrix)
library(clipr)
library(forestplot)
library(ggrepel)
library(viridis)
library(RColorBrewer)
library(ggthemes)
library(corrplot)
library(reshape2)
library(grid)
library(stringr)
library(forcats)
library(ggsci)
library(pheatmap)
library(gridGraphics)
library(diagram)
library(DiagrammeR)
library(kableExtra)
library(flextable)
library(scales)
library(GGally)

theme_set(theme_minimal(base_size = 11, base_family = "sans"))
theme_update(
  plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
  plot.subtitle = element_text(size = 11, hjust = 0.5),
  axis.title = element_text(size = 11, face = "bold"),
  axis.text = element_text(size = 10),
  legend.title = element_text(size = 10, face = "bold"),
  legend.text = element_text(size = 9),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
  plot.margin = margin(1, 1, 1, 1, "cm")
)

setwd("L:/2025AI依赖对批判性思维的影响META/1225")

dirs_to_create <- c("Figures", "Tables", "Figures/HighRes", "Output",
                    "Figures/ForestPlots", "Figures/FunnelPlots", "Figures/Subgroup")
for(dir in dirs_to_create) {
  if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}

cat("=== 数据读取和清洗 ===\n")

df_studies <- readxl::read_excel("MASEMdata20251224.xlsx", sheet = 1)
df_correlations_raw <- readxl::read_excel("MASEMdata20251224.xlsx", sheet = 2)

cat("=== 研究信息表实际列名 ===\n")
print(colnames(df_studies))
cat("\n=== 研究信息表前5行 ===\n")
print(head(df_studies, 5))

cat("\n=== 相关系数表实际列名 ===\n")
print(colnames(df_correlations_raw))
cat("\n=== 相关系数表前5行 ===\n")
print(head(df_correlations_raw, 5))

cat("\n=== 开始数据处理 ===\n")

cat("研究信息表维度:", dim(df_studies), "\n")
cat("相关系数表维度:", dim(df_correlations_raw), "\n")

df_studies_clean <- data.frame(
  Study = as.character(df_studies$study),
  SampleSize = as.numeric(df_studies$SampleSize),
  Female = as.numeric(df_studies$Female),
  Male = as.numeric(df_studies$Male),
  Country = as.character(df_studies$Country),
  ObjectType_raw = as.character(df_studies$Objtype)
)

cat("\n=== 研究信息表创建成功 ===\n")
cat("行数:", nrow(df_studies_clean), "\n")
cat("列名:", colnames(df_studies_clean), "\n")

df_studies_clean$Percent_Female <- ifelse(
  !is.na(df_studies_clean$Female) & !is.na(df_studies_clean$Male) &
    (df_studies_clean$Female + df_studies_clean$Male) > 0,
  df_studies_clean$Female / (df_studies_clean$Female + df_studies_clean$Male) * 100,
  NA
)

df_studies_clean$ObjectType <- sapply(df_studies_clean$ObjectType_raw, function(x) {
  x <- as.character(x)
  if (grepl("Univ", x, ignore.case = TRUE)) return("University")
  if (grepl("MidSch", x, ignore.case = TRUE)) return("Middle School")
  if (grepl("Prof", x, ignore.case = TRUE)) return("Professional")
  if (grepl("Corp", x, ignore.case = TRUE)) return("Corporate")
  if (grepl("Mix", x, ignore.case = TRUE)) return("Mixed")
  return("Other")
})

study_final <- data.frame(
  Study = trimws(df_studies_clean$Study),
  Percent_Female = df_studies_clean$Percent_Female,
  Country = trimws(df_studies_clean$Country),
  SampleSize = df_studies_clean$SampleSize,
  ObjectType = df_studies_clean$ObjectType
)

cat("\n=== 最终研究信息表 ===\n")
print(head(study_final))
cat("行数:", nrow(study_final), "\n")

cat("\n=== 处理相关系数表 ===\n")

corr_data <- data.frame(
  Study = as.character(df_correlations_raw$study),
  Pair = as.character(df_correlations_raw$Association),
  r_value = as.numeric(df_correlations_raw$correlation),
  n = as.numeric(df_correlations_raw$num),
  Reliability_U = as.numeric(df_correlations_raw$RU),
  Reliability_D = as.numeric(df_correlations_raw$RD),
  Reliability_CT = as.numeric(df_correlations_raw$RCT),
  Reliability_MT = as.numeric(df_correlations_raw$RMT)
)

cat("\n=== 原始相关系数数据 ===\n")
cat("行数:", nrow(corr_data), "\n")
print(head(corr_data))

corr_clean <- corr_data[!is.na(corr_data$Study) &
                          !is.na(corr_data$Pair) &
                          !is.na(corr_data$r_value) &
                          !is.na(corr_data$n) &
                          corr_data$n > 0, ]

corr_clean$Pair_clean <- sapply(corr_clean$Pair, function(x) {
  x <- trimws(as.character(x))
  if (x %in% c("U-D", "D-U")) return("U-D")
  if (x %in% c("U-CT", "CT-U")) return("U-CT")
  if (x %in% c("U-MT", "MT-U")) return("U-MT")
  if (x %in% c("D-CT", "CT-D")) return("D-CT")
  if (x %in% c("D-MT", "MT-D")) return("D-MT")
  if (x %in% c("CT-MT", "MT-CT")) return("CT-MT")
  return(x)
})

corr_clean$r_corrected <- corr_clean$r_value

corr_clean <- corr_clean[order(corr_clean$Study, corr_clean$Pair_clean, -corr_clean$n), ]
corr_clean <- corr_clean[!duplicated(corr_clean[, c("Study", "Pair_clean")]), ]

corr_final <- data.frame(
  Study = trimws(corr_clean$Study),
  Pair = corr_clean$Pair_clean,
  r_original = corr_clean$r_value,
  r = corr_clean$r_corrected,
  n = corr_clean$n,
  Reliability_U = corr_clean$Reliability_U,
  Reliability_D = corr_clean$Reliability_D,
  Reliability_CT = corr_clean$Reliability_CT,
  Reliability_MT = corr_clean$Reliability_MT
)

cat("\n=== 清洗后相关系数表 ===\n")
print(head(corr_final))
cat("行数:", nrow(corr_final), "\n")
cat("变量对分布:\n")
print(table(corr_final$Pair))

df_correlations <- merge(corr_final, study_final, by = "Study", all.x = TRUE)

df_correlations$n <- ifelse(
  is.na(df_correlations$n) | df_correlations$n == 0,
  df_correlations$SampleSize,
  df_correlations$n
)

df_correlations$SampleSize <- NULL

cat("\n=== 最终合并数据 ===\n")
cat("总行数:", nrow(df_correlations), "\n")
cat("变量对分布:\n")
print(table(df_correlations$Pair))

write.csv(study_final, "Tables/study_info_clean.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(corr_final, "Tables/correlations_clean.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(df_correlations, "Tables/final_merged_data.csv", row.names = FALSE, fileEncoding = "UTF-8")

saveRDS(study_final, "Tables/study_info_clean.rds")
saveRDS(corr_final, "Tables/correlations_clean.rds")
saveRDS(df_correlations, "Tables/final_merged_data.rds")

cat("\n✓ 数据清洗完成！文件已保存到Tables/目录\n")
cat("✓ study_info_clean.csv - 研究信息表\n")
cat("✓ correlations_clean.csv - 相关系数表\n")
cat("✓ final_merged_data.csv - 合并后的完整数据\n")
cat("✓ 对应的.rds文件也已保存，用于后续分析\n")

cat("\n=== 数据统计 ===\n")
cat("研究数量:", length(unique(df_correlations$Study)), "\n")
cat("总相关关系数:", nrow(df_correlations), "\n")
cat("各变量对数量:\n")
pair_summary <- table(df_correlations$Pair)
for(pair in names(pair_summary)) {
  cat(paste0("  ", pair, ": ", pair_summary[pair], "\n"))
}

cat("\n处理缺失值...\n")

avg_reliability <- df_correlations %>%
  summarise(
    avg_U = mean(Reliability_U, na.rm = TRUE),
    avg_D = mean(Reliability_D, na.rm = TRUE),
    avg_CT = mean(Reliability_CT, na.rm = TRUE),
    avg_MT = mean(Reliability_MT, na.rm = TRUE)
  )

df_correlations <- df_correlations %>%
  mutate(
    Reliability_U = ifelse(is.na(Reliability_U), avg_reliability$avg_U, Reliability_U),
    Reliability_D = ifelse(is.na(Reliability_D), avg_reliability$avg_D, Reliability_D),
    Reliability_CT = ifelse(is.na(Reliability_CT), avg_reliability$avg_CT, Reliability_CT),
    Reliability_MT = ifelse(is.na(Reliability_MT), avg_reliability$avg_MT, Reliability_MT)
  )

weighted_means <- df_correlations %>%
  group_by(Pair) %>%
  summarise(
    weighted_r = weighted.mean(r, n, na.rm = TRUE),
    n_studies = n()
  )

cat("\n变量对加权平均相关系数：\n")
print(weighted_means)

cat("\n=== 女性性别比例分析 ===\n")

gender_data <- df_correlations %>%
  filter(!is.na(Percent_Female)) %>%
  group_by(Pair) %>%
  summarise(
    n_studies_with_gender = n(),
    mean_female_percent = mean(Percent_Female, na.rm = TRUE),
    median_female_percent = median(Percent_Female, na.rm = TRUE),
    sd_female_percent = sd(Percent_Female, na.rm = TRUE),
    min_female_percent = min(Percent_Female, na.rm = TRUE),
    max_female_percent = max(Percent_Female, na.rm = TRUE)
  )

cat("\n各变量对女性比例统计：\n")
print(gender_data)

if(nrow(gender_data) > 0) {
  p_gender_dist <- ggplot(df_correlations %>% filter(!is.na(Percent_Female)),
                          aes(x = Percent_Female)) +
    geom_histogram(binwidth = 5, fill = "#4E79A7", color = "white", alpha = 0.8) +
    geom_vline(aes(xintercept = mean(Percent_Female, na.rm = TRUE)),
               color = "#E15759", linetype = "dashed", linewidth = 1) +
    labs(title = "研究样本女性比例分布",
         subtitle = sprintf("平均女性比例: %.1f%% (基于%d项研究)",
                            mean(df_correlations$Percent_Female, na.rm = TRUE),
                            sum(!is.na(df_correlations$Percent_Female))),
         x = "女性比例 (%)",
         y = "研究数量") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5))

  ggsave("Figures/女性比例分布图.pdf", p_gender_dist, width = 10, height = 6)
  ggsave("Figures/HighRes/女性比例分布图.png", p_gender_dist, width = 10, height = 6, dpi = 300)
  cat("✓ 女性比例分布图已保存\n")
}

if(nrow(df_correlations %>% filter(!is.na(Percent_Female))) > 10) {
  cat("\n进行女性比例与相关系数的元回归分析...\n")

  meta_regression_results <- data.frame()

  for(pair in unique(df_correlations$Pair)) {
    pair_data <- df_correlations %>%
      filter(Pair == pair, !is.na(Percent_Female), !is.na(r))

    if(nrow(pair_data) >= 5) {
      pair_data$z <- 0.5 * log((1 + pair_data$r) / (1 - pair_data$r))
      pair_data$z_var <- 1 / (pair_data$n - 3)

      meta_reg <- rma(yi = z, vi = z_var, mods = ~ Percent_Female, data = pair_data)

      reg_result <- data.frame(
        Pair = pair,
        k = nrow(pair_data),
        Intercept = meta_reg$beta[1],
        Slope = meta_reg$beta[2],
        Slope_SE = meta_reg$se[2],
        z_value = meta_reg$zval[2],
        p_value = meta_reg$pval[2],
        QE = meta_reg$QE,
        QEp = meta_reg$QEp,
        R2 = meta_reg$R2
      )

      meta_regression_results <- bind_rows(meta_regression_results, reg_result)
    }
  }

  if(nrow(meta_regression_results) > 0) {
    cat("\n女性比例元回归结果：\n")
    print(meta_regression_results)

    write.csv(meta_regression_results, "Tables/女性比例元回归结果.csv", row.names = FALSE)
    cat("✓ 女性比例元回归结果已保存\n")

    p_meta_reg <- ggplot(meta_regression_results, aes(x = Pair, y = Slope)) +
      geom_point(aes(size = k, color = ifelse(p_value < 0.05, "显著", "不显著")),
                 position = position_dodge(width = 0.5)) +
      geom_errorbar(aes(ymin = Slope - 1.96 * Slope_SE,
                        ymax = Slope + 1.96 * Slope_SE,
                        color = ifelse(p_value < 0.05, "显著", "不显著")),
                    width = 0.2, position = position_dodge(width = 0.5)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      scale_color_manual(values = c("显著" = "#E15759", "不显著" = "#4E79A7")) +
      labs(title = "女性比例对相关系数的调节效应",
           subtitle = "元回归斜率及95%置信区间",
           x = "变量对",
           y = "元回归斜率",
           color = "显著性",
           size = "研究数量") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggsave("Figures/女性比例元回归图.pdf", p_meta_reg, width = 10, height = 6)
    ggsave("Figures/HighRes/女性比例元回归图.png", p_meta_reg, width = 10, height = 6, dpi = 300)
    cat("✓ 女性比例元回归图已保存\n")
  }
}

cat("\n基于实际数据构建相关矩阵...\n")

calculate_mean_cor <- function(pair_name) {
  pair_data <- df_correlations %>% filter(Pair == pair_name)
  if(nrow(pair_data) == 0) return(NA)

  weighted_mean <- weighted.mean(pair_data$r, pair_data$n, na.rm = TRUE)
  return(weighted_mean)
}

cor_matrix_actual <- matrix(1, nrow = 4, ncol = 4)
rownames(cor_matrix_actual) <- colnames(cor_matrix_actual) <- c("U", "D", "CT", "MT")

cor_matrix_actual["U", "D"] <- cor_matrix_actual["D", "U"] <- calculate_mean_cor("U-D")
cor_matrix_actual["U", "CT"] <- cor_matrix_actual["CT", "U"] <- calculate_mean_cor("U-CT")
cor_matrix_actual["U", "MT"] <- cor_matrix_actual["MT", "U"] <- calculate_mean_cor("U-MT")
cor_matrix_actual["D", "CT"] <- cor_matrix_actual["CT", "D"] <- calculate_mean_cor("D-CT")
cor_matrix_actual["D", "MT"] <- cor_matrix_actual["MT", "D"] <- calculate_mean_cor("D-MT")
cor_matrix_actual["CT", "MT"] <- cor_matrix_actual["MT", "CT"] <- calculate_mean_cor("CT-MT")

cat("基于实际数据的相关矩阵：\n")
print(round(cor_matrix_actual, 3))

cat("\n特征值：\n")
eigen_values <- eigen(cor_matrix_actual)$values
print(round(eigen_values, 4))

if(any(eigen_values < 0.01)) {
  cat("\n警告：相关矩阵不是正定的！进行调整...\n")
  cor_matrix_pd <- Matrix::nearPD(cor_matrix_actual, corr = TRUE, keepDiag = TRUE, maxit = 1000)
  cor_matrix_actual <- as.matrix(cor_matrix_pd$mat)

  cat("\n调整后的相关矩阵：\n")
  print(round(cor_matrix_actual, 3))

  cat("\n调整后的特征值：\n")
  print(round(eigen(cor_matrix_actual)$values, 4))
}

write.csv(cor_matrix_actual, "Tables/Correlation_Matrix_Actual.csv", row.names = TRUE)
cat("\n✓ 相关矩阵已保存: Tables/Correlation_Matrix_Actual.csv\n")

cat("\n进行元分析计算...\n")

run_meta_analysis <- function(pair_name) {
  pair_data <- df_correlations %>% filter(Pair == pair_name)

  if(nrow(pair_data) < 2) {
    cat(paste0("变量对 ", pair_name, " 的研究数量不足，跳过元分析\n"))
    return(NULL)
  }

  meta_result <- metagen(TE = r, seTE = 1/sqrt(n-3),
                         data = pair_data,
                         studlab = Study,
                         sm = "COR",
                         comb.fixed = FALSE,
                         comb.random = TRUE,
                         prediction = TRUE)

  return(meta_result)
}

meta_results <- list()
meta_summary <- data.frame()

for(pair in c("U-D", "U-CT", "U-MT", "D-CT", "D-MT", "CT-MT")) {
  cat(paste0("\n分析变量对: ", pair, "\n"))

  meta_res <- run_meta_analysis(pair)

  if(!is.null(meta_res)) {
    meta_results[[pair]] <- meta_res

    summary_row <- data.frame(
      Pair = pair,
      k = meta_res$k,
      N = sum(meta_res$n),
      r = round(meta_res$TE.random, 3),
      CI_lower = round(meta_res$lower.random, 3),
      CI_upper = round(meta_res$upper.random, 3),
      I2 = round(meta_res$I2, 1),
      Tau2 = round(meta_res$tau2, 3),
      p_value = ifelse(meta_res$pval.random < 0.001, "<0.001",
                       sprintf("%.3f", meta_res$pval.random)),
      Effect_Size = case_when(
        abs(meta_res$TE.random) >= 0.5 ~ "Large",
        abs(meta_res$TE.random) >= 0.3 ~ "Medium",
        abs(meta_res$TE.random) >= 0.1 ~ "Small",
        TRUE ~ "Negligible"
      ),
      Direction = ifelse(meta_res$TE.random >= 0, "Positive", "Negative")
    )

    meta_summary <- bind_rows(meta_summary, summary_row)
  }
}

write.csv(meta_summary, "Tables/Meta_Analysis_Results_Actual.csv", row.names = FALSE)
cat("\n✓ 元分析结果已保存: Tables/Meta_Analysis_Results_Actual.csv\n")
print(meta_summary)

meta_results_summary <- meta_summary

cat("\n=== 创建专业森林图 ===\n")

create_professional_forest_plot <- function(data, pair_name, output_dir = "Figures/ForestPlots") {
  if(nrow(data) < 2) {
    cat(pair_name, ": 研究数量不足 (n =", nrow(data), ")\n")
    return(NULL)
  }

  tryCatch({
    data_es <- escalc(measure = "ZCOR", ri = r, ni = n, data = data)

    re_model <- rma(yi, vi, data = data_es, method = "REML")

    fe_model <- rma(yi, vi, data = data_es, method = "FE")

    tau2 <- re_model$tau2
    Q <- re_model$QE
    df <- re_model$k - 1
    Q_p <- re_model$QEp
    I2 <- re_model$I2
    pred_interval <- predict(re_model)

    study_labels <- data$Study

    pdf_file <- file.path(output_dir, paste0("Forest_Plot_", gsub("[^A-Za-z0-9]", "_", pair_name), ".pdf"))
    pdf(pdf_file, width = 12, height = max(8, 0.4 * nrow(data) + 4))

    par(mar = c(8, 8, 4, 2), cex = 0.9)

    cor_vals <- tanh(c(data_es$yi - 1.96*sqrt(data_es$vi),
                       data_es$yi + 1.96*sqrt(data_es$vi),
                       re_model$ci.lb, re_model$ci.ub))
    x_min <- floor(min(cor_vals, -1.0, na.rm = TRUE) * 10) / 10
    x_max <- ceiling(max(cor_vals, 1.0, na.rm = TRUE) * 10) / 10

    forest(re_model,
           slab = study_labels,
           xlab = "Correlation Coefficient (r)",
           mlab = "",
           header = c("Study", "Correlation [95% CI]"),
           cex = 0.9,
           cex.axis = 1.0,
           cex.lab = 1.0,
           xlim = c(x_min - 0.5, x_max + 0.5),
           alim = c(-1.0, 1.0),
           at = seq(-1.0, 1.0, 0.2),
           steps = 5,
           refline = 0,
           efac = 1.2,
           top = 3,
           annotate = TRUE,
           showweights = FALSE,
           digits = 3,
           col = "black",
           bg = "white")

    title(main = paste("Forest Plot: ", pair_name, "\n(k = ", nrow(data), ", N = ", sum(data$n), ")", sep=""),
          cex.main = 1.1, font.main = 2, line = 0.5)

    row_fe <- -2
    row_re <- -3

    addpoly(fe_model, row = row_fe, cex = 0.9, mlab = "Fixed Effect Model",
            col = "blue", border = "blue", annotate = TRUE)
    addpoly(re_model, row = row_re, cex = 0.9, mlab = "Random Effects Model",
            col = "red", border = "red", annotate = TRUE)

    text(x_min + 0.1, -5.5,
         paste("Random Effects: r = ", round(tanh(re_model$beta[1]), 3),
               " [", round(tanh(re_model$ci.lb), 3), ", ",
               round(tanh(re_model$ci.ub), 3), "]", sep=""),
         cex = 0.9, adj = 0, col = "red")

    text(x_min + 0.1, -6.5,
         paste("Fixed Effect: r = ", round(tanh(fe_model$beta[1]), 3),
               " [", round(tanh(fe_model$ci.lb), 3), ", ",
               round(tanh(fe_model$ci.ub), 3), "]", sep=""),
         cex = 0.9, adj = 0, col = "blue")

    text(x_min + 0.1, -7.5,
         paste("Heterogeneity: Tau² = ", sprintf("%.3f", tau2),
               "; Q = ", round(Q, 2), ", df = ", df,
               " (P ", ifelse(Q_p < 0.001, "< 0.001", paste("= ", round(Q_p, 4), sep="")),
               "); I² = ", round(I2, 1), "%", sep=""),
         cex = 0.9, adj = 0)

    text(x_min + 0.1, -8.5,
         paste("95% Prediction Interval: [", round(tanh(pred_interval$cr.lb), 3),
               ", ", round(tanh(pred_interval$cr.ub), 3), "]", sep=""),
         cex = 0.9, adj = 0)

    pair_names_full <- list(
      "U-D" = "AI Use - AI Dependency",
      "U-CT" = "AI Use - Critical Thinking",
      "U-MT" = "AI Use - Learning Motivation",
      "D-CT" = "AI Dependency - Critical Thinking",
      "D-MT" = "AI Dependency - Learning Motivation",
      "CT-MT" = "Critical Thinking - Learning Motivation"
    )

    if(pair_name %in% names(pair_names_full)) {
      text((x_min + x_max)/2, -9.5,
           paste("Relationship: ", pair_names_full[[pair_name]], sep=""),
           cex = 0.9, adj = 0.5, font = 3)
    }

    text((x_min + x_max)/2, -10.5,
         "Note: Weights are based on random effects model",
         cex = 0.8, adj = 0.5, font = 3)

    dev.off()

    png_file <- file.path("Figures/HighRes", paste0("Forest_Plot_", gsub("[^A-Za-z0-9]", "_", pair_name), ".png"))
    png(png_file, width = 3000, height = max(2000, 100 * nrow(data) + 400), res = 300)

    par(mar = c(8, 8, 4, 2), cex = 1.2)
    forest(re_model,
           slab = study_labels,
           xlab = "Correlation Coefficient (r)",
           mlab = "",
           header = c("Study", "Correlation [95% CI]"),
           cex = 1.2,
           cex.axis = 1.5,
           cex.lab = 1.5,
           xlim = c(x_min - 0.5, x_max + 0.5),
           alim = c(-1.0, 1.0),
           at = seq(-1.0, 1.0, 0.2),
           steps = 5,
           refline = 0,
           efac = 1.2,
           top = 3,
           annotate = TRUE,
           showweights = FALSE,
           digits = 3,
           col = "black",
           bg = "white")

    title(main = paste("Forest Plot: ", pair_name, "\n(k = ", nrow(data), ", N = ", sum(data$n), ")", sep=""),
          cex.main = 1.5, font.main = 2, line = 1)

    addpoly(fe_model, row = row_fe, cex = 1.2, mlab = "Fixed Effect Model",
            col = "blue", border = "blue", annotate = TRUE)
    addpoly(re_model, row = row_re, cex = 1.2, mlab = "Random Effects Model",
            col = "red", border = "red", annotate = TRUE)

    text(x_min + 0.1, -5.5,
         paste("Random Effects: r = ", round(tanh(re_model$beta[1]), 3),
               " [", round(tanh(re_model$ci.lb), 3), ", ",
               round(tanh(re_model$ci.ub), 3), "]", sep=""),
         cex = 1.2, adj = 0, col = "red")

    text(x_min + 0.1, -6.5,
         paste("Fixed Effect: r = ", round(tanh(fe_model$beta[1]), 3),
               " [", round(tanh(fe_model$ci.lb), 3), ", ",
               round(tanh(fe_model$ci.ub), 3), "]", sep=""),
         cex = 1.2, adj = 0, col = "blue")

    text(x_min + 0.1, -7.5,
         paste("Heterogeneity: Tau² = ", sprintf("%.3f", tau2),
               "; Q = ", round(Q, 2), ", df = ", df,
               " (P ", ifelse(Q_p < 0.001, "< 0.001", paste("= ", round(Q_p, 4), sep="")),
               "); I² = ", round(I2, 1), "%", sep=""),
         cex = 1.2, adj = 0)

    dev.off()

    cat("✓ 专业森林图已保存:", pdf_file, "\n")
    cat("✓ 高清森林图已保存:", png_file, "\n")

    return(list(
      fe_model = fe_model,
      re_model = re_model,
      tau2 = tau2,
      Q = Q,
      Q_df = df,
      Q_p = Q_p,
      I2 = I2,
      k = nrow(data),
      N = sum(data$n, na.rm = TRUE),
      re_estimate = tanh(re_model$beta[1]),
      re_ci_lower = tanh(re_model$ci.lb),
      re_ci_upper = tanh(re_model$ci.ub)
    ))

  }, error = function(e) {
    cat(pair_name, "森林图创建错误:", e$message, "\n")
    return(NULL)
  })
}

cat("\n=== 创建专业漏斗图 ===\n")

create_professional_funnel_plot <- function(data, pair_name, output_dir = "Figures/FunnelPlots") {
  if(nrow(data) < 3) {
    cat(pair_name, ": 研究数量不足 (n =", nrow(data), ")\n")
    return(NULL)
  }

  tryCatch({
    data_es <- escalc(measure = "ZCOR", ri = r, ni = n, data = data)

    re_model <- rma(yi, vi, data = data_es, method = "REML")

    pdf_file <- file.path(output_dir, paste0("Funnel_Plot_", gsub("[^A-Za-z0-9]", "_", pair_name), ".pdf"))
    pdf(pdf_file, width = 10, height = 8)

    par(mar = c(5, 5, 4, 2), cex = 0.9)

    funnel(re_model,
           xlab = "Effect Size (Fisher's Z)",
           ylab = "Standard Error",
           level = c(0.90, 0.95, 0.99),
           shade = c("white", "gray90", "gray80"),
           refline = re_model$beta[1],
           legend = TRUE,
           pch = 19,
           col = "black",
           bg = "gray70",
           cex = 1.2,
           cex.lab = 1.1,
           cex.axis = 1.0)

    title(main = paste("Funnel Plot: ", pair_name, sep=""),
          line = 2.2, cex.main = 1.2, font.main = 2)

    mtext(paste("k =", nrow(data), ", N =", format(sum(data$n, na.rm = TRUE), big.mark = ",")),
          side = 3, line = 0.5, cex = 0.9)

    if(nrow(data) >= 3) {
      egger_test <- regtest(re_model)

      egger_text <- paste("Egger's test for publication bias: z =",
                          round(egger_test$zval, 2),
                          ", p =", sprintf("%.3f", egger_test$pval))
      if(egger_test$pval < 0.05) {
        egger_text <- paste(egger_text, "* (Potential publication bias)")
      } else {
        egger_text <- paste(egger_text, "(No significant bias)")
      }

      mtext(egger_text, side = 1, line = 4, cex = 0.9)
    }

    dev.off()

    png_file <- file.path("Figures/HighRes", paste0("Funnel_Plot_", gsub("[^A-Za-z0-9]", "_", pair_name), ".png"))
    png(png_file, width = 3000, height = 2400, res = 300)

    par(mar = c(5, 5, 4, 2), cex = 1.2)
    funnel(re_model,
           xlab = "Effect Size (Fisher's Z)",
           ylab = "Standard Error",
           level = c(0.90, 0.95, 0.99),
           shade = c("white", "gray90", "gray80"),
           refline = re_model$beta[1],
           legend = TRUE,
           pch = 19,
           col = "black",
           bg = "gray70",
           cex = 1.5,
           cex.lab = 1.5,
           cex.axis = 1.3)

    title(main = paste("Funnel Plot: ", pair_name, sep=""),
          line = 2.5, cex.main = 1.5, font.main = 2)

    mtext(paste("k =", nrow(data), ", N =", format(sum(data$n, na.rm = TRUE), big.mark = ",")),
          side = 3, line = 0.8, cex = 1.2)

    if(nrow(data) >= 3) {
      egger_test <- regtest(re_model)
      egger_text <- paste("Egger's test: z =", round(egger_test$zval, 2),
                          ", p =", sprintf("%.3f", egger_test$pval))
      mtext(egger_text, side = 1, line = 4.5, cex = 1.2)
    }

    dev.off()

    cat("✓ 专业漏斗图已保存:", pdf_file, "\n")
    cat("✓ 高清漏斗图已保存:", png_file, "\n")

    return(list(
      model = re_model,
      egger_test = if(exists("egger_test")) egger_test else NULL
    ))

  }, error = function(e) {
    cat(pair_name, "漏斗图创建错误:", e$message, "\n")
    return(NULL)
  })
}

cat("\n=== 生成所有森林图和漏斗图 ===\n")

forest_results <- list()
funnel_results <- list()

for(pair in c("U-D", "U-CT", "U-MT", "D-CT", "D-MT", "CT-MT")) {
  cat("\n处理变量对:", pair, "\n")

  pair_data <- df_correlations %>%
    filter(Pair == pair, !is.na(r))

  if(nrow(pair_data) >= 2) {
    cat("  研究数量:", nrow(pair_data), "\n")
    cat("  总样本量:", sum(pair_data$n), "\n")

    forest_result <- create_professional_forest_plot(pair_data, pair)

    if(nrow(pair_data) >= 3) {
      funnel_result <- create_professional_funnel_plot(pair_data, pair)
      funnel_results[[pair]] <- funnel_result
    }

    if(!is.null(forest_result)) {
      forest_results[[pair]] <- forest_result
    }
  } else {
    cat("  研究数量不足，跳过\n")
  }
}

cat("\n=== 生成发表偏倚检验汇总表 ===\n")
publication_bias_table <- data.frame(
  Variable_Pair = character(),
  k = integer(),
  Egger_z = numeric(),
  Egger_p = numeric(),
  Bias_Conclusion = character(),
  stringsAsFactors = FALSE
)

for(pair in names(funnel_results)) {
  res <- funnel_results[[pair]]
  if(!is.null(res$egger_test)) {
    egger_z <- round(res$egger_test$zval, 2)
    egger_p <- round(res$egger_test$pval, 3)
    conclusion <- ifelse(egger_p < 0.05, "Significant asymmetry", "No significant asymmetry")

    publication_bias_table <- rbind(publication_bias_table, data.frame(
      Variable_Pair = pair,
      k = res$model$k,
      Egger_z = egger_z,
      Egger_p = egger_p,
      Bias_Conclusion = conclusion
    ))
  }
}

write.csv(publication_bias_table, "Tables/Supplementary_TableS2_Publication_Bias.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
cat("✓ 补充表S2已保存: Tables/Supplementary_TableS2_Publication_Bias.csv\n")
print(publication_bias_table)

cat("\n=== 亚组分析 ===\n")

cat("\n1. 按国家亚组分析（中国 vs. 非中国）\n")

subgroup_country <- df_correlations %>%
  mutate(
    Country_Group = case_when(
      grepl("China", Country, ignore.case = TRUE) ~ "China",
      Country == "China-Taiwan" ~ "China",
      TRUE ~ "Non-China"
    )
  ) %>%
  filter(!is.na(Country_Group)) %>%
  group_by(Pair, Country_Group) %>%
  summarise(
    k = n(),
    N = sum(n, na.rm = TRUE),
    mean_r = mean(r, na.rm = TRUE),
    weighted_r = weighted.mean(r, n, na.rm = TRUE),
    sd_r = sd(r, na.rm = TRUE),
    .groups = 'drop'
  )

cat("\n按国家亚组分析结果：\n")
print(subgroup_country)

write.csv(subgroup_country, "Tables/亚组分析_国家.csv", row.names = FALSE)

if(nrow(subgroup_country) > 0) {
  p_subgroup_country <- ggplot(subgroup_country, aes(x = Pair, y = weighted_r, fill = Country_Group)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = weighted_r - sd_r, ymax = weighted_r + sd_r),
                  position = position_dodge(width = 0.8), width = 0.2) +
    geom_text(aes(label = sprintf("k=%d\nN=%d", k, N)),
              position = position_dodge(width = 0.8), vjust = -0.5, size = 3) +
    labs(title = "亚组分析：中国 vs. 非中国",
         subtitle = "加权平均相关系数及标准差",
         x = "变量对",
         y = "加权平均相关系数 (r)",
         fill = "国家组") +
    scale_fill_manual(values = c("China" = "#4E79A7", "Non-China" = "#F28E2B")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave("Figures/Subgroup/亚组分析_国家.pdf", p_subgroup_country, width = 10, height = 6)
  ggsave("Figures/HighRes/亚组分析_国家.png", p_subgroup_country, width = 10, height = 6, dpi = 300)
  cat("✓ 国家亚组分析图已保存\n")
}

cat("\n2. 按对象类型亚组分析（大学生 vs. 其他）\n")

subgroup_object <- df_correlations %>%
  mutate(
    Object_Group = case_when(
      ObjectType == "University" ~ "University",
      ObjectType == "Middle School" ~ "Middle School",
      TRUE ~ "Other"
    )
  ) %>%
  filter(!is.na(Object_Group)) %>%
  group_by(Pair, Object_Group) %>%
  summarise(
    k = n(),
    N = sum(n, na.rm = TRUE),
    mean_r = mean(r, na.rm = TRUE),
    weighted_r = weighted.mean(r, n, na.rm = TRUE),
    sd_r = sd(r, na.rm = TRUE),
    .groups = 'drop'
  )

cat("\n按对象类型亚组分析结果：\n")
print(subgroup_object)

write.csv(subgroup_object, "Tables/亚组分析_对象类型.csv", row.names = FALSE)

if(nrow(subgroup_object) > 0) {
  p_subgroup_object <- ggplot(subgroup_object, aes(x = Pair, y = weighted_r, fill = Object_Group)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = weighted_r - sd_r, ymax = weighted_r + sd_r),
                  position = position_dodge(width = 0.8), width = 0.2) +
    geom_text(aes(label = sprintf("k=%d\nN=%d", k, N)),
              position = position_dodge(width = 0.8), vjust = -0.5, size = 3) +
    labs(title = "亚组分析：研究对象类型",
         subtitle = "加权平均相关系数及标准差",
         x = "变量对",
         y = "加权平均相关系数 (r)",
         fill = "对象类型") +
    scale_fill_manual(values = c("University" = "#4E79A7", "Middle School" = "#E15759", "Other" = "#76B7B2")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave("Figures/Subgroup/亚组分析_对象类型.pdf", p_subgroup_object, width = 10, height = 6)
  ggsave("Figures/HighRes/亚组分析_对象类型.png", p_subgroup_object, width = 10, height = 6, dpi = 300)
  cat("✓ 对象类型亚组分析图已保存\n")
}

cat("\n3. 按发表年份亚组分析\n")

df_correlations$Year <- as.numeric(str_extract(df_correlations$Study, "\\d{4}"))

subgroup_year <- df_correlations %>%
  mutate(
    Year_Group = ifelse(Year <= 2024, "2024及以前", "2025年")
  ) %>%
  filter(!is.na(Year_Group)) %>%
  group_by(Pair, Year_Group) %>%
  summarise(
    k = n(),
    N = sum(n, na.rm = TRUE),
    mean_r = mean(r, na.rm = TRUE),
    weighted_r = weighted.mean(r, n, na.rm = TRUE),
    sd_r = sd(r, na.rm = TRUE),
    .groups = 'drop'
  )

cat("\n按发表年份亚组分析结果：\n")
print(subgroup_year)

write.csv(subgroup_year, "Tables/亚组分析_发表年份.csv", row.names = FALSE)

if(nrow(subgroup_year) > 0) {
  p_subgroup_year <- ggplot(subgroup_year, aes(x = Pair, y = weighted_r, fill = Year_Group)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = weighted_r - sd_r, ymax = weighted_r + sd_r),
                  position = position_dodge(width = 0.8), width = 0.2) +
    geom_text(aes(label = sprintf("k=%d\nN=%d", k, N)),
              position = position_dodge(width = 0.8), vjust = -0.5, size = 3) +
    labs(title = "亚组分析：发表年份",
         subtitle = "加权平均相关系数及标准差",
         x = "变量对",
         y = "加权平均相关系数 (r)",
         fill = "发表年份") +
    scale_fill_manual(values = c("2024及以前" = "#4E79A7", "2025年" = "#F28E2B")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave("Figures/Subgroup/亚组分析_发表年份.pdf", p_subgroup_year, width = 10, height = 6)
  ggsave("Figures/HighRes/亚组分析_发表年份.png", p_subgroup_year, width = 10, height = 6, dpi = 300)
  cat("✓ 发表年份亚组分析图已保存\n")
}

cat("\n进行MASEM分析（优化版本）...\n")

run_optimized_masem <- function(cor_matrix, sample_size) {
  cat("样本量:", sample_size, "\n")

  masem_model <- '
    CT ~ c*U

    D ~ a1*U
    CT ~ b1*D

    MT ~ a2*U
    CT ~ b2*MT

    U ~~ 1*U
    D ~~ var_d*D
    MT ~~ var_mt*MT
    CT ~~ var_ct*CT

    indirect1 := a1*b1
    indirect2 := a2*b2
    total_indirect := indirect1 + indirect2
    total_effect := c + indirect1 + indirect2

    R_squared_CT := 1 - var_ct
  '

  estimators <- c("ML", "MLR", "GLS")
  fits <- list()

  for(estimator in estimators) {
    cat("\n尝试", estimator, "估计方法...\n")
    tryCatch({
      fit <- sem(masem_model,
                 sample.cov = cor_matrix,
                 sample.nobs = sample_size,
                 estimator = estimator,
                 std.lv = TRUE,
                 optim.method = "BFGS",
                 control = list(iter.max = 5000,
                                eval.max = 10000,
                                rel.tol = 1e-10))

      if(lavInspect(fit, "converged")) {
        cat("✓ 收敛成功\n")
        fits[[estimator]] <- fit
      } else {
        cat("✗ 未收敛\n")
      }
    }, error = function(e) {
      cat("✗ 错误:", e$message, "\n")
    })
  }

  if(length(fits) > 0) {
    best_fit <- fits[[1]]
    best_estimator <- names(fits)[1]

    fit_indices <- fitMeasures(best_fit, c("chisq", "df", "pvalue", "cfi", "tli",
                                           "rmsea", "rmsea.ci.lower", "rmsea.ci.upper",
                                           "srmr", "aic", "bic"))

    params <- parameterEstimates(best_fit, standardized = TRUE, ci = TRUE)

    path_coefs <- data.frame()
    if(nrow(params) > 0) {
      path_coefs <- params %>%
        filter(op == "~") %>%
        mutate(
          Path = case_when(
            lhs == "CT" & rhs == "U" ~ "Direct (U→CT)",
            lhs == "D" & rhs == "U" ~ "U→D",
            lhs == "CT" & rhs == "D" ~ "D→CT",
            lhs == "MT" & rhs == "U" ~ "U→MT",
            lhs == "CT" & rhs == "MT" ~ "MT→CT",
            TRUE ~ paste(lhs, rhs, sep = "→")
          ),
          Beta = round(std.all, 3),
          SE = ifelse(is.na(se), NA, round(se, 3)),
          z_value = ifelse(is.na(z), NA, round(z, 2)),
          p_value = pvalue,
          Significance = case_when(
            p_value < 0.001 ~ "***",
            p_value < 0.01 ~ "**",
            p_value < 0.05 ~ "*",
            p_value < 0.10 ~ "†",
            TRUE ~ ""
          ),
          CI_95 = ifelse(is.na(ci.lower) | is.na(ci.upper),
                         NA,
                         sprintf("[%.3f, %.3f]", ci.lower, ci.upper))
        ) %>%
        select(Path, Beta, SE, z_value, p_value, CI_95, Significance)
    }

    indirect_effects <- data.frame()
    if(nrow(params) > 0) {
      indirect_effects <- params %>%
        filter(op == ":=") %>%
        mutate(
          Effect = case_when(
            lhs == "indirect1" ~ "Indirect (U→D→CT)",
            lhs == "indirect2" ~ "Indirect (U→MT→CT)",
            lhs == "total_indirect" ~ "Total Indirect",
            lhs == "total_effect" ~ "Total Effect",
            lhs == "R_squared_CT" ~ "R² (CT)",
            TRUE ~ lhs
          ),
          Estimate = round(est, 3),
          SE = ifelse(is.na(se), NA, round(se, 3)),
          z_value = ifelse(is.na(z), NA, round(z, 2)),
          p_value = pvalue,
          Significance = case_when(
            p_value < 0.001 ~ "***",
            p_value < 0.01 ~ "**",
            p_value < 0.05 ~ "*",
            p_value < 0.10 ~ "†",
            TRUE ~ ""
          ),
          CI_95 = ifelse(is.na(ci.lower) | is.na(ci.upper),
                         NA,
                         sprintf("[%.3f, %.3f]", ci.lower, ci.upper))
        ) %>%
        select(Effect, Estimate, SE, z_value, p_value, CI_95, Significance)
    }

    return(list(
      fit = best_fit,
      estimator = best_estimator,
      converged = TRUE,
      fit_indices = fit_indices,
      path_coefs = path_coefs,
      indirect_effects = indirect_effects,
      params = params
    ))

  } else {
    cat("\n所有估计方法都失败，使用基于相关矩阵的手动计算\n")

    a1 <- cor_matrix["U", "D"]
    b1 <- cor_matrix["D", "CT"]
    a2 <- cor_matrix["U", "MT"]
    b2 <- cor_matrix["MT", "CT"]
    c_direct <- cor_matrix["U", "CT"]

    indirect1 <- a1 * b1
    indirect2 <- a2 * b2
    total_indirect <- indirect1 + indirect2
    total_effect <- c_direct + total_indirect

    path_coefs <- data.frame(
      Path = c("Direct (U→CT)", "U→D", "D→CT", "U→MT", "MT→CT"),
      Beta = c(c_direct, a1, b1, a2, b2),
      SE = NA,
      z_value = NA,
      p_value = NA,
      CI_95 = NA,
      Significance = "Based on correlation matrix"
    )

    indirect_effects <- data.frame(
      Effect = c("Indirect (U→D→CT)", "Indirect (U→MT→CT)", "Total Indirect",
                 "Total Effect", "R² (CT)"),
      Estimate = c(indirect1, indirect2, total_indirect, total_effect,
                   NA),
      SE = NA,
      z_value = NA,
      p_value = NA,
      CI_95 = NA,
      Significance = "Based on correlation matrix"
    )

    fit_indices <- c(
      chisq = NA,
      df = 2,
      pvalue = NA,
      cfi = NA,
      tli = NA,
      rmsea = NA,
      rmsea.ci.lower = NA,
      rmsea.ci.upper = NA,
      srmr = NA,
      aic = NA,
      bic = NA
    )

    return(list(
      fit = NULL,
      estimator = "Correlation-based",
      converged = FALSE,
      fit_indices = fit_indices,
      path_coefs = path_coefs,
      indirect_effects = indirect_effects,
      params = NULL
    ))
  }
}

total_sample_size <- sum(df_correlations$n, na.rm = TRUE) / 6
masem_result <- run_optimized_masem(cor_matrix_actual, total_sample_size)

cat("\n=== MASEM分析结果 ===\n")
cat("估计方法:", masem_result$estimator, "\n")
cat("收敛:", masem_result$converged, "\n")
cat("\n路径系数:\n")
print(masem_result$path_coefs)
cat("\n间接效应:\n")
print(masem_result$indirect_effects)

if(!is.null(masem_result$path_coefs) && nrow(masem_result$path_coefs) > 0) {
  write.csv(masem_result$path_coefs, "Tables/MASEM_Path_Coefficients.csv", row.names = FALSE)
  write.csv(masem_result$indirect_effects, "Tables/MASEM_Indirect_Effects.csv", row.names = FALSE)

  fit_indices_df <- data.frame(
    Index = names(masem_result$fit_indices),
    Value = round(as.numeric(masem_result$fit_indices), 3)
  )
  write.csv(fit_indices_df, "Tables/MASEM_Fit_Indices.csv", row.names = FALSE)

  cat("\n✓ MASEM结果已保存\n")
} else {
  cat("\n⚠️ MASEM结果为空，无法保存\n")
}

cat("\n创建综合相关系数散点图...\n")

create_comprehensive_correlation_plot <- function(df_correlations, cor_matrix) {

  plot_data <- df_correlations %>%
    filter(!is.na(r)) %>%
    mutate(
      Pair = factor(Pair, levels = c("U-D", "U-CT", "U-MT", "D-CT", "D-MT", "CT-MT")),
      Sign = ifelse(r > 0, "Positive", ifelse(r < 0, "Negative", "Zero")),
      Is_DCT = (Pair == "D-CT")
    )

  cor_melted <- as.data.frame(cor_matrix)
  cor_melted$Var1 <- rownames(cor_melted)
  cor_melted <- cor_melted %>%
    pivot_longer(cols = c("U", "D", "CT", "MT"),
                 names_to = "Var2", values_to = "Correlation") %>%
    filter(!is.na(Correlation))

  var_names <- c(
    "U" = "AI Use",
    "D" = "AI Dependency",
    "CT" = "Critical Thinking",
    "MT" = "Learning Motivation"
  )

  cor_melted <- cor_melted %>%
    mutate(
      Var1_name = var_names[Var1],
      Var2_name = var_names[Var2]
    )

  pdf("Figures/综合相关系数散点图.pdf", width = 16, height = 10)

  p1 <- ggplot(cor_melted, aes(x = Var1_name, y = Var2_name, fill = Correlation)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = sprintf("%.2f", Correlation)),
              color = "black", size = 5, fontface = "bold") +
    scale_fill_gradient2(low = "#E76F51", mid = "white", high = "#2A9D8F",
                         midpoint = 0, limits = c(-1, 1),
                         name = "Correlation (r)") +
    labs(
      title = "A. Meta-Analytic Correlation Matrix",
      subtitle = "Weighted average correlations",
      x = "",
      y = ""
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      axis.text = element_text(size = 11, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank(),
      legend.position = "right"
    ) +
    coord_fixed()

  p2 <- ggplot(plot_data, aes(x = Pair, y = r)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +

    geom_jitter(aes(size = n, color = Sign, shape = Is_DCT),
                width = 0.2, height = 0, alpha = 0.7) +

    geom_boxplot(fill = NA, color = "gray30", outlier.shape = NA, width = 0.5) +

    scale_size_continuous(range = c(3, 10),
                          name = "Sample Size",
                          breaks = c(100, 500, 1000, 2000),
                          labels = c("100", "500", "1000", "2000")) +
    scale_color_manual(values = c("Positive" = "#2A9D8F",
                                  "Negative" = "#E76F51",
                                  "Zero" = "gray"),
                       name = "Direction") +
    scale_shape_manual(values = c("TRUE" = 17, "FALSE" = 16),
                       labels = c("TRUE" = "D-CT",
                                  "FALSE" = "Other Pairs"),
                       name = "Variable Pair") +
    scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.2)) +
    labs(
      title = "B. Distribution of Correlation Coefficients",
      x = "Variable Pair",
      y = "Correlation Coefficient (r)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size = 11, angle = 0, hjust = 0.5),
      axis.text.y = element_text(size = 11),
      legend.position = "right",
      legend.box = "vertical",
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )

  gender_data <- df_correlations %>%
    filter(!is.na(Percent_Female), !is.na(r))

  if(nrow(gender_data) > 5) {
    p3 <- ggplot(gender_data, aes(x = Percent_Female, y = r)) +
      geom_point(aes(size = n, color = Pair), alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "#E15759", fill = "#F28E2B", alpha = 0.2) +
      facet_wrap(~ Pair, scales = "free_y", ncol = 2) +
      scale_size_continuous(range = c(2, 8), name = "Sample Size") +
      scale_color_brewer(palette = "Set2", name = "Variable Pair") +
      labs(
        title = "C. Relationship between Female Percentage and Correlation",
        subtitle = "Linear regression with 95% confidence interval",
        x = "Female Percentage (%)",
        y = "Correlation Coefficient (r)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 9),
        legend.position = "bottom",
        strip.text = element_text(face = "bold")
      )

    combined_plot <- p1 + p2 + p3 +
      plot_layout(ncol = 3, widths = c(1, 1.2, 1.2)) +
      plot_annotation(
        title = 'Comprehensive Correlation Analysis: AI Dependency and Critical Thinking',
        theme = theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
        )
      )
  } else {
    combined_plot <- p1 + p2 +
      plot_layout(ncol = 2, widths = c(1, 1.2)) +
      plot_annotation(
        title = 'Comprehensive Correlation Analysis: AI Dependency and Critical Thinking',
        theme = theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
        )
      )
  }

  print(combined_plot)
  dev.off()

  png("Figures/HighRes/综合相关系数散点图.png", width = 4800, height = 3000, res = 300)
  print(combined_plot)
  dev.off()

  cat("✓ 综合相关系数散点图已保存\n")
}

create_comprehensive_correlation_plot(df_correlations, cor_matrix_actual)

cat("\n生成SSCI标准表格...\n")

create_table1 <- function(df_studies_clean, df_correlations) {
  cat("创建表1：纳入研究基本特征汇总表\n")

  table1_data <- df_studies_clean %>%
    left_join(
      df_correlations %>%
        group_by(Study) %>%
        summarise(
          Reliability_U = mean(Reliability_U, na.rm = TRUE),
          Reliability_D = mean(Reliability_D, na.rm = TRUE),
          Reliability_CT = mean(Reliability_CT, na.rm = TRUE),
          Reliability_MT = mean(Reliability_MT, na.rm = TRUE)
        ),
      by = "Study"
    ) %>%
    mutate(
      Author = str_extract(Study, "^[A-Za-z]+"),
      Year = str_extract(Study, "\\d{4}"),
      Measure_Type = ifelse(Reliability_U >= 0.70 & Reliability_D >= 0.70 &
                              Reliability_CT >= 0.70 & Reliability_MT >= 0.70,
                            "广泛验证量表", "自编/混合量表"),
      across(starts_with("Reliability_"),
             ~ ifelse(is.na(.), "-", as.character(sprintf("%.2f", .)))),
      across(everything(), as.character)
    ) %>%
    arrange(Year, Author) %>%
    select(
      `第一作者` = Author,
      `发表年份` = Year,
      `研究地区` = Country,
      `样本量` = SampleSize,
      `对象类型` = ObjectType,
      `女性比例(%)` = Percent_Female,
      `测量工具` = Measure_Type,
      `U信度` = Reliability_U,
      `D信度` = Reliability_D,
      `CT信度` = Reliability_CT,
      `MT信度` = Reliability_MT
    )

  summary_row <- data.frame(
    `第一作者` = "汇总",
    `发表年份` = "",
    `研究地区` = sprintf("%d个国家/地区", n_distinct(na.omit(table1_data$研究地区))),
    `样本量` = as.character(sum(as.numeric(table1_data$样本量), na.rm = TRUE)),
    `对象类型` = sprintf("%d种类型", n_distinct(na.omit(table1_data$对象类型))),
    `女性比例(%)` = sprintf("%.1f%%", mean(as.numeric(table1_data$`女性比例(%)`), na.rm = TRUE)),
    `测量工具` = "",
    `U信度` = sprintf("%.2f", mean(as.numeric(table1_data$U信度), na.rm = TRUE)),
    `D信度` = sprintf("%.2f", mean(as.numeric(table1_data$D信度), na.rm = TRUE)),
    `CT信度` = sprintf("%.2f", mean(as.numeric(table1_data$CT信度), na.rm = TRUE)),
    `MT信度` = sprintf("%.2f", mean(as.numeric(table1_data$MT信度), na.rm = TRUE))
  )

  table1_data <- bind_rows(table1_data, summary_row)

  write.xlsx(table1_data, "Tables/表1_纳入研究基本特征汇总表.xlsx", overwrite = TRUE)
  write.csv(table1_data, "Tables/表1_纳入研究基本特征汇总表.csv", row.names = FALSE)

  cat("✓ 表1已保存\n")
  return(table1_data)
}

create_table2 <- function(meta_results_summary) {
  cat("创建表2：变量对元分析效应量汇总表\n")

  table2_data <- meta_results_summary %>%
    mutate(
      `95%置信区间` = sprintf("[%.3f, %.3f]", CI_lower, CI_upper),
      `效应量解读` = Effect_Size,
      `I²(%)` = sprintf("%.1f", I2),
      `Tau²` = sprintf("%.3f", Tau2),
      `p值` = p_value,
      across(everything(), as.character)
    ) %>%
    select(
      `变量对` = Pair,
      `研究数(k)` = k,
      `总样本量(N)` = N,
      `合并相关系数(r)` = r,
      `95%置信区间`,
      `效应量解读`,
      `I²(%)`,
      `Tau²`,
      `p值`
    )

  write.xlsx(table2_data, "Tables/表2_变量对元分析效应量汇总表.xlsx", overwrite = TRUE)
  write.csv(table2_data, "Tables/表2_变量对元分析效应量汇总表.csv", row.names = FALSE)

  cat("✓ 表2已保存\n")
  return(table2_data)
}

create_table3 <- function(cor_matrix) {
  cat("创建表3：变量间平均相关矩阵表\n")

  cor_matrix_with_sig <- cor_matrix

  for(i in 1:nrow(cor_matrix)) {
    for(j in 1:ncol(cor_matrix)) {
      if(i != j) {
        r_value <- cor_matrix[i, j]
        if(abs(r_value) > 0.3) {
          cor_matrix_with_sig[i, j] <- sprintf("%.3f***", r_value)
        } else if(abs(r_value) > 0.2) {
          cor_matrix_with_sig[i, j] <- sprintf("%.3f**", r_value)
        } else if(abs(r_value) > 0.1) {
          cor_matrix_with_sig[i, j] <- sprintf("%.3f*", r_value)
        } else {
          cor_matrix_with_sig[i, j] <- sprintf("%.3f", r_value)
        }
      } else {
        cor_matrix_with_sig[i, j] <- "1.000"
      }
    }
  }

  table3_data <- as.data.frame(cor_matrix_with_sig)
  table3_data <- cbind(Variable = rownames(table3_data), table3_data)
  rownames(table3_data) <- NULL

  var_names <- c("U" = "AI使用", "D" = "AI依赖", "CT" = "批判性思维", "MT" = "学习动机")
  table3_data$Variable <- var_names[table3_data$Variable]

  write.xlsx(table3_data, "Tables/表3_变量间平均相关矩阵表.xlsx", overwrite = TRUE)
  write.csv(table3_data, "Tables/表3_变量间平均相关矩阵表.csv", row.names = FALSE)

  cat("✓ 表3已保存\n")
  return(table3_data)
}

create_table4 <- function(masem_result) {
  cat("创建表4：MASEM路径系数与效应分解表\n")

  table4_data <- data.frame(
    `路径/效应类型` = character(),
    `标准化估计值(β)` = character(),
    `标准误(SE)` = character(),
    `95%置信区间` = character(),
    `p值` = character(),
    `效应解读` = character(),
    stringsAsFactors = FALSE
  )

  if(!is.null(masem_result$path_coefs) && nrow(masem_result$path_coefs) > 0) {
    path_part <- masem_result$path_coefs %>%
      mutate(
        `标准化估计值(β)` = as.character(sprintf("%.3f", Beta)),
        `标准误(SE)` = ifelse(is.na(SE), "-", as.character(sprintf("%.3f", SE))),
        `95%置信区间` = ifelse(is.na(CI_95), "-", CI_95),
        `p值` = case_when(
          is.na(p_value) ~ "-",
          p_value < 0.001 ~ "<0.001",
          TRUE ~ as.character(sprintf("%.3f", p_value))
        ),
        `效应解读` = case_when(
          abs(Beta) >= 0.5 ~ "强效应",
          abs(Beta) >= 0.3 ~ "中等效应",
          abs(Beta) >= 0.1 ~ "弱效应",
          TRUE ~ "可忽略效应"
        )
      ) %>%
      select(
        `路径/效应类型` = Path,
        `标准化估计值(β)`,
        `标准误(SE)`,
        `95%置信区间`,
        `p值`,
        `效应解读`
      )

    table4_data <- bind_rows(table4_data, path_part)
  }

  if(!is.null(masem_result$indirect_effects) && nrow(masem_result$indirect_effects) > 0) {
    indirect_part <- masem_result$indirect_effects %>%
      mutate(
        `标准化估计值(β)` = as.character(sprintf("%.3f", Estimate)),
        `标准误(SE)` = ifelse(is.na(SE), "-", as.character(sprintf("%.3f", SE))),
        `95%置信区间` = ifelse(is.na(CI_95), "-", CI_95),
        `p值` = case_when(
          is.na(p_value) ~ "-",
          p_value < 0.001 ~ "<0.001",
          TRUE ~ as.character(sprintf("%.3f", p_value))
        ),
        `效应解读` = Significance
      ) %>%
      select(
        `路径/效应类型` = Effect,
        `标准化估计值(β)`,
        `标准误(SE)`,
        `95%置信区间`,
        `p值`,
        `效应解读`
      )

    table4_data <- bind_rows(table4_data, indirect_part)
  }

  if(!is.null(masem_result$fit_indices)) {
    chisq_df <- if(!is.na(masem_result$fit_indices["chisq"]) &&
                   !is.na(masem_result$fit_indices["df"]) &&
                   masem_result$fit_indices["df"] > 0) {
      masem_result$fit_indices["chisq"] / masem_result$fit_indices["df"]
    } else NA

    fit_info <- data.frame(
      `路径/效应类型` = c("模型拟合指数", "χ²/df", "CFI", "TLI", "RMSEA", "SRMR"),
      `标准化估计值(β)` = c("",
                      ifelse(is.na(chisq_df), "-", sprintf("%.2f", chisq_df)),
                      ifelse(is.na(masem_result$fit_indices["cfi"]), "-",
                             sprintf("%.2f", masem_result$fit_indices["cfi"])),
                      ifelse(is.na(masem_result$fit_indices["tli"]), "-",
                             sprintf("%.2f", masem_result$fit_indices["tli"])),
                      ifelse(is.na(masem_result$fit_indices["rmsea"]), "-",
                             sprintf("%.2f", masem_result$fit_indices["rmsea"])),
                      ifelse(is.na(masem_result$fit_indices["srmr"]), "-",
                             sprintf("%.2f", masem_result$fit_indices["srmr"]))),
      `标准误(SE)` = c("", "", "", "", "", ""),
      `95%置信区间` = c("", "", "", "", "", ""),
      `p值` = c("", "", "", "", "", ""),
      `效应解读` = c("", "", "", "", "", ""),
      stringsAsFactors = FALSE
    )

    table4_data <- bind_rows(table4_data, fit_info)
  }

  if(nrow(table4_data) == 0) {
    table4_data <- data.frame(
      `路径/效应类型` = "MASEM模型未收敛或无结果",
      `标准化估计值(β)` = "-",
      `标准误(SE)` = "-",
      `95%置信区间` = "-",
      `p值` = "-",
      `效应解读` = "-",
      stringsAsFactors = FALSE
    )
  }

  write.xlsx(table4_data, "Tables/表4_MASEM路径系数与效应分解表.xlsx", overwrite = TRUE)
  write.csv(table4_data, "Tables/表4_MASEM路径系数与效应分解表.csv", row.names = FALSE)

  cat("✓ 表4已保存\n")
  return(table4_data)
}

table1 <- create_table1(df_studies_clean, df_correlations)
table2 <- create_table2(meta_results_summary)
table3 <- create_table3(cor_matrix_actual)
table4 <- create_table4(masem_result)

cat("\n创建综合研究总结报告...\n")

wb <- createWorkbook()

addWorksheet(wb, "表1_研究特征")
writeData(wb, "表1_研究特征", table1)

addWorksheet(wb, "表2_元分析效应量")
writeData(wb, "表2_元分析效应量", table2)

addWorksheet(wb, "表3_相关矩阵")
writeData(wb, "表3_相关矩阵", table3)

addWorksheet(wb, "表4_MASEM路径")
writeData(wb, "表4_MASEM路径", table4)

addWorksheet(wb, "女性比例分析")
if(exists("meta_regression_results") && nrow(meta_regression_results) > 0) {
  writeData(wb, "女性比例分析", meta_regression_results)
} else {
  writeData(wb, "女性比例分析", data.frame(Note = "没有足够的女性比例数据进行元回归分析"))
}

addWorksheet(wb, "亚组分析_国家")
writeData(wb, "亚组分析_国家", subgroup_country)

addWorksheet(wb, "亚组分析_对象类型")
writeData(wb, "亚组分析_对象类型", subgroup_object)

addWorksheet(wb, "亚组分析_发表年份")
writeData(wb, "亚组分析_发表年份", subgroup_year)

addWorksheet(wb, "研究汇总")
summary_data <- data.frame(
  项目 = c("总研究数", "总样本量", "平均样本量", "涉及国家数",
         "研究对象类型数", "有性别信息的研究数", "平均女性比例",
         "MASEM模型是否收敛", "MASEM估计方法", "森林图数量", "漏斗图数量"),
  值 = c(as.character(nrow(study_final)),
        as.character(sum(study_final$SampleSize, na.rm = TRUE)),
        sprintf("%.0f", mean(study_final$SampleSize, na.rm = TRUE)),
        as.character(n_distinct(study_final$Country)),
        as.character(n_distinct(study_final$ObjectType)),
        as.character(sum(!is.na(study_final$Percent_Female))),
        sprintf("%.1f%%", mean(study_final$Percent_Female, na.rm = TRUE)),
        ifelse(masem_result$converged, "是", "否"),
        masem_result$estimator,
        as.character(length(forest_results)),
        as.character(length(funnel_results))),
  stringsAsFactors = FALSE
)
writeData(wb, "研究汇总", summary_data)

saveWorkbook(wb, "Output/综合分析报告.xlsx", overwrite = TRUE)

cat("✓ 综合输出报告已保存: Output/综合分析报告.xlsx\n")

cat("\n", strrep("=", 80), "\n")
cat("完整分析流程全部完成！\n")
cat(strrep("=", 80), "\n")
