library(tidyverse)
library(readxl)
library(fixest)
library(modelsummary)
library(scales)
library(broom)
library(patchwork)
library(plm)

FONT <- "HiraKakuProN-W3"

extract_school_data <- function(file_paths, skip_pattern, out_csv) {
  df <- file_paths |>
    keep(file.exists) |>
    set_names(basename(.)) |>
    map_df(function(x) {
      skip_num <- if (str_detect(basename(x), skip_pattern)) 2 else 3
      read_excel(x, skip = skip_num, .name_repair = "unique_quiet")
    }, .id = "source_file")

  df <- df |>
    mutate(
      enrolment_male_ratio = 入学者数...5 / 入学者数...3,
      graduate_male_ratio  = 卒業者数...6 / 卒業者数...4
    ) |>
    select(
      source_file,
      prefecture = ...1,
      school = 学校数,
      enrolment = 入学者数...3,
      graduate = 卒業者数...4,
      enrolment_male = 入学者数...5,
      graduate_male = 卒業者数...6,
      enrolment_male_ratio,
      graduate_male_ratio
    )

  write_csv(df, out_csv)
  df
}

file_paths_3gr <- c(
  sprintf("nurse3_graduate/3gr0%d.xls", 7:9),
  "nurse3_graduate/3gr10.xls",
  sprintf("nurse3_graduate/3gr%d.xlsx", 11:13),
  sprintf("nurse3_graduate/3gr%d.xls", 14:17),
  sprintf("nurse3_graduate/3gr%d.xlsx", 18:25)
)

file_paths_4gr <- c(
  sprintf("nurse4_graduate/4gr0%d.xls", 7:9),
  "nurse4_graduate/4gr10.xls",
  sprintf("nurse4_graduate/4gr%d.xlsx", 11:13),
  sprintf("nurse4_graduate/4gr%d.xls", 14:17),
  "nurse4_graduate/4gr19.xls",
  sprintf("nurse4_graduate/4gr%d.xlsx", 18:25)
)

file_paths_5gr <- c(
  sprintf("nurse5_graduate/5gr0%d.xls", 7:9),
  "nurse5_graduate/5gr10.xls",
  sprintf("nurse5_graduate/5gr%d.xlsx", 11:13),
  sprintf("nurse5_graduate/5gr%d.xls", 14:17),
  "nurse5_graduate/5gr18.xlsx",
  "nurse5_graduate/5gr19.xls",
  sprintf("nurse5_graduate/5gr%d.xlsx", 20:25)
)

if (!file.exists("output3gr.csv")) extract_school_data(file_paths_3gr, "3gr0[789]", "output3gr.csv")
if (!file.exists("output4gr.csv")) extract_school_data(file_paths_4gr, "4gr0[789]", "output4gr.csv")
if (!file.exists("output5gr.csv")) extract_school_data(file_paths_5gr, "5gr0[789]", "output5gr.csv")

load_gr <- function(path, type) {
  read_csv(path, show_col_types = FALSE) |>
    mutate(
      school_type = type,
      grad_year   = 2000 + as.numeric(str_extract(source_file, "(?<=gr)[0-9]+")),
      prefecture  = str_remove(prefecture, "^[0-9]+")
    )
}

df_all <- bind_rows(
  load_gr("output3gr.csv", "専門学校"),
  load_gr("output4gr.csv", "大学"),
  load_gr("output5gr.csv", "短期大学")
) |>
  mutate(entry_year = if_else(school_type == "大学", grad_year - 4L, grad_year - 3L)) |>
  filter(prefecture != "全国")

ENTRY_MIN <- 2012
ENTRY_MAX <- 2021

df_panel <- df_all |>
  filter(entry_year >= ENTRY_MIN & entry_year <= ENTRY_MAX) |>
  select(prefecture, entry_year, school_type, enrolment, enrolment_male) |>
  pivot_wider(names_from = school_type, values_from = c(enrolment, enrolment_male)) |>
  drop_na(enrolment_大学, enrolment_専門学校, enrolment_短期大学,
          enrolment_male_大学, enrolment_male_専門学校, enrolment_male_短期大学) |>
  mutate(
    total            = enrolment_大学 + enrolment_専門学校 + enrolment_短期大学,
    uni              = enrolment_大学,
    male             = enrolment_male_大学 + enrolment_male_専門学校 + enrolment_male_短期大学,
    total_male_ratio = male / total,
    uni_ratio        = enrolment_大学 / total,
    junior_col_ratio = enrolment_短期大学 / total
  ) |>
  filter(total > 0)

stopifnot(nrow(df_panel) == 470)
stopifnot(all(df_panel$uni_ratio > 0))

df_control <- read_csv("SSDSE.csv", locale = locale(encoding = "CP932"), show_col_types = FALSE) |>
  mutate(
    aging_rate       = over_65 / population,
    active_job_ratio = recruit / jobhunt,
    entry_year       = as.numeric(year)
  ) |>
  select(entry_year, prefecture, aging_rate, active_job_ratio)

df_panel <- df_panel |> left_join(df_control, by = c("entry_year", "prefecture"))
stopifnot(!anyNA(df_panel$aging_rate), !anyNA(df_panel$active_job_ratio))

df_panel <- df_panel |>
  mutate(
    log_male  = log(male),
    log_total = log(total),
    log_uni   = log(uni),
    total_100 = total / 100,
    uni_100   = uni / 100
  )

nat_2021 <- df_panel |>
  filter(entry_year == 2021) |>
  summarise(
    uni_male  = sum(enrolment_male_大学),
    uni_total = sum(enrolment_大学),
    voc_male  = sum(enrolment_male_専門学校),
    voc_total = sum(enrolment_専門学校)
  )

cat(sprintf("大学 男子比率(2021) = %d / %d = %.2f%%\n",
            nat_2021$uni_male, nat_2021$uni_total,
            nat_2021$uni_male / nat_2021$uni_total * 100))
cat(sprintf("専門学校 男子比率(2021) = %d / %d = %.2f%%\n",
            nat_2021$voc_male, nat_2021$voc_total,
            nat_2021$voc_male / nat_2021$voc_total * 100))

tbl_2021 <- matrix(
  c(nat_2021$uni_male, nat_2021$uni_total - nat_2021$uni_male,
    nat_2021$voc_male, nat_2021$voc_total - nat_2021$voc_male),
  nrow = 2, byrow = TRUE,
  dimnames = list(c("大学", "専門学校"), c("男子", "女子"))
)
print(tbl_2021)
print(chisq.test(tbl_2021))
print(chisq.test(tbl_2021, correct = FALSE))
print(prop.test(
  x = c(nat_2021$uni_male, nat_2021$voc_male),
  n = c(nat_2021$uni_total, nat_2021$voc_total)
))

nat <- df_panel |>
  group_by(entry_year) |>
  summarise(across(starts_with("enrolment_"), sum), .groups = "drop") |>
  mutate(total = enrolment_大学 + enrolment_専門学校 + enrolment_短期大学)

types <- c("大学", "専門学校", "短期大学")

shares <- map_dfr(types, function(k) {
  tibble(
    school_type = k,
    entry_year  = nat$entry_year,
    share       = nat[[paste0("enrolment_", k)]] / nat$total,
    male_ratio  = nat[[paste0("enrolment_male_", k)]] / nat[[paste0("enrolment_", k)]]
  )
})

decomp <- shares |>
  filter(entry_year %in% c(ENTRY_MIN, ENTRY_MAX)) |>
  pivot_wider(names_from = entry_year, values_from = c(share, male_ratio), names_sep = "_") |>
  rename(s0 = paste0("share_", ENTRY_MIN),      s1 = paste0("share_", ENTRY_MAX),
         m0 = paste0("male_ratio_", ENTRY_MIN), m1 = paste0("male_ratio_", ENTRY_MAX)) |>
  mutate(
    composition = (s1 - s0) * (m0 + m1) / 2,
    within      = (s0 + s1) / 2 * (m1 - m0)
  )

print(decomp |> mutate(across(where(is.numeric), ~ round(.x * 100, 3))))

r0 <- sum(nat$enrolment_male_大学[nat$entry_year == ENTRY_MIN],
          nat$enrolment_male_専門学校[nat$entry_year == ENTRY_MIN],
          nat$enrolment_male_短期大学[nat$entry_year == ENTRY_MIN]) /
  nat$total[nat$entry_year == ENTRY_MIN]
r1 <- sum(nat$enrolment_male_大学[nat$entry_year == ENTRY_MAX],
          nat$enrolment_male_専門学校[nat$entry_year == ENTRY_MAX],
          nat$enrolment_male_短期大学[nat$entry_year == ENTRY_MAX]) /
  nat$total[nat$entry_year == ENTRY_MAX]
comp_total   <- sum(decomp$composition)
within_total <- sum(decomp$within)

cat(sprintf("\n実際の変化 %+.2f pt = 構成効果 %+.2f pt + 課程内効果 %+.2f pt\n",
            (r1 - r0) * 100, comp_total * 100, within_total * 100))

VAR_LABELS <- c(
  total_male_ratio = "男子入学者比率",
  uni_ratio        = "大学シェア",
  junior_col_ratio = "短期大学シェア",
  aging_rate       = "高齢化率",
  active_job_ratio = "有効求人倍率",
  total             = "総入学者数",
  uni               = "大学入学者数",
  log_total         = "log(総入学者数)",
  log_uni           = "log(大学入学者数)",
  total_100         = "総入学者数（100人単位）",
  uni_100           = "大学入学者数（100人単位）"
)

datasummary(
  (`男子入学者比率`   = total_male_ratio) +
  (`大学シェア`       = uni_ratio) +
  (`短期大学シェア`   = junior_col_ratio) +
  (`高齢化率`         = aging_rate) +
  (`有効求人倍率`     = active_job_ratio) +
  (`総入学者数`       = total) +
  (`大学入学者数`     = uni) +
  (`log(総入学者数)`  = log_total) +
  (`log(大学入学者数)`= log_uni) ~
    (`平均` = Mean) + (`標準偏差` = SD) + (`最小` = Min) + (`中央値` = Median) + (`最大` = Max),
  data   = df_panel,
  fmt    = 3,
  output = "latex",
  title  = "記述統計（N = 470、2012〜2021年度）"
)

modelsA <- list(
  "model1" = feols(total_male_ratio ~ uni_ratio,
                    data = df_panel, vcov = "hetero"),
  "model2" = feols(total_male_ratio ~ uni_ratio | prefecture,
                    data = df_panel, cluster = ~ prefecture),
  "model3" = feols(total_male_ratio ~ uni_ratio | prefecture + entry_year,
                    data = df_panel, cluster = ~ prefecture),
  "model4" = feols(total_male_ratio ~ uni_ratio + junior_col_ratio |
                      prefecture + entry_year,
                    data = df_panel, cluster = ~ prefecture),
  "model5" = feols(total_male_ratio ~ uni_ratio + junior_col_ratio +
                      aging_rate + active_job_ratio |
                      prefecture + entry_year,
                    data = df_panel, cluster = ~ prefecture)
)

rows_fe_A <- tibble::tribble(
  ~term,             ~model1, ~model2, ~model3, ~model4, ~model5,
  "都道府県固定効果", "なし",  "あり",  "あり",  "あり",  "あり",
  "年度固定効果",     "なし",  "なし",  "あり",  "あり",  "あり"
)

modelsummary(
  modelsA, stars = TRUE, fmt = 3,
  coef_map = VAR_LABELS,
  gof_map  = c("nobs", "r.squared"),
  add_rows = rows_fe_A,
  notes    = "列(1)は不均一分散頑健標準誤差、列(2)〜(5)は都道府県単位でクラスター化した標準誤差。",
  output   = "latex",
  title    = "regression_table_A"
)

modelsB <- list(
  "model6" = feols(total_male_ratio ~ total_100 + junior_col_ratio +
                      aging_rate + active_job_ratio |
                      prefecture + entry_year,
                    data = df_panel, cluster = ~ prefecture),
  "model7" = feols(total_male_ratio ~ log_total + junior_col_ratio +
                      aging_rate + active_job_ratio |
                      prefecture + entry_year,
                    data = df_panel, cluster = ~ prefecture),
  "model8" = feols(total_male_ratio ~ uni_100 + junior_col_ratio +
                      aging_rate + active_job_ratio |
                      prefecture + entry_year,
                    data = df_panel, cluster = ~ prefecture),
  "model9" = feols(total_male_ratio ~ log_uni + junior_col_ratio +
                      aging_rate + active_job_ratio |
                      prefecture + entry_year,
                    data = df_panel, cluster = ~ prefecture)
)

rows_fe_B <- tibble::tribble(
  ~term,             ~model6, ~model7, ~model8, ~model9,
  "都道府県固定効果", "あり",  "あり",  "あり",  "あり",
  "年度固定効果",     "あり",  "あり",  "あり",  "あり"
)

modelsummary(
  modelsB, stars = TRUE, fmt = 3,
  coef_map = VAR_LABELS,
  gof_map  = c("nobs", "r.squared"),
  add_rows = rows_fe_B,
  notes    = "標準誤差はすべて都道府県単位でクラスター化。総入学者数・大学入学者数は100人単位に変換して推定（model6・model8）。",
  output   = "latex",
  title    = "regression_table_B"
)

df_pdata <- pdata.frame(df_panel, index = c("prefecture", "entry_year"))
hausman_re <- plm(total_male_ratio ~ uni_ratio + aging_rate + active_job_ratio,
                   data = df_pdata, model = "random")
hausman_fe_plm <- plm(total_male_ratio ~ uni_ratio + aging_rate + active_job_ratio,
                       data = df_pdata, model = "within")
print(phtest(hausman_fe_plm, hausman_re))

resid_ar1 <- function(model, data) {
  d <- data |>
    mutate(r = as.numeric(resid(model))) |>
    arrange(prefecture, entry_year) |>
    group_by(prefecture) |>
    mutate(r_lag = lag(r)) |>
    ungroup() |>
    filter(!is.na(r_lag))
  c(rho = cor(d$r, d$r_lag), var = var(as.numeric(resid(model))))
}

m_pool_diag <- feols(total_male_ratio ~ uni_ratio + junior_col_ratio +
                        aging_rate + active_job_ratio, data = df_panel)
m_twfe_diag <- feols(total_male_ratio ~ uni_ratio + junior_col_ratio +
                        aging_rate + active_job_ratio | prefecture + entry_year,
                      data = df_panel)

print(resid_ar1(m_pool_diag, df_panel))
print(resid_ar1(m_twfe_diag, df_panel))

print(pbgtest(plm(total_male_ratio ~ uni_ratio + junior_col_ratio + aging_rate +
                     active_job_ratio, data = df_pdata, model = "within", effect = "twoways")))

etable(
  feols(total_male_ratio ~ uni_ratio + junior_col_ratio + aging_rate + active_job_ratio |
          prefecture + entry_year, data = df_panel, vcov = "iid"),
  feols(total_male_ratio ~ uni_ratio + junior_col_ratio + aging_rate + active_job_ratio |
          prefecture + entry_year, data = df_panel, cluster = ~ prefecture)
)

df_all_wrong <- df_all |> mutate(entry_year_wrong = grad_year)

df_panel_wrong <- df_all_wrong |>
  filter(entry_year_wrong >= ENTRY_MIN & entry_year_wrong <= ENTRY_MAX) |>
  select(prefecture, entry_year = entry_year_wrong, school_type, enrolment, enrolment_male) |>
  pivot_wider(names_from = school_type, values_from = c(enrolment, enrolment_male)) |>
  drop_na(enrolment_大学, enrolment_専門学校, enrolment_短期大学,
          enrolment_male_大学, enrolment_male_専門学校, enrolment_male_短期大学) |>
  mutate(
    total            = enrolment_大学 + enrolment_専門学校 + enrolment_短期大学,
    male             = enrolment_male_大学 + enrolment_male_専門学校 + enrolment_male_短期大学,
    total_male_ratio = male / total,
    uni_ratio        = enrolment_大学 / total,
    junior_col_ratio = enrolment_短期大学 / total
  ) |>
  filter(total > 0) |>
  left_join(df_control, by = c("entry_year", "prefecture"))

model_wrong <- feols(
  total_male_ratio ~ uni_ratio + junior_col_ratio + aging_rate + active_job_ratio |
    prefecture + entry_year,
  data    = df_panel_wrong,
  cluster = ~ prefecture
)
print(summary(model_wrong))

model_simple_wrong <- feols(total_male_ratio ~ uni_ratio, data = df_panel_wrong, vcov = "hetero")
print(summary(model_simple_wrong))

cat(sprintf("\n年度未調整: beta(uni_ratio) = %.4f (p = %.4f)\n",
            coef(model_wrong)["uni_ratio"], pvalue(model_wrong)["uni_ratio"]))
cat(sprintf("年度調整済: beta(uni_ratio) = %.4f (p = %.4f)\n",
            coef(modelsA$model5)["uni_ratio"], pvalue(modelsA$model5)["uni_ratio"]))

nat_type <- nat

nat_idx <- nat |>
  mutate(
    male_sum   = enrolment_male_大学 + enrolment_male_専門学校 + enrolment_male_短期大学,
    female_sum = total - male_sum
  ) |>
  mutate(
    総入学者数 = total       / total[entry_year == ENTRY_MIN]       * 100,
    男子       = male_sum    / male_sum[entry_year == ENTRY_MIN]    * 100,
    女子       = female_sum  / female_sum[entry_year == ENTRY_MIN]  * 100
  ) |>
  select(entry_year, 総入学者数, 男子, 女子) |>
  pivot_longer(-entry_year, names_to = "series", values_to = "index") |>
  mutate(series = factor(series, levels = c("総入学者数", "男子", "女子")))

SERIES_COLORS <- c("総入学者数" = "darkgreen", "男子" = "#2980b9", "女子" = "#e74c3c")
SERIES_LTY    <- c("総入学者数" = "dotted",   "男子" = "solid",  "女子" = "dashed")
SERIES_SHAPE  <- c("総入学者数" = 16,        "男子" = 17,        "女子" = 15)

fig1 <- ggplot(nat_idx, aes(entry_year, index,
                            color = series, linetype = series, shape = series)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray70") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  scale_color_manual(values = SERIES_COLORS) +
  scale_linetype_manual(values = SERIES_LTY) +
  scale_shape_manual(values = SERIES_SHAPE) +
  scale_x_continuous(breaks = ENTRY_MIN:ENTRY_MAX) +
  labs(title = "男女別・総数別入学者数の推移（2012年度=100）",
       x = "入学年度", y = "指数（2012年度=100）", color = NULL, linetype = NULL, shape = NULL) +
  theme_bw(base_family = FONT) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("図1_男女別推移.png", fig1, width = 7.5, height = 4.6, dpi = 300)

TYPE_COLORS <- c("大学" = "#2980b9", "専門学校" = "#e74c3c", "短期大学" = "#f39c12")
TYPE_LTY    <- c("大学" = "solid",   "専門学校" = "dashed",  "短期大学" = "dotted")
TYPE_SHAPE  <- c("大学" = 16,        "専門学校" = 17,        "短期大学" = 15)

share_long <- nat_type |>
  transmute(entry_year,
            大学 = enrolment_大学 / total,
            専門学校 = enrolment_専門学校 / total,
            短期大学 = enrolment_短期大学 / total) |>
  pivot_longer(-entry_year, names_to = "school_type", values_to = "share") |>
  mutate(school_type = factor(school_type, levels = c("大学", "専門学校", "短期大学")))

maleratio_long <- nat_type |>
  transmute(entry_year,
            大学 = enrolment_male_大学 / enrolment_大学,
            専門学校 = enrolment_male_専門学校 / enrolment_専門学校,
            短期大学 = enrolment_male_短期大学 / enrolment_短期大学) |>
  pivot_longer(-entry_year, names_to = "school_type", values_to = "male_ratio") |>
  mutate(school_type = factor(school_type, levels = c("大学", "専門学校", "短期大学")))

p2a <- ggplot(share_long, aes(entry_year, share,
                              color = school_type, linetype = school_type, shape = school_type)) +
  geom_line(linewidth = 1) + geom_point(size = 2.3) +
  scale_color_manual(values = TYPE_COLORS) +
  scale_linetype_manual(values = TYPE_LTY) +
  scale_shape_manual(values = TYPE_SHAPE) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = ENTRY_MIN:ENTRY_MAX) +
  labs(title = "(a) 課程別入学者シェアの推移", x = "入学年度", y = "シェア",
       color = "課程", linetype = "課程", shape = "課程") +
  theme_bw(base_family = FONT) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

p2b <- ggplot(maleratio_long, aes(entry_year, male_ratio,
                                  color = school_type, linetype = school_type, shape = school_type)) +
  geom_line(linewidth = 1) + geom_point(size = 2.3) +
  scale_color_manual(values = TYPE_COLORS) +
  scale_linetype_manual(values = TYPE_LTY) +
  scale_shape_manual(values = TYPE_SHAPE) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = ENTRY_MIN:ENTRY_MAX) +
  labs(title = "(b) 課程別男子入学者比率の推移", x = "入学年度", y = "男子比率",
       color = "課程", linetype = "課程", shape = "課程") +
  theme_bw(base_family = FONT) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

fig2 <- p2a + p2b
ggsave("図2_課程別推移.png", fig2, width = 10, height = 4.3, dpi = 300)

lab0 <- sprintf("%d年度\n男子比率", ENTRY_MIN)
lab1 <- sprintf("%d年度\n男子比率", ENTRY_MAX)
labs_order <- c(lab0, "構成効果\n（大学化）", "課程内効果\n（各課程内の低下）", lab1)

waterfall <- tibble(
  label = factor(labs_order, levels = labs_order),
  x     = 1:4,
  ymin  = c(0, r0 + pmin(comp_total, 0), r0 + comp_total + pmin(within_total, 0), 0),
  ymax  = c(r0, r0 + pmax(comp_total, 0), r0 + comp_total + pmax(within_total, 0), r1),
  is_total = c(TRUE, FALSE, FALSE, TRUE)
)

fig3 <- ggplot(waterfall) +
  geom_rect(aes(xmin = x - 0.35, xmax = x + 0.35, ymin = ymin, ymax = ymax, fill = is_total),
            color = "black", linewidth = 0.3) +
  geom_text(aes(x = x, y = pmax(ymin, ymax) + 0.004,
                label = percent(ymax - ymin, accuracy = 0.01)), family = FONT) +
  scale_x_continuous(breaks = 1:4, labels = labs_order) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("TRUE" = "#34495e", "FALSE" = "#7fb3d5"), guide = "none") +
  labs(title = sprintf("男子入学者比率の変化（%d→%d年度）の要因分解", ENTRY_MIN, ENTRY_MAX),
       x = NULL, y = "男子入学者比率") +
  theme_bw(base_family = FONT)
ggsave("図3_要因分解.png", fig3, width = 7.5, height = 4.2, dpi = 300)

models_coef <- list(
  "(1) プーリングOLS"       = modelsA$model1,
  "(2) 都道府県FE"          = modelsA$model2,
  "(3) 二元FE"              = modelsA$model3,
  "(4) 二元FE+短大シェア"   = modelsA$model4,
  "(5) 二元FE+全統制変数"   = modelsA$model5
)

coef_df <- map_dfr(names(models_coef), function(nm) {
  t <- broom::tidy(models_coef[[nm]], conf.int = TRUE) |> filter(term == "uni_ratio")
  tibble(model = nm, estimate = t$estimate, conf.low = t$conf.low, conf.high = t$conf.high)
}) |>
  mutate(model = factor(model, levels = rev(names(models_coef))))

fig4 <- ggplot(coef_df, aes(x = estimate, y = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), color = "#2980b9", size = 0.7) +
  scale_x_continuous(labels = number_format(accuracy = 0.01)) +
  labs(title = "大学シェア（uni_ratio）の推定係数と95%信頼区間",
       x = "大学シェアの推定係数と95%信頼区間", y = NULL) +
  theme_bw(base_family = FONT)
ggsave("図4_係数プロット.png", fig4, width = 7.8, height = 3.4, dpi = 300)
