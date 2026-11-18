# =============================================================================
# Cognitive Triage on Social Media: Do Quality Cues Shape Speed, Skepticism,
# and Discernment in Veracity Judgments?
# =============================================================================

library(tidyverse)
library(lme4)
library(lmerTest)
library(patchwork)

all_data            <- read_csv("clean_data.csv", show_col_types = FALSE)
participant_summary <- read_csv("participant_summary.csv", show_col_types = FALSE)

# =============================================================================
# Exclusions
# =============================================================================

attention <- all_data %>%
  filter(topic == "attention_check") %>%
  group_by(participant_id) %>%
  summarise(num_passed = sum(correct), .groups = "drop")

exclusions <- participant_summary %>%
  left_join(attention, by = "participant_id") %>%
  mutate(
    num_passed        = coalesce(num_passed, 0L),
    failed_completion = !completed,
    failed_attention  = completed & num_passed == 0
  )

# Completion-time bounds are based on participants who completed the experiment
# and passed at least one attention check
valid     <- exclusions %>% filter(!failed_completion, !failed_attention)
ct_bounds <- mean(valid$completion_time_ms) + c(-3, 3) * sd(valid$completion_time_ms)

exclusions <- exclusions %>%
  mutate(
    failed_completion_time = !failed_completion & !failed_attention &
      (completion_time_ms < ct_bounds[1] | completion_time_ms > ct_bounds[2]),
    included = !failed_completion & !failed_attention & !failed_completion_time
  )

cat("Participants who did not complete the experiment:", sum(exclusions$failed_completion), "\n")
cat("Participants who failed both attention checks:", sum(exclusions$failed_attention), "\n")
cat("Participants outside the completion-time bounds:", sum(exclusions$failed_completion_time), "\n")
cat("Participants included:", sum(exclusions$included), "\n")

# Exclude participants who failed one or more criteria
all_data <- all_data %>%
  filter(participant_id %in% exclusions$participant_id[exclusions$included])

# =============================================================================
# Trial data
# =============================================================================

trials <- all_data %>%
  filter(topic != "attention_check") %>%
  arrange(participant_id, trial_index) %>%
  group_by(participant_id) %>%
  mutate(trial_order = row_number()) %>%
  ungroup() %>%
  mutate(
    quality_label = factor(if_else(quality == "LQ", "Low quality", "Normal quality"),
                           levels = c("Normal quality", "Low quality")),
    prior_for_topic = case_when(
      topic == "nuclear"      ~ prior_nuclear,
      topic == "immigration"  ~ prior_immigration,
      topic == "social media" ~ prior_social_media
    ),
    congruence = case_when(
      (stance == "pro" & prior_for_topic > 0) |
        (stance == "con" & prior_for_topic < 0) ~ "congruent",
      (stance == "pro" & prior_for_topic < 0) |
        (stance == "con" & prior_for_topic > 0) ~ "incongruent"
    )
  )

# -----------------------------------------------------------------------------
# Response-time trimming
# -----------------------------------------------------------------------------

rt_floor <- 300
rt_upper <- mean(trials$rt) + 3 * sd(trials$rt)

n_trials <- nrow(trials)
n_fast   <- sum(trials$rt < rt_floor)
n_slow   <- sum(trials$rt > rt_upper)

cat(sprintf("\nTrials below %d ms: %d (%.2f%%)\n",
            rt_floor, n_fast, 100 * n_fast / n_trials))
cat(sprintf("Trials above %.0f ms (grand mean + 3 SD): %d (%.2f%%)\n",
            rt_upper, n_slow, 100 * n_slow / n_trials))
cat(sprintf("Trials removed: %d of %d (%.2f%%)\n",
            n_fast + n_slow, n_trials, 100 * (n_fast + n_slow) / n_trials))

trials <- trials %>% filter(rt >= rt_floor, rt <= rt_upper)

cat("Trials retained:", nrow(trials), "\n")

# =============================================================================
# SDT parameters
# =============================================================================

# -----------------------------------------------------------------------------
# Correction for extreme proportions
# -----------------------------------------------------------------------------

correct_proportion <- function(hits, n) {
  p <- hits / n
  case_when(
    p == 0 ~ 1 / (2 * n),
    p == 1 ~ 1 - 1 / (2 * n),
    TRUE   ~ p
  )
}

# -----------------------------------------------------------------------------
# d' and c for a set of trials
# -----------------------------------------------------------------------------

sdt_params <- function(df) {
  signal_trials <- df %>% filter(true_value)
  noise_trials  <- df %>% filter(!true_value)

  hit_rate <- correct_proportion(sum(signal_trials$judgment), nrow(signal_trials))
  fa_rate  <- correct_proportion(sum(noise_trials$judgment),  nrow(noise_trials))

  z_hit <- qnorm(hit_rate)
  z_fa  <- qnorm(fa_rate)

  tibble(
    d_prime = z_hit - z_fa,
    c       = -0.5 * (z_hit + z_fa)
  )
}

# -----------------------------------------------------------------------------
# Function to calculate SDT parameters for a single participant
# -----------------------------------------------------------------------------

calculate_sdt_participant <- function(participant_data) {

  overall     <- sdt_params(participant_data)
  congruent   <- sdt_params(participant_data %>% filter(congruence == "congruent"))
  incongruent <- sdt_params(participant_data %>% filter(congruence == "incongruent"))

  tibble(
    d_prime       = overall$d_prime,
    c_overall     = overall$c,
    c_congruent   = congruent$c,
    c_incongruent = incongruent$c,
    delta_c       = congruent$c - incongruent$c
  )
}

# -----------------------------------------------------------------------------
# SDT parameters per participant: overall, by topic, and by quality
# -----------------------------------------------------------------------------

sdt_data <- trials %>%
  group_by(participant_id) %>%
  group_modify(~ calculate_sdt_participant(.x)) %>%
  ungroup()

sdt_by_topic <- trials %>%
  group_by(participant_id, topic) %>%
  group_modify(~ calculate_sdt_participant(.x)) %>%
  ungroup()

sdt_by_quality <- trials %>%
  group_by(participant_id, quality_label) %>%
  group_modify(~ sdt_params(.x)) %>%
  ungroup()

# =============================================================================
# Descriptive statistics
# =============================================================================

# -----------------------------------------------------------------------------
# Table 1: d' and c by topic
# -----------------------------------------------------------------------------

summarise_sdt <- function(data) {
  data %>%
    summarise(
      d_prime_M  = mean(d_prime),
      d_prime_SD = sd(d_prime),
      c_M        = mean(c_overall),
      c_SD       = sd(c_overall),
      .groups = "drop"
    )
}

table1 <- bind_rows(
  sdt_by_topic %>% group_by(topic) %>% summarise_sdt(),
  sdt_data %>% summarise_sdt() %>% mutate(topic = "All topics")
)

cat("\n=== Table 1: d' and c by topic ===\n")
print(table1)

# -----------------------------------------------------------------------------
# Prior-belief reliance
# -----------------------------------------------------------------------------

cat("\n=== Prior-belief reliance ===\n")
for (v in c("c_congruent", "c_incongruent", "delta_c")) {
  cat(sprintf("%-14s M = %6.2f, SD = %5.2f\n",
              v, mean(sdt_data[[v]]), sd(sdt_data[[v]])))
}

sdt_by_topic %>%
  group_by(topic) %>%
  summarise(delta_c_M = mean(delta_c), delta_c_SD = sd(delta_c), .groups = "drop") %>%
  print()

# -----------------------------------------------------------------------------
# Response time
# -----------------------------------------------------------------------------

summarise_rt <- function(data) {
  data %>%
    summarise(
      n   = n(),
      M   = mean(rt),
      SD  = sd(rt),
      Mdn = median(rt),
      .groups = "drop"
    )
}

rt_descriptives <- bind_rows(
  trials %>% mutate(group = "All posts") %>% group_by(group) %>% summarise_rt(),
  trials %>% group_by(group = as.character(quality_label)) %>% summarise_rt(),
  trials %>% group_by(group = topic) %>% summarise_rt()
)

cat("\n=== Response time (ms) ===\n")
rt_descriptives %>%
  mutate(across(where(is.double), round)) %>%
  as.data.frame() %>%
  print()

# =============================================================================
# Figures
# =============================================================================

fill_colours <- c("Normal quality" = "#5B9BD5", "Low quality" = "#ED7D31")

theme_manuscript <- theme_classic(base_size = 11) +
  theme(
    axis.line          = element_line(colour = "grey40", linewidth = 0.3),
    axis.ticks         = element_line(colour = "grey40", linewidth = 0.3),
    axis.text          = element_text(colour = "grey20"),
    panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
    legend.position    = "none"
  )

# -----------------------------------------------------------------------------
# Figure 1: d' and c by quality
# -----------------------------------------------------------------------------

# Means with within-subject 95% CIs (Cousineau, 2005; Morey, 2008)
within_ci <- function(data, yvar) {
  n_levels <- n_distinct(data$quality_label)
  data %>%
    group_by(participant_id) %>%
    mutate(y_norm = .data[[yvar]] - mean(.data[[yvar]])) %>%
    ungroup() %>%
    mutate(y_norm = y_norm + mean(.data[[yvar]])) %>%
    group_by(quality_label) %>%
    summarise(
      estimate = mean(.data[[yvar]]),
      ci       = qt(0.975, n() - 1) * sd(y_norm) / sqrt(n()) *
                 sqrt(n_levels / (n_levels - 1)),
      .groups  = "drop"
    )
}

dprime_fig_summary <- within_ci(sdt_by_quality, "d_prime")
c_fig_summary      <- within_ci(sdt_by_quality, "c")

# Both panels span the same range on the y-axis
ci_range <- function(summary, include = NULL) {
  range(c(summary$estimate - summary$ci, summary$estimate + summary$ci, include))
}
sdt_fig_span <- 1.35 * max(diff(ci_range(dprime_fig_summary)),
                           diff(ci_range(c_fig_summary, include = 0)))
centred_ylim <- function(r) mean(r) + c(-0.5, 0.5) * sdt_fig_span

sdt_dot_panel <- function(summary, ylab, ylim, zero_line = FALSE) {
  ggplot(summary, aes(x = quality_label, y = estimate, colour = quality_label)) +
    { if (zero_line) geom_hline(yintercept = 0, linetype = "dashed",
                                colour = "grey60", linewidth = 0.4) } +
    geom_line(aes(group = 1), colour = "grey70", linewidth = 0.5) +
    geom_errorbar(aes(ymin = estimate - ci, ymax = estimate + ci),
                  width = 0.07, linewidth = 0.6) +
    geom_point(size = 3.2) +
    scale_colour_manual(values = fill_colours) +
    scale_x_discrete(expand = expansion(add = 0.5)) +
    coord_cartesian(ylim = ylim) +
    labs(x = NULL, y = ylab) +
    theme_manuscript
}

p_fig_dprime <- sdt_dot_panel(
  dprime_fig_summary,
  expression("Discrimination (" * italic(d) * minute * ")"),
  ylim = centred_ylim(ci_range(dprime_fig_summary))
)

p_fig_c <- sdt_dot_panel(
  c_fig_summary,
  expression("Response criterion (" * italic(c) * ")"),
  ylim      = centred_ylim(ci_range(c_fig_summary, include = 0)),
  zero_line = TRUE
)

fig1 <- (p_fig_dprime + p_fig_c) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag          = element_text(face = "bold", margin = margin(t = 2, l = 8)),
        plot.tag.location = "panel",
        plot.tag.position = "topleft")

ggsave("figure1.png", fig1, width = 6.5, height = 3.4, dpi = 600)
print(fig1)

# -----------------------------------------------------------------------------
# Figure 2: Response-time distribution by quality
# -----------------------------------------------------------------------------

rt_fig_data <- trials %>% mutate(rt_s = rt / 1000)

rt_medians <- rt_fig_data %>%
  group_by(quality_label) %>%
  summarise(median_s = median(rt_s), .groups = "drop")

rt_legend_labels <- setNames(
  as.expression(lapply(seq_len(nrow(rt_medians)), \(i) bquote(
    .(as.character(rt_medians$quality_label[i])) ~
      "(" * italic("Mdn") == .(sprintf("%.2f", rt_medians$median_s[i])) ~ "s)"
  ))),
  rt_medians$quality_label
)

fig2 <- ggplot(rt_fig_data,
               aes(x = rt_s, colour = quality_label, fill = quality_label)) +
  geom_density(alpha = 0.15, linewidth = 0.6, bounds = c(rt_floor / 1000, Inf)) +
  geom_vline(data = rt_medians,
             aes(xintercept = median_s, colour = quality_label),
             linetype = "dashed", linewidth = 0.4, show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0, 40, 5), limits = c(0, 40),
                     expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_colour_manual(values = fill_colours, labels = \(x) rt_legend_labels[x]) +
  scale_fill_manual(values = fill_colours, labels = \(x) rt_legend_labels[x]) +
  labs(x = "Response time (s)", y = "Density", colour = NULL, fill = NULL) +
  theme_manuscript +
  theme(legend.position        = "inside",
        legend.position.inside = c(0.98, 0.98),
        legend.justification   = c(1, 1),
        legend.background      = element_blank())

ggsave("figure2.png", fig2, width = 6.5, height = 3.4, dpi = 600)
print(fig2)

# =============================================================================
# Hypothesis tests
# =============================================================================

model_data <- trials %>%
  mutate(
    Response   = as.integer(judgment),
    Veracity   = if_else(true_value, 0.5, -0.5),
    Quality    = if_else(quality == "LQ", 0.5, -0.5),
    Congruence = if_else(congruence == "congruent", 0.5, -0.5),
    TrialOrder = (trial_order - mean(trial_order)) / diff(range(trial_order)),
    log_rt     = log(rt)
  )

# -----------------------------------------------------------------------------
# Table 2: Response time (LMM)
# -----------------------------------------------------------------------------

h1_model <- lmer(
  log_rt ~ Quality +                     # H1: faster responses to low quality
    (1 + Quality | participant_id) +
    (1 + Quality | post_id),
  data = model_data
)

cat("\n=== Table 2: Response time (LMM) ===\n")
print(summary(h1_model))

# -----------------------------------------------------------------------------
# Table 3: Veracity judgments (probit GLMM)
# -----------------------------------------------------------------------------

h23_model <- glmer(
  Response ~ Veracity +                  # d'
    Quality +                            # H2: more conservative c for low quality
    Veracity:Quality +                   # H3: higher d' for normal quality
    (1 + Veracity + Quality + Veracity:Quality | participant_id) +
    (1 + Quality | post_id),
  data = model_data,
  family = binomial(link = "probit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000))
)

cat("\n=== Table 3: Veracity judgments (probit GLMM) ===\n")
print(summary(h23_model))
print(confint(h23_model, parm = "beta_", method = "Wald"))

# =============================================================================
# Exploratory analyses
# =============================================================================

# -----------------------------------------------------------------------------
# Belief congruence (Table 4)
# -----------------------------------------------------------------------------

h1_cong_model <- lmer(
  log_rt ~ Quality * Congruence +
    (1 + Quality | participant_id) +
    (1 + Quality | post_id),
  data = model_data
)

cat("\n=== Response time with congruence (LMM) ===\n")
print(summary(h1_cong_model))

h23_cong_model <- glmer(
  Response ~ Veracity * Quality * Congruence +
    (1 + Veracity + Quality + Veracity:Quality | participant_id) +
    (1 + Quality | post_id),
  data = model_data,
  family = binomial(link = "probit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000))
)

cat("\n=== Table 4: Veracity judgments with congruence (probit GLMM) ===\n")
print(summary(h23_cong_model))

# -----------------------------------------------------------------------------
# Trial order
# -----------------------------------------------------------------------------

h1_order_model <- lmer(
  log_rt ~ Quality * TrialOrder +
    (1 + Quality | participant_id) +
    (1 + Quality | post_id),
  data = model_data
)

cat("\n=== Response time with trial order (LMM) ===\n")
print(summary(h1_order_model))

h23_order_model <- glmer(
  Response ~ Veracity * Quality * TrialOrder +
    (1 + Veracity + Quality + Veracity:Quality | participant_id) +
    (1 + Quality | post_id),
  data = model_data,
  family = binomial(link = "probit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000))
)

cat("\n=== Veracity judgments with trial order (probit GLMM) ===\n")
print(summary(h23_order_model))
print(confint(h23_order_model, parm = "beta_", method = "Wald"))
