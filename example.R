# ======================================================================
# Illustration: qualbias workflow
# Story: Coding workplace-harassment allegations in employee exit narratives
#
# 800 narratives are coded as:
#   no_allegation / ambiguous_concern / credible_allegation
#
# Four LLM instruments code all 800 narratives. An expert and two students
# code overlapping bridge subsamples. We estimate coding-process components,
# generate a multiverse of LLM-panel measurements, and assess whether the
# estimated association between credible allegations and months-to-exit is
# robust to coding-process variation.
#
# Before running:
#   1. Save the supplied prototype as "qualbias.R"
#   2. Install: glmnet, Matrix, softImpute, ggplot2
# ======================================================================

source("qualbias.R")

set.seed(20261030)

library(ggplot2)

# ----------------------------------------------------------------------
# 1. Create an illustrative item-level dataset
# ----------------------------------------------------------------------

n_items <- 800L

items <- data.frame(
  item_id = sprintf("case_%03d", seq_len(n_items)),
  # Placeholder only: no actual narrative text is required for this example.
  text = paste("Anonymized employee-exit narrative", seq_len(n_items)),
  department = sample(
    c("Sales", "Operations", "Engineering"),
    n_items,
    replace = TRUE,
    prob = c(0.35, 0.40, 0.25)
  ),
  manager_level = sample(
    c("frontline", "middle", "executive"),
    n_items,
    replace = TRUE,
    prob = c(0.65, 0.28, 0.07)
  ),
  complaint_channel = sample(
    c("informal", "formal"),
    n_items,
    replace = TRUE,
    prob = c(0.62, 0.38)
  ),
  evidence_strength = sample(
    c("low", "medium", "high"),
    n_items,
    replace = TRUE,
    prob = c(0.35, 0.40, 0.25)
  ),
  baseline_tenure = round(
    pmax(rnorm(n_items, mean = 4.5, sd = 2.2), 0.2),
    1
  ),
  firm_id = sample(
    paste0("firm_", 1:12),
    n_items,
    replace = TRUE
  ),
  stringsAsFactors = FALSE
)

items$department <- factor(items$department)
items$manager_level <- factor(items$manager_level)
items$complaint_channel <- factor(items$complaint_channel)
items$evidence_strength <- factor(
  items$evidence_strength,
  levels = c("low", "medium", "high")
)

# Latent item-level construct used only to generate realistic dummy data.
# It is NOT supplied to the qualbias model.
true_credible_probability <- plogis(
  -2.0 +
    0.9 * (items$evidence_strength == "medium") +
    2.4 * (items$evidence_strength == "high") +
    0.45 * (items$complaint_channel == "formal") +
    0.30 * (items$manager_level == "executive") +
    0.20 * (items$department == "Sales")
)

items$latent_credible <- rbinom(
  n_items,
  size = 1,
  prob = true_credible_probability
)

# Downstream outcome:
# months between the narrative's focal complaint/event and employee exit.
# Credible allegations are associated with faster exits in the data-generating
# process, but downstream analysts do not observe latent_credible.
firm_effect <- rnorm(length(unique(items$firm_id)), sd = 1.0)
names(firm_effect) <- unique(items$firm_id)

items$months_to_exit <- round(
  pmax(
    1,
    20 -
      4.0 * items$latent_credible -
      0.55 * items$baseline_tenure +
      1.2 * (items$manager_level == "executive") +
      firm_effect[items$firm_id] +
      rnorm(n_items, sd = 3.5)
  ),
  1
)

# Variables used in the downstream analysis.
analysis_items <- items[
  ,
  c(
    "item_id",
    "department",
    "baseline_tenure",
    "firm_id",
    "months_to_exit"
  )
]


# ----------------------------------------------------------------------
# 2. Define seven coding instruments
# ----------------------------------------------------------------------

coders <- data.frame(
  coder_id = c(
    "expert_1",
    "student_1",
    "student_2",
    "llm_base",
    "llm_context",
    "llm_history",
    "llm_cautious"
  ),
  coder_type = c(
    "expert",
    "student",
    "student",
    "llm",
    "llm",
    "llm",
    "llm"
  ),
  description = c(
    "Experienced workplace-investigation researcher",
    "Student coder 1",
    "Student coder 2",
    "LLM with baseline codebook prompt",
    "LLM with randomly assigned policy examples/context",
    "LLM with randomized access to its prior label",
    "LLM instructed to require explicit evidence"
  ),
  stringsAsFactors = FALSE
)

# Four LLMs code every item.
llm_ids <- c(
  "llm_base",
  "llm_context",
  "llm_history",
  "llm_cautious"
)

# The expert codes a 300-item stratified bridge sample.
high_evidence <- items$item_id[items$evidence_strength == "high"]
other_items <- setdiff(items$item_id, high_evidence)

expert_items <- c(
  sample(high_evidence, 120),
  sample(other_items, 180)
)

# Student 1: 180 items overlap with the expert, plus 40 additional items.
student_1_items <- c(
  sample(expert_items, 180),
  sample(setdiff(items$item_id, expert_items), 40)
)

# Student 2: shares 120 cases with student 1, overlaps further with the
# expert, and includes additional unique cases.
student_2_items <- c(
  sample(intersect(student_1_items, expert_items), 120),
  sample(setdiff(expert_items, student_1_items), 60),
  sample(
    setdiff(items$item_id, union(expert_items, student_1_items)),
    40
  )
)

# ----------------------------------------------------------------------
# 3. Construct coding assignments and experimental conditions
# ----------------------------------------------------------------------

make_assignments <- function(item_ids, coder_id, run_id) {
  output <- data.frame(
    item_id = item_ids,
    coder_id = coder_id,
    run_id = run_id,
    assignment_order = sample(seq_along(item_ids)),
    stringsAsFactors = FALSE
  )
  
  output <- output[order(output$assignment_order), ]
  rownames(output) <- NULL
  output
}

assignment_list <- list(
  make_assignments(expert_items, "expert_1", "human_session_expert"),
  make_assignments(student_1_items, "student_1", "human_session_student_1"),
  make_assignments(student_2_items, "student_2", "human_session_student_2"),
  make_assignments(items$item_id, "llm_base", "llm_baseline"),
  make_assignments(items$item_id, "llm_context", "llm_context_experiment"),
  make_assignments(items$item_id, "llm_history", "llm_history_experiment"),
  make_assignments(items$item_id, "llm_cautious", "llm_cautious_prompt")
)

assignments <- do.call(rbind, assignment_list)
rownames(assignments) <- NULL

# Experimental/recorded coding-process variables.
assignments$student_indicator <- as.integer(
  assignments$coder_id %in% c("student_1", "student_2")
)

assignments$human_indicator <- as.integer(
  assignments$coder_id %in% c("expert_1", "student_1", "student_2")
)

assignments$llm_indicator <- as.integer(
  assignments$coder_id %in% llm_ids
)

assignments$cautious_prompt <- as.integer(
  assignments$coder_id == "llm_cautious"
)

# Randomized within llm_context:
# Some items are coded with policy examples / expanded context.
assignments$policy_examples <- 0L

context_rows <- assignments$coder_id == "llm_context"

assignments$policy_examples[context_rows] <- rbinom(
  sum(context_rows),
  size = 1,
  prob = 0.50
)

# Randomized within llm_history:
# The model is sometimes shown its immediately previous label.
assignments$history_access <- 0L

history_rows <- assignments$coder_id == "llm_history"

assignments$history_access[history_rows] <- rbinom(
  sum(history_rows),
  size = 1,
  prob = 0.50
)

# Human fatigue is represented by late-session status.
# Students are somewhat more vulnerable than the expert.
assignments$sequence_progress <- ave(
  assignments$assignment_order,
  interaction(assignments$coder_id, assignments$run_id),
  FUN = function(x) {
    if (length(unique(x)) == 1L) {
      rep(0, length(x))
    } else {
      (x - min(x)) / (max(x) - min(x))
    }
  }
)

assignments$human_late <- as.integer(
  assignments$human_indicator == 1 &
    assignments$sequence_progress > 0.65
)

# The prior label is populated while labels are generated below.
assignments$prior_label <- "none"


# ----------------------------------------------------------------------
# 4. Generate labels with known systematic coding-process structure
# ----------------------------------------------------------------------

labels <- c(
  "no_allegation",
  "ambiguous_concern",
  "credible_allegation"
)

draw_label <- function(utility, labels) {
  probability <- exp(utility - max(utility))
  probability <- probability / sum(probability)
  
  sample(labels, size = 1L, prob = probability)
}

# Join item characteristics to assignments for data generation.
generation_data <- merge(
  assignments,
  items[
    ,
    c(
      "item_id",
      "department",
      "complaint_channel",
      "evidence_strength",
      "latent_credible"
    )
  ],
  by = "item_id",
  all.x = TRUE,
  sort = FALSE
)

generation_data <- generation_data[
  order(generation_data$coder_id, generation_data$assignment_order),
]

# Generate labels coder by coder so llm_history can use a genuine prior label.
for (coder in unique(generation_data$coder_id)) {
  coder_rows <- which(generation_data$coder_id == coder)
  previous_label <- "none"
  
  for (row in coder_rows) {
    evidence <- as.character(generation_data$evidence_strength[row])
    department <- as.character(generation_data$department[row])
    coder_id <- generation_data$coder_id[row]
    
    # Baseline utilities for the three codebook categories.
    utility <- c(
      no_allegation = 1.15,
      ambiguous_concern = 0.10,
      credible_allegation = -1.00
    )
    
    # Item evidence affects all coding instruments.
    if (evidence == "medium") {
      utility["no_allegation"] <- utility["no_allegation"] - 0.40
      utility["ambiguous_concern"] <- utility["ambiguous_concern"] + 0.45
      utility["credible_allegation"] <- utility["credible_allegation"] + 0.75
    }
    
    if (evidence == "high") {
      utility["no_allegation"] <- utility["no_allegation"] - 1.15
      utility["ambiguous_concern"] <- utility["ambiguous_concern"] + 0.25
      utility["credible_allegation"] <- utility["credible_allegation"] + 2.15
    }
    
    # Latent construct adds realistic but imperfect item-level signal.
    if (generation_data$latent_credible[row] == 1) {
      utility["no_allegation"] <- utility["no_allegation"] - 0.70
      utility["ambiguous_concern"] <- utility["ambiguous_concern"] + 0.25
      utility["credible_allegation"] <- utility["credible_allegation"] + 1.25
    }
    
    # Departmental language differences:
    # Sales narratives use more informal language and produce threshold
    # variation across coding instruments.
    if (department == "Sales") {
      utility["ambiguous_concern"] <- utility["ambiguous_concern"] + 0.20
    }
    
    # Expert: comparatively strong evidence sensitivity.
    if (coder_id == "expert_1") {
      if (evidence == "high") {
        utility["credible_allegation"] <- utility["credible_allegation"] + 0.80
      }
      if (evidence == "low") {
        utility["no_allegation"] <- utility["no_allegation"] + 0.25
      }
    }
    
    # Students initially classify many uncertain cases as no allegation,
    # but improve with assignment sequence / practice.
    if (coder_id %in% c("student_1", "student_2")) {
      progress <- generation_data$sequence_progress[row]
      
      utility["no_allegation"] <- utility["no_allegation"] + 0.65 - 0.70 * progress
      utility["ambiguous_concern"] <- utility["ambiguous_concern"] - 0.15 + 0.30 * progress
      utility["credible_allegation"] <- utility["credible_allegation"] - 0.30 + 0.65 * progress
      
      # Late-session fatigue moves difficult cases toward no allegation.
      if (generation_data$human_late[row] == 1 && evidence != "high") {
        utility["no_allegation"] <- utility["no_allegation"] + 0.35
        utility["credible_allegation"] <- utility["credible_allegation"] - 0.25
      }
    }
    
    # Baseline LLM: moderately sensitive to evidence, but tends toward
    # ambiguous classifications in Sales narratives.
    if (coder_id == "llm_base") {
      if (department == "Sales") {
        utility["ambiguous_concern"] <- utility["ambiguous_concern"] + 0.45
      }
    }
    
    # Context LLM: randomized policy examples improve detection of
    # evidence-consistent credible allegations.
    if (coder_id == "llm_context") {
      if (generation_data$policy_examples[row] == 1) {
        utility["no_allegation"] <- utility["no_allegation"] - 0.25
        utility["credible_allegation"] <- utility["credible_allegation"] + 0.70
      }
    }
    
    # Cautious LLM: requires explicit evidence and therefore applies a
    # systematically higher credible-allegation threshold.
    if (coder_id == "llm_cautious") {
      utility["no_allegation"] <- utility["no_allegation"] + 0.40
      utility["credible_allegation"] <- utility["credible_allegation"] - 0.90
      
      if (evidence == "high") {
        utility["credible_allegation"] <- utility["credible_allegation"] + 0.45
      }
    }
    
    # History LLM: randomly receives its actual immediately preceding label.
    # When access is granted, it exhibits serial self-anchoring.
    if (coder_id == "llm_history") {
      if (
        generation_data$history_access[row] == 1 &&
        previous_label != "none"
      ) {
        generation_data$prior_label[row] <- previous_label
        utility[previous_label] <- utility[previous_label] + 0.90
      } else {
        generation_data$prior_label[row] <- "none"
      }
    }
    
    assigned_label <- draw_label(utility, labels)
    
    generation_data$label[row] <- assigned_label
    
    if (coder_id == "llm_history") {
      previous_label <- assigned_label
    }
  }
}

assignments <- generation_data[
  ,
  c(
    "item_id",
    "coder_id",
    "run_id",
    "assignment_order",
    "student_indicator",
    "human_indicator",
    "llm_indicator",
    "cautious_prompt",
    "policy_examples",
    "history_access",
    "sequence_progress",
    "human_late",
    "prior_label",
    "label"
  )
]

assignments$prior_label <- factor(
  assignments$prior_label,
  levels = c(
    "none",
    "no_allegation",
    "ambiguous_concern",
    "credible_allegation"
  )
)

runs <- data.frame(
  run_id = unique(assignments$run_id),
  stringsAsFactors = FALSE
)

# ----------------------------------------------------------------------
# 5. Create the qualbias data object and inspect the coding design
# ----------------------------------------------------------------------

qb <- qb_data(
  assignments = assignments,
  items = items,
  coders = coders,
  runs = runs,
  codebook_id = "workplace_harassment_codebook_v1"
)

reliability <- qb_reliability(qb)
design <- qb_design(qb)

cat("\n--- Coding design ---\n")
print(design)

cat("\n--- Overall agreement diagnostics ---\n")
print(reliability[c("agreement", "kappa", "gwet_ac1")])

cat("\n--- Coder-category marginal distributions ---\n")
print(reliability$coder_marginals)


# ----------------------------------------------------------------------
# 6. Specify named coding-process components
# ----------------------------------------------------------------------
#
# Interpretation:
# - Because evidence strength is recorded rather than randomized, its
#   component is associational and used for sensitivity analysis.
# - Policy examples and history access are randomized LLM conditions;
#   their estimated components have causal interpretations within this
#   simulated experimental design.
# - Student learning and late-session human fatigue are recorded process
#   variables and therefore associational in this illustration.
#

components <- list(
  
  qb_component(
    name = "evidence_sensitivity",
    formula = ~ evidence_strength,
    kind = "difference",
    level = "item",
    description =
      "Coder-specific response to low, medium, and high evidentiary strength."
  ),
  
  qb_component(
    name = "department_thresholds",
    formula = ~ department,
    kind = "difference",
    level = "item",
    description =
      "Coder-specific category thresholds for departmental language and context."
  ),
  
  qb_component(
    name = "policy_examples",
    formula = ~ policy_examples,
    kind = "difference",
    level = "assignment",
    description =
      "Randomized availability of policy definitions and worked examples for llm_context."
  ),
  
  qb_component(
    name = "prior_label_anchor",
    formula = ~ history_access * prior_label,
    kind = "difference",
    level = "assignment",
    description =
      "Randomized access to the history LLM's immediately preceding label."
  ),
  
  qb_component(
    name = "student_learning",
    formula = ~ student_indicator * sequence_progress,
    kind = "difference",
    level = "assignment",
    description =
      "Changes in student decisions over the coding sequence."
  ),
  
  qb_component(
    name = "human_fatigue",
    formula = ~ human_late,
    kind = "difference",
    level = "assignment",
    description =
      "Late-session coding differences among human coders."
  ),
  
  qb_component(
    name = "cautious_prompt",
    formula = ~ cautious_prompt,
    kind = "difference",
    level = "assignment",
    description =
      "Higher evidentiary threshold induced by the cautious LLM instruction."
  )
)


# ----------------------------------------------------------------------
# 7. Fit the regularized multinomial coding model
# ----------------------------------------------------------------------

fit <- qb_fit(
  data = qb,
  components = components,
  folds = 5,
  alpha = 0.5,
  seed = 20261030,
  lambda_choice = "1se"
)

cat("\n--- Cross-fitted predictive diagnostics ---\n")
print(
  data.frame(
    crossfit_log_loss = fit$crossfit_log_loss,
    crossfit_accuracy = fit$crossfit_accuracy
  )
)

cat("\n--- Component correlations requiring caution ---\n")
print(fit$specification$component_correlations)

cat("\n--- Estimated coding components ---\n")
print(qb_component_summary(fit))

print(qb_plot_components(fit))


# ----------------------------------------------------------------------
# 8. Discover recurring unexplained disagreement patterns
# ----------------------------------------------------------------------
#
# This step uses cross-fitted residuals. It can reveal systematic patterns
# not captured by the named components, such as a recurring combination of
# item characteristics and coder-category behavior.
#

fit <- qb_residuals(
  fit,
  ranks = 1:3,
  folds = 4,
  stability_repetitions = 15,
  stability_fraction = 0.80,
  seed = 20261030
)

cat("\n--- Residual-axis stability ---\n")
print(fit$residual$stability)

print(qb_plot_residual(fit, axis = 1)$items)
print(qb_plot_residual(fit, axis = 1)$loadings)


# ----------------------------------------------------------------------
# 9. Define a target coding process and component-defined regimes
# ----------------------------------------------------------------------
#
# The target is explicitly an equal-weight panel of all four LLM processes.
# The expert and students are not treated as automatic ground truth; they
# serve as bridge instruments that make coding differences observable.
#

target <- qb_target_panel(
  coder_ids = llm_ids,
  weights = setNames(rep(1, length(llm_ids)), llm_ids)
)

regimes <- list(
  
  qb_regime(
    name = "observed_process",
    multipliers = c(),
    evidence = "confirmed",
    description =
      "Observed estimated coding process, including fitted components."
  ),
  
  qb_regime(
    name = "no_prior_label_anchoring",
    multipliers = c(prior_label_anchor = 0),
    evidence = "confirmed",
    description =
      "Counterfactual measurement in which fitted prior-label anchoring is removed."
  ),
  
  qb_regime(
    name = "strong_prior_label_anchoring",
    multipliers = c(prior_label_anchor = 1.75),
    evidence = "confirmed",
    description =
      "Counterfactual measurement with stronger prior-label anchoring."
  ),
  
  qb_regime(
    name = "no_policy_examples",
    multipliers = c(policy_examples = 0),
    evidence = "confirmed",
    description =
      "Counterfactual measurement without the fitted policy-example contribution."
  ),
  
  qb_regime(
    name = "strong_cautious_threshold",
    multipliers = c(cautious_prompt = 1.75),
    evidence = "confirmed",
    description =
      "Counterfactual measurement with a stronger cautious-prompt threshold."
  )
)

# Keep one observed assignment row for every item × LLM target coder.
# These rows supply assignment-level experimental/process variables needed
# to predict the target panel's labels.
target_assignment_data <- assignments[
  assignments$coder_id %in% llm_ids,
  setdiff(names(assignments), c("label", "run_id")),
  drop = FALSE
]

# qb_prediction_grid requires item_id + coder_id uniqueness.
target_assignment_data <- target_assignment_data[
  !duplicated(
    target_assignment_data[c("item_id", "coder_id")]
  ),
]

generated <- qb_generate(
  fit = fit,
  regimes = regimes,
  target = target,
  items = items$item_id,
  assignment_data = target_assignment_data,
  draws = 150,
  seed = 20261030
)


# ----------------------------------------------------------------------
# 10. Measurement-level multiverse diagnostics
# ----------------------------------------------------------------------

cat("\n--- Proportion of panel labels changed relative to observed process ---\n")
print(
  qb_compare_labels(
    generated,
    reference = "observed_process",
    level = "item"
  )
)

print(qb_plot_prevalence(generated))
print(qb_plot_transitions(generated, reference = "observed_process"))
print(qb_plot_stability(generated))


# ----------------------------------------------------------------------
# 11. Downstream analysis:
#     Do credible allegations predict shorter time to employee exit?
# ----------------------------------------------------------------------

estimate_retention_gap <- function(label_data, item_data) {
  analysis_data <- merge(
    label_data,
    item_data,
    by = "item_id",
    all.x = TRUE,
    sort = FALSE
  )
  
  analysis_data$credible_allegation <- as.integer(
    analysis_data$label == "credible_allegation"
  )
  
  model <- lm(
    months_to_exit ~
      credible_allegation +
      baseline_tenure +
      department +
      factor(firm_id),
    data = analysis_data
  )
  
  coefficient <- summary(model)$coefficients
  
  data.frame(
    regime = analysis_data$regime[1],
    draw = analysis_data$draw[1],
    retention_gap_months =
      coefficient["credible_allegation", "Estimate"],
    standard_error =
      coefficient["credible_allegation", "Std. Error"],
    stringsAsFactors = FALSE
  )
}

downstream_estimates <- do.call(
  rbind,
  lapply(
    split(
      generated$items,
      interaction(
        generated$items$regime,
        generated$items$draw,
        drop = TRUE
      )
    ),
    estimate_retention_gap,
    item_data = analysis_items
  )
)

downstream_summary <- aggregate(
  retention_gap_months ~ regime,
  downstream_estimates,
  function(x) {
    c(
      mean = mean(x),
      p05 = quantile(x, 0.05),
      p50 = median(x),
      p95 = quantile(x, 0.95)
    )
  }
)

downstream_summary <- do.call(
  data.frame,
  downstream_summary
)

names(downstream_summary) <- c(
  "regime",
  "mean_gap_months",
  "p05",
  "median_gap_months",
  "p95"
)

cat("\n--- Downstream coding-dependent retention-gap estimates ---\n")
print(downstream_summary)


# ----------------------------------------------------------------------
# 12. Analysis-specific visualization 1:
#     Coding-to-conclusion uncertainty distribution
# ----------------------------------------------------------------------

ggplot(
  downstream_estimates,
  aes(
    x = reorder(regime, retention_gap_months, FUN = median),
    y = retention_gap_months,
    fill = regime
  )
) +
  geom_violin(alpha = 0.75, colour = NA) +
  geom_boxplot(
    width = 0.14,
    outlier.shape = NA,
    colour = "grey20",
    fill = "white"
  ) +
  geom_hline(
    yintercept = -2,
    linetype = "dashed",
    colour = "firebrick"
  ) +
  coord_flip() +
  labs(
    title = "Coding-process-dependent conclusion",
    subtitle =
      "Estimated difference in months-to-exit for credible allegations; dashed line = substantively important 2-month reduction",
    x = NULL,
    y = "Estimated retention gap (months)",
    fill = "Coding regime"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")


# ----------------------------------------------------------------------
# 13. Analysis-specific visualization 2:
#     Coding tipping-point curve for prior-label anchoring
# ----------------------------------------------------------------------

anchor_strength <- seq(0, 2, by = 0.25)

anchor_regimes <- lapply(anchor_strength, function(value) {
  qb_regime(
    name = paste0("anchor_", format(value, nsmall = 2)),
    multipliers = c(prior_label_anchor = value),
    evidence = "confirmed",
    description = paste(
      "Prior-label anchoring multiplier:",
      value
    )
  )
})

anchor_generated <- qb_generate(
  fit = fit,
  regimes = anchor_regimes,
  target = target,
  items = items$item_id,
  assignment_data = target_assignment_data,
  draws = 80,
  seed = 20261031
)

anchor_estimates <- do.call(
  rbind,
  lapply(
    split(
      anchor_generated$items,
      interaction(
        anchor_generated$items$regime,
        anchor_generated$items$draw,
        drop = TRUE
      )
    ),
    estimate_retention_gap,
    item_data = analysis_items
  )
)

anchor_estimates$anchor_multiplier <- anchor_strength[
  match(
    anchor_estimates$regime,
    vapply(anchor_regimes, `[[`, character(1), "name")
  )
]

anchor_summary <- do.call(
  rbind,
  lapply(
    split(anchor_estimates, anchor_estimates$anchor_multiplier),
    function(x) {
      data.frame(
        anchor_multiplier = x$anchor_multiplier[1],
        mean_gap = mean(x$retention_gap_months),
        lower = quantile(x$retention_gap_months, 0.05),
        upper = quantile(x$retention_gap_months, 0.95)
      )
    }
  )
)

ggplot(
  anchor_summary,
  aes(x = anchor_multiplier, y = mean_gap)
) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    fill = "steelblue",
    alpha = 0.20
  ) +
  geom_line(linewidth = 1.1, colour = "steelblue4") +
  geom_point(size = 2.2, colour = "steelblue4") +
  geom_hline(
    yintercept = -2,
    linetype = "dashed",
    colour = "firebrick"
  ) +
  labs(
    title = "Coding tipping-point curve",
    subtitle =
      "How the downstream retention conclusion changes as prior-label anchoring is varied",
    x = "Multiplier on fitted prior-label anchoring component",
    y = "Estimated retention gap (months)"
  ) +
  theme_minimal(base_size = 12)


# ----------------------------------------------------------------------
# 14. Analysis-specific visualization 3:
#     Item-level coding uncertainty versus analytical consequence
# ----------------------------------------------------------------------
#
# The x-axis is the share of generated measurements that do not use the
# item's modal label. The y-axis is a simple regression influence proxy:
# leverage × absolute residual under one observed-process draw.
#

item_label_distribution <- do.call(
  rbind,
  lapply(
    split(generated$items, generated$items$item_id),
    function(x) {
      label_share <- prop.table(table(x$label))
      
      data.frame(
        item_id = x$item_id[1],
        coding_uncertainty = 1 - max(label_share),
        modal_label = names(label_share)[which.max(label_share)],
        stringsAsFactors = FALSE
      )
    }
  )
)

observed_draw_1 <- generated$items[
  generated$items$regime == "observed_process" &
    generated$items$draw == 1,
]

influence_data <- merge(
  observed_draw_1,
  analysis_items,
  by = "item_id",
  all.x = TRUE
)

influence_data$credible_allegation <- as.integer(
  influence_data$label == "credible_allegation"
)

observed_model <- lm(
  months_to_exit ~
    credible_allegation +
    baseline_tenure +
    department +
    factor(firm_id),
  data = influence_data
)

influence_data$influence_proxy <-
  hatvalues(observed_model) * abs(residuals(observed_model))

item_consequence <- merge(
  item_label_distribution,
  influence_data[
    ,
    c(
      "item_id",
      "influence_proxy",
      "months_to_exit",
      "credible_allegation"
    )
  ],
  by = "item_id"
)

ggplot(
  item_consequence,
  aes(
    x = coding_uncertainty,
    y = influence_proxy,
    colour = modal_label
  )
) +
  geom_point(alpha = 0.70, size = 2) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    colour = "grey25",
    linetype = "dashed"
  ) +
  labs(
    title = "Which coding uncertainties matter most downstream?",
    subtitle =
      "Items in the upper-right combine unstable coding with high analytical influence",
    x = "Coding uncertainty across regimes and draws",
    y = "Regression influence proxy",
    colour = "Modal generated label"
  ) +
  theme_minimal(base_size = 12)


# ----------------------------------------------------------------------
# 15. Export a reproducibility/provenance manifest
# ----------------------------------------------------------------------

qb_manifest(
  fit = fit,
  generated = generated,
  file = "qualbias_workplace_harassment_manifest.R"
)

cat("\nAnalysis complete.\n")