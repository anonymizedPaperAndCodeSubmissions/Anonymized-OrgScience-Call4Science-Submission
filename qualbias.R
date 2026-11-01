# ======================================================================
# qualbias
#
# Estimate systematic differences in how coding instruments apply one
# fixed deductive codebook, vary their fitted strengths, and generate
# alternative labels for explicit coding targets.
#
# Required:
#   glmnet, Matrix, softImpute
#
# Suggested:
#   ggplot2
# ======================================================================


# ----------------------------------------------------------------------
# Utilities
# ----------------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


qb_require <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(
      "Package '", package, "' must be installed.",
      call. = FALSE
    )
  }
}


qb_assert_columns <- function(data, columns, object = "data") {
  missing <- setdiff(columns, names(data))
  
  if (length(missing)) {
    stop(
      object, " is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


qb_assert_unique <- function(data, columns, object = "data") {
  qb_assert_columns(data, columns, object)
  
  if (anyNA(data[columns])) {
    stop(
      object, " contains missing key values.",
      call. = FALSE
    )
  }
  
  if (anyDuplicated(data[columns])) {
    stop(
      object, " must be unique by ",
      paste(columns, collapse = ", "),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


qb_bind <- function(objects) {
  objects <- objects[!vapply(objects, is.null, logical(1))]
  
  if (!length(objects)) {
    return(data.frame())
  }
  
  columns <- unique(unlist(lapply(objects, names)))
  
  objects <- lapply(objects, function(x) {
    for (column in setdiff(columns, names(x))) {
      x[[column]] <- NA
    }
    
    x[columns]
  })
  
  output <- do.call(rbind, objects)
  rownames(output) <- NULL
  output
}


qb_join <- function(x, y, by, y_name = "metadata") {
  qb_assert_unique(y, by, y_name)
  
  order_name <- ".qb_order"
  
  while (order_name %in% names(x)) {
    order_name <- paste0(order_name, "_")
  }
  
  x[[order_name]] <- seq_len(nrow(x))
  
  output <- merge(
    x,
    y,
    by = by,
    all.x = TRUE,
    sort = FALSE,
    suffixes = c("", paste0(".", y_name))
  )
  
  output <- output[
    order(output[[order_name]]),
    ,
    drop = FALSE
  ]
  
  output[[order_name]] <- NULL
  rownames(output) <- NULL
  output
}


qb_softmax <- function(eta) {
  eta <- as.matrix(eta)
  eta <- eta - apply(eta, 1L, max)
  probability <- exp(eta)
  probability / rowSums(probability)
}


qb_draw_category <- function(probability, uniforms) {
  probability <- as.matrix(probability)
  probability <- probability / rowSums(probability)
  
  cumulative <- t(apply(probability, 1L, cumsum))
  cumulative[, ncol(cumulative)] <- 1
  
  vapply(seq_len(nrow(probability)), function(i) {
    which(uniforms[i] <= cumulative[i, ])[1L]
  }, integer(1))
}


qb_hash <- function(file) {
  if (is.null(file)) {
    return(NA_character_)
  }
  
  if (!file.exists(file)) {
    stop("Codebook file does not exist.", call. = FALSE)
  }
  
  unname(tools::md5sum(file))
}


qb_group_folds <- function(group, folds = 5L, seed = 1L) {
  group <- as.character(group)
  unique_group <- unique(group)
  
  folds <- min(as.integer(folds), length(unique_group))
  
  if (folds < 2L) {
    stop("At least two groups are required.", call. = FALSE)
  }
  
  set.seed(seed)
  
  shuffled <- sample(unique_group)
  
  mapping <- setNames(
    rep(seq_len(folds), length.out = length(shuffled)),
    shuffled
  )
  
  unname(mapping[group])
}


qb_weighted_vote <- function(
    labels,
    coders,
    weights,
    tie_uniform
) {
  valid <- !is.na(labels)
  
  labels <- labels[valid]
  coders <- coders[valid]
  
  if (!length(labels)) {
    return(NA_character_)
  }
  
  score <- tapply(weights[coders], labels, sum)
  winners <- sort(names(score)[score == max(score)])
  
  if (length(winners) == 1L) {
    return(winners)
  }
  
  index <- floor(tie_uniform * length(winners)) + 1L
  winners[min(index, length(winners))]
}


qb_top_correlations <- function(blocks, threshold = 0.8) {
  if (length(blocks) < 2L) {
    return(data.frame())
  }
  
  output <- list()
  position <- 0L
  
  for (i in seq_len(length(blocks) - 1L)) {
    for (j in (i + 1L):length(blocks)) {
      a <- blocks[[i]]$matrix
      b <- blocks[[j]]$matrix
      
      correlation <- suppressWarnings(cor(a, b))
      
      maximum <- max(abs(correlation), na.rm = TRUE)
      
      if (is.finite(maximum) && maximum >= threshold) {
        position <- position + 1L
        
        output[[position]] <- data.frame(
          component_a = blocks[[i]]$name,
          component_b = blocks[[j]]$name,
          maximum_absolute_correlation = maximum,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  qb_bind(output)
}


# ----------------------------------------------------------------------
# Data construction
# ----------------------------------------------------------------------

# Convert one-column-per-coder data into assignment rows.
qb_wide_to_long <- function(
    data,
    item_col,
    coder_cols,
    text_col = NULL,
    keep_cols = NULL,
    coder_ids = coder_cols,
    na_values = c("", "NA", "N/A")
) {
  qb_assert_columns(
    data,
    unique(c(item_col, coder_cols, text_col, keep_cols))
  )
  
  if (length(coder_cols) != length(coder_ids)) {
    stop(
      "coder_cols and coder_ids must have equal length.",
      call. = FALSE
    )
  }
  
  assignments <- lapply(seq_along(coder_cols), function(j) {
    label <- as.character(data[[coder_cols[j]]])
    label[is.na(label) | trimws(label) %in% na_values] <- NA
    
    output <- data.frame(
      item_id = as.character(data[[item_col]]),
      coder_id = as.character(coder_ids[j]),
      label = label,
      stringsAsFactors = FALSE
    )
    
    output[!is.na(output$label), , drop = FALSE]
  })
  
  item_columns <- unique(c(item_col, text_col, keep_cols))
  item_columns <- item_columns[!is.na(item_columns)]
  
  items <- data[item_columns]
  names(items)[names(items) == item_col] <- "item_id"
  
  if (!is.null(text_col)) {
    names(items)[names(items) == text_col] <- "text"
  }
  
  list(
    assignments = qb_bind(assignments),
    items = items
  )
}


# Construct the central fixed-codebook data object.
qb_data <- function(
    assignments,
    items,
    coders,
    runs = NULL,
    label_map = NULL,
    codebook_id = "fixed_codebook",
    codebook_file = NULL
) {
  qb_assert_columns(
    assignments,
    c("item_id", "coder_id", "label"),
    "assignments"
  )
  
  qb_assert_columns(items, "item_id", "items")
  qb_assert_columns(coders, "coder_id", "coders")
  
  assignments$item_id <- as.character(assignments$item_id)
  assignments$coder_id <- as.character(assignments$coder_id)
  assignments$label <- as.character(assignments$label)
  
  items$item_id <- as.character(items$item_id)
  coders$coder_id <- as.character(coders$coder_id)
  
  if (!"run_id" %in% names(assignments)) {
    assignments$run_id <- "default_run"
  }
  
  assignments$run_id <- as.character(assignments$run_id)
  
  if (is.null(runs)) {
    runs <- data.frame(
      run_id = unique(assignments$run_id),
      stringsAsFactors = FALSE
    )
  }
  
  runs$run_id <- as.character(runs$run_id)
  
  qb_assert_unique(items, "item_id", "items")
  qb_assert_unique(coders, "coder_id", "coders")
  qb_assert_unique(runs, "run_id", "runs")
  
  qb_assert_unique(
    assignments,
    c("item_id", "coder_id", "run_id"),
    "assignments"
  )
  
  if (length(setdiff(assignments$item_id, items$item_id))) {
    stop("Assignments contain unknown items.", call. = FALSE)
  }
  
  if (length(setdiff(assignments$coder_id, coders$coder_id))) {
    stop("Assignments contain unknown coders.", call. = FALSE)
  }
  
  if (length(setdiff(assignments$run_id, runs$run_id))) {
    stop("Assignments contain unknown runs.", call. = FALSE)
  }
  
  assignments$local_label <- assignments$label
  
  if (!is.null(label_map)) {
    qb_assert_columns(
      label_map,
      c("local_label", "canonical_label"),
      "label_map"
    )
    
    qb_assert_unique(label_map, "local_label", "label_map")
    
    matched <- match(
      assignments$local_label,
      label_map$local_label
    )
    
    if (anyNA(matched)) {
      stop(
        "At least one label is absent from label_map.",
        call. = FALSE
      )
    }
    
    assignments$label <- label_map$canonical_label[matched]
  }
  
  labels <- sort(unique(assignments$label))
  
  if (length(labels) < 2L) {
    stop("At least two labels are required.", call. = FALSE)
  }
  
  structure(
    list(
      assignments = assignments,
      items = items,
      coders = coders,
      runs = runs,
      label_map = label_map,
      labels = labels,
      codebook_id = codebook_id,
      codebook_hash = qb_hash(codebook_file)
    ),
    class = "qb_data"
  )
}


qb_table <- function(data) {
  output <- qb_join(
    data$assignments,
    data$items,
    "item_id",
    "item"
  )
  
  output <- qb_join(
    output,
    data$coders,
    "coder_id",
    "coder"
  )
  
  qb_join(
    output,
    data$runs,
    "run_id",
    "run"
  )
}


# Add normalized sequence variables.
qb_add_sequence <- function(
    data,
    order_col,
    group_cols = c("coder_id", "run_id"),
    prefix = "sequence"
) {
  qb_assert_columns(
    data$assignments,
    c(order_col, group_cols)
  )
  
  assignment <- data$assignments
  
  group <- interaction(
    assignment[group_cols],
    drop = TRUE,
    lex.order = TRUE
  )
  
  groups <- split(seq_len(nrow(assignment)), group)
  
  progress <- quadratic <- centered <-
    rep(NA_real_, nrow(assignment))
  
  for (indices in groups) {
    value <- as.numeric(assignment[[order_col]][indices])
    
    minimum <- min(value, na.rm = TRUE)
    maximum <- max(value, na.rm = TRUE)
    
    local_progress <- if (maximum > minimum) {
      (value - minimum) / (maximum - minimum)
    } else {
      rep(0, length(value))
    }
    
    progress[indices] <- local_progress
    quadratic[indices] <- local_progress^2
    centered[indices] <- value - mean(value, na.rm = TRUE)
  }
  
  assignment[[paste0(prefix, "_progress")]] <- progress
  assignment[[paste0(prefix, "_quadratic")]] <- quadratic
  assignment[[paste0(prefix, "_centered")]] <- centered
  
  data$assignments <- assignment
  data
}


# Add simple frequent-term presence features.
qb_add_text_terms <- function(
    data,
    text_col = "text",
    max_terms = 50L,
    min_df = 5L,
    stopwords = c(
      "the", "and", "for", "that", "with", "this",
      "from", "was", "were", "are", "but", "not"
    )
) {
  qb_assert_columns(data$items, text_col)
  
  text <- tolower(as.character(data$items[[text_col]]))
  text[is.na(text)] <- ""
  
  tokens <- strsplit(
    gsub("[^[:alnum:]_]+", " ", text),
    "\\s+"
  )
  
  tokens <- lapply(tokens, function(x) {
    unique(
      setdiff(
        x[nchar(x) >= 3],
        stopwords
      )
    )
  })
  
  frequency <- sort(
    table(unlist(tokens)),
    decreasing = TRUE
  )
  
  vocabulary <- head(
    names(frequency[frequency >= min_df]),
    max_terms
  )
  
  for (term in vocabulary) {
    name <- make.names(paste0("txt_", term))
    
    data$items[[name]] <- as.integer(
      vapply(tokens, function(x) {
        term %in% x
      }, logical(1))
    )
  }
  
  attr(data, "text_feature_map") <- data.frame(
    term = vocabulary,
    variable = make.names(paste0("txt_", vocabulary)),
    document_frequency = as.integer(frequency[vocabulary]),
    stringsAsFactors = FALSE
  )
  
  data
}


# ----------------------------------------------------------------------
# Descriptive diagnostics
# ----------------------------------------------------------------------

qb_reliability <- function(data) {
  assignments <- data$assignments
  
  groups <- split(assignments$label, assignments$item_id)
  
  counts <- lapply(groups, function(x) {
    if (length(x) < 2L) {
      return(c(agree = 0, pairs = 0))
    }
    
    frequency <- table(x)
    
    c(
      agree = sum(frequency * (frequency - 1) / 2),
      pairs = length(x) * (length(x) - 1) / 2
    )
  })
  
  counts <- do.call(rbind, counts)
  
  agreement <- sum(counts[, "agree"]) /
    sum(counts[, "pairs"])
  
  prevalence <- prop.table(
    table(factor(assignments$label, levels = data$labels))
  )
  
  chance <- sum(prevalence^2)
  
  kappa <- (agreement - chance) / (1 - chance)
  
  ac1_chance <- sum(prevalence * (1 - prevalence)) /
    (length(data$labels) - 1)
  
  ac1 <- (agreement - ac1_chance) / (1 - ac1_chance)
  
  coder_marginals <- as.data.frame(
    prop.table(
      table(
        coder_id = assignments$coder_id,
        label = factor(assignments$label, levels = data$labels)
      ),
      1
    )
  )
  
  names(coder_marginals)[3] <- "proportion"
  
  list(
    agreement = agreement,
    kappa = kappa,
    gwet_ac1 = ac1,
    prevalence = prevalence,
    coder_marginals = coder_marginals
  )
}


qb_design <- function(data) {
  assignments <- data$assignments
  
  coder_items <- split(assignments$item_id, assignments$coder_id)
  coders <- names(coder_items)
  
  overlap <- outer(
    coders,
    coders,
    Vectorize(function(a, b) {
      length(intersect(coder_items[[a]], coder_items[[b]]))
    })
  )
  
  dimnames(overlap) <- list(coders, coders)
  
  # Connectedness of coder graph through shared items.
  adjacency <- overlap > 0
  visited <- setNames(rep(FALSE, length(coders)), coders)
  components <- 0L
  
  for (coder in coders) {
    if (visited[coder]) {
      next
    }
    
    components <- components + 1L
    queue <- coder
    visited[coder] <- TRUE
    
    while (length(queue)) {
      current <- queue[1]
      queue <- queue[-1]
      
      neighbor <- coders[adjacency[current, ]]
      
      for (next_coder in neighbor) {
        if (!visited[next_coder]) {
          visited[next_coder] <- TRUE
          queue <- c(queue, next_coder)
        }
      }
    }
  }
  
  warnings <- character()
  
  if (components > 1L) {
    warnings <- c(
      warnings,
      "The coder comparison graph is disconnected."
    )
  }
  
  if (any(table(assignments$label) < 20L)) {
    warnings <- c(
      warnings,
      "At least one category has fewer than 20 assignments."
    )
  }
  
  list(
    n_items = length(unique(assignments$item_id)),
    n_coders = length(unique(assignments$coder_id)),
    n_assignments = nrow(assignments),
    coder_overlap = overlap,
    connected_components = components,
    warnings = warnings
  )
}


# ----------------------------------------------------------------------
# Component definitions
# ----------------------------------------------------------------------

# kind = "shared": average response to the feature.
# kind = "difference": coder differences in response to the feature.
qb_component <- function(
    name,
    formula,
    kind = c("difference", "shared"),
    level = c("item", "assignment", "run"),
    description = NULL
) {
  kind <- match.arg(kind)
  level <- match.arg(level)
  
  if (!inherits(formula, "formula") || length(formula) != 2L) {
    stop("formula must be one-sided.", call. = FALSE)
  }
  
  structure(
    list(
      name = name,
      formula = formula,
      kind = kind,
      level = level,
      description = description
    ),
    class = "qb_component"
  )
}


qb_formula_recipe <- function(formula, data) {
  terms <- stats::terms(formula, data = data)
  
  frame <- stats::model.frame(
    terms,
    data = data,
    na.action = stats::na.fail,
    drop.unused.levels = FALSE
  )
  
  matrix <- stats::model.matrix(terms, frame)
  
  if ("(Intercept)" %in% colnames(matrix)) {
    matrix <- matrix[
      ,
      colnames(matrix) != "(Intercept)",
      drop = FALSE
    ]
  }
  
  if (!ncol(matrix)) {
    stop("Component produced no columns.", call. = FALSE)
  }
  
  list(
    terms = terms,
    contrasts = attr(matrix, "contrasts"),
    xlevels = lapply(
      frame[vapply(frame, is.factor, logical(1))],
      levels
    ),
    columns = colnames(matrix),
    matrix = matrix
  )
}


qb_apply_recipe <- function(recipe, data) {
  frame <- stats::model.frame(
    recipe$terms,
    data = data,
    xlev = recipe$xlevels,
    na.action = stats::na.fail,
    drop.unused.levels = FALSE
  )
  
  matrix <- stats::model.matrix(
    recipe$terms,
    frame,
    contrasts.arg = recipe$contrasts
  )
  
  if ("(Intercept)" %in% colnames(matrix)) {
    matrix <- matrix[
      ,
      colnames(matrix) != "(Intercept)",
      drop = FALSE
    ]
  }
  
  missing <- setdiff(recipe$columns, colnames(matrix))
  
  if (length(missing)) {
    matrix <- cbind(
      matrix,
      matrix(
        0,
        nrow(matrix),
        length(missing),
        dimnames = list(NULL, missing)
      )
    )
  }
  
  matrix[, recipe$columns, drop = FALSE]
}


qb_reference_rows <- function(data, level) {
  switch(
    level,
    item = !duplicated(data$item_id),
    run = !duplicated(data$run_id),
    assignment = rep(TRUE, nrow(data))
  )
}


# Within-block centering and orthonormalization.
qb_basis <- function(raw, reference_raw, tolerance = 1e-8) {
  center <- colMeans(reference_raw)
  centered <- sweep(raw, 2L, center, "-")
  
  decomposition <- svd(centered)
  
  retained <- which(
    decomposition$d >
      tolerance * max(decomposition$d)
  )
  
  if (!length(retained)) {
    stop("Component has no variation after centering.", call. = FALSE)
  }
  
  transform <- decomposition$v[, retained, drop = FALSE] %*%
    diag(
      sqrt(nrow(centered)) /
        decomposition$d[retained],
      nrow = length(retained)
    )
  
  list(
    matrix = centered %*% transform,
    center = center,
    transform = transform
  )
}


# Weighted coder contrasts spanning the weighted-mean-zero space.
qb_coder_contrasts <- function(coders, weights) {
  weights <- weights[coders]
  
  if (anyNA(weights) || any(weights < 0) || sum(weights) <= 0) {
    stop("Invalid coder weights.", call. = FALSE)
  }
  
  if (length(coders) == 1L) {
    return(matrix(0, 1, 0, dimnames = list(coders, NULL)))
  }
  
  q <- qr.Q(
    qr(matrix(weights, ncol = 1)),
    complete = TRUE
  )
  
  contrast <- q[, -1L, drop = FALSE]
  rownames(contrast) <- coders
  colnames(contrast) <- paste0("coder_basis_", seq_len(ncol(contrast)))
  contrast
}


qb_prepare_design <- function(
    data,
    components,
    coder_weights = NULL,
    correlation_warning = 0.8
) {
  table <- qb_table(data)
  coders <- data$coders$coder_id
  
  if (is.null(coder_weights)) {
    coder_weights <- setNames(rep(1, length(coders)), coders)
  }
  
  contrast <- qb_coder_contrasts(coders, coder_weights)
  coder_index <- match(table$coder_id, coders)
  
  blocks <- list()
  
  # Intercept is represented by glmnet's category intercept.
  # Coder propensity is an explicit centered difference block.
  if (ncol(contrast)) {
    blocks[[1L]] <- list(
      name = "coder_propensity",
      kind = "difference",
      formula = NULL,
      level = "coder",
      recipe = NULL,
      basis = contrast[coder_index, , drop = FALSE],
      matrix = contrast[coder_index, , drop = FALSE],
      raw_columns = colnames(contrast)
    )
  }
  
  for (component in components) {
    recipe <- qb_formula_recipe(component$formula, table)
    
    reference <- qb_reference_rows(table, component$level)
    
    standardized <- qb_basis(
      recipe$matrix,
      recipe$matrix[reference, , drop = FALSE]
    )
    
    z <- standardized$matrix
    
    if (component$kind == "shared") {
      design <- z
    } else {
      if (!ncol(contrast)) {
        stop(
          "Difference components require at least two coders.",
          call. = FALSE
        )
      }
      
      design <- do.call(
        cbind,
        lapply(seq_len(ncol(z)), function(j) {
          z[, j] * contrast[coder_index, , drop = FALSE]
        })
      )
    }
    
    blocks[[length(blocks) + 1L]] <- list(
      name = component$name,
      kind = component$kind,
      formula = component$formula,
      level = component$level,
      description = component$description,
      recipe = recipe,
      center = standardized$center,
      transform = standardized$transform,
      matrix = unname(design),
      basis_dimension = ncol(z),
      coder_contrast = contrast,
      raw_columns = recipe$columns
    )
  }
  
  if (anyDuplicated(vapply(blocks, `[[`, character(1), "name"))) {
    stop("Component names must be unique.", call. = FALSE)
  }
  
  position <- 0L
  
  for (j in seq_along(blocks)) {
    columns <- position + seq_len(ncol(blocks[[j]]$matrix))
    blocks[[j]]$columns <- columns
    position <- max(columns)
  }
  
  x <- do.call(cbind, lapply(blocks, `[[`, "matrix"))
  colnames(x) <- unlist(lapply(blocks, function(block) {
    paste0(block$name, "::", seq_len(ncol(block$matrix)))
  }))
  
  correlation <- qb_top_correlations(blocks, correlation_warning)
  
  if (nrow(correlation)) {
    warning(
      "Named component blocks are highly correlated. ",
      "Their separate contributions may be weakly determined.",
      call. = FALSE
    )
  }
  
  list(
    table = table,
    x = Matrix::Matrix(x, sparse = TRUE),
    blocks = blocks,
    coders = coders,
    coder_weights = coder_weights[coders],
    coder_contrast = contrast,
    labels = data$labels,
    component_correlations = correlation
  )
}


qb_apply_design <- function(specification, newdata) {
  coder_index <- match(newdata$coder_id, specification$coders)
  
  if (anyNA(coder_index)) {
    stop("Prediction contains unknown coders.", call. = FALSE)
  }
  
  matrices <- vector("list", length(specification$blocks))
  
  for (j in seq_along(specification$blocks)) {
    block <- specification$blocks[[j]]
    
    if (block$name == "coder_propensity") {
      matrices[[j]] <- specification$coder_contrast[
        coder_index,
        ,
        drop = FALSE
      ]
      
      next
    }
    
    raw <- qb_apply_recipe(block$recipe, newdata)
    
    z <- sweep(raw, 2L, block$center, "-") %*%
      block$transform
    
    if (block$kind == "shared") {
      matrices[[j]] <- z
    } else {
      matrices[[j]] <- do.call(
        cbind,
        lapply(seq_len(ncol(z)), function(k) {
          z[, k] *
            specification$coder_contrast[
              coder_index,
              ,
              drop = FALSE
            ]
        })
      )
    }
  }
  
  Matrix::Matrix(do.call(cbind, matrices), sparse = TRUE)
}


# ----------------------------------------------------------------------
# Multinomial fitting and cross-fitting
# ----------------------------------------------------------------------

qb_extract_coefficients <- function(fit, lambda, columns, labels) {
  coefficient_list <- stats::coef(
    fit$glmnet.fit,
    s = lambda
  )
  
  intercept <- setNames(numeric(length(labels)), labels)
  
  beta <- matrix(
    0,
    nrow = length(columns),
    ncol = length(labels),
    dimnames = list(columns, labels)
  )
  
  for (label in labels) {
    coefficient <- as.matrix(coefficient_list[[label]])
    
    intercept[label] <- coefficient["(Intercept)", 1L]
    
    available <- intersect(columns, rownames(coefficient))
    
    beta[available, label] <- coefficient[available, 1L]
  }
  
  list(intercept = intercept, beta = beta)
}


qb_fit_once <- function(
    data,
    components,
    coder_weights,
    folds,
    alpha,
    seed,
    lambda_choice,
    correlation_warning
) {
  qb_require("glmnet")
  qb_require("Matrix")
  
  design <- qb_prepare_design(
    data,
    components,
    coder_weights,
    correlation_warning
  )
  
  y <- factor(
    design$table$label,
    levels = data$labels
  )
  
  fold_id <- qb_group_folds(
    design$table$item_id,
    folds,
    seed
  )
  
  fit <- glmnet::cv.glmnet(
    x = design$x,
    y = y,
    family = "multinomial",
    alpha = alpha,
    foldid = fold_id,
    type.multinomial = "grouped",
    standardize = FALSE
  )
  
  lambda <- if (lambda_choice == "min") {
    fit$lambda.min
  } else {
    fit$lambda.1se
  }
  
  coefficients <- qb_extract_coefficients(
    fit,
    lambda,
    colnames(design$x),
    data$labels
  )
  
  structure(
    list(
      data = data,
      components = components,
      specification = design,
      cv_fit = fit,
      lambda = lambda,
      intercept = coefficients$intercept,
      beta = coefficients$beta,
      alpha = alpha,
      seed = seed
    ),
    class = "qb_fit"
  )
}


qb_eta <- function(fit, newdata) {
  x <- qb_apply_design(fit$specification, newdata)
  
  contribution <- lapply(
    fit$specification$blocks,
    function(block) {
      as.matrix(
        x[, block$columns, drop = FALSE] %*%
          fit$beta[block$columns, , drop = FALSE]
      )
    }
  )
  
  names(contribution) <- vapply(
    fit$specification$blocks,
    `[[`,
    character(1),
    "name"
  )
  
  total <- matrix(
    rep(fit$intercept, each = nrow(newdata)),
    nrow = nrow(newdata),
    dimnames = list(NULL, fit$data$labels)
  )
  
  for (value in contribution) {
    total <- total + value
  }
  
  list(
    intercept = fit$intercept,
    contributions = contribution,
    total = total,
    x = x
  )
}


# Fit final model and item-grouped out-of-fold probabilities.
qb_fit <- function(
    data,
    components,
    coder_weights = NULL,
    folds = 5L,
    alpha = 0.5,
    seed = 1L,
    lambda_choice = c("min", "1se"),
    correlation_warning = 0.8
) {
  lambda_choice <- match.arg(lambda_choice)
  
  design_check <- qb_design(data)
  
  if (design_check$connected_components > 1L) {
    stop(
      "The coder comparison graph is disconnected.",
      call. = FALSE
    )
  }
  
  final <- qb_fit_once(
    data,
    components,
    coder_weights,
    folds,
    alpha,
    seed,
    lambda_choice,
    correlation_warning
  )
  
  table <- final$specification$table
  fold_id <- qb_group_folds(table$item_id, folds, seed)
  out_of_fold <- matrix(
    NA_real_,
    nrow(table),
    length(data$labels),
    dimnames = list(NULL, data$labels)
  )
  
  for (fold in sort(unique(fold_id))) {
    validation_items <- unique(table$item_id[fold_id == fold])
    
    train_assignments <- data$assignments[
      !data$assignments$item_id %in% validation_items,
      ,
      drop = FALSE
    ]
    
    validation_assignments <- data$assignments[
      data$assignments$item_id %in% validation_items,
      ,
      drop = FALSE
    ]
    
    train_items <- data$items[
      data$items$item_id %in% train_assignments$item_id,
      ,
      drop = FALSE
    ]
    
    training <- qb_data(
      train_assignments,
      train_items,
      data$coders,
      data$runs,
      data$label_map,
      data$codebook_id
    )
    
    fold_fit <- qb_fit_once(
      training,
      components,
      coder_weights,
      max(2L, folds - 1L),
      alpha,
      seed + fold,
      lambda_choice,
      correlation_warning = Inf
    )
    
    validation_table <- qb_table(
      structure(
        list(
          assignments = validation_assignments,
          items = data$items[
            data$items$item_id %in% validation_items,
            ,
            drop = FALSE
          ],
          coders = data$coders,
          runs = data$runs,
          labels = data$labels
        ),
        class = "qb_data"
      )
    )
    
    probability <- qb_softmax(
      qb_eta(fold_fit, validation_table)$total
    )
    
    original_key <- paste(
      table$item_id,
      table$coder_id,
      table$run_id,
      sep = "\r"
    )
    
    validation_key <- paste(
      validation_table$item_id,
      validation_table$coder_id,
      validation_table$run_id,
      sep = "\r"
    )
    
    out_of_fold[match(validation_key, original_key), ] <- probability
  }
  
  final$fitted_probability <- qb_softmax(
    qb_eta(final, table)$total
  )
  
  final$crossfit_probability <- out_of_fold
  
  observed <- match(table$label, data$labels)
  
  final$crossfit_log_loss <- -mean(log(
    out_of_fold[cbind(seq_len(nrow(table)), observed)] + 1e-12
  ))
  
  final$crossfit_accuracy <- mean(
    data$labels[max.col(out_of_fold)] == table$label
  )
  
  final
}


qb_component_summary <- function(fit) {
  qb_bind(lapply(
    fit$specification$blocks,
    function(block) {
      coefficient <- fit$beta[block$columns, , drop = FALSE]
      
      data.frame(
        component = block$name,
        encoded_column = rownames(coefficient),
        label = rep(
          colnames(coefficient),
          each = nrow(coefficient)
        ),
        coefficient = as.vector(coefficient),
        stringsAsFactors = FALSE
      )
    }
  ))
}


qb_compare_fits <- function(a, b) {
  data.frame(
    model = c("a", "b"),
    lambda = c(a$lambda, b$lambda),
    crossfit_log_loss = c(
      a$crossfit_log_loss,
      b$crossfit_log_loss
    ),
    crossfit_accuracy = c(
      a$crossfit_accuracy,
      b$crossfit_accuracy
    ),
    stringsAsFactors = FALSE
  )
}


# ----------------------------------------------------------------------
# Residual discovery
# ----------------------------------------------------------------------

qb_residual_matrix <- function(fit) {
  table <- fit$specification$table
  labels <- fit$data$labels
  items <- fit$data$items$item_id
  coders <- fit$data$coders$coder_id
  
  residual <- matrix(
    NA_real_,
    nrow = length(items),
    ncol = length(coders) * length(labels),
    dimnames = list(
      items,
      as.vector(outer(coders, labels, paste, sep = "\r"))
    )
  )
  
  count <- matrix(
    0L,
    length(items),
    length(coders),
    dimnames = list(items, coders)
  )
  
  sum_array <- array(
    0,
    dim = c(length(items), length(coders), length(labels))
  )
  
  for (row in seq_len(nrow(table))) {
    i <- match(table$item_id[row], items)
    c <- match(table$coder_id[row], coders)
    y <- match(table$label[row], labels)
    
    observed <- numeric(length(labels))
    observed[y] <- 1
    
    sum_array[i, c, ] <- sum_array[i, c, ] +
      observed - fit$crossfit_probability[row, ]
    
    count[i, c] <- count[i, c] + 1L
  }
  
  for (i in seq_along(items)) {
    for (c in seq_along(coders)) {
      if (count[i, c] > 0) {
        columns <- paste(coders[c], labels, sep = "\r")
        residual[i, columns] <- sum_array[i, c, ] / count[i, c]
      }
    }
  }
  
  list(
    matrix = residual,
    items = items,
    coders = coders,
    labels = labels,
    count = count
  )
}


qb_softimpute <- function(x, rank, lambda) {
  qb_require("softImpute")
  
  fitted <- softImpute::softImpute(
    x,
    rank.max = rank,
    lambda = lambda,
    type = "svd"
  )
  
  softImpute::complete(x, fitted)
}


qb_residual_cv <- function(
    residual,
    ranks,
    lambdas,
    folds,
    seed
) {
  blocks <- which(
    apply(
      array(
        is.finite(residual$matrix),
        dim = c(
          length(residual$items),
          length(residual$coders),
          length(residual$labels)
        )
      ),
      c(1, 2),
      any
    ),
    arr.ind = TRUE
  )
  
  set.seed(seed)
  fold_id <- sample(
    rep(seq_len(folds), length.out = nrow(blocks))
  )
  
  output <- list()
  position <- 0L
  
  for (rank in ranks) {
    for (lambda in lambdas) {
      for (fold in seq_len(folds)) {
        training <- residual$matrix
        selected <- blocks[fold_id == fold, , drop = FALSE]
        
        validation_cells <- matrix(
          FALSE,
          nrow(training),
          ncol(training)
        )
        
        for (row in seq_len(nrow(selected))) {
          i <- selected[row, 1]
          c <- selected[row, 2]
          
          columns <- ((c - 1L) * length(residual$labels) + 1L):
            (c * length(residual$labels))
          
          validation_cells[i, columns] <- is.finite(
            residual$matrix[i, columns]
          )
          
          training[i, columns] <- NA
        }
        
        completed <- qb_softimpute(training, rank, lambda)
        
        truth <- residual$matrix[validation_cells]
        prediction <- completed[validation_cells]
        
        position <- position + 1L
        
        output[[position]] <- data.frame(
          rank = rank,
          lambda = lambda,
          fold = fold,
          rmse = sqrt(mean((truth - prediction)^2)),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  qb_bind(output)
}


qb_residuals <- function(
    fit,
    ranks = 1:4,
    lambdas = NULL,
    folds = 5L,
    stability_repetitions = 30L,
    stability_fraction = 0.8,
    seed = 1L
) {
  residual <- qb_residual_matrix(fit)
  
  observed_scale <- stats::sd(
    residual$matrix[is.finite(residual$matrix)]
  )
  
  if (is.null(lambdas)) {
    lambdas <- observed_scale * c(0.05, 0.2, 0.8)
  }
  
  ranks <- ranks[
    ranks >= 1 &
      ranks <= min(dim(residual$matrix))
  ]
  
  validation <- qb_residual_cv(
    residual,
    ranks,
    lambdas,
    folds,
    seed
  )
  
  summary <- aggregate(
    rmse ~ rank + lambda,
    validation,
    mean
  )
  
  selected <- summary[which.min(summary$rmse), ]
  
  completed <- qb_softimpute(
    residual$matrix,
    selected$rank,
    selected$lambda
  )
  
  decomposition <- svd(
    completed,
    nu = selected$rank,
    nv = selected$rank
  )
  
  scores <- decomposition$u[, seq_len(selected$rank), drop = FALSE] %*%
    diag(decomposition$d[seq_len(selected$rank)], nrow = selected$rank)
  
  loadings <- decomposition$v[, seq_len(selected$rank), drop = FALSE]
  
  axis_names <- paste0("residual_axis_", seq_len(selected$rank))
  
  colnames(scores) <- colnames(loadings) <- axis_names
  rownames(scores) <- residual$items
  rownames(loadings) <- colnames(residual$matrix)
  
  # Deterministic sign convention.
  for (axis in seq_len(selected$rank)) {
    anchor <- which.max(abs(loadings[, axis]))
    
    if (loadings[anchor, axis] < 0) {
      loadings[, axis] <- -loadings[, axis]
      scores[, axis] <- -scores[, axis]
    }
  }
  
  # Calibrate one common utility scale for the complete residual surface.
  table <- fit$specification$table
  
  assignment_residual <- matrix(
    0,
    nrow(table),
    length(fit$data$labels)
  )
  
  for (row in seq_len(nrow(table))) {
    i <- match(table$item_id[row], residual$items)
    c <- match(table$coder_id[row], residual$coders)
    
    columns <- ((c - 1L) * length(residual$labels) + 1L):
      (c * length(residual$labels))
    
    value <- completed[i, columns]
    assignment_residual[row, ] <- value - mean(value)
  }
  
  observed <- match(table$label, fit$data$labels)
  base_eta <- log(fit$fitted_probability + 1e-12)
  
  objective <- function(scale) {
    probability <- qb_softmax(base_eta + scale * assignment_residual)
    
    -sum(log(
      probability[cbind(seq_len(nrow(table)), observed)] + 1e-12
    ))
  }
  
  scale <- optimize(objective, c(0, 20))$minimum
  
  # Axis stability based on loading correlation under item subsampling.
  set.seed(seed + 1000L)
  
  stability <- matrix(
    NA_real_,
    nrow = stability_repetitions,
    ncol = selected$rank
  )
  
  for (repetition in seq_len(stability_repetitions)) {
    sampled_items <- sample(
      residual$items,
      max(
        selected$rank + 2L,
        floor(length(residual$items) * stability_fraction)
      )
    )
    
    local_matrix <- residual$matrix[
      sampled_items,
      ,
      drop = FALSE
    ]
    
    local_completed <- qb_softimpute(
      local_matrix,
      selected$rank,
      selected$lambda
    )
    
    local_svd <- svd(
      local_completed,
      nu = selected$rank,
      nv = selected$rank
    )
    
    local_loading <- local_svd$v[, seq_len(selected$rank), drop = FALSE]
    
    correlation <- cor(loadings, local_loading)
    
    remaining <- seq_len(selected$rank)
    
    for (axis in seq_len(selected$rank)) {
      candidate <- remaining[
        which.max(abs(correlation[axis, remaining]))
      ]
      
      stability[repetition, axis] <- abs(
        correlation[axis, candidate]
      )
      
      remaining <- setdiff(remaining, candidate)
    }
  }
  
  stability_summary <- data.frame(
    axis = axis_names,
    mean_loading_correlation = colMeans(stability, na.rm = TRUE),
    tenth_percentile = apply(
      stability,
      2,
      quantile,
      probs = 0.10,
      na.rm = TRUE
    ),
    stable =
      colMeans(stability, na.rm = TRUE) >= 0.8 &
      apply(
        stability,
        2,
        quantile,
        probs = 0.10,
        na.rm = TRUE
      ) >= 0.6,
    stringsAsFactors = FALSE
  )
  
  residual_fit <- list(
    raw = residual,
    completed = completed,
    scores = scores,
    loadings = loadings,
    singular_values = decomposition$d[seq_len(selected$rank)],
    rank = selected$rank,
    lambda = selected$lambda,
    validation = validation,
    validation_summary = summary,
    utility_scale = scale,
    stability = stability_summary,
    cross_fitted = TRUE
  )
  
  class(residual_fit) <- "qb_residual_fit"
  
  fit$residual <- residual_fit
  fit
}


qb_inspect_residual <- function(fit, axis, top = 10L) {
  if (is.null(fit$residual)) {
    stop("No residual model is attached.", call. = FALSE)
  }
  
  axis_name <- if (is.numeric(axis)) {
    colnames(fit$residual$scores)[axis]
  } else {
    axis
  }
  
  score <- fit$residual$scores[, axis_name]
  loading <- fit$residual$loadings[, axis_name]
  
  item_table <- data.frame(
    item_id = names(score),
    score = score,
    stringsAsFactors = FALSE
  )
  
  item_table <- qb_join(
    item_table,
    fit$data$items,
    "item_id",
    "item"
  )
  
  parsed <- strsplit(names(loading), "\r", fixed = TRUE)
  
  loading_table <- data.frame(
    coder_id = vapply(parsed, `[`, character(1), 1L),
    label = vapply(parsed, `[`, character(1), 2L),
    loading = loading,
    stringsAsFactors = FALSE
  )
  
  list(
    positive_items = head(
      item_table[order(item_table$score, decreasing = TRUE), ],
      top
    ),
    negative_items = head(
      item_table[order(item_table$score), ],
      top
    ),
    positive_loadings = head(
      loading_table[order(loading_table$loading, decreasing = TRUE), ],
      top
    ),
    negative_loadings = head(
      loading_table[order(loading_table$loading), ],
      top
    ),
    stability = fit$residual$stability[
      fit$residual$stability$axis == axis_name,
      ,
      drop = FALSE
    ]
  )
}


# ----------------------------------------------------------------------
# Regimes and predictions
# ----------------------------------------------------------------------

qb_regime <- function(
    name,
    multipliers = numeric(),
    evidence = c("exploratory", "confirmed"),
    description = NULL
) {
  evidence <- match.arg(evidence)
  
  if (length(multipliers) && is.null(names(multipliers))) {
    stop("multipliers must be named.", call. = FALSE)
  }
  
  structure(
    list(
      name = name,
      multipliers = multipliers,
      evidence = evidence,
      description = description
    ),
    class = "qb_regime"
  )
}


qb_components <- function(fit) {
  names <- vapply(
    fit$specification$blocks,
    `[[`,
    character(1),
    "name"
  )
  
  if (!is.null(fit$residual)) {
    names <- c(
      names,
      "residual_complete",
      colnames(fit$residual$scores)
    )
  }
  
  names
}


qb_validate_regime <- function(fit, regime) {
  unknown <- setdiff(names(regime$multipliers), qb_components(fit))
  
  if (length(unknown)) {
    stop(
      "Unknown regime components: ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (
    regime$evidence == "confirmed" &&
    !is.null(fit$residual)
  ) {
    axes <- intersect(
      names(regime$multipliers),
      fit$residual$stability$axis
    )
    
    unstable <- axes[
      !fit$residual$stability$stable[
        match(axes, fit$residual$stability$axis)
      ]
    ]
    
    if (length(unstable)) {
      stop(
        "Confirmed regime uses unstable residual axes: ",
        paste(unstable, collapse = ", "),
        call. = FALSE
      )
    }
  }
  
  invisible(TRUE)
}


qb_prediction_grid <- function(
    fit,
    items = fit$data$items$item_id,
    coders = fit$data$coders$coder_id,
    assignment_data = NULL
) {
  grid <- expand.grid(
    item_id = items,
    coder_id = coders,
    stringsAsFactors = FALSE
  )
  
  grid <- qb_join(grid, fit$data$items, "item_id", "item")
  grid <- qb_join(grid, fit$data$coders, "coder_id", "coder")
  
  if (!is.null(assignment_data)) {
    qb_assert_unique(
      assignment_data,
      c("item_id", "coder_id"),
      "assignment_data"
    )
    
    grid <- qb_join(
      grid,
      assignment_data,
      c("item_id", "coder_id"),
      "assignment"
    )
  }
  
  grid
}


qb_residual_grid <- function(fit, grid) {
  if (is.null(fit$residual)) {
    return(NULL)
  }
  
  items <- fit$residual$raw$items
  coders <- fit$residual$raw$coders
  labels <- fit$residual$raw$labels
  axes <- colnames(fit$residual$scores)
  
  complete <- array(
    0,
    dim = c(nrow(grid), length(labels)),
    dimnames = list(NULL, labels)
  )
  
  axis_array <- array(
    0,
    dim = c(length(axes), nrow(grid), length(labels)),
    dimnames = list(axes, NULL, labels)
  )
  
  for (row in seq_len(nrow(grid))) {
    i <- match(grid$item_id[row], items)
    c <- match(grid$coder_id[row], coders)
    
    if (is.na(i) || is.na(c)) {
      stop(
        "Residual contributions are available only for observed items and coders.",
        call. = FALSE
      )
    }
    
    columns <- ((c - 1L) * length(labels) + 1L):
      (c * length(labels))
    
    value <- fit$residual$completed[i, columns]
    value <- value - mean(value)
    
    complete[row, ] <- fit$residual$utility_scale * value
    
    for (axis in seq_along(axes)) {
      axis_value <- fit$residual$scores[i, axis] *
        fit$residual$loadings[columns, axis]
      
      axis_value <- axis_value - mean(axis_value)
      
      axis_array[axis, row, ] <-
        fit$residual$utility_scale * axis_value
    }
  }
  
  list(
    complete = complete,
    axes = axis_array
  )
}


# qb_regime_probabilities <- function(
#     fit,
#     regime,
#     grid,
#     process = c("coder", "shared")
# ) {
#   process <- match.arg(process)
#   qb_validate_regime(fit, regime)
#   
#   eta_parts <- qb_eta(fit, grid)
#   contributions <- eta_parts$contributions
#   
#   total <- matrix(
#     rep(fit$intercept, each = nrow(grid)),
#     nrow(grid),
#     dimnames = list(NULL, fit$data$labels)
#   )
#   
#   for (block in fit$specification$blocks) {
#     multiplier <- regime$multipliers[[block$name]] %||% 1
#     
#     include <- if (process == "shared") {
#       block$kind == "shared"
#     } else {
#       TRUE
#     }
#     
#     if (include) {
#       total <- total + multiplier * contributions[[block$name]]
#     }
#   }
#   
#   if (process == "coder" && !is.null(fit$residual)) {
#     residual <- qb_residual_grid(fit, grid)
#     
#     default <- regime$multipliers[["residual_complete"]] %||% 1
#     
#     for (axis in seq_len(dim(residual$axes)[1])) {
#       axis_name <- dimnames(residual$axes)[[1]][axis]
#       
#       multiplier <- regime$multipliers[[axis_name]] %||% default
#       
#       total <- total + multiplier * residual$axes[axis, , ]
#     }
#   }
#   
#   qb_softmax(total)
# }

qb_regime_probabilities <- function(
    fit,
    regime,
    grid,
    process = c("coder", "shared")
) {
  process <- match.arg(process)
  qb_validate_regime(fit, regime)
  
  eta_parts <- qb_eta(fit, grid)
  contributions <- eta_parts$contributions
  
  total <- matrix(
    rep(fit$intercept, each = nrow(grid)),
    nrow(grid),
    dimnames = list(NULL, fit$data$labels)
  )
  
  for (block in fit$specification$blocks) {
    multiplier <- if (block$name %in% names(regime$multipliers)) {
      regime$multipliers[[block$name]]
    } else {
      1
    }
    
    include <- if (process == "shared") {
      block$kind == "shared"
    } else {
      TRUE
    }
    
    if (include) {
      total <- total + multiplier * contributions[[block$name]]
    }
  }
  
  if (process == "coder" && !is.null(fit$residual)) {
    residual <- qb_residual_grid(fit, grid)
    
    default <- if ("residual_complete" %in% names(regime$multipliers)) {
      regime$multipliers[["residual_complete"]]
    } else {
      1
    }
    
    for (axis in seq_len(dim(residual$axes)[1])) {
      axis_name <- dimnames(residual$axes)[[1]][axis]
      
      multiplier <- if (axis_name %in% names(regime$multipliers)) {
        regime$multipliers[[axis_name]]
      } else {
        default
      }
      
      total <- total + multiplier * residual$axes[axis, , ]
    }
  }
  
  qb_softmax(total)
}

# ----------------------------------------------------------------------
# Targets and label generation
# ----------------------------------------------------------------------

qb_target_shared <- function() {
  list(type = "shared")
}


qb_target_coder <- function(coder_id) {
  list(type = "coder", coders = coder_id)
}


qb_target_assignments <- function(coder_ids) {
  list(type = "assignments", coders = coder_ids)
}


qb_target_panel <- function(
    coder_ids,
    weights = NULL
) {
  if (is.null(weights)) {
    weights <- setNames(rep(1, length(coder_ids)), coder_ids)
  }
  
  list(
    type = "panel",
    coders = coder_ids,
    weights = weights[coder_ids]
  )
}


qb_generate <- function(
    fit,
    regimes,
    target,
    items = fit$data$items$item_id,
    assignment_data = NULL,
    draws = 200L,
    seed = 1L
) {
  if (inherits(regimes, "qb_regime")) {
    regimes <- list(regimes)
  }
  
  regime_names <- vapply(regimes, `[[`, character(1), "name")
  
  if (anyDuplicated(regime_names)) {
    stop("Regime names must be unique.", call. = FALSE)
  }
  
  coders <- if (target$type == "shared") {
    fit$data$coders$coder_id[1L]
  } else {
    target$coders
  }
  
  grid <- qb_prediction_grid(
    fit,
    items,
    coders,
    assignment_data
  )
  
  # expand.grid orders all items within coder.
  I <- length(items)
  C <- length(coders)
  
  set.seed(seed)
  
  uniforms <- array(
    runif(draws * I * C),
    dim = c(draws, I, C)
  )
  
  tie_uniform <- matrix(
    runif(draws * I),
    nrow = draws,
    ncol = I
  )
  
  assignment_output <- list()
  item_output <- list()
  position <- 0L
  
  for (regime in regimes) {
    probability <- qb_regime_probabilities(
      fit,
      regime,
      grid,
      process = if (target$type == "shared") {
        "shared"
      } else {
        "coder"
      }
    )
    
    for (draw in seq_len(draws)) {
      coder_labels <- matrix(
        NA_character_,
        nrow = I,
        ncol = C,
        dimnames = list(items, coders)
      )
      
      for (c in seq_len(C)) {
        rows <- ((c - 1L) * I + 1L):(c * I)
        
        selected <- qb_draw_category(
          probability[rows, , drop = FALSE],
          uniforms[draw, , c]
        )
        
        coder_labels[, c] <- fit$data$labels[selected]
      }
      
      position <- position + 1L
      
      assignment_output[[position]] <- data.frame(
        regime = regime$name,
        draw = draw,
        item_id = rep(items, times = C),
        coder_id = rep(coders, each = I),
        label = as.vector(coder_labels),
        stringsAsFactors = FALSE
      )
      
      if (target$type != "assignments") {
        item_label <- if (
          target$type %in% c("shared", "coder")
        ) {
          coder_labels[, 1L]
        } else {
          vapply(seq_len(I), function(i) {
            qb_weighted_vote(
              coder_labels[i, ],
              coders,
              target$weights,
              tie_uniform[draw, i]
            )
          }, character(1))
        }
        
        item_output[[position]] <- data.frame(
          regime = regime$name,
          draw = draw,
          item_id = items,
          label = item_label,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  structure(
    list(
      assignments = qb_bind(assignment_output),
      items = qb_bind(item_output),
      regimes = regimes,
      target = target,
      labels = fit$data$labels,
      seed = seed
    ),
    class = "qb_generated"
  )
}


# ----------------------------------------------------------------------
# Generated-label diagnostics
# ----------------------------------------------------------------------

qb_compare_labels <- function(
    generated,
    reference,
    level = c("item", "assignment")
) {
  level <- match.arg(level)
  
  data <- if (level == "item") {
    generated$items
  } else {
    generated$assignments
  }
  
  keys <- c("draw", "item_id")
  
  if (level == "assignment") {
    keys <- c(keys, "coder_id")
  }
  
  base <- data[
    data$regime == reference,
    c(keys, "label")
  ]
  
  alternatives <- setdiff(unique(data$regime), reference)
  
  output <- lapply(alternatives, function(regime) {
    alternative <- data[
      data$regime == regime,
      c(keys, "label")
    ]
    
    matched <- merge(
      base,
      alternative,
      by = keys,
      suffixes = c("_reference", "_alternative")
    )
    
    data.frame(
      reference = reference,
      alternative = regime,
      level = level,
      proportion_changed = mean(
        matched$label_reference != matched$label_alternative
      ),
      comparisons = nrow(matched),
      stringsAsFactors = FALSE
    )
  })
  
  qb_bind(output)
}


qb_transitions <- function(
    generated,
    reference,
    level = c("item", "assignment")
) {
  level <- match.arg(level)
  
  data <- if (level == "item") {
    generated$items
  } else {
    generated$assignments
  }
  
  keys <- c("draw", "item_id")
  
  if (level == "assignment") {
    keys <- c(keys, "coder_id")
  }
  
  base <- data[
    data$regime == reference,
    c(keys, "label")
  ]
  
  qb_bind(lapply(
    setdiff(unique(data$regime), reference),
    function(regime) {
      alternative <- data[
        data$regime == regime,
        c(keys, "label")
      ]
      
      matched <- merge(
        base,
        alternative,
        by = keys,
        suffixes = c("_reference", "_alternative")
      )
      
      transition <- as.data.frame(
        table(
          reference_label = matched$label_reference,
          alternative_label = matched$label_alternative
        )
      )
      
      transition$alternative <- regime
      transition
    }
  ))
}


qb_prevalence <- function(
    generated,
    level = c("item", "assignment")
) {
  level <- match.arg(level)
  
  data <- if (level == "item") {
    generated$items
  } else {
    generated$assignments
  }
  
  groups <- split(
    data,
    interaction(data$regime, data$draw, drop = TRUE)
  )
  
  qb_bind(lapply(groups, function(group) {
    frequency <- table(
      factor(group$label, levels = generated$labels)
    )
    
    data.frame(
      regime = group$regime[1],
      draw = group$draw[1],
      label = generated$labels,
      prevalence = as.numeric(frequency) / sum(frequency),
      stringsAsFactors = FALSE
    )
  }))
}


qb_stability <- function(generated) {
  groups <- split(generated$items, generated$items$item_id)
  
  qb_bind(lapply(groups, function(group) {
    probability <- prop.table(table(group$label))
    positive <- probability[probability > 0]
    
    data.frame(
      item_id = group$item_id[1],
      modal_label = names(probability)[which.max(probability)],
      stability = max(probability),
      normalized_entropy = -sum(positive * log(positive)) /
        log(length(generated$labels)),
      number_of_labels = length(probability),
      stringsAsFactors = FALSE
    )
  }))
}


# ----------------------------------------------------------------------
# Graphics
# ----------------------------------------------------------------------

qb_plot_components <- function(fit) {
  qb_require("ggplot2")
  
  data <- qb_component_summary(fit)
  
  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = stats::reorder(encoded_column, coefficient),
      y = coefficient,
      fill = label
    )
  ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~ component, scales = "free_y") +
    ggplot2::labs(
      title = "Estimated named coding components",
      x = NULL,
      y = "Multinomial utility coefficient"
    ) +
    ggplot2::theme_minimal(base_size = 10)
}


qb_plot_residual <- function(fit, axis, top = 10L) {
  qb_require("ggplot2")
  
  inspected <- qb_inspect_residual(fit, axis, top)
  
  items <- rbind(
    transform(inspected$positive_items, direction = "positive"),
    transform(inspected$negative_items, direction = "negative")
  )
  
  items$display <- if ("text" %in% names(items)) {
    substr(items$text, 1, 80)
  } else {
    items$item_id
  }
  
  loadings <- rbind(
    transform(inspected$positive_loadings, direction = "positive"),
    transform(inspected$negative_loadings, direction = "negative")
  )
  
  loadings$display <- paste(
    loadings$coder_id,
    loadings$label,
    sep = " -> "
  )
  
  list(
    items = ggplot2::ggplot(
      items,
      ggplot2::aes(
        x = stats::reorder(display, score),
        y = score,
        fill = direction
      )
    ) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = paste("Representative items:", axis),
        x = NULL,
        y = "Item score"
      ) +
      ggplot2::theme_minimal(base_size = 10),
    
    loadings = ggplot2::ggplot(
      loadings,
      ggplot2::aes(
        x = stats::reorder(display, loading),
        y = loading,
        fill = direction
      )
    ) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = paste("Coder-category loadings:", axis),
        x = NULL,
        y = "Loading"
      ) +
      ggplot2::theme_minimal(base_size = 10)
  )
}


qb_plot_prevalence <- function(generated) {
  qb_require("ggplot2")
  
  prevalence <- qb_prevalence(generated)
  
  summary <- aggregate(
    prevalence ~ regime + label,
    prevalence,
    mean
  )
  
  ggplot2::ggplot(
    summary,
    ggplot2::aes(
      x = regime,
      y = prevalence,
      fill = label
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::labs(
      title = "Category prevalence under coding regimes",
      x = NULL,
      y = "Mean prevalence",
      fill = "Category"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}


qb_plot_transitions <- function(generated, reference) {
  qb_require("ggplot2")
  
  transition <- qb_transitions(generated, reference)
  
  ggplot2::ggplot(
    transition,
    ggplot2::aes(
      x = reference_label,
      y = alternative_label,
      fill = Freq
    )
  ) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::facet_wrap(~ alternative) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::labs(
      title = "Paired label transitions",
      x = "Reference label",
      y = "Alternative label",
      fill = "Count"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}


qb_plot_stability <- function(generated) {
  qb_require("ggplot2")
  
  stability <- qb_stability(generated)
  
  ggplot2::ggplot(
    stability,
    ggplot2::aes(
      x = stability,
      fill = modal_label
    )
  ) +
    ggplot2::geom_histogram(bins = 20, alpha = 0.8) +
    ggplot2::labs(
      title = "Item stability across coding regimes",
      x = "Modal-label share",
      y = "Items",
      fill = "Modal label"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}


# ----------------------------------------------------------------------
# Provenance export
# ----------------------------------------------------------------------

qb_manifest <- function(fit, generated = NULL, file) {
  record <- list(
    exported_at = as.character(Sys.time()),
    codebook_id = fit$data$codebook_id,
    codebook_hash = fit$data$codebook_hash,
    labels = fit$data$labels,
    components = lapply(fit$components, function(x) {
      list(
        name = x$name,
        formula = deparse(x$formula),
        kind = x$kind,
        level = x$level,
        description = x$description
      )
    }),
    coder_weights = fit$specification$coder_weights,
    lambda = fit$lambda,
    alpha = fit$alpha,
    crossfit_log_loss = fit$crossfit_log_loss,
    crossfit_accuracy = fit$crossfit_accuracy,
    component_correlations =
      fit$specification$component_correlations,
    residual = if (!is.null(fit$residual)) {
      list(
        rank = fit$residual$rank,
        lambda = fit$residual$lambda,
        utility_scale = fit$residual$utility_scale,
        stability = fit$residual$stability,
        cross_fitted = fit$residual$cross_fitted
      )
    } else {
      NULL
    },
    regimes = if (!is.null(generated)) {
      generated$regimes
    } else {
      NULL
    },
    target = if (!is.null(generated)) {
      generated$target
    } else {
      NULL
    },
    session = utils::capture.output(sessionInfo())
  )
  
  dput(record, file = file)
  invisible(record)
}