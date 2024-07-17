#' @importFrom magrittr set_rownames

test_that("evaluate: general test", {
  ## function for consistency checks:
  check_results <- function(results, alpha, benchmark) {
    checks <- lapply(1:length(results), function(g) {
      r <- results[[g]]
      data.frame(
        ci_ordering = (r$estimate > r$lower) & (r$estimate < r$upper),
        pval_bounded = all(is.na(r$pval)) | ((r$pval >= 0) & (r$pval <= 1)),
        reject_vs_ci = ((r$lower > benchmark[g]) | (r$upper < benchmark[g])) == r$reject,
        reject_vs_pval = all(is.na(r$pval)) | !((r$pval > alpha) & r$reject),
        tstat_vs_pval = all(is.na(r$pval)) | (sapply(seq_along(r$tstat), \(j){
          all(r$pval[j] >= r$pval[r$tstat > r$tstat[j]])
        }))
      ) %>%
        magrittr::set_rownames(rownames(r))
    })

    names(checks) <- names(results)
    checks$names_correct <- all(names(results) == names(data))
    alpha_adj <- attr(results, "alpha_adj")
    checks$alpha_adj_plausible <- is.na(alpha_adj) | (alpha_adj <= alpha)

    return(checks)
  }

  ## sample data:
  set.seed(123)
  data <- draw_data_roc(
    n = 400,
    prev = c(0.25, 0.75),
    m = 4,
    auc = c(0.9, 0.95),
    e = 10,
    modnames = LETTERS[1:4]
  )

  ## problem parameters:
  contrast <- define_contrast("raw")
  benchmark <- c(0.85, 0.80)
  alpha <- 0.05

  # settings grid:
  S1 <- expand.grid(
    alternative = c("greater", "two.sided"),
    adjustment = c("none", "bonferroni", "maxt", "bootstrap", "mbeta"),
    transformation = c("none", "logit", "arcsin"),
    regu = c("0_0_0", "1_0.5_0.25"),
    pars = list(list(nboot = 100)),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::filter(!(adjustment == "mbeta" & transformation != "none"))

  S2 <- expand.grid(
    alternative = c("greater", "two.sided"),
    adjustment = c("bootstrap"),
    transformation = c("none", "logit", "arcsin"),
    regu = "2_1_0.5",
    pars = expand.grid(
      nboot = 100,
      type = "wild",
      dist = c("Normal", "Rademacher"),
      res_tra = 0:3,
      stringsAsFactors = FALSE
    ) %>% split(seq(8)),
    stringsAsFactors = FALSE
  )

  S <- rbind(S1, S2)

  ## run tests
  msg <- TRUE

  for (i in 1:nrow(S)) {
    results <- list()
    checks <- list()


    if (msg) {
      message("----------------------------------------")
      message(">>> evaluate() test ", i, " out of ", nrow(S), "...")
      message(">>> inputs:")
      print(S[i, ])
    }

    for (analysis in c("full", "co-primary")) {
      set.seed(123)
      results[[analysis]] <- evaluate(data,
        contrast = contrast,
        benchmark = benchmark,
        alpha = alpha,
        alternative = S$alternative[i],
        adjustment = S$adjustment[i],
        transformation = S$transformation[i],
        analysis = analysis,
        regu = S$regu[i],
        pars = S$pars[[i]]
      )

      # check if result is of appropriate class:
      expect_s3_class(results[[analysis]], "cases_results")

      # check results for consistency:
      checks[[analysis]] <- check_results(results[[analysis]], alpha, benchmark)
    }

    # check for consistency between analysis 'co-primary' and 'full':
    checks[["between_analyses"]] <-
      lapply(names(data), \(gn){
        rf <- results[["full"]]
        rc <- results[["co-primary"]]
        data.frame(
          estimate = rf[[gn]]$estimate == rf[[gn]]$estimate,
          lower = rf[[gn]]$lower <= rf[[gn]]$lower,
          upper = rf[[gn]]$upper >= rf[[gn]]$upper,
          tstat = all(is.na(rf$tstat)) | (rf[[gn]]$tstat <= rf[[gn]]$tstat),
          pval = all(is.na(rf$pval)) | (rf[[gn]]$pval >= rf[[gn]]$pval)
        )
      })
    names(checks[["between_analyses"]]) <- names(data)

    ## message results:
    if (msg) {
      message(">>> results of consistency checks:")
      print(checks)
      message(">>> ... done!")
      message("----------------------------------------")
    }

    expect_true(all(sapply(checks, \(x) all(sapply(x, all))))) %>% stopifnot()
  }
})
