test_that("evaluate: general test", {
  ## sample data
  set.seed(123)
  data <- draw_data_roc(n=400,
                        prev=c(0.25, 0.75),
                        m =4,
                        auc=c(0.9, 0.95),
                        e = 10,
                        modnames = LETTERS[1:4])
  
  ## problem parameters:
  contrast <- define_contrast("raw")
  benchmark <- c(0.85, 0.80)
  alpha <- 0.05
  
  # settings grid:
  S1 <- expand.grid(
    alternative = c("greater", "two.sided"),
    adjustment = c("none", "bonferroni", "maxt", "bootstrap", "mbeta"),
    transformation = c("none", "logit"),
    analysis = c("full", "co-primary"),
    regu = c("0_0_0", "1_0.5_0.25"),
    pars = list(list(nboot = 100)),
    stringsAsFactors = FALSE
  ) %>% 
    dplyr::filter(!(adjustment=="mbeta" & transformation=="logit"))
  
  S2 <- expand.grid(
    alternative = c("greater", "two.sided"),
    adjustment = c("bootstrap"),
    transformation = c("none", "logit"),
    analysis = c("full", "co-primary"),
    regu = "2_1_0.5",
    pars = expand.grid(nboot = 100,
                       type = "wild",
                       dist = c("Normal", "Rademacher"),
                       res_tra = 0:3,
                       stringsAsFactors = FALSE) %>% split(seq(8)),
    stringsAsFactors = FALSE
  )
  
  S <- rbind(S1, S2)
  
  ## run tests
  msg <- TRUE
  for(i in 1:nrow(S)){
    
    if(msg){
      message(">>> evaluate() test ", i, " out of ", nrow(S), ":")
      print(S[i,])
    }
    
    results <- evaluate(data,
                        contrast = contrast,
                        benchmark = benchmark, 
                        alpha = alpha, 
                        alternative = S$alternative[i], 
                        adjustment = S$adjustment[i],
                        transformation = S$transformation[i],
                        analysis = S$analysis[i],
                        regu = S$regu[i],
                        pars = S$pars[[i]])
    
    # check if result is of appropriate class:
    expect_s3_class(results, "cases_results")
    
    # check
    
    if(i==1){
      print(results)
    }
    
  }
})
