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
  alpha <- 0.025
  alternative <- c("greater")
  
  ## analysis parameters:
  adjustment <- c("none", "bonferroni", "maxt", "bootstrap", "mbeta")
  transformation <- c("none")
  analysis <- c("co-primary")
  regu <- c(TRUE)
  pars <- list(nboot = 1000)
  
  ## run tests
  for(adj in adjustment){
    
    results <- evaluate(data,
                        contrast = contrast,
                        benchmark = benchmark, 
                        alpha = alpha, 
                        alternative = alternative, 
                        adjustment = adj,
                        transformation = transformation,
                        analysis = analysis,
                        regu = regu,
                        pars = pars)
    
    expect_s3_class(results, "cases_results")
    
  }
})
