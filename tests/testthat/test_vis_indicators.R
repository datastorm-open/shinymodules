context("vis_indicators")

# data
v_1 <- c(0.09822715, 0.44045708, 0.80693429, 0.94263754, 0.49243824, 0.43379539, 0.80545231, 0.38238803, 0.80566838, 0.77344846) # runif(1:10)
v_2 <- c(0.51462944, 0.22276735, 0.51794175, 0.16683729, 0.09125272, 0.58646399, 0.58923419, 0.10818732, 0.13619656, 0.45254192) # runif(1:10)

rmse_target <- 0.4199440147 # Metrics::rmse(v_1, v_2)
mae_target <- 0.373353612 # Metrics::mae(v_1, v_2)
mape_target <- 0.9312561881 # Metrics::mape(v_1, v_2)
mape_star_target <- 0.624186121 # 

test_that("check sub-functions", {
  expect_equal(rmse_target, round(shinymodules:::.rmse(v_1, v_2), 10))
  expect_equal(mae_target, round(shinymodules:::.mae(v_1, v_2), 10))
  expect_equal(mape_target, round(shinymodules:::.mape(v_1, v_2), 10))
  expect_equal(mape_star_target, round(shinymodules:::.mape_e(v_1, v_2), 10))
  
  expect_equivalent(data.frame(mape = mape_target, rmse = rmse_target, mea = mae_target, mape_e = mape_star_target), 
                    round(shinymodules:::compute_idc(data = data.table(obs = v_1, fit = v_2), 
                                                     col_obs = "obs", 
                                                     col_fit = "fit", 
                                                     by = "Aucun",
                                                     dec = 10), 10))
  
  expect_equivalent(
    data.frame(data.table(
            obs = v_1, fit = v_2, by = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2))[, 
            list(mape = .mape(obs, fit), rmse = .rmse(obs, fit),
                 mae = .mae(obs, fit), mape_e = .mape_e(obs, fit)), by = by]), 
    shinymodules:::compute_idc(data = data.table(obs = v_1, fit = v_2, by_col = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2)), 
                               col_obs = "obs", 
                               col_fit = "fit", 
                               by = "by_col",
                               dec = 10))

  expect_equal(data.table(obs = v_1, fit = v_2, by_var = as.character(c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2))),
               add_by(data = data.table(obs = v_1, fit = v_2, by_var = as.character(c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2))), by_col = "Aucun"))
    
  expect_equal(data.table(obs = v_1, fit = v_2, by_var = as.character(c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2))),
               add_by(data = data.table(obs = v_1, fit = v_2, by_var = as.character(c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2))), by_col = "by_var"))
  
  expect_equal(data.table(obs = v_1, fit = v_2, by_var = factor(c("[0.5,1]", "[0.5,1]", "(1,1.5]", "(1.5,2.5]", "(1.5,2.5]", "(2.5,3]", "(3,4]", "(3,4]", "(4,4.5]", "(4.5,5]"),
                                                                levels = c("[0.5,1]", "(1,1.5]", "(1.5,2.5]", "(2.5,3]", "(3,4]", "(4,4.5]", "(4.5,5]"))),
               add_by(data = data.table(obs = v_1, fit = v_2, by_var = seq(0.5, 5, length.out = 10)), by_col = "by_var", nb_quantiles = 7))
  
})