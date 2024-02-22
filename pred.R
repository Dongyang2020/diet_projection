pred_kcal = function(st_yr,ed_yr, df_fc, B, ns, n){
  df_pred = data.frame()
  for (j in c(st_yr,ed_yr)){
    for (i in 1:B) {
      str_j = as.character(j)
      boot <- sample(n, ns, replace = TRUE)
      fit.b <- lm(value ~ year, data = df_fc[boot,])
      df_pred[i,str_j] <- predict(fit.b, list(year = j)) + sample(resid(fit.b), size = 1)
    }
    df_pred["lwr",str_j] = quantile(df_pred[1:B,str_j], c(0.05, 0.95))[1]
    df_pred["upr",str_j] = quantile(df_pred[1:B,str_j], c(0.05, 0.95))[2]
  }
  output = c(df_pred["upr",as.character(st_yr)], df_pred["upr",as.character(ed_yr)],
             df_pred["lwr",as.character(ed_yr)],df_pred["lwr",as.character(st_yr)])
  return (output)
}
