###########################################
# Functions to read plots data ------------
###########################################



get_data <-  function(ff) read.csv(ff,
                                    stringsAsFactors = FALSE)




### Growth error

growth_dead_tree <- function(i, data, yy, j){
 dbh1 <- data[data$tree_id== j &
              data$year == yy[i], 'dbh']
 dbh2 <- data[data$tree_id== j &
              data$year == yy[i+1], 'dbh']
 df <- data.frame(tree_id = j,
                  year1 = yy[i], year2 = yy[i+1],
                  dbh1 = dbh1, dbh2 = dbh2,
                  code_diam1 = data[data$tree_id== j &
                                    data$year == yy[i], 'code_diam'],
                  code_diam2 = data[data$tree_id== j &
                                    data$year == yy[i+1], 'code_diam'],
                  code_status1 = data[data$tree_id== j &
                                    data$year == yy[i], 'code_status'],
                  code_status2 = data[data$tree_id== j &
                                    data$year == yy[i+1], 'code_status']
                  )
 return(df)
}


growth_tree_all <- function(j, df){
years <- sort(df[df$tree_id== j, ]$year)
list_t <- vector('list')
list_t <- lapply(seq_len(length(years) -1), growth_dead_tree, df, years, j)
res <- do.call(rbind, list_t)
return(res)
}

save_data_growth <-  function(df, df_c){
require(parallel)
cl <- makeCluster(12, type="FORK")
trees_ids<- unique(df$tree_id)
list_all <- parLapply(cl, trees_ids, growth_tree_all, df)
stopCluster(cl)
res <- do.call(rbind, list_all)
res$G <- (res$dbh2-res$dbh1)/(res$year2-res$year1)
res$same_code_diam <-  res$code_diam1 == res$code_diam2
res <- dplyr::left_join(res, df_c[, c('tree_id', 'code_species')], by = 'tree_id')
write.csv(res, file.path('output', 'df_growth.csv'), row.names = FALSE)
}

get_data_growth <-  function(){
read.csv(file.path('output', 'df_growth.csv'))
}



cook_outlier_detec <- function(df, x, y){
 require(MASS)
 df <-  df[complete.cases(df[, c(x, y)]), ]
 fr <- as.formula(paste(y, " ~ ", x))
 ols <- lm(fr, df)
 d1 <- cooks.distance(ols)
 r <- stdres(ols)
 a <- cbind(df, d1, r)
 a_out <- a[d1 > 6*mean(d1), ]
 points(a_out[[x]], a_out[[y]], pch = 4)
 return(a_out$tree_id)
}


plot_quant_reg <- function(df, x, y,
                           probs_vec = c(0.005, 0.995),
                           smooth_dg = 3){
 require(quantreg)
 require(splines)
 df <-  df[complete.cases(df[, c(x, y)]), ]
 x_seq_pred <- seq(from = min(df[[x]], na.rm = TRUE),
                   to = max(df[[x]], na.rm = TRUE),
                   length.out = 100)
 fr <- as.formula(paste(y, " ~ ", paste("bs(", x, ", df = smooth_dg)")))
 df_pred <- data.frame( 0, x_seq_pred)
 names(df_pred) <- c(y, x)
 X <- model.matrix(fr, df_pred)
 for(tau in probs_vec){
         fit <- rq(fr, tau, data=df)
         accel.fit <- X %*% fit$coef
         lines(x_seq_pred,accel.fit, col = 'black')
         if(tau == probs_vec[1]){
         vec_pb <- df[[y]] < predict(fit)
         }else{
         vec_pb <- df[[y]] > predict(fit)
         }
         points(df[vec_pb, x], df[vec_pb, y], pch = 16, col = 'red')
         df[[paste0('tau',tau)]] <- vec_pb
         }
 return(df$tree_id[apply(df[, paste0('tau', probs_vec)], MARGIN = 1, sum)>0])
}

plot_growth_error <-  function(df){
 plot(df$dbh1, df$G, cex = 0.2,
      xlab = 'Intial DBH (cm)', ylab = 'Diameter Growth (cm/yr.)')
 quant_id <- plot_quant_reg(df, 'dbh1', 'G')
 cook_id <- cook_outlier_detec(df, 'dbh1', 'G')
}


save_growth_error <-  function(df){
 plot(df$dbh1, df$G, cex = 0.2,
      col = c('green', 'black')[unclass(factor(df$same_code_diam))],
      xlab = 'Intial DBH (cm)', ylab = 'Diameter Growth (cm/yr.)')
 abline(h = quantile(df$G, probs = c(0.0025, 0.9975), na.rm = TRUE),
        col = 'gray')
 quant_id <- plot_quant_reg(df, 'dbh1', 'G')
 cook_id <- cook_outlier_detec(df, 'dbh1', 'G')
 all_id <- c(as.character(quant_id), as.character(cook_id))
 write.csv(data.frame(tree_id = df[df$tree_id %in% all_id[duplicated(all_id)], ]),
           file = file.path('output', 'tree_wrong_growth.csv'),
           row.names = FALSE)
}




# plots allo
plot_allo_error <- function(data, df_c){
 data <- dplyr::left_join(data, df_c[, c('tree_id', 'code_species')], by = 'tree_id')
 par(mfrow = c(2, 2))
 plot(data$dbh, data$h_tot, xlab = 'dbh', ylab = 'h', cex = 0.3)
 plot_quant_reg(data, 'dbh', 'h_tot')
 cook_outlier_detec(data, 'dbh', 'h_tot')
 data$crown_r <- apply(data[ , paste0('crown_r', 1:4)],
                       MARGIN = 1, mean, na.rm = TRUE)
 plot(data$dbh, data$crown_r,
      xlab = 'dbh', ylab= 'crown radius',
      cex = 0.3)
 plot_quant_reg(data, 'dbh', 'crown_r')
 cook_outlier_detec(data, 'dbh', 'crown_r')
 data$crown_h <- apply(data[ , paste0('crown_h', 1:4)],
                       MARGIN = 1, mean, na.rm = TRUE)
 vec_pb <- data$h_tot/data$crown_h<1
 plot(data$h_tot, data$crown_h,
      xlab = 'h', ylab= 'crown height',
      col = unclass(factor(vec_pb & !is.na(vec_pb))),
      cex = 0.3)
 plot_quant_reg(data, 'h_tot', 'crown_h')
 cook_outlier_detec(data, 'h_tot', 'crown_h')
 lines(0:100, 0:100, col = 'red')
}


save_allo_error <-  function(data){
 plot(data$dbh, data$h_tot, xlab = 'dbh', ylab = 'h', cex = 0.3,
      col = unclass(factor(data$plot_id)))
 abline(h=50)
 quant_id_1<- plot_quant_reg(data, 'dbh', 'h_tot')
 cook_outlier_detec(data, 'dbh', 'h_tot')
 data$crown_r <- apply(data[ , paste0('crown_r', 1:4)],
                       MARGIN = 1, mean, na.rm = TRUE)
 plot(data$dbh, data$crown_r,
      xlab = 'dbh', ylab= 'crown radius',
      cex = 0.3)
 quant_id_2 <- plot_quant_reg(data, 'dbh', 'crown_r')
 cook_outlier_detec(data, 'dbh', 'crown_r')
 vec_pb <- data$h_tot/apply(data[ , paste0('crown_h', 1:4)],
                             MARGIN = 1, mean, na.rm = TRUE)<1
 outlier_3 <- data$tree_id[vec_pb & !is.na(vec_pb)]
 write.csv(data.frame(tree_id = unique(c(quant_id_1, quant_id_2, outlier_3))),
           file = file.path('output', 'tree_wrong_allo.csv'),
           row.names = FALSE)

 vec_pb <- (data$h_tot>50 & !is.na(data$h_tot)) | (data$crown_r>7 & !is.na(data$crown_r))
 d <- data$tree_id[ vec_pb & !is.na(vec_pb)]
 print(dim(d))
 write.csv(d, file.path('output', 'data_wrong_allo2.csv'), row.names = FALSE)
}


## Tables for data paper

# TABLE 1 PLOT DESCRIPTION plot_id (check diff with plot_id) area, elvation, lat, long,

table_plot <- function(df_p){
table_p <- df_p[, c('plot_id', 'area', 'elevation', 'slope', 'aspect', 'lat',
                    'long', 'MAT', 'MAP', 'geol')]
write.csv(table_p, file.path('output', 'table_plot.csv'), row.names = FALSE)
}


# TABLE 2 plot_id year_first_meas N census, main species, N initial G initial

table_stand_descrip<- function(df_p, df_m, df_c, treshold_sp= 0.1){
require(dplyr)
table_p2 <- df_p[, c("plot_id", "area")]
df <- dplyr::left_join(df_m, df_c[, c('tree_id', 'code_species')], by = 'tree_id')
table_p3 <- df %>% dplyr::group_by(plot_id) %>%
                dplyr::summarise(first_year = min(year),
                          n_census = n_distinct(year))
df <- df %>% dplyr::filter(code_status %in% c('0', '8881', '8882')) %>%
         dplyr::arrange(year) %>% dplyr::distinct(tree_id, .keep_all = TRUE)
main_sp <- tapply(df$code_species,
                  df$plot_id,
                  function(x) paste(names(table(x))[table(x)/
                                                    length(x)>treshold_sp],
                                    collapse = ' and '))
n_init<- tapply(df$code_species,
                df$plot_id,
                length)
ba_init<- tapply(pi*df$dbh^2/4,
                df$plot_id,
                sum)
table_p4 <- data.frame(plot_id = names(main_sp),
                       main_sp = main_sp,
                       n_init = n_init, ba_init = ba_init,
                       stringsAsFactors = FALSE)
tab <- dplyr::left_join(table_p2, table_p3, by = 'plot_id')
tab <- dplyr::left_join(tab, table_p4, by = 'plot_id')
tab$ba_init <- tab$ba_init/(tab$area * 10000)
tab$n_init <- tab$n_init/(tab$area)
write.csv(tab, file.path('output', 'table_stand_descrip.csv'), row.names = FALSE)
}



table_stand_allo<- function(df_p, df_m, df_c){
require(dplyr)
df_m <- df_m %>% dplyr::rowwise()  %>%
    dplyr::mutate(crown_h = mean(c(crown_h1, crown_h2,
                            crown_h3, crown_h4),
                          na.rm = TRUE),
           crown_r = mean(c(crown_r1, crown_r2,
                            crown_r3, crown_r4),
                          na.rm = TRUE))
tab1 <- df_m %>% dplyr::group_by( plot_id) %>%
    dplyr::summarise(n_h = sum(!is.na(h_tot)),
              n_crown_h = sum(!is.na(crown_h)),
              n_crown_r = sum(!is.na(crown_r)))
df <- df_m %>%
         dplyr::arrange(year) %>% dplyr::distinct(tree_id, .keep_all = TRUE)
tab2 <- df %>% dplyr::group_by(plot_id) %>%
    dplyr::summarise(dead_init_tf = sum(code_status %in% c("9991", "9990"))>0)

tab3 <- df_c%>% dplyr::group_by(plot_id) %>%
    dplyr::summarise(xy_tf = sum(!is.na(x))>0,
                     n_year_1m30 = sum(!is.na(year_1m30)))

tab <- dplyr::left_join(tab1, tab2,  by = 'plot_id')
tab <- dplyr::left_join(tab, tab3,  by = 'plot_id')
write.csv(tab, file.path('output', 'table_stand_allo.csv'), row.names = FALSE)
}

#
