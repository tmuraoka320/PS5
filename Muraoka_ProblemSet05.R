### 
### Problem Set 05
### Taishi Muraoka
### March 10
###


##
## setup
##

set.seed(12435)

options(stringsAsFactors=F)

library(foreign)

anes <- read.dta("C:/Users/Taishi/Documents/PS5/anes_timeseries_2012_stata12.dta")



##
## Q1
##

# subset the dataset to the variable of interests. The explanatory variables include:
# prevote_primv = did R vote in the Presidential primary or caucus
# usworld_stay = country would be better off if we just stayed home
# war_worthit = was war worth the cost
# gayrt_adopt = should gay and lesbian couples be allowed to adopt
# penalty_favdpen = R favor/oppose death penalty
# relig_import = is religion important part of R life
# dem3_passport = have passport
df <- anes[,c("ft_dpc", "prevote_primv", "usworld_stay", "war_worthit",
              "gayrt_adopt", "penalty_favdpen", "relig_import",
              "dem3_passport")]

# insert NAs to responses "-8. Don't know" or "-9. Refused"
# obviously, it is easier and simpler to use for-loop here
df2 <- data.frame(apply(df, 2, function(x){
                                sapply(x, function(y){
                                            y[which(y==" -2")] <- NA
                                            y[which(y==" -8")] <- NA
                                            y[which(y==" -9")] <- NA
                                            y[substr(y, start=1, stop=2)=="-8"] <- NA
                                            y[substr(y, start=1, stop=2)=="-9"] <- NA
                                              # check if a value is -2, -8, or -9
                                              # and insert NAs
                                            y # return column vales with NAs
                                          })  
                               }))

df2 <- na.omit(df2) # delete NAs (case-wise deletion)

df2$ft_dpc <- as.numeric(df2$ft_dpc) # change the dependent variable to numeric

set.seed(12435)

sub <- sample(1:dim(df2)[1], 400) # generate 400 random numbers from 1 to 5369

train_df <- df2[!(1:dim(df2)[1] %in% sub),] # get train data
  
test_df <- df2[(1:dim(df2)[1] %in% sub),] # get test data

train_df <- rbind(train_df, test_df[test_df$ft_dpc==0,]) # merge observations with
                                                         # ft_dpc==0 in the test data
                                                         # for convenience (N=5015)

test_df <- test_df[test_df$ft_dpc!=0,] # delete observations with ft_dpc==0 for
                                       # convenience (N = 354)


# first model
m1 <- lm(ft_dpc ~ prevote_primv + usworld_stay + war_worthit, data=train_df)

summary(m1)

# second model
m2 <- lm(ft_dpc ~ gayrt_adopt + penalty_favdpen + relig_import + dem3_passport,
         data=train_df)

summary(m2)

# third model
m3 <- lm(ft_dpc ~ prevote_primv + usworld_stay + war_worthit + gayrt_adopt + 
           penalty_favdpen + relig_import + dem3_passport, data=train_df)

summary(m3)



##
## Q2
##

# prediction from model 1
p1 <- predict(m1, newdata=test_df[,2:4])

# prediction from model 2
p2 <- predict(m2, newdata=test_df[,5:8])

# prediction from model 3
p3 <- predict(m3, newdata=test_df[,2:8])

par(mfrow=c(2,2))

# plot the prediction from Model 1
plot(test_df[,1], p1, main="Model 1", xlab="Observed", ylab="Predicted", 
     xlim=c(0,100), ylim=c(0,100), pch=19, col=rgb(0.5,0.5,0.5,0.3))

lines(0:100, 0:100, lty=2)

# plot the prediction from Model 2
plot(test_df[,1], p2, main="Model 2", xlab="Observed", ylab="Predicted", 
     xlim=c(0,100), ylim=c(0,100), pch=19, col=rgb(0.5,0.5,0.5,0.3))

lines(0:100, 0:100, lty=2)

# plot the prediction from Model 3
plot(test_df[,1], p3, main="Model 3", xlab="Observed", ylab="Predicted", 
     xlim=c(0,100), ylim=c(0,100), pch=19, col=rgb(0.5,0.5,0.5,0.3))

lines(0:100, 0:100, lty=2)
# overall, model fits are pretty bad as points in the panels are not close to
# the 45 degree line



##
## Q3
##

# this function takes observed values of Y (vector), and predicted values of Ys from
# several models (matrix). It calculates RMSE, MAD, RMSLE, MAPE, and MEAPE stats and 
# returns them in a matrix form (row = model, column = fit stats).
fit_stat <- function(observed_y, predicted_y_mat){
  # calculate e_i
  e_i <- apply(predicted_y_mat, 2, function(x){
                                    mapply(function(y, z){abs(y - z)}, 
                                           y=x, z=observed_y)
                                   })
  # calculate a_i
  a_i <- apply(e_i, 2, function(x){
                        mapply(function(y, z){y/abs(z)*100}, y=x, z=observed_y)
                       })
  # get n
  n <- dim(predicted_y_mat)[1]
  # calculate RMSE
  RMSE <- apply(e_i, 2, function(x){sqrt(sum(x^2)/n)})
  # calculate MAD
  MAD <- apply(e_i, 2, median)
  # calculate RMSLE
  RMSLE <- apply(predicted_y_mat, 2, function(x){
                                      inside <- mapply(function(y, z){
                                                        (log(y+1) - log(z+1))^2
                                                       }, y=x, z=observed_y)
                                      sqrt(sum(inside)/n)
                                     })
  # calculate MAPE
  MAPE <- apply(a_i, 2, function(x){sum(x)/n})
  # calculate MEAPE
  MEAPE <- apply(a_i, 2, median)
  # combine all the stats
  out_mat <- t(rbind(RMSE, MAD, RMSLE, MAPE, MEAPE))
  # change row names
  model_name <- sapply(1:dim(predicted_y_mat)[2], function(x){paste("Model", x)})
  rownames(out_mat) <- model_name
  return(out_mat)
}



##
## Q4
##

# this function allows you to get fit stats that you want. The function takes, 
# 1) observed values of Y (vector), 2) predicted values of Ys from several models
# (matrix), and 3) five additional arguments about which stats you want to calculate
# (TRUE or FALSE). For each fit stat, you can select whether you want it or not. BY 
# defalt, the function returns all of them.
select_fit_stat <- function(observed_y, predicted_y_mat, 
                            want_RMSE=TRUE, want_MAD=TRUE, want_RMSLE=TRUE, 
                            want_MAPE=TRUE, want_MEAPE=TRUE){
  want_column <- c(want_RMSE, want_MAD, want_RMSLE, want_MAPE, want_MEAPE)
                   # bind the third to seventh arguments
  out <- fit_stat(observed_y, predicted_y_mat) # use the function above
  return(out[,want_column]) # subset based on want_column and return
}



##
## Q5
##

# create a predicted Y matrix using p1, p2, and p3
test_mat <- as.matrix(cbind(p1, p2, p3))

fit_stat(test_df$ft_dpc, test_mat) # use the function in Q3

select_fit_stat(test_df$ft_dpc, test_mat) # use the function in Q4

select_fit_stat(test_df$ft_dpc, test_mat, F, T, T, F, F) # just want MAD and RMSLE



##
## Q6
##

# this function allows you to get six fit stats. The function takes, 1) observed 
# values of Y (vector), 2) predicted values of Ys from several models (matrix), 
# and 3) naive forecasts of Y (vector). It calculates RMSE, MAD, RMSLE, MAPE, MEAPE,
# and MPAE stats and returns them in a matrix form (row = model, column = fit stats).
# If a vector for naive forecasts is not assigned, it returns NA in the last column of
# the output.
fit_stat2 <- function(observed_y, predicted_y_mat, naive_forecast=NULL){
  # calculate e_i
  e_i <- apply(predicted_y_mat, 2, function(x){
                                    mapply(function(y, z){abs(y - z)}, 
                                           y=x, z=observed_y)
                                   })
  # calculate a_i
  a_i <- apply(e_i, 2, function(x){
                        mapply(function(y, z){y/abs(z)*100}, y=x, z=observed_y)
                       })
  # get n
  n <- dim(predicted_y_mat)[1]
  # calculate RMSE
  RMSE <- apply(e_i, 2, function(x){sqrt(sum(x^2)/n)})
  # calculate MAD
  MAD <- apply(e_i, 2, median)
  # calculate RMSLE
  RMSLE <- apply(predicted_y_mat, 2, function(x){
                                      inside <- mapply(function(y, z){
                                                        (log(y+1) - log(z+1))^2
                                                       }, y=x, z=observed_y)
                                      sqrt(sum(inside)/n)
                                     })
  # calculate MAPE
  MAPE <- apply(a_i, 2, function(x){sum(x)/n})
  # calculate MEAPE
  MEAPE <- apply(a_i, 2, median)
  # calculate MRAE
  if(!is.null(naive_forecast)){ # if naive_forecast is included
    MRAE <- apply(e_i, 2, function(x){
                            median(mapply(function(y, z){
                                            y/z
                                          }, y=x, z=naive_forecast))
                          })
  }
  else{ # if naive_forecast is not included
    MRAE <- c(NA, NA, NA)
  }
  # combine all the stats
  out_mat <- t(rbind(RMSE, MAD, RMSLE, MAPE, MEAPE, MRAE))
  # change row names
  model_name <- sapply(1:dim(predicted_y_mat)[2], function(x){paste("Model", x)})
  rownames(out_mat) <- model_name
  return(out_mat)
}

set.seed(12435)

naive_y <- test_df$ft_dpc - rnorm(dim(test_df)[1], 10, 0.2) # create a vector for
                                                            # naive forecast

fit_stat2(test_df$ft_dpc, test_mat, naive_y) # with a vector for naive forecast

fit_stat2(test_df$ft_dpc, test_mat) # without a vector for naive forecast