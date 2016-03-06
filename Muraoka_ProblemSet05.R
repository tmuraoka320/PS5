### 
### Problem Set 05
### Taishi Muraoka
### March 10
###


##
## setup
##
rm(list=ls())

set.seed(12435)

options(stringsAsFactors=F)

library(foreign)

anes <- read.dta("C:/Users/Taishi/Documents/PS5/anes_timeseries_2012_stata12.dta")

#model1 <- lm(ft_dpc ~ ft_hclinton, anes)

## make a prediction for a single observation with hypothetical clinton score of 77
#predict(model1, data.frame(ft_hclinton=77))
## we would expect a Obama score of 71.7



##
## Q1
##

# subset the dataset to the variable of interests
df <- anes[,c("ft_dpc", "prevote_primv", "usworld_stay", "war_worthit",
              "gayrt_adopt", "penalty_favdpen", "relig_import",
              "dem3_passport")]

# insert NAs to responses "-8. Don't know" or "-9. Refused"
# obviously, it is easier and simpler to use for-loop here
df2 <- data.frame(sapply(2:dim(df)[2], function(x){
                                        vec <- df[,x] # change a column to a vector
                                        DK <- (as.character(vec)=="-8. Don't know" |
                                               as.character(vec)=="-9. Refused")
                                               # check if a value is -8 or -9
                                        vec[DK] <- NA # if yes, insert NAs
                                        return(vec)
                                       }))

df2$ft_dpc <- df$ft_dpc # add the dependent variable

colnames(df2) <- colnames(df[c(2:dim(df)[2],1)]) # change the column names

df2 <- na.omit(df2) # delete NAs

sub <- sample(1:dim(df2)[1], 373) # generate random number from 1 to 5373

train_df <- df2[!(1:dim(df2)[1] %in% sub),] # get train data (N=5000)
  
test_df <- df2[(1:dim(df2)[1] %in% sub), 1:dim(df2)[2]] # get test data (N=373)

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
p1 <- predict(m1, newdata=test_df[,1:3])

# prediction from model 2
p2 <- predict(m2, newdata=test_df[,4:7])

# prediction from model 3
p3 <- predict(m3, newdata=test_df[,1:7])

par(mfrow=c(2,2))

# plot the prediction from Model 1
plot(test_df[,8], p1, main="Model 1", xlab="Observed", ylab="Predicted", 
     xlim=c(0,100), ylim=c(0,100), pch=19, col=rgb(0.5,0.5,0.5,0.3))

lines(0:100, 0:100, lty=2)

# plot the prediction from Model 2
plot(test_df[,8], p2, main="Model 2", xlab="Observed", ylab="Predicted", 
     xlim=c(0,100), ylim=c(0,100), pch=19, col=rgb(0.5,0.5,0.5,0.3))

lines(0:100, 0:100, lty=2)

# plot the prediction from Model 3
plot(test_df[,8], p3, main="Model 3", xlab="Observed", ylab="Predicted", 
     xlim=c(0,100), ylim=c(0,100), pch=19, col=rgb(0.5,0.5,0.5,0.3))

lines(0:100, 0:100, lty=2)
# overall, model fits are pretty bad as points in the panels are not close to
# the 45 degree line



##
## Q3
##

# this function takes observed values of Y, and predicted values of Ys from
# several models. It calculates RMSE, MAD, RMSLE, MAPE, and MEAPE stats and returns
# them in a matrix form (row = model, column = fit stats).
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
  return(t(rbind(RMSE, MAD, RMSLE, MAPE, MEAPE)))
}



##
## Q4
##

# this function allows you to get fit stats that you want. The function takes, 
# 1) observed values of Y, 2) predicted values of Ys from several models, and 
# 3) five additional arguments about which stats you want to calculate. For each fit 
# stat, you can select whether you want it or not. BY defalt, the function returns 
# all of them.
select_fit_stat <- function(observed_y, predicted_y_mat, 
                            want_RMSE=TRUE, want_MAD=TRUE, want_RMSLE=TRUE, 
                            want_MAPE=TRUE, want_MEAPE=TRUE){
  want_column <- cbind(want_RMSE, want_MAD, want_RMSLE, want_MAPE, want_MEAPE)
  out <- fit_stat(observed_y, predicted_y_mat) # use the function above
  return(out[,want_column]) # subset based on want_column and return
}



##
## Q5
##

# create a matrix using p1, p2, and p3
test_mat <- as.matrix(cbind(p1, p2, p3))

fit_stat(test_df$ft_dpc, test_mat) # use the function in Q3

select_fit_stat(test_df$ft_dpc, test_mat) # use the function in Q4

select_fit_stat(test_df$ft_dpc, test_mat, F, T, T, F, F) # just want MAD and RMSLE



##
## Q6
##