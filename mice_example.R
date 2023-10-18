if (!("pacman" %in% installed.packages()[,])) {
  install.packages("pacman")
}
pacman::p_load(tidyverse, mice, miceadds)

# data.frame with missing data example
df <- data.frame(id = c(1, 2, 3, 4, 5),
                  x = c(46, NA, 23, 24, 47),
                  y = c(NA, 43, 36, 21, 49), 
                  z = c(16, 28, 36, NA, 42)) %>%
  # if your model will have interactions you should include those in the dataset for imputation
  mutate(x = scale(x, center = TRUE, scale = FALSE),
         y = scale(y, center = TRUE, scale = FALSE),
         across(c("x", "y"), .fns = as.numeric)) %>%
  rowwise() %>%
  mutate(x.y = x*y) %>%
  as.data.frame()

# remove id so it doesn't play a role in the imputation process 
ini <- mice(df, maxit = 0, pri = FALSE)
pred <- ini$pred
pred[,c("id")] = 0

# imputing missing values getting 25 datasets, 50 dataframes within each 
df_imputed <- mice(df, m = 25, maxit = 50, method = 'pmm', seed = 500, pred=pred)
summary(df_imputed)

# include all data frames into one - this is useful for visualization purposes
## do not be misled and use the complete function alone as it will actually only give you one dataset
df_imputed2 <- lapply(1:25, function(i) complete(df_imputed, action = i))
df_imputed_complete <- datalist2mids(df_imputed2) 

# example model
m <- with(df_imputed, lm(z ~ x*y))
summary(est <- pool(m))

