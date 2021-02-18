if (!("pacman" %in% installed.packages()[,])) {
  install.packages("pacman")
}
pacman::p_load(tidyverse, mice, miceadds)

# data.frame with missing data example
df <- data.frame(id = c(1, 2, 3, 4, 5),
                 x = c(46, NA, 23, 24, 47),
                 y = c(NA, 43, 36, 21, 49), 
                 z1 = c(16, 28, 36, NA, 42),
                 z2 = c(33, 45, NA, 16, 2),
                 z3 = c(34, 18, 39, 4, NA)) %>%
  # if your model will have interactions you should include those in the dataset for imputation
  mutate(x = scale(x, center = TRUE, scale = FALSE),
         y = scale(y, center = TRUE, scale = FALSE),
         across(c("x", "y"), .fns = as.numeric)) %>%
  rowwise() %>%
  mutate(x.y = x*y) %>%
  as.data.frame() %>%
  # you will want your data in a long format if it isn't already
  pivot_longer(., cols = 4:6, names_to = "type", values_to = "value")

# set id as a grouping variable 
ini <- mice(df, maxit = 0, pri = FALSE)
pred <- ini$pred
pred[,c("id")] -2

# set multilevel imputation method
meth <- ini$meth
meth[c("value")] <- "2l.norm"
meth[c("x", "y", "x.y")] <- "pmm"
meth[c("type")] <- ""

# imputing missing values getting 25 datasets, 50 dataframes within each 
df_imputed <- mice(df, m = 25, maxit = 50, method = meth, seed = 500, pred=pred)
summary(df_imputed)

# include all data frames into one - this is useful for visualization purposes
## do not be misled and use the complete alone as it will actually only give you one dataset
df_imputed2 <- lapply(1:25, function(i) complete(df_imputed, action = i))
df_imputed_complete <- datalist2mids(df_imputed2) 

# example model
m <- with(df_imputed, lm(z ~ x*y))
summary(est <- pool(m))