if (!("pacman" %in% installed.packages()[,])) {
  install.packages("pacman")
}
pacman::p_load(tidyverse, mice, miceadds, lme4, lmerTest, optimx)

# data.frame with missing data example
df <- data.frame(id = c(1, 2, 3, 4, 5),
                 x = c(46, NA, 23, 24, 47),
                 y1 = c(16, 28, 36, NA, 42),
                 y2 = c(33, 45, NA, 16, 2),
                 y3 = c(34, 18, 39, 4, NA),
                 y4 = c(NA, 26, 47, 19, 24)) %>%
  # you will want your data in a long format if it isn't already
  pivot_longer(., cols = 3:6, names_to = "type", values_to = "value") %>%
  # if your model will have interactions you should include those in the dataset for imputation
  mutate(x = as.numeric(scale(x, center = TRUE, scale = FALSE)),
         type = as.numeric(gsub("[^0-9.-]", "", type))) %>%
  rowwise() %>%
  mutate(x.y = x*type) %>%
  as.data.frame() 

# set id as a grouping variable 
ini <- mice(df, maxit = 0, pri = FALSE)
pred <- ini$pred
pred[, "id"] <- -2

# set multilevel imputation method
meth <- ini$meth
meth[c("value")] <- "2l.norm"
meth[c("x", "x.y")] <- "pmm"
meth[c("type")] <- ""

# imputing missing values getting 25 datasets, 50 dataframes within each 
df_imputed <- mice(df, m = 25, maxit = 50, method = meth, seed = 500, pred=pred)
summary(df_imputed)

# include all data frames into one - this is useful for visualization purposes
## do not be misled and use the complete alone as it will actually only give you one dataset
df_imputed2 <- lapply(1:25, function(i) complete(df_imputed, action = i))
df_imputed_complete <- datalist2mids(df_imputed2) 

# example model
m <- with(df_imputed, lmer(value ~ x*type + (1 | id))) 
pool(m)
summary(est)
