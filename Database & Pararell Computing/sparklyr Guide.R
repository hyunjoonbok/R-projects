## sparklyr: R interface for Apache Spark
require(sparklyr)
spark_install(version = "2.1.0")
spark_installed_versions()

# Connecting to Spark in local machine
# It provides a remote dplyr data source to the Spark cluster
conf$sparklyr.cores.local <- 4
conf$sparklyr.shell.driver-memory <- "8G"
conf$spark.memory.fraction <- 0.6

sc <- spark_connect(master = "local", version = "2.1.0")

# Using Spark and R inside a Hadoop based Data Lake is becoming a common practice at companies. 
# Currently, there is no good way to manage user connections to the Spark service centrally. 
# There are some caps and settings that can be applied, but in most cases there are configurations that the R user will need to customize.

conf <- spark_config()

conf$spark.executor.memory <- "300M"
conf$spark.executor.cores <- 2
conf$spark.executor.instances <- 3
conf$spark.dynamicAllocation.enabled <- "false"

sc <- spark_connect(master = "yarn-client",
                    version = "2.1.0",
                    config = conf)

# Using Dplyr
# and copying some datasets from R into the Spark cluster
require(nycflights13)
require(Lahman)
require(dplyr)
iris_tbl <- copy_to(sc, iris); iris_tbl
flights_tbl <- copy_to(sc, nycflights13::flights, "flights")
batting_tbl <- copy_to(sc, Lahman::Batting, "batting")
src_tbls(sc)


# Simple filtering example
flights_tbl %>% filter(dep_delay == 2)

delay <- flights_tbl %>% 
  group_by(tailnum) %>%
  summarise(count = n(), dist = mean(distance), delay = mean(arr_delay)) %>%
  filter(count > 20, dist < 2000, !is.na(delay)) %>%
  collect
delay

# plot delays
require(ggplot2)
ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area(max_size = 2)

# SQL query in tables within Spark cluster
require(DBI)
iris_preview <- dbGetQuery(sc, "SELECT * FROM iris LIMIT 10")
iris_preview

# Distributed R
## Execute random R code using "spark_apply"
spark_apply(iris_tbl, function(data) {
  data[1:4] + rgamma(1,2)
})


# Machine Learning
iris_tbl <- copy_to(sc, iris, "iris", overwrite = TRUE)
iris_tbl

## 1. Linear Regression
lm_model <- iris_tbl %>%
  select(Petal_Width, Petal_Length) %>%
  ml_linear_regression(Petal_Length ~ Petal_Width)

iris_tbl %>%
  select(Petal_Width, Petal_Length) %>%
  collect %>%
  ggplot(aes(Petal_Length, Petal_Width)) +
  geom_point(aes(Petal_Width, Petal_Length), size = 2, alpha = 0.5) +
  geom_abline(aes(slope = coef(lm_model)[["Petal_Width"]],
                  intercept = coef(lm_model)[["(Intercept)"]]),
              color = "red") +
  labs(
    x = "Petal Width",
    y = "Petal Length",
    title = "Linear Regression: Petal Length ~ Petal Width",
    subtitle = "Use Spark.ML linear regression to predict petal length as a function of petal width."
  )

### Good Spark's Machine learning example workflow (using linear regression) that can be applied for below 

# 1. Copy data into Spark
mtcars_tbl <- copy_to(sc, mtcars, "mtcars")
mtcars_tbl

# 2. Transform the data with Spark SQL, feature transformers, and DataFrame functions
# transform our data set, and then partition into 'training', 'test'
partitions <- mtcars_tbl %>%
  filter(hp >= 100) %>%
  sdf_mutate(cyl8 = ft_bucketizer(cyl, c(0,8,12))) %>% # Use Spark feature transformers to bucket cars into two groups based on cylinders
  sdf_partition(training = 0.7, test = 0.3, seed = 0623) # Use Spark DataFrame functions to partition the data into test and training
partitions
# 3. fit a linear mdoel to the training dataset
fit <- partitions$training %>%
  ml_linear_regression(mpg ~ wt + cyl); fit
summary(fit)

# 4. Let's use our Spark model fit to predict the average fuel consumption on our test data set, and compare the predicted response with the true measured fuel consumption

# Score the data
pred <- sdf_predict(fit, partitions$test) %>%
  collect()

# Plot the predicted versus actual mpg
ggplot(pred, aes(x = mpg, y = prediction)) +
  geom_abline(lty = "dashed", col = "red") +
  geom_point() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_fixed(ratio = 1) +
  labs(
    x = "Actual Fuel Consumption",
    y = "Predicted Fuel Consumption",
    title = "Predicted vs. Actual Fuel Consumption"
  )



## 2. Logistic Regression
beaver <- beaver2
beaver$activ <- factor(beaver$activ, labels = c("Non-Active", "Active"))
copy_to(sc, beaver, "beaver")

beaver_tbl <- tbl(sc, "beaver")

glm_model <- beaver_tbl %>%
  mutate(binary_response = as.numeric(activ == "Active")) %>%
  ml_logistic_regression(binary_response ~ temp); glm_model


## 3. PCA
pca_model <- tbl(sc, "iris") %>%
  select(-Species) %>%
  ml_pca(); pca_model

## 4. Random Forest
rf_model <- iris_tbl %>%
  ml_random_forest(Species ~ Petal_Length + Petal_Width, type = "classification"); rf_model

rf_predict <- sdf_predict(rf_model, iris_tbl) %>%
  ft_string_indexer("Species", "Species_idx") %>%
  collect()

table(rf_predict$Species_idx, rf_predict$prediction)

## 5. FT STRING INDEXING
# Use ft_string_indexer and ft_index_to_string to convert a character column into a numeric column and back again.
ft_string2idx <- iris_tbl %>%
  ft_string_indexer("Species", "Species_idx") %>%
  ft_index_to_string("Species_idx", "Species_remap") %>%
  collect

table(ft_string2idx$Species, ft_string2idx$Species_remap)
