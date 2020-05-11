# Use H2o to predict arrival delay using historical airline data with Destination to Chicago Airport


# Load data
require(h2o)
h2o.init(nthreads = -1)

#### Predict Airlines Delays ####

# Define Data paths
flights_hex <- h2o.importFile(path = "C:/Users/bokhy/Desktop/R/h2o/flights.csv", destination_frame = "flights_hex")
weather_hex <- h2o.importFile(path = "C:/Users/bokhy/Desktop/R/h2o/weather.csv", destination_frame = "weather_hex")

# Summary
dim(flights_hex)
h2o.describe(flights_hex)
h2o.describe(weather_hex)
h2o.hist(x = flights_hex$Year)

# Plot flights over the years using user-defined function
flights_hex$IsArrDelayedNumeric <- ifelse(flights_hex$IsArrDelayed == "YES", 1,0)
flights_hex$IsWeatherDelayedNumeric <- ifelse(flights_hex$WeatherDelay > 0, 1,0)
#dplyr
flights_count <- h2o.group_by(data= flights_hex, by = "Year", nrow("Year"), sum("IsArrDelayedNumeric"),sum("IsWeatherDelayedNumeric"));flights_count
#bring it back to R
flights_count_df <- as.data.frame(flights_count)
flights_count_df2 <- t(flights_count_df[,2:4])
colnames(flights_count_df2) <- flights_count_df$Year
flights_count_df2
# barplot
barplot(flights_count_df2, beside = T, col = c("dark blue", "red", "purple"))
# Filter flights before 2003 (because no pueple things)
flights_hex <- flights_hex[flights_hex$Year > 2003,]
dim(flights_hex)
# Filter flights delayed, but not delayed by weather
flights_hex <- flights_hex[(flights_hex[, "IsArrDelayed"] == "NO") |(flights_hex[, "WeatherDelay"] >0), ]
# Weather delay only happens 2.17% of the time
responseCount = as.data.frame(h2o.table(flights_hex$IsArrDelayed))
print("Total number of flights in dataset...")
print(prettyNum(nrow(flights_hex), big.mark = ","))
print("Number of flights delayed arriving in Chicago due to weather...")
print(prettyNum(responseCount[2,2], big.mark = ","))

# Parameter Creation
hour1 <- flights_hex$CRSArrTime %/% 100; hour1 # separate hour
mins1 <- flights_hex$CRSArrTime %% 100; mins1 # separate min
arrTime <- hour1*60 + mins1;arrTime # arrival time in minutes

hour2 <- flights_hex$CRSDepTime %/% 100; hour2
mins2 <- flights_hex$CRSDepTime %% 100; mins2
depTime <- hour2*60 + mins2

travelTime <- ifelse(arrTime - depTime > 0, arrTime - depTime, NA)
flights_hex$TravelTime <- travelTime
flights_hex


## Build ML Model =====
# Subset frame down to predictors and response
myY = "IsArrDelayed"
myX = c("Year","Month","DayofMonth","DayOfWeek", "UniqueCarrier","Origin","TravelTime")

# Split frame into test/train
splitt = h2o.splitFrame(data = flights_hex[,c(myY,myX)], destination_frames = c("train_hex", "valid_hex"));splitt
train = splitt[[1]]
valid = splitt[[2]]

# Build GLM model
arr_delay_glm = h2o.glm(x = myX, y= myY, training_frame = train, validation_frame = valid, family = "binomial", alpha = 0.5, lambda_search = T)

# Build GBM model
arr_delay_gbm = h2o.gbm(x = myX, y= myY, training_frame = train, validation_frame = valid, distribution = "bernoulli", ntrees = 50)

## Build Random Forest Model
arr_delay_RF <- h2o.randomForest(y = myY, x = myX, training_frame = train, validation_frame = valid, ntrees = 100,
                             max_depth = 5, model_id = "drf_model", balance_classes = T)
## Build Deep Learning Model
arr_delay_DL <- h2o.deeplearning(y = myY, x = myX, training_frame = train, validation_frame = valid, hidden=c(10, 10),
                            epochs = 5, balance_classes = T, loss = "Automatic", variable_importances = T)

# Report AUC on above models
glm_model = arr_delay_glm
gbm_model = arr_delay_gbm
auc = data.frame(GLM_AUC = c(h2o.auc(glm_model, train = T), h2o.auc(glm_model, valid = T)),
                 GBM_AUC = h2o.auc(gbm_model, train = T), h2o.auc(gbm_model, valid = T))
row.names(auc) = c("Training set", "Validation set")
auc


h2o.shutdown()
