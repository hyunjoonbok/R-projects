# Twitter (Social Media) Analysis and Visualization


## [Twitter] ==========================================================================================## 
require(twitteR)
require(rtweet)
require(ggplot2)
require(dplyr)

appname <- "Direct2Drive"
api_key <- "NhQedPz0zvKa3Zd2hhyhOLFSy"
api_secret <- "rbrwm86yzYbuuxb0uh6jUqJC9H1C7O3u5jr2e7rQXxLDZ9nTyw"
token <- "21245956-IpjXsPiY6SFDqRSmeq3yc8kcbU1i2FjoAAPb0ILva"
token_secret <- "UTSEW7iGz6Y5K4f5B76JHAgcx1MxZg3Z9jJqJtgUjYvfE"

setup_twitter_oauth(api_key, api_secret, token, token_secret)

# Serach tweets
MyTweets <- searchTwitter("#sale", lang = "en")


# OR

## create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = api_key,
  consumer_secret = api_secret)


# Search tweets up to 5000 tweets that has D2D
rt <- search_tweets(
  "D2D, Direct2Drive", n = 500, include_rts = FALSE, retryonratelimit = TRUE)

## plot time series of tweets
ts_plot(rt, "12 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) 




# 2. Search by geo-location

## search for 10,000 tweets sent from the US
rt <- search_tweets(
  "lang:en", geocode = lookup_coords("usa"), n = 500, retryonratelimit = TRUE
)

## create lat/lng variables using all available tweet and profile geo-location data
rt <- lat_lng(rt)

## plot state boundaries
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)

## plot lat and lng points onto state map
with(rt, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))



# 3. Live streaming tweets

## Stream keywords used to filter tweets
q <- "hillaryclinton,imwithher,realdonaldtrump,election,obama"

## Stream time in seconds so for one minute set timeout = 60
## For larger chunks of time, I recommend multiplying 60 by the number
## of desired minutes. This method scales up to hours as well
## (x * 60 = x mins, x * 60 * 60 = x hours)
## Stream for 30 minutes
streamtime <- 60

## Filename to save json data (backup)
filename <- "rtelect.json"

## Stream election tweets
rt <- stream_tweets(q = q, timeout = streamtime, file_name = filename)

## Preview users data
users_data(rt)

## Plot time series of all tweets aggregated by second
ts_plot(rt, by = "secs")

## plot multiple time series by first grouping the data by screen name
rt %>%
  dplyr::group_by(screen_name) %>%
  ts_plot()
  )


#4. Retrieve a list of all the accounts a user follows.
## get user IDs of accounts followed by CNN
fds <- get_friends("Direct2Drive")

## lookup data on those accounts
fds_data <- lookup_users(fds$user_id)


#5. Retrieve a list of the accounts following a user
## get user IDs of accounts following CNN
flw <- get_followers("Direct2Drive", n = 10000)

## lookup data on those accounts
flw_data <- lookup_users(flw$user_id)


#6. Get the most recent 1,000 tweets from cnn and foxnews.
## get user IDs of accounts followed by CNN
tmls <- get_timelines(c("cnn", "foxnews"), n = 1000)

## plot the frequency of tweets for each user over time
tmls %>%
  dplyr::filter(created_at > "2017-10-29") %>%
  dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by news organization",
    subtitle = "Twitter status (tweet) counts aggregated by day from October/November 2017",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )