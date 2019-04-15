library(ggplot2)
library(naniar)
#---------------------------- 1: DOWNLOADING FILE --------------------------------#

# Getting data from website
if(!file.exists("./data")){dir.create("./data")}
url <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/00366/AReM.zip'
download.file(url, destfile = './data/AReM.zip',method='curl' )
unzip('./data/AReM.zip',exdir = './data')

readFile <- function(file) {
      df <- read.csv(file, header = TRUE, skip = 4)
      names(df)[1] <- 'time'
      df$time <- as.integer(df$time)
      label <- gsub('^.*data/\\s*|\\s*/dataset.*$', '', file)
      df$label <- label
      df
}

folders <- list.dirs(path = ".", full.names = TRUE, recursive = TRUE)
fileNames <- NULL
activity.raw <- NULL
for(folder in folders) {
      fileNames <- Sys.glob(paste0(folder,"/*.csv"))
      inner.df = data.frame()
      for(fileName in fileNames) {
            inner.df <- rbind(inner.df,readFile(fileName))
      }
      activity.raw <- rbind(activity.raw,inner.df)
}
activity.raw$label <- as.factor(activity.raw$label)

# ---- checking data ---- #

any(is.na(activity.raw))

# found nas were because of file not being formatted correctly
# removing those lines and re-reading

activity.clean <- activity.raw[complete.cases(activity.raw), ]
bending2File <- 'data/bending2/dataset4.csv'
bending2 <- read.table(bending2File, sep = "" , header = F , skip=4,
                       na.strings ="", stringsAsFactors= T)

bending2$label = "bending2"
names(bending2) <- names(activity.clean)

activity <- rbind(activity.clean,bending2)

pl <- ggplot(activity, aes(x=avg_rss12, fill=label, color=label)) +
      geom_histogram(bins=100, position="identity", alpha=0.6)

pl

pl2 <- ggplot(activity, aes(x=avg_rss13, fill=label, color=label)) +
      geom_histogram(bins=100, position="identity", alpha=0.6)

pl2

pl3 <- ggplot(activity, aes(x=avg_rss23, fill=label, color=label)) +
      geom_histogram(bins=100, position="identity", alpha=0.6)

pl3

pl4 <- ggplot(activity, aes(x=var_rss12, fill=label, color=label)) +
      geom_histogram(bins=100, position="identity", alpha=0.6)

pl4

pl5 <- ggplot(activity, aes(x=var_rss13, fill=label, color=label)) +
      geom_histogram(bins=100, position="identity", alpha=0.6)

pl5

pl6 <- ggplot(activity, aes(x=var_rss23, fill=label, color=label)) +
      geom_histogram(bins=100, position="identity", alpha=0.6)

pl6

# --- model --- #
activityCluster <- kmeans(activity[,-8],centers = 7, nstart = 20)

table(activityCluster$cluster,activity$label)

library(cluster)
clusplot(activity,activityCluster$cluster, color = T, shade = T, labels = 0, lines = 0)