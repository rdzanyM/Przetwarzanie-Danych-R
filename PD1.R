#setwd("C:/Users/Michal/Desktop/R/files")
#Tags <- read.csv("Tags.csv")
#Posts <- read.csv("Posts.csv")
#Users <- read.csv("Users.csv")


#install.packages("sqldf")
#library(sqldf)


#Dlaczego Posts.Title dziala???
sqldf::sqldf("SELECT Users.DisplayName,
             Users.Id,
             Users.Location,
             SUM(Posts.FavoriteCount) AS FavoriteTotal,
             Posts.Title AS MostFavoriteQuestion,
             MAX(Posts.FavoriteCount) AS MostFavoriteQuestionLikes
             FROM Posts
             JOIN Users ON Users.Id=Posts.OwnerUserId
             WHERE Posts.PostTypeId=1
             GROUP BY OwnerUserId
             ORDER BY FavoriteTotal DESC
             LIMIT 10")






#Zwraca 10 pytań (id, tytuł, pac) z największą liczbą odpowiedzi o dodatnim wyniku na dane pytanie (oznaczoną jako pac).
sqldf::sqldf("SELECT Posts.ID,
             Posts.Title,
             Posts2.PositiveAnswerCount
             FROM Posts
             JOIN (
               SELECT
               Posts.ParentID,
               COUNT(*) AS PositiveAnswerCount
               FROM Posts
               WHERE Posts.PostTypeID=2 AND Posts.Score>0
               GROUP BY Posts.ParentID
             ) AS Posts2
             ON Posts.ID=Posts2.ParentID
             ORDER BY Posts2.PositiveAnswerCount DESC
             LIMIT 10")
#basic
p <- subset(Posts, PostTypeId == 2 & Score > 0)
p <- aggregate(p$ParentId, p["ParentId"], length)
colnames(p) <- c("ParentId", "PositiveAnswerCount")
p <- merge(Posts, p, by.x = "Id", by.y = "ParentId")[c("Id", "Title", "PositiveAnswerCount")]
head(p[order(-p$PositiveAnswerCount),], 10)

#..

#..





