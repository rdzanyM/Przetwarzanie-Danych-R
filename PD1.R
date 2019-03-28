options(stringsAsFactors = FALSE)
setwd("C:/Users/Michal/Desktop/R/files")
Tags <- read.csv("Tags.csv")
Posts <- read.csv("Posts.csv")
Users <- read.csv("Users.csv")
Votes <- read.csv("Votes.csv")
Comments <- read.csv("Comments.csv")
Badges <- read.csv("Badges.csv")

#install.packages("sqldf")
#library(sqldf)

#1)
#Zwraca 10 u¿ytkowników, których pytania zosta³y w sumie dodane do ulubionch najwiêksz¹ iloœæ razy.
#Zwraca nazwê, id, lokacjê u¿ytkownika, sumê dodañ do ulubionych dla wszystkich zadanych przez niego pytañ,
#,pytanie tego u¿ytkownika dodane do ulubionych najwiêksz¹ liczbê razy i liczbê dodañ do ulubionych dla tego pytania
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
#basic
q <- subset(Posts, PostTypeId == 1 & !is.na(FavoriteCount))[c("OwnerUserId", "Title", "FavoriteCount")]
u_q <- merge(Users, q, by.x = "Id", by.y = "OwnerUserId")[c("DisplayName", "Id", "Location", "FavoriteCount", "Title")]
g <- do.call(data.frame, aggregate(u_q["FavoriteCount"], u_q[c("DisplayName", "Id", "Location")], function(x)
  c(sum = sum(x), max = max(x))))
g <- head(g[order(-g$FavoriteCount.sum),], 10)
g <- merge(g, q, by.x = c("Id", "FavoriteCount.max"), by.y = c("OwnerUserId", "FavoriteCount"))
g <- g[,c(3,1,4,5,6,2)]
colnames(g) <- c("DisplayName", "Id", "Location", "FavoriteTotal", "MostFavoriteQuestion", "MostFavoriteQuestionLikes")
g[order(-g$FavoriteTotal),]

#..


#..


#2)
#Zwraca 10 pytañ (id, tytu³, pac) z najwiêksz¹ liczb¹(oznaczon¹ jako pac) odpowiedzi o dodatnim wyniku na dane pytanie.
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


#3)
#Dla ka¿dego roku zwraca tytu³ pytania, które dosta³o najwiêcej upvotów w danym roku, ten rok i t¹ liczbê upVotów.
sqldf::sqldf("SELECT Posts.Title,
             UpVotesPerYear.Year,
             MAX(UpVotesPerYear.Count) AS Count
             FROM (
             SELECT
             PostId,
             COUNT(*) AS Count,
             STRFTIME('%Y', Votes.CreationDate) AS Year
             FROM Votes
             WHERE VoteTypeId=2
             GROUP BY PostId, Year
             ) AS UpVotesPerYear
             JOIN Posts ON Posts.Id=UpVotesPerYear.PostId
             WHERE Posts.PostTypeId=1
             GROUP BY Year")
#basic
uv <- subset(Votes, VoteTypeId == 2)[c("PostId", "CreationDate")]
#uv["CreationDate"] <- format(as.Date(unlist(uv["CreationDate"])), "%Y") #bardzo wolne
uv["CreationDate"] <- substr(unlist(uv["CreationDate"]), 1, 4)
uv_y <- aggregate(cbind(PostId)~PostId+CreationDate, uv, length)
colnames(uv_y) <- c("Id", "Year", "Count")
q <- subset(Posts, PostTypeId == 1)[c("Id", "Title")]
q_uv_y <- subset(merge(q, uv_y, by = "Id"), select = -Id)
g <- aggregate(Count ~ Year, data = q_uv_y, max)
g <- merge(g, q_uv_y, by = c("Year", "Count"))
g[,c(3,1,2)]

#..


#..


#4)
#Zwraca pytania z najwiêksz¹ ró¿nic¹ pomiêdzy wynikiem najwy¿ej punktowanej i zaakceptowanej odpowiedzi,
#posortowane malej¹co wzglêdem tej ró¿nicy
sqldf::sqldf("SELECT Questions.Id,
             Questions.Title,
             BestAnswers.MaxScore,
             Posts.Score AS AcceptedScore,
             BestAnswers.MaxScore-Posts.Score AS Difference
             FROM (
             SELECT Id, ParentId, MAX(Score) AS MaxScore
             FROM Posts
             WHERE PostTypeId==2
             GROUP BY ParentId
             ) AS BestAnswers
             JOIN (
             SELECT * FROM Posts
             WHERE PostTypeId==1
             ) AS Questions
             ON Questions.Id=BestAnswers.ParentId
             JOIN Posts ON Questions.AcceptedAnswerId=Posts.Id
             WHERE Difference>50
             ORDER BY Difference DESC")
#basic
q <- subset(Posts, PostTypeId == 1)[c("Id", "Title", "AcceptedAnswerId")]
a <- subset(Posts, PostTypeId == 2)[c("Id", "ParentId", "Score")]
ba <- aggregate(Score ~ ParentId, data = a, max)
q_ba <- merge(q, ba, by.x = "Id", by.y = "ParentId")
aa <- a[c("Id", "Score")]
colnames(aa) <- c("Id", "AcceptedScore")
q_ba_aa <- merge(q_ba, aa, by.x = "AcceptedAnswerId", by.y = "Id")
q_d = cbind(q_ba_aa, q_ba_aa["Score"] - q_ba_aa["AcceptedScore"])
colnames(q_d) <- c("d", "Id", "Title", "MaxScore", "AcceptedScore", "Difference")
q_d <- subset(q_d, Difference > 50, select = -d)
q_d[order(-q_d$Difference),]

#..


#..


#5)
#Zwraca 10 pytañ (tytu³ pytania, 'wynik komentarzy autora') z najwiêkszym 'wynikiem komentarzy autora',
#gdzie 'wynik komentarzy autora' to suma punktów uzyskanych przez autora pytania w komentarzach do tego pytania
sqldf::sqldf("SELECT Posts.Title,
             CmtTotScr.CommentsTotalScore
             FROM (
             SELECT
             PostID,
             UserID,
             SUM(Score) AS CommentsTotalScore
             FROM Comments
             GROUP BY PostID, UserID
             ) AS CmtTotScr
             JOIN Posts ON Posts.ID=CmtTotScr.PostID AND Posts.OwnerUserId=CmtTotScr.UserID
             WHERE Posts.PostTypeId=1
             ORDER BY CmtTotScr.CommentsTotalScore DESC
             LIMIT 10")
#basic
c <- Comments[c("PostId", "UserId", "Score")]
c <- aggregate(Score ~ PostId + UserId, data = c, sum)
colnames(c) <- c("pid", "uid", "CommentsTotalScore")
q <- subset(Posts, PostTypeId == 1)[c("Id", "Title", "OwnerUserId")]
q_cts <- merge(q, c, by.x = c("Id", "OwnerUserId"), by.y = c("pid", "uid"))[,c(3,4)]
head(q_cts[order(-q_cts$CommentsTotalScore),], 10)

#..


#..


#6)
#Zwraca u¿ytkowników (id, nazwê, reputacjê, wiek, lokacjê), którzy s¹ w posiadaniu 'wartoœciowej' odznaki
#Odznaka jest 'wartoœciowa' wtedy i tylko wtedy gdy jest z³ota i zosta³a zdobyta przez od 2 do 10 u¿ytkowników
sqldf::sqldf("SELECT DISTINCT Users.Id,
              Users.DisplayName,
              Users.Reputation,
              Users.Age,
              Users.Location
              FROM (
                SELECT
                Name, UserID
                FROM Badges
                WHERE Name IN (
                  SELECT
                  Name
                  FROM Badges
                  WHERE Class=1
                  GROUP BY Name
                  HAVING COUNT(*) BETWEEN 2 AND 10
                )
                AND Class=1
              ) AS ValuableBadges
              JOIN Users ON ValuableBadges.UserId=Users.Id")
#basic
b <- subset(Badges, Class == 1, select = c("Name", "UserId"))
vbNames <- subset(aggregate(b$Name, b["Name"], length), x <= 10 & x >= 2, select = "Name")
vbUid <- subset(b, Name %in% unlist(vbNames), select = "UserId")
subset(Users, Id %in% unlist(vbUid), select = c("Id", "DisplayName", "Reputation", "Age", "Location"))

#..


#..


#7)
#Zwraca 10 pytañ (tytu³ i liczbê 'starych' upvotów) z najwiêksz¹ liczb¹ 'starych' upvotów i bez 'nowych' upvotów.
#Upvote jest 'nowy' jeœli by³ dodany w 2016 lub 2017 roku. Upvoty które nie s¹ 'nowe', s¹ 'stare'.
sqldf::sqldf("SELECT Posts.Title,
              VotesByAge2.OldVotes
              FROM Posts JOIN (
                SELECT
                PostId,
                MAX(CASE WHEN VoteDate = 'new' THEN Total ELSE 0 END) NewVotes,
                MAX(CASE WHEN VoteDate = 'old' THEN Total ELSE 0 END) OldVotes,
                SUM(Total) AS Votes
                FROM (
                  SELECT
                  PostId,
                  CASE STRFTIME('%Y', CreationDate)
                    WHEN '2017'
                      THEN 'new'
                    WHEN '2016'
                      THEN 'new'
                    ELSE 'old'
                  END VoteDate,
                  COUNT(*) AS Total
                  FROM Votes
                  WHERE VoteTypeId=2
                  GROUP BY PostId, VoteDate
                ) AS VotesByAge
                GROUP BY VotesByAge.PostId
                HAVING NewVotes=0
              ) AS VotesByAge2 ON VotesByAge2.PostId=Posts.ID
              WHERE Posts.PostTypeId=1
              ORDER BY VotesByAge2.OldVotes DESC
              LIMIT 10")
#basic
v <- subset(Votes, VoteTypeId == 2, select = c("PostId", "CreationDate"))
v["CreationDate"] <- substr(unlist(v["CreationDate"]), 1, 4)
isNew <- v$CreationDate == 2017 | v$CreationDate == 2016
nvId <- unique(v[isNew,"PostId"])
ov <- subset(v[!isNew,], !PostId%in%nvId)
q <- subset(Posts, PostTypeId == 1 & !Id%in%nvId)[c("Id", "Title")]
g <- aggregate(ov$PostId, ov["PostId"], length)
q_ov <- merge(q, g, by.x = "Id", by.y = "PostId")
q_ov <- q_ov[,c(2,3)]
colnames(q_ov) <- c("Title", "OldVotes") 
head(q_ov[order(-q_ov$OldVotes),], 10)


#..


#..

