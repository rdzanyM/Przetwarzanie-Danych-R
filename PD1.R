options(stringsAsFactors = FALSE)
setwd("C:/Users/Michal/Desktop/R/files") #directory with csv files
Tags <- read.csv("Tags.csv")
Posts <- read.csv("Posts.csv")
Users <- read.csv("Users.csv")
Votes <- read.csv("Votes.csv")
Comments <- read.csv("Comments.csv")
Badges <- read.csv("Badges.csv")

#install.packages("sqldf")
library(sqldf)
#install.packages("dplyr")
library(dplyr)
#install.packages("data.table")
library(data.table)
TagsDT <- as.data.table(read.csv("Tags.csv"))
PostsDT <- as.data.table(read.csv("Posts.csv"))
setkey(PostsDT, Id)
UsersDT <- as.data.table(read.csv("Users.csv"))
setkey(UsersDT, Id)
VotesDT <- as.data.table(read.csv("Votes.csv"))
CommentsDT <- as.data.table(read.csv("Comments.csv"))
BadgesDT <- as.data.table(read.csv("Badges.csv"))
#install.packages("microbenchmark")
library(microbenchmark)
#install.packages("ggplot2")
library(ggplot2)

same <- function(x, y)
{
  x == y | is.na(x) & is.na(y)
}
same_df <- function(x, y, uniqueCol)
{
  all(same(x[order(x[[uniqueCol]]),], y[order(y[[uniqueCol]]),]))
}

test <- function(z, uniqueCol)
{
  df1 <- z("sqldf")
  df2 <- z("baser")
  df3 <- z("dplyr")
  df4 <- z("data.table")
  all(df1 == df2)
  all(df1 == df3)
  all(df1 == df4)
  stopifnot(same_df(df1, df2, uniqueCol) & same_df(df1, df3, uniqueCol) & same_df(df1, df4, uniqueCol))
  microbenchmark(
    sqldf=z("sqldf"),
    baser=z("baser"),
    dplyr=z("dplyr"),
    data.table=z("data.table"),
    times = 8
  )
}

#1)
#Zwraca 10 u¿ytkowników, których pytania zosta³y w sumie dodane do ulubionch najwiêksz¹ iloœæ razy.
#Zwraca nazwê, id, lokacjê u¿ytkownika, sumê dodañ do ulubionych dla wszystkich zadanych przez niego pytañ,
#,pytanie tego u¿ytkownika dodane do ulubionych najwiêksz¹ liczbê razy i liczbê dodañ do ulubionych dla tego pytania
Z1 <- function (x)
{
  if(x == "sqldf")
  {
    return(
    sqldf("SELECT Users.DisplayName,
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
           LIMIT 10"))
  }
  if(x == "baser")
  {
    q <- subset(Posts, PostTypeId == 1 & !is.na(FavoriteCount))[c("OwnerUserId", "Title", "FavoriteCount")]
    u_q <- merge(Users, q, by.x = "Id", by.y = "OwnerUserId")[c("DisplayName", "Id", "Location", "FavoriteCount", "Title")]
    g <- do.call(data.frame, aggregate(u_q["FavoriteCount"], u_q[c("DisplayName", "Id", "Location")], function(x)
      c(sum = sum(x), max = max(x))))
    g <- head(g[order(-g$FavoriteCount.sum),], 10)
    g <- merge(g, q, by.x = c("Id", "FavoriteCount.max"), by.y = c("OwnerUserId", "FavoriteCount"))
    g <- g[,c(3,1,4,5,6,2)]
    colnames(g) <- c("DisplayName", "Id", "Location", "FavoriteTotal", "MostFavoriteQuestion", "MostFavoriteQuestionLikes")
    return(g[order(-g$FavoriteTotal),])
  }
  if(x == "dplyr")
  {
    q_by_u <- 
      filter(Posts, PostTypeId == 1, !is.na(FavoriteCount), !is.na(OwnerUserId))[c("OwnerUserId", "Title", "FavoriteCount")] %>% 
      group_by(OwnerUserId)
    sc <- 
      q_by_u %>% 
      slice(which.max(FavoriteCount))
    d <- 
      q_by_u %>% 
      summarise(FavoriteTotal = sum(FavoriteCount)) %>% 
      arrange(-FavoriteTotal) %>% 
      head(10) %>% 
      inner_join(sc) %>% 
      inner_join(Users[c("DisplayName", "Id", "Location")], by = c("OwnerUserId" = "Id")) %>% 
      select(DisplayName, Id = OwnerUserId, Location, FavoriteTotal, MostFavoriteQuestion = Title, MostFavoriteQuestionLikes = FavoriteCount)
    return(as.data.frame(d))
  }
  if(x == "data.table")
  {
    #setkey(UsersDT, Id) jeœli nie ustawione wczeœniej
    q <- PostsDT[PostTypeId == 1 & !is.na(FavoriteCount) & !is.na(OwnerUserId),.(OwnerUserId, Title, FavoriteCount)]
    #sd <- q[,.SD[which.max(FavoriteCount)], keyby = OwnerUserId] #wolne :(
    sd <- q[q[,.I[which.max(FavoriteCount)], keyby = OwnerUserId]$V1] #szybkie :)
    fv <- 
      q[,.(FavoriteTotal = sum(FavoriteCount)), by = OwnerUserId
        ][order(-FavoriteTotal)
          ][1:10]
    setkey(fv, OwnerUserId)
    d <- 
      fv[sd, nomatch = 0
         ][UsersDT[,.(DisplayName, Id, Location)], nomatch = 0
           ][order(-FavoriteTotal)
             ][,.(DisplayName, Id = OwnerUserId, Location, FavoriteTotal, MostFavoriteQuestion = Title, MostfavoriteQuestionLikes = FavoriteCount)]
    return(as.data.frame(d))
  }
  stop("Wrong argument. Has to be one of ('sqldf' 'baser' 'dplyr' 'data.table')")
}

#2)
#Zwraca 10 pytañ (id, tytu³, pac) z najwiêksz¹ liczb¹(oznaczon¹ jako pac) odpowiedzi o dodatnim wyniku na dane pytanie.
Z2 <- function (x)
{
  if(x == "sqldf")
  {
    return(
      sqldf("SELECT Posts.ID,
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
            LIMIT 10"))
  }
  if(x == "baser")
  {
    p <- subset(Posts, PostTypeId == 2 & Score > 0)
    p <- aggregate(p$ParentId, p["ParentId"], length)
    colnames(p) <- c("ParentId", "PositiveAnswerCount")
    p <- merge(Posts, p, by.x = "Id", by.y = "ParentId")[c("Id", "Title", "PositiveAnswerCount")]
    return(head(p[order(-p$PositiveAnswerCount),], 10))
  }
  if(x == "dplyr")
  {
    d <- 
      filter(Posts, PostTypeId == 2, Score > 0) %>%
      group_by(ParentId) %>%
      summarise(PositiveAnswerCount = n()) %>%
      inner_join(Posts, by = c("ParentId" = "Id")) %>%
      arrange(-PositiveAnswerCount) %>%
      select(Id = ParentId, Title, PositiveAnswerCount) %>%
      head(10)
    return(as.data.frame(d))
  }
  if(x == "data.table")
  {
    #setkey(PostsDT, Id) jeœli nie ustawione wczesniej
    d <-
      PostsDT[PostTypeId == 2 & Score > 0
              ][,.(PositiveAnswerCount = .N), keyby = ParentId
                ][PostsDT, nomatch = 0
                  ][order(-PositiveAnswerCount)
                    ][1:10,.(Id = ParentId, Title, PositiveAnswerCount)]
    return(as.data.frame(d))
  }
  stop("Wrong argument. Has to be one of ('sqldf' 'baser' 'dplyr' 'data.table')")
}

#3)
#Dla ka¿dego roku zwraca tytu³ pytania, które dosta³o najwiêcej upvotów w danym roku, ten rok i t¹ liczbê upVotów.
Z3 <- function (x)
{
  if(x == "sqldf")
  {
    return(
      sqldf("SELECT Posts.Title,
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
            GROUP BY Year"))
  }
  if(x == "baser")
  {
    uv <- subset(Votes, VoteTypeId == 2)[c("PostId", "CreationDate")]
    #uv["CreationDate"] <- format(as.Date(unlist(uv["CreationDate"])), "%Y")  #na górze bardzo wolny sposób,
    uv["CreationDate"] <- strtoi(substr(unlist(uv["CreationDate"]), 1, 4))    #na dole substr, szybszy ni¿ as.Date,
                                                                              #funkcje z base i tak s¹ najwolniejsze,
    uv_y <- aggregate(cbind(PostId)~PostId+CreationDate, uv, length)          #strtoi trochê przyœpiesza aggregate.
    colnames(uv_y) <- c("Id", "Year", "Count")
    q <- subset(Posts, PostTypeId == 1)[c("Id", "Title")]
    q_uv_y <- subset(merge(q, uv_y, by = "Id"), select = -Id)
    g <- aggregate(Count ~ Year, data = q_uv_y, max)
    g <- merge(g, q_uv_y, by = c("Year", "Count"))
    return(g[,c(3,1,2)])
  }
  if(x == "dplyr")
  {
    q <-
      filter(Posts, PostTypeId == 1)[c("Id", "Title")]
    q_uv_y <- 
      filter(Votes, VoteTypeId == 2) %>%
      select(Id = PostId, CreationDate) %>%
      mutate(Year = replace(CreationDate, TRUE, substr(unlist(CreationDate), 1, 4))) %>%
      group_by(Id, Year) %>%
      summarise(Count = n()) %>%
      inner_join(q)
    d <-
      q_uv_y %>%
      group_by(Year) %>%
      summarise(Count = max(Count)) %>% 
      inner_join(q_uv_y) %>%
      select(Title, Year, Count)
    return(as.data.frame(d))
  }
  if(x == "data.table")
  {
    q_uv_y <- 
      VotesDT[VoteTypeId == 2,.(Id = PostId, Year = substr(unlist(CreationDate), 1, 4))
              ][,.(Count = .N), by =.(Id, Year)
                ][PostsDT[PostTypeId == 1,.(Id, Title)], on =.(Id = Id), nomatch = 0]
    d <- 
      q_uv_y[,.(Count = max(Count)), by = Year
             ][q_uv_y,.(Title, Year, Count), on =.(Count = Count, Year = Year), nomatch = 0]
    return(as.data.frame(d))
  }
  stop("Wrong argument. Has to be one of ('sqldf' 'baser' 'dplyr' 'data.table')")
}

#4)
#Zwraca pytania z najwiêksz¹ ró¿nic¹ pomiêdzy wynikiem najwy¿ej punktowanej i zaakceptowanej odpowiedzi,
#posortowane malej¹co wzglêdem tej ró¿nicy
Z4 <- function (x)
{
  if(x == "sqldf")
  {
    return(
      sqldf("SELECT Questions.Id,
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
            ORDER BY Difference DESC"))
  }
  if(x == "baser")
  {
    q <- subset(Posts, PostTypeId == 1 & !is.na(AcceptedAnswerId))[c("Id", "Title", "AcceptedAnswerId")]
    a <- subset(Posts, PostTypeId == 2)[c("Id", "ParentId", "Score")]
    ba <- aggregate(Score ~ ParentId, data = a, max)
    q_ba <- merge(q, ba, by.x = "Id", by.y = "ParentId")
    aa <- a[c("Id", "Score")]
    colnames(aa) <- c("Id", "AcceptedScore")
    q_ba_aa <- merge(q_ba, aa, by.x = "AcceptedAnswerId", by.y = "Id")
    q_d = cbind(q_ba_aa, q_ba_aa["Score"] - q_ba_aa["AcceptedScore"])
    colnames(q_d) <- c("aaid", "Id", "Title", "MaxScore", "AcceptedScore", "Difference")
    q_d <- subset(q_d, Difference > 50, select = -aaid)
    return(q_d[order(-q_d$Difference),])
  }
  if(x == "dplyr")
  {
    q <- filter(Posts, PostTypeId == 1, !is.na(AcceptedAnswerId))[c("Id", "Title", "AcceptedAnswerId")]
    a <- filter(Posts, PostTypeId == 2)[c("Id", "ParentId", "Score")]
    aa <- select(a, Id, AcceptedScore = Score)
    d <- 
      group_by(a, ParentId) %>% 
      summarise(MaxScore = max(Score)) %>%
      inner_join(q, by = c("ParentId" = "Id")) %>%
      inner_join(aa, by = c("AcceptedAnswerId" = "Id")) %>%
      mutate(Difference = MaxScore - AcceptedScore) %>%
      filter(Difference > 50) %>%
      select(Id = ParentId, Title, MaxScore, AcceptedScore, Difference) %>%
      arrange(-Difference)
    return(as.data.frame(d))
  }
  if(x == "data.table")
  {
    a <- PostsDT[PostTypeId == 2,.(Id, ParentId, Score)]
    d <- 
      a[,.(MaxScore = max(Score)), keyby = ParentId
        ][PostsDT[PostTypeId == 1 & !is.na(AcceptedAnswerId),.(Id, Title, AcceptedAnswerId)], nomatch = 0
          ][a[,.(Id, AcceptedScore = Score)], on =.(AcceptedAnswerId = Id), nomatch = 0
            ][,.(Id = ParentId, Title, MaxScore, AcceptedScore, Difference = MaxScore - AcceptedScore)
              ][Difference > 50
                ][order(-Difference)]
    return(as.data.frame(d))
  }
  stop("Wrong argument. Has to be one of ('sqldf' 'baser' 'dplyr' 'data.table')")
}

#5)
#Zwraca 10 pytañ (tytu³ pytania, 'wynik komentarzy autora') z najwiêkszym 'wynikiem komentarzy autora',
#gdzie 'wynik komentarzy autora' to suma punktów uzyskanych przez autora pytania w komentarzach do tego pytania
Z5 <- function (x)
{
  if(x == "sqldf")
  {
    return(
      sqldf("SELECT Posts.Title,
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
            LIMIT 10"))
  }
  if(x == "baser")
  {
    c <- Comments[c("PostId", "UserId", "Score")]
    c <- aggregate(Score ~ PostId + UserId, data = c, sum)
    colnames(c) <- c("pid", "uid", "CommentsTotalScore")
    q <- subset(Posts, PostTypeId == 1)[c("Id", "Title", "OwnerUserId")]
    q_cts <- merge(q, c, by.x = c("Id", "OwnerUserId"), by.y = c("pid", "uid"))[,c(3,4)]
    return(head(q_cts[order(-q_cts$CommentsTotalScore),], 10))
  }
  if(x == "dplyr")
  {
    cts <-
      select(Comments, PostId, UserId, Score) %>%
      group_by(PostId, UserId) %>%
      summarise(CommentsTotalScore = sum(Score))
    d <- 
      filter(Posts, PostTypeId == 1)[c("Id", "Title", "OwnerUserId")] %>% 
      inner_join(cts, by = c("Id" = "PostId", "OwnerUserId" = "UserId")) %>%
      select(Title, CommentsTotalScore) %>%
      arrange(-CommentsTotalScore) %>%
      head(10)
    return(as.data.frame(d))
  }
  if(x == "data.table")
  {
    setkeyv(PostsDT, c("Id", "OwnerUserId"))
    d <-
      CommentsDT[,.(CommentsTotalScore = sum(Score)), keyby =.(PostId, UserId)
                 ][PostsDT[PostTypeId == 1,.(Id, Title, OwnerUserId)], nomatch = 0
                   ][order(-CommentsTotalScore),.(Title, CommentsTotalScore)
                     ][1:10]
    return(as.data.frame(d))
  }
  stop("Wrong argument. Has to be one of ('sqldf' 'baser' 'dplyr' 'data.table')")
}

#6)
#Zwraca u¿ytkowników (id, nazwê, reputacjê, wiek, lokacjê), którzy s¹ w posiadaniu 'wartoœciowej' odznaki
#Odznaka jest 'wartoœciowa' wtedy i tylko wtedy gdy jest z³ota i zosta³a zdobyta przez od 2 do 10 u¿ytkowników
Z6 <- function (x)
{
  if(x == "sqldf")
  {
    return(
      sqldf("SELECT DISTINCT Users.Id,
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
            JOIN Users ON ValuableBadges.UserId=Users.Id"))
  }
  if(x == "baser")
  {
    b <- subset(Badges, Class == 1, select = c("Name", "UserId"))
    vbNames <- subset(aggregate(b$Name, b["Name"], length), x <= 10 & x >= 2, select = "Name")
    vbUid <- subset(b, Name %in% unlist(vbNames), select = "UserId")
    return(subset(Users, Id %in% unlist(vbUid), select = c("Id", "DisplayName", "Reputation", "Age", "Location")))
  }
  if(x == "dplyr")
  {
    b <- 
      filter(Badges, Class == 1) %>%
      select(Name, UserId)
    vbNames <-
      group_by(b, Name) %>%
      summarise(Count = n()) %>%
      filter(dplyr::between(Count, 2, 10)) %>%
      select(Name)
    vbUid <- 
      filter(b, Name %in% unlist(vbNames)) %>%
      select(UserId)
    d <-
      filter(Users, Id %in% unlist(vbUid)) %>%
      select(Id, DisplayName, Reputation, Age, Location)
    return(as.data.frame(d))
  }
  if(x == "data.table")
  {
    b <- BadgesDT[Class == 1,.(Name, UserId)]
    vbNames <- b[,.(Count = .N), by = Name][data.table::between(Count, 2, 10), Name]
    vbUid <- b[Name %in% unlist(vbNames), UserId]
    d <- UsersDT[Id %in% unlist(vbUid),.(Id, DisplayName, Reputation, Age, Location)]
    return(as.data.frame(d))
  }
  stop("Wrong argument. Has to be one of ('sqldf' 'baser' 'dplyr' 'data.table')")
}

#7)
#Zwraca 10 pytañ (tytu³ i liczbê 'starych' upvotów) z najwiêksz¹ liczb¹ 'starych' upvotów i bez 'nowych' upvotów.
#Upvote jest 'nowy' jeœli by³ dodany w 2016 lub 2017 roku. Upvoty które nie s¹ 'nowe', s¹ 'stare'.
Z7 <- function (x)
{
  if(x == "sqldf")
  {
    return(
      sqldf("SELECT Posts.Title,
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
            LIMIT 10"))
  }
  if(x == "baser")
  {
    v <- subset(Votes, VoteTypeId == 2, select = c("PostId", "CreationDate"))
    v["CreationDate"] <- substr(unlist(v["CreationDate"]), 1, 4)
    isNew <- v$CreationDate == 2017 | v$CreationDate == 2016
    nvId <- unique(v[isNew, "PostId"])
    ov <- subset(v[!isNew,], !PostId%in%nvId)
    q <- subset(Posts, PostTypeId == 1 & !Id%in%nvId)[c("Id", "Title")]
    g <- aggregate(ov$PostId, ov["PostId"], length)
    q_ov <- merge(q, g, by.x = "Id", by.y = "PostId")
    q_ov <- q_ov[,c(2,3)]
    colnames(q_ov) <- c("Title", "OldVotes") 
    return(head(q_ov[order(-q_ov$OldVotes),], 10))
  }
  if(x == "dplyr")
  {
    v <- 
      filter(Votes, VoteTypeId == 2) %>% 
      select(PostId, CreationDate) %>%
      mutate(CreationDate = replace(CreationDate, TRUE, substr(unlist(CreationDate), 1, 4)))
    isNew <- v$CreationDate == 2017 | v$CreationDate == 2016
    nvId <- unique(v[isNew, "PostId"])
    q <- 
      filter(Posts, PostTypeId == 1, !Id%in%nvId) %>% 
      select(Id, Title)
    d <- 
      filter(v[!isNew,], !PostId%in%nvId) %>% 
      group_by(PostId) %>% 
      summarise(OldVotes = n()) %>% 
      inner_join(q, by = c("PostId" = "Id")) %>% 
      arrange(-OldVotes) %>% 
      head(10) %>%
      select(Title, OldVotes)
    return(as.data.frame(d))
  }
  if(x == "data.table")
  {
    v <- VotesDT[VoteTypeId == 2,.(PostId, Year = substr(unlist(CreationDate), 1, 4))]
    isNew <- v$Year == 2017 | v$Year == 2016
    nvId <- unlist(unique(v[isNew, "PostId"]))
    nv <- v[isNew, "PostId"][,.N, keyby = PostId]
    setkey(PostsDT,Id) #jeœli nie ustawione wczeœniej (zmienione w Z5)
    ov <- v[!isNew, "PostId"][,.(OldVotes = .N), keyby = PostId]
    d <- 
      nv[PostsDT[PostTypeId == 1,.(Id, Title)]
         ][is.na(N)
           ][ov, nomatch = 0
             ][order(-OldVotes),.(Title, OldVotes)
               ][1:10]
    #Joining is faster(whole method nearly twice as fast), %in% operator is slow, %in% could be better, a hashset would be nice :/
    #q <- PostsDT[PostTypeId == 1 & !Id%in%nvId,.(Id, Title)]
    #setkey(q, Id)
    #d <-
    #  v[!isNew & !PostId%in%nvId,.(OldVotes = .N), keyby = PostId
    #    ][q, nomatch = 0
    #      ][order(-OldVotes),.(Title, OldVotes)
    #        ][1:10]
    return(as.data.frame(d))
  }
  stop("Wrong argument. Has to be one of ('sqldf' 'baser' 'dplyr' 'data.table')")
}

#test1
m <- test(Z1, "Id")
autoplot(m)

#test2
test(Z2, "Id")
autoplot(m)

#test3
test(Z3, "Title")
autoplot(m)

#test4
test(Z4, "Id")
autoplot(m)

#test5
test(Z5, "Title")
autoplot(m)

#test6
test(Z6, "Id")
autoplot(m)

#test7
test(Z7, "Title")
autoplot(m)

