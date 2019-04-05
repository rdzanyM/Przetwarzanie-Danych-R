Testy
-----
Funkcja `same` sprawdza równość podanych argumentów, zakładając, że
`NA == NA`.  
Funkcja `same_df` porównuje ramki danych przy użyciu funkcji `same`.
`same_df` przyjmuje także argument `uniqueCol` - jest to nazwa takiej
kolumny w podanych ramkach danych, że wartości w tych kolumnach są
unikalne i atomowe.  
Funkcja `test`, korzystając z funkcji `same_df`, sprawdza poprawność
działania podanej funkcji `z` i zwraca `microbenchmark` wywołań funkcji
`z` dla różynych sposobów implementacji zapytań SQL.
```r
    same <- function(x, y)
    {
      x == y | is.na(x) & is.na(y)
    }
    same_df <- function(x, y, uniqueCol)
    {
      all(same(x[order(x[[uniqueCol]]),], y[order(y[[uniqueCol]]),]))
    }

    test <- function(z, uniqCol)
    {
      df1 <- z("sqldf")
      df2 <- z("baser")
      df3 <- z("dplyr")
      df4 <- z("data.table")
      all(df1 == df2)
      all(df1 == df3)
      all(df1 == df4)
      stopifnot(same_df(df1, df2, uniqCol) & same_df(df1, df3, uniqCol) & same_df(df1, df4, uniqCol))
      microbenchmark(
        sqldf = z("sqldf"),
        baser = z("baser"),
        dplyr = z("dplyr"),
        data.table = z("data.table"),
        times = 256
      )
    }
```
Zadanie1
--------
Zwraca 10 użytkowników, których pytania zostały w sumie dodane do
ulubionch największą ilość razy.  
Zwraca nazwę, id, lokację użytkownika, sumę dodań do ulubionych dla
wszystkich zadanych przez niego pytań, pytanie tego użytkownika dodane
do ulubionych największą liczbę razy i liczbę dodań do ulubionych dla
tego pytania.
### Kod
```r
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
        setkey(UsersDT, Id)
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
```
### Benchmark

![](report_files/figure-markdown_strict/test1-1.png)

    ## Unit: milliseconds
    ##        expr       min        lq      mean    median        uq       max
    ##       sqldf 311.24879 311.49984 315.59249 311.75089 317.76433 323.77778
    ##       baser 106.61793 106.92625 107.18777 107.23457 107.47269 107.71080
    ##       dplyr  61.69659  61.81442  82.72344  61.93224  93.23686 124.54147
    ##  data.table  17.55364  18.16966  30.14134  18.78569  36.43519  54.08469
    ##  neval cld
    ##      3   c
    ##      3  b 
    ##      3 ab 
    ##      3 a


Zadanie2
--------
Zwraca 10 pytań (id, tytuł, liczba odpowiedzi o dodatnim wyniku na dane
pytanie) z największą liczbą odpowiedzi o dodatnim wyniku na dane
pytanie.
### Kod
```r
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
        setkey(PostsDT, Id)
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
```
### Benchmark

![](report_files/figure-markdown_strict/test2-1.png)

    ## Unit: milliseconds
    ##        expr       min        lq      mean    median        uq       max
    ##       sqldf 218.13817 218.66634 219.02221 219.19450 219.46423 219.73396
    ##       baser 319.35252 321.79855 337.13621 324.24457 346.02805 367.81152
    ##       dplyr  50.44193  51.30551  63.01088  52.16909  69.29535  86.42162
    ##  data.table  23.98974  24.39495  25.54598  24.80016  26.32410  27.84804
    ##  neval cld
    ##      3  b 
    ##      3   c
    ##      3 a  
    ##      3 a


Zadanie3
--------
Dla każdego roku zwraca tytuł pytania, które dostało najwięcej upvotów w
danym roku, ten rok i tę liczbę upVotów.
### Kod
```r
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
        uv["CreationDate"] <- strtoi(substr(unlist(uv["CreationDate"]), 1, 4))    #na dole substr, szybszy niż as.Date,
                                                                                  #funkcje z base i tak są najwolniejsze,
        uv_y <- aggregate(cbind(PostId)~PostId+CreationDate, uv, length)          #strtoi trochę przyśpiesza aggregate.
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
```
### Benchmark

![](report_files/figure-markdown_strict/test3-1.png)

    ## Unit: milliseconds
    ##        expr        min         lq      mean     median        uq       max
    ##       sqldf 1129.60923 1130.40076 1131.1002 1131.19229 1131.8457 1132.4991
    ##       baser 2793.65266 2881.63722 2916.2323 2969.62178 2977.5221 2985.4224
    ##       dplyr  357.75648  358.22840  361.0218  358.70032  362.6545  366.6086
    ##  data.table   91.91799   91.95124  109.3548   91.98449  118.0731  144.1618
    ##  neval  cld
    ##      3   c 
    ##      3    d
    ##      3  b  
    ##      3 a


Zadanie4
--------
Zwraca pytania z największą różnicą pomiędzy wynikiem najwyżej
punktowanej i zaakceptowanej odpowiedzi, posortowane malejąco względem
tej różnicy.
### Kod
```r
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
```
### Benchmark

![](report_files/figure-markdown_strict/test4-1.png)

    ## Unit: milliseconds
    ##        expr      min        lq      mean    median        uq       max
    ##       sqldf 287.7213 288.12242 288.44511 288.52352 288.80700 289.09049
    ##       baser 323.1020 324.18648 327.49799 325.27093 329.69598 334.12103
    ##       dplyr  45.9789  46.07128  60.86113  46.16365  68.30225  90.44085
    ##  data.table  25.7830  27.14478  28.23067  28.50655  29.45450  30.40245
    ##  neval cld
    ##      3  b 
    ##      3   c
    ##      3 a  
    ##      3 a


Zadanie5
--------
Zwraca 10 pytań (tytuł pytania, *wynik komentarzy autora*) z największym
*wynikiem komentarzy autora*, gdzie *wynik komentarzy autora* to suma
punktów uzyskanych przez autora pytania w komentarzach do tego pytania.
### Kod
```r
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
```
### Benchmark

![](report_files/figure-markdown_strict/test5-1.png)

    ## Unit: milliseconds
    ##        expr        min         lq       mean     median         uq
    ##       sqldf  529.57540  531.41259  548.60427  533.24977  558.11870
    ##       baser 3095.02991 3107.30477 3118.86543 3119.57964 3130.78318
    ##       dplyr  185.92564  187.95352  206.40851  189.98141  216.64995
    ##  data.table   24.26563   24.55424   24.69273   24.84285   24.90628
    ##         max neval  cld
    ##   582.98762     3   c 
    ##  3141.98673     3    d
    ##   243.31850     3  b  
    ##    24.96971     3 a


Zadanie6
--------
Zwraca użytkowników (id, nazwę, reputację, wiek, lokację), którzy są w
posiadaniu *wartościowej* odznaki.  
Odznaka jest *wartościowa* wtedy i tylko wtedy gdy jest <span
style="color:gold">złota</span> i została zdobyta przez od 2 do 10
użytkowników.
### Kod
```r
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
```
### Benchmark

![](report_files/figure-markdown_strict/test6-1.png)

    ## Unit: milliseconds
    ##        expr        min         lq       mean     median         uq
    ##       sqldf 244.582972 245.622675 249.706017 246.662378 252.267539
    ##       baser   3.610326   3.718710   3.818746   3.827094   3.922956
    ##       dplyr   9.522575   9.574509   9.784365   9.626442   9.915260
    ##  data.table  10.413045  10.648492  11.057873  10.883939  11.380287
    ##         max neval cld
    ##  257.872700     3   b
    ##    4.018818     3  a 
    ##   10.204078     3  a 
    ##   11.876635     3  a


Zadanie7
--------
Zwraca 10 pytań (tytuł i liczbę *starych* upvotów) z największą liczbą
*starych* upvotów i bez *nowych* upvotów. Upvote jest *nowy* jeśli był
dodany w 2016 lub 2017 roku. Upvoty które nie są *nowe*, są *stare*.
### Kod
```r
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
        setkey(PostsDT,Id)
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
```
### Benchmark

![](report_files/figure-markdown_strict/test7-1.png)

    ## Unit: milliseconds
    ##        expr       min        lq      mean    median        uq       max
    ##       sqldf 1085.0832 1089.5892 1092.9599 1094.0951 1096.8983 1099.7015
    ##       baser  774.4692  816.1940  840.6801  857.9188  873.7856  889.6523
    ##       dplyr  185.2515  213.6659  241.8929  242.0803  270.2137  298.3470
    ##  data.table  116.4455  116.6178  131.2931  116.7900  138.7170  160.6439
    ##  neval cld
    ##      3   c
    ##      3  b 
    ##      3 a  
    ##      3 a


Podsumowanie
------------
Najszybszym z używanych pakietów okazał się `data.table`. Pakiet `sqldf`
i funkcje z `base` działały najwolniej.  
Wyjątkiem jest zadanie 6, gdzie to właśnie funkcje bazowe są najszybsze.
W rozwiązaniu zadania 6 nie ma żadnych funkcji typu `merge/aggregate`
które nie działają optymalnie. Są za to operatory *%in%*, które także
działają nieoptymalnie, ale w tym zadaniu po prawej stronie tego
operatora są na tyle małe zbiory danych, że nie ma to dużego znaczenia.
### Benchmark

![](report_files/figure-markdown_strict/benchmark_summary-1.png)

    ##   sqldf baser dplyr data.table
    ## a     0     1     4          7
    ## b     3     2     2          0
    ## c     4     2     0          0
    ## d     0     2     0          0
