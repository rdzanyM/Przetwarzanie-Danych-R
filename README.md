## Ważne informacje
Używane ramki danych pochodzą z serwisu https://travel.stackexchange.com.  
Pełen zbiór danych dostępny jest pod adresem https://archive.org/download/stackexchange.  
Do poprawnego skompilowania pliku *report.rmd* wymagane jest podanie ścieżki do katalogu z plikami *.csv* z danymi i zainstalowanie używanych pakietów (w sekcji "*r setup*").  

## Cele projektu
* Stworzenie funkcji w bazowym R, dplyr i data.table, odpowiadających podanym zapytaniom SQL.
* Dodanie słownego opisu tych zapytań.
* Sprawdzenie poprawności i szybkości działania napisanych funkcji i podanych zapytań.
* Stworzenie raportu w R markdown (poniżej wersja skompilowana do *.md*).

-----

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
`z` dla różnych sposobów implementacji zapytań SQL.
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
![image](https://user-images.githubusercontent.com/43205483/55647161-421b8700-57dd-11e9-8375-4373885f2449.png)

    ## Unit: milliseconds
    ##        expr       min        lq      mean    median        uq       max
    ##       sqldf 307.98127 313.37295 322.02825 315.60815 319.39235 509.72518
    ##       baser 103.12010 106.13636 116.51952 107.85490 114.15429 299.30195
    ##       dplyr  59.14506  63.05282  77.21802  65.41900 100.59381 162.65804
    ##  data.table  15.59986  17.76056  20.68818  18.35502  19.04063  79.32536
    ##  neval  cld
    ##    256    d
    ##    256   c 
    ##    256  b  
    ##    256 a

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
![image](https://user-images.githubusercontent.com/43205483/55647335-b524fd80-57dd-11e9-90dd-23f57ed4b0d1.png)

    ## Unit: milliseconds
    ##        expr       min        lq      mean    median        uq       max
    ##       sqldf 214.43383 217.95671 220.76567 219.52561 221.09573 263.42449
    ##       baser 310.62435 318.86808 347.78644 354.10982 357.50727 608.99847
    ##       dplyr  47.87069  50.00265  57.85319  50.77529  52.48480 299.82991
    ##  data.table  21.98177  24.02916  33.32505  24.73591  26.05971  84.06222
    ##  neval  cld
    ##    256   c 
    ##    256    d
    ##    256  b  
    ##    256 a

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
![image](https://user-images.githubusercontent.com/43205483/55647454-ff0de380-57dd-11e9-8c0e-a53b41cfad3a.png)

    ## Unit: milliseconds
    ##        expr        min         lq      mean    median        uq       max
    ##       sqldf 1117.14182 1140.66950 1180.2424 1160.2845 1195.5205 1600.3274
    ##       baser 2769.37430 2924.50994 3031.2806 3030.5408 3130.5430 3355.6524
    ##       dplyr  298.84789  309.86895  353.0406  351.7184  361.7882  627.1778
    ##  data.table   86.56408   91.08848  129.9587  132.6421  144.3365  471.3775
    ##  neval  cld
    ##    256   c 
    ##    256    d
    ##    256  b  
    ##    256 a

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
![image](https://user-images.githubusercontent.com/43205483/55647496-177dfe00-57de-11e9-944e-c3b720bda0a4.png)

    ## Unit: milliseconds
    ##        expr       min        lq      mean    median        uq       max
    ##       sqldf 285.33072 289.07139 298.40947 292.61193 302.51179 387.49589
    ##       baser 316.08049 324.03909 354.94032 356.70528 365.78118 649.99912
    ##       dplyr  43.99228  45.85985  51.09023  46.63762  48.67926 127.28268
    ##  data.table  24.11865  26.90974  31.62791  27.89032  29.37444  77.57932
    ##  neval  cld
    ##    256   c 
    ##    256    d
    ##    256  b  
    ##    256 a

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
![image](https://user-images.githubusercontent.com/43205483/55647527-2c5a9180-57de-11e9-9823-8672dd7a7ff9.png)

    ## Unit: milliseconds
    ##        expr        min         lq       mean     median        uq
    ##       sqldf  520.89609  529.02466  540.80773  531.43578  537.1939
    ##       baser 3050.96291 3111.06454 3161.79285 3141.44953 3185.5974
    ##       dplyr  182.36458  189.72482  217.36317  198.30129  242.7616
    ##  data.table   22.88456   25.32134   29.17579   25.96056   26.7447
    ##        max neval  cld
    ##   661.4508   256   c 
    ##  3646.4635   256    d
    ##   487.9943   256  b  
    ##   105.8375   256 a

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
![image](https://user-images.githubusercontent.com/43205483/55647557-45fbd900-57de-11e9-9dc3-e9a72efa8faf.png)

    ## Unit: milliseconds
    ##        expr        min         lq       mean     median         uq
    ##       sqldf 243.419079 246.448690 251.278501 248.009990 250.580613
    ##       baser   3.288460   3.486959   3.942877   3.608684   3.768386
    ##       dplyr   8.729814   9.719430  11.416260  10.062440  10.407913
    ##  data.table   9.538996  10.925610  12.034940  11.534242  12.209177
    ##        max neval cld
    ##  307.52269   256   c
    ##   40.91115   256 a  
    ##   50.26211   256  b 
    ##   54.73171   256  b

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
![image](https://user-images.githubusercontent.com/43205483/55647608-5f9d2080-57de-11e9-9fbd-833ca927ec60.png)

    ## Unit: milliseconds
    ##        expr       min        lq      mean    median        uq       max
    ##       sqldf 1075.4355 1085.5595 1116.9866 1090.9932 1120.5495 1431.2258
    ##       baser  734.8553  800.2029  829.6848  814.9402  837.0574 1099.5315
    ##       dplyr  182.2591  231.8440  247.3661  238.0461  245.6042  539.7154
    ##  data.table  109.8629  117.2362  156.7559  161.9373  166.8749  417.9829
    ##  neval  cld
    ##    256    d
    ##    256   c 
    ##    256  b  
    ##    256 a

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
![image](https://user-images.githubusercontent.com/43205483/55647644-78a5d180-57de-11e9-89f4-76904ee2a5e7.png)

    ##   sqldf baser dplyr data.table
    ## a     0     1     0          6
    ## b     0     0     7          1
    ## c     5     2     0          0
    ## d     2     4     0          0
