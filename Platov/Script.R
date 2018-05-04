#if (!require("XML")) { install.packages("XML"); require("XML") }
if (!require("ggplot2")) { install.packages("ggplot2"); require("ggplot2") }
#if (!require("RCurl")) { install.packages("RCurl"); require("RCurl") }
if (!require("qdap")) { install.packages("qdap"); require("qdap") }
if (!require("rvest")) { install.packages("rvest"); require("rvest") }

#if (!require("gridExtra")) { install.packages("gridExtra"); require("gridExtra") }
#if (!require("xlsx")) { install.packages("xlsx"); require("xlsx") }
#if (!require("rJava")) { install.packages("rJava"); require("rJava") }

#if (!require("mclust")) { install.packages("mclust"); require("mclust") }


# Parsing -----

Sys.getlocale()
LANG = "ru_RU.65001"
LANG <- Sys.getenv("LANG")
Sys.setlocale("LC_ALL", "Russian_Russia.65001")
Sys.setlocale("LC_ALL", "Russian_Russia.20866")
Sys.setlocale("LC_ALL", "Russian_Russia.1251")

# u = "http://en.wikipedia.org/wiki/World_population"
#u = "http://rov.aero/raspisanie_reysov"
u = "https://rasp.yandex.ru/station/9866615?start=2018-05-05T00%3A00%3A00&span=24"
#u = "https://raspisanie.msk.ru/plane/s9866615"
#u = "https://ru.wikipedia.org/wiki/Список_наиболее_загруженных_аэропортов_России"

#u <- getURL(u)

# Yandex Rasp
sDate = Sys.Date() + 1
u = paste( "https://rasp.yandex.ru/station/9866615?start=", Sys.Date()+1, "T00%3A00%3A00&span=24", sep = "")

# Tables
i = 1
u = paste("https://rasp.yandex.ru/station/", station, "?start=", Sys.Date() + 1 + i, "T00%3A00%3A00&span=24", sep = "")
sDate = Sys.Date() + 1 + i
OneDay_TimeTable = function(u, sDate)
{ 
    tables = read_html(u, encoding = "UTF-8")

    dff = html_nodes(tables, "table")

    df = html_table( dff[9], fill = T)[[1]]
    df = df[-c(1:6), c(2:5)]

    colnames( df ) = df[1,]
    df = df[-c(1), ]



    library(stringr)
    df$`Авиакомпания` = str_split_fixed( df$`рейс`, ",", 2)[,2]
    df$`рейс` = str_split_fixed(df$`рейс`, ",", 2)[, 1]

    df$`Номер рейса` = apply(str_split_fixed(df$`рейс`, " ", 3)[, 1:2], MARGIN = 1, FUN = paste, collapse = " ")


    #df$`рейс` = str_split_fixed(df$`рейс`, " ", 3)[, 3]
    #df$`рейс` = str_split_fixed(df$`рейс`, " ", 2)[, 3]
    #df$`рейс` = str_split_fixed(df$`рейс`, " — ", 2)[,2]

    df_div3 = html_nodes(tables, xpath = '//*[@class="b-timetable__tripname"]')
    df$`Направление` = str_split_fixed( html_text(html_nodes(df_div3, "a")), " — ", 3)[, 2]

    df_div4 = html_nodes(tables, xpath = '//*[@class="b-timetable__description"]')
    df$`Самолёт` = str_split_fixed( html_text( df_div4 ), ", ", 2)[, 1]

    #df = df[ , c(7,5,2,6, 8)]
    df$`расписание`[1] = strsplit(df$`расписание`[1], ", ")[[1]][1]

    df$`расписание` = as.POSIXlt(paste( sDate, df$`расписание`))

    return(df)
}

station = "9600213" # Sheremetievo
station = "9866615" # Platov
i = 2

OneWeek_TimeTable = function(station)
{
    df = NULL

    for(i in c(1:7))
        {
        cat(".",i,".")
        df_tmp = OneDay_TimeTable(
            paste("https://rasp.yandex.ru/station/",station,"?start=", Sys.Date() + 1 + i, "T00%3A00%3A00&span=24", sep = ""),
            Sys.Date() + 1 + i)
        df = rbind(df, df_tmp)
    }

    A320_1 = "Airbus A320"
    A320_2 = "Airbus А320"

    df$`Самолёт`[df$`Самолёт` == A320_2] = A320_1

    return(df)
}

df = OneWeek_TimeTable("9866615")




sort( unique( df$`Самолёт` ) )

dff = df[, c(1,2,4,5)]

dff = as.data.frame(sort(table(df$`Направление`), decreasing = T))
dff = as.data.frame(sort(table(df$`Авиакомпания`), decreasing = T))
dff = as.data.frame(sort(table(df$`Самолёт`), decreasing = T))


ggplot(data = dff) +
    geom_bar(stat = "identity", aes(reorder(Var1, Freq), Freq), fill = "steelblue") +
    geom_text(aes(reorder(Var1, Freq), label = Freq, y = Freq), size = 3, hjust = -0.1, colour = "dimgray", vjust = 0.25) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("") + ylab("") + coord_flip() +
    scale_y_continuous(breaks = seq(0, max(dff$Freq), 5))


