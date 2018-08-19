#if (!require("XML")) { install.packages("XML"); require("XML") }
if (!require("ggplot2")) { install.packages("ggplot2"); require("ggplot2") }
#if (!require("RCurl")) { install.packages("RCurl"); require("RCurl") }
if (!require("qdap")) { install.packages("qdap"); require("qdap") }
if (!require("rvest")) { install.packages("rvest"); require("rvest") }
if (!require("RJSONIO")) { install.packages("RJSONIO"); require("RJSONIO") }

if (!require("RCurl")) { install.packages("RCurl"); require("RCurl") }

#if (!require("xlsx")) { install.packages("xlsx"); require("xlsx") }
#if (!require("rJava")) { install.packages("rJava"); require("rJava") }

#if (!require("mclust")) { install.packages("mclust"); require("mclust") }

library(gridExtra)

PlotsOutput = T

dataDir = "C:/Users/stepa/OneDrive/DataScience/Airports/Schedule/AirportsSchedule/Data"
plotDir = "C:/Users/stepa/OneDrive/DataScience/Airports/Schedule/AirportsSchedule/Plots"

#ddd = c(983245, 1230601, 1501318)
#diff(ddd)[2] / diff(ddd)[1]


# Parsing Airports of Regions ----
#u = "http://rov.aero/raspisanie_reysov"
#u = file.path(paste(dataDir, "/Data/", "/Platov_12.06.html", sep = ""))
           
u_Platov = paste(dataDir, "/Platov_13.08.html", sep ="")
u_Samara = paste(dataDir, "/Kuf_13.08.html", sep = "")
u_Ekb    = paste(dataDir, "/SVX_13.08.html", sep = "")
u_NiNo   = paste(dataDir, "/Goj_13.08.html", sep = "")


u = u_Ekb
AllDayRepeats = function(from1, to1, i) {
    df_dates = seq(from1, to1, by = "days")
    df_dates_2 = as.integer(format(seq(from1, to1, by = "days"), "%w"))
    df_dates_2[df_dates_2 == 0] = 7
    return(df_dates[df_dates_2 == i])
}

AR_schedule_parser = function(u, Geo, from2, to2) {
    tables = read_html(u, options = c("HUGE"))
    tables2 = html_nodes(tables, "table")

    city = as.character(html_nodes(tables, "caption"))
    city = gsub("<caption>", "", city)
    city = gsub("</caption>", "", city)

    li = html_table(tables, fill = T)

    df_tmp = data.frame(Рейс = NA, Самолёт = NA, Направление = NA, Расписание = "2000-01-01 00:00:00", stringsAsFactors = F)
    df_tmp[[4]] = as.POSIXlt(df_tmp[, 4], tz = "GMT")


    j = 54
    for (j in c(2:(length(city)+1))) {
    #length(html_table(tables, fill = T)))) {
        df2 = html_table(tables, fill = T)[[j]]

        flight = as.character(html_nodes(tables2[j], "font"))
        flight = gsub("<font class=\"daflight\">","",flight)
        flight = gsub("</font>", "", flight)

        jet = NULL
        for (k in c(1:dim(df2)[1])) {
            jet = c(jet,gsub(pattern = flight[k], replacement = "", df2[k, 1]))
        }


        data.frame(
            Рейс = df2[, 1],
            Расписание = substr(df2[, 3], 1, 5)
            )

        tmp = gsub("период", "", df2[, 5])

        from = substr(tmp, 1, 5)
        to = substr(tmp, 7, 11)

        from1 = as.POSIXlt(paste(from, ".", year, sep = ""), format = "%d.%m.%Y")
        to1 = as.POSIXlt(paste(to, ".", year, sep = ""), format = "%d.%m.%Y")


        #AllDayRepeats( from1[1], to1[1], 4)

        #as.POSIXlt(paste(AllDayRepeats(from1[1], to1[1], 4), " ", substr(df2[1, 3], 1, 5), ":00", sep = ""), tz = "GMT")

        #Sys.timezone(location = TRUE)


        #i = 9
        for (i in c(1:dim(df2)[1])) {
            if (length(grep("пн", gsub("дни прибытия рейсов", "", df2[i, 6]))) > 0) {
                if (length(AllDayRepeats(from1[i], to1[i], 1)) > 0) {
                    df_tmp = rbind(df_tmp, data.frame(
                        Рейс = flight[i],
                        Самолёт = jet[i],
                        Направление = city[j-1],
                        Расписание = as.POSIXlt(paste(AllDayRepeats(from1[i], to1[i], 1), " ", substr(df2[i, 3], 1, 5), ":00", sep = ""), tz = "GMT"), stringsAsFactors = F))
                }
            }
            if (length(grep("вт", gsub("дни прибытия рейсов", "", df2[i, 6]))) > 0) {
                if (length(AllDayRepeats(from1[i], to1[i], 2)) > 0) {
                    df_tmp = rbind(df_tmp, data.frame(
                        Рейс = flight[i],
                        Самолёт = jet[i],
                        Направление = city[j-1],
                        Расписание = as.POSIXlt(paste(AllDayRepeats(from1[i], to1[i], 2), " ", substr(df2[i, 3], 1, 5), ":00", sep = ""), tz = "GMT"), stringsAsFactors = F))
                }
            }
            if (length(grep("ср", gsub("дни прибытия рейсов", "", df2[i, 6]))) > 0) {
                if (length(AllDayRepeats(from1[i], to1[i], 3)) > 0) {
                    df_tmp = rbind(df_tmp, data.frame(
                        Рейс = flight[i],
                        Самолёт = jet[i],
                        Направление = city[j-1],
                        Расписание = as.POSIXlt(paste(AllDayRepeats(from1[i], to1[i], 3), " ", substr(df2[i, 3], 1, 5), ":00", sep = ""), tz = "GMT"), stringsAsFactors = F))
                }
            }
            if (length(grep("чт", gsub("дни прибытия рейсов", "", df2[i, 6]))) > 0) {
                if (length(AllDayRepeats(from1[i], to1[i], 4)) > 0) {
                    df_tmp = rbind(df_tmp, data.frame(
                        Рейс = flight[i],
                        Самолёт = jet[i],
                        Направление = city[j-1],
                        Расписание = as.POSIXlt(paste(AllDayRepeats(from1[i], to1[i], 4), " ", substr(df2[i, 3], 1, 5), ":00", sep = ""), tz = "GMT"), stringsAsFactors = F))
                }
            }
            if (length(grep("пт", gsub("дни прибытия рейсов", "", df2[i, 6]))) > 0) {
                if (length(AllDayRepeats(from1[i], to1[i], 5)) > 0) {
                    df_tmp = rbind(df_tmp, data.frame(
                        Рейс = flight[i],
                        Самолёт = jet[i],
                        Направление = city[j-1],
                        Расписание = as.POSIXlt(paste(AllDayRepeats(from1[i], to1[i], 5), " ", substr(df2[i, 3], 1, 5), ":00", sep = ""), tz = "GMT"), stringsAsFactors = F))
                }
            }
            if (length(grep("сб", gsub("дни прибытия рейсов", "", df2[i, 6]))) > 0) {
                if (length(AllDayRepeats(from1[i], to1[i], 6)) > 0) {
                    df_tmp = rbind(df_tmp, data.frame(
                        Рейс = flight[i],
                        Самолёт = jet[i],
                        Направление = city[j-1],
                        Расписание = as.POSIXlt(paste(AllDayRepeats(from1[i], to1[i], 6), " ", substr(df2[i, 3], 1, 5), ":00", sep = ""), tz = "GMT"), stringsAsFactors = F))
                }
            }
            if (length(grep("вс", gsub("дни прибытия рейсов", "", df2[i, 6]))) > 0) {
                if (length(AllDayRepeats(from1[i], to1[i], 7)) > 0) {
                    df_tmp = rbind(df_tmp, data.frame(
                        Рейс = flight[i],
                        Самолёт = jet[i],
                        Направление = city[j-1],
                        Расписание = as.POSIXlt(paste(AllDayRepeats(from1[i], to1[i], 7), " ", substr(df2[i, 3], 1, 5), ":00", sep = ""), tz = "GMT"), stringsAsFactors = F))
                }
            }
        }

        #sort( df_tmp[-c(1),], Расписание )

        #gsub("дни прибытия рейсов", "", df2[, 6])

    }
    df_tmp = df_tmp[-c(1),]


    df3 = cbind(df_tmp[, 1], df_tmp[, 4], data.frame(Ожидается = rep(NA, dim(df_tmp)[1]), Статус = rep(NA, dim(df_tmp)[1]), Авиакомпания = substr( df_tmp$`Рейс`, 1, 2) ), df_tmp[, 1], df_tmp[, 3], df_tmp[, 2])
    colnames(df3) = c("рейс", "расписание", "ожидаемое время", "статус", "Авиакомпания", "Номер рейса", "Направление", "Самолёт")

    df = df3[(df3$`расписание` >= as.POSIXct(from2, tz = "GMT")) & (df3$`расписание` <= as.POSIXct(to2, tz = "GMT")),]

    df = df[order(df$расписание),]

    df = df[!duplicated(df[, c(2, 7, 8)]), ]
    df$`Направление` = as.character(df$`Направление`)
    df$`Авиакомпания` = as.character(df$`Авиакомпания`)
    df$`Самолёт` = as.character(df$`Самолёт`)

    df$`Самолёт` = gsub("A-", "Airbus A", df$`Самолёт`)
    df$`Самолёт` = gsub("EMB-", "Embraer ", df$`Самолёт`)
    df$`Самолёт` = gsub("B-", "Boeing ", df$`Самолёт`)
    df$`Самолёт` = gsub("CRJ-", "Bombardier CRJ", df$`Самолёт`)
    df$`Самолёт` = gsub("RRJ-95LR", "Сухой Суперджет 100", df$`Самолёт`)
    df$`Самолёт` = gsub("RRJ-95B", "Сухой Суперджет 100", df$`Самолёт`)

    # IATA codes http://www.iata.org/publications/Pages/code-search.aspx
    df$`Авиакомпания` = gsub("SU", "Аэрофлот", df$`Авиакомпания`)
    df$`Авиакомпания` = gsub("A4", "Азимут", df$`Авиакомпания`)
    df$`Авиакомпания` = gsub("FV", "Россия", df$`Авиакомпания`)
    df$`Авиакомпания` = gsub("DP", "Победа", df$`Авиакомпания`)
    df$`Авиакомпания` = gsub("UT", "UTair", df$`Авиакомпания`)
    df$`Авиакомпания` = gsub("U6", "Ур.Авиа", df$`Авиакомпания`)
    df$`Авиакомпания` = gsub("YC", "Ямал", df$`Авиакомпания`)

    df$`Авиакомпания` = gsub("4G", "Газпром", df$`Авиакомпания`)
    df$`Авиакомпания` = gsub("7J", "Таджик", df$`Авиакомпания`)
    df$`Авиакомпания` = gsub("7R", "RusLine", df$`Авиакомпания`)
    df$`Авиакомпания` = gsub("8Q", "OnurAir", df$`Авиакомпания`)
    df$`Авиакомпания` = gsub("9U", "Молдавия", df$`Авиакомпания`)
    df$`Авиакомпания` = gsub("AY", "FinnAir", df$`Авиакомпания`)
    df$`Авиакомпания` = gsub("EL", "EllinAir", df$`Авиакомпания`)

    df$`Авиакомпания` = gsub("FZ", "FlyDubai", df$`Авиакомпания`)
    df$`Авиакомпания` = gsub("HY", "UzberAir", df$`Авиакомпания`)
    df$`Авиакомпания` = gsub("KC", "Astana", df$`Авиакомпания`)
    df$`Авиакомпания` = gsub("KK", "AtlasGlobal", df$`Авиакомпания`)
    df$`Авиакомпания` = gsub("N4", "NordWind", df$`Авиакомпания`)
    df$`Авиакомпания` = gsub("OK", "CzechAir", df$`Авиакомпания`)
    df$`Авиакомпания` = gsub("R3", "Якутия", df$`Авиакомпания`)

    df$`Авиакомпания` = gsub("WZ", "RedWings", df$`Авиакомпания`)
    df$`Авиакомпания` = gsub("XC", "Corendon", df$`Авиакомпания`)
    df$`Авиакомпания` = gsub("YK", "AviaTraffic", df$`Авиакомпания`)
    df$`Авиакомпания` = gsub("ZF", "Azur", df$`Авиакомпания`)

    if (Geo) {
        # Направления разлёта
        nrow <- nrow(df)
        counter <- 1
        df$lon[counter] <- 0
        df$lat[counter] <- 0
        while (counter <= nrow) {
            CityName <- gsub(' ', '%20', df$`Направление`[counter]) #remove space for URLs
            CountryCode <- ""
            url <- paste(
                        "http://nominatim.openstreetmap.org/search?city="
                        , CityName
                        #    , "&countrycodes="
                        #    , CountryCode
                        , "&limit=9&format=json"
                        , sep = "")
            x <- fromJSON(url)
            if (is.vector(x)) {
                df$lon[counter] <- x[[1]]$lon
                df$lat[counter] <- x[[1]]$lat
            }
            counter <- counter + 1
        }

        df3 = as.data.frame(table(df$`Направление`))

        for (i in c(1:nrow(df3))) {
            df[df$`Направление` == df3[i, 1], 11] = df3[i, 2]
        }



        df$lat = as.numeric(as.character(df$lat))
        df$lon = as.numeric(as.character(df$lon))
    }

    return(df)
}

year    = "2018"
FromDay = "2018-09-17 00:00:00"
ToDay = "2018-09-23 23:59:59"

df_Platov = AR_schedule_parser(u_Platov, F, FromDay, ToDay)
df_Samara = AR_schedule_parser(u_Samara, F, FromDay, ToDay)
df_Ekb    = AR_schedule_parser(u_Ekb,    F, FromDay, ToDay)
df_NiNo   = AR_schedule_parser(u_NiNo,   F, FromDay, ToDay)

#write.csv2(df_Platov, file.path(dataDir, paste("TimeTable_Platov_", as.Date(Sys.Date()), " site", ".csv", sep = "")))
#write.csv2(df_Ekb, file.path(dataDir, paste("TimeTable_Ekb_", as.Date(Sys.Date()), " site", ".csv", sep = "")))
#write.csv2(df_Samara, file.path(dataDir, paste("TimeTable_Samara_", as.Date(Sys.Date()), " site", ".csv", sep = "")))
#write.csv2(df_NiNo, file.path(dataDir, paste("TimeTable_NiNo_", as.Date(Sys.Date()), " site", ".csv", sep = "")))





# Parsing Yandex -----

"9600213" # Sheremetievo
"9866615" # Platov
"9600365" # Borispol
"9600396" # Simpferopol
station = "9600396"
airport = "Симферополь"
airport_en = "Simpheropol"
station = "9866615"
airport = "Платов"
airport_en = "Platov"

#Sys.getlocale()
#LANG = "ru_RU.65001"
#LANG <- Sys.getenv("LANG")
#Sys.setlocale("LC_ALL", "Russian_Russia.65001")
#Sys.setlocale("LC_ALL", "Russian_Russia.20866")
#Sys.setlocale("LC_ALL", "Russian_Russia.1251")

# u = "http://en.wikipedia.org/wiki/World_population"
#u = "https://rasp.yandex.ru/station/9866615?start=2018-05-05T00%3A00%3A00&span=24"
#u = "https://raspisanie.msk.ru/plane/s9866615"
#u = "https://ru.wikipedia.org/wiki/Список_наиболее_загруженных_аэропортов_России"

#u <- getURL(u)

# Yandex Rasp
sDate = Sys.Date() + 1
#u = paste( "https://rasp.yandex.ru/station/9866615?start=", Sys.Date()+1, "T00%3A00%3A00&span=24", sep = "")

# Tables
#i = 13
#u = paste("https://rasp.yandex.ru/station/", station, "?start=", Sys.Date() + 1 + i, "T00%3A00%3A00&span=24", sep = "")
#sDate = Sys.Date() + 1 + i

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

    #str_split_fixed(html_text(html_nodes(df_div3, "a")), " — ", 3)[1, 1]
    df$`Направление` = str_split_fixed( html_text(html_nodes(df_div3, "a")), " — ", 3)[, 2]

    df_div4 = html_nodes(tables, xpath = '//*[@class="b-timetable__description"]')
    df$`Самолёт` = str_split_fixed( html_text( df_div4 ), ", ", 2)[, 1]

    #df = df[ , c(7,5,2,6, 8)]
    df$`расписание`[1] = strsplit(df$`расписание`[1], ", ")[[1]][1]

    df$`расписание` = as.POSIXlt(paste( sDate, df$`расписание`))

    return(df)
}


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

    A321_1 = "Airbus А321"
    A321_2 = "Airbus A321"
    df$`Самолёт`[df$`Самолёт` == A321_2] = A321_1

    return(df)
}





df = OneWeek_TimeTable(station)

# Направления разлёта
nrow <- nrow(df)
counter <- 1
df$lon[counter] <- 0
df$lat[counter] <- 0
while (counter <= nrow) {
    CityName <- gsub(' ', '%20', df$`Направление`[counter]) #remove space for URLs
    CountryCode <- ""
    url <- paste(
    "http://nominatim.openstreetmap.org/search?city="
    , CityName
    #    , "&countrycodes="
    #    , CountryCode
    , "&limit=9&format=json"
    , sep = "")
    x <- fromJSON(url)
    if (is.vector(x)) {
        df$lon[counter] <- x[[1]]$lon
        df$lat[counter] <- x[[1]]$lat
    }
    counter <- counter + 1
}

df3 = as.data.frame(table(df$`Направление`))

for (i in c(1:nrow(df3))) {
    df[df$`Направление` == df3[i, 1], 11] = df3[i, 2]
}



df$lat = as.numeric(as.character(df$lat))
df$lon = as.numeric(as.character(df$lon))
df$`расписание` = as.POSIXct(df$`расписание`)
df$`Направление` = as.character(df$`Направление`)
colnames(df)[11] = "Частота"

write.csv2(df, file.path(dataDir, paste("TimeTable_", airport_en, "_", as.Date(Sys.Date()), ".csv", sep = "")))

#df = read.csv2( file.path(dataDir, "TimeTable_2018-05-25.csv"))[,c(2:12)]

df_Platov = df





# Analysis -----
Freq_plot = function(dff, p_title) {
    return(ggplot(data = dff) +
            geom_bar(stat = "identity", aes(reorder(Var1, Freq), Freq), fill = "cyan4") +
            geom_text(aes(reorder(Var1, Freq), label = Freq, y = Freq), size = 3, hjust = -0.1, colour = "dimgray", vjust = 0.25) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            xlab("") + ylab("") + coord_flip() +
            scale_y_continuous(breaks = round(seq(0, max(dff$Freq), max(dff$Freq) / 25))) +
            theme(plot.margin = unit(c(0, 0.2, 0, 0), "cm")) +
            ggtitle(p_title)
        )
}

Frequncies_Routes_Capacities = function(df, airport, airport_en, PlotsOutput) {

    ########################
    # Frequncies plots

    df_cities = as.data.frame(sort(table(df$`Направление`), decreasing = T))
    df_airlines = as.data.frame(sort(table(df$`Авиакомпания`), decreasing = T))
    df_jets = as.data.frame(sort(table(df$`Самолёт`), decreasing = T))

    p1 = Freq_plot(df_cities, paste("Рейсов из аэропорта", airport, "\nпо направлениям (в неделю)"))
    p1
    p2 = Freq_plot(df_airlines, paste("Количество рейсов из аэропорта ", airport, "\nпо авиакомпаниям (в неделю)"))
    p2
    p3 = Freq_plot(df_jets, paste("Количество рейсов из аэропорта ", airport, "\nпо типам самолётов (в неделю)"))
    p3


    if (PlotsOutput) {
        #pdf(file = file.path(paste(plotDir, "/City_vs_Freq ", Sys.Date(), " ", airport, ".pdf", sep = "")), width = 12, height = 12, pointsize = 10)
        png(filename = file.path(paste(plotDir, "/City_vs_Freq ", Sys.Date(), " ", airport_en, ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "Times", restoreConsole = TRUE, type = c("cairo-png"))
        p1 + annotate(geom = "text", x = 15, y = 80.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 10, alpha = 0.5)
        dev.off()

        png(filename = file.path(plotDir, paste("Airline_vs_Freq ", Sys.Date(), " ", airport_en, ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))
            p2 + annotate(geom = "text", x = 7, y = 25.0, xend = Inf, yend = Inf, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 10, alpha = 0.5, family = 'Arial')
        dev.off()

        png(filename = file.path(plotDir, paste("Jet_vs_Freq ", Sys.Date(), " ", airport_en, ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))
            p3 + annotate(geom = "text", x = 7, y = 35, xend = Inf, yend = Inf, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 7, alpha = 0.5, family = 'Arial')
        dev.off()
    }




    ########################
    # Frequency by Day plot
    wdays = c( "Пн", "Вт", "Ср", "Чт", "Пт", "Сб", "Вс")

    #weekdays(df$`расписание` )

    tab2 = apply(table( as.integer(format(df$`расписание`, format = "%w")), df$`Направление`), FUN = sum, MARGIN = 1)
    df_wd_fly = data.frame(Var1 = as.character(names(tab2)), Freq = tab2 )
    df_wd_fly$Var1 = as.numeric( df_wd_fly$Var1 ) - 1
    df_wd_fly[ df_wd_fly$Var1 == 0, 1] = 7



    dff = df_wd_fly
    p4 = ggplot(data = dff) +
            geom_bar(stat = "identity", aes(Var1, Freq), fill = "cyan4") +
            geom_text(aes(Var1, Freq, label = Freq, y = Freq), size = 3, colour = "dimgray", vjust = -0.3) +
            #        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            xlab("") + ylab("") + # coord_flip() +
            scale_x_continuous(breaks = 1:7, labels = wdays) +
            theme(plot.margin = unit(c(0, 0.2, 0, 0), "cm")) +
            ggtitle( paste("Количество рейсов из аэропорта", airport, "\nпо дням недели")) +
            annotate(geom = "text", x = 4, y = 17, xend = Inf, yend = Inf, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 9, alpha = 0.5, family = 'Arial')
    p4






    tab2 = apply(table(as.integer(format(df$`расписание`, format = "%H")), df$`Направление`), FUN = sum, MARGIN = 1)
    df_h_fly = data.frame(Var1 = as.character(names(tab2)), Freq = tab2)
    df_h_fly$Var1 = as.numeric(df_h_fly$Var1)

    #df_h_fly[df_h_fly$Var1 == 0, 1] = 7



    dff = df_h_fly
    p5 = ggplot(data = dff) +
            geom_bar(stat = "identity", aes(Var1, Freq), fill = "cyan4") +
            geom_text(aes(Var1, Freq, label = Freq, y = Freq), size = 3, colour = "dimgray", vjust = -0.3) +
            #        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            xlab("") + ylab("") + # coord_flip() +
            scale_x_continuous(breaks = df_h_fly$Var1, labels = df_h_fly$Var1) +
            theme(plot.margin = unit(c(0, 0.2, 0, 0), "cm")) +
            ggtitle(paste("Количество рейсов из аэропорта", airport, "\nпо времени дня за неделю")) +
            annotate(geom = "text", x = 11, y = 13, xend = Inf, yend = Inf, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 7, alpha = 0.5, family = 'Arial')
    p5

    if (PlotsOutput) {
        png(filename = file.path(plotDir, paste("Freq_per_day ", Sys.Date(), " ", airport_en, ".png", sep = "")), width = 800, height = 400, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))
        p4
        dev.off()

        png(filename = file.path(plotDir, paste("Freq_per_hour ", Sys.Date(), " ", airport_en, ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))
        p5
        dev.off()
    }


    ###########
    # Capacity 
    # http://www.wsdot.wa.gov/Aviation/planning/EconomicCalc/Documents/AirplaneCapacityTable
    df_capacity = data.frame(
        type = c(
            "Airbus A319", "Airbus A320", "Airbus А321", "Airbus A321", "Airbus A321 SH", "Airbus A330-200", "Airbus A330-300", "Airbus А330-200", "Airbus А330-300",
            "Boeing 737-100", "Boeing 737-200", "Boeing 737-300", "Boeing 737-400", "Boeing 737-500", "Boeing 737-600", "Boeing 737-700", "Boeing 737-800", "Boeing 737-800W", "Boeing 737-900", "Boeing 737-900ER",
            "Boeing 757-200", "Boeing 757-300",
            "Boeing 767-200", "Boeing 767-200ER", "Boeing 767-300", "Boeing 767-300ER", "Boeing 767-400ER",
            "Boeing 777-200", "Boeing 777-200ER", "Boeing 777-200LR", "Boeing 777-300", "Boeing 777-300ER", "Boeing 777-8X", "Boeing 777-9X", "Boeing 747-400",
            "Canadair regional jet", "Bombardier CRJ200",
            "Embraer 170", "Embraer 175", "Embraer EMB 175", "Embraer 190", "Embraer 195",
            "AT-72-5",
            "Сухой Суперджет 100", "ТУ-204", "ЯК-42", "АН-24"),
        capacity = c(
            134, 164, 199, 199, 199, 293, 335, 293, 335,
            103, 133, 149, 168, 132, 130, 148, 189, 189, 189, 215,
            214, 260,
            214, 214, 261, 261, 296,
            300, 300, 300, 365, 365, 365, 415, 416,
            76, 50,
            76, 84, 106, 106, 116,
            74,
            95, 210, 120, 50))

    df_capacity$type = as.character(df_capacity$type)
    df_jets$Var1 = as.character(df_jets$Var1)

    cap = NULL
    for (i in c(1:dim(df_jets)[1])) {
        if (df_jets[i, 1] %in% df_capacity[, 1]) {
            cap = c(cap, df_capacity[df_capacity[, 1] == df_jets[i, 1], 2])
        } else cap = c(cap, 0)
        }

    df_jets$`Вместимость` = cap
    df_jets$`Ёмкость` = df_jets$Freq * df_jets$`Вместимость`


    df_tmp = df_jets[, c(1, 4)]
    colnames(df_tmp) = c("Var1", "Freq")

    p6 = Freq_plot(df_tmp, paste("Ёмкость рейсов из аэропорта", airport, "\nпо типам самолётов (в неделю)")) + annotate(geom = "text", x = 5, y = 5000, xend = Inf, yend = Inf, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 10, alpha = 0.5, family = 'Arial') + ylab(paste("Общая ёмкость по типам (в одном направлении в месяц):", sum(df_jets$`Ёмкость`) * 4))
    p6



    # Пассажиропоток
    sum(df_jets$`Ёмкость`) * 8 * 0.7




    # Ёмкость по направленимям
    df$`Самолёт` = as.character(df$`Самолёт`)
    df$`Кресла` = rep(0, dim(df)[1])
    for (i in c(1:dim(df_capacity)[1])) {
        df$`Кресла`[df$`Самолёт` == df_capacity$type[i]] = df_capacity$capacity[i]
    }

    tab3 = table(df$`Направление`, df$`Кресла`)

    for (i in c(1:dim(tab3)[1])) {
        tab3[i,] = tab3[i,] * as.integer(colnames(tab3))
    }

    df_city_capacity = as.data.frame(sort(apply(tab3, MARGIN = 1, sum), decreasing = T))
    df_city_capacity = cbind(rownames(df_city_capacity), df_city_capacity)
    df_city_capacity[, 1] = as.character(df_city_capacity[, 1])
    colnames(df_city_capacity) = c("Var1", "Freq")

    sum(df_city_capacity$Freq)

    p7 = Freq_plot(df_city_capacity, paste("Ёмкость из аэропорта", airport, "\nпо направлениям (в неделю)")) +
        annotate(geom = "text", x = 15, y = 9000, xend = Inf, yend = Inf, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 10, alpha = 0.5, family = 'Arial') + ylab(paste("Общая ёмкость по типам (в одном направлении в месяц):", sum(df_jets$`Ёмкость`) * 4))
    p7








    # По АК

    #for (i in c(1:dim(df_jets)[1])) {
    #    df[df$`Самолёт` == df_capacity$type[i], 12] = df_capacity$capacity[i]
    #}

    tab3 = table(df$`Авиакомпания`, df$`Кресла`)

    for (i in c(1:dim(tab3)[1])) {
        tab3[i,] = tab3[i,] * as.integer(colnames(tab3))
    }

    df_airline_capacity = as.data.frame(sort(apply(tab3, MARGIN = 1, sum), decreasing = T))
    df_airline_capacity = cbind(rownames(df_airline_capacity), df_airline_capacity)
    df_airline_capacity[, 1] = as.character(df_airline_capacity[, 1])
    colnames(df_airline_capacity) = c("Var1", "Freq")

    sum(df_airline_capacity$Freq)

    p8 = Freq_plot(df_airline_capacity, paste("Ёмкость из аэропорта", airport, "\nпо авиакомпаниям (в неделю)")) +
        annotate(geom = "text", x = 9, y = 3000, xend = Inf, yend = Inf, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 10, alpha = 0.5, family = 'Arial') + ylab(paste("Общая ёмкость по типам (в одном направлении в месяц):", sum(df_jets$`Ёмкость`) * 4))
    p8


    if (PlotsOutput) {
        png(filename = file.path(plotDir, paste("Jet_vs_Capacity ", Sys.Date(), " ", airport_en, ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))
        p6
        dev.off()

        png(filename = file.path(plotDir, paste("City_vs_Capacity ", Sys.Date(), " ", airport_en, ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))
        p7
        dev.off()

        png(filename = file.path(plotDir, paste("Airline_vs_Capacity ", Sys.Date(), " ", airport_en, ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))
        p8
        dev.off()

    }

    return(list(
        data.frame(City = df_cities[, 1], Freq = df_cities[, 2], Capacity = df_city_capacity[, 2]),
        data.frame(City = df_airlines[order(as.character(df_airlines$Var1)), 1], Freq = df_airlines[order(as.character(df_airlines$Var1)), 2], Capacity = df_airline_capacity[order(df_airline_capacity$Var1), 2]),
        df_jets,
        df_wd_fly, df_h_fly,
        p1, p2, p3, p4, p5, p6, p7, p8))
}


Analysis_Rostov = Frequncies_Routes_Capacities( df_Platov, "Ростов", "Platov", T )
Analysis_Ekb    = Frequncies_Routes_Capacities( df_Ekb, "Екб", "Ekb", F )
Analysis_Samara = Frequncies_Routes_Capacities( df_Samara, "Самара", "Samara", F )
Analysis_NiNo   = Frequncies_Routes_Capacities( df_NiNo, "НиНо", "NiNo", F )

#Analysis_Sim = Frequncies_Routes_Capacities(df_Sim, "Симф", "Simf", F)




# AR group comparison plots ----

# Routes number
df_RoutesN = data.frame(
    Var1 = c("Екб", "Ростов", "Самара", "НиНо"),
    Freq = c(    dim(Analysis_Ekb[[1]])[1],
                    dim(Analysis_Rostov[[1]])[1],
                    dim(Analysis_Samara[[1]])[1],
                    dim(Analysis_NiNo[[1]])[1]))
p1 = Freq_plot( df_RoutesN, "Число направлений по городам")

# Airlines number
df_AirlinesN = data.frame(
    Var1 = c("Екб", "Ростов", "Самара", "НиНо"),
    Freq = c(dim(Analysis_Ekb[[2]])[1],
                    dim(Analysis_Rostov[[2]])[1],
                    dim(Analysis_Samara[[2]])[1],
                    dim(Analysis_NiNo[[2]])[1]))
p2 = Freq_plot(df_AirlinesN, "Число АК по городам")

# Jets number
df_JetsN = data.frame(
    Var1 = c("Екб", "Ростов", "Самара", "НиНо"),
    Freq = c(dim(Analysis_Ekb[[3]])[1],
                    dim(Analysis_Rostov[[3]])[1],
                    dim(Analysis_Samara[[3]])[1],
                    dim(Analysis_NiNo[[3]])[1]))
p3 = Freq_plot(df_JetsN, "Число типов по городам")

grid.arrange(p1, p2, p3, ncol = 3)



# Main jet type
df_JetMain = data.frame(
    Var1 = c("Екб", "Ростов", "Самара", "НиНо"),
    Freq = c( Analysis_Ekb[[3]][1,1],
              Analysis_Rostov[[3]][1, 1],
              Analysis_Samara[[3]][1, 1],
              Analysis_NiNo[[3]][1, 1]))

library(stringr)
df_AB_competition = rbind(
    data.frame(
        City = "Екб",
        Airbus = sum(str_count(df_Ekb$`Самолёт`, pattern = "Airbus")) / dim(df_Ekb)[1] * 100,
        Boeing = sum(str_count(df_Ekb$`Самолёт`, pattern = "Boeing")) / dim(df_Ekb)[1] * 100,
        Bombardier = sum(str_count(df_Ekb$`Самолёт`, pattern = "Bombardier")) / dim(df_Ekb)[1] * 100,
        Embraer = sum(str_count(df_Ekb$`Самолёт`, pattern = "Embraer")) / dim(df_Ekb)[1] * 100,
        Сухой = sum(str_count(df_Ekb$`Самолёт`, pattern = "Сухой")) / dim(df_Ekb)[1] * 100,
        ATR = sum(str_count(df_Ekb$`Самолёт`, pattern = "AT")) / dim(df_Ekb)[1] * 100),
    data.frame(
        City = "Ростов",
        Airbus = sum(str_count(df_Platov$`Самолёт`, pattern = "Airbus")) / dim(df_Platov)[1] * 100,
        Boeing = sum(str_count(df_Platov$`Самолёт`, pattern = "Boeing")) / dim(df_Platov)[1] * 100,
        Bombardier = sum(str_count(df_Platov$`Самолёт`, pattern = "Bombardier")) / dim(df_Platov)[1] * 100,
        Embraer = sum(str_count(df_Platov$`Самолёт`, pattern = "Embraer")) / dim(df_Platov)[1] * 100,
        Сухой = sum(str_count(df_Platov$`Самолёт`, pattern = "Сухой")) / dim(df_Platov)[1] * 100,
        ATR = sum(str_count(df_Platov$`Самолёт`, pattern = "AT")) / dim(df_Platov)[1] * 100),
    data.frame(
        City = "Самара",
        Airbus = sum(str_count(df_Samara$`Самолёт`, pattern = "Airbus")) / dim(df_Samara)[1] * 100,
        Boeing = sum(str_count(df_Samara$`Самолёт`, pattern = "Boeing")) / dim(df_Samara)[1] * 100,
        Bombardier = sum(str_count(df_Samara$`Самолёт`, pattern = "Bombardier")) / dim(df_Samara)[1] * 100,
        Embraer = sum(str_count(df_Samara$`Самолёт`, pattern = "Embraer")) / dim(df_Samara)[1] * 100,
        Сухой = sum(str_count(df_Samara$`Самолёт`, pattern = "Сухой")) / dim(df_Samara)[1] * 100,
        ATR = sum(str_count(df_Samara$`Самолёт`, pattern = "AT")) / dim(df_Samara)[1] * 100),
    data.frame(
        City = "НиНо",
        Airbus = sum(str_count(df_NiNo$`Самолёт`, pattern = "Airbus")) / dim(df_NiNo)[1] * 100,
        Boeing = sum(str_count(df_NiNo$`Самолёт`, pattern = "Boeing")) / dim(df_NiNo)[1] * 100,
        Bombardier = sum(str_count(df_NiNo$`Самолёт`, pattern = "Bombardier")) / dim(df_NiNo)[1] * 100,
        Embraer = sum(str_count(df_NiNo$`Самолёт`, pattern = "Embraer")) / dim(df_NiNo)[1] * 100,
        Сухой = sum(str_count(df_NiNo$`Самолёт`, pattern = "Сухой")) / dim(df_NiNo)[1] * 100,
        ATR = sum(str_count(df_NiNo$`Самолёт`, pattern = "AT")) / dim(df_NiNo)[1] * 100))

# melt the data frame for plotting
library(reshape2)
data.m <- melt(df_AB_competition, id.varСухой = 'City')
colnames( data.m ) = c("Город","Тип","Доля")

# plot everything
p4 = ggplot(data.m, aes(Город, Доля)) +
    geom_bar(aes(fill = Тип), stat = "identity", position = "dodge") + xlab("") + ylab("Доля") +
    geom_text(aes(fill = Тип, label = round(Доля), y = round(Доля)), size = 3, hjust = 0.3, colour = "dimgray", vjust = -0.3,
    position = position_dodge(width = 0.9)) +
    theme(plot.margin = unit(c(0, 0.2, 0, 0), "cm"))# + ggtitle("Доли по типам")

p4


grid.arrange(Analysis_Ekb[[9]], Analysis_Rostov[[9]], Analysis_Samara[[9]], Analysis_NiNo[[9]], ncol = 2)
grid.arrange(Analysis_Ekb[[10]], Analysis_Rostov[[10]], Analysis_Samara[[10]], Analysis_NiNo[[10]], ncol = 2)

grid.arrange(Analysis_Ekb[[6]], Analysis_Rostov[[6]], Analysis_Samara[[6]], Analysis_NiNo[[6]], ncol = 2)
grid.arrange(Analysis_Ekb[[7]], Analysis_Rostov[[7]], Analysis_Samara[[7]], Analysis_NiNo[[7]], ncol = 2)
grid.arrange(Analysis_Ekb[[8]], Analysis_Rostov[[8]], Analysis_Samara[[8]], Analysis_NiNo[[8]], ncol = 2)

grid.arrange(Analysis_Ekb[[11]], Analysis_Rostov[[11]], Analysis_Samara[[11]], Analysis_NiNo[[13]], ncol = 2)
grid.arrange(Analysis_Ekb[[12]], Analysis_Rostov[[12]], Analysis_Samara[[12]], Analysis_NiNo[[13]], ncol = 2)
grid.arrange(Analysis_Ekb[[13]], Analysis_Rostov[[13]], Analysis_Samara[[13]], Analysis_NiNo[[13]], ncol = 2)




if (PlotsOutput) {
    library(gridExtra)

    # General frequencies
    png(filename = file.path(plotDir, paste("AR frequencies ", Sys.Date(), ".png", sep = "")), width = 1280, height = 300, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))
    
    grid.arrange(
        p1 + annotate(geom = "text", x = 3, y = 35.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 13, alpha = 0.5),
        p2 + annotate(geom = "text", x = 3, y = 15.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 13, alpha = 0.5),
        p3 + annotate(geom = "text", x = 3, y = 9.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 13, alpha = 0.5),
        ncol = 3)

    dev.off()


    # Types of jets shares
    png(filename = file.path(plotDir, paste("AR jets ", Sys.Date(), ".png", sep = "")), width = 640, height = 300, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))
        p4 + annotate(geom = "text", x = 2.5, y = 25.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 13, alpha = 0.5)
    dev.off()

    # Frequencies by day and hour
    png(filename = file.path(plotDir, paste("AR days freqs ", Sys.Date(), ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = 60, family = "", restoreConsole = TRUE, type = c("windows"))
        grid.arrange(
            Analysis_Ekb[[9]] + annotate(geom = "text", x = 3.5, y = 25.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            Analysis_NiNo[[9]] + annotate(geom = "text", x = 3.5, y = 25.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            Analysis_Rostov[[9]] + annotate(geom = "text", x = 3.5, y = 25.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            Analysis_Samara[[9]] + annotate(geom = "text", x = 3.5, y = 25.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            ncol = 2)
    dev.off()

    png(filename = file.path(plotDir, paste("AR hours freqs ", Sys.Date(), ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = 60, family = "", restoreConsole = TRUE, type = c("windows"))
        grid.arrange(
            Analysis_Ekb[[10]] + annotate(geom = "text", x = 3.5, y = 25.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            Analysis_NiNo[[10]] + annotate(geom = "text", x = 3.5, y = 25.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            Analysis_Rostov[[10]] + annotate(geom = "text", x = 3.5, y = 25.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            Analysis_Samara[[10]] + annotate(geom = "text", x = 3.5, y = 25.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            ncol = 2)
    dev.off()

    # Cities, airlines, jets frequencies
    png(filename = file.path(plotDir, paste("AR cities freqs ", Sys.Date(), ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = 60, family = "", restoreConsole = TRUE, type = c("windows"))#,    compression = "lzw")
        grid.arrange(
            Analysis_Ekb[[6]] + annotate(geom = "text", x = 35, y = 25.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            Analysis_NiNo[[6]] + annotate(geom = "text", x = 10, y = 25.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            Analysis_Rostov[[6]] + annotate(geom = "text", x = 25, y = 25.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            Analysis_Samara[[6]] + annotate(geom = "text", x = 25, y = 25.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            ncol = 2)
    dev.off()


    png(filename = file.path(plotDir, paste("AR airlines freqs ", Sys.Date(), ".png", sep = "")), width = 1280, height = 1280, units = "px", pointsize = 12, bg = "white", res = 72, family = "", restoreConsole = TRUE, type = c("windows")) #,    compression = "lzw")
        grid.arrange(
            Analysis_Ekb[[7]] + annotate(geom = "text", x = 15, y = 75.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            Analysis_NiNo[[7]] + annotate(geom = "text", x = 10, y = 25.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            Analysis_Rostov[[7]] + annotate(geom = "text", x = 15, y = 25.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            Analysis_Samara[[7]] + annotate(geom = "text", x = 15, y = 25.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            ncol = 2)
    dev.off()

    png(filename = file.path(plotDir, paste("AR jets freqs ", Sys.Date(), ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = 72, family = "", restoreConsole = TRUE, type = c("windows")) #,    compression = "lzw")
        grid.arrange(
            Analysis_Ekb[[8]] + annotate(geom = "text", x = 10, y = 75.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            Analysis_NiNo[[8]] + annotate(geom = "text", x = 10, y = 15.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            Analysis_Rostov[[8]] + annotate(geom = "text", x = 5, y = 40.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            Analysis_Samara[[8]] + annotate(geom = "text", x = 10, y = 30.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            ncol = 2)
    dev.off()





    # Cities, airlines, jets frequencies
    png(filename = file.path(plotDir, paste("AR jets capacities ", Sys.Date(), ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = 72, family = "", restoreConsole = TRUE, type = c("windows")) #,    compression = "lzw")
    grid.arrange(
            Analysis_Ekb[[11]] + annotate(geom = "text", x = 15, y = 25.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            Analysis_NiNo[[11]] + annotate(geom = "text", x = 10, y = 25.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            Analysis_Rostov[[11]] + annotate(geom = "text", x = 5, y = 25.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            Analysis_Samara[[11]] + annotate(geom = "text", x = 15, y = 25.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            ncol = 2)
    dev.off()


    png(filename = file.path(plotDir, paste("AR cities capacities ", Sys.Date(), ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = 72, family = "", restoreConsole = TRUE, type = c("windows")) #,    compression = "lzw")
    grid.arrange(
            Analysis_Ekb[[12]] + annotate(geom = "text", x = 15, y = 75.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            Analysis_NiNo[[12]] + annotate(geom = "text", x = 10, y = 15.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            Analysis_Rostov[[12]] + annotate(geom = "text", x = 15, y = 25.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            Analysis_Samara[[12]] + annotate(geom = "text", x = 15, y = 25.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            ncol = 2)
    dev.off()

    png(filename = file.path(plotDir, paste("AR airlines capacities ", Sys.Date(), ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = 72, family = "", restoreConsole = TRUE, type = c("windows")) #,    compression = "lzw")
    grid.arrange(
            Analysis_Ekb[[13]] + annotate(geom = "text", x = 10, y = 75.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            Analysis_NiNo[[13]] + annotate(geom = "text", x = 10, y = 15.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            Analysis_Rostov[[13]] + annotate(geom = "text", x = 5, y = 40.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            Analysis_Samara[[13]] + annotate(geom = "text", x = 10, y = 30.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 22, alpha = 0.5),
            ncol = 2)
    dev.off()

}




# Mosaic plot ----
if (!require("vcd")) { install.packages("vcd"); require("vcd") }

tab = table(df$`Авиакомпания`, df$`Направление`)

#mosaic(df$`Авиакомпания` ~ df$`Направление`)

tab = tab[as.character(df_airlines$Var1), as.character(df_cities$Var1)]
#df_cross = as.data.frame.matrix(tab[1:5, 1:5])

#mosaic(tab, legend = T, zero_size = 0, zero_split = T)
#strucplot(tab[1:5, 1:5], legend = T, zero_size = 0, labeling = T)

#mosaic(~`Направление` + `Авиакомпания`, data = df, zero_size = 0, zero_split = F, keep_aspect_ratio = F, ctx = 0.2)
#mosaic(~`Авиакомпания` + `Направление`, data = df, zero_size = 0, legend = T)

#
mosaicplot(tab, las = 2, col = "cyan4", main = "")
mosaicplot(t(tab), las = 2, col = "cyan4", main = "")

if (PlotsOutput) {
    png(filename = file.path(plotDir, paste("Mosaic_Airline_vs_City ", Sys.Date(), " ", airport_en, ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png")) 

    par(mar = rep(.5, 4))

    mosaicplot(tab, las = 2, col = "cyan4", main = "", cex = 1.1)
    text(x = grconvertX(0.5, from = "npc"), y = grconvertY(0.5, from = "npc"),
        labels = "Степан Сушко", cex = 3, font = 2, col = adjustcolor("steelblue", alpha.f = 0.35), srt = 45)
    dev.off()

    png(filename = file.path(plotDir, paste("Mosaic_City_vs_Airline ", Sys.Date(), " ", airport_en, ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))

    par(mar = rep(.5, 4))

    mosaicplot(t(tab), las = 2, col = "cyan4", main = "", cex = 1.1)
    text(x = grconvertX(0.5, from = "npc"), y = grconvertY(0.5, from = "npc"),
            labels = "Степан Сушко", cex = 3, font = 2, col = adjustcolor("steelblue", alpha.f = 0.35), srt = 45)
    dev.off()
}

# АК vs Самолёт
tab2 = table(df$`Авиакомпания`, df$`Самолёт`)
tab2 = tab2[as.character(df_airlines$Var1), as.character(df_jets$Var1)]

mosaicplot(tab2, las = 2, col = "cyan4", main = "")
mosaicplot(t(tab2), las = 2, col = "cyan4", main = "")

if (PlotsOutput) {
    png(filename = file.path(plotDir, paste("Mosaic_Airline_vs_Jet ", Sys.Date(), " ", airport_en, ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))

    par(mar = rep(.5, 4))
    mosaicplot(tab2, las = 2, col = "cyan4", main = "", cex = 1.1)

    text(   x = grconvertX(0.5, from = "npc"), y = grconvertY(0.5, from = "npc"),
            labels = "Степан Сушко", cex = 3, font = 2, col = adjustcolor("steelblue", alpha.f = 0.35), srt = 45)

    dev.off()




    png(filename = file.path(plotDir, paste("Mosaic_Jet_vs_Airline ", Sys.Date(), " ", airport_en, ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))

    par(mar = rep(.5, 4))

    mosaicplot( t(tab2), las = 2, col = "cyan4", main = "", cex = 1.1)

    text(x = grconvertX(0.5, from = "npc"), # align to center of plot X axis
           y = grconvertY(0.5, from = "npc"), # align to center of plot Y axis
            labels = "Степан Сушко", # our watermark
            cex = 3, font = 2, # large, bold font - hard to miss
            col = adjustcolor("steelblue", alpha.f = 0.35), # translucent (0.2 = 20%) red color
            srt = 45) # srt = angle of text: 45 degree angle to X axis

    dev.off()
}



# АК vs Самолёт
tab3 = table(df$`Направление`, df$`Самолёт`)
tab3 = tab3[as.character(df_cities$Var1), as.character(df_jets$Var1)]

mosaicplot(tab3, las = 2, col = "cyan4", main = "")
mosaicplot(t(tab3), las = 2, col = "cyan4", main = "")

if (PlotsOutput) {
    png(filename = file.path(plotDir, paste("Mosaic_Airline_vs_Jet ", Sys.Date(), " ", airport_en, ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))

    par(mar = rep(.5, 4))
    mosaicplot(tab3, las = 2, col = "cyan4", main = "")

    text(x = grconvertX(0.5, from = "npc"), y = grconvertY(0.5, from = "npc"),
            labels = "Степан Сушко", cex = 3, font = 2, col = adjustcolor("steelblue", alpha.f = 0.35), srt = 45)

    dev.off()




    png(filename = file.path(plotDir, paste("Mosaic_Jet_vs_Airline ", Sys.Date(), " ", airport_en, ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))

    par(mar = rep(.5, 4))

    mosaicplot(t(tab3), las = 2, col = "cyan4", main = "", cex = 1.1)

    text(x = grconvertX(0.5, from = "npc"), # align to center of plot X axis
           y = grconvertY(0.5, from = "npc"), # align to center of plot Y axis
            labels = "Степан Сушко", # our watermark
            cex = 3, font = 2, # large, bold font - hard to miss
            col = adjustcolor("steelblue", alpha.f = 0.35), # translucent (0.2 = 20%) red color
            srt = 45) # srt = angle of text: 45 degree angle to X axis

    dev.off()
}


# График разлёта (Bubble plot) ----

#if (!require("ggmap")) { install.packages("ggmap"); require("ggmap") }

#main_airport = geocode( airport_en, override_limit = T)

if (!require("RgoogleMaps")) { install.packages("RgoogleMaps"); require("RgoogleMaps") }

map <- GetMap(center = c( main_airport$lat, main_airport$lon), zoom = 3,
       size = c(640, 640), destfile = file.path(tempdir(), "meuse.png"),
        maptype = "mobile", SCALE = 1);

df = df_Platov
airport_en = "Platov"

df2 = unique(df[, c(9:11)])
#df2 = rbind(df2, c(main_airport$lon, main_airport$lat, sum(df2$`Частота`)))
df2 = df2[df2$lon != 0,]

bubbleMap(
          df2, coords = c("lon", "lat"), map, zcol = "Частота",
          key.entries = c(2, 5, 8, 50, 330),
          colPalette = colorRampPalette(c("cyan4", "tomato"))(5),
          do.sqrt = T, alpha = 1.0, verbose = 0.95) #, legendLoc = "")


if (PlotsOutput) {
    png(filename = file.path(plotDir, paste("Bubble_plot_Cities ",Sys.Date()," ",airport_en,".png", sep="")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))

    par(mar = rep(.5, 4))
    par(cex = 3)

    bubbleMap(
          df2, coords = c("lon", "lat"), map, zcol = "Частота",
          key.entries = c(2, 5, 8 , 50, 330),
          colPalette = colorRampPalette(c("cyan4", "tomato"))( 5 ),
          do.sqrt = T, alpha = 1.0, verbose = 0.95)#, legendLoc = "")

    text(x = grconvertX(0.5, from = "npc"), y = grconvertY(0.5, from = "npc"),
            labels = "Степан Сушко\nдля\n       Ростов-Транспорт", cex = 2, font = 2, col = adjustcolor("steelblue", alpha.f = 0.35), srt = 45)

    dev.off()
}





