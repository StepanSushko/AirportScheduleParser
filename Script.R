#if (!require("XML")) { install.packages("XML"); require("XML") }
if (!require("ggplot2")) { install.packages("ggplot2"); require("ggplot2") }
#if (!require("RCurl")) { install.packages("RCurl"); require("RCurl") }
if (!require("qdap")) { install.packages("qdap"); require("qdap") }
if (!require("rvest")) { install.packages("rvest"); require("rvest") }
if (!require("RJSONIO")) { install.packages("RJSONIO"); require("RJSONIO") }

#if (!require("gridExtra")) { install.packages("gridExtra"); require("gridExtra") }
#if (!require("xlsx")) { install.packages("xlsx"); require("xlsx") }
#if (!require("rJava")) { install.packages("rJava"); require("rJava") }

#if (!require("mclust")) { install.packages("mclust"); require("mclust") }


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

PlotsOutput = F

dataDir = "C:/Users/stepa/OneDrive/DataScience/Airports/ЯндексТаблоПарсер/Platov"
plotDir = "C:/Users/stepa/OneDrive/DataScience/Airports"

#ddd = c(983245, 1230601, 1501318)
#diff(ddd)[2] / diff(ddd)[1]


# Parsing Airports of Regions ----
u = "http://rov.aero/raspisanie_reysov"
u = file.path(paste(dataDir, "/Data/", "/Platov_12.06.html", sep = ""))
u_Platov = "C:/Users/stepa/OneDrive/DataScience/Airports/Platov_12.06.html"
u_Samara = "C:/Users/stepa/OneDrive/DataScience/Airports/Samara_18.06.html"
u_Ekb = "C:/Users/stepa/OneDrive/DataScience/Airports/Ekb_18.06.html"
u_NiNo = "C:/Users/stepa/OneDrive/DataScience/Airports/NiNo_18.06.html"
#if (!require("XML")) install.packages("XML")

if (!require("RCurl")) { install.packages("RCurl"); require("RCurl") }

year = "2018"

u = u_Platov
AllDayRepeats = function(from1, to1, i) {
    df_dates = seq(from1, to1, by = "days")
    df_dates_2 = as.integer(format(seq(from1, to1, by = "days"), "%w"))
    df_dates_2[df_dates_2 == 0] = 7
    return(df_dates[df_dates_2 == i])
}

AR_schedule_parser = function(u, Geo) {
    tables = read_html(u, options = c("HUGE"))

    city = as.character(html_nodes(tables, "caption"))
    city = gsub("<caption>", "", city)
    city = gsub("</caption>", "", city)

    j = 5 # 49
    li = html_table(tables, fill = T)


    df_tmp = data.frame(Рейс = flight[i], Самолёт = jet[i], Направление = city[j], Расписание = "2000-01-01 00:00:00", stringsAsFactors = F)
    df_tmp[[4]] = as.POSIXlt(df_tmp[, 4], tz = "GMT")

    j=1
    for (j in c(2:(length(city)+1))) {
    #length(html_table(tables, fill = T)))) {
        df2 = html_table(tables, fill = T)[[j]]

        tables2 = html_nodes(tables, "table")
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

        tmp = gsub("период","", df2[,5])
        from = substr(tmp, 1, 5)
        to = substr(tmp, 7, 11)
        from1 = as.POSIXlt(paste(from, ".", year, sep = ""), format = "%d.%m.%Y")
        to1 = as.POSIXlt(paste(to, ".", year, sep = ""), format = "%d.%m.%Y")


        #AllDayRepeats( from1[1], to1[1], 4)

        #as.POSIXlt(paste(AllDayRepeats(from1[1], to1[1], 4), " ", substr(df2[1, 3], 1, 5), ":00", sep = ""), tz = "GMT")

        #Sys.timezone(location = TRUE)


        #i = 3

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

    df = df3[(df3$`расписание` >= as.POSIXct("2018-06-18 00:00:00", tz = "GMT")) & (df3$`расписание` <= as.POSIXct("2018-06-24 23:59:59))", tz = "GMT")),]

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

    df$`Авиакомпания` = gsub("SU", "Аэрофлот", df$`Авиакомпания`)
    df$`Авиакомпания` = gsub("A4", "Азимут", df$`Авиакомпания`)
    df$`Авиакомпания` = gsub("FV", "Россия", df$`Авиакомпания`)
    df$`Авиакомпания` = gsub("DP", "Победа", df$`Авиакомпания`)
    df$`Авиакомпания` = gsub("UT", "UTair", df$`Авиакомпания`)
    df$`Авиакомпания` = gsub("U6", "Уральские Авиалинии", df$`Авиакомпания`)
    df$`Авиакомпания` = gsub("YC", "Ямал", df$`Авиакомпания`)

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

df_Platov = AR_schedule_parser(u_Platov)
df_Samara = AR_schedule_parser(u_Samara)
df_Ekb = AR_schedule_parser(u_Ekb)
df_NiNo = AR_schedule_parser(u_NiNo, F)

df = df_Samara
df = df_Ekb
airport = "Самара"
airport_en = "Samara"

write.csv2(df, file.path(dataDir, paste("TimeTable_", airport, "_", as.Date(Sys.Date())," site", ".csv", sep = "")))



# Parsing Yandex -----

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



# Frequncies plots ----
Freq_plot = function(dff, p_title)
{ 
    return(ggplot(data = dff) +
        geom_bar(stat = "identity", aes(reorder(Var1, Freq), Freq), fill = "cyan4") +
        geom_text(aes(reorder(Var1, Freq), label = Freq, y = Freq), size = 3, hjust = -0.1, colour = "dimgray", vjust = 0.25) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        xlab("") + ylab("") + coord_flip() +
        scale_y_continuous(breaks = round(seq(0, max(dff$Freq), max(dff$Freq)/25))) +
        theme(plot.margin = unit(c(0, 0.2, 0, 0), "cm")) + 
        ggtitle( p_title )
    )
}

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
    #pdf(file = file.path(paste(plotDir, "/City_vs_Freq ", sDate, " ", airport, ".pdf", sep = "")), width = 12, height = 12, pointsize = 10)
    png(filename = file.path(paste(plotDir, "/City_vs_Freq ", sDate, " ", airport_en, ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "Times", restoreConsole = TRUE, type = c("cairo-png"))
    p1 + annotate(geom = "text", x = 15, y = 80.0, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 10, alpha = 0.5)
    dev.off()

    png(filename = file.path(plotDir, paste("Airline_vs_Freq ", sDate, " ", airport_en, ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))
        p2 + annotate(geom = "text", x = 7, y = 25.0, xend = Inf, yend = Inf, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 10, alpha = 0.5, family = 'Arial')
    dev.off()

    png(filename = file.path(plotDir, paste("Jet_vs_Freq ", sDate, " ", airport_en, ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))
        p3 + annotate(geom = "text", x = 7, y = 35, xend = Inf, yend = Inf, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 7, alpha = 0.5, family = 'Arial')
    dev.off()
}





# Frequncy by Day plot ---- 
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
df_wd_fly = data.frame(Var1 = as.character(names(tab2)), Freq = tab2)
df_wd_fly$Var1 = as.numeric(df_wd_fly$Var1)

#df_wd_fly[df_wd_fly$Var1 == 0, 1] = 7



dff = df_wd_fly
p5 = ggplot(data = dff) +
        geom_bar(stat = "identity", aes(Var1, Freq), fill = "cyan4") +
        geom_text(aes(Var1, Freq, label = Freq, y = Freq), size = 3, colour = "dimgray", vjust = -0.3) +
        #        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        xlab("") + ylab("") + # coord_flip() +
        scale_x_continuous(breaks = df_wd_fly$Var1, labels = df_wd_fly$Var1) +
        theme(plot.margin = unit(c(0, 0.2, 0, 0), "cm")) +
        ggtitle(paste("Количество рейсов из аэропорта", airport, "\nпо времени дня за неделю")) +
        annotate(geom = "text", x = 11, y = 13, xend = Inf, yend = Inf, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 7, alpha = 0.5, family = 'Arial')
p5

if (PlotsOutput) {
    png(filename = file.path(plotDir, paste("Freq_per_day ", sDate, " ", airport_en, ".png", sep = "")), width = 800, height = 400, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))
    p4
    dev.off()

    png(filename = file.path(plotDir, paste("Freq_per_hour ", sDate, " ", airport_en, ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))
    p5
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
    png(filename = file.path(plotDir, paste("Mosaic_Airline_vs_City ", sDate, " ", airport_en, ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png")) 

    par(mar = rep(.5, 4))

    mosaicplot(tab, las = 2, col = "cyan4", main = "", cex = 1.1)
    text(x = grconvertX(0.5, from = "npc"), y = grconvertY(0.5, from = "npc"),
        labels = "Степан Сушко", cex = 3, font = 2, col = adjustcolor("steelblue", alpha.f = 0.35), srt = 45)
    dev.off()

    png(filename = file.path(plotDir, paste("Mosaic_City_vs_Airline ", sDate, " ", airport_en, ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))

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
    png(filename = file.path(plotDir, paste("Mosaic_Airline_vs_Jet ", sDate, " ", airport_en, ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))

    par(mar = rep(.5, 4))
    mosaicplot(tab2, las = 2, col = "cyan4", main = "", cex = 1.1)

    text(   x = grconvertX(0.5, from = "npc"), y = grconvertY(0.5, from = "npc"),
            labels = "Степан Сушко", cex = 3, font = 2, col = adjustcolor("steelblue", alpha.f = 0.35), srt = 45)

    dev.off()




    png(filename = file.path(plotDir, paste("Mosaic_Jet_vs_Airline ", sDate, " ", airport_en, ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))

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
    png(filename = file.path(plotDir, paste("Mosaic_Airline_vs_Jet ", sDate, " ", airport_en, ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))

    par(mar = rep(.5, 4))
    mosaicplot(tab3, las = 2, col = "cyan4", main = "")

    text(x = grconvertX(0.5, from = "npc"), y = grconvertY(0.5, from = "npc"),
            labels = "Степан Сушко", cex = 3, font = 2, col = adjustcolor("steelblue", alpha.f = 0.35), srt = 45)

    dev.off()




    png(filename = file.path(plotDir, paste("Mosaic_Jet_vs_Airline ", sDate, " ", airport_en, ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))

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

if (!require("ggmap")) { install.packages("ggmap"); require("ggmap") }

main_airport = geocode( airport_en, override_limit = T)

if (!require("RgoogleMaps")) { install.packages("RgoogleMaps"); require("RgoogleMaps") }

map <- GetMap(center = c( main_airport$lat, main_airport$lon), zoom = 3,
       size = c(640, 640), destfile = file.path(tempdir(), "meuse.png"),
        maptype = "mobile", SCALE = 1);



df2 = unique(df[, c(9:11)])
#df2 = rbind(df2, c(main_airport$lon, main_airport$lat, sum(df2$`Частота`)))
df2 = df2[df2$lon != 0,]

bubbleMap(
          df2, coords = c("lon", "lat"), map, zcol = "Частота",
          key.entries = c(2, 5, 8, 50, 330),
          colPalette = colorRampPalette(c("cyan4", "tomato"))(5),
          do.sqrt = T, alpha = 1.0, verbose = 0.95) #, legendLoc = "")


if (PlotsOutput) {
    png(filename = file.path(plotDir, paste("Bubble_plot_Cities ",sDate," ",airport_en,".png", sep="")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))

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







# Capacity ----
# http://www.wsdot.wa.gov/Aviation/planning/EconomicCalc/Documents/AirplaneCapacityTable
df_capacity = data.frame(
    type = c(
        "Airbus A319", "Airbus A320", "Airbus А321", "Airbus A321", "Airbus A321 SH", "Airbus A330-200", "Airbus A330-300",
        "Boeing 737-100", "Boeing 737-200", "Boeing 737-300", "Boeing 737-400", "Boeing 737-500", "Boeing 737-600", "Boeing 737-700", "Boeing 737-800", "Boeing 737-800W", "Boeing 737-900", "Boeing 737-900ER",
        "Boeing 757-200", "Boeing 757-300",
        "Boeing 767-200", "Boeing 767-200ER", "Boeing 767-300", "Boeing 767-300ER", "Boeing 767-400ER",
        "Boeing 777-200", "Boeing 777-200ER", "Boeing 777-200LR", "Boeing 777-300", "Boeing 777-300ER", "Boeing 777-8X", "Boeing 777-9X", "Boeing 747-400",
        "Canadair regional jet", "Bombardier CRJ200", 
        "Embraer 170", "Embraer 175", "Embraer EMB 175", "Embraer 190", "Embraer 195",
        "AT-72-5",
        "Сухой Суперджет 100", "ТУ-204", "ЯК-42", "АН-24" ),
    capacity = c(
        134, 164, 199, 199, 199, 293, 335,
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
    if ( df_jets[i, 1] %in% df_capacity[, 1]) {
        cap = c(cap, df_capacity[df_capacity[, 1] == df_jets[i, 1], 2])
    } else cap = c(cap, 0)
}

df_jets$`Вместимость` = cap
df_jets$`Ёмкость`     = df_jets$Freq * df_jets$`Вместимость`


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
df_city_capacity[,1] = as.character( df_city_capacity[,1])
colnames( df_city_capacity ) = c("Var1", "Freq")

sum( df_city_capacity$Freq )

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
    png(filename = file.path(plotDir, paste("Jet_vs_Capacity ", sDate, " ", airport_en, ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))
    p6
    dev.off()

    png(filename = file.path(plotDir, paste("City_vs_Capacity ", sDate, " ", airport_en, ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))
    p7
    dev.off()

    png(filename = file.path(plotDir, paste("Airline_vs_Capacity ", sDate, " ", airport_en, ".png", sep = "")), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))
    p8
    dev.off()

}


