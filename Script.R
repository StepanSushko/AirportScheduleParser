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

dataDir = "C:/Users/stepa/OneDrive/DataScience/Airports/ЯндексТаблоПарсер/Platov"
plotDir = "C:/Users/stepa/OneDrive/DataScience/Airports/ЯндексТаблоПарсер/Platov/Plots"

#Sys.getlocale()
#LANG = "ru_RU.65001"
#LANG <- Sys.getenv("LANG")
#Sys.setlocale("LC_ALL", "Russian_Russia.65001")
#Sys.setlocale("LC_ALL", "Russian_Russia.20866")
#Sys.setlocale("LC_ALL", "Russian_Russia.1251")

# u = "http://en.wikipedia.org/wiki/World_population"
#u = "http://rov.aero/raspisanie_reysov"
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
            Sys.Date() + 3 + i)
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



"9600213" # Sheremetievo
"9866615" # Platov
"9600365" # Borispol
"9600177" # Gumrak
"9600396" # Simpferopol
station = "9600366" # Pulkovo

#df = OneWeek_TimeTable("9600396")


#library(RJSONIO)
#nrow <- nrow(df)
#counter <- 1
#df$lon[counter] <- 0
#df$lat[counter] <- 0
#while (counter <= nrow) {
#    CityName <- gsub(' ', '%20', df$`Направление`[counter]) #remove space for URLs
#    CountryCode <- ""
#    url <- paste(
#    "http://nominatim.openstreetmap.org/search?city="
#    , CityName
    #    , "&countrycodes="
    #    , CountryCode
#    , "&limit=9&format=json"
#    , sep = "")
#    x <- fromJSON(url)
#    if (is.vector(x)) {
#        df$lon[counter] <- x[[1]]$lon
#        df$lat[counter] <- x[[1]]$lat
#    }
#    counter <- counter + 1
#}

#df3 = as.data.frame(table(df$`Направление`))

#for (i in c(1:nrow(df3))) {
#    df[df$`Направление` == df3[i, 1], 11] = df3[i, 2]
#}
#

#write.csv2(df, file.path(dataDir, "TimeTable.csv"))

df = read.csv2( file.path(dataDir, "TimeTable_Simferopol.csv"))[,c(2:12)]

df$lat = as.numeric(as.character(df$lat))
df$lon = as.numeric(as.character(df$lon))
df$`расписание` = as.POSIXct(df$`расписание`)
df$`Направление` = as.character(df$`Направление`)
colnames(df)[11] = "Частота"

airport = "Симферополь"

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
p2 = Freq_plot(df_airlines, paste("Количество рейсов из аэропорта ", airport, "\nпо авиакомпаниям (в неделю)"))
p3 = Freq_plot(df_jets, paste("Количество рейсов из аэропорта ", airport, "\nпо типам самолётов (в неделю)"))



#pdf(file = paste(plotDir, "/Freq_cities.pdf", sep = ""), width = 12, height = 12, pointsize = 10)
png(filename = file.path(plotDir, "City_vs_Freq.png"), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))
p1 + annotate(geom = "text", x = 15, y = 80.0, xend = Inf, yend = Inf, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 10, alpha = 0.5)
dev.off()

png(filename = file.path(plotDir, "Airline_vs_Freq.png"), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))
p2 + annotate(geom = "text", x = 7, y = 25.0, xend = Inf, yend = Inf, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 10, alpha = 0.5, family = 'Arial')
dev.off()

png(filename = file.path(plotDir, "Jet_vs_Freq.png"), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))
p3 + annotate(geom = "text", x = 7, y = 35, xend = Inf, yend = Inf, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 7, alpha = 0.5, family = 'Arial')
dev.off()






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


png(filename = file.path(plotDir, "Freq_per_day.png"), width = 800, height = 400, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))
p4
dev.off()




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
        ggtitle( paste("Количество рейсов из аэропорта", airport, "\nпо времени дня за неделю")) +
        annotate(geom = "text", x = 11, y = 13, xend = Inf, yend = Inf, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 7, alpha = 0.5, family = 'Arial')

png(filename = file.path(plotDir, "Freq_per_hour.png"), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))
p5
dev.off()








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

png(filename = file.path(plotDir, "Mosaic_Airline_vs_City.png"), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))

par(mar = rep(.5, 4))
mosaicplot(tab, las = 2, col = "cyan4", main = "", cex = 1.1)

text(x = grconvertX(0.5, from = "npc"), y = grconvertY(0.5, from = "npc"),
        labels = "Степан Сушко", cex = 3, font = 2, col = adjustcolor("steelblue", alpha.f = 0.35), srt = 45)

dev.off()



png(filename = file.path(plotDir, "Mosaic_City_vs_Airline.png"), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))

par(mar = rep(.5, 4))

mosaicplot(t(tab), las = 2, col = "cyan4", main = "", cex = 1.1)

text(x = grconvertX(0.5, from = "npc"), y = grconvertY(0.5, from = "npc"),
        labels = "Степан Сушко", cex = 3, font = 2, col = adjustcolor("steelblue", alpha.f = 0.35), srt = 45)
dev.off()


# АК vs Самолёт
tab2 = table(df$`Авиакомпания`, df$`Самолёт`)
tab2 = tab2[as.character(df_airlines$Var1), as.character(df_jets$Var1)]



png(filename = file.path(plotDir, "Mosaic_Airline_vs_Jet.png"), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))

par(mar = rep(.5, 4))
mosaicplot(tab2, las = 2, col = "cyan4", main = "", cex = 1.1)

text(   x = grconvertX(0.5, from = "npc"), y = grconvertY(0.5, from = "npc"),
        labels = "Степан Сушко", cex = 3, font = 2, col = adjustcolor("steelblue", alpha.f = 0.35), srt = 45)

dev.off()




png(filename = file.path(plotDir, "Mosaic_Jet_vs_Airline.png"), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))

par(mar = rep(.5, 4))

mosaicplot( t(tab2), las = 2, col = "cyan4", main = "", cex = 1.1)

text(x = grconvertX(0.5, from = "npc"), # align to center of plot X axis
       y = grconvertY(0.5, from = "npc"), # align to center of plot Y axis
        labels = "Степан Сушко", # our watermark
        cex = 3, font = 2, # large, bold font - hard to miss
        col = adjustcolor("steelblue", alpha.f = 0.35), # translucent (0.2 = 20%) red color
        srt = 45) # srt = angle of text: 45 degree angle to X axis

dev.off()





# Bubble plot ----


library(ggmap)
main_airport = geocode( "Simferopol", override_limit = T)

if (!require("RgoogleMaps")) { install.packages("RgoogleMaps"); require("RgoogleMaps") }

map <- GetMap(center = c( main_airport$lat, main_airport$lon), zoom = 3,
       size = c(640, 640), destfile = file.path(tempdir(), "meuse.png"),
        maptype = "mobile", SCALE = 1);




df2 = unique(df[, c(9:11)])
#df2 = rbind(df2, c(main_airport$lon, main_airport$lat, sum(df2$`Частота`)))
df2 = df2[df2$lon!=0,] 



png(filename = file.path(plotDir, "Bubble_plot_Cities.png"), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))

par(mar = rep(.5, 4))
par(cex = 3)

bubbleMap(
      df2, coords = c("lon", "lat"), map, zcol = "Частота",
      key.entries = c(2, 5, 8 , 50, 330),
      colPalette = colorRampPalette(c("cyan4", "tomato"))( 5 ),
      do.sqrt = T, alpha = 1.0, verbose = 0.95, legendLoc = "")

text(x = grconvertX(0.5, from = "npc"), y = grconvertY(0.5, from = "npc"),
        labels = "Степан Сушко\nдля\n       Ростов-Транспорт", cex = 2, font = 2, col = adjustcolor("steelblue", alpha.f = 0.35), srt = 45)

dev.off()








# Capacity ----
# http://www.wsdot.wa.gov/Aviation/planning/EconomicCalc/Documents/AirplaneCapacityTable
df_capacity = data.frame(
    type = c("Airbus A319", "Boeing 737-800", "Airbus A320", "Сухой Суперджет 100", "Boeing 737-500", "Airbus А321", "Canadair regional jet", "Boeing 737-400", "Embraer EMB 175", "Canadair regional jet",
        "Boeing 777-300", "Boeing 747-400", "ТУ-204", "Boeing 777-300ER", "Bombardier CRJ200", "Embraer 190"),
    capacity = c(134, 175, 164, 95, 132, 199, 76, 159, 80, 76,
    400, 470, 210, 400, 50, 105))
df_capacity$type = as.character(df_capacity$type)
df_jets$Var1 = as.character(df_jets$Var1)

cap = NULL
for (i in c(1:dim(df_jets)[1])) {
    if ( df_jets[i, 1] %in% df_capacity[, 1]) {
        cap = c(cap, df_capacity[df_jets[i, 1] == df_capacity[, 1], 2])
    } else cap = c(cap, 0)
}

df_jets$`Вместимость` = cap
df_jets$`Ёмкость`     = df_jets$Freq * df_jets$`Вместимость`


df_tmp = df_jets[, c(1, 4)]
colnames(df_tmp) = c("Var1", "Freq")

p6 = Freq_plot(df_tmp, paste("Ёмкость рейсов из аэропорта", airport, "\nпо типам самолётов (в неделю)")) + annotate(geom = "text", x = 5, y = 5000, xend = Inf, yend = Inf, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 10, alpha = 0.5, family = 'Arial') + ylab(paste("Общая ёмкость по типам (в одном направлении в месяц):", sum(df_jets$`Ёмкость`) * 4))

png(filename = file.path(plotDir, "Jet_vs_Capacity.png"), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))
p6
dev.off()

# Пассажиропоток
sum(df_jets$`Ёмкость`) * 8 * 0.7




# Ёмкость по направленимям
df$`Самолёт` = as.character(df$`Самолёт`)
for (i in c(1:dim(df_capacity)[1])) {
    df[ df$`Самолёт` == df_capacity$type[i] , 12] = df_capacity$capacity[i]
}

tab3 = table(df$`Направление`, df$V12) 

for (i in c(1:dim(tab3)[1])) {
    tab3[i,] = tab3[i,] * as.integer(colnames(tab3))
}

df_city_capacity = as.data.frame(sort(apply(tab3, MARGIN = 1, sum), decreasing = T))
df_city_capacity = cbind(rownames(df_city_capacity), df_city_capacity)
df_city_capacity[,1] = as.character( df_city_capacity[,1])
colnames( df_city_capacity ) = c("Var1", "Freq")

sum( df_city_capacity$Freq )

p7 = Freq_plot(df_city_capacity, paste("Ёмкость из аэропорта", airport, "\nпо направлениям (в неделю)")) +
    annotate(geom = "text", x = 15, y = 9000, xend = Inf, yend = Inf, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 10, alpha = 0.5, family = 'Arial')

png(filename = file.path(plotDir, "City_vs_Capacity.png"), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))
p7
dev.off()





# По АК

#for (i in c(1:dim(df_jets)[1])) {
#    df[df$`Самолёт` == df_capacity$type[i], 12] = df_capacity$capacity[i]
#}

tab3 = table(df$`Авиакомпания`, df$V12)

for (i in c(1:dim(tab3)[1])) {
    tab3[i,] = tab3[i,] * as.integer(colnames(tab3))
}

df_airline_capacity = as.data.frame(sort(apply(tab3, MARGIN = 1, sum), decreasing = T))
df_airline_capacity = cbind(rownames(df_airline_capacity), df_airline_capacity)
df_airline_capacity[, 1] = as.character(df_airline_capacity[, 1])
colnames(df_airline_capacity) = c("Var1", "Freq")

sum(df_airline_capacity$Freq)

p8 = Freq_plot(df_airline_capacity, paste("Ёмкость из аэропорта", airport, "\nпо авиакомпаниям (в неделю)")) +
    annotate(geom = "text", x = 9, y = 3000, xend = Inf, yend = Inf, label = 'Степан Сушко', color = 'white', angle = 45, fontface = 'bold', size = 10, alpha = 0.5, family = 'Arial')

png(filename = file.path(plotDir, "Airline_vs_Capacity.png"), width = 800, height = 800, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("cairo-png"))
p8
dev.off()




