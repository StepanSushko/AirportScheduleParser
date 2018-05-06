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
i = 13
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

    return(df)
}


"9600213" # Sheremetievo
"9866615" # Platov
"9600365" # Borispol
"9600177" # Gumrak
"9600396" # Simpferopol
station = "9600366" # Pulkovo

df  = OneWeek_TimeTable("9866615")
#dff = df[, c(1,2,4,5)]
df$`расписание`


# Frequncies plots ----
Freq_plot = function(dff, p_title)
{ 
    return(ggplot(data = dff) +
        geom_bar(stat = "identity", aes(reorder(Var1, Freq), Freq), fill = "cyan3") +
        geom_text(aes(reorder(Var1, Freq), label = Freq, y = Freq), size = 3, hjust = -0.1, colour = "dimgray", vjust = 0.25) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        xlab("") + ylab("") + coord_flip() +
        scale_y_continuous(breaks = seq(0, max(dff$Freq), 5)) +
        theme(plot.margin = unit(c(0, 0.2, 0, 0), "cm")) + 
        ggtitle( p_title )
    )
}

df_cities = as.data.frame(sort(table(df$`Направление`), decreasing = T))
df_airlines = as.data.frame(sort(table(df$`Авиакомпания`), decreasing = T))
df_jets = as.data.frame(sort(table(df$`Самолёт`), decreasing = T))

Freq_plot(df_cities, "Рейсов из аэропорта Платов\nпо направлениям (в неделю)")
Freq_plot(df_airlines, "Количество рейсов из аэропорта Платов\nпо авиакомпаниям (в неделю)")
Freq_plot(df_jets, "Количество рейсов из аэропорта Платов\nпо типам самолётов (в неделю)")


# Frequncy by Day plot ---- 
wdays = c( "Пн", "Вт", "Ср", "Чт", "Пт", "Сб", "Вс")

tab2 = apply(table(as.character(df$`расписание`$wday + 1), df$`Направление`), FUN = sum, MARGIN = 1)
df_wd_fly = data.frame(Var1 = as.character(names(tab2)), Freq = tab2)
df_wd_fly$Var1 = as.integer(df_wd_fly$Var1)

dff = df_wd_fly
ggplot(data = dff) +
        geom_bar(stat = "identity", aes(Var1, Freq), fill = "cyan3") +
        geom_text(aes(Var1, Freq, label = Freq, y = Freq), size = 3, colour = "dimgray", vjust = -0.3) +
        #        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        xlab("") + ylab("") + # coord_flip() +
        scale_x_continuous(breaks = 1:7, labels = wdays) +
        theme(plot.margin = unit(c(0, 0.2, 0, 0), "cm")) +
        ggtitle("Количество рейсов из аэропорта Платов \nпо дням недели")



# Capacity ----
df_capacity = data.frame(
    type = c("Airbus A319", "Boeing 737-800", "Сухой Суперджет 100", "Airbus A320", "Boeing 737-500", "Airbus А321", "Canadair regional jet", "Boeing 737-400", "Embraer EMB 175"),
    capacity = c(134, 175, 95, 164, 132, 199, 76, 159, 80))
df_capacity$type = as.character(df_capacity$type)
df_jets$Var1 = as.character(df_jets$Var1)

#within(df_jets, `Вместимость` <- factor(`Вместимость`, labels = df_capacity$capacity))

#factor(df_jets$Var1)
#factor(df_capacity$capacity)

df_jets$`Вместимость` =  df_capacity[ , 2]
#which(df_capacity$type==df_jets$Var1)
#df_capacity[, 2]

#df_jets$Var1[ df_capacity$, ]
df_jets$`Ёмкость` = df_jets$Freq * df_jets$`Вместимость`

sum(df_jets$`Ёмкость`) * 8 * 0.7




# Mosaic plot ----
tab = table(df$`Авиакомпания`, df$`Направление`)


mosaic(df$`Авиакомпания` ~ df$`Направление`)
print(tbl)
mosaic(tbl)



tab = tab[ as.character(df_airlines$Var1), as.character(df_cities$Var1)]
df_cross = as.data.frame.matrix(tab)

mosaic(tab, legend = T, zero_size = 0)

mosaic(~`Направление` + `Авиакомпания`, data = df, zero_size = 0, zero_split = F, keep_aspect_ratio = F, ctx = 0.2)
mosaic(~`Авиакомпания` + `Направление`, data = df, zero_size = 0, legend = T)

# Plots
if (!require("vcd")) { install.packages("vcd"); require("vcd") }
mosaic(tab, legend = T)
assoc(tab, legend = T, compress = T)


res4



library(MASS)
data("Titanic")
mosaic(Titanic)

## Formula interface for tabulated data plus shading and legend:
mosaic(~Sex + Age + Survived, data = Titanic,
  main = "Survival on the Titanic", shade = TRUE, legend = TRUE)




