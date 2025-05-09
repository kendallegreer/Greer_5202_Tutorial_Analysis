## code for Santa Cruz, Galapagos
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(patchwork)
library(plotly)
library(ggpmisc)

# Cerro Mesa Data and Figures ---------------------------------------------
Cerro_Mesa_1 <- read_csv("Cerro_Mesa1.csv")
Cerro_Mesa_Clean <- Cerro_Mesa_1 |>
  filter(`SPCOND uS/CM`> 120) 

###4 frame graph with diel pH, DO, SPC, and Temp over 24 hours at Cerro Mesa

CM_DO <- ggplot(Cerro_Mesa_Clean)+
  geom_point(aes(x = `TIME (HH:MM:SS)`, y = `ODO % SAT`), color = "lightblue") +
  ggtitle("Dissolved Oxygen %") +
  theme_bw()
CM_pH <- ggplot(Cerro_Mesa_Clean)+
  geom_point(aes(x = `TIME (HH:MM:SS)`, y = `PH`), color = "lightpink") +
  ggtitle("pH") +
  theme_bw()
CM_SPC <- ggplot(Cerro_Mesa_Clean)+
  geom_point(aes(x = `TIME (HH:MM:SS)`, y = `SPCOND uS/CM`), color = "purple") +
  ggtitle("Specific Conductivity") +
  theme_bw()
CM_Temp <- ggplot(Cerro_Mesa_Clean)+
  geom_point(aes(x = `TIME (HH:MM:SS)`, y = `TEMP C`), color = "lightgreen") +
  ggtitle("Temperature") +
  theme_bw()
((CM_DO | CM_pH) / (CM_SPC | CM_Temp)) + 
  plot_annotation(title = "Cerro Mesa Water Quality Parameters Over 24 Hours")


###Cerro Mesa Linear Regressions
CM_DOvpH <- ggplot(Cerro_Mesa_Clean, aes(x = `ODO % SAT`, y = PH))+
  geom_point(color = "#1b9e77") + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +  
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    label.x = "right", label.y = "top"
  )+
  ggtitle("pH v Dissolved Oxygen %") +
  theme_bw() 
CM_pHvSPC <- ggplot(Cerro_Mesa_Clean, aes(x = `PH`, y = `SPCOND uS/CM`))+
  geom_point(color = "#d95f02") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    label.x = "right", label.y = "top"
  )+
  ggtitle("Specific Conductivity v pH") +
  theme_bw()
CM_SPCvDO <- ggplot(Cerro_Mesa_Clean, aes(x = `SPCOND uS/CM`, y = `TEMP C`))+
  geom_point(color = "#7570b3") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    label.x = "right", label.y = "top"
  )+
  ggtitle("Specific Conductivity") +
  theme_bw()
CM_TempvDO <- ggplot(Cerro_Mesa_Clean, aes(x = `ODO % SAT`, y = `TEMP C`))+
  geom_point(color = "#66a20e") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    label.x = "right", label.y = "top"
  )+
  ggtitle("Dissolved Oxygen v Temperature") +
  theme_bw()
((CM_DOvpH | CM_pHvSPC) / (CM_SPCvDO | CM_TempvDO)) + 
  plot_annotation(title = "Cerro Mesa Water Chemistry Linear Regressions")


###2-Parameter comparison diel graphs

##makes all dates 7/17 so the plot will give a diel 24-hour graph
Cerro_Mesa_Clean$Time24 <- as.POSIXct(
  paste("07/17/2024", Cerro_Mesa_Clean$`TIME (HH:MM:SS)`),
  format = "%m/%d/%Y %H:%M:%S",
  tz = "UTC"
)

#DO v pH
plot_ly(data = Cerro_Mesa_Clean) |>
  add_markers(x = ~Time24, y = ~`ODO % SAT`, name = "Dissolved Oxygen %", yaxis = "y1",
              marker = list(color = 'lightblue')) |>
  add_markers(x = ~Time24, y = ~PH, name = "pH", yaxis = "y2",
              marker = list(color = 'lightpink')) |>
  layout(
    title = "Cerro Mesa pH vs DO% - 24 Hour Diel Cycle",
    xaxis = list(
      title = "Time of Day",
      tickformat = "%H:%M",
      type = "date",
      range = c(
        as.POSIXct("2024-07-17 00:00:00", tz = "UTC"),
        as.POSIXct("2024-07-17 23:59:59", tz = "UTC")
      )
    ),
    yaxis = list(title = "Dissolved Oxygen %", color = "black"),
    yaxis2 = list(
      title = "pH",
      overlaying = "y",
      side = "right",
      color = "black"
    )
  )


#SPC v pH
plot_ly(data = Cerro_Mesa_Clean) |>
  add_markers(x = ~Time24, y = ~`SPCOND uS/CM`, name = "Specific Conductivity", yaxis = "y1",
              marker = list(color = "purple")) |>
  add_markers(x = ~Time24, y = ~PH, name = "pH", yaxis = "y2",
              marker = list(color = 'lightpink')) |>
  layout(
    title = "Cerro Mesa pH vs Specific Conductivity - 24 Hour Diel Cycle",
    xaxis = list(
      title = "Time of Day",
      tickformat = "%H:%M",
      type = "date",
      range = c(
        as.POSIXct("2024-07-17 00:00:00", tz = "UTC"),
        as.POSIXct("2024-07-17 23:59:59", tz = "UTC")
      )
    ),
    yaxis = list(title = "Specific Conductivity", color = "black"),
    yaxis2 = list(
      title = "pH",
      overlaying = "y",
      side = "right",
      color = "black"
    )
  )

#SPC v DO
plot_ly(data = Cerro_Mesa_Clean) |>
  add_markers(x = ~Time24, y = ~`SPCOND uS/CM`, name = "Specific Conductivity", yaxis = "y1",
              marker = list(color = "purple")) |>
  add_markers(x = ~Time24, y = ~`ODO % SAT`, name = "Dissolved Oxygen %", yaxis = "y2",
              marker = list(color = 'lightblue')) |>
  layout(
    title = "Cerro Mesa SPC vs DO% - 24 Hour Diel Cycle",
    xaxis = list(
      title = "Time of Day",
      tickformat = "%H:%M",
      type = "date",
      range = c(
        as.POSIXct("2024-07-17 00:00:00", tz = "UTC"),
        as.POSIXct("2024-07-17 23:59:59", tz = "UTC")
      )
    ),
    yaxis = list(title = "Specific Conductivity", color = "black"),
    yaxis2 = list(
      title = "Dissolved Oxygen %",
      overlaying = "y",
      side = "right",
      color = "black"
    )
  )

#Temp v DO
plot_ly(data = Cerro_Mesa_Clean) |>
  add_markers(x = ~Time24, y = ~`TEMP C`, name = "Temperature (C)", yaxis = "y1",
              marker = list(color = "lightgreen")) |>
  add_markers(x = ~Time24, y = ~`ODO % SAT`, name = "Dissolved Oxygen %", yaxis = "y2",
              marker = list(color = 'lightblue')) |>
  layout(
    title = "Cerro Mesa Temperature vs DO% - 24 Hour Diel Cycle",
    xaxis = list(
      title = "Time of Day",
      tickformat = "%H:%M",
      type = "date",
      range = c(
        as.POSIXct("2024-07-17 00:00:00", tz = "UTC"),
        as.POSIXct("2024-07-17 23:59:59", tz = "UTC")
      )
    ),
    yaxis = list(title = "Temperature (C)", color = "black"),
    yaxis2 = list(
      title = "Dissolved Oxygen",
      overlaying = "y",
      side = "right",
      color = "black"
    )
  )




# Cascajo data and figures ------------------------------------------------
Cascajo <- read_csv("Cascajo_2.csv")

Cascajo_Clean <- Cascajo |>
  filter(`Fault Code`== 0) |>
  filter(`SPCOND (uS/cm)`> 120) 


###4 frame graph with diel pH, DO, SPC, and Temp over 24 hours at Cascajo

DO <- ggplot(Cascajo_Clean)+
  geom_point(aes(x = `Time (HH:MM:SS)`, y = `ODO%`), color = "lightblue") +
  ggtitle("Dissolved Oxygen %") +
  theme_bw()
pH <- ggplot(Cascajo_Clean)+
  geom_point(aes(x = `Time (HH:MM:SS)`, y = `pH`), color = "lightpink") +
  ggtitle("pH") +
  theme_bw()
SPC <- ggplot(Cascajo_Clean)+
  geom_point(aes(x = `Time (HH:MM:SS)`, y = `SPCOND (uS/cm)`), color = "purple") +
  ggtitle("Specific Conductivity") +
  theme_bw()
Temp <- ggplot(Cascajo_Clean)+
  geom_point(aes(x = `Time (HH:MM:SS)`, y = `Temp C`), color = "lightgreen") +
  ggtitle("Temperature") +
  theme_bw()
((DO | pH) / (SPC | Temp)) + 
  plot_annotation(title = "Cascajo Water Quality Parameters Over 24 Hours")


###4 Frame Linear Regression Plot

C_DOvpH <- ggplot(Cascajo_Clean, aes(x = `ODO%`, y = pH))+
  geom_point(color = "#1b9e77") + 
  geom_smooth(method = "lm", se = FALSE, color = "#1b6e77") +  
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    label.x = "right", label.y = "top"
  )+
  ggtitle("pH v Dissolved Oxygen %") +
  theme_bw() 
C_pHvSPC <- ggplot(Cascajo_Clean, aes(x = `pH`, y = `SPCOND (uS/cm)`))+
  geom_point(color = "#d95f02") +
  geom_smooth(method = "lm", se = FALSE, color = "#d92f02") +  
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    label.x = "right", label.y = "top"
  )+
  ggtitle("Specific Conductivity v pH") +
  theme_bw()
C_SPCvDO <- ggplot(Cascajo_Clean, aes(x = `SPCOND (uS/cm)`, y = `Temp C`))+
  geom_point(color = "#7570b3") +
  geom_smooth(method = "lm", se = FALSE, color = "#7540b1") +  
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    label.x = "right", label.y = "top"
  )+
  ggtitle("Specific Conductivity") +
  theme_bw()
C_TempvDO <- ggplot(Cascajo_Clean, aes(x = `ODO%`, y = `Temp C`))+
  geom_point(color = "#66a20e") +
  geom_smooth(method = "lm", se = FALSE, color = "#66a90e") +  
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    label.x = "right", label.y = "top"
  )+
  ggtitle("Dissolved Oxygen v Temperature") +
  theme_bw()
((C_DOvpH | C_pHvSPC) / (C_SPCvDO | C_TempvDO)) + 
  plot_annotation(title = "Cascajo Water Quality Linear Regressions")


###Two-parameter diel comparisons

##makes all dates 7/17 so the plot will give a diel 24-hour graph
Cascajo_Clean$Time24 <- as.POSIXct(
  paste("07/17/2024", Cascajo_Clean$`Time (HH:MM:SS)`),
  format = "%m/%d/%Y %H:%M:%S",
  tz = "UTC"
)

#DO v pH
plot_ly(data = Cascajo_Clean) |>
  add_markers(x = ~Time24, y = ~`ODO%`, name = "Dissolved Oxygen %", yaxis = "y1",
              marker = list(color = 'lightblue')) |>
  add_markers(x = ~Time24, y = ~pH, name = "pH", yaxis = "y2",
              marker = list(color = 'lightpink')) |>
  layout(
    title = "Cascajo pH vs DO% - 24 Hour Diel Cycle",
    xaxis = list(
      title = "Time of Day",
      tickformat = "%H:%M",
      type = "date",
      range = c(
        as.POSIXct("2024-07-17 00:00:00", tz = "UTC"),
        as.POSIXct("2024-07-17 23:59:59", tz = "UTC")
      )
    ),
    yaxis = list(title = "Dissolved Oxygen %", color = "black"),
    yaxis2 = list(
      title = "pH",
      overlaying = "y",
      side = "right",
      color = "black"
    )
  )

##SPC v pH
plot_ly(data = Cascajo_Clean) |>
  add_markers(x = ~Time24, y = ~`SPCOND (uS/cm)`, name = "Specific Conductivity", yaxis = "y1",
              marker = list(color = 'purple')) |>
  add_markers(x = ~Time24, y = ~pH, name = "pH", yaxis = "y2",
              marker = list(color = 'lightpink')) |>
  layout(
    title = "Cascajo pH vs SPC - 24 Hour Diel Cycle",
    xaxis = list(
      title = "Time of Day",
      tickformat = "%H:%M",
      type = "date",
      range = c(
        as.POSIXct("2024-07-17 00:00:00", tz = "UTC"),
        as.POSIXct("2024-07-17 23:59:59", tz = "UTC")
      )
    ),
    yaxis = list(title = "SPC uS/cm", color = "black"),
    yaxis2 = list(
      title = "pH",
      overlaying = "y",
      side = "right",
      color = "black"
    )
  )

##SPC v DO
plot_ly(data = Cascajo_Clean) |>
  add_markers(x = ~Time24, y = ~`SPCOND (uS/cm)`, name = "Specific Conductivity", yaxis = "y1",
              marker = list(color = 'purple')) |>
  add_markers(x = ~Time24, y = ~`ODO%`, name = "Dissolved Oxygen %", yaxis = "y2",
              marker = list(color = 'lightblue'))|>
  layout(
    title = "Cascajo SPC vs DO% - 24 Hour Diel Cycle",
    xaxis = list(
      title = "Time of Day",
      tickformat = "%H:%M",
      type = "date",
      range = c(
        as.POSIXct("2024-07-17 00:00:00", tz = "UTC"),
        as.POSIXct("2024-07-17 23:59:59", tz = "UTC")
      )
    ),
    yaxis = list(title = "Dissolved Oxygen %", color = "black"),
    yaxis2 = list(
      title = "SPC uS/cm",
      overlaying = "y",
      side = "right",
      color = "black"
    )
  )

#Temp vs DO%
plot_ly(data = Cascajo_Clean) |>
  add_markers(x = ~Time24, y = ~`Temp C`, name = "Temperature", yaxis = "y1",
              marker = list(color = 'lightgreen')) |>
  add_markers(x = ~Time24, y = ~`ODO%`, name = "Dissolved Oxygen %", yaxis = "y2",
              marker = list(color = 'lightblue'))|>
  layout(
    title = "Cascajo Temp vs DO% - 24 Hour Diel Cycle",
    xaxis = list(
      title = "Time of Day",
      tickformat = "%H:%M",
      type = "date",
      range = c(
        as.POSIXct("2024-07-17 00:00:00", tz = "UTC"),
        as.POSIXct("2024-07-17 23:59:59", tz = "UTC")
      )
    ),
    yaxis = list(title = "Dissolved Oxygen %", color = "black"),
    yaxis2 = list(
      title = "Temperature",
      overlaying = "y",
      side = "right",
      color = "black"
    )
  )



# Laureles data and figures  ----------------------------------------------

Laureles <- read_csv("Laureles_1.csv")

Laureles_Clean <- Laureles |>
  filter(`SPCOND uS/CM`> 70) |>
  filter(`SPCOND uS/CM`< 89) 


###4 frame graph with diel pH, DO, SPC, and Temp over 24 hours at Laureles

L_DO <- ggplot(Laureles_Clean)+
  geom_point(aes(x = `TIME (HH:MM:SS)`, y = `ODO % SAT`), color = "lightblue") +
  ggtitle("Dissolved Oxygen %") +
  theme_bw()
L_pH <- ggplot(Laureles_Clean)+
  geom_point(aes(x = `TIME (HH:MM:SS)`, y = `PH`), color = "lightpink") +
  ggtitle("pH") +
  theme_bw()
L_SPC <- ggplot(Laureles_Clean)+
  geom_point(aes(x = `TIME (HH:MM:SS)`, y = `SPCOND uS/CM`), color = "purple") +
  ggtitle("Specific Conductivity") +
  theme_bw()
L_Temp <- ggplot(Laureles_Clean)+
  geom_point(aes(x = `TIME (HH:MM:SS)`, y = `TEMP C`), color = "lightgreen") +
  ggtitle("Temperature") +
  theme_bw()
((L_DO | L_pH) / (L_SPC | L_Temp)) + 
  plot_annotation(title = "Laureles Water Quality Parameters Over 24 Hours")


###Laureles Linear Regressions

L_DOvpH <- ggplot(Laureles_Clean, aes(x = `ODO % SAT`, y = PH))+
  geom_point(color = "#1b9e77") + 
  geom_smooth(method = "lm", se = FALSE, color = "#1b6e77") +  
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    label.x = "right", label.y = "top"
  )+
  ggtitle("pH v Dissolved Oxygen %") +
  theme_bw() 
L_pHvSPC <- ggplot(Laureles_Clean, aes(x = `PH`, y = `SPCOND uS/CM`))+
  geom_point(color = "#d95f02") +
  geom_smooth(method = "lm", se = FALSE, color = "#d92f02") +  
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    label.x = "right", label.y = "top"
  )+
  ggtitle("Specific Conductivity v pH") +
  theme_bw()
L_SPCvDO <- ggplot(Laureles_Clean, aes(x = `SPCOND uS/CM`, y = `TEMP C`))+
  geom_point(color = "#7570b3") +
  geom_smooth(method = "lm", se = FALSE, color = "#7540b1") +  
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    label.x = "right", label.y = "top"
  )+
  ggtitle("Specific Conductivity") +
  theme_bw()
L_TempvDO <- ggplot(Laureles_Clean, aes(x = `ODO % SAT`, y = `TEMP C`))+
  geom_point(color = "#66a20e") +
  geom_smooth(method = "lm", se = FALSE, color = "#66a90e") +  
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    label.x = "right", label.y = "top"
  )+
  ggtitle("Dissolved Oxygen v Temperature") +
  theme_bw()
((L_DOvpH | L_pHvSPC) / (L_SPCvDO | L_TempvDO)) + 
  plot_annotation(title = "Laureles Quality Linear Regressions")


###Two-parameter diel comparisons

##makes all dates 7/19 so the plot will give a diel 24-hour graph
Laureles_Clean$Time24 <- as.POSIXct(
  paste("07/19/2024", Laureles_Clean$`TIME (HH:MM:SS)`),
  format = "%m/%d/%Y %H:%M:%S",
  tz = "UTC"
)

#DO v pH
plot_ly(data = Laureles_Clean) |>
  add_markers(x = ~Time24, y = ~`ODO % SAT`, name = "Dissolved Oxygen %", yaxis = "y1",
              marker = list(color = 'lightblue')) |>
  add_markers(x = ~Time24, y = ~PH, name = "pH", yaxis = "y2",
              marker = list(color = 'lightpink')) |>
  layout(
    title = "Laureles pH vs DO% - 24 Hour Diel Cycle",
    xaxis = list(
      title = "Time of Day",
      tickformat = "%H:%M",
      type = "date",
      range = c(
        as.POSIXct("2024-07-19 00:00:00", tz = "UTC"),
        as.POSIXct("2024-07-19 23:59:59", tz = "UTC")
      )
    ),
    yaxis = list(title = "Dissolved Oxygen %", color = "black"),
    yaxis2 = list(
      title = "pH",
      overlaying = "y",
      side = "right",
      color = "black"
    )
  )


#SPC v pH
plot_ly(data = Laureles_Clean) |>
  add_markers(x = ~Time24, y = ~`SPCOND uS/CM`, name = "Specific Conductivity", yaxis = "y1",
              marker = list(color = "purple")) |>
  add_markers(x = ~Time24, y = ~PH, name = "pH", yaxis = "y2",
              marker = list(color = 'lightpink')) |>
  layout(
    title = "Laureles pH vs DO% - 24 Hour Diel Cycle",
    xaxis = list(
      title = "Time of Day",
      tickformat = "%H:%M",
      type = "date",
      range = c(
        as.POSIXct("2024-07-19 00:00:00", tz = "UTC"),
        as.POSIXct("2024-07-19 23:59:59", tz = "UTC")
      )
    ),
    yaxis = list(title = "Specific Conductivity", color = "black"),
    yaxis2 = list(
      title = "pH",
      overlaying = "y",
      side = "right",
      color = "black"
    )
  )

#SPC v DO
plot_ly(data = Laureles_Clean) |>
  add_markers(x = ~Time24, y = ~`SPCOND uS/CM`, name = "Specific Conductivity", yaxis = "y1",
              marker = list(color = "purple")) |>
  add_markers(x = ~Time24, y = ~`ODO % SAT`, name = "Dissolved Oxygen %", yaxis = "y2",
              marker = list(color = 'lightblue')) |>
  layout(
    title = "Laureles SPC vs DO% - 24 Hour Diel Cycle",
    xaxis = list(
      title = "Time of Day",
      tickformat = "%H:%M",
      type = "date",
      range = c(
        as.POSIXct("2024-07-19 00:00:00", tz = "UTC"),
        as.POSIXct("2024-07-19 23:59:59", tz = "UTC")
      )
    ),
    yaxis = list(title = "Specific Conductivity", color = "black"),
    yaxis2 = list(
      title = "Dissolved Oxygen %",
      overlaying = "y",
      side = "right",
      color = "black"
    )
  )

#Temp v DO
plot_ly(data = Laureles_Clean) |>
  add_markers(x = ~Time24, y = ~`TEMP C`, name = "Temperature (C)", yaxis = "y1",
              marker = list(color = "lightgreen")) |>
  add_markers(x = ~Time24, y = ~`ODO % SAT`, name = "Dissolved Oxygen %", yaxis = "y2",
              marker = list(color = 'lightblue')) |>
  layout(
    title = "Laureles Temperature vs DO% - 24 Hour Diel Cycle",
    xaxis = list(
      title = "Time of Day",
      tickformat = "%H:%M",
      type = "date",
      range = c(
        as.POSIXct("2024-07-19 00:00:00", tz = "UTC"),
        as.POSIXct("2024-07-19 23:59:59", tz = "UTC")
      )
    ),
    yaxis = list(title = "Temperature (C)", color = "black"),
    yaxis2 = list(
      title = "Dissolved Oxygen",
      overlaying = "y",
      side = "right",
      color = "black"
    )
  )

