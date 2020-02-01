
library(plotly)
rhum = c(45:100)
temp =seq(0,31,0.5)

surv <- outer(temp, rhum, Survival)




f1 <- list(
  family = "Arial, sans-serif",
  size = 16,
  color = "grey"
)
f2 <- list(
  family = "Old Standard TT, serif",
  size = 12,
  color = "black"
)
tit<- list(
  title = "Survival Function",
  titlefont = f1,
  tickfont = f2
)
x <- list(
  title = "RH",
  titlefont = f1,
  showticklabels = TRUE,
  tickangle = 30,
  tickfont = f2,
  exponentformat = "E",
  nticks= 8,
  range= c(45,100),
  backgroundcolor="rgb(230, 230,200)",
  gridcolor="rgb(255,255,255)",
  showbackground=TRUE,
  zerolinecolor="rgb(255,255,255"
)
y <- list(
  title = "Temp",
  titlefont = f1,
  showticklabels = TRUE,
  tickangle = -45,
  tickfont = f2,
  exponentformat = "E",
  nticks = 8,
  range= c(0,30),
  backgroundcolor="rgb(230, 230,200)",
  gridcolor="rgb(255,255,255)",
  showbackground=TRUE,
  zerolinecolor="rgb(255,255,255"
)
z <- list(
  title = "Surv.",
  titlefont = f1,
  showticklabels = TRUE,
  tickangle = 0,
  tickfont = f2,
  exponentformat = "E",
  nticks= 10,
  #range= c(0,1),
  backgroundcolor="rgb(230, 230,200)",
  gridcolor="rgb(255,255,255)",
  showbackground=TRUE,
  zerolinecolor="rgb(255,255,255"
)


plot_ly(x= rhum,y=temp,z=surv, type="surface",
        colors = colorRamp(c("yellow", "red"))) %>% 
  layout(
    title = tit,
    scene = list(
      xaxis = x,
      yaxis = y,
      zaxis = z
    ))


##Custom ticks
axx <- list(
  ticketmode = 'array',
  ticktext = c("Huey", "Dewey", "Louie"),
  tickvals = c(0,25,50),
  range = c(-25,75)
)

