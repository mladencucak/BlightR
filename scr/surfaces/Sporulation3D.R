
#Sporulation
require(plotly)

rhum = rev(82:100)
temp = rev(5:27)

spor <- outer(temp, rhum, Sporulation)




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
  title = "Sporuation Function",
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
  range= c(85,100),
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
  range= c(6,26),
  backgroundcolor="rgb(230, 230,200)",
  gridcolor="rgb(255,255,255)",
  showbackground=TRUE,
  zerolinecolor="rgb(255,255,255"
)
z <- list(
  title = "Spor.",
  titlefont = f1,
  showticklabels = TRUE,
  tickangle = 0,
  tickfont = f2,
  exponentformat = "E",
  nticks= 10,
  range= c(0,1),
  backgroundcolor="rgb(230, 230,200)",
  gridcolor="rgb(255,255,255)",
  showbackground=TRUE,
  zerolinecolor="rgb(255,255,255"
)


plot_ly(x= rhum,y=temp,z=spor, type="surface",
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

