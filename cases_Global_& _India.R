global <- as.data.frame(read.csv("owid-covid-data.csv"))
global <- subset(global, select = c(3:12))
str(global)
global <- global[complete.cases(global), ]
global$date <- as.Date(strptime(global$date ,format="%Y-%m-%d"),format="%Y-%m-%d")
str(global)

world <- cbind(global[global$location == "World", 2:5])

w <- plot_ly(world, x = ~date,mode = "lines")
w <- w %>% add_trace(y = ~total_cases, name = "cases")
w <- w %>% add_trace(y = ~total_deaths, name = "deaths")
w <- w %>% add_trace(y = ~new_cases, name = "new cases")
w

anim_w <- w + 
  geom_point() +
  transition_reveal(date)

#?animate
animate(anim_w, 250, fps = 10,  width = 1200, height = 1000, 
        renderer = ffmpeg_renderer()) -> for_mp4
anim_save("animation_world.mp4", animation = for_mp4 )
  
########################################################################################################

india <- cbind(global[global$location == "India", 2:5])

i <- plot_ly(india, x = ~date,mode = "lines")
i <- i %>% add_trace(y = ~total_cases, name = "cases")
i <- i %>% add_trace(y = ~total_deaths, name = "deaths")
i <- i %>% add_trace(y = ~new_cases, name = "new cases")
i


anim <- i + 
  geom_point() +
  transition_reveal(date)

#?animate
animate(anim, 250, fps = 10,  width = 1200, height = 1000, 
        renderer = ffmpeg_renderer()) -> for_mp4
anim_save("animation.mp4", animation = for_mp4 )

