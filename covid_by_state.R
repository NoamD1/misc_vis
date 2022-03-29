# Trying to recreate the chart from this tutorial 
# https://medium.com/the-stata-guide/stata-graphs-tilemaps-part-ii-usa-48355d366157

library(tidyverse)
library(geofacet)
library(ggtext)
library(showtext)
font_add_google(name = "Oswald", family = "Oswald")
showtext_auto()
showtext_opts(dpi = 300)

df <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/rolling-averages/us-states.csv")
df2 <- df %>% 
  arrange(date) %>% 
  separate(geoid, into=c("geoid11", "geoid22"), sep = "-") %>% 
  mutate(geoid2=parse_integer(geoid22)) %>% 
  group_by(geoid2) %>% 
  arrange(geoid2, date) %>% 
  mutate(casese_ma=zoo::rollapply(cases_avg_per_100k, 7,mean, align='right',fill=NA),
         deaths_ma=zoo::rollapply(deaths_avg_per_100k, 7,mean, align='right',fill=NA)) %>%
  ungroup() %>% 
  # filter(date>=lubridate::dmy("01-07-2021")) %>%
    filter(date>=lubridate::ymd(Sys.Date()-365))%>%
    mutate(max_case=max(casese_ma, na.rm = T),
         max_deaths=max(deaths_ma, na.rm=T)) %>% 
  mutate(casese_ma2=(casese_ma-0)/((3*ceiling(max_case/3))-0),
         deaths_ma2=((deaths_ma-0)/((3*ceiling(max_deaths/3))-0))*-1) %>% 
  pivot_longer(cols = casese_ma2:deaths_ma2) 

ggplot(data=df2, aes(x=date, y=value, fill=name))+
  geom_area()+
  facet_geo(~state, grid = "us_state_grid2")+
  scale_x_date(date_breaks = "3 month", date_labels = "%b\n%y")+
    theme_minimal()+
    labs(title = "Covid cases and deaths per 100,000 peoplr, By state",
         subtitle = "Daily number of  <span style='color:#ff9999;'> cases </span> and
                     <span style='color:#808080;'> deaths </span>
                    at each state, April 2021 - March 2022",
         x="",
         y="",
         caption = "NY Times covid data from: https://github.com/nytimes/covid-19-data")+
    scale_fill_manual(name=NULL, 
                       values = c("casese_ma2"="#ff9999", 'deaths_ma2'='#808080'))+
    theme(legend.position = "none",
        plot.title = element_text(family = "Oswald", colour = '#1369CB', lineheight = 1.2, size = 14),
        plot.subtitle = element_markdown(lineheight = 1.1, size = 12, face = "italic", ),
        strip.text = element_text(family = "Oswald",size = 10),
        axis.title.y = element_text(family = "Oswald",size = 12),
        axis.title.x = element_text(family = "Oswald",size = 12),
        plot.caption = element_text(family = "Oswald", colour = '#8d99ae', size = 10),
        axis.text.y = element_blank()
        # legend.text = element_text(family = "Oswald",size = font_size)
    )

ggsave("covid_by_state.png", bg = "white", dpi = 300)
  

