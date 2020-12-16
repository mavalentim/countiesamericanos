install.packages("tidycensus")

library(tidycensus)

counties <- tidycensus::fips_codes

countiesnamefreq <- counties %>%
  count(county)%>%
  arrange(desc(n))

top <- countiesnamefreq%>%
  top_n(10)%>%
  arrange(desc(n))

ggplot(data = top, aes(reorder(county, -n) ,y=n, group=1)) + geom_col(color = 'chocolate4')+
  expand_limits(y=0) +
  labs(x = "Nome do County", y = "Quantos tem esse nome nos EUA", 
       title = "Quais os nome mais comuns de county nos EUA")+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))

naorepetidos<- countiesnamefreq%>%
  filter(n==1)%>%
  mutate(unico = 1)

ultima <- counties%>%
  left_join(naorepetidos, by ='county')%>%
  group_by(state_name)%>%
  summarize(unicos = sum(unico))%>%
  top_n(10)


ggplot(data = ultima, aes( unicos,y=reorder(state_name, unicos), group=1)) + geom_col()+
  labs(x = "Quantos counties sem nome repetido ele tem", y = "Nome do Estado", 
       title = "Quais os Estados com menos nome repetidos nos counties")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
       
