library(maps)
library(ggplot2)

us <- map_data("state")
arr <- USArrests %>% 
  add_rownames("region") %>% 
  mutate(region=tolower(region))
arr
st <- data.frame(x)$Freq[-9] #cut DC 
counts<-data.frame(rep(NA, 50))
counts$stCounts <- stCounts[-9]
counts$region <- arr$region
gg <- ggplot()
gg <- gg + geom_map(data=us, map=us,
                    aes(x=long, y=lat, map_id=region),
                    fill="#ffffff", color="#ffffff", size=0.15)
### FILL HERE WITH WHAT WE want to see 
gg <- gg + geom_map(data=counts, map=us,
                    aes(fill=stCounts, map_id=region),
                    color="#ffffff", size=0.15)
gg <- gg + scale_fill_continuous(low='thistle2', high='darkblue', 
                                 guide='colorbar')
gg <- gg + labs(x=NULL, y=NULL)
gg <- gg + coord_map("albers", lat0 = 39, lat1 = 45) 
gg <- gg + theme(panel.border = element_blank(),panel.background = element_blank(),axis.ticks = element_blank(), axis.text = element_blank())
gg
