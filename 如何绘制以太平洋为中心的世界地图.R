# 之前的地图
library(sf)
library(tidyverse)
read_sf("world_high_resolution_mill.geo.json") -> wdmp
readxl::read_xlsx("world-covid19.xlsx") -> df
df %>% 
  mutate(date = as.Date(date)) %>% 
  dplyr::filter(date == "2022-04-20") %>% 
  select(iso, confirmed) -> df

wdmp %>% 
  left_join(df,  by = c("code" = "iso")) -> mapdf

range(mapdf$confirmed, na.rm = T)

source("theme_gray.R")

mapdf %>% 
  mutate(group = cut(confirmed, 
                     breaks = 10^(0:8))) %>% 
  ggplot() + 
  geom_sf(aes(fill = group), size = 0.2, 
          color = "black") + 
  scico::scale_fill_scico_d(
    palette = "bilbao", na.value = "grey30", 
    begin = 0.1,
    name = "新冠确诊人数", 
    labels = c("10~100", "1k~10k",
               "10k~100k", "100k~1m", "1m~10m",
               "10m~100m", "无数据")
  ) + 
  labs(title = "新型冠状病毒肺炎累计确诊病例的分布",
       subtitle = "截止 2022-04-19，累计确诊：513347879 例  病死： 6219973 例\n（治愈数据从 2021 年 8 月 5 日后缺失）",
       caption = "数据来源：约翰·霍普金斯大学 \n 绘制：微信公众号 RStata")

ggsave("map0.png", device = png, width = 10, height = 8)
knitr::plot_crop("map0.png") 

# 如果强行更改 crs：
mapdf %>% 
  mutate(group = cut(confirmed, 
                     breaks = 10^(0:8))) %>% 
  ggplot() + 
  geom_sf(aes(fill = group), size = 0.2, 
          color = "black") + 
  scico::scale_fill_scico_d(
    palette = "bilbao", na.value = "grey30", begin = 0.1,
    name = "新冠确诊人数", 
    labels = c("10~100", "1k~10k",
               "10k~100k", "100k~1m", "1m~10m",
               "10m~100m", "无数据")
  ) + 
  labs(title = "新型冠状病毒肺炎累计确诊病例的分布",
       subtitle = "截止 2022-04-19，累计确诊：513347879 例  病死： 6219973 例\n（治愈数据从 2021 年 8 月 5 日后缺失）",
       caption = "数据来源：约翰·霍普金斯大学 \n 绘制：微信公众号 RStata") +
  coord_sf(crs = "+proj=eck3 +lat_ts=0 +lat_0=0 +lon_0=150 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") 

ggsave("maperror.png", device = png, width = 10, height = 8)
knitr::plot_crop("maperror.png") 

# 如何绘制正确的？
rnaturalearthdata::countries50 %>% 
  st_as_sf() -> worldMap 

worldMap %>% 
  select(country = admin, continent, 
         iso3 = adm0_a3) %>% 
  as_tibble() %>% 
  st_sf() %>% 
  st_transform(4326) -> worldMap

# library(leaflet)
# library(rstatatools)
# leaflet() %>% 
#   geoqmap() -> map
# mapview::mapview(worldMap, map = map)

# 修正中国的部分
read_sf("2021行政区划/省.shp") %>% 
  st_union() %>% 
  nngeo::st_remove_holes() -> cn

read_sf("2021行政区划/九段线.shp") -> jdx
cn %>% 
  st_sf() %>% 
  mutate(country = "China",
         continent = "Asia",
         iso3 = "CHN") -> cn

worldMap %>% 
  st_make_valid() %>% 
  st_difference(cn) %>% 
  dplyr::filter(!country %in% c("China", "Hong Kong S.A.R.", "Macao S.A.R")) -> worldMap

worldMap %>% 
  add_row(cn) %>% 
  arrange(country) -> worldMap

# 调整为太平洋为中心
target_crs <- st_crs("+proj=eck3 +lat_ts=0 +lat_0=0 +lon_0=150 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

# 生成一个细长的多边形
offset <- 180 - 150 

polygon <- st_polygon(x = list(rbind(
  c(-0.0001 - offset, 90),
  c(0 - offset, 90),
  c(0 - offset, -90),
  c(-0.0001 - offset, -90),
  c(-0.0001 - offset, 90)
))) %>%
  st_sfc() %>%
  st_sf() %>% 
  st_set_crs(4326)

plot(polygon)
plot(worldMap["country"], add = T)

# 从世界地图上删除和这个多边形重合的部分 
worldMap %>% 
  st_make_valid() %>% 
  st_transform(4326) %>% 
  st_difference(polygon) -> world2

# 转换坐标系
world2 %>% 
  st_transform(crs = target_crs) -> world3

ggplot(data = world3, aes(group = iso3)) +
  geom_sf(fill = "grey") + 
  coord_sf(crs = target_crs)

# 保存为 shp 数据
dir.create("worldmap")
world3 %>% 
  st_make_valid() %>% 
  st_write("worldmap/worldmap.shp", layer_options = "ENCODING=UTF-8", delete_layer = TRUE, layer = "MULTIPOLYGON") 
# 保存九段线数据
dir.create("jdx")
jdx %>% 
  st_transform(target_crs) %>% 
  st_make_valid() %>% 
  st_collection_extract("LINESTRING") %>% 
  st_write("jdx/jdx.shp", layer_options = "ENCODING=UTF-8", delete_layer = TRUE, layer = "MULTILINESTRING") 

# 绘图案例：展示新冠疫情确诊人数
readxl::read_xlsx("world-covid19.xlsx") -> df
df %>% 
  mutate(date = as.Date(date)) %>% 
  dplyr::filter(date == "2022-04-20") %>% 
  select(iso, confirmed) -> df
  
world3 %>% 
  left_join(df,  by = c("iso3" = "iso")) -> mapdf

range(mapdf$confirmed, na.rm = T)

source("theme_gray.R")

mapdf %>% 
  mutate(group = cut(confirmed, 
                     breaks = 10^(0:8))) %>% 
  ggplot() + 
  geom_sf(aes(fill = group), size = 0.2, 
          color = "black") + 
  geom_sf(data = jdx, color = "black", 
          size = 0.2) +  
  scico::scale_fill_scico_d(
    palette = "bilbao", na.value = "grey30", 
    name = "新冠确诊人数", 
    labels = c("10~100", "1k~10k",
               "10k~100k", "100k~1m", "1m~10m",
               "10m~100m", "无数据")
  ) + 
  labs(title = "新型冠状病毒肺炎累计确诊病例的分布",
       subtitle = "截止 2022-04-19，累计确诊：513347879 例  病死： 6219973 例\n（治愈数据从 2021 年 8 月 5 日后缺失）",
       caption = "数据来源：约翰·霍普金斯大学 \n 绘制：微信公众号 RStata")

ggsave("map.png", device = png, width = 10, height = 8)
knitr::plot_crop("map.png") 

# 添加三角形
mapdf %>% 
  st_point_on_surface() %>% 
  st_coordinates() %>% 
  as_tibble() -> point
library(ggtriangles)
mapdf %>% 
  mutate(group = cut(confirmed, 
                     breaks = 10^(0:8))) %>% 
  mutate(x = point$X, y = point$Y) %>% 
  ggplot() + 
  geom_sf(aes(fill = group), size = 0.2, 
          color = "black") + 
  geom_sf(data = jdx, color = "black", 
          size = 0.2) + 
  geom_triangles(aes(x, y, 
                     triangle_height = confirmed,
                     fill = group), 
                 triangle_width = 0.2,
                 colo = "gray30", size = 0.1,
                 alpha = 0.8) + 
  scale_triangle_height(trans = "log10") + 
  guides(triangle_height = "none") + 
  scico::scale_fill_scico_d(
    palette = "bilbao", na.value = "grey30", 
    name = "新冠确诊人数", begin = 0.2,
    labels = c("10~100", "1k~10k",
               "10k~100k", "100k~1m", "1m~10m",
               "10m~100m", "无数据")
  ) + 
  labs(title = "新型冠状病毒肺炎累计确诊病例的分布",
       subtitle = "截止 2022-04-19，累计确诊：513347879 例  病死： 6219973 例\n（治愈数据从 2021 年 8 月 5 日后缺失）",
       caption = "数据来源：约翰·霍普金斯大学 \n 绘制：微信公众号 RStata") + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggsave("map2.png", device = png, width = 10, height = 8)
knitr::plot_crop("map2.png") 

# 以 0 度为中心的：
rnaturalearthdata::countries50 %>% 
  st_as_sf() -> worldMap 

worldMap %>% 
  select(country = admin, continent, 
         iso3 = adm0_a3) %>% 
  as_tibble() %>% 
  st_sf() %>% 
  st_transform(4326) -> worldMap

# library(leaflet)
# library(rstatatools)
# leaflet() %>% 
#   geoqmap() -> map
# mapview::mapview(worldMap, map = map)

# 修正中国的部分
read_sf("2021行政区划/省.shp") %>% 
  st_union() %>% 
  nngeo::st_remove_holes() -> cn

cn %>% 
  st_sf() %>% 
  mutate(country = "China",
         continent = "Asia",
         iso3 = "CHN") -> cn

worldMap %>% 
  st_make_valid() %>% 
  st_difference(cn) %>% 
  dplyr::filter(!country %in% c("China", "Hong Kong S.A.R.", "Macao S.A.R")) -> worldMap

worldMap %>% 
  add_row(cn) %>% 
  arrange(country) -> worldMap

# 调整为大西洋为中心
target_crs <- st_crs("+proj=eck3 +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

# 生成一个细长的多边形
offset <- 180 - 0

polygon <- st_polygon(x = list(rbind(
  c(-0.0001 - offset, 90),
  c(0 - offset, 90),
  c(0 - offset, -90),
  c(-0.0001 - offset, -90),
  c(-0.0001 - offset, 90)
))) %>%
  st_sfc() %>%
  st_sf() %>% 
  st_set_crs(4326)

plot(polygon)
plot(worldMap["country"], add = T)

# 从世界地图上删除和这个多边形重合的部分 
worldMap %>% 
  st_make_valid() %>% 
  st_transform(4326) %>% 
  st_difference(polygon) -> world2

# 转换坐标系
world2 %>% 
  st_transform(crs = target_crs) -> world3

ggplot(data = world3, aes(group = iso3)) +
  geom_sf(fill = "grey") + 
  coord_sf(crs = target_crs) -> p
ggsave("temp.png")

# 保存为 shp 数据
dir.create("worldmap0")
world3 %>% 
  select(-contains("1")) %>%  
  st_make_valid() %>% 
  st_transform(4326) %>% 
  st_write("worldmap0.geojson")

read_sf("worldmap0.geojson") -> world3

ggplot(data = world3, aes(group = iso3)) +
  geom_sf(fill = "grey") + 
  coord_sf(crs = target_crs) -> p
ggsave("temp.png")
