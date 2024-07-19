path_db <- "C:/Users/zdenda/Downloads/CEITEC"
con <- DBI::dbConnect(RSQLite::SQLite(), file.path(path_db, "mydatabase.db"))

controls <- dplyr::tbl(con, "HCstimul")
boxplot_epoch(controls, subject = 1, sensor = "E65", time_lim = c(1:5))

DBI::dbDisconnect(con)


db_sub <- controls %>%
  dplyr::filter(electrode == "E65" & time %in% c(251:260))  %>%
  dplyr::select(time, signal, epoch, subject)
db_sub <- db_sub |>
  group_by(subject, time) %>%
  mutate(average = mean(signal, na.rm = TRUE))
db_df <- as.data.frame(db_sub)
db_df$epoch <- factor(db_df$epoch)

fig <- plot_ly(db_df, x = ~time, y = ~signal) |>
  add_boxplot(hovertext = paste("Trial :", db_df$epoch))
fig2 <- fig |> layout(xaxis = list(title = "Time point"),
                       yaxis = list(title = TeX("\\mu V"))) |>
  config(mathjax = 'cdn')
fig2





### vytvoreni ukazkoveho dat. souboru pro HC
vynechat <- seq(4,40, by = 2)
sub01 <- controls %>%
  dplyr::filter(subject == 1 & !epoch %in% vynechat)  %>%
  dplyr::select(time, signal, epoch, electrode)

sub01df <- as.data.frame(sub01)
sub01df$epoch <- factor(sub01df$epoch)

sub01df01 <- sub01df |>
  dplyr::filter(electrode == "E65" & time %in% c(251:260))  |>
  dplyr::select(time, signal, epoch)

fig <- plot_ly(sub01df01, x = ~time, y = ~signal) |>
  add_boxplot(hovertext = paste("Trial :", sub01df01$epoch))
fig2 <- fig |> layout(xaxis = list(title = "Time point"),
                      yaxis = list(title = TeX("\\mu V"))) |>
  config(mathjax = 'cdn')
fig2



## vykresleni boxplotu vedle sebe
# nejdriv definujeme usek t.p. 251 - 260 a elektrodu E65
sub01box <- controls %>%
  dplyr::filter(subject == 1 & electrode == "E7" & time %in% c(551:560))  %>%
  dplyr::select(time, signal, epoch)

sub01box <- as.data.frame(sub01box)

## asi to chce trochu sofistikovanejsi vyber - udelat prumer a vzit polovinu pod a polovinu nad
## resp. proporcne tomu, jak to je ve skutecnosti

## zrovna na tohle by se hodil ten funkcionalni boxplot, ktery by mi ty krivky mohl
## hezky seradit...



sub01box$selection <- 1
sub01boxsim <- subset(sub01box, !sub01box$epoch %in% vynechat)
sub01boxsim$selection <- 0

sub01c <- rbind(sub01box, sub01boxsim)
sub01c$selection <- factor(sub01c$selection)
levels(sub01c$selection) <- c("all", "subset")

boxplot(sub01boxsim$signal ~ sub01boxsim$time)

sub01c$time <- factor(sub01c$time)

ggplot(sub01c, aes(x = time, y = signal, fill = selection)) +
  geom_boxplot() +
  labs(y = "Amplitude", x = "Time point") +
guides(
  fill = guide_legend(
    title = "Data",
    theme = theme(legend.title = element_text(hjust = 0))
  )
)

sub01c |>
  subset(!epoch %in% c(47,48)) |>
  ggplot(aes(x = time, y = signal, fill = selection)) +
  geom_boxplot() +
  labs(y = "Amplitude", x = "Time point") +
  guides(
    fill = guide_legend(
      title = "Data",
      theme = theme(legend.title = element_text(hjust = 0))
    )
  )



## zkouska fda fboxplot
# potrebuju matici p x n, kde p je pocet cas. bodu, n je pocet krivek (tj. epochy)
library(tidyr)
matrix01 <- controls %>%
  dplyr::filter(subject == 1 & electrode == "E65")  %>%
  dplyr::select(time, signal, epoch)

matrix01 <- as.data.frame(matrix01)
matrix01 <- matrix01 |>
  spread(key = epoch, value = signal)

matrix01 <- matrix01[,-1]
matrix01 <- as.matrix(matrix01)

library(fda)
fbox01 <- fbplot(matrix01)
sort(fbox01$depth)

v.ex <- c(2, 38, 3, 8, 4, 14, 32, 40, 20, 11, 27, 35, 37, 33, 24, 29, 17, 18, 21)


sub01box$selection <- 1
sub01boxsim <- subset(sub01box, !sub01box$epoch %in% v.ex)
sub01boxsim$selection <- 0

sub01c <- rbind(sub01box, sub01boxsim)
sub01c$selection <- factor(sub01c$selection)
levels(sub01c$selection) <- c("all", "subset")

sub01c$time <- factor(sub01c$time)

ggplot(sub01c, aes(x = time, y = signal, fill = selection)) +
  geom_boxplot() +
  labs(y = "Amplitude", x = "Time point") +
  guides(
    fill = guide_legend(
      title = "Data",
      theme = theme(legend.title = element_text(hjust = 0))
    )
  )

sub01c |>
  subset(!epoch %in% c(47,48)) |>
  ggplot(aes(x = time, y = signal, fill = selection)) +
  geom_boxplot() +
  labs(y = "Amplitude", x = "Time point") +
  guides(
    fill = guide_legend(
      title = "Data",
      theme = theme(legend.title = element_text(hjust = 0))
    )
  )

plot(matrix01[,1], ylim = c(-50, 50), type = "n")
for(i in v.ex){
  lines(matrix01[,i], col = "grey80")
}
lines(rowMeans(matrix01[,-c(v.ex, 47, 48)]), col = "red")
lines(rowMeans(matrix01[,-c(47, 48)]), col = "blue")

lines(matrix01[,30], col = "red")
lines(matrix01[,29], col = "grey50")


## ok, porovname tvar prumerne krivky pro odstraneni podle hloubky vs. odstraneni
## od "oka" kazdy druhy... a vezmu, co bude lepsi
## nemusi to uplne odpovidat, protoze to jsou jen data na zobrazovani, ukazka do R
## nema smysl tim ztracet prilis casu

plot(matrix01[,1], ylim = c(-20, 20), type = "n")
#lines(rowMeans(matrix01[,-c(vynechat, 47, 48)]), col = "red")
lines(rowMeans(matrix01[,-c(v.ex, 47, 48)]), col = "orange")
lines(rowMeans(matrix01[,-c(47, 48)]), col = "blue")


## vytvoreni ukazkoveho rda control_segmented
# vybereme HC subjekt 01, vyradime epochy z v.ex, chceme vsechny elektrody
# postupne matici stimulu a odpovedi

segmented01 <- controls %>%
  dplyr::filter(subject == 1 & !epoch %in% v.ex)  |>
  dplyr::select(electrode, epoch, time, signal)

segmented01 <- as.data.frame(segmented01)
segmented01$epoch <- factor(segmented01$epoch)
epochy_lev <- levels(segmented01$epoch)
s01 <- segmented01 |>
  dplyr::filter(epoch == epochy_lev[1]) |>
  dplyr::select(electrode, signal)

segmented01matrix <- matrix(s01$signal, nrow = 204, byrow = FALSE)


segmented01 <- segmented01 |>
  spread(key = electrode, value = signal)


control_segmented <- data.frame()
control_segmented$stimulus <- segmented01

## vytvoreni db jenom s ukazkovymi daty
library(DBI)
library(dbplyr)
library(dplyr)

path_db <- "C:/Users/zdenda/Downloads/CEITEC"
con <- DBI::dbConnect(RSQLite::SQLite(), file.path(path_db, "exampledata.db"))

DBI::dbWriteTable(con, "control_segmented", segmented01)
data <- dplyr::tbl(con, "control_segmented")






## https://owncloud.cesnet.cz/index.php/s/prT2QGPwycuxUqV/download
con <- DBI::dbConnect(RSQLite::SQLite(),
                 dbname = "exampledata",
                 host = "https://owncloud.cesnet.cz/index.php/s/prT2QGPwycuxUqV")


## pro vytvoreni test dat do R potrebuju jen opravdu maly soubor, osekat to co nejvic
# nechat jen stimul, neresit resp
# vyberu 15 epoch
# elektrody potrebuju vsecky
# jen 50 casovych bodu
# jeden HC a jeden PAC
v.exx <- c(v.ex, c(6,7,9,12, 15, 19, 23, 26, 30, 31, 42, 43, 45, 46))

sub01 <- controls %>%
  dplyr::filter(subject == 1 & !epoch %in% v.exx & time %in% c(251:300) )  %>%
  dplyr::select(time, signal, epoch, electrode)

sub01df <- as.data.frame(sub01)
sub01df$epoch <- factor(sub01df$epoch)

save(sub01df, file = "demo_hc.rda")

patients <- dplyr::tbl(con, "Pstimul")

pac01 <- patients |>
  dplyr::filter(subject == 1 & !epoch %in% v.exx & time %in% c(251:300) )  %>%
  dplyr::select(time, signal, epoch, electrode)

pac01df <- as.data.frame(pac01)
pac01df$epoch <- factor(pac01df$epoch)
sub01df$subject <- rep(1, length(sub01df$time))
pac01df$subject <- rep(2, length(pac01df$time))

epochdata <- rbind(sub01df[,-5], pac01df[,-5])
epochdata$subject <- factor(epochdata$subject)
epochdata$time <- epochdata$time - 251
save(epochdata, file = "epochdata.rda")

boxplot_epoch(epochdata, subject = 1, channel = "E3", time_lim = c(260:270))

   epochdata |>
     dplyr::filter(subject == 1 & electrode == "E4") |>
     ggplot(aes(time, signal, color = epoch)) +
     geom_path() +
     theme_minimal() +
     xlab("Time point") + ylab("Amplitude (microvolts)") +
     labs(color = "Trial")

## vyvoj fce point_mesh
   n <- 100
   r <- 396
   N <- round(sqrt(2*n))
   x.vec <- rep(seq(-r, r, length.out = N), N)
   y.vec <- rep(seq(-r, r, length.out = N), each = N)
   mesh.square <- cbind(x.vec, y.vec)

   eu.vec <- edist0(x.vec, y.vec)
   out.vec <- which(eu.vec > r)
   mesh.square[out.vec,] <- NA

## plot
data("HCGSN256")
plot_point_mesh(mesh.square, names = T)


## vypocet inverze
Xcp <- na.omit(mesh.square)
X.P <- XP.IM(HCGSN256$D2)

ycp.IM2 <- mesh.square[,1]

R <- X.P
system.time(RI1 <- solve(R))
system.time(RI2 <- chol2inv(chol(R)))
system.time(RI3 <- qr.solve(R))



SplajnIM1 <- function(xi, xj)
  ## Funkce na vypocet prirozeneho kubickeho splajnu IM1
  ## jinak receno: vypocet matice S pro IM1
  ## xi, xj . . . vektory stejne delky
{
  k1 <- length(xi)
  k2 <- length(xj)

  xip <- rep(xi,times=k2)
  xjp <- rep(xj,each=k1)
  v <- ((abs(xip-xjp))^3)/12
  output <- matrix(v,nrow=k1,ncol=k2)
  return(output)
}

x1 <- seq(0,1, length = 10)
R <- SplajnIM1(x1,x1)




### vyber ROI
rep("frontal|central", ROIs)
vyber <- c("frontal", "central")
grep(paste(vyber, collapse = "|"), ROIs)


F1 <- pick_region(region = c("frontal", "temporal", "central"))

plot(HCGSN256$D2, type = "n")
points(F1, col = "blue")
LF1 <- pick_region(region = "frontal", hemisphere = "left")
points(LF1, col = "red", pch = 16)
points(pick_region(hemisphere = "right"), col = "black")


locations <- F1
mesh <- Fmesh1

make_polygon <- function(locations, mesh) {
  ## exclude points outside the defined polygon
  N <- dim(mesh)[1]
  coord.sf <- st_as_sf(locations, coords = c("x","y"))
  concave.sf <- coord.sf %>%
    summarise() %>%
    concaveman::concaveman(concavity = 2)
  polyg <- st_zm(concave.sf)
  polyg.sp <- as_Spatial(polyg) # spatial polygon
  coord.sp <- as_Spatial(coord.sf) # spatial points
  square.sp <- sp::SpatialPoints(na.omit(mesh))
  mesh.poly <- sp::point.in.polygon(square.sp@coords[,1], square.sp@coords[,2],
                                    polyg.sp@polygons[[1]]@Polygons[[1]]@coords[,1],
                                    polyg.sp@polygons[[1]]@Polygons[[1]]@coords[,2])

  idx <- which(mesh.poly == 1)
  mesh.wrap <- na.omit(mesh)[idx,]

  mesh.polygon <- matrix(NA, N, 2)
  mesh.poly.NA <- rep(0,N)
  mesh.poly.NA[-which(is.na(mesh[,1]))] <- mesh.poly
  mesh.polygon[which(mesh.poly.NA == 1),1] <- mesh.wrap$x
  mesh.polygon[which(mesh.poly.NA == 1),2] <- mesh.wrap$y
  return(mesh.polygon)

}


library(ptinpoly) # priklad z netu
poly.vertices <- data.frame(x=c(20,40,80,50,40,30), y=c(30,20,70,80,50,60))
p <- data.frame(x=runif(100, min=0, max=100), y=runif(100, min=0, max=100))

outside <- (pip2d(as.matrix(poly.vertices), as.matrix(p)) < 0)

plot(p$x, p$y, col=ifelse(outside, "red", "black"))
polygon(poly.vertices$x, poly.vertices$y, border="blue", col=NA)

poly.vertices <- F1
p <- na.omit(Fmesh1)

outside <- (pip2d(as.matrix(poly.vertices), as.matrix(p)) < 0)

plot(p$x, p$y, col=ifelse(outside, "red", "black"))
polygon(poly.vertices$x, poly.vertices$y, border="blue", col=NA)

## tohle nedela pekny polygon, ale "zuby"


## tohle niz FUNGUJE DOBRE, akorat to pri circle nedela diru na ucho, ale to asi uplne nevadi,
## to pujde snad osefovat tim, ze tam dam jako vstup polygon. sit

# Convert df to point feature
black <- pick_region(hemisphere = "left")
blk <- st_as_sf(black, coords = c("x", "y"))
blk <- coord.sf
# Convert to multipoint
blk_mp <- st_combine(blk)
# Define convex hull
blk_poly <- st_convex_hull(blk_mp)


plot(black)
red <- na.omit(Fmesh1)
points(red, col = "red")
plot(blk_poly, add = TRUE)
rd <- st_as_sf(red, coords = c("x", "y"))
rd_inside <- st_intersection(rd, blk_poly)

plot(black)
points(red)
plot(blk_poly, add = TRUE)
plot(rd_inside, pch = 24, col = "red", bg = "red", add = TRUE)

x <- unique(mesh.inside[,1])
y <- unique(mesh.inside[,2])
nx <- length(x)
ny <- length(y)

x.seq <- rep(x, ny)
y.seq <- rep(y, each = nx)
ctverec <- cbind(x.seq, y.seq)

plot(ctverec, cex = 0.5)
points(mesh.inside, col = "red", cex = 0.5, pch = 16)
