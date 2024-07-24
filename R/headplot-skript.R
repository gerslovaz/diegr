M3 <- point_mesh(2, n = 1000, type = "polygon")
M3 <- na.omit(M3)
M4.3d <- point_mesh(3, n = 1000)
M3$index <- 1:length(M3$x)


seqy <- unique(M3$y)
k.y <- length(seqy)

TRIMAT <- c()
for (i in (1:(k.y - 1))) {

  rada1 <- M3[M3$y == seqy[i],]
  rada2 <- M3[M3$y == seqy[i + 1],]

  rastr <- sort(unique(c(rada1$x, rada2$x)))
  k <- length(rastr)
  x1 <- unique(rada1$x)
  x2 <- unique(rada2$x)

  doplneno1 <- rep(NA, length.out = length(rastr))
  doplneno1[which(rastr %in% x1)] <- rada1$index

  doplneno2 <- rep(NA, length.out = length(rastr))
  doplneno2[which(rastr %in% x2)] <- rada2$index

  col1 <- c(doplneno1[1], rep(doplneno1[2:(k - 1)], each = 2), doplneno1[k])
  col2licha <- doplneno1[2:k]
  col2suda <- doplneno2[2:k]
  col2 <- c(rbind(col2licha, col2suda))
  col3 <- rep(doplneno2[1:(k - 1)], each = 2)

  TRI <- cbind(col1, col2, col3)
  TRIMAT <- rbind(TRIMAT, na.omit(TRI))
}

M4 <- M3 |> dplyr::filter(y %in% seqy) # podmnozina meshe, mam jen 4 hodnoty y
M4$index <- 1:length(M4$x)
rada1 <- M4[M4$y == seqy[1],]
rada2 <- M4[M4$y == seqy[2],]

rastr <- sort(unique(c(rada1$x, rada2$x)))
k <- length(rastr)
x1 <- unique(rada1$x)
x2 <- unique(rada2$x)

doplneno1 <- rep(NA, length.out = length(rastr))
doplneno1[which(rastr %in% x1)] <- rada1$index

doplneno2 <- rep(NA, length.out = length(rastr))
doplneno2[which(rastr %in% x2)] <- rada2$index

col1 <- c(doplneno1[1], rep(doplneno1[2:(k - 1)], each = 2), doplneno1[k])
col2licha <- doplneno1[2:k]
col2suda <- doplneno2[2:k]
col2 <- c(rbind(col2licha, col2suda))
col3 <- rep(doplneno2[1:(k - 1)], each = 2)

TRI <- cbind(col1, col2, col3)
TRI <- na.omit(TRI)

M4.3d <- recompute_3d(HCGSN256$D2, HCGSN256$D3, M4[,1:2])


triangles3d(M4.3d$x[TRIMAT], M4.3d$y[TRIMAT], M4.3d$z[TRIMAT], alpha = 0.7) # NE

open3d()
polygons3d(M4.3d$x[TRIMAT], M4.3d$y[TRIMAT], M4.3d$z[TRIMAT], alpha = 0.7)


open3d()
wire3d( mesh3d(x = M4.3d$x, y = M4.3d$y, z = M4.3d$z, triangles = t(TRIMAT)) )
## mesh3d potrebuje homogenni souradnice, proto to musim zadat jako x, y, z
## triangles musi byt 3xn, nikoliv obracene
open3d()
shade3d(mesh3d(x = M4.3d$x, y = M4.3d$y, z = M4.3d$z, triangles = t(TRIMAT)), col = yc12[1:1142])

CS12 <- create_scale(c(-40,40))
yc12 <- rep(CS12$colors[1:20], each = 60)



beta.hat <- IM(HCGSN256$D2,prumerep$average[1:204])
X.Pcp <- XP_IM(HCGSN256$D2, M3) # pocita to docela dlouho uz na 30tis. siti, chce to nejak optimalizovat
y.Pcp <- X.Pcp%*%beta.hat
ycp.IM2 <- M1[,1]
ycp.IM2[-which(is.na(ycp.IM2))] <- y.Pcp[1:dim(as.matrix(na.omit(M1)))[1]]
yc12 <- cut(v1)
