myCheck <- function() {

  wIds=c("A1","B1","C1","D1","E1","F1")
  pd <- c(1,2,3.3, 2.5, 5.5, 7.8)
  pd <- data.frame(matrix(pd, ncol=1))
  rownames(pd) <- wIds;
  colnames(pd) <- c("param1")

  pdm <- list()
  for(i in 1:length(wIds)) {
    pdm[[wIds[i]]] <- pd[i,1]
  }
  cids <- c("c1","c2","c3","c4","c5","c6")

  ann <- list()
  for(i in 1:length(wIds)) {
    ann[[wIds[i]]] <- cids[i]
  }

  ps <- new("plateset", setname="test_set")
  p1 <- new("plate", plateId = "P1",
            format=6,
            wellIds=c("A","B","C","D","E","F"),
            plateData = pd,
            plateDataMap = pdm,
            plateAnnotMap = data.frame(t(data.frame(ann)))
  )


  p2 <- new("plate", plateId = "P2",
            format=6,
            wellIds=c("A","B","C","D","E","F"),
            plateData = pd,
            plateDataMap = pdm,
            plateAnnotMap = data.frame(t(data.frame(ann)))
  )

  ps2 <- aso:::addPlate(ps, p1)
  ps2 <- aso:::addPlate(ps2, p2)

  # ps2 <- addPlate(ps, p1)
  return(ps2)
}
