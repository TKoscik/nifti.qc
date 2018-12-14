thresh.apply <- function(data,
                         dir=c("gt", ">",
                               "ge", ">=",
                               "lt", "<",
                               "le", ">=",
                               "eq", "=", "==",
                               "btt", "between", "><",
                               "bet", ">=<",
                               "bte", "><=",
                               "bee", ">=<=",
                               "ott", "outside", "<>",
                               "oet", "<=>",
                               "ote", "<>=",
                               "oee", "<=>="),
                         value,
                         which.format=c("zeroed", "logical", "binary", "index.vec", "index.arr")) {

  which.data <- switch(dir[1],
    `gt`=(data > value[1])*1,
    `>`=(data > value[1])*1,

    `ge`=(data >= value[1])*1,
    `>=`=(data >= value[1])*1,

    `lt`=(data < value[1])*1,
    `<`=(data < value[1])*1,

    `le`=(data <= value[1])*1,
    `>=`=(data <= value[1])*1,

    `eq`=(data == value[1])*1,
    `=`=(data == value[1])*1,
    `==`=(data == value[1])*1,

    `btt`    =((data > value[1]) & (data < value[2]))*1,
    `between`=((data > value[1]) & (data < value[2]))*1,
    `><`     =((data > value[1]) & (data < value[2]))*1,

    `bet`=((data >= value[1]) & (data < value[2]))*1,
    `>=<`=((data >= value[1]) & (data < value[2]))*1,

    `bte`=((data > value[1]) & (data <= value[2]))*1,
    `><=`=((data > value[1]) & (data <= value[2]))*1,

    `bee`=((data >= value[1]) & (data <= value[2]))*1,
    `>=<=`=((data >= value[1]) & (data <= value[2]))*1,

    `ott`=((data < value[1]) | (data > value[2]))*1,
    `outside`=((data < value[1]) | (data > value[2]))*1,
    `<>`=((data < value[1]) | (data > value[2]))*1,

    `oet`=((data <= value[1]) | (data > value[2]))*1,
    `<=>`=((data <= value[1]) | (data > value[2]))*1,

    `ote`=((data < value[1]) | (data >= value[2]))*1,
    `<>=`=((data < value[1]) | (data >= value[2]))*1,

    `oee`=((data <= value[1]) | (data >= value[2]))*1,
    `<=>=`=((data <= value[1]) | (data >= value[2]))*1)

  output <- switch(which.format[1],
    `zeroed` = data * which.data,
    `logical` = array(as.logical(which.data), dim=dim(data)),
    `binary` = which.data,
    `index.vec` = which(array(as.logical(which.data), dim=dim(data))),
    `index.arr` = which(array(as.logical(which.data), dim=dim(data)), arr.ind = TRUE))

  return(output)
}
