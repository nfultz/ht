#' Tiny Hash Table
#' 
#' This is a very basic implementation of a hash table using the \code{digest} package,
#' primarily for teaching functions and S3 for R programmers.
#' 
#' \code{ht} is an S3 class that extends\code{environment}, and additionally provides \code{[} and \code{[<-}.
#' 
#' Currently, default options are assummed for \code{digest}and hash collisions are not dealt with at all.
#' 
#' @name ht
#' @import digest
#' @examples
#'  x <- ht();
#'  x[1] <- 1
#'  x[1:2] <- 3:4
#'  x[1]
#'  x[1:2]
#'  x[[digest(1:2)]]
#'  mget(ls(x),x)
NULL

#' @export
#' @rdname ht
ht <- function(){
  `class<-`(new.env(parent = emptyenv()) , "ht");
}

#' @param index  The object to use as a key
#' @export
#' @rdname ht
`[.ht` <- function(x, index) {
  x[[digest(index)]]$value;
}

#' @param value The value of the hash table
#' @export
#' @rdname ht
`[<-.ht` <- function(x, index, value) {
  x[[digest(index)]] <- list(key=index, value=value);
  x;
}