#' Tiny Hash Table
#' 
#' This is a very basic implementation of a hash table using the \code{digest} package,
#' primarily for teaching functions and S3 for R programmers.
#' 
#' \code{ht} is an S3 class that extends \code{environment}, and additionally provides \code{[} and \code{[<-}.
#' It can use arbitrary R objects as keys and values.
#' 
#' Currently, default options are assummed for \code{digest} and hash collisions are not dealt with at all.
#' 
#' @name ht
#' @seealso \code{\link[digest]{digest}}
#' @import digest
#' @examples
#'  x <- ht()
#'  x[1] <- 1
#'  x[1:2] <- 3:4
#'  x[1]
#'  x[1:2]
#'  1:2 %in.ht% x
#'  mget(ls(x),x)
#'  if(require(digest)) x[[digest(1:2)]]
NULL

#' @export
#' @rdname ht
ht <- function(){
  structure(new.env(parent = emptyenv()), class="ht");
}

#' @param x an \code{ht} object
#' @param index  A key object
#' @export
#' @rdname ht
`[.ht` <- function(x, index) {
  x[[digest(index)]]$value;
}

#' @param value A value object
#' @export
#' @rdname ht
`[<-.ht` <- function(x, index, value) {
  x[[digest(index)]] <- list(key=index, value=value);
  x;
}

#' @export
#' @rdname ht
`%in.ht%` <- function(index, x) {
  exists(digest(index), envir = x, inherits = FALSE)
}
