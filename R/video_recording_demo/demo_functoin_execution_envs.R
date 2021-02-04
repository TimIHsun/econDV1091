greeting
greeting("John")

greetingPersonFromSameLocation <- function(username){
  browser()
  greeting2 <- function(username){
    browser()
    cat("Hi, ", username,". I live in ", location, ". ")
  }
  greeting2(username)
}
location <- "Taipei"
greetingPersonFromSameLocation("John")
