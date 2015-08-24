setClass(
  Class = "Circle", 
  representation = representation(
    radius = "numeric", 
    diameter = "numeric"
  )
)

# Value setting methods
# Note that the second argument to a function that is defined with setReplaceMethod() must be named value
setGeneric("radius<-", function(self, value) standardGeneric("radius<-"))
setReplaceMethod("radius", 
                 "Circle", 
                 function(self, value) {
                   self@radius <- value
                   self
                 }
)

setGeneric("diameter<-", function(self, value) standardGeneric("diameter<-"))
setReplaceMethod("diameter", 
                 "Circle", 
                 function(self, value) {
                   self@diameter <- value
                   self
                 }
)

# Value getting methods
setGeneric("radius", function(self) standardGeneric("radius"))
setMethod("radius", 
          signature(self = "Circle"), 
          function(self) {
            self@radius
          }
)

setGeneric("diameter", function(self) standardGeneric("diameter"))
setMethod("diameter", 
          signature(self = "Circle"), 
          function(self) {
            self@diameter
          }
)


# Method that calculates one value from another
setGeneric("calc_diameter", function(self) { standardGeneric("calc_diameter")})
setMethod("calc_diameter", 
          signature(self = "Circle"), 
          function(self) {
            self@diameter <- self@radius * 2
            self
          }
)


a <- new("Circle")
radius(a) <- 2
a <- calc_diameter(a)
radius(a)
diameter(a)

