### print.lm is a function which replaces the normal print format of lm with GLM-friendly notation.
# Currently this only works with a single categorical explanatory variable.
# Example usage:
# > source("printlm.r")
# > mod
# Y_i = 59 + 6*X_1i + e_i
# X_1i = 0 means Sex is not male.
# X_1i = 1 means Sex is male.
# > h3gmodel
# Y_i = 62 + -4*X_1i + 4*X_2i + e_i
# X_1i = 0 means height3group is not short.
# X_1i = 1 means height3group is short.
# X_2i = 0 means height3group is not tall.
# X_2i = 1 means height3group is tall.

# Example usage:
#> get_index(attr(mymodel$coefficients, "names"), "(Intercept)")
#[1] 1
#> get_index(attr(mymodel$coefficients, "names"), "Sexmale")
#[1] 2
#> get_index(attr(mymodel$coefficients, "names"), "junk")
#[1] 0
get_index <- function(vector, entry){
    for(i in seq(vector)){
        if(vector[i] == entry){
            return(i)
        }
    }
    return(0)
}

# This only works with a single categorical explanatory variable.
# Possible issue: this assumes coefficients are in same order as levels. Are they always?
print.lm <- function(mod){
    intercept_index <- get_index(attr(mod$coefficients, "names"), "(Intercept)")
    xvar <- attr(mod$xlevels, "names") # (for example, "Sex")
    if(!is.null(xvar)){
        levels <- eval(parse(text=paste("mod$xlevels$", xvar[1], sep="")))
    }
    formula <- paste("Y_i =", mod$coefficients[intercept_index])
    desc <- list()
    k <- 1
    coeffs <- mod$coefficients[c(1:length(mod$coefficients)) != intercept_index]
    # This skips the intercept
    for(c in coeffs){
         desc[2*k-1] <- paste("X_", k, "i = 0 means ", xvar, " is not ", levels[k+1], ".", sep="")
         desc[2*k] <- paste("X_", k, "i = 1 means ", xvar, " is ", levels[k+1], ".", sep="")
         formula <- paste(formula, " + ", round(c, digits=5), "*X_", k, "i", sep="")
         k <- k + 1
    }
    cat(formula, "+ e_i", "\n")
    for(d in desc){
        cat(d, "\n")
    }
}

# BEN'S NOTES TO SELF START HERE
# Some things you can print out:
# print(summary(mymodel))
# model.matrix(mymodel)
# mycoefs <- coef(mymodel)
# attributes(mycoefs) <- NULL  # makes vector of coefficients plain

# a model is a list! the entries have names!
# mymodel has two attributes: "names" ("coefficients", "residuals", ...) and "class" ("lm")
# list names: attr(mymodel, "names")
# get entry from model list: mymodel$insert_name_here
# xname <- attr(mymodel$xlevels, "names") # xname <- "Sex"
# xcount <- length(mymodel$xlevels$Sex)
# my_x_levels <- mymodel$xlevels$Sex # my_x_levels <- "female" "male"
# same as above when you don't know "Sex"
# my_x_levels <- eval(parse(text=paste("mymodel$xlevels$",attr(mymodel$xlevels, "names"), sep="")))
# my_assign <- mymodel$assign
# END OF BEN'S NOTES TO SELF
