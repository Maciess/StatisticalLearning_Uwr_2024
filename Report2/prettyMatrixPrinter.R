# https://stackoverflow.com/questions/45591286/for-r-markdown-how-do-i-display-a-matrix-from-r-variable
# Code author: stackoverflow user RLesur


# Define a generic method that transforms an object x in a LaTeX string
as_latex = function(x, ...) {
  UseMethod('as_latex', x)
}

# Define a class latex for LaTeX expressions
as_latex.character = function(x) {
  structure(
    paste(x, collapse = ' '), 
    class = c('latex', 'character')
  )
}

# A character string of class latex is rendered in display mode
# Define a knit_print() method for the latex class
knit_print.latex = function(x, ...) {
  knitr::asis_output(
    paste0('$$', x, '$$')
  )
} 

# Now, define a method as_latex for matrix
as_latex.matrix = function(x, ...) {
  as_latex(c(
    '\\begin{bmatrix}',
    paste(
      t(round(x,4)),
      rep(c(rep('&', nrow(x) - 1), '\\\\'), ncol(x)),
      collapse = ''
    ),
    '\\end{bmatrix}'
  ))
}

# Indicate to knitr that matrix are rendered as latex
knit_print.matrix = function(x, ...) {
  knitr::knit_print(as_latex(x))
}

# Build a knitr inline hook to display inline latex in inline mode
default_inline_hook = knitr::knit_hooks$get('inline')
knitr::knit_hooks$set(inline = function(x) {
  x = paste(gsub('\\$\\$', '$', x))
  default_inline_hook(x)
})