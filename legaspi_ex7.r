
GetMultipliers <- function(x, y, degree, n){
   for(i in 1:n){
     term = y[i]
     for(j in 1:n){
       if(i == j ) { next; }
       
       term = paste(term, "((x - ", sep = " * ")
       term = paste(term, x[j], sep = "")
       term = paste(term, ")/", sep = "")
       
       term = paste(term, "(", sep = "")
       term = paste(term, x[i], sep = "")
       term = paste(term, " - ", sep = "")
       term = paste(term, x[j], sep = "")
       term = paste(term, "))", sep = "")
     }
     if(i == 1){
       polynomial_result = term
       next
     }
     polynomial_result = paste(polynomial_result, term, sep = " + ")
   }
  return(polynomial_result)
}

Lagrange <- function(independent, dependent){
  n = length(independent)
  degree = n - 1
  
  o = order(independent)
  x = independent[o]
  y = dependent[o]
  
  polynomial_result = GetMultipliers(x, y, degree, n)
  #print(polynomial_result)
  args = "x"
  result = list(f = eval(parse(text = paste('function(', args, ') { return(' , polynomial_result , ')}'))))
  return(result)
}

ColName <- function(){
  cols = c("I", "Xi", "|X-Xi|")
  for (i in 1:4) {
    col_name = paste("Pi", i, sep = "")
    cols <- c(cols, col_name)
  }
  return(cols)
}

RowName <- function(len){
  rows = c()
  for (i in 1:len) {
    rows <- c(rows, i)
  }
  return(rows)
}

InitMatrix <- function(x, y, given_x){
  len = length(x)
  m = matrix(data=NA, nrow=len, ncol=(3+len), dimnames = list(RowName(len), ColName()))
  
  for (c in 1:len) { #fill-up the first 4 columns of matrix
    m[c, 1] = c
    m[c, 2] = x[c]
    m[c, 3] = (given_x - x[c])
    m[c, 4] = y[c]
  }
  
  return(m)
}

FillMatrix <- function(x, y, given_x, m, n){
  count = n
  for (k in 1:(n-1)) {
    count = count - 1
    for (row in 1:count) {
      m[row, (k+4)] = ( (m[row, 3] * m[(row+1),(k+3)]) + ((m[(row+k), 2] - given_x) * m[row, (k+3)]) ) / (m[(row+k), 2] - m[row, 2])
    }
  }
  return(m)
}

Neville <- function(independent, dependent, given_x){
  n = length(independent)
  degree = n - 1
  
  o = order(independent)
  x = independent[o]
  y = dependent[o]
  
  m = InitMatrix(x, y, given_x)
  matrix_res = FillMatrix(x, y, given_x, m, n)
  res_final = list(mat = matrix_res, result = matrix_res[1, (n+3)]) 
}

#x = c(4.25, 1, 3.5, 5)
#y = c(1.4469, 0, 1.2528, 1.6094)
x = c(1, 3, 4, 5)
y = c(0, 1.0986, 1.3863, 1.6094)

ret_1 = Lagrange(x, y)
print(ret_1$f)
print(ret_1$f(2))

ret_2 = Neville(x, y, 2)
print(ret_2$mat)
print(ret_2$result)










