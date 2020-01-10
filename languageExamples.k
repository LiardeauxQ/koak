def binary : 1 (x y) y;

def fib(x)
  if (x < 3) then
    1
  else
    fib(x-1)+fib(x-2);

# Iterative fib.
def fibi(x)
  var a = 1, b = 1, c = 0 in
  (for i = 3, i < x, 1.0 in
    c = (a + b) :
    a = b :
    b = c) :
  b;

# Call it.
fibi(10);
