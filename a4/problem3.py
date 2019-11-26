'''
CIS 624
Assignment 4
Problem 3
Takes two inputs and applies the Ackermann function to them,
using a Y-combinator.
'''
def main(m,n):
    Y = lambda f: (lambda x: x(x))(lambda y: f(lambda *args: y(y)(*args)))
    aMan = lambda f: lambda m,n : (n+1 if m == 0 else (f(m-1, 1) if n == 0 else f(m-1, f(m, n-1))))
    print(Y (aMan) (m,n))

if __name__ == '__main__':
    m,n = input("Enter two non-negative numbers:\n").split()
    main(int(m), int(n))
