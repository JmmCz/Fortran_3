import pyLib
import numpy 

A = [2,4,3,4,5,4,3,2,4]
A = numpy.array(A).reshape(3,3)
print(A)
B = [3,4,2,3,3,5,3,4,5]
B = numpy.array(B).reshape(3,3)
print(B)
C = pyLib.mm(A,B)
print(C)

D = numpy.array([[-1,2,1],[1,-3,-2], [3,-1,-1]], dtype='float32', order='F')
X = numpy.array([[-1],[-1],[4]], dtype='float32', order='F')
pyLib.gauss(D, X)

print(X)
