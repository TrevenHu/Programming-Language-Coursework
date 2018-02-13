from misc import Failure

class Vector(object):
    '''
    The constructor should take a single argument. 
    If this argument is either an int or a long or an instance of a class derived from one of these, 
    then consider this argument to be the length of the Vector. 
    In this case, construct a Vector of the specified length with each element is initialized to 0.0. 
    If the length is negative, raise a ValueError with an appropriate message. 
    If the argument is not considered to be the length, 
    then if the argument is a sequence (such as a list), 
    then initialize with vector with the length and values of the given sequence. 
    If the argument is not used as the length of the vector and if it is not a sequence, 
    then raise a TypeError with an appropriate message.
    '''
    def __init__(self, value):
    	try:
    		if value < 0:# Vector(-4)
    			raise ValueError("Vector length cannot be negative")
    	except TypeError:
    		pass
    	try:
      		self.data = [0.0] * value # Vector(3)
      	# Vector([4.5, "foo", 0])
    	except Exception: 
      		self.data = list(value)
    '''
    return a string of python code which could be used to initialize the Vector. 
    This string of code should consist of the name of the class 
    followed by an open parenthesis followed by the contents of the vector 
    represented as a list followed by a close parenthisis
    '''
    def __repr__(self):
    	return "Vector(" + repr(self.data) + ")"

     # The function __len__ should return the length of the Vector
    def __len__(self):
    	return len(self.data)
     
    # The function __iter__ should return an object that can iterate over the elements of the Vector.
    def __iter__(self):
     	for d in self.data:
     		yield d

    # vector + vector(list)
    def __add__(self, vec):
    	return Vector(([x + y for x, y in zip(list(self), list(vec))]))
    
    #  Reflected operands
    def __radd__(self, vec):
    	return Vector(([x + y for x, y in zip(list(self), list(vec))]))
    # self add (+= operators)
	def __iadd__(self, vec):
		self.data = Vector(([x + y for x, y in zip(list(self), list(vec))]))
        return self.data
    '''
    takes either a Vector or a sequence and returns the dot product of the argument with current Vector instance. 
    The dot product is defined as the sum of the component-wise products. 
    The behavior of this function if any elements are not numeric is undefined.
    '''
    def dot(self, vec):
	    try:
	    	return sum([x * y for x, y in zip(self, vec)])
	    except:
	      	return sum([str(x) + str(y) for x, y in zip(self, vec)])

    '''
    the __getitem__ and __setitem__ methods  allow element level access to the Vector. 
    Indexing should be 0 based (as in C). 
    If the index is negative, it should translate to the length of the Vector plus the index. 
    Thus, index -1 is the last element. 
    If the index is out of range, your implementation should raise an IndexError with an appropriate message. 
    This behavior should be identical to that of a list. These methods should preserve the length of the Vector.
    '''
    '''
    __getitem__ and __setitem__ methods  allow slice level access to the Vector. 
    These methods should preserve the length of the Vector. 
    If an assignment to a slice would change the length of the vector, raise a ValueError exception. 
    The semantics otherwise should mimic those of list.
    '''
    def __getitem__(self,i):
     	return self.data[i]

    def __setitem__(self, i, x):
     	self.data[i] = x
    '''
    Comparison functions for Vectors. 
    Two vectors should be considered equal if each element in the first Vector is equal to the respective element in the second Vector. 
    A Vector, a, should be considered greater than a Vector, b, if the largest element of a is greater than the largest element of b. 
    If the largest elements of both are equal, then compare the second-largest elements, and so forth. 
    If every pair compared in this fashion is equal, then a should not be considered greater than b, but a should be considered greater than or equal to b. 
    Note that if a is greater than b, then a is also greater than or equal to b. If a is greater than b, then b is less than a. 
    This is a nonstandard method for comparing vectors, and for a pair of vectors v and w, v>=w does not imply that v>w or v==w. 
    When a Vector is compared to something that isn't a Vector, they should never be equal. 
    You can assume that a Vector will never be compared with something that is not a Vector for any comparison operators other than "==", "!=". 
    (i.e. you don't need to handle non-vectors when you implement <, >, <=, >=). 
    You can also assume that vectors will not be compared with a Vector of a different length.
    '''
    def __gt__(self, vec):
    	if not isinstance(vec, Vector):
    		return False
    	for x, y in zip(sorted(self, reverse=True), sorted(vec, reverse=True)):
    		if not x > y:
    			return False
    	return True

    def __ge__(self, vec):
    	if not isinstance(vec, Vector):
    		return False
        for x, y in zip(sorted(self, reverse=True), sorted(vec, reverse=True)):
        	if not x >= y:
        		return False
        return True

    def __lt__(self, vec):
    	return not self.__ge__(vec)

    def __le__(self, vec):
    	return not self.__gt__(vec)

    def __eq__(self, vec):
    	if not isinstance(vec, Vector):
    		return False
    	for x,y in zip(self, vec):
    		if not x == y:
    			return False
    	return True

    def __ne__(self, vec):
    	return not self.__eq__(vec)





