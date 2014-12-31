octopi_test
access: linearize
define:
	sz = 10
variables:
	t1:(sz,sz,sz)
	A:(sz,sz)
	B:(sz,sz)
	C:(sz,sz)
	D:(sz,sz,sz)
	U:(sz,sz,sz)

	t2:(sz,sz,sz)
operations:
	t1:(l,j,n) += Sum(m,(B:(m,j)*U:(l,m,n)))
	t2:(l,j,n) += Sum(m,(C:(m,l)*t1:(j,n,m)))
	D:(l,j,n) += Sum(m,(A:(m,n)*t2:(l,m,j)))

