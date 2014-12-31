nek5000
access: linearize
define:
	N = 10
	J = 10
	M = 10
	I = 10
	L = 10
	K = 10
variables:
	A:(L,K)
	C:(N,I)
	temp3:(N,J,K)
	B:(M,J)
	U:(L,M,N)
	D:(I,J,K)
operations:
	temp3:(n,j,k) += Sum(l,(Sum(m,(A:(l,k)*B:(m,j)*U:(l,m,n)))))
	D:(n,j,k) += Sum(l,(C:(l,n)*temp3:(l,j,k)))

