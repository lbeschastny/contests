#! python3
#WARNING! Wrote under windows. Not tested on linux.
#dict - matrix storage
d={}
import sys
N=int(sys.argv[1])
#print(N)
if N<=0:
	exit()
if N%2!=0:
	exit()
N2=N//2
N22=N2**2
#init dict
for i in range(N2):
	for j in range(N2):
		d[i*N2+j]=(i,j)

#def simple print
def p(*x):
	print(*x, sep='', end='')

#def generator 
def seq_turn(deep):
	if deep>=N22:
		yield d
	else:
		for phi in range(4):
			d[deep]=(d[deep][1],N-1-d[deep][0])
			for z in seq_turn(deep+1):
				yield z
#Simple checking matrix wholeness
def check(m):
	for i in range(N22):
		for j in range(i+1,N22):
			if abs(m[i][0]-m[j][0])+abs(m[i][1]-m[j][1])==1:
				return 1
	return 0

#Simple output matrix
def prettyp(m):
##	p("+"*(N+2)+'\n')
	for i in range(N):
##		p("+")
		for j in range(N):
			if (i,j) in m.values():
				p("X")
			else:
				p(".")
##		p('+')
		p('\n')
##	p("+"*(N+2)+'\n')
	p("\n")

#Matrix uniq config counter
und_cnt=0

#Init iterator
qq=seq_turn(0)

#Loop the iterator values
for m in qq:
##	if check(m)==0:
	und_cnt+=1
	prettyp(m)
#Output Matrix uniq config counter value
##p(und_cnt)
