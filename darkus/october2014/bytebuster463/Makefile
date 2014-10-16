all:
	fsharpc solution.fs

run:
	time mono solution.exe $N > /dev/null 2> time$N

check:
	mono solution.exe $N | ./n.pl > result$N
