all:
	lein with-profile build uberjar

run:
	time java -jar ./target/crypto-square-0.1.0-SNAPSHOT-standalone.jar $N > /dev/null 2> time$N

check:
	java -jar ./target/crypto-square-0.1.0-SNAPSHOT-standalone.jar $N | ./n.pl > result$N
