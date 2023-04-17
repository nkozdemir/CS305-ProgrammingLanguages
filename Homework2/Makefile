parser: lex.yy.c kozdemir-hw2.tab.c kozdemir-hw2.tab.h
	gcc -o kozdemir-hw2 lex.yy.c kozdemir-hw2.tab.c -lfl

lex.yy.c: kozdemir-hw2.flx
	flex kozdemir-hw2.flx

kozdemir-hw2.tab.c kozdemir-hw2.tab.h: kozdemir-hw2.y
	bison -d kozdemir-hw2.y

clean:
	rm -f lex.yy.c
	rm -f kozdemir-hw2.tab.c
	rm -f kozdemir-hw2.tab.h
	rm -f kozdemir-hw2