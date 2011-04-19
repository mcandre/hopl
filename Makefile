all: hopl.dot
	dot -Tpng hopl.dot > hopl.png

hopl.dot: hopl.hs
	runhaskell hopl.hs > hopl.dot

clean:
	-rm *.png
	-rm *.dot