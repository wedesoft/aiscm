.SUFFIXES: .tmp .ini .pov .png .gif

POVRAY = povray
CONVERT = convert

LFRAME = 79

logo: blc.png blc.gif

blc.png: blca.png
	$(CONVERT) blca.png -background white -flatten blc.png

blc.gif: blc$(LFRAME).png
	$(CONVERT) blc??.png -delay 4 -colors 64 +dither $@

blca.png: blc.ini blc.pov opensans.ttf
	$(POVRAY) blc

blca$(LFRAME).png: blc.ini blc.pov opensans.ttf
	$(POVRAY) blcanim

blc$(LFRAME).png: blca$(LFRAME).png
	for i in $(shell seq -f '%02g' 0 $(LFRAME)); do \
		$(CONVERT) blca$$i.png -background white -flatten blc$$i.png; \
	done

clean:
	rm -f *.png *.gif
