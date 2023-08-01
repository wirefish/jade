# Client-facing files: html, css, js, images, and icons.

BUILDDIR = build
CLIENTDIR = $(BUILDDIR)/client
MDFILES = $(wildcard client/*.md)
CPFILES = $(wildcard client/*.css) $(wildcard client/*.js) client/game.html

.PHONY: client config html images fonts icons

client: config html images fonts icons config

config: $(CLIENTDIR)/nginx.conf

$(CLIENTDIR)/nginx.conf: config/nginx.conf
	cp $< $@

html: $(MDFILES:%.md=$(BUILDDIR)/%.html) $(CPFILES:%=$(BUILDDIR)/%)

$(CLIENTDIR)/%.html: client/%.md client/template.html | $(CLIENTDIR)
	pandoc -s --template client/template.html $< -o $@

$(CLIENTDIR)/game.html: client/game.html | $(CLIENTDIR)
	cp $< $@

$(CLIENTDIR)/%.js: client/%.js | $(CLIENTDIR)
	cp $< $@

$(CLIENTDIR)/%.css: client/%.css | $(CLIENTDIR)
	cp $< $@

$(CLIENTDIR):
	@mkdir -p $@

FONTDIR = $(CLIENTDIR)/fonts
FONTS = $(wildcard client/fonts/*.woff)
fonts: $(FONTS:%=$(BUILDDIR)/%)
$(FONTDIR)/%: client/fonts/% | $(FONTDIR)
	cp $< $@
$(FONTDIR):
	@mkdir -p $@

IMAGEDIR = $(BUILDDIR)/client/images
IMAGES = $(wildcard client/images/*)
images: $(IMAGES:%=$(BUILDDIR)/%)
$(IMAGEDIR)/%: client/images/% | $(IMAGEDIR)
	cp $< $@
$(IMAGEDIR):
	@mkdir -p $@

icons: $(IMAGEDIR)/icons.png
$(IMAGEDIR)/icons.png: tools/make_icons.py client/icons.txt | $(IMAGEDIR)
	tools/make_icons.py client/icons.txt -o $(IMAGEDIR)
	mv $(IMAGEDIR)/icons.css $(CLIENTDIR)
