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

.PHONY: ui_icons

icons:  $(IMAGEDIR)/neighbor_icons.png $(IMAGEDIR)/inventory_icons.png \
	$(IMAGEDIR)/avatar_icons.png ui_icons

$(IMAGEDIR)/neighbor_icons.png: tools/make_icons.py client/icons.txt | $(IMAGEDIR)
	tools/make_icons.py -n neighbor -s 34 -g items -g creatures -g avatars -g other \
		-o $(IMAGEDIR) client/icons.txt
	mv $(IMAGEDIR)/neighbor_icons.css $(CLIENTDIR)

$(IMAGEDIR)/inventory_icons.png: tools/make_icons.py client/icons.txt | $(IMAGEDIR)
	tools/make_icons.py -n inventory -s 24 -g items \
		-o $(IMAGEDIR) client/icons.txt
	mv $(IMAGEDIR)/inventory_icons.css $(CLIENTDIR)

$(IMAGEDIR)/avatar_icons.png: tools/make_icons.py client/icons.txt | $(IMAGEDIR)
	tools/make_icons.py -n avatar -s 60 -g avatars \
		-o $(IMAGEDIR) client/icons.txt
	mv $(IMAGEDIR)/avatar_icons.css $(CLIENTDIR)

ICONDIR = $(CLIENTDIR)/icons
ICONS = $(wildcard client/icons/*.png)
ui_icons: $(ICONS:%=$(BUILDDIR)/%)
$(ICONDIR)/%: client/icons/% | $(ICONDIR)
	cp $< $@
$(ICONDIR):
	@mkdir -p $@
