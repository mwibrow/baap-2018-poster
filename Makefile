latexmk=latexmk
latex=lualatex
biber=biber
output_dir=build
input_dir=src
syntex=1
gs=gs
target=main
jobname=poster
src=$(input_dir)/$(target).tex
plots=$(input_dir)/plots
screenshots=$(input_dir)/images/CALVin-screenshots
TEXINPUTS:=$(input_dir)//:$(TEXINPUTS)
BIBINPUTS:=$(input_dir)//:$(BIBINPUTS)
dpi=300
underline=printf '=%.0s' {1..40}


all: images plots poster

final: dpi=1200
final: all

poster: paths latex biber rerun rererun
	cp $(output_dir)/$(jobname).pdf ./

latex rerun rererun: $(src)
	TEXINPUTS="$(TEXINPUTS)" $(latex) -recorder --output-directory=$(output_dir) --interaction=nonstopmode --jobname=$(jobname) $(src)

biber:
	BIBINPUTS="$(BIBINPUTS)" $(biber) $(output_dir)/$(jobname)

paths:
	@ echo; echo Creating paths; echo `$(underline)`; \
	mkdir -p build src/plots/images $(screenshots)/jpgs

plots: paths
	@ cd $(plots); \
	echo; echo Creating plots; echo `$(underline)`; \
	export DPI=$(dpi); \
	for script in *.R; do echo Executing $$script; Rscript $$script; done

images: paths
ifeq ($(dpi), 300)
	@ cd $(screenshots); \
	echo; echo Scaling and converting images; echo `$(underline)`; \
	mogrify -verbose -format jpg -path jpgs -scale 25% pngs/*.png;
else
    @ cd $(screenshots); \
	echo; echo Converting images; echo `$(underline)`; \
	mogrify -verbose -format jpg -path jpgs pngs/*.png
endif


clear-fontcache:
	luaotfload-tool --cache=erase

clean: FORCE
	rm -R build

FORCE:
