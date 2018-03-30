latexmk=latexmk
latex=lualatex
biber=biber
output_dir=build
input_dir=src
syntex=1
gs=gs
target=main
jobname=poster
cmyk_profile=color_profiles/AdobeICCProfiles/CMYK/CoatedFOGRA39.icc
src=$(input_dir)/$(target).tex
plots=$(input_dir)/plots
TEXINPUTS:=$(input_dir)//:$(TEXINPUTS)
BIBINPUTS:=$(input_dir)//:$(BIBINPUTS)

poster: paths latex biber rerun rererun
	cp $(output_dir)/$(jobname).pdf ./

latex rerun rererun: $(src)
	TEXINPUTS="$(TEXINPUTS)" $(latex) -recorder --output-directory=$(output_dir) --interaction=nonstopmode --jobname=$(jobname) $(src)

biber:
	BIBINPUTS="$(BIBINPUTS)" $(biber) $(output_dir)/$(jobname)

paths:
	mkdir -p build

plots:
	@ cd $(plots); \
	for script in *.R; do echo Executing $$script; Rscript $$script; done

cmyk:
	gs \
		-o $(jobname)-cmyk.pdf \
		-sDEVICE=pdfwrite \
		-dOverrideICC=true \
		-sProcessColorModel=DeviceCMYK \
		-sColorConversionStrategy=CMYK \
		-sColorConversionStrategyForImages=CMYK \
		-sDefaultCMYKProfile=$(cmyk_profile) \
		-sOutputCMYKProfile=$(cmyk_profile) \
		-dEncodeColorImages=false \
		-dRenderIntent=3 \
		-dDeviceGrayToK=true \
		$(jobname).pdf

clear-fontcache:
	luaotfload-tool --cache=erase

clean: FORCE
	rm -R build

FORCE:
