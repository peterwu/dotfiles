include config.mk

# Build iosevka-fusion
build: build/private-build-plans.toml
	cd build/ && \
	npm run build -- ttf::iosevka-fusion

build/private-build-plans.toml: iosevka-fusion.toml
	cp $< $@

# Set up environment to build iosevka
setup:
	curl -L -O $(IOSEVKA_URL)
	tar xzf v$(IOSEVKA_VERSION).tar.gz
	mv Iosevka-$(IOSEVKA_VERSION) build
	cd build/ && npm install --cache /tmp/$(IOSEVKA_VERSION)-npm-cache

# Install patched fonts
install:
	[ -d  ~/.local/share/fonts/iosevka-fusion-fonts ] && rm -rf ~/.local/share/fonts/iosevka-fusion-fonts
	mkdir -p ~/.local/share/fonts/iosevka-fusion-fonts/
	cp -Rf ./build/dist/iosevka-fusion/ttf/*.ttf ~/.local/share/fonts/iosevka-fusion-fonts/
	fc-cache -vf

clean:
	rm -f v$(IOSEVKA_VERSION).tar.gz
	rm -rf build/dist/

nuke: clean
	rm -rf build/

.PHONY: clean nuke build setup install
