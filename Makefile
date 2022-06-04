.cask: Cask
	cask install

compile: .cask
	cask build

# test: .cask
# 	if [ "$$CI" != true ]; then make compile; fi # Locally, always rebuild
# 	cask exec buttercup tests/

tests/toml-test:
	wget "https://github.com/BurntSushi/toml-test/releases/download/v1.1.0/toml-test-v1.1.0-linux-amd64.gz"
	7z x toml-test-v1.1.0-linux-amd64.gz
	rm toml-test-v1.1.0-linux-amd64.gz
	chmod +x toml-test-v1.1.0-linux-amd64
	mv toml-test-v1.1.0-linux-amd64 tests/toml-test

test-decoder: tests/toml-test
	(cd tests && ./toml-test -v -- bash decoder-wrapper)

test-encoder: tests/toml-test
	(cd tests && ./toml-test -v -- bash encoder-wrapper)

.PHONY: test compile
