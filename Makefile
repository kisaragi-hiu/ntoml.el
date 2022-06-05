.cask: Cask
	cask install

compile: .cask
	cask build

test: .cask
	cask exec buttercup tests/

tests/toml-test:
	wget "https://github.com/BurntSushi/toml-test/releases/download/v1.1.0/toml-test-v1.1.0-linux-amd64.gz"
	7z x toml-test-v1.1.0-linux-amd64.gz
	rm toml-test-v1.1.0-linux-amd64.gz
	chmod +x toml-test-v1.1.0-linux-amd64
	mv toml-test-v1.1.0-linux-amd64 tests/toml-test

GREP := | grep -a -i "fail"

test-decoder: tests/toml-test
	bash tests/test-decoder $(GREP)

test-encoder: tests/toml-test
	bash tests/test-encoder $(GREP)

.PHONY: test compile
