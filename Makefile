.PHONY: hlint stylish test-haskell deploy-contracts

# Admin Config
BLOC_ADMIN_USERNAME ?= "admin"
BLOC_ADMIN_PASSWORD ?= "password"
BLOC_ADMIN_FAUCET ?= "True"

# Bloc Config
BLOC_SCHEME ?= "Http"
BLOC_HOST ?= "localhost"
BLOC_PORT ?= 10001
BLOC_PATH ?= "/bloc/v2.1"

# Contracts Config
CONTRACTS_YAML ?= "./contracts-iam.yaml"
CONTRACTS_DIR ?= "./contracts-iam"

BUILD_ROOT ?= "."
VERBOSE_CONTRACT_UPLOAD ?= "SILENT"

deploy-contracts:
		BLOC_SCHEME=$(BLOC_SCHEME) \
		BLOC_HOST=$(BLOC_HOST) \
		BLOC_PORT=$(BLOC_PORT) \
		BLOC_PATH=$(BLOC_PATH) \
    BLOC_ADMIN_USERNAME=$(BLOC_ADMIN_USERNAME) \
    BLOC_ADMIN_PASSWORD=$(BLOC_ADMIN_PASSWORD) \
    BLOC_ADMIN_FAUCET=$(BLOC_ADMIN_FAUCET) \
    CONTRACTS_DIR=$(CONTRACTS_DIR) \
    CONTRACTS_YAML=$(CONTRACTS_YAML) \
    BUILD_ROOT=$(BUILD_ROOT) \
    VERBOSE_CONTRACT_UPLOAD=$(VERBOSE_CONTRACT_UPLOAD) \
    upload-contracts

test-ci:
		stack test

hlint:
	hlint src "--ignore=Parse error" -XTypeApplications; \
		hlint test/Data "--ignore=Parse error" -XTypeApplications

stylish:
	find ./src -name "*.hs" | xargs stylish-haskell -c ./.stylish_haskell.yaml -i; \
		find ./uploader -name "*.hs" | xargs stylish-haskell -c ./.stylish_haskell.yaml -i; \
		find ./test -name "*.hs" | xargs stylish-haskell -c ./.stylish_haskell.yaml -i

