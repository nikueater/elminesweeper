
all: develop

develop: 
	npx parcel src/index.html

production:
	npx parcel build src/index.html --out-dir docs
