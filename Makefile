
all: develop

develop: 
	npx parcel src/index.html

production:
	rm -rf ./dist/*
	npx parcel build src/index.html
