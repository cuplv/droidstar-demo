default:
	cp -r server docker-server/_server
	docker build docker-server --tag droidstar-demo-server:latest

	cd client && elm make src/Main.elm --output index.html
	mkdir -p docker-client/_html
	cp client/index.html docker-client/_html/index.html
	cp -r client/css docker-client/_html/css
	docker build docker-client --tag droidstar-demo-client:latest

clean:
	rm -rf docker-client/_html
	rm -rf docker-server/_server
